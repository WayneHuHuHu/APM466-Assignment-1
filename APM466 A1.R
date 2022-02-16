setwd("~/Downloads/APM466 A1")
library(readxl)
#All bonds data collected
Bonds_Data = read_excel("Bond Data.xlsx")
#Bonds selected for further computation
Selected_Bonds = read_excel("Selected Bonds.xlsx")
#To compiute YTM
YTM = data.frame()
date = c("10/01/2022","11/01/2022","12/01/2022","13/01/2022","14/01/2022","17/01/2022","18/01/2022","19/01/2022","20/01/2022","21/01/2022")
Dates = as.Date(date, "%d/%m/%y")
library(jrvFinance)
#To compute YTM of bond i on day j
for(i in c(1:11)){
    for (j in c(1:10)){
      YTM[i,j]=bond.yield(Dates[j],Selected_Bonds$Maturity[i],Selected_Bonds$Coupon[i],freq = 2,Selected_Bonds[[i,j+6]],"ACT/ACT",comp.freq = 2,redemption_value = 100)
    }  
}
#To compute spot rate, first compute dirty price of each bond
day = c(10, 11, 12, 13, 14, 17, 18, 19, 20, 21)
i = 1
while(i <= 10){
  colname_DP = paste("Dirty Price", toString(i), sep = "")
  Accured_Interest = (2*31+2*30+(day[i]))/365*Selected_Bonds$Coupon*100
  Dirty_Price = Accured_Interest + Selected_Bonds[[paste("CPD", toString(i), sep="")]]
  Selected_Bonds[[colname_DP]] = Dirty_Price
  i = i + 1
}
#There are two bonds that matures at different months, hence the last coupon payment date was different
Adj_Accured_Interest_L286 = (31/365)*Selected_Bonds[[2,3]]*100
Adj_Accured_Interest_M359 = (31/365)*Selected_Bonds[[4,3]]*100
for (i in c(1:10)) {
  Selected_Bonds[2,i+16] = Selected_Bonds[2,i+16]+Adj_Accured_Interest_L286
  Selected_Bonds[4,i+16] = Selected_Bonds[4,i+16]+Adj_Accured_Interest_M359
}
#Interpolation
YTM_Final = data.frame()
for (i in c(1:10)) {
  for (j in c(1:10)) {
    YTM_Final[i,j] = approx(Selected_Bonds$MonthToMaturity,YTM[[j]],xout = 6*i)$y
  }
}
colnames(YTM_Final) = Dates
rownames(YTM_Final) = c("0.5years","1year","1.5years","2years","2.5years","3years","3.5years","4years","4.5years","5years")
#First Compute spot rate for bonds mature in less than six months
Spot_Rate = data.frame()
for (i in c(1:10)) {
  Price1 = Selected_Bonds[[1,i+16]]
  Coupon1 = Selected_Bonds[[1,3]]*100/2
  Year_To_Maturity1 = Selected_Bonds[[1,6]]/12
  Spot_Rate[1,i] = 2*((Price1/(Coupon1+100))^(-1/(2*Year_To_Maturity1))-1)
}
#For spot rate in following time periods
Month_Since_Last_Coupon = c(4,5,4,4,4,4,4,4,4,4,4)
for (i in c(2:11)) {
  for (j in c(1:10)) {
    Price = Selected_Bonds[[i,j+16]]
    Coupon = Selected_Bonds$Coupon[[i]]*100/2
    CouponPV = 0
    Year_To_Maturity = Selected_Bonds$MonthToMaturity[i]/12
    Coupon_Time = seq((6-Month_Since_Last_Coupon[i])/12,(Selected_Bonds$MonthToMaturity[i]-1)/12,1/2)
    for (k in c(1:length(Coupon_Time))) {
     CouponPV = CouponPV+Coupon*(1+Spot_Rate[k,j]/2)^(-2*Coupon_Time[k]) 
    }
    Spots_Price = Price-CouponPV
    CouponPV = 0
    Spot_Rate[i,j] = 2*((Spots_Price/(Coupon+100))^(-1/(2*Year_To_Maturity))-1)
  }
}
colnames(Spot_Rate) = Dates
rownames(Spot_Rate) = c("0 years","0.5years","1year","1.5years","2years","2.5years","3years","3.5years","4years","4.5years","5years")
#Forward Rate
Forward_Rate = data.frame()
for (i in c(1:4)) {
  for (j in c(1:10)) {
    Year = (1+Spot_Rate[2*i,j]^(2*i))
    One_Year_Foward_Rate = (1+Spot_Rate[2+2*i,j]/2)^(2+2*i)
    Forward_Rate[i,j] = 2*((One_Year_Foward_Rate/Year)^(1/2)-1)
  }
}
colnames(Forward_Rate) = Dates
rownames(Forward_Rate) = c("1yr-1yr","1yr-2yr","1yr-3yr","1yr-4yr")
#Covariance matrices, eigenvalue and eigenvector for daily log-returns of yield
log_return_yield1 = log_return_yield2 = log_return_yield3 = log_return_yield4 = log_return_yield5 = vector(mode = "numeric",length = 9)
for (i in c(1:9)) {
  log_return_yield1[i]=log(YTM_Final[2,i+1]/YTM_Final[2,i])
  log_return_yield2[i]=log(YTM_Final[4,i+1]/YTM_Final[4,i])
  log_return_yield3[i]=log(YTM_Final[6,i+1]/YTM_Final[6,i])
  log_return_yield4[i]=log(YTM_Final[8,i+1]/YTM_Final[8,i])
  log_return_yield5[i]=log(YTM_Final[10,i+1]/YTM_Final[10,i])
}
X_yield = data.frame(log_return_yield1,log_return_yield2,log_return_yield3,log_return_yield4,log_return_yield5)
Cov_yield = cov(X_yield,X_yield)
Eigen_Space_Yield = eigen(Cov_yield)
print(Eigen_Space_Yield$values)
print(Eigen_Space_Yield$vectors)
colnames(Cov_yield) = c("X_1","X_2","X_3","X_4","X_5")
rownames(Cov_yield) = c("X_1","X_2","X_3","X_4","X_5")
#Covariance matrices, eigenvalue and eigenvector for forward rates
oneyr_oneyr = oneyr_twoyr = oneyr_threeyr = oneyr_fouryr = vector(mode = "numeric", length = 4)
for (i in c(1:4)) {
  oneyr_oneyr[i] = log(Forward_Rate[1,i+1]/Forward_Rate[1,i])
  oneyr_twoyr[i] = log(Forward_Rate[2,i+1]/Forward_Rate[2,i])
  oneyr_threeyr[i] = log(Forward_Rate[3,i+1]/Forward_Rate[3,i])
  oneyr_fouryr[i] = log(Forward_Rate[4,i+1]/Forward_Rate[4,i])
}
X_forward = data.frame(oneyr_oneyr,oneyr_twoyr,oneyr_threeyr,oneyr_fouryr)
Cov_forward = cov(X_forward,X_forward)
Eigen_Space_forward = eigen(Cov_forward)
print(Eigen_Space_forward$values)
print(Eigen_Space_forward$vectors)
colnames(Cov_forward) = c("X_1","X_2","X_3","X_4")
rownames(Cov_forward) = c("X_1","X_2","X_3","X_4")
#Plot Yield Curve
plot(YTM_Final$`2020-01-10`,col = "2", type = "l", ylim = c(0,0.02), xaxt = "n", xlab = "", ylab = "Yield to Maturity", main = "5-Year Yield Curve" )
axis(side = 1,at = c(1:10), tcl = -0.2, las = 2, labels = c("0.5years","1year","1.5years","2years","2.5years","3years","3.5years","4years","4.5years","5years"))
title(xlab = "Years", mgp = c(4,1,0))
lines(YTM_Final$`2020-01-11`,col = "3")
lines(YTM_Final$`2020-01-12`,col = "4")
lines(YTM_Final$`2020-01-13`,col = "5")
lines(YTM_Final$`2020-01-14`,col = "6")
lines(YTM_Final$`2020-01-17`,col = "7")
lines(YTM_Final$`2020-01-18`,col = "8")
lines(YTM_Final$`2020-01-19`,col = "9")
lines(YTM_Final$`2020-01-20`,col = "10")
lines(YTM_Final$`2020-01-21`,col = "11")
legend("bottomright", date, lty = c(1,1), lwd = c(2,2), cex = 0.5, col = c(2:12))
#Plot the Spot Curve
plot(Spot_Rate$`2020-01-10`,col = "2", type = "l", ylim = c(0,0.02), xaxt = "n", xlab = "", ylab = "Spot Rate", main = "5-Year Spot Rate" )
axis(side = 1,at = c(1:11), tcl = -0.2, las = 2, labels = c("0years","0.5years","1year","1.5years","2years","2.5years","3years","3.5years","4years","4.5years","5years"))
title(xlab = "Years", mgp = c(4,1,0))
lines(Spot_Rate$`2020-01-11`,col = "3")
lines(Spot_Rate$`2020-01-12`,col = "4")
lines(Spot_Rate$`2020-01-13`,col = "5")
lines(Spot_Rate$`2020-01-14`,col = "6")
lines(Spot_Rate$`2020-01-17`,col = "7")
lines(Spot_Rate$`2020-01-18`,col = "8")
lines(Spot_Rate$`2020-01-19`,col = "9")
lines(Spot_Rate$`2020-01-20`,col = "10")
lines(Spot_Rate$`2020-01-21`,col = "11")
legend("bottomright", date, lty = c(1,1), lwd = c(2,2), cex = 0.5, col = c(2:12))
#Plot the forward rate curve
plot(Forward_Rate$`2020-01-10`,col = "2", type = "l", ylim = c(0,0.1), xaxt = "n", xlab = "", ylab = "Forward Rate", main = "Forward Rate" )
axis(side = 1,at = c(1:4), tcl = -0.2, las = 2, labels = c("1yr-1yr","1yr-2yr","1yr-3yr","1yr-4yr"))
title(xlab = "Years", mgp = c(4,1,0))
lines(Forward_Rate$`2020-01-11`,col = "3")
lines(Forward_Rate$`2020-01-12`,col = "4")
lines(Forward_Rate$`2020-01-13`,col = "5")
lines(Forward_Rate$`2020-01-14`,col = "6")
lines(Forward_Rate$`2020-01-17`,col = "7")
lines(Forward_Rate$`2020-01-18`,col = "8")
lines(Forward_Rate$`2020-01-19`,col = "9")
lines(Forward_Rate$`2020-01-20`,col = "10")
lines(Forward_Rate$`2020-01-21`,col = "11")
legend("bottomright", date, lty = c(1,1), lwd = c(2,2), cex = 0.5, col = c(2:12))
