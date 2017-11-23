library(fpp)
library(dplyr)

RS = read.csv('/Users/brettscroggins/Downloads/RSGCSN.csv') %>%
  select(-DATE) %>%
  ts(start=c(1992,1), frequency=12)

tr = window(RS, end=c(2011,12))
te = window(RS, start=c(2012,1))
plot(RS)
abline(v=c(2011,12), col='grey')



### 1 ###

f.HW = ets(tr, model="AAM", restrict = FALSE)
summary(f.HW)

fc.HW = forecast(f.HW, h = 68)
plot(fc.HW)
points(te, col='red', pch=19)

plot(fc.HW, xlim=c(2009,2018), ylim=c(40000,60000))
points(te, col='red', pch=19)

accuracy(fc.HW, te)



### 2 ###

f.HW2 = ets(tr, model="AAM", damped = FALSE, restrict = FALSE)
summary(f.HW2)

fc.HW2 = forecast(f.HW2, h = 68)
plot(fc.HW2, xlim=c(2009,2018), ylim=c(40000,60000))
points(te, col='red', pch=19)

accuracy(fc.HW2, te)



### 3 ###

f.O = ets(tr, model="ZZZ", restrict = FALSE)
summary(f.O)

fc.O = forecast(f.O, h = 68)
plot(fc.O, xlim=c(2009,2018), ylim=c(40000,60000))
points(te, col='red', pch=19)

accuracy(fc.O, te)



### 4 ###

L = BoxCox.lambda(tr)
z = BoxCox(tr, L)

fB.O = ets(tr, model="ZZZ", restrict = FALSE, lambda = L)
summary(fB.O)

fBc.O = forecast(fB.O, h = 68)
plot(fBc.O, xlim=c(2009,2018), ylim=c(40000,60000))
points(te, col='red', pch=19)

accuracy(fBc.O, te)



### 5 ###

fB.OD = ets(tr, model="ZZZ", damped = TRUE, restrict = FALSE, lambda = L)
summary(fB.OD)

fBc.OD = forecast(fB.OD, h = 68)
plot(fBc.OD, xlim=c(2009,2018), ylim=c(40000,60000))
points(te, col='red', pch=19)

accuracy(fBc.OD, te)



### 6 ###

rmse = 1E8
year = NULL

for (i in 1992:2006){
  
  tr2 = window(RS, start=c(i,1), end=c(2011,12))
  L = BoxCox.lambda(tr2)
  f.loop = ets(tr2, model="ZZZ", restrict = FALSE, lambda = L)
  temp_year = i
  temp_rmse = accuracy(f.loop)[2]
  
  if (temp_rmse < rmse){
    rmse = temp_rmse
    year = temp_year
  }
}
print(year)
print(rmse)

trr = window(RS, start=c(2003,1), end=c(2011,12))



### 7 ###

L = BoxCox.lambda(trr)
z = BoxCox(trr, L)

f = ets(trr, model="ZZZ", restrict = FALSE, lambda = L)
summary(f)

fc = forecast(f, h = 68)
plot(fc, xlim=c(2009,2018), ylim=c(40000,60000))
points(te, col='red', pch=19)

accuracy(fc, te)



### 8 ###

trrr = window(RS, start=c(2003,1), end=c(2017,8))

ff = ets(trrr, model="MAA", restrict = FALSE)
summary(ff)

ffc = forecast(ff, h = 64)
plot(ffc)

### AICc is way to compare models based on the same data set
### - lower is better

### BIC is way to compare results in compared to Bayes
### - lower is better also
