library(forecast) 
library(rminer)


### ---- Possivel código para série temporal, isto é, se considerarmos uma série temporal como a do gáfico do artigo


m <- dat[1, "Month.of.absence"]
j <- 1
count_hours <- rep(0,(12*3 + 1))
for(i in 1:nrow(dat)){
  if(dat[i, "Month.of.absence"] != m){
    m <- dat[i, "Month.of.absence"]
    j <- j+1
  }
  count_hours[j] <- count_hours[j] + dat[i, ncol(dat)]
}

ts_count_hours <- ts(count_hours, frequency = 1)
plot(ts_count_hours, type = "o")   ### acrescentar legenda
#tsdisplay(ts_count_hours)


H <- 3 # number of ahead predictions: last 3 months (May/2010 to Jul/2010)
ts_train <- ts(count_hours[1:(length(count_hours)-H)], frequency = 1)
target <- count_hours[((length(count_hours)-H+1)):length(count_hours)]

# arima method:
AR <- auto.arima(ts_train)
forec_ar <- forecast(AR, h = H) # 1 to H ahead forecasts 
pred_ar <- forec_ar$mean[1: H] # AR predictions 
txt <- paste("AR SMAPE:", round(mmetric(target, pred_ar, metric="SMAPE"),2),"\n") 
mgraph(target, pred_ar, 
       graph = "REG", Grid = 10,
       col = c("black", "blue"), 
       leg=list(pos = "topleft", leg = c("target", "predictions")), main = txt)

# neural network modeling:
ts_train <- CasesSeries(ts_count_hours, c(1,2,3)) # data.frame from time series 
LD <- nrow(d)
dtr <- 1:(LD-H) # train indices 
NN <- fit(y~., d[dtr,], model="mlpe")
# from 1 to H ahead forecasts: 
pred_ar2 <- lforecast(NN, d, start = (LD-H+1), horizon = H)
txt <- paste("NN SMAPE:", round(mmetric(target, pred_ar2, metric="SMAPE"),2),"\n")
mgraph(target, pred_ar2,
       graph = "REG", Grid = 10,
       col = c("black","blue"), 
       leg = list(pos = "topleft", leg = c("target","predictions")), main = txt)

