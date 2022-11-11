data<-read.csv("WindPower_Data.csv")


plot(data)

data<-data[,-c(1)]
data.ts = ts(data$ACTUAL.WIND.MW.,
           frequency = 23,
           start = c(23))

ggAcf(data.ts) + ggtitle("White Noise Test using ACF Plot")

res.p=periodogram(data.ts)
df.p = data.frame(freq = res.p$freq,
                  spec = res.p$spec,
                  time = 1/res.p$freq) %>% 
  arrange(desc(spec))
head(df.p, 3)

summary(data.ts)
plot(data.ts)
plot(decompose(data.ts))


theme_set(theme_bw())
autoplot(data.ts) +
  ggtitle("Time Series Plot of the Time-Series") +
  theme(plot.title = element_text(hjust = 0.5)) #for centering the text'

# Creating a VAR model with vars
# var.a <- vars::VAR(data.ts,
#                    lag.max = 2, #highest lag order for lag length selection according to the choosen ic
#                    ic = "AIC", #information criterion
#                    type = "none") #type of deterministic regressors to include
# summary(var.a)

nvalid=round(nrow(data)*0.3)
ntrain=nrow(data)-(nvalid)
# train.data<-window(data,start=c(23,1),end=c(23,ntrain))
# valid.data<-window(data,start=c(23,ntrain+1),end=c(23,nrow(data)))

train.data<-data[1:ntrain,]
  train.data.y<-train.data[,1]
  train.data<-train.data[,-c(1)]
valid.data<-data[ntrain+1:nrow(data),]
  valid.data.y<-valid.data[,1]
  valid.data<-valid.data[,-c(1)]
  
  train.data.ts = ts(train.data$ACTUAL.WIND.MW.,
               frequency = 23,
               start = c(23))

fit <- tslm(train.data.ts ~ trend + season)
plot(forecast(fit, h=nrow(valid.data)))
summary(model)
------------------
  
install.packages("summarytools")
install.packages("tseries")
install.packages("forecast")
library(forecast)
library(ggplot2)
library(tseries)
library(summarytools)

as.data.frame(data.ts)
cycle(data.ts)
plot(data.ts, ylab="Passengers (1000s)", type="o")