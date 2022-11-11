

weather1<-read.csv("Carlow Weather.csv")
weather2<-read.csv("Cavan Weather.csv")
weather3<-read.csv("Clare Weather.csv")
weather4<-read.csv("Cork Weather.csv")
weather5<-read.csv("Donegal Weather.csv")
weather6<-read.csv("Dublin Weather.csv")
weather7<-read.csv("Galway Weather.csv")
weather8<-read.csv("Kerry Weather.csv")
weather9<-read.csv("Mayo Weather.csv")
weather10<-read.csv("Meath Weather.csv")
weather11<-read.csv("Roscommon Weather.csv")
weather12<-read.csv("Tipperary Weather.csv")
weather13<-read.csv("Wexford Weather.csv")


weather1<-weather1[((dim(weather1)[1])-840):((dim(weather1)[1])-1),c("wdsp","msl")]
weather2<-weather2[((dim(weather2)[1])-840):((dim(weather2)[1])-1),c("wdsp","msl")]
weather3<-weather3[((dim(weather3)[1])-840):((dim(weather3)[1])-1),c("wdsp","msl")]
weather4<-weather4[((dim(weather4)[1])-840):((dim(weather4)[1])-1),c("wdsp","msl")]
weather5<-weather5[((dim(weather5)[1])-840):((dim(weather5)[1])-1),c("wdsp","msl")]
weather6<-weather6[((dim(weather6)[1])-840):((dim(weather6)[1])-1),c("wdsp","msl")]
weather7<-weather7[((dim(weather7)[1])-840):((dim(weather7)[1])-1),c("wdsp","msl")]
weather8<-weather8[((dim(weather8)[1])-840):((dim(weather8)[1])-1),c("wdsp","msl")]
weather9<-weather9[((dim(weather9)[1])-840):((dim(weather9)[1])-1),c("wdsp","msl")]
weather10<-weather10[((dim(weather10)[1])-840):((dim(weather10)[1])-1),c("wdsp","msl")]
weather11<-weather11[((dim(weather11)[1])-840):((dim(weather11)[1])-1),c("wdsp","msl")]
weather12<-weather12[((dim(weather12)[1])-840):((dim(weather12)[1])-1),c("wdsp","msl")]
weather13<-weather13[((dim(weather13)[1])-840):((dim(weather13)[1])-1),c("wdsp","msl")]


colnames(weather1) <- c('Carlow.wdsp','Carlow.msl')
colnames(weather2) <- c('Cavan.wdsp','Cavan.msl')
colnames(weather3) <- c('Clare.wdsp','Clare.msl')
colnames(weather4) <- c('Cork.wdsp','Cork.msl')
colnames(weather5) <- c('Donegal.wdsp','Donegal.msl')
colnames(weather6) <- c('Dublin.wdsp','Dublin.msl')
colnames(weather7) <- c('Galway.wdsp','Galway.msl')
colnames(weather8) <- c('Kerry.wdsp','Kerry.msl')
colnames(weather9) <- c('Mayo.wdsp','Mayo.msl')
colnames(weather10) <- c('Meath.wdsp','Meath.msl')
colnames(weather11) <- c('Roscommon.wdsp','Roscommon.msl')
colnames(weather12) <- c('Tipperary.wdsp','Tipperary.msl')
colnames(weather13) <- c('Wexford.wdsp','Wexford.msl')

weather_data <- cbind(weather1,weather2,weather3,weather4,weather5,weather6,weather7
                      ,weather8,weather9,weather10,weather11,weather12,weather13)


#----------------
# 
# power23<-read.csv("Wind Power Gen/WindGeneration_23.Sep.2022.00.00_23.Sep.2022.23.59.csv")
# power24<-read.csv("Wind Power Gen/WindGeneration_24.Sep.2022.00.00_24.Sep.2022.23.59.csv")
# power25<-read.csv("Wind Power Gen/WindGeneration_25.Sep.2022.00.00_25.Sep.2022.23.59.csv")
power25<-read.csv("Wind Power Gen/WindGeneration_25.Sep.2022.00.00_25.Sep.2022.23.59.csv")
power26<-read.csv("Wind Power Gen/WindGeneration_26.Sep.2022.00.00_26.Sep.2022.23.59.csv")
power27<-read.csv("Wind Power Gen/WindGeneration_27.Sep.2022.00.00_27.Sep.2022.23.59.csv")
power28<-read.csv("Wind Power Gen/WindGeneration_28.Sep.2022.00.00_28.Sep.2022.23.59.csv")
power29<-read.csv("Wind Power Gen/WindGeneration_29.Sep.2022.00.00_29.Sep.2022.23.59.csv")
power30<-read.csv("Wind Power Gen/WindGeneration_30.Sep.2022.00.00_30.Sep.2022.23.59.csv")

#power_data<-rbind(power23,power24,power25,power26,power27,power28,power29,power30)
power_data<-rbind(power25,power26,power27,power28,power29,power30)

power25=power25[,-c(2,5)]

power_data = power_data %>% 
  mutate(
    time = parse_date_time(DATE...TIME, orders = "dmy HM"),
    ACTUAL.WIND.MW. = as.numeric(ACTUAL.WIND.MW.),
    date = date(time),
    wday = wday(time),
    hour = format.Date(time, "%H"),
    DATE...TIME = paste0(date, "-", hour)
  )

# aggregate data
power_data = power_data %>% 
  group_by(DATE...TIME) %>% 
  summarise(ACTUAL.WIND.MW. = sum(ACTUAL.WIND.MW.),
            date = head(date, 1),
            wday = head(as.numeric(wday), 1),
            hour = head(as.numeric(hour), 1))

power_data=power_data[,-c(3,4,5)]
#power_data = power_data[seq(1, nrow(power_data), 4), -c(2,4)]

#-------------

data<-cbind(power_data,weather_data)
write.csv(data,"WindPower_Data.csv", row.names = FALSE)

#--------------------------

demand_data<-read.csv("SystemDemand_27.Aug.2022.00.00_25.Sep.2022.23.59 (1).csv")
demand_data = demand_data %>% 
  mutate(
    time = parse_date_time(DATE...TIME, orders = "dmy HM"),
    ACTUAL.WIND.MW. = as.numeric(ACTUAL.DEMAND.MW.),
    date = date(time),
    wday = wday(time),
    hour = format.Date(time, "%H"),
    DATE...TIME = paste0(date, "-", hour)
  )

# aggregate data
demand_data = demand_data %>% 
  group_by(DATE...TIME) %>% 
  summarise(ACTUAL.DEMAND.MW. = sum(ACTUAL.DEMAND.MW.),
            date = head(date, 1),
            wday = head(as.numeric(wday), 1),
            hour = head(as.numeric(hour), 1))


data.ts = ts(demand_data$ACTUAL.DEMAND.MW.,
             frequency = 24,
             start = c(23))
#White Noise Test
ggAcf(data.ts)+ggtitle("White Noise Test using ACF Plot")
#Random Walk Test

getNumericRounding()
library(vrtest)
x=Auto.Q(data.ts)

plot(data.ts)
plot(decompose(data.ts))
