setwd("~/Dropbox/MASTER/Complementos/Trabajo")

Preludio <- read.csv("Prelude1.csv", header = F, sep=";")

colnames(Preludio) <- c('pista','date', 'comando', 'canal', 'nota', 'dinamica')

Pre <- Preludio$nota
#PreDifln <- diff(log(Pre))

plot(Preludio$nota,  type = "l")

library(forecast)

Acf(Pre)
Pacf(Pre)
Acf(Pre*Pre)

auto.arima(Pre, max.p=5, max.q=5, seasonal=F) 
auto.arima(Pre, max.p=5, max.q=5, seasonal=T)

PreArima <- Arima(Pre, order=c(2,1,2), include.mean=F)
tsdisplay(PreArima$resid)

PreArimaForecast <- forecast.Arima(PreArima, h=16)
plot.forecast(PreArimaForecast)

PreArimaSeasonal <- Arima(Pre, order=c(1,0,1), seasonal = list(order = c(3,0,0)), include.mean=F)
tsdisplay(PreArimaSeasonal$resid)
PreArimaSeasonalForecast <- forecast.Arima(PreArimaSeasonal, h=16)
plot.forecast(PreArimaSeasonalForecast)



