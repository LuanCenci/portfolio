library(readxl)
library(plotly)
library(ggplot2)
library(zoo)

cotacaoHistorica <- read_excel("C:/Users/winseven/Downloads/xgvxConsulta.xls")

cotacaoHistorica$Data <- converterData(cotacaoHistorica$Data)

rollmean(cotacaoHistorica$cotacao,5,align = "right")

plot_ly(data = cotacaoHistorica, x = ~Data, y = ~cotacao, 
        mode = "lines")

ggplot(data = cotacaoHistorica,aes(x = Data,y = cotacao)) + 
  geom_line(color = "red") + 
  xlab("") +
  theme(panel.background = element_rect(fill = "white"))



SerieTemporal <- ts(data = cotacaoHistorica$cotacao, start = data_ts_anual(cotacaoHistorica$Data[1]),
                    end = data_ts_anual(cotacaoHistorica$Data[6609]), frequency = 365)

plot(SerieTemporal)

window(SerieTemporal,start = c(2000,1),end = c(2000,12))

decompose(SerieTemporal)

