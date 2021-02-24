library(tibble)
library(httr)
library(rvest)
library(dplyr)
library(ggplot2)
library(forecast)
library(plotly)
library(zoo)
library(magrittr)

url <- "https://ciis.fmrp.usp.br/covid19//wp-content/isa/dadosMunicipioFlorianopolis.html"

tabela_html <- url %>%
  httr::GET() %>%
  httr::content('text', encoding = 'utf-8') %>%
  xml2::read_html() %>%
  rvest::html_node('table')

tabela_html_th <- url %>%
    httr::GET() %>%
    httr::content('text', encoding = 'utf-8') %>%
    xml2::read_html() %>%
    rvest::html_nodes('th')

nomes <- tabela_html_th %>% rvest::html_text()

Serie <- tabela_html %>% rvest::html_table()

colnames(Serie) <- nomes
  
edit(Serie)

#write.csv(nomes,"C:/Users/winseven/Desktop/nomes.csv")  

nomes <- read.csv("C:/Users/winseven/Desktop/nomes.csv")
nomes <- nomes$x
  
colnames(Serie) <- nomes

Serie$Data <- lubridate::as_date(Serie$Data)

data_ts_anual <- function(data) {
  ano <- lubridate::year(data)
  numero <- as.numeric(data - (lubridate::floor_date(data, "year") - 1))
  c(ano, numero)
}

data_ts_anual(Serie$Data[1])

SerieTemporal <- ts(Serie$casosConfirmadosTotais,
                    c(2020,1), 
                    c(2020,207),
                    (365*207))

plot(SerieTemporal)

                                    
plot_ly(data = Serie, x = as.Date(Serie$Data), y=Serie$casosConfirmadosTotais,mode = 'lines') %>%
 add_trace()  

ggplot(data = Serie,aes(x = as.Date(Data),y = casosConfirmadosTotais)) + 
  geom_line(color = "Red") + 
  xlab("") +
  theme(panel.background = element_rect(fill = "white"))

ggplot(data = Serie,aes(x = as.Date(Data),y = Serie$casosConfirmadosNovos)) + 
  geom_line(color = "red") + 
  xlab("") +
  theme(panel.background = element_rect(fill = "white"))


subst <- window(SerieTemporal,start = c(2020,1),end = c(2020,20))
subst
