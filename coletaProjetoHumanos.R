library(tibble)
library(httr)
library(rvest)
library(dplyr)
library(ggplot2)
library(forecast)
library(plotly)
library(zoo)

titulos <- seq(1:10)

urls <- c("projetohumanos.com.br/wiki/extras-episodio-01/",
          "projetohumanos.com.br/wiki/extras-episodio-02/",
          "projetohumanos.com.br/wiki/extras-episodio-03/",
          "projetohumanos.com.br/wiki/extras-episodio-04/",
          "projetohumanos.com.br/wiki/extras-episodio-05/",
          "projetohumanos.com.br/wiki/extras-episodio-06/",
          "projetohumanos.com.br/wiki/extras-episodio-07/",
          "projetohumanos.com.br/wiki/extras-episodio-08/",
          "projetohumanos.com.br/wiki/extras-episodio-09/",
          "projetohumanos.com.br/wiki/extras-episodio-10/")

titulos = urls
titulo = urls

for(i in urls){
  
  titulo[i] <- urls[i] %>%
                httr::GET() %>%
                httr::content('text', encoding = 'utf-8') %>%
                xml2::read_html() %>%
                rvest::html_nodes('h1.entry-title')
  
  titulos[i] <- titulo[i] %>% rvest::html_text()
}

