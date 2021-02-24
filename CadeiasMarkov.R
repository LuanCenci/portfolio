library(tidyverse)
library(magrittr)
library(plotly)

p <- matrix(c(0.7,0.20,0.10,0.15,0.80,0.05,0.15,0.15,0.70),nrow = 3,ncol = 3)
p

x0 <- matrix(c(0.15,0.20,0.65),nrow = 3,ncol = 1)
x0

X <- p%*%x0
X * 100000

(p^2)

xis <- vector("list",length(c))

for(i in 1:length(c)){
  if(i == 1){
    xis[[i]] <- p%*%x0
  }else{
    xis[[i]] <- (p*c[i])%*%x0
  }
}

names(xis) <- c

xis$`1`

estadosFinais <- matrix(data = 1:15,nrow = 5,ncol = 3)
colnames(estadosFinais) <- c("A","B","C")
estadosFinais

estadosFinais[1,] = xis$`1`
estadosFinais[2,] = xis$`2`
estadosFinais[3,] = xis$`3`
estadosFinais[4,] = xis$`4`
estadosFinais[5,] = xis$`5`


estadosFinaisDataFrame <- as.data.frame(estadosFinais)
estadosFinaisDataFrame$Ano <- c
estadosFinaisDataFrame

plot_ly(data = estadosFinaisDataFrame, x=~Ano, y=~A, name = colnames(estadosFinaisDataFrame)[1], type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~B, name = colnames(estadosFinaisDataFrame)[2], type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~c, name = colnames(estadosFinaisDataFrame)[3], type = 'scatter', mode = 'lines+markers') 





