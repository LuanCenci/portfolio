library(tidyverse)

credito <- read.csv("C:/Users/winseven/Desktop/R/portfolio/dados/credito2.csv", sep=",",na.strings = "",stringsAsFactors = TRUE)

nomesColunas<-read.csv(file="C:/Users/winseven/Desktop/senoide.csv")

colnames(credito) <- nomesColunas$x

summary(credito)

head(credito)

## explorar dados categóricos

barplot(table(credito$historico),main = "historico",xlab="historico")
barplot(table(credito$proposito),main = "proposito",xlab="proposito")
barplot(table(credito$estadoCivil),main = "Estado Civil",xlab = "Estado Civil")
barplot(table(credito$fiador),main = "Fiador",xlab = "Fiador")
barplot(table(credito$habitacao),main = "Habitação",xlab = "Habitação")
barplot(table(credito$emprestimoExistente),main ="Emprestimos",xlab ="Emprestimos")
barplot(table(credito$profissao),main ="Profissão",xlab ="Profissão")
barplot(table(credito$dependentes),main ="Dapendentes",xlab ="Dapendentes")
barplot(table(credito$socioEmpresa),main ="Socio Empresa",xlab ="Socio Empresa")
barplot(table(credito$estrangeiro),main ="Estrangeiro",xlab ="Estrangeiro")

## explorar dados numericos
summary(credito$duracao)
boxplot(credito$duracao)
boxplot(credito$duracao,outline = F)
hist(credito$duracao)

summary(credito$periodoParcelamento)
boxplot(credito$periodoParcelamento)
boxplot(credito$periodoParcelamento,outline = F)
hist(credito$periodoParcelamento)

summary(credito$valor)
boxplot(credito$valor)
boxplot(credito$valor,outline = F)
hist(credito$valor)

summary(credito$idade)
boxplot(credito$idade)
boxplot(credito$idade,outline = F)
hist(credito$idade)

## verificar completude de dados
credito[!complete.cases(credito),]
credito[is.null(credito$periodoResidencial),]

##Ajeitar duracao
summary(credito$duracao)
median(credito$duracao,na.rm = T)
credito[is.na(credito$duracao),]$duracao = median(credito$duracao,na.rm = T)

summary(credito$valor)
median(credito$valor,na.rm = T)
credito[is.na(credito$valor),]$valor = median(credito$valor,na.rm = T)

summary(credito$idade)
median(credito$idade,na.rm = T)
credito[is.na(credito$idade),]$idade = median(credito$idade,na.rm = T)

## verificar falta de padrão 
summary(credito$idade)
credito[credito$idade<0 | credito$idade >115,]$idade

## verificar dados duplicados
duplicado <- credito[duplicated(dados$Id),]

credito <- credito[!credito$id %in% c(duplicado$id),]

##valores fora de dominio

summary(credito$idade)
credito[credito$idade < 0 | credito$idade > 115,]$idade
credito[is.na(credito$idade),]

##outliers 
desvioPadrao = sd(credito$duracao,na.rm = T)
desvioPadrao

credito[credito$duracao >= 2*desvioPadrao, ]$duracao

median(credito$duracao)
credito[credito$duracao >= 2*desvioPadrao,]$duracao = median(credito$duracao)

boxplot(credito$duracao)
##valor - outliers
desvioPadrao = sd(credito$valor)
desvioPadrao

boxplot(credito$valor)
boxplot(credito$valor,outline = F)
x = boxplot(credito$valor)$out
x

credito[credito$valor >= 3*desvioPadrao,]$valor <- median(credito$valor)

##Salvar os dados

install.packages("mongolite")

library(mongolite)

url_path = 'mongodb://localhost:27017/admin'

mongoDB <- mongo(collection = "primeiroAjuste",
                 db = "desafioFinal",
                 url = url_path,
                 verbose = TRUE)

print(mongoDB)

mongoDB$insert(credito)











