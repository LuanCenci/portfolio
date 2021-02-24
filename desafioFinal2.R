install.packages("RPostgreSQL")
install.packages("RPostgres")

library(RPostgreSQL)
library(RPostgres)
library(DBI)
library(dplyr)
library(magrittr)

conexao = dbConnect( RPostgres::Postgres(),
                     dbname = "credito", 
                     host= "localhost",
                     port = "5432",
                     user = "postgres",
                     password = "112358")

result <- dbGetQuery(conexao,"select * from visaoGeral")

# Verificar valores faltantes

summary(result)

vazios <- result[!complete.cases(result),]

result %>% count(ResidenciaDesde,sort = TRUE)

result %>% count(HABITACAO,sort = TRUE)

result[is.na(result$ResidenciaDesde),]$ResidenciaDesde = 4

result[result$HABITACAO == "proria",]$HABITACAO = "propria"

result[result$HABITACAO == "de graca",]$HABITACAO = "gratuita"

result %>% count(HABITACAO,sort = TRUE)

result %>% count(ESTADOCIVIL,sort = TRUE)

result[result$ESTADOCIVIL == "maculino div/dep",]$ESTADOCIVIL = "masculino div/dep"

result %>% count(PROPOSITO,sort = TRUE)

result[result$PROPOSITO == "qualificacao",]$PROPOSITO = "educacao"

result[result$PROPOSITO == "quitar divida",]$PROPOSITO = "outros"

result[result$PROPOSITO == "reforma casa",]$PROPOSITO = "reforma"

result[result$PROPOSITO == "veiculo usado",]$PROPOSITO = "veiculo"
result[result$PROPOSITO == "veiculo novo",]$PROPOSITO = "veiculo"

result %>% count(PROPOSITO,sort = TRUE)

result %>% count(OUTROSFINANCIAMENTOS,sort = TRUE)

result %>% count(SocioEmpresa,sort = TRUE)

result %>% count(FIADOR,sort = TRUE)

result[result$FIADOR == "co requerente",]$FIADOR = "co-requerente"
result[result$FIADOR == "co aplicante",]$FIADOR = "co-aplicante"

result[result$FIADOR == "co-requerente",]$FIADOR = "co-participativo"
result[result$FIADOR == "co-aplicante",]$FIADOR = "co-participativo"

result %>% count(FIADOR,sort = TRUE)

result %>% count(EMPREGO,sort = TRUE)

result %>% count(TempoParcelamento,sort = TRUE)

result %>% count(INVESTIMENTOS,sort = TRUE)

result$INVESTIMENTOS = stringr::str_remove_all(result$INVESTIMENTOS,"/u000A")

result %>% count(Status,sort = TRUE)

result[duplicated(result$IDCREDITO),]

# idades fora de limite

summary(result$Idade)

result[result$Idade < 0 | result$Idade > 110]$Idade

result[is.na(result$Idade)]

# outliers

boxplot(result$Valor)
boxplot(result$Valor)$out

boxplot(result$Valor,outline = FALSE)

# frequencia
categorias <- table(cut(result$Idade, breaks = seq(18,75,by = 5),right = FALSE))
categorias

tabela <- rbind(categorias, 
                porcentagem = 100 * prop.table(categorias))


tabelaCategorias <- as.data.frame(
                      t(cbind(
                             tabela,
                             c(sum(tabela[1,]),sum(tabela[2,])
                               ))),row.names = c(colnames(tabela),"Total"))

tabelaCategorias <- transform(tabelaCategorias,porcentagem = round(porcentagem,digits = 2))
tabelaCategorias

barplot(
     table(result$HABITACAO),
     ylab = "Frequencia",
     cex.names =  0.7,
     names.arg = c("Própria","Alugada","Gratuita"),
     col = "darkgrey",
     border = "darkgrey",
     axes = TRUE,
     ylim = c(0,750)
     )


barplot(
     table(result$FIADOR),
     ylab = "Frequencia" ,
     cex.names = 0.85,
     names.arg = c("Sim","Não","co-participativo"),
     col = "#53575b",
     border = "dimgray",
     axes = TRUE,
     ylim = c(0,950))


write.csv2(result,"C:/Users/winseven/Desktop/R/portfolio/dados/result.csv")

sum(tabelaCategorias$categorias[1-10])



