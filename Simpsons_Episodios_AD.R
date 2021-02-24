library(tidyverse)

simpsons <- read.csv("C:/Users/winseven/Downloads/datasets_553928_1008865_simpsons_episodes (1).csv",sep = ",", stringsAsFactors = FALSE)

nomes <-  colnames(simpsons)

##Ajustando dados faltantes
simpsons[!complete.cases(simpsons$imdb_rating),]
simpsons[is.na(simpsons$imdb_rating),]$imdb_rating = median(simpsons$imdb_rating,na.rm = T)
simpsons[is.na(simpsons$imdb_votes),]$imdb_votes = median(simpsons$imdb_votes,na.rm = T)
simpsons[!complete.cases(simpsons$us_viewers_in_millions),]$us_viewers_in_millions = median(simpsons$us_viewers_in_millions,na.rm = T)
simpsons[!complete.cases(simpsons$views),]$views = median(simpsons$views,na.rm = T)

summary(simpsons)

## eliminando colunas que não usarei
simpsons$image_url <- NULL
simpsons$video_url <- NULL

## dados Exploratorios

summary(simpsons$imdb_rating)
summary(simpsons$imdb_votes)
summary(simpsons$us_viewers_in_millions)
summary(simpsons$views)
summary(simpsons$original_air_year)

## Verificando o comportamento das colunas
hist(simpsons$imdb_rating,main = "",ylab = "", xlab = "", col = "#FEDC03")
par(new = TRUE)
plot(density(simpsons$imdb_rating),main = "",ylab = "", xlab = "", axes=F, lwd=2.5,col = "Blue" )

hist(simpsons$imdb_votes,main = "",ylab = "", xlab = "", col = "#FEDC03")
par(new = TRUE)
plot(density(simpsons$imdb_votes),main = "",ylab = "", xlab = "", axes=F, lwd=2.5,col = "Blue" )

hist(simpsons$us_viewers_in_millions,main = "",ylab = "", xlab = "", col = "#FEDC03")
par(new = TRUE)
plot(density(simpsons$us_viewers_in_millions),main = "",ylab = "", xlab = "", axes=F, lwd=2.5,col = "Blue" )

hist(simpsons$views,main = "",ylab = "", xlab = "", col = "#FEDC03")
par(new = TRUE)
plot(density(simpsons$views),main = "",ylab = "", xlab = "", axes=F, lwd=2.5,col = "Blue" )

hist(simpsons$original_air_year,main = "",ylab = "", xlab = "", col = "#FEDC03")
par(new = TRUE)
plot(density(simpsons$original_air_year),main = "",ylab = "", xlab = "", axes=F, lwd=2.5,col = "Blue" )

## Quantidade de dados
dadosAnuais <- data.frame(ano = simpsons$original_air_year,
                          notaIMDB = simpsons$imdb_rating,
                          votacaoIMDB = simpsons$imdb_votes)
head(dadosAnuais)


mediasAnuais <-aggregate(dadosAnuais$notaIMDB, by = list(dadosAnuais$ano), FUN = mean)
colnames(mediasAnuais) <- c("Anos","Medias")
head(mediasAnuais)

QuantidadeEpisodios <- dadosAnuais %>% count(ano)
colnames(QuantidadeEpisodios) <- c("Anos","Episodios")
head(QuantidadeEpisodios)

ggplot(data = QuantidadeEpisodios,aes(Anos,Episodios)) + geom_col()

library(plotly)

plot_ly(data = QuantidadeEpisodios, x= ~Anos,y=~Episodios,type = 'bar', text = QuantidadeEpisodios$Episodios,
        textposition = 'auto',
        marker = list(color = 'rgb(254, 220, 3)', line = list(color = 'rgb(255, 255, 255)',width = 1.5)))

dadosAnuais$visualizoesEUA <- simpsons$us_viewers_in_millions
head(dadosAnuais)

MediasVisualizacoesAnuais <- aggregate(dadosAnuais$visualizoesEUA, by = list(dadosAnuais$ano), FUN = mean)
colnames(MediasVisualizacoesAnuais) <- c("Anos","Medias")
head(MediasVisualizacoesAnuais)

plot_ly(data = MediasVisualizacoesAnuais, x= ~Anos,y=~Medias,type = 'bar', 
        text = MediasVisualizacoesAnuais$Medias,
        textposition = 'auto',
        marker = list(color = 'rgb(254, 220, 3)', line = list(color = 'rgb(254, 220, 3)',width = 1.5)))

maioresVisualizacoesSerie <- data.frame(episodio = simpsons$number_in_series, 
                                        visualizoesEUA = simpsons$views,
                                        codigoProducao = simpsons$production_code,
                                        tituloEpisodio = simpsons$title)

maioresVisualizacoesSerie <- maioresVisualizacoesSerie[order(maioresVisualizacoesSerie$visualizoesEUA,decreasing = TRUE),]

head(maioresVisualizacoesSerie)

episodiosSerie <- data.frame(episodio = simpsons$number_in_series, visualizoesEUA = simpsons$views)
episodiosSerie <- episodiosSerie[order(episodiosSerie),]
head(episodiosSerie)

plot_ly(data = episodiosSerie, x = ~episodio, y = ~visualizoesEUA, mode = "Line")

ggplot(data = episodiosSerie,aes(x = episodio, y = visualizoesEUA)) + 
        geom_line(color = "#fdd824",size = 1) + 
        theme(panel.background = element_rect(fill = "#FFFFFF"),
              panel.grid.major = element_line(colour = "#FFFFFF")) + 
        labs(x = "Episodios", y = "Visulizações nos EUA",title = "Visualizações por Episodios\n dos Simpsons",
             subtitle = "Os pirmeiros 600 episodios",caption = "")

dezMaioresVisualizacoesSerie <- maioresVisualizacoesSerie[1:10,]

plot_ly(dezMaioresVisualizacoesSerie,
        x = ~visualizoesEUA, 
        y = ~codigoProducao,
        type = 'bar', 
        orientation = 'h',
        text = ~visualizoesEUA,
        marker = list(color = "#fdd824"))

maioresVisualizacoesSerie

