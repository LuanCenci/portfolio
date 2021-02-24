install.packages("gstat")

library(geobr)
mg<-read_state(code_state ='MG')
#caso queira visualizar, basta executar plot(mg)
plot(mg) 

library(readr)
temperatura_minasgerais <- read_delim("https://www.luisotavio.pro/blog/temperatura_minasgerais.txt", 
                                      "/t", escape_double = FALSE, trim_ws = TRUE)

library(sf)
temperatura_minasgerais.sf <- st_as_sf(temperatura_minasgerais,coords = c('Longitude','Latitude'),crs=4674) #o código 4674 é uma referência as coordenadas, esse é o código utilizado para o Brasil e outros países próximos.

library(dplyr)
estrutura.mg <- st_make_grid(mg,cellsize = c(.07,.07)) %>% 
  st_as_sf() %>%
  filter(st_contains(mg,.,sparse = FALSE))

plot(estrutura.mg)

library(gstat)
modelo<-gstat(formula = temp~1,
              data = as(temperatura_minasgerais.sf,'Spatial'),
              set=list(idp=3))

temp.interpolacao <- predict(modelo,as(estrutura.mg,'Spatial')) %>%
  st_as_sf()

library(ggplot2)
library(fields) #biblioteca para usar a paleta de cores tim.colors
ggplot(temp.interpolacao) + 
  geom_sf(aes(fill=var1.pred,col=var1.pred))+
  geom_sf(data=mg,fill='transparent')+
  scale_color_gradientn(colors = tim.colors(50),
                        limits=c(19,28))+
  scale_fill_gradientn(colors = tim.colors(50),
                       limits=c(19,28))+
  theme_bw()+
  labs(title = "Dados Interpolados",
       fill ='ºC',
       color= 'ºC')

#definindo os parâmetros
qualidade<-0.002 #quanto menor, melhor será a qualidade e mais demorada será a execução do código
dist_grupamento<-0.1
corte_distancia<-0.75

library(png)
library(grid)
r <- readPNG('C:/Users/winseven/Pictures/campo.png') #Ler a imagem de fundo que vamos usar para o campo de futebol
rg <- rasterGrob(r, width=unit(0.9,"npc"), height=unit(0.9,"npc")) #Ajustes para usar a imagem como fundo do mapa de calor

library(readr)
coordenadas <- read_delim("https://www.luisotavio.pro/blog/dados_victorferraz.txt", 
                          "\t", escape_double = FALSE, trim_ws = TRUE)

head(coordenadas)

valor_y<-seq(-0.05,0.05,qualidade)
valor_x<-seq(-0.05,0.05,qualidade)

criar_coordenadas<- function(valor_x,valor_y){
  c(coordenadas[,"x"]+valor_x,coordenadas[,"y"]+valor_y)
}

library(data.table)
criando_coordenadas <-lapply(valor_x, function(valor_x) lapply(valor_y, function(valor_y) criar_coordenadas(valor_x,valor_y))) #Cria lista com as coordenadas
coordenadas_fake<-rbindlist(unlist(criando_coordenadas, recursive = FALSE)) #transforma a lista em data frame

library(proxy)
distancias<-dist(coordenadas_fake,coordenadas,method = "euclidean")

library(dplyr)
distancias_rank<-sapply(1:nrow(distancias), function(x){min_rank(distancias[x,])}) %>%
  t()

matriz_logica<-(distancias_rank<=2) & (distancias<dist_grupamento) #pra ser verdadeiro tem que ser uma das 
#duas distâncias mais próximas e menor que dist_grupamento

distancias_media<-sapply(1:nrow(distancias),function(x) mean(distancias[x,matriz_logica[x,]],na.rm = T))

percentil<-quantile(distancias_media, corte_distancia,na.rm = T) 
coordenadas_fake<-coordenadas_fake[distancias_media<percentil,]
coordenadas_fake<-anti_join(coordenadas_fake,coordenadas) #evitar que pontos falsos repitam pontos reais
coordenadas_fake<-unique(coordenadas_fake) #remover pontos duplicados

vertices<-data.frame(x=c(0,0,1,1),y=c(0,1,0,1))
dados_estrutura<-rbind(coordenadas,coordenadas_fake,vertices)
dados_estrutura$id<-1:nrow(dados_estrutura)

library(sf)
lista <- lapply(1:nrow(dados_estrutura), function(x){
  ## create a matrix of coordinates that also 'close' the polygon
  res <- matrix(as.numeric(c(dados_estrutura[x, 'x'], dados_estrutura[x, 'y'],
                             dados_estrutura[x, 'x'], dados_estrutura[x, 'y']-qualidade/2,
                             dados_estrutura[x, 'x']-qualidade/2, dados_estrutura[x, 'y']-qualidade/2,
                             dados_estrutura[x, 'x']-qualidade/2, dados_estrutura[x, 'y'],
                             dados_estrutura[x, 'x'], dados_estrutura[x, 'y']))  ## need to close the polygon
                , ncol =2, byrow = T
  )
  ## create polygon objects
  st_polygon(list(res))
})
sfdf <- st_sf(id = dados_estrutura[, 'id'], st_sfc(lista)) #transformando para o formato adequado


estrutura_mapa <-st_make_grid(sfdf,cellsize = c(qualidade,qualidade)) %>%
  st_as_sf() 
plot(estrutura_mapa)












