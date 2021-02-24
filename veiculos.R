veiculos <- read.csv("C:/Users/winseven/Desktop/R/portfolio/dados/vehicles.csv",
                     na.strings = "",
                     stringsAsFactors = TRUE)

head(veiculos)

sumario <- summary(veiculos)

colnames(veiculos)

#write.csv(colnames(veiculos),"C:/Users/winseven/Desktop/R/portfolio/dados/dataframeNomes.csv")

summary(veiculos$county)

nomeVeiculos <- read.csv("C:/Users/winseven/Desktop/R/portfolio/dados/dataframeNomes.csv",
                     na.strings = "",
                     stringsAsFactors = TRUE)

colnames(veiculos) <- nomeVeiculos$xPortugues


