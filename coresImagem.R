library(jpeg)
library(magrittr)

img <- readJPEG("C:/Users/winseven/Pictures/homer-simpso.jpg")

plot(as.raster(img))

img_matrix <- apply(img, 3, as.numeric)

km <- kmeans(img_matrix, centers = 6)

library(tibble)
library(dplyr)
img_df <- tibble(
  r = img_matrix[,1], 
  g = img_matrix[,2], 
  b = img_matrix[,3],
  cluster = km$cluster
)

centroides <- img_df %>%
  group_by(cluster) %>%
  summarise_all(mean)

centroides

centroides <- centroides %>%
  mutate(cor = rgb(r, g, b))

centroides$cor

exibir <- function(x) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))
  
  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")
}

exibir(sort(centroides$cor))