# Получим ДКП матрицу

N = 8
a <- c(1/sqrt(N),1/sqrt(N),1/sqrt(N),1/sqrt(N),1/sqrt(N),1/sqrt(N),1/sqrt(N),1/sqrt(N))
a

b <- matrix(0, nrow = 8, ncol = 8)
for(i in 0:nrow(b)) {
  for(j in 0:ncol(b)) {
    b[i, j] <- sample(sqrt(2/N)*cos((2*j+1)*i*pi/(2*N)), 1, replace=TRUE)
  }
}
b

DCT <- rbind(a,b)
DCT



library(jpeg)
####### read photo #######
photo <- readJPEG("1.jpeg")
####### Creating separated matrix for every RGB color scale #######
r <- photo[,,1]
g <- photo[,,2]
b <- photo[,,3]
####### Introducing PCA method #######
r.pca <- prcomp(r, center = F)
g.pca <- prcomp(g, center = F)
b.pca <- prcomp(b, center = F)
rgb.pca <- list(r.pca, g.pca, b.pca)
for (i in seq.int(3, round(nrow(photo) - 10), length.out = 10)) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pca.img, paste('1photo_', round(i,0), '_components.jpg', sep = ''))
}

View(photo)
View(r)
View(g)
View(b)
length(photo)
dim(photo)
