#Autor: Mateusz Bieniek

library(cec2013)
library(DEoptim)
source("my_kmeans.R")
source("counterplots.R")

#WYMIAR 
D <- 2
#NUMER FUNKCJI: 1...28
I <- 11
#ILOSC KLASTROW
k <- 3

lower <- rep(-10, D)
upper <- -lower

fn=function(x){cec2013(I, x)}

outDEoptim <- DEoptim(fn=function(x){cec2013(I, x)}, lower, upper, 
                      DEoptim.control(itermax = 100, storepopfrom = 1, storepopfreq = 2))

tab <- data.frame(outDEoptim$member$storepop[[4]])

#a<-k_means(iris[, 3:4], 3, 100,2)

wynik<-k_means(tab, 3, 100, fn ,D, method ="rand", selection = "choice", punishment ="no")

#irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)


#irisCluster$cluster <- as.factor(irisCluster$cluster)
#ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point()

counterplot(wynik, fn)

#p2 <- ggplot() + geom_point(data = a, aes(x, y, colour = klaster))

