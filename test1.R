#Autor: Mateusz Bieniek

library(cec2013)
library(DEoptim)
source("my_kmeans_test.R")
source("counterplots.R")

#WYMIAR 
D <- 2
#NUMER FUNKCJI: 1...28
I <- 11

lower <- rep(-5, D)
upper <- -lower

fn=function(x){cec2013(I, x)}

outDEoptim <- DEoptim(fn=function(x){cec2013(I, x)}, lower, upper, 
                      DEoptim.control(itermax = 100, storepopfrom = 1, storepopfreq = 2, NP=10))

tab <- data.frame(outDEoptim$member$storepop[[4]])


wynik<-k_means_test(tab, 2, 100, fn ,D, method = "rand", selection = "choice",punishment ="no")


counterplot(wynik, fn)


