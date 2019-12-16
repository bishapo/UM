#Autor: Mateusz Bieniek

library(cec2013)
library(DEoptim)
source("my_kmeans.R")
source("counterplots.R")

#WYMIAR 
D <- 30
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

N<-50

wyniki <- rep(0,N)
for(i in 1:N)
{
  wynik<-k_means(tab, k, 100, fn ,D, method ="rand", selection = "random", punishment ="no")
  wyniki[i]<-mean(fn(na.exclude(matrix(unlist((wynik[[2]])),k,D))))
  print(i)
}

wyniki <- data.frame(wyniki)
wyniki[,2] <- c(1:N)

ggplot(data = wyniki, aes(wyniki, color = "red")) + geom_histogram()

#counterplot(wynik, fn)

