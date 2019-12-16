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
wynik2 <- rep(999,10)
wyniki <- rep(0,N)
for(i in 1:N)
{
 for(l in 1:10)
  {
     wynik1<-k_means(tab, k, 100, fn ,D, method ="rand", selection = "choice", punishment ="no")
     wynik0 <- mean(fn(na.exclude(matrix(unlist((wynik1[[2]])),k,D))))
     print("minimum")
     print(wynik0)
    if((wynik0 < wynik2) | l == 1 )
     {
        wynik <- wynik1
     }
     wynik2<-mean(fn(na.exclude(matrix(unlist((wynik1[[2]])),k,D))))
  }
  wyniki[i]<-mean(fn(na.exclude(matrix(unlist((wynik[[2]])),k,D))))
  print(i)
}

wyniki <- data.frame(wyniki)
wyniki[,2] <- c(1:N)

ggplot(data = wyniki, aes(wyniki, color = "red")) + geom_histogram()

counterplot(wynik, fn)

