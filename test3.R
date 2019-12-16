#Autor: Mateusz Bieniek

library(cec2013)
library(DEoptim)
source("my_kmeans.R")
source("counterplots.R")

#WYMIAR 
D <- 10
#NUMER FUNKCJI: 1...28
I <- 11

lower <- rep(-10, D)
upper <- -lower

fn=function(x){cec2013(I, x)}


N<-50

wyniki <- rep(0,N)
for(i in 1:N)
{
  outDEoptim <- DEoptim(fn=function(x){cec2013(I, x)}, lower, upper, 
                        DEoptim.control(itermax = 100, storepopfrom = 1, storepopfreq = 2))
  
  
  tab <- data.frame(outDEoptim$member$storepop[[4]])
  wynik<-k_means(tab, 3, 100, fn ,D, method ="rand", selection = "choice")
  wyniki[i]<-mean(fn(na.exclude(matrix(unlist((wynik[[2]])),3,D))))
  print(i)
}

wyniki <- data.frame(wyniki)
wyniki[,2] <- c(1:N)

ggplot(data = wyniki, aes(wyniki, color = "red")) + geom_histogram()

counterplot(wynik, fn)

