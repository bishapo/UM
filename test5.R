#Autor: Mateusz Bieniek

library(cec2013)
library(DEoptim)
source("my_kmeans.R")
source("counterplots.R")

#WYMIAR 
D <- 2
#NUMER FUNKCJI: 1...28
I <- 11

lower <- rep(-10, D)
upper <- -lower

fn=function(x){cec2013(I, x)}

outDEoptim <- DEoptim(fn=function(x){cec2013(I, x)}, lower, upper, 
                      DEoptim.control(itermax = 100, storepopfrom = 1, storepopfreq = 2, NP = 20))
tab <- data.frame(outDEoptim$member$storepop[[4]])


N<-10
wynik2 <- rep(0,10)
wynik3 <- rep(0,10)
wynik4 <- rep(0,10)
wynik5 <- rep(0,10)
wynik6 <- rep(0,10)
wynik7 <- rep(0,10)
wyniki <- matrix(0,N,7)
for(i in 2:N)
{
  for( l in 1:10)
  {
    wynik<-k_means(tab, i, 100, fn ,D, method ="rand", selection = "classic", punishment = "no")
    wynik0<-k_means(tab, i, 100, fn ,D, method ="rand", selection = "choice", punishment = "no")
    wynik1<-k_means(tab, i, 100, fn ,D, method ="rand", selection = "Random", punishment = "no")
    wynik8<-k_means(tab, i, 100, fn ,D, method ="rand", selection = "classic", punishment = "yes")
    wynik9<-k_means(tab, i, 100, fn ,D, method ="rand", selection = "choice", punishment = "yes")
    wynik10<-k_means(tab, i, 100, fn ,D, method ="rand", selection = "Random", punishment = "yes")
    wynik2[l]<-mean(fn(na.exclude(matrix(unlist((wynik[[2]])),i,D))))
    wynik3[l]<-mean(fn(na.exclude(matrix(unlist((wynik0[[2]])),i,D))))
    wynik4[l]<-mean(fn(na.exclude(matrix(unlist((wynik1[[2]])),i,D))))
    wynik5[l]<-mean(fn(na.exclude(matrix(unlist((wynik8[[2]])),i,D))))
    wynik6[l]<-mean(fn(na.exclude(matrix(unlist((wynik9[[2]])),i,D))))
    wynik7[l]<-mean(fn(na.exclude(matrix(unlist((wynik10[[2]])),i,D))))
  }
  wyniki[i-1,1]<-mean(na.exclude(wynik2))
  wyniki[i-1,2]<-mean(na.exclude(wynik3))
  wyniki[i-1,3]<-mean(na.exclude(wynik4))
  wyniki[i-1,4]<-mean(na.exclude(wynik5))
  wyniki[i-1,5]<-mean(na.exclude(wynik6))
  wyniki[i-1,6]<-mean(na.exclude(wynik7))
  print(i)
}

wyniki <- data.frame(wyniki)
wyniki <- wyniki[1:N-1,]
wyniki <- data.frame(wyniki)
wyniki[,7] <- c(2:N)


colnames(wyniki) <- c("Uzyskane_minimum","Choice","Random","pClassic","pChoice","pRandom","ilosc_klastrow")

p3<-ggplot(data = wyniki, aes(x=ilosc_klastrow)) +
  geom_line(aes(y=Uzyskane_minimum, color = "Classic")) +
  geom_line(aes(y=Choice,color = "Choice")) +
  geom_line(aes(y=Random, color = "Random")) +
  geom_line(aes(y=pClassic,color = "pClassic")) +
  geom_line(aes(y=pChoice,color = "pChoice")) +
  geom_line(aes(y=pRandom, color = "pRandom")) +
  # geom_point() +
  scale_x_discrete(limits=c(1:N))

counterplot(wynik, fn)

