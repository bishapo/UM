#Autor: Mateusz Bieniek

library(contoureR)
library(ggplot2)

counterplot <- function(dane, fn)
{

  points <- dane[[1]]
  clust <- dane[[2]]
  colnames(points) <- c("x","y","klaster")

  b<-unlist(points[,3])
  b<-replace(b,b==1,'pierwszy')
  b<-replace(b,b==2,'drugi')
  b<-replace(b,b==3,'trzeci')
  b<-replace(b,b==4,'czwarty')
  b<-replace(b,b==5,'piaty')
  b<-replace(b,b==6,'szosty')
  b<-replace(b,b==7,'siodmy')
  b<-replace(b,b==8,'osmy')
  b<-replace(b,b==9,'dziewiaty')
  b<-replace(b,b==10,'dziesiaty')
  
  points[,3]<-b

  #Do wykreslenia poziomic
  
  a1    = -10; b1 = +10; n  = 150
  x    = runif(n*n,a1,b1)
  y    = runif(n*n,a1,b1)
  df   = data.frame(x,y)
  m<-matrix(unlist(df),22500,2)
  df$z = fn(m)
  
  #Limit the Domain
  df.sub = subset(df,x^2 + y^2 < 10000)
  
  #Execute the Contouring Routine
  df.cnt = getContourLines(df.sub,nlevels=20)
  
  #Now Plot the Data
  ggplot() + geom_point(data = points, aes(x, y, color = klaster))  +
  geom_path(data=df.cnt,aes(x,y,group=Group)) + 
  scale_shape_manual(values = c(1:10)) +
  theme_bw() + geom_point(data = clust, aes(par1, par2, shape = row.names(clust)), size=3)
}