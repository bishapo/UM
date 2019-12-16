#Autor: Mateusz Bieniek

#funkcja obliczajaca odleglosc euklidesowa
euclid <- function(x1,x2)
{
  return(sqrt(rowSums((x1-x2)^2)))
}

rand_points <- function(data, k)
{
  return(sample(1:dim(data)[1],k))
}

#funkcja znajdujaca maksymalnie od siebie oddalone punkty
max_distance <- function(data, k)
{
  n <- dim(data)[1]
  compare <- matrix(0,n,n)
  for (i in 1:n) # od 1 do n punktow
  {
    for (j in 2:n) # od 2 do n
    {
      compare[i,j] <- euclid(data[i,],data[j,]) #pierwszy punkt z drugim, pierwszy z trzecim itd
    }
  }
  pair <- which(compare==max(compare), arr.ind = TRUE)[1,]
  #po wyborze najbardziej oddalonej od siebie pary zostaja wybrane (jesli to konieczne) kolejne punkty
  #pierwszenstwo ma punkt najbardziej oddalony od ktoregos z pary
  compare1 <- matrix(0, 2, n)
  compare1[1,] <- compare[pair[1],] 
  compare1[1,pair[2]] <- 0
  compare1[2,] <- compare[pair[2],] 
  compare1[2,pair[1]] <- 0
  compare1 <- colSums(compare1)
  
  result <- c(pair)
  if(k==2) return(result)
  result[1:2] <- c(pair)
  
  for (l in 1 : (k-2))
  {
    result[l+2]<-which(compare1==max(compare1), arr.ind = TRUE)
    compare1[result[l+2]] <- 0
  }
  return(result)
}

k_means_test <- function(data, k, max_iter, fn, dim, method, selection, punishment)
  #k - liczba klastrow, fn - funkcja celu, dim - wymiar, method - rand lub max_distance
  #selection - choice lub random, punishment - yes lub no
{
  # losowanie punktow poczatkowych
  cat("\ndane wejsciowe:\n")
  print(data)
  
  if(method == "max_distance")
  {
    cat("\nwybrano metode najbardziej oddalonych punktow poczatkowych\n")
    points <- max_distance(data,k)
  }
  else
  {
    cat("\nwybrano metode losowego wyboru punktow poczatkowych\n")
    points <- rand_points(data,k)
  }
  clusters <- data[points,1:dim]
  data[,dim+1]<-c(0)
  cat("\nklastry poczatkowe \n")
  print(clusters)
  cat("\nklastry zostaly wybrane sposrod punktow: \n")
  print(data)
  
  #do kazdego klastra zostana przypisane najblizej lezace punkty
  #dla kazdego punktu obliczane sa odleglosci od wszystkich klastrow
  #punkt przypisywany jest do najblizszego klastra jesli spelnia warunki
  clustlist = list() #inicjalizacja listy na macierze klastrow
  w = list()
  s = list()
  pun = list()
  for (i in 1:max_iter) 
  {
    for (j in 1:k) #petla po wszystkich klastrach
    {
      # macierz klastrow
      clustlist[[j]] <- t(matrix(clusters[j,1:dim],dim,dim(data)[1]))
      w[[j]]<-euclid(data[,1:dim],unlist(clustlist[[j]]))
      if(punishment == "yes")
      {
        c <- matrix(unlist(clustlist[[j]]),dim(data)[1],dim)
        c[is.na(c)] <- 20
        pun[[j]] <- fn(matrix(unlist(data[,1:2]),dim(data)[1],dim)) > fn(c)
      }
    }
    #jesli punkt poprawia srodek klastra to odleglosc zostaje bez zmian
    #jesli pogarsza, to kara jest zwiekszenie odleglosci o 1
    y<-matrix(unlist(w),dim(data)[1],k) #macierz odleglosci punktow od klastrow
    if(punishment == "yes")
    {
      pn<-matrix(as.integer(unlist(pun)),dim(data)[1],k)
      y<- y + pn*3
    }
    
    cat("\nMacierz odleglosci punktow od klastrow\n")
    print(y)
    
    for (l in 1:dim(data)[1]) 
    {
      near <- which.min(y[l,]) # ktoremu klastrowi najblizszy jest dany punkt
      # obliczenie nowego klastra po dodaniu tego punktu
      
      # algorytm nie bierze pod uwage punktow, ktore sa juz w danym klastrze
      if(selection == "classic")
      {
        data[l,dim+1]<-which.min(y[l,]) # przypisanie numeru klastra
      }
      if((i == 1 | (data[l,dim+1] !=  near)) & selection != "classic")
      {
        
        ncenter <- ((clusters[near,] + data[l,1:D])/2)
        ncenter <-unlist(ncenter)
        
        # warunek dodania punktu do klastra
        
        if (fn(ncenter) <= fn(unlist(clusters[near,])))
        {
          data[l,dim+1]<-which.min(y[l,]) # przypisanie numeru klastra
        }
        else
        {
          if(selection == "random")
          {
            data[l,dim+1]<-sample(k,1) #w.p.p przypisanie losowego klastra
          }
          else #"choice"
          {
            data[l,dim+1]<-"nieuzywany" #w.p.p przypisanie nieuzywanego klastra
          }
          
        }  
      }
    }
    colnames(data)[dim+1] <- "klaster"
    # wybor nowych srodkow klastrow
    
    old_clusters <- clusters
    for (m in 1:k) 
    {
      u = data[data$klaster == m,]
      
      clusters[m,] <- colSums(u[1:dim])/dim(u)[1]
      
    }
    
    #jesli klastry nie zmieniaja sie nastepuje zatrzymanie algorytmu
    if(sum(old_clusters - clusters, na.rm=TRUE) == 0 )
    {
      cat("wykonane iteracje: ", i)
      cat("\nkoncowe klastry\n")
      print(clusters)
      
      #moze sie zdarzyc, iz powstanie pusty klaster
      if(is.nan(sum(clusters)))
      {
        print("przynajmniej jeden z klastrow jest pusty")
      }
      
      data2 <- list(data, clusters)
      return(data2)
    }
    print("nowe klastry")
    print(clusters)
    cat("\nPunkty wraz z przypisanymi im klastrami po ", i)
    cat(" iteracjach\n")
    print(data)
    data2 <- list(data, clusters)
  }
  return(data2)
}
