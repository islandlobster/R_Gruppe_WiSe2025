# Paul Proft, Lionel Assick


Monte_Carlo<-function(n){
  #check if n numerical
  if(!is.numeric(n)|n<1)print("n must be a positive integer of ideally large size")
  
  #declare Q and adjacent values(Area)
  #Q<-[-1,1]x[-1,1] -> Area=4
  Q_area<-4
  
  #generate n random coords in vector
  #list of length n of vectors of length 2 or 2 vectors of length n in list of len 2
  x<-runif(n, min=-1, max=1)
  y<-runif(n, min=-1, max=1)
  v<-list(x,y) #access by v[[1]][i], v[[2]][i] for ith coord pair
  
  #plot here
  plot(v[[1]], v[[2]], xlim=c(-1,1), ylim=c(-1,1), xlab="X", ylab="Y", asp=1, main=paste("Monte Carlo Simulation with n=", n))
  
  #check if coords in pi somehow -> delete if not (aka use filter of vector)
  #length of mathematical vector < radius(1)
  l<-numeric()
  for(i in 1:n){
    if(sqrt(v[[1]][i]^2+v[[2]][i]^2)<=1){l<-c(l, i)}
  }
  
  
  #set colours
  color<-ifelse((v[[1]][]^2+v[[2]][]^2)<=1, "red", "blue")
  points(v[[1]], v[[2]], col=color)
  
  #count new n
  k<-length(l)
  
  #apply MC-formula and return area guess of pi
  pi_est<-(k/n)*Q_area
  
  #print difference to pi 
  cat("Monte Carlo estimate: ", pi_est, "\n")
  cat("Difference to actual pi: ", abs(pi-pi_est), "\n")
}
#tests
Monte_Carlo(100)
Monte_Carlo(1000)
Monte_Carlo(10000)
Monte_Carlo(100000)
Monte_Carlo(1000000)