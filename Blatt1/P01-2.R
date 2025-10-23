# Paul Proft, Lionel Assick

vec_dist <-function(x, y= NULL, p=2){
  #screen inputs
  if(!is.vector(x))print("x must be a vector")
  if(!is.vector(y)& !is.null(y))print("x must be a vector or empty")
  if(!is.numeric(p)|p<1)print("p must be number >=1 or empty")
  #bc R doenst have types in method-declaration
  if((length(y)==1)&(length(x)>1)){p <- y
  y <- NULL}
  
  #logic:
  if(!is.null(y)){
    #y and p<inf
    if(is.finite(p)){s<- 0
      for(i in 1: length(x)) {s <- s + (abs(x[i]-y[i]))^p}
      return (s^(1/p))}else
    #y and p=inf
      {return (max(abs(x-y)))}
  }else{
    #no y and p<inf
    if(is.finite(p)){s<-0
      for(i in 1: length(x)) {s <- s + (abs(x[i]))^p}
      return (s^(1/p))}else
    #no y and p=inf
      {return (max(abs(x)))}
  }
}
#tests
vec_dist(c(4,3),c(1,-1)) 
vec_dist(c(4,1,-2),c(-1,2,-2), 1)
vec_dist(c(7,-3,42),c(-7,-15,39), Inf)
vec_dist(c(1,sqrt(3)))
vec_dist(c(3,0,-1), 1)
vec_dist(c(7,-39,22), Inf)
