# Paul Proft, Lionel Assick

primes_before<-function(x){
  if(!is.numeric(x)|x<1)print("x must be positive integer")
  
  if(x<=2)return("no primes before input")
  
  #solved CS way bc i cant figure out the macros
  f<-numeric()
  for(i in 2:x){
    b<-TRUE
    for(n in 2:max(i-1, 2)){
      if(i%%n == 0)b<-FALSE
    }
    if(b==TRUE){f<-c(f, i)}
  }
  return(f)
}
#tests

primes_before(9)
primes_before(2)
primes_before(85)
primes_before(3)