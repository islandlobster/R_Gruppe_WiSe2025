# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser


`%o%` <-function(f, g){
  function(...){
    f(g(...))
  }
}




#tests
mean_c <- mean%o%c
mean_c(1,2)

mean_seq <- mean%o%seq
mean_seq(1,5)
