# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser


#Subtask: 1)
#f is function R->R aka:
#vector<double, n> f(vector<double, n> x)
midpoint <- function(f, a, b){
  stopifnot("Midpoint: f must be a function R->R"=is.function(f))
  stopifnot("Midpoint: a and b must be numeric"=is.numeric(a), is.numeric(b))
  stopifnot("Midpoint: a and b must have the same length"=length(a) == length(b))
  stopifnot("Midpoint: a must be less than b"=a < b)
  
  n<-length(a)
  Area <- numeric(n)
  for(i in 1:n) {
    Area[i] <- (b[i] - a[i]) * (f((a[i] + b[i]) / 2))
  }
  return(Area)
}

trapezoid <- function(f, a, b){
  stopifnot("Trapezoid: f must be a function R->R"=is.function(f))
  stopifnot("Trapezoid: a and b must be numeric"=is.numeric(a), is.numeric(b))
  stopifnot("Trapezoid: a and b must have the same length"=length(a) == length(b))
  stopifnot("Trapezoid: a must be less than b"=a < b)
  
  n<-length(a)
  Area <- numeric(n)
  for(i in 1:n){
    #find height: d * sin(alpha)
    A <- b[i]-a[i]
    C <- abs(f(a[i]) - f(b[i]))
    #unequal edges/2 * height
    Area[i] <- A/2*(f(a[i])+f(b[i]))
  }
  return(Area)
}

#tests for 1
midpoint(function(x) x, (0:4)*2, (1:5)*2)
## [1] 2 6 10 14 18
midpoint(sin, (0:4)/2*pi, (1:5)/2*pi)
##[1] 1.110721 1.110721 -1.110721 -1.110721 1.110721
trapezoid(function(x) x, (0:4)*2, (1:5)*2)
## [1] 2 6 10 14 18
trapezoid(sin, (0:4)/2*pi, (1:5)/2*pi)
## [1] 0.7853982 0.7853982 -0.7853982 -0.7853982 0.7853982




#Subtask: 2)
#function-Objekt(muss ich mich dran gewöhnen), xcord, xcord, amnt_partitions, midpoint or trapezoid
nc_integrate <- function(f, lower, upper, n, rule){
  stopifnot("nc_integrate: f must be a function R->R"=is.function(f))
  stopifnot("nc_integrate: lower and upper must be numeric"=is.numeric(lower), is.numeric(upper))
  stopifnot("nc_integrate: lower, upper and n must be of length 1"=length(lower) == 1, length(upper) == 1, length(n) == 1)
  stopifnot("nc_integrate: lower must be less than upper"=lower < upper)
  stopifnot("nc_integrate: n must be a positive integer"=is.numeric(n), n > 0, n == floor(n))
  stopifnot("nc_integrate: rule must be a function"=is.function(rule))
  
  total_area <- 0
  partition_width <- (upper - lower) / n
  a_vec <- seq(lower, upper - partition_width, by = partition_width)
  b_vec <- seq(lower + partition_width, upper, by = partition_width)
  areas <- rule(f, a_vec, b_vec)
  total_area <- sum(areas)
  return(total_area)
}

#tests for 2
#To-Do: Get pow running in R
nc_integrate(function(x) 3*x^2, 0, 2, n=4, rule = midpoint)
## [1] 7.875
nc_integrate(function(x) 3*x^2, 0, 2, n=4, rule = trapezoid)
## [1] 8.25
nc_integrate(sin, 0, pi, n=4, rule = midpoint)
## [1] 2.052344
nc_integrate(sin, 0, pi, n=4, rule = trapezoid)
## [1] 1.896119





#Subtask: 3)
newton_cotes <- function(coef){
  stopifnot("newton_cotes: coef must be numeric"=is.numeric(coef))
  
  rule <- function(f, a, b){
    stopifnot("newton_cotes rule: f must be a function R->R"=is.function(f))
    stopifnot("newton_cotes rule: a and b must be numeric"=is.numeric(a), is.numeric(b))
    stopifnot("newton_cotes rule: a and b must have the same length"=length(a) == length(b))
    stopifnot("newton_cotes rule: a must be less than b"=a < b)
    
    m <- length(coef)
    n <- length(a)
    
    Area <- numeric(n)
    
    for (i in 1:n) {
      if (m == 1) {
        # midpoint rule (special case)
        t <- (a[i] + b[i]) / 2
        Area[i] <- (b[i] - a[i]) * f(t)
        next
      }
      
      S <- 0
      for (j in 1:m) {
        w_j <- coef[j] / sum(coef)
        t_j <- a[i] + (j - 1) * (b[i] - a[i]) / (m - 1)
        S <- S + w_j * f(t_j)
      }
      Area[i] <- (b[i] - a[i]) * S
    }
    return(Area)
  }
  return(rule)
}

#tests for 3
nc_integrate(function(x) 3*x^2, 0, 2, n=4, rule = midpoint)
## [1] 7.875
nc_integrate(function(x) 3*x^2, 0, 2, n=4, rule = newton_cotes(1))
##[1] 7.875
nc_integrate(function(x) 3*x^2, 0, 2, n=4, rule = trapezoid)
##[1] 8.25
nc_integrate(function(x) 3*x^2, 0, 2, n=4, rule = newton_cotes(c(1,1)))
  ##[1] 8.25
simpson <- newton_cotes(c(1, 4, 1))
boole <- newton_cotes(c(7, 32, 12, 32, 7))
integrate(sin, 0, pi*11)
## 2 with absolute error < 0.00012
nc_integrate(sin, 0, pi*11, n=8, rule = midpoint)
## [1] 5.195247
nc_integrate(sin, 0, pi*11, n=8, rule = trapezoid)
## [1] -2.886325
nc_integrate(sin, 0, pi*11, n=8, rule = simpson)
## [1] 2.50139
nc_integrate(sin, 0, pi*11, n=8, rule = boole)
##[1] 1.985243







