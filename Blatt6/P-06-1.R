# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser

# 1.
log2 <- function(x) {
logb(x, base = 2)
}

library(rlang)
log2_shift <- function(x) {
base::log2(x + 1)
}

# 2.
sapply(c("sum","line","eigen.values","det","plot","inverse"),
       exists, where = asNamespace("base"), inherits = FALSE)

# 3. 
x1 <- 1
f1 <- function() {
x2 <- 4
x1 + x2
}
f1()
## 5
e1 <- env(x1 = 0)
with(e1, f1())
## 5
x1 <- 6
f1()
## 10
with(e1, f1())
## 10
x2 <- 10
f2 <- function() {
y <- 5
y + #ToDo
}
f2()
## 15
x2 <- 1
f2()
## 6
e2 <- env(x2 = 3)
with(e2, f2())
## 8

# 4.
env_bind_new <- function(e = NULL, vars) {
  for (nm in names(vars)) {
    if (exists(nm, envir = e, inherits = FALSE)) {
      warning(paste0("'", nm, "' already exists in ", format(e), ". Cannot replace it!"))
    } else {
      env_bind(e, !!nm := vars[[nm]])
    }
  }
}
e1 <- env(x = 1, y = 2, z = 3)
v <- c(x = -1, y = -9, w = 4)
env_bind_new(e1, v)
## Warning: 'x' already exists in environment at 0x563e78c8a880. Cannot replace it.
## Warning: 'y' already exists in environment at 0x563e78c8a880. Cannot replace it.
e1$x
## [1] 1
e1$w
## [1] 4

# 5. 
library(rlang)
f <- function() {
  p <- sample(1:100, size = 1)
  q <- sample(1:100, size = 1)
  assign("ef", environment(), envir = parent.env(environment()))
  return(p*q)
}
#here
pq <- f()
pq
## 570

library(stringr)
str_glue("The product of {ef$p} and {ef$q} is {ef$p * ef$q}.")




