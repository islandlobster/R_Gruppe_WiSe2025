# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser

library(rlang)

e1 <- rlang::env(a=1,)
y <- 5
#Schritt 1

f <- function(x){print(x+a)}
#Schritt 2

environment(f) <- e1
# Schritt 3

#After step 2 f(y) will throw an error, since there is no variable 'a' in the global environment
#After step 3, however, f(y) will evaluate to 6, since it is now enclosing e1, rather than the global environment