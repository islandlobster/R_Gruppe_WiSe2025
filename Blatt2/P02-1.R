# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser

#namen und cond + vertehen was singular beudetet

lsq <- function(M, y, cond = 1e6) {
  # ToDo: ... checks for appropriate arguments ...
  
  #M und y haben positive Längen (sind also nicht leer).
  stopifnot("M must have positive length" = length(M)>0)
  stopifnot("y must have positive length" = length(y)>0)
  
  #M ist eine numerische (also Typ integer oder double) Matrix.
  #y ist numerischer atomarer Vektor (oder numerische Matrix mit einer Spalte).
  stopifnot("M must be a numeric matrix" = is.matrix(M) & (is.double(M) | is.integer(M)))
  stopifnot("y must have numeric type" = is.atomic(y) & (is.double(y) |is.integer(y)))
  
  #Keines der Argumente enthält N A-, N aN - oder Inf -werte.
  stopifnot("M must not have NA values" = !all(is.na(M)))
  stopifnot("y must not have NA values" = !all(is.na(y)))
  stopifnot("M must not have any NaN values" = !any(is.nan(M)))
  stopifnot("y must not have any NaN values" = !any(is.nan(y)))
  stopifnot("M must not have any Inf values" = !any(is.infinite(M)))
  stopifnot("y must not have any Inf values" = !any(is.infinite(y)))
  
  #Die Dimensionen sind kompatibel, d.h., M T y kann berechnet werden.
  stopifnot("Dimensions of M and y must be compatible (scheme: number of rows of M = length of y)" = nrow(M) == length(y))
  
  #Die Konditionszahl von M T M , vergleichen Sie diese mit dem Schwellenwert cond und geben Sie ggf.einen Fehlermeldung aus
  #cat("Condition number of M^T * M:", kappa(t(M)%*%M), "\n")
  stopifnot("conidtion number of M^T * M exceeds cond" = !(cond < kappa(t(M)%*%M)))
  
  A <- t(M)%*%M
  return(solve(A, t(M)%*%y))
}


#tests
M <- matrix(c(1:6), nrow=3, ncol=2)
y <- c(7:9)
lsq(M, y)

M2 <- matrix(c(1:12), nrow=4, ncol=3)
M2
y2 <- c(1:4)
y2
lsq(M2, y2)
