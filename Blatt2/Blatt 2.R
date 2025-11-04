#Aufgabe 1
lsq <- function(M, y, cond = 1e6) {
  # ToDo: ... checks for appropriate arguments ...
  #1.
  if (length(M) == 0) stop("Matrix M darf nicht leer sein.")
  if (length(y) == 0) stop("Vektor y darf nicht leer sein.")
  #2
  if (!is.matrix(M)) stop("M muss eine Matrix sein.")
  if (!is.numeric(M)) stop("M muss numerisch sein")
  #3
  if(!((is.numeric(y) && is.atomic(y)) || (is.matrix(y) && ncol(y) == 1))) {
    stop("y muss ein numerischer atomarer Vektor oder eine numerische Matrix mit einer Spalte sein.")
  }
  #4
  if (any(!is.finite(M))) stop("M darf keine NA, NaN oder Inf Werte enthalten.")
  if (any(!is.finite(y))) stop("y darf keine NA, NaN oder Inf Werte enthalten.")
  #5
  if (!(nrow(M) == length(y))) stop("Die Dimensionen sind nicht kompatibel")
  
  A <- t(M)%*%M
  #6
  k <- kappa(A)
  if (k > cond) {
    stop(stop(paste("Das Gleichungssystem ist schlecht konditioniert:",
                    "kappa =", k, ">", cond))
    )
  }
  return(solve(A, t(M)%*%y))
}



M2 <- matrix(c(1:12), nrow=4, ncol=3)
M2
y2 <- c(1:4)
lsq(M2, y2, 1e20)
