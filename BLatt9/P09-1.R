# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser

`%->%` <- function(lhs, rhs) {
  assign(deparse(substitute(rhs)), lhs, envir = parent.frame())
  rhs
}


#test

c(4,5) %->% x1 |>
sum() %->% x2 |>
sqrt() -> res

cat(x1,"\n", x2,"\n", res,"\n")
