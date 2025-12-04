# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser

solve_difference_eq <- function(f, x, n){
    xi <- as.vector(x)
    N <- as.vector(n)
    m <- length(xi)
    if(!is.function(f)) stop("f is not a function")
    if(!is.numeric(xi)) stop("x cannot be coerced to numeric")
    if(!is.numeric(N)) stop("n cannot be coerced to numeric")
    if(length(N) != 1) stop("n must be a single number")
    k <- N + m
    temp <- as.vector(f(xi))
    if(!is.numeric(temp)) stop("output of f cannot be coerced to numeric")
    if(length(temp) != 1) stop("f must return a single number")
    res <- xi
    while(length(res)<k) res <- append(res, f(res[(length(res)-m+1):length(res)]))
    print(res)
}