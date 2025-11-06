# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser

#helper
?diag
body(diag)
body(`diag<-`)
head(`diag<-`)

empty_vec_like <- function(x) {
  switch(typeof(x),
         "logical"   = logical(0),
         "integer"   = integer(0),
         "double"    = numeric(0),
         "character" = character(0),
         "complex"   = complex(0),
         "raw"       = raw(0),
         NULL)
}



n_diag <- function(M, o=0){
  #check for matrix
  stopifnot("M must be matrix"=is.matrix((M)))
  #check for offset (always needed prio over new_char)
  stopifnot("offset must be numeric"=is.numeric(o))
  
  #get dimension-boundaries
  r <- dim(M)
  x <- r[1]
  y <- r[2]
  
  #check boundaries and then manually find n_diag by offset o - dont forget to loop <- handles diag()
  empty_vec_like <- function(x) {
    switch(typeof(x),
           "logical"   = logical(0),
           "integer"   = integer(0),
           "double"    = numeric(0),
           "character" = character(0),
           "complex"   = complex(0),
           "raw"       = raw(0),
           NULL)
  }
  o_diag <- empty_vec_like(M)
  
  
  if(o<0){
    for (cur in 1:min(r)) {
      if(((cur+abs(o))<=y)&(cur<=x)){o_diag <- c(o_diag, M[cur, cur+abs(o)])}
    }
  }else{
    for (cur in 1:min(r)) {
      if((cur<=y)&((cur+o)<=x)){o_diag <- c(o_diag, M[cur+o, cur])}
    }
  }
  
  #return n_diag only when not n_diag(M) <- x
  return(o_diag)
}


  
#setter and getter cause thats how diag does it
`n_diag<-`<- function(M, o=0, value){
  #check for matrix
  stopifnot("M must be matrix"=is.matrix((M)))
  #check for offset (always needed prio over new_char)
  stopifnot("offset must be numeric"=is.numeric(o))
  
  #check value
  stopifnot("value must be atomic"=is.atomic(value))
  if(length(value)==1){value <- rep(value, length(n_diag(M, o)))}
  stopifnot("length of value must fit to n_diag"=length(value)==length(n_diag(M, o)))
  
  #get dimension-boundaries
  r <- dim(M)
  x <- r[1]
  y <- r[2]
  
  #if x exists -> set n_diag and return M - supposed to handle n_diag(M) <- x
  if(o<0){
    for (cur in 1:min(r)) {
      if(((cur+abs(o))<=y)&(cur<=x)){M[cur, cur+abs(o)] <- value[cur]}
    }
  }else{
    for (cur in 1:min(r)) {
      if((cur<=y)&((cur+o)<=x)){M[cur+o, cur] <- value[cur]}
    }
  }
  
  return(M)
}  




#tests
M <- matrix(c(1:12), 3, 4)
M
diag(M)
diag(M) <- 3

n_diag(M, -1)
n_diag(M, 1)
n_diag(M, 0)
n_diag(M, 4)
n_diag(M, 0) <- 9
n_diag(M, -1) <- c(1:2)
n_diag(M, -1) <- c(4:2)
n_diag(M, 1) <- c(7:9)

