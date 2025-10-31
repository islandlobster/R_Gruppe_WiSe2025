# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser

matrix_man <- function(v, nrow=NULL, ncol=NULL, row_names=NULL, col_names=NULL) {...
  
  #handle len 1 exception
  if(length(v)==1 & (!is.null(nrow) & !is.null(ncol))){tmp <- v[1]; v <- rep(tmp, times= nrow*ncol); rows <- nrow} #create vctor of appropriate length
  #handle elementary dimensions 
  stopifnot("either nrow or ncol must be provided" = !is.null(nrow) | !is.null(ncol))
  if(is.null(nrow) & !is.null(ncol)){nrow = length(v)/ncol}
  if(is.null(ncol) & !is.null(nrow)){ncol = length(v)/nrow}
  rows <- nrow
  cols <- ncol
  stopifnot("input vector does not fit dimensions" = length(v)==(rows*cols))
  
  
  #create dimensions
  dim(v) <- c(rows, cols)
  
  #names
  if(!is.null(row_names)){
    stopifnot("length of row names does not fit number of rows" = length(row_names)==rows)
    rownames(v) <- row_names
  }
  if(!is.null(col_names)){
    stopifnot("length of column names does not fit number of columns" = length(col_names)==cols)
    colnames(v) <- col_names
  }
  
  return (v)
}

#tests
matrix_man(1:6)
matrix_man(1:6, ncol=1)
matrix_man(1:6, 3)
matrix_man(1:6, ncol=3)
matrix_man(1:6, ncol=6)
matrix_man(1:6, ncol=4)
matrix_man(1:6, nrow=2)
matrix_man(1:6, nrow=7)
matrix_man(1:6, ncol=2, nrow=2)
matrix_man(1:6, ncol=2, nrow=3)
matrix_man(1:6, ncol=2, nrow=1)
matrix_man(0, ncol=3, nrow=2)
matrix_man(1:6, ncol=3, col_names=LETTERS[1:3])
matrix_man(1:6, ncol=3, col_names=LETTERS[1:2])
matrix_man(1:6, ncol=3, row_names=letters[24 + 1:2])
matrix_man(1:6, ncol=3, col_names=LETTERS[1:3], row_names=letters[24 + 1:2])