# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser

library(tibble)

#implementation tibble
tibble_man <- function(x){
  x <- list(x)
  
  #zu tibble machen
  class(x) <- c("tbl_df", "tbl", "data.frame") 
  
  #rownames setzten
  n <- length(x[[1]])  # Anzahl der Zeilen
  attr(x, "row.names") <- .set_row_names(n)
  
  #to-do: assign names as tibbles do
  #names(x) <- sapply(x[], typeof)
  
  names(x) <- make.names("x")
  
  return(x)
}

#tests-tibble
x <- list(a<-1:3, b<-letters[1:3])

my_tb <- tibble_man(x)
my_tb
attributes(my_tb)

tb <- tibble(x)
tb
attributes(tb)

identical(tb, my_tb)

#ich glaube tibble funktioniert anders als aufgabenstellung: macht anstatt intuitiv_tibble(x) immerintuitiv_tibble(list(x))



#implementation factor
factor_man <- function(y){
  levels(y) <- c(sapply(y, as.character))
    
  class(y) <- "factor"
  
  return(y)
}



#tests-factor
y <- c(1:3)

my_fac <- factor_man(y)
my_fac
attributes(my_fac)

fac <- factor(y)
fac
attributes(fac)

identical(fac, my_fac)
