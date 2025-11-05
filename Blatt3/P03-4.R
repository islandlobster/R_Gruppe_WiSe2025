#TODO: names

#EXTRA: make the sudoku pretty
print.sudoku <- function(sud){
    print(matrix(sud, nrow=prod(attr(sud, 'size')), ncol=prod(attr(sud, 'size'))))
    cat("size: ", attr(sud, 'size'), '\n')
}

#1 DONE: create sudoku function and its validation of input
sudoku <- function(M, n=NA, m=NA){
    tempN <- as.integer(n)
    tempM <- as.integer(m)
    k <- as.integer(nrow(M))
    #check if input matrix is valid
    if(!is.matrix(M)) stop("First input needs to be a matrix")
    if(!is.numeric(M)) stop("Matrix needs to be numerical")
    if(k != ncol(M)) stop("Matrix needs to be quadratic")
    #check if n and m are valid
    if(anyNA(tempN)){
        if(anyNA(tempM)) stop("n or m need to be set as numbers")
        if(!(length(tempM) == 1)) stop("m needs to be a singular number")
        if(k%%tempM != 0) stop("Number of rows has to be divisible by m")
        #create sudoku
        res <- as.integer(M)
        tempN <- as.integer(k/tempM)
        class(res) <- 'sudoku'
        attributes(res) <- list(size = c(tempN, tempM))
        return(res)
    #more checks if n and m are valid
    } else{
        if(!(length(tempN) == 1)) stop("n needs to be a singular number")
        if(k%%tempN != 0) stop("Number of rows has to be divisible by n")
        #create sudoku
        res <- as.integer(M)
        #if m did not match - well, now it does!
        tempM <- as.integer(k/tempN)
        attributes(res) <- list(size = c(tempN, tempM))
        class(res) <- 'sudoku'
        return(res)
    }
}

#2 DONE: create size function
size <- function(sud){
    return(attr(sud, 'size'))
}

`size<-` <- function(sud, value){
    attr(sud, 'size') <- value
    return(sud)
}

#3 DONE: create is_sudoku function
is_sudoku <- function(sud){
    if((!identical(class(sud),"sudoku")) || (length(size(sud))!=2) || ((prod(size(sud))^2)!=length(sud))) return(FALSE)
    return(TRUE)
}

#4 DONE: create is_pre_valid function
is_pre_valid <- function(M){
    if(is.matrix(M)){
        temp <- matrix(M, nrow=1)
    } else{
        temp <- M
    }
    if(!anyDuplicated(temp[which(is.na(temp))])!=0) return(FALSE)
    return(TRUE)
}

#5 DONE: create p_table function
p_table <- function(n, m){
    k = n*m
    temp1 = 1:k
    temp2 = unlist(rep(split(temp1,ceiling(seq_along(temp1) / m)), each=m), use.names=FALSE)
    temp3 = rep(temp2, each=n)
    res = matrix(temp3, nrow=k, ncol=k)
    return(res)
}

#6 TODO: create is_filled, is_valid, is_solved functions

#7 TODO: create is_sol_of function

#8 TODO: create non_valid_values function

mat <- matrix(c(1,3,4,2,2,4,1,3,3,1,2,4,4,2,3,1), nrow=4, ncol=4)
sud <- sudoku(mat, 2)
sud
p_table(3,2)
p_table(2,3)
p_table(2,2)