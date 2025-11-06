# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser

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
    if(any(!(M %in% c(1:k, NA)))) stop("Matrix cannot contain numbers larger than its number of rows")
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

#2 DONE: create size functions
size <- function(sud){
    return(attr(sud, 'size'))
}

`size<-` <- function(sud, value){
    attr(sud, 'size') <- value
    return(sud)
}

#3 DONE: create is_sudoku function
is_sudoku <- function(sud){
    if((!identical(class(sud),"sudoku")) || (length(size(sud))!=2) || ((prod(size(sud))^2)!=length(sud)) || !identical(typeof(sud),"integer") || !any(!(sud %in% 1:prod(size(sudoku))))) return(FALSE)
    return(TRUE)
}

#4 DONE: create is_pre_valid function
is_pre_valid <- function(M){
    #convert matrix to pseudo-vector if necessary
    if(is.matrix(M)){
        temp <- matrix(M, nrow=1)
    } else{
        temp <- M
    }
    #test if any duplicates are present except NA
    if(anyDuplicated(temp[which(!is.na(temp))])!=0) return(FALSE)
    return(TRUE)
}

#5 DONE: create p_table function
p_table <- function(n, m){
    #variables for convenience
    k = n*m
    temp1 = 1:k
    #get numbers in correct order
    temp2 = unlist(rep(split(temp1,ceiling(seq_along(temp1) / m)), each=m), use.names=FALSE)
    #repeat each number in correct amount
    temp3 = rep(temp2, each=n)
    res = matrix(temp3, nrow=k, ncol=k)
    return(res)
}

#6.1 DONE: create is_filled, is_valid, is_solved functions
is_filled <- function(sud){
    return(!anyNA(sud) && is_sudoku(sud))
}

#6.2 DONE: finish is_valid function
is_valid <- function(sud){
    if(!is_sudoku(sud)) return(FALSE)
    temp <- matrix(sud, nrow=prod(size(sud)))
    rows <- unlist(lapply(split(temp, row(temp)), 'is_pre_valid'), use.names=FALSE)
    cols <- unlist(lapply(split(temp, col(temp)), 'is_pre_valid'), use.names=FALSE)
    fields <- unlist(lapply(split(temp, p_table(size(sud)[1], size(sud)[2])), 'is_pre_valid'), use.names=FALSE)
    return(!(any(!rows) || any(!cols) || any(!fields)))
}

#6.3 DONE: test is_solved function
is_solved <- function(sud){
    return(is_filled(sud) && is_valid(sud))
}

#7 DONE: create is_sol_of function
is_sol_of <- function(S1, S2){
    if(!is_solved(S1) || !is_valid(S2) || length(S1)!=length(S2) || !identical(size(S1),size(S2))) return(FALSE)
    return(!any(!(sud1 == sud2 | is.na(sud2))))
}

#8 DONE: create non_valid_values function
non_valid_values <- function(sud){
    #sudoku is solved
    if(is_solved(sud)){
        print(NULL)
    } else{
        k <- prod(size(sud))
        M <- matrix(sud, nrow=k)
        #sudoku is not filled
        if(!is_filled(sud)){
            print("NAs at the following coordinates:")
            print(which(is.na(M), arr.ind=TRUE))
        }
        #sudoku is invalid
        if(!is_valid(sud)){
            rows <- unlist(lapply(split(M, row(M)), 'is_pre_valid'), use.names=FALSE)
            cols <- unlist(lapply(split(M, col(M)), 'is_pre_valid'), use.names=FALSE)
            fields <- unlist(lapply(split(M, p_table(size(sud)[1], size(sud)[2])), 'is_pre_valid'), use.names=FALSE)
            fieldMat <- matrix(fields, nrow=size(sud)[2])
            if(any(!rows)){
                print("Duplicates in the following rows:")
                print(which(!rows))
            }
            if(any(!cols)){
                print("Duplicates in the following columns:")
                print(which(!cols))
            }
            if(any(!fields)){
                print("Duplicates in the following fields:")
                print(fieldMat)
                print(which(!fieldMat, arr.ind=TRUE))
            }
        }
    }
}