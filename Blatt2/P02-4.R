# Paul Proft, Lionel Assick, Kira Nname, Emma Nname

#nur noch namen

collatz <- function(x, max_iter=10^4){
    #generate the output sequence
    res = rep(as.integer(c(0)), times=max_iter)
    #vectors, sequences, ... - YOU SHALL NOT PASS!!!
    stopifnot(length(x)==1)
    #add first entry to sequence
    res[1] = as.integer(x)
    #abort if not convertable
    stopifnot(!anyNA(res))
    #generate output length
    n = as.integer(NA)
    for(i in 1:(max_iter-1)){
        #check for the end or overflow
        if(res[i]==1 | anyNA(res)){
            n = i
            break
        }
        #If these numbers get any bigger, my fear of heights
        #will make me Collatz!
        if((res[i]%%2)==0){
            res[(i+1)] = as.integer(res[i]/2)
        } else{
            res[(i+1)] = as.integer(3*res[i]+1)
        }
    }
    #trim sequence if necessary
    if(!is.na(n)){
        res = as.integer(res[1:n])
    }
    return(list(seq_out = res, len_out = n))
}

str(collatz(837799))
#Bonus: To use a german saying: "This is the drop,
#that makes the barrel OVERFLOW"