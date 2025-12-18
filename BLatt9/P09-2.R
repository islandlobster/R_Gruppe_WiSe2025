# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser

library(rlang)
library(stringr)

operators <- c("!", "&", "|", "<=", ">=", "==")
vars <- c("a", "b", "c", "d", "e", "f", "g")

#DONE 1: write validate_Prop function
validate_operator <- function(x) if(!(rlang::as_string(x) %in% operators)) stop("invalid operator")

validate_var <- function(x) if(!(rlang::as_string(x) %in% vars)) stop("invalid variable name")

validate_Prop <- function(x){
    if(class(x)!="Prop") stop("class must be Prop")
    for(i in 1:length(x)){
        if(is_symbol(x[[i]])){
            if(i > 1){
                validate_var(x[[i]])
            } else if(rlang::as_string(x[[i]]) == "("){
                if(is_call(x[[i+1]])) validate_Prop(structure(expr(!!x[[i+1]]), class="Prop"))
            } else{
                validate_operator(x[[i]])
            }
        } else if(is_syntactic_literal(x[[i]])){
            if(!((typeof(x[[i]]) == "double") && (x[[i]] %in% c(0,1)))) stop("invalid value")
        } else{
            validate_Prop(structure(expr(!!x[[i]]), class="Prop"))
        }
    }
    return(x)
}

#DONE 2: write Prop constructor
Prop <- function(x){
    temp <- enexpr(x)
    if(is_call(temp)){
        temp1 <- structure(temp, class="Prop")
    } else{
        temp1 <- structure(expr(((!!temp))), class="Prop")
    }
    res <- validate_Prop(temp1)
    if(identical(temp1, res)) return(res)
}

#DONE 3: overload print for Prop
print.Prop <- function(x){
    expr_str <- c("==", "<=", ">=", "&", "\\|", "!")
    unicode_str <- c("\u2194","\u2190","\u2192","\u2227","\u2228","\u00AC")
    res <- rlang::expr_text(x)
    for(i in 1:length(expr_str)) res <- str_replace_all(res, expr_str[i], unicode_str[i])
    print(res, quote = FALSE)
}

#TODO 4: write interpret function
#interpret <- function(p, vars, append = FALSE){}

#TODO 5: write is_tautology function
#is_tautology <- function(p){}
