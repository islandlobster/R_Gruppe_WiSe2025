# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser
library(stringr)
inputDates <- list("13.05.2024", "3.12.23", "2. September 2012", 
                   "21/5, 2024", "Mi., 7. Aug. 2024")

monthsDE <- c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", 
              "August", "September", "Oktober", "November", "Dezember")

str_to_date <- function(sDate){
    info <- str_subset(str_split(sDate, "[:punct:]|[:space:]")[[1]], "[:alnum:]")
    n <- length(info)
    sDateDay <- info[n-2]
    sDateMonth <- info[n-1]
    sDateYear <- info[n]
    countDay <- str_count(sDateDay)
    countMonth <- str_count(sDateMonth)
    countYear <- str_count(sDateYear)
    #DONE: create day substring
    if(countDay==2){
        resDay <- sDateDay
    }else if(countDay==1){
        resDay <- str_c("0", sDateDay)
    }
    #DONE: create month substring
    if(countMonth==2){
        resMonth <- sDateMonth
    }else if(countMonth==1){
        resMonth <- str_c("0", sDateMonth)
    }else{
        tempMonth <- str_which(monthsDE, sDateMonth)
        countTemp <- str_count(tempMonth)
        if(countTemp==2){
            resMonth <- tempMonth
        }else if(countTemp==1){
            resMonth <- str_c("0", tempMonth)
        }
    }
    #DONE: create year substring
    if(countYear==4){
        resYear <- sDateYear
    }else if(countYear==2){
        resYear <- str_c("20", sDateYear)
    }
    #DONE: combine substrings
    res <- str_c(resYear, resMonth, resDay, sep="-")
    return(as.Date(res))
}

str(lapply(inputDates, str_to_date))