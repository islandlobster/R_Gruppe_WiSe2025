# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser

library(tidyverse)

#DONE 1: load tibbles from files
patients <- read_csv("patients.csv", col_types = cols(`start of treatment` = col_date("%Y-%m-%d")))
measurements <- read_csv("measurements1.csv", col_types = cols(dated = col_date("%Y-%m-%d")))

#DONE 2: reformat measurements
measurements <- pivot_wider(measurements, names_from = type, values_from = value, values_fill = -1)

#DONE 3: replace NAs
#See previous step

#DONE 4: create tibble incompletePatients
temp1 <- measurements %>% dplyr::group_by(pid) %>% dplyr::summarize(expl=sum((syst==-1)|(diast==-1)), impl=62-n())
incompletePatients <-full_join(patients, temp1,by = join_by(id == pid))


#DONE 5: extract tibble nMissing
nMissing <- incompletePatients %>% group_by(group) %>% summarize(missing=sum(expl+impl))

#TODO 6: more tibbles...


#TODO 7: generate effectSizes tibble


#TODO 8*: repeat 6&7 with data of incompletePatients

