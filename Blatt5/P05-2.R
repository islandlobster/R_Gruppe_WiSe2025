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
incompletePatients <- select(full_join(temp1, patients,by = join_by(pid == id)), pid, `first name`, `last name`, group, expl, impl)


#DONE 5: extract tibble nMissing
nMissing <- incompletePatients %>% group_by(group) %>% summarize(missing=sum(expl+impl))

#DONE 6: more tibbles...
completePatients <- filter(patients, !(id %in% incompletePatients$pid),  `start of treatment`>=as.Date("2024-07-06"), `start of treatment`<=as.Date("2024-08-15"))
temp2 <- inner_join(measurements, completePatients, by=join_by(pid==id)) %>% select(pid, dated, syst, diast, `start of treatment`)
preTemp <- filter(temp2, dated<`start of treatment`)
postTemp <- filter(temp2, dated>`start of treatment`+10)
preTemp <- preTemp %>% group_by(pid) %>% reframe(sPre=mean(syst), dPre=mean(diast))
postTemp <- postTemp %>% group_by(pid) %>% reframe(sPost=mean(syst), dPost=mean(diast))
workingSet <- inner_join(preTemp, postTemp, by = join_by(pid==pid))

#DONE 7: generate effectSizes tibble
temp3 <- inner_join(workingSet, completePatients, by=join_by(pid==id)) %>% select(pid, group, sex, sPre, dPre, sPost, dPost)
effectSizes <- temp3 %>% group_by(group, sex) %>% reframe(deltaSysMean=sPost-sPre, deltaDiaMean=dPost-dPre, deltaDistMean=(sPost-sPre)-(dPost-dPre))
