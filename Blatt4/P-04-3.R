# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser
library(dplyr)


#include library and data
library(tibble)
data(iris)
irisT <- as_tibble(iris)

#To-Do
#check for na values in the dataset
if(any(is.na(irisT))){sprintf("Dataset has NA")}else{sprintf("Dataset is valid")}
na_test

#print columns
attr <- as.character(colnames(irisT))
attr


#count num of rows and take vector of unique species
num_lilies <- nrow(irisT)
unique_species <- unique(irisT$Species)
sprintf("In iris sind Daten von %s Lilien enthalten, die zu %d verschiedenen Spezies gehören.", num_lilies, length(unique_species))


#sort by petal length descending cut off onto 8th row then sort same length ith sepal ascending
sort_iris <- irisT %>% arrange(desc(Petal.Length), Sepal.Length) %>% slice(1:8)
sort_iris

#add colour to Iris with random values
coulours <- c("red", "pink", "purple", "white")
set.seed(42)
coulour <- sample(coulours, num_lilies, replace = TRUE)
irisT <- irisT %>% mutate(Colour = coulour)
irisT

#find and store minMaxVals
minMaxVals <- irisT %>%
  group_by(Species) %>%
  summarise(
    min_Sepal.Length = min(Sepal.Length),
    max_Sepal.Length = max(Sepal.Length),
    min_Sepal.Width = min(Sepal.Width),
    max_Sepal.Width = max(Sepal.Width),
    mean_Sepal.Proportion = mean(Sepal.Width / Sepal.Length),
    min_Petal.Length = min(Petal.Length),
    max_Petal.Length = max(Petal.Length),
    min_Petal.Width = min(Petal.Width),
    max_Petal.Width = max(Petal.Width),
    mean_Petal.Proportion = mean(Petal.Width / Petal.Length),
    most_cmn_clr = names(sort(table(Colour), decreasing=TRUE))[1]
  )
minMaxVals


#cerates reprensetatives and adds them to tibble
representative <- minMaxVals %>%
  rowwise() %>%
  mutate(
    Sepal.Length = round(runif(1, min_Sepal.Length, max_Sepal.Length),1),
    Sepal.Width  = round(runif(1, min_Sepal.Width,  max_Sepal.Width),1),
    Petal.Length = round(runif(1, min_Petal.Length, max_Petal.Length),1),
    Petal.Width  = round(runif(1, min_Petal.Width,  max_Petal.Width),1),
    Colour = most_cmn_clr
  ) %>%
  ungroup() %>%
  select(Species, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, Colour)
representative  
irisT <- bind_rows(irisT, representative)  
irisT
