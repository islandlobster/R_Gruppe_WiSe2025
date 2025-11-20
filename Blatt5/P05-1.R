# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser

library(tidyverse)

getwd()
setwd("Blatt5")

us_states <- read.csv("us_states.csv")
us_cities <- read.csv("us_cities.csv")
marketcap <- read.csv("companies_mc.csv")
employers <- read.csv("employers.csv")
mass_shootings <- read.csv("mass_shootings_2022.csv")
measurements <- read.csv("measurements1.csv")
patients <- read.csv("patients.csv")
revenues <- read_delim("revenues.csv", delim = ";") #delim weil Stadt und Staat durch Komma getrennt in einem Eintrag gemeinsam gespeichert wurden

us_states |> mutate_if(is.character,str_trim) -> us_states
revenues |> mutate_if(is.character,str_trim) -> revenues
us_cities |> mutate_if(is.character,str_trim) -> us_cities
employers |> mutate_if(is.character,str_trim) -> employers
mass_shootings |> mutate_if(is.character,str_trim) -> mass_shootings
marketcap |> mutate_if(is.character,str_trim) -> marketcap

# Aufgabe 5.1.1
#trim dollar signs from market cap
marketcap <- marketcap |> mutate(market_cap = str_remove(market_cap, "\\$"))
#calc every market cap into numeric dollars (T = trillion = *1e12, B = billion = *1e9, M = million = *1e6)
marketcap <- marketcap |> mutate(market_cap = case_when(
  str_ends(market_cap, "T") ~ as.numeric(str_remove(market_cap, "T")) * 1e12,
  str_ends(market_cap, "B") ~ as.numeric(str_remove(market_cap, "B")) * 1e9,
  str_ends(market_cap, "M") ~ as.numeric(str_remove(market_cap, "M")) * 1e6,
  TRUE ~ as.numeric(market_cap)
))
#rename market_cap into market_cap_billion and divide by 1e9
marketcap <- marketcap |> mutate(market_cap_billion = market_cap / 1e9) |> select(-market_cap)


# AUfgabe 5.1.2
#remove % sign of rev_change
revenues <- revenues |> mutate(rev_change = str_remove(rev_change, "%"))
#apply -/+ for decrease/increse of rev_change
revenues <- revenues |> mutate(rev_change = case_when(
  str_starts(rev_change, "Decrease") ~ paste0("-", str_remove(rev_change, "Decrease ")),
  str_starts(rev_change, "Increase") ~ paste0("+", str_remove(rev_change, "Increase ")),
  TRUE ~ rev_change
))
#convert rev_change into numeric, divide by 100 and add to 100
revenues <- revenues |> mutate(rev_change = as.numeric(rev_change) / 100)
revenues <- revenues |> mutate(rev_change = 1 + rev_change)


# Aufgabe 5.1.3
#calc rev_2020 based on revenue 2021 and rev_change 
mutate(revenues, rev_2020 = revenue_2021 / rev_change) -> revenues
#copy revenues to tmp_revenues2 bc it looks weird on the original
revenues2 <- revenues
#sort revenues by rev_2021 
revenues2 <- revenues2 |> arrange(desc(revenue_2021))
#add rank_2021 column
revenues2 <- revenues2 |> mutate(rank_2021 = row_number())
#sort revenues2 by rev_2020
revenues2 <- revenues2 |> arrange(desc(rev_2020))
#add rank_2020 column
revenues2 <- revenues2 |> mutate(rank_2020 = row_number())
#calc rank_change column
revenues2 <- revenues2 |> mutate(rank_change = rank_2020 - rank_2021)
revenues2 <- revenues2 |> arrange(desc(rank_change))
big_winner <- revenues2 |> slice(1) |> select(company, rank_change)
big_loser <- revenues2 |> arrange(rank_change) |> slice(1) |> select(company, rank_change)
big_winner
big_loser


# Aufgabe 5.1.4
#split headquarter into city and state
revenues <- revenues |> separate(headquarter, into = c("hq_city", "hq_state"), sep = ", ")


# Aufgabe 5.1.5
#create revenues3 copy of revenues 
revenues3 <- revenues
#join states onto revenues3
revenues3 <- revenues3 |> left_join(us_states, by = c("hq_state" = "name"))
#filter revenues3 for only capitals
revenues3 <- revenues3 |> filter(hq_city == capital)
#return size of revenues3
nrow(revenues3)


# Aufgabe 5.1.6
#join revenues onto emplyoers
employers2 <- employers
revenues4 <- revenues
employers2 <- employers2 |> left_join(revenues4, by = c("employer" = "company"))
#cp into tibble: employer, revenue_2021, employees -> get rev_per_emp
employers2 <- employers2 |> mutate(rev_per_emp = revenue_2021 / employees)
#find lowest and apply half to all NAs
lowest_rev_per_emp <- employers2 |> filter(!is.na(rev_per_emp)) |> arrange(rev_per_emp) |> slice(1) |> select(rev_per_emp) |> as.numeric()
employers2 <- employers2 |> mutate(rev_per_emp = ifelse(is.na(rev_per_emp), lowest_rev_per_emp / 2, rev_per_emp))
#cp into tibble: sector, tot_rev_per_sec, tot_emp_per_sec -> get avg_rev_per_emp_per_sec
sector_summary <- employers2 |> group_by(sector) |> summarise(
  tot_rev_per_sec = sum(revenue_2021),
  tot_emp_per_sec = sum(employees)
)
sector_summary <- sector_summary |> mutate(avg_rev_per_emp_per_sec = tot_rev_per_sec / tot_emp_per_sec)


# Aufgabe 5.1.7
#sort cities by census_2020 descending
cities <- us_cities |> arrange(desc(Census_2020))
cities <- cities |> slice(1:300)
#of origonal 300, sort by state and suborder by census_2020 descending
cities <- cities |> arrange(state, desc(Census_2020))
#add new col: start_pos_state onto every entry, the pos_in_state = pos - start_pos_state + 1 
cities <- cities |> group_by(state) |> mutate(start_pos_state = min(row_number()))
cities <- cities |> mutate(pos_in_state = row_number() - start_pos_state + 1)
cities <- cities |> ungroup() 
#join cp of that onto cp of states
states <- us_states
cities <- cities |> left_join(states, by = c("state" = "name"))
cities <- cities |> filter (city == capital)
#muatate new col on cp of states: pos_in_state
cities <- cities |> select(city, Census_2020, state, pos_in_state)

# Aufgabe 5.1.8
#cp of states
states2 <- us_states
#for each entry in mass_shootings ++ to the state entry the num_shootings by 1
states2 <- mutate(states2, num_shootings = 0)
for(i in 1:nrow(mass_shootings)){
  for(n in 1:nrow(states2)){
    if(mass_shootings$State[i] == states2$name[n]){
      states2$num_shootings[n] <- states2$num_shootings[n] + 1
    }
  }
}
#arrange descending by num_shootings
states2 <- states2 |> arrange(desc(num_shootings))


