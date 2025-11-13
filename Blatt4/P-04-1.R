# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser
s <- "Three principles underlying R (according to Chambers): 
Everything that exists in R is an object; everything that 
happens in R is a function call; interfaces to other software are part of R."

# 1) Anzahl verschiedener Buchstaben (case-insensitive)
letters_only <- gregexpr("[A-Za-z]", s)[[1]]
distinct_letters <- unique(tolower(regmatches(s, gregexpr("[A-Za-z]", s))[[1]]))
count_distinct <- length(distinct_letters)

# 2) Anzahl aller Buchstaben insgesamt
count_letters <- length(letters_only)

# 3) Anzahl der Zeichen, die keine Buchstaben sind
count_nonletters <- nchar(s) - count_letters

# 4) Anzahl der Großbuchstaben insgesamt
count_uppercase <- length(gregexpr("[A-Z]", s)[[1]])

# 5) Wie oft kommt 'R' oder 'r' vor
count_r <- length(gregexpr("[Rr]", s)[[1]])

cat("1) Verschiedene Buchstaben (case-insensitive):", count_distinct, "\n")
cat("   Buchstaben:", paste(sort(distinct_letters), collapse = ", "), "\n")
cat("2) Buchstaben insgesamt:", count_letters, "\n")
cat("3) Nicht-Buchstaben:", count_nonletters, "\n")
cat("4) Großbuchstaben:", count_uppercase, "\n")
cat("5) Anzahl von 'R' oder 'r':", count_r, "\n")
