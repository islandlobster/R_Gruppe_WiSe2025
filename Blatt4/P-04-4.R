# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser

# ------------------------------------------------------------
# Datenbereinigung: pers_data.csv
# ------------------------------------------------------------
# Ziel: Daten aus einer CSV-Datei einlesen, Spaltennamen vereinheitlichen,
# Kommentare und Einheiten bereinigen und Längenangaben in cm umrechnen.
# ------------------------------------------------------------

library(tidyverse)

# Datei einlesen
pers_data <- read_csv("pers_data.csv")
print("Originaldaten:")
print(pers_data)

# Spaltennamen etwas aufräumen (Kleinbuchstaben + Unterstriche)
pers_data <- pers_data %>%
  rename_with(~ gsub(" ", "_", tolower(.x)))

# Kommentare in Klammern entfernen und Namen ordentlich formatieren
# Außerdem: Gewicht auf Zahl ohne Einheit kürzen
pers_data <- pers_data %>%
  mutate(
    surname = str_remove_all(surname, "\\(.*?\\)") %>%
      str_trim() %>%
      str_to_title(),
    first_name = str_remove_all(first_name, "\\(.*?\\)") %>%
      str_trim() %>%
      str_to_title(),
    weight = str_remove(weight, "kg.*") %>%
      as.numeric()
  )

# Kleine Hilfsfunktion, um verschiedene Längenangaben in cm umzuwandeln
to_cm <- function(x) {
  x <- str_trim(x)
  
  if (str_detect(x, "cm")) {
    as.numeric(str_remove(x, "cm"))
    
  } else if (str_detect(x, "m")) {
    as.numeric(str_remove(x, "m")) * 100
    
  } else if (str_detect(x, "'")) {
    # Beispiel: 6'2'' → 6 Fuß + 2 Zoll
    parts <- str_match(x, "(\\d+)'(\\d*)")
    foot <- as.numeric(parts[2])
    inch <- as.numeric(parts[3])
    foot * 30.5 + inch * 2.5
    
  } else {
    as.numeric(x)
  }
}

# Funktion auf die Spalte mit der Körpergröße anwenden
pers_data <- pers_data %>%
  mutate(height = map_dbl(height, to_cm))

# Ergebnis ansehen
print("Bereinigte Daten:")
print(pers_data)

# Kurze Kontrolle: funktioniert Rechnen mit den numerischen Spalten?
cat("\nDurchschnittliche Körpergröße:", mean(pers_data$height), "cm\n")
cat("Durchschnittliches Körpergewicht:", mean(pers_data$weight), "kg\n")



