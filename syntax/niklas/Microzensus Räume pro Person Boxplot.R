library(haven)
EH2021_Vivienda <- read_sav("../../data_raw/2021_houshold-survey/EH2021_Vivienda.sav",encoding = "latin1")
View(EH2021_Vivienda)
head(EH2021_Vivienda)

library(haven)
library(dplyr)
library(psych)

names(EH2021_Vivienda)

library(haven)
library(dplyr)

# Datensatz laden
EH2021_Vivienda <- read_sav("~/GitHub/SAE_WS25/data_raw/2021_Encuesta_de_hogares_2021_Microcensus/EH2021_Vivienda.sav")

# Wealth Index: Räume pro Person
EH2021_Vivienda <- EH2021_Vivienda %>%
  mutate(
    wealth_index_rooms_per_person = s07a_24  / totper
  )


# Ergebnisse prüfen
summary(EH2021_Vivienda$wealth_index_rooms_per_person)
hist(EH2021_Vivienda$wealth_index_rooms_per_person,
     main = "Wealth Index: Räume pro Person",
     xlab = "Räume pro Person",breaks = 10
     )

