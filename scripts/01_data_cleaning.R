
library(tidyverse)
library(lubridate)
library(plm)
library(tidyverse)
library(readxl)
library(ggplot2)
library(ineq)
library(sandwich)
library(lmtest)
library(stargazer)

# dati presi da Eurostat, Enac e Assaeroporti
`eurostat` <- read.csv("data/raw/avia_par.gz")
enac_p <- read_excel("data/raw/passeggeri.xlsx")
enac_m <- read_excel("data/raw/movimenti.xlsx")

# funzione di pulizia ricorsiva per Assaeroporti
# Lo script legge la cartella, estrae l'anno a 4 cifre direttamente dal nome di ciascun file e unisce automaticamente tutti i
# file storici in un unico grande dataset chiamato assa
files <- list.files(
  path = "data/raw/assaereoporti",
  pattern = "*.xlsx",
  full.names = TRUE
)
read_clean <- function(file){
  read_excel(file, skip = 1, col_types = "text") %>%
    mutate(year = as.numeric(str_extract(file, "\\d{4}")))
}
assa <- files %>%
  map_df(read_clean)

# selezione e rinomina delle variabili Assaeroporti
assa <- assa %>%
  select(Aeroporto, Movimenti, Passeggeri, 'Cargo (Tons)', year)

assa <- assa %>%
  rename(
    airport = Aeroporto,
    movements = Movimenti,
    pax = Passeggeri,
    cargo = `Cargo (Tons)`
  )

assa <- assa %>%
  filter(!is.na(airport)) %>%
  filter(airport != "TOTALI")

# devo fare in modo che i dati enac siano compatibili con assaeroporti: i dati ENAC originari hanno gli anni distribuiti in colonne orizzontali
# per poterli analizzare in un modello panel, si applica la funzione pivot_longer per "ruotare" la tabella in verticale, creando un record per
# ogni combinazione di Aeroporto, Anno e Passeggeri. Uguale per movimenti.
enac_pax <- enac_p %>%
  pivot_longer(
    cols = starts_with("20"),   # tutte le colonne anni
    names_to = "year",
    values_to = "pax"
  )

enac_pax <- enac_pax %>%
  rename(
    airport = `Aeroporto`,
    icao = ICAO
  ) %>%
  mutate(year = as.numeric(year))

enac_pax <- enac_pax %>%
  mutate(year = as.numeric(year)) %>%
  filter(year >= 2010, year <= 2019)

enac_mov <- enac_m %>%
  pivot_longer(
    cols = starts_with("20"),   # tutte le colonne anni
    names_to = "year",
    values_to = "movements"
  )

enac_mov <- enac_mov %>%
  rename(
    airport = `Aeroporto`,
    icao = ICAO
  ) %>%
  mutate(year = as.numeric(year))

enac_mov <- enac_mov %>%
  mutate(year = as.numeric(year)) %>%
  filter(year >= 2010, year <= 2019)


# voglio unire i dataset assa e enac_mov
sort(unique(assa$airport))

sort(unique(enac_mov$airport))

# risoluzione delle incoerenze nei nomi
mapping <- tibble(
  airport_assa = c(
    "Alghero",
    "Ancona",
    "Bari",
    "Bergamo",
    "Bologna",
    "Brescia",
    "Brindisi",
    "Cagliari",
    "Catania",
    "Cuneo",
    "Firenze",
    "Genova",
    "Milano Linate",
    "Napoli",
    "Palermo",
    "Pisa",
    "Rimini",
    "Taranto-Grottaglie",
    "Torino",
    "Trapani",
    "Treviso",
    "Trieste",
    "Venezia",
    "Verona"
  ),
  
  airport_enac = c(
    "Alghero Fertilia",
    "Ancona Falconara",
    "Bari Palese Macchie",
    "Bergamo Orio al Serio",
    "Bologna Borgo Panigale",
    "Brescia Montichiari",
    "Brindisi Casale",
    "Cagliari Elmas",
    "Catania Fontanarossa",
    "Cuneo Levaldigi",
    "Firenze Peretola",
    "Genova Sestri",
    "Milano Linate",
    "Napoli Capodichino",
    "Palermo Punta Raisi",
    "Pisa S. Giusto",
    "Rimini Miramare",
    "Taranto Grottaglie",
    "Torino Caselle",
    "Trapani Birgi",
    "Treviso S. Angelo",
    "Trieste Ronchi dei Legionari",
    "Venezia Tessera",
    "Verona Villafranca"
  )
)

assa <- assa %>%
  left_join(mapping,
            by = c("airport" = "airport_assa")) %>%
  mutate(
    airport = coalesce(airport_enac, airport)
  ) %>%
  select(-airport_enac)

# unisco i dati di Assaeroporti (da cui prendo passeggeri e merci) con i dati ufficiali ENAC (da cui decidi di tenere la variabile dei movimenti,
# più pulita dal punto di vista operativo)
data <- assa %>%
  left_join(enac_mov, by = c("airport", "year"))

data <- data %>%
  rename(movements = movements.y) %>%
  select(-movements.x)


# converto tutte le colonne in formato numerico e divido il numero di passeggeri per 1.000 per normalizzare la scala numerica
data <- data %>%
  mutate(
    pax = as.numeric(pax),
    movements = as.numeric(movements),
    cargo = as.numeric(cargo)
  )

summary(data$pax)

data <- data%>%
  mutate(
    pax = pax / 1000
  )

# isolo le variabili grezze prima che subiscano trasformazioni logaritmiche o ritardi temporali (lag)
# Utilizzo stargazer per stampare una tabella riassuntiva che riporta per ciascuna variabile: il numero di osservazioni (N),
# la media, la deviazione standard, il valore minimo e il valore massimo
data_grezza <- data %>% select(pax, movements, cargo)

stargazer(as.data.frame(data_grezza), type = "text", 
          title = "Statistiche Descrittive: Dati Aeroportuali Annuali (2010-2019)",
          covariate.labels = c("Passeggeri Totali (pax)", "Movimenti Aerei (movements)", "Merci (cargo)"),
          digits = 0,
          out = "output/tables/tabella_descrittiva_enac.txt")
