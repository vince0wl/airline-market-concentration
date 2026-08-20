# Questo script si occupa della pulizia dei dati Eurostat relativi alle tratte 
# aeree mensili (city-pairs) nel decennio 2010-2019. L'obiettivo è preparare il 
# database granulare per studiare la stagionalità e gli shock di rete.

# --- 1. CARICAMENTO DATASET EUROSTAT (PERCORSO RELATIVO) ---
# Il file originale viene mantenuto compresso in formato .gz per ottimizzare lo spazio su GitHub
eurostat <- read.csv("data/raw/avia_par.gz")

# --- 2. DATA MANIPULATION E RE-DATING ---
# Rinomino le variabili di interesse per allinearle allo standard dello studio
df <- eurostat %>%
  rename(
    route = airp_pr,
    time = TIME_PERIOD,
    pax = OBS_VALUE
  ) %>%
  filter(!is.na(pax)) # Rimozione delle rotte inattive o con dati mancanti

# Controllo del range temporale (verifica decennio di transizione pre-pandemico 2010-2019)
range(df$time) 

# Conversione del tempo in formato Date utilizzando lubridate::ym (Year-Month)
df <- df %>%
  mutate(time = ym(time))

# Selezione e ordinamento cronologico e geografico per rotta
df <- df %>% 
  select(route, time, pax) %>% 
  arrange(route, time) 

# Normalizzazione della scala: divido i passeggeri per 1.000
df <- df %>%
  mutate(
    pax = pax / 1000
  )

# --- 3. GENERAZIONE STATISTICHE DESCRITTIVE AGGREGATE ---
# Selezione della variabile passeggeri prima delle trasformazioni logaritmiche
df_grezzo <- df %>% select(pax)

stargazer(as.data.frame(df_grezzo), type = "text", 
          title = "Statistiche Descrittive: Flussi Mensili per Rotta (Eurostat)",
          covariate.labels = c("Passeggeri per Tratta/Mese"),
          digits = 0,
          out = "output/tables/tabella_descrittiva_eurostat.txt")
# INTERPRETAZIONE DEI RISULTATI DESCRITTIVI (N = 102.145):
# - Media: 14 (14.000 passeggeri mensili per singola tratta).
# - Deviazione Standard: 18 (18.000 passeggeri).
# - Min: 0 | Max: 212 (212.000 passeggeri).
# NOTA METODOLOGICA: La deviazione standard superiore alla media certifica l'elevata 
# frammentazione ed eterogeneità del network italiano, dove pochissime rotte chiave 
# (es. Catania-Roma, Linate-Roma) assorbono la stragrande maggioranza del traffico complessivo.


# --- 4. FEATURE ENGINEERING: PROXY VETTORI LOW-COST (LCC) ---
# Identifico le principali basi operative dei vettori Low-Cost in Italia tramite codici ICAO:
# LIME = Bergamo Orio al Serio
# LIPE = Bologna Borgo Panigale
# LIRP = Pisa San Giusto
# LIRA = Roma Ciampino
# LIRN = Napoli Capodichino
lcc_icao_codes <- c("LIME", "LIPE", "LIRP", "LIRA", "LIRN")

# Creazione della variabile dummy binaria (0/1) per tracciare le rotte LCC
df <- df %>%
  mutate(
    is_lcc_route = if_else(str_detect(route, paste(lcc_icao_codes, collapse="|")), 1, 0)
  )

# Il dataset "df" è ora pulito, formattato e pronto per la fase di aggregazione e modellizzazione
