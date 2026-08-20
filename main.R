# Questo script permette di riprodurre l'intero studio della tesi in sequenza automatica.
# Eseguendo questo singolo file, verranno installate le dipendenze, elaborati i dati, 
# stimate le regressioni e salvati i grafici nelle rispettive cartelle.

cat("=== Avvio dell'analisi empirica del network aereo italiano (2010-2019) ===\n\n")

# 1. Configurazione dell'ambiente e installazione delle librerie necessarie
source("scripts/install_dependencies.R")

# 2. Pulizia dei dati annuali ENAC e Assaeroporti ed estrazione statistiche descrittive
source("scripts/01_data_cleaning.R")

# 3. Costruzione e ingegnerizzazione delle variabili a livello di aeroporto
source("scripts/02_variables_enac.R")

# 4. Esecuzione dei modelli econometrici (Fixed Effects e OLS) a livello scalo
source("scripts/03_econometrics_enac.R")

# 5. Aggregazione nazionale, stima modelli di concentrazione (HHI/Gini) e correzioni HAC
source("scripts/04_national_analysis.R")

# 6. Caricamento e cleaning del dataset mensile di rete Eurostat
source("scripts/05_eurostat_clean_prep.R")

# 7. Modelli econometrici sul network mensile, gestione degli shock e robustezza CR2
source("scripts/06_eurostat_econometrics.R")

# 8. Generazione ed esportazione automatica di tutti i grafici della tesi
source("scripts/07_plots_generator.R")

cat("\n[SUCCESS] Complimenti Dora! Tutti i modelli sono stati stimati e i grafici sono stati salvati correttamente in 'output/plots/'.\n")
