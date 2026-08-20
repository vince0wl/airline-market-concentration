# ==============================================================================
# install_dependencies.R
# ==============================================================================
# Script per verificare, installare e caricare automaticamente tutti i pacchetti 
# R richiesti per l'analisi della concentrazione del mercato aereo.

cat("=== Verifica e installazione dei pacchetti R necessari ===\n\n")

# Lista dei pacchetti richiesti
required_packages <- c(
  "tidyverse",  # Data cleaning, manipulation e ggplot2
  "lubridate",  # Gestione date Eurostat
  "readxl",     # Lettura file Excel Assaeroporti/ENAC
  "plm",        # Modelli Panel (Effetti Fissi)
  "ineq",       # Indice di Gini e Curva di Lorenz
  "sandwich",   # Correzione di Newey-West (HAC)
  "lmtest",     # Test dei coefficienti (coeftest)
  "stargazer"   # Tabelle delle regressioni
)

# Funzione per installare i pacchetti mancanti
install_if_missing <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  if (length(missing_packages) > 0) {
    cat("Installazione dei pacchetti mancanti:", paste(missing_packages, collapse = ", "), "\n")
    install.packages(missing_packages, dependencies = TRUE, repos = "https://cloud.r-project.org")
  } else {
    cat("Tutti i pacchetti necessari sono già installati sul sistema!\n")
  }
}

# Esecuzione dell'installazione
install_if_missing(required_packages)

# Verifica del corretto caricamento di ciascun pacchetto
cat("\n=== Verifica caricamento librerie ===\n")
for (pkg in required_packages) {
  status <- require(pkg, character.only = TRUE, quietly = TRUE)
  if (status) {
    cat(sprintf("[OK] %s caricato correttamente.\n", pkg))
  } else {
    cat(sprintf("[ERRORE] Impossibile caricare %s.\n", pkg))
  }
}

cat("\nConfigurazione completata con successo! Sei pronto per eseguire gli script della tesi.\n")
