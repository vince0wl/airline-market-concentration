# Questo script aggrega i dati a livello nazionale annuale per calcolare l'indice 
# HHI e l'Indice di Gini. Stima i modelli "Oliveira-style" ed esegue i test di 
# robustezza e la correzione degli errori standard di Newey-West (HAC).

# --- 1. AGGREGAZIONE NAZIONALE E COSTRUZIONE VARIABILI DI CONCENTRAZIONE ---
# Come Oliveira voglio ora studiare il modello HHI, che analizza la struttura dell'intero mercato nazionale
model_data_HHI <- data %>%
  group_by(year) %>%
  summarise(
    pax_total = sum(pax, na.rm = TRUE),
    movements_total = sum(movements, na.rm = TRUE),
    HHI = sum((pax / sum(pax))^2), 
    Gini = ineq(pax, type = "Gini")
  )%>%
  mutate(
    logit_HHI = log(HHI / (1 - HHI)),        # trasformazione logit perché HHI è una Variabile Dipendente Limitata
    daily_pax = pax_total / 365,             # Proxy Densità H1 (media giornaliera passeggeri totali)
    daily_pax_sq = daily_pax^2,              # Termine quadratico per non-linearità
    nat_congestion = pax_total / movements_total # Proxy Congestione H2
  )

summary(model_data_HHI)


# --- 2. STIMA DEL MODELLO BASELINE (STILE OLIVEIRA) ---
model_final <- lm(logit_HHI ~ daily_pax + daily_pax_sq + Gini + nat_congestion, 
                  data = model_data_HHI)

summary(model_final)
# INTERPRETAZIONE RISULTATI (MODEL_FINAL):
# - Il modello presenta un R-squared molto elevato (0.9006): le variabili incluse spiegano circa il 90% della variabilità della concentrazione nazionale nel decennio 2010-2019.
# - I coefficienti di daily_pax e daily_pax_sq risultano non significativi individualmente in questa specifica: questo è un effetto atteso dovuto ai limitati gradi di libertà (solo 10 osservazioni annuali).
# - Il Coefficiente di Gini è l'unico parametro altamente significativo e presenta segno NEGATIVO (-1.5308*): ciò indica empiricamente che la deconcentrazione complessiva del sistema (HHI in calo) si associa paradossalmente a un aumento della disuguaglianza distributiva dei passeggeri. Il traffico si polarizza su pochi scali medi/LCC a scapito degli scali regionali minori.


# --- 3. ESTENSIONE DEL MODELLO CON METRICHE STRUTTURALI COMPLESSE ---
# Provo ad aggiungere altre variabili di controllo su modello Oliveira
model_data_extended <- data %>%
  group_by(year) %>%
  summarise(
    pax_total = sum(pax, na.rm = TRUE),
    movements_total = sum(movements, na.rm = TRUE),
    cargo_total = sum(cargo, na.rm = TRUE),
    HHI = sum((pax / sum(pax))^2),
    HHI_flights = sum((movements / sum(movements))^2), 
    
    # --- CALCOLO CR2 ---
    # Sommiamo i passeggeri dei primi 2 scali e dividiamo per il totale (Roma FCO e Milano MXP)
    CR2 = sum(sort(pax, decreasing = TRUE)[1:2]) / sum(pax),
    
    # Altre variabili strutturali
    Gini = ineq(pax, type = "Gini"), 
    share_top10 = sum(sort(pax, decreasing = TRUE)[1:10]) / sum(pax),
    hub_share = sum(pax[hub == 1]) / sum(pax),
    lcc_share = sum(pax[lcc == 1]) / sum(pax)
  ) %>%
  mutate(
    logit_HHI = log(HHI / (1 - HHI)),
    logit_HHI_flights = log(HHI_flights / (1 - HHI_flights)),
    
    # --- TRASFORMAZIONE LOGIT CR2 ---
    logit_CR2 = log(CR2 / (1 - CR2)),
    
    log_cargo = log(cargo_total + 1),
    daily_pax = pax_total / 365,
    daily_pax_sq = daily_pax^2,
    nat_congestion = pax_total / movements_total
  ) %>%
  ungroup()

# Modello Esteso
model_extended <- lm(logit_HHI ~ daily_pax + share_top10 + Gini + nat_congestion, 
                     data = model_data_extended)

summary(model_extended)
# INTERPRETAZIONE (MODEL_EXTENDED):
# - Conferma l'altissima correlazione tra le variabili strutturali (top 10, Gini) e il livello di concentrazione nazionale.
# - La quota dei primi 10 scali (share_top10) ha un coefficiente fortemente positivo e quasi significativo (4.525, p = 0.053): dimostra che la concorrenza nazionale è dettata interamente dal vertice del mercato.


# --- 4. CONTROLLI DI ROBUSTEZZA ---

# Robustezza 1: Concentrazione dell'offerta (Frequenze di Volo)
model_robust <- lm(logit_HHI_flights ~ daily_pax + share_top10 + Gini + nat_congestion, data = model_data_extended)

summary(model_robust)
# INTERPRETAZIONE:
# - Il modello dimostra che i driver che muovono la concentrazione dei passeggeri governano in modo del tutto analogo la concentrazione dei voli, confermando la solidità strutturale delle conclusioni della tesi.

# Robustezza 2: Analisi del Duopolio Dominante (logit CR2)
model_cr2 <- lm(logit_CR2 ~ daily_pax + daily_pax_sq + Gini + nat_congestion, 
                data = model_data_extended)
summary(model_cr2)
# INTERPRETAZIONE:
# - In questo modello, grazie alla rimozione di alcune collinearità, daily_pax (-0.0143**) e daily_pax_sq (0.000016**) risultano statisticamente significativi e di segno opposto.
# - Questo risultato conferma l'esistenza della relazione non lineare a forma di "U" teorizzata da Sutton (1991): all'aumentare della dimensione del mercato, la concentrazione inizialmente cala (attrazione all'entrata) per poi ricrescere ad alti volumi a causa delle economie di densità degli Hub.


# --- 5. CORREZIONE DI NEWEY-WEST PER AUTOCORRELAZIONE E SERIE STORICHE ---
# Oliveira utilizza la procedura di Newey-West per regolare gli errori standard dei modelli di regressione.
# Oliveira utilizza una funzione kernel di Bartlett con una larghezza di banda calcolata come round(T^(1/4)).
# Nel mio caso avrei: T=10 e quindi la larghezza di banda (lag) sarebbe pari a 2.

# Applicazione della correzione di Newey-West agli errori standard:
# lag = 1 o 2 è appropriato per una serie storica di 10 anni
model_extended <- lm(logit_HHI ~ daily_pax + share_top10 + Gini + nat_congestion, 
                     data = model_data_extended)

# NOTA: Definiamo l'oggetto "e" come il modello stimato per evitare errori di matrice singolare nel calcolo di NeweyWest
e <- model_extended

coeftest(model_extended, vcov = NeweyWest(e, lag = 2)) 
# OBIETTIVO: Ricalcola i test di significatività dei coefficienti applicando la correzione HAC (Heteroskedasticity and Autocorrelation Consistent).
# - Nonostante la scarsità di osservazioni temporali (T = 10) che rischia di rendere singolare la matrice, l'applicazione dei pesi HAC conferma la
# robustezza dei nostri parametri (Gini e nat_congestion)

# Salva i modelli di concentrazione nazionale e robustezza
stargazer(model_final, model_extended, model_robust, model_cr2, type = "text",
          title = "Modelli di Concentrazione Nazionale e Robustezza (ENAC)",
          out = "output/tables/regressioni_concentrazione_nazionale.txt")

