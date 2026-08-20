# Questo script analizza la concentrazione delle rotte mensili (city-pairs),
# testa l'effetto attrazione all'entrata (H1) ed esamina gli shock nazionali.

# --- 1. AGGREGAZIONE MENSILE DEL NETWORK ---
# Calcolo HHI, Gini e le Top10 Rotte
network <- df %>%
  group_by(time) %>%
  mutate(
    # Quota della rotta sul traffico nazionale del mese
    route_share = pax / sum(pax)
  ) %>%
  summarise(
    pax_total = sum(pax),
    pax_lcc = sum(pax[is_lcc_route == 1], na.rm = TRUE),
    # HHI del Network: somma dei quadrati delle quote delle rotte
    HHI_network = sum(route_share^2),
    # Gini del Network: disuguaglianza nella distribuzione tra rotte
    Gini_network = ineq(pax, type = "Gini"),
    # Quota delle Top 10 rotte del mese
    share_top10 = sum(sort(pax, decreasing = TRUE)[1:10]) / sum(pax)
  ) %>%
  ungroup()


# --- 2. IMPLEMENTAZIONE MODELLO BASELINE (STILE OLIVEIRA) ---
model_df <- network %>%
  arrange(time) %>%
  mutate(
    lcc_share = pax_lcc / pax_total,
    # Trasformazione Logit
    logit_HHI = log(HHI_network / (1 - HHI_network)),
    # Calcolo passeggeri giornalieri medi nazionali
    daily_pax = pax_total / 30,
    daily_pax_sq = daily_pax^2,
    # Crescita mensile (Entry-attraction effect)
    growth = log(pax_total) - dplyr::lag(log(pax_total))
  )

# Modello Econometrico Baseline Completo
model_network <- lm(logit_HHI ~ daily_pax + daily_pax_sq + Gini_network + 
                      share_top10 + growth + lcc_share, 
                    data = model_df)

summary(model_network)
# INTERPRETAZIONE RISULTATI (MODEL_NETWORK):
# - R-squared estremamente elevato (0.989): il modello descrive quasi la totalità della varianza della concentrazione delle rotte.
# - daily_pax (-0.0007***) e growth (-0.0326***) sono significativi e negativi: confermano pienamente l'ipotesi H1 (effetto attrazione all'entrata). La crescita del traffico favorisce una naturale frammentazione delle quote sulle rotte.
# - Gini_network (+0.764***) e share_top10 (+7.274***) hanno segno positivo e alta significatività: indicano che, sebbene l'HHI tenda a calare per via dell'espansione, la polarizzazione strutturale del traffico sul nucleo delle 10 rotte principali (es. Catania-Roma, Linate-Roma) mantiene alta la concentrazione sistemica (H3).
# - lcc_share (-0.153) risulta non significativo in questa specifica a causa della forte collinearità con la crescita di volume.


# --- 3. TEST DI RIDUZIONE E CORREZIONE HAC DI NEWEY-WEST ---
# Provo a togliere daily_pax e il suo quadrato per isolare i driver strutturali puri
model_network <- lm(logit_HHI ~ Gini_network + share_top10 + growth + lcc_share, 
                    data = model_df)

summary(model_network)
# INTERPRETAZIONE DOPO LA RIDUZIONE:
# - Rimuovendo daily_pax, la variabile lcc_share assume un coefficiente positivo e altamente significativo.
# - OBIETTIVO METODOLOGICO: Questo evidenzia il ruolo "duplice" delle Low-Cost nel network italiano. Se da un lato favoriscono la deconcentrazione iniziale all'apertura di nuove rotte (effetto catturato da growth), dall'altro, una volta consolidate sulle tratte primarie, si trasformano in nuovi oligopoli locali, innalzando la concentrazione del sistema.

# Applicazione della correzione Newey-West (HAC) con lag = 4 per dati mensili
coeftest(model_network, vcov = NeweyWest(model_network, lag = 4))
# NOTA DI VALIDAZIONE: La correzione HAC conferma la robustezza dei parametri stimati, riducendo gli errori standard senza alterare la significatività di growth, Gini e share_top10.


# --- 4. COSTRUZIONE DATASET AVANZATO CON CONTROLLI E SHOCK ---
# Calcolo controlli aggiuntivi nel blocco summarise mensile per stimare l'impatto degli eventi storici
network_enhanced <- df %>%
  group_by(time) %>%
  summarise(
    pax_total = sum(pax),
    n_routes = n_distinct(route), # Dimensione del network (Capillarità fisica)
    HHI_network = sum((pax/sum(pax))^2),
    Gini_network = ineq(pax, type = "Gini")
  ) %>%
  mutate(
    year = year(time),
    month = month(time),
    logit_HHI = log(HHI_network / (1 - HHI_network)),
    daily_pax = pax_total / 30,
    # Dummy per Eventi Italiani
    dummy_alitalia_2017 = if_else(year == 2017, 1, 0), # Crisi/Transizione Alitalia
    dummy_linate_bridge = if_else(year == 2019 & month %in% c(7,8,9), 1, 0) # Chiusura Linate
  )

# NOTA SU SVILUPPI SUCCESSIVI (MODEL_ENHANCED):
# Questo dataset consente la stima del modello strutturale finale (Tabella 4.6 della tesi), dove:
# - n_routes (-0.0016***) conferma che l'espansione fisica del network riduce la concentrazione.
# - dummy_alitalia_2017 (-0.0171**) evidenzia come la crisi di Alitalia abbia favorito la frammentazione a vantaggio dei competitor.
# - dummy_linate_bridge (-0.0344***) mostra un impatto di deconcentrazione forzata doppio rispetto ad Alitalia, scardinando temporaneamente la dominanza delle rotte business del Nord Italia.

# --- 5. MODELLO AVANZATO CON CONTROLLI DI STRUTTURA E SHOCK DI RETE ---
model_enhanced <- lm(logit_HHI ~ daily_pax + n_routes + Gini_network + 
                       dummy_alitalia_2017 + dummy_linate_bridge, 
                     data = network_enhanced)
summary(model_enhanced)

# Applicazione della correzione Newey-West (HAC) per verificare la significatività robusta
coeftest(model_enhanced, vcov = NeweyWest(model_enhanced, lag = 4))
# INTERPRETAZIONE RISULTATI (MODEL_ENHANCED):
# - n_routes (-0.0016***) conferma che l'espansione della capillarità fisica riduce significativamente l'HHI
# - dummy_alitalia_2017 (-0.0171*) evidenzia che l'instabilità della compagnia di bandiera nel 2017 ha favorito una leggera deconcentrazione a vantaggio dei competitor 
# - dummy_linate_bridge (-0.0344***) mostra un impatto deconcentrante doppio rispetto ad Alitalia, provando che la chiusura temporanea estiva dello scalo milanese ha scosso l'equilibrio del network


# --- 6. IDENTIFICAZIONE EMPIRICA DELLE DUE ROTTE DOMINANTI PER OGNI MESE ---
top_routes_check <- df %>%
  group_by(time) %>%
  # Ordiniamo per passeggeri decrescenti
  arrange(time, desc(pax)) %>%
  # Selezioniamo le prime due righe per ogni gruppo temporale
  slice(1:2) %>%
  # Selezioniamo le colonne utili per la verifica
  select(time, route, pax) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# Visualizza le prime righe per vedere quali sono
head(top_routes_check, 10)
# NOTA DI ANALISI: Poiché i flussi Eurostat sono direzionali, le prime due rotte mensili 
# rappresentano quasi sistematicamente i due sensi di marcia (andata/ritorno) dello stesso collegamento
# I dati rivelano che il vertice è monopolizzato dall'asse Catania-Roma FCO (flussi turistici/familiari) 
# e dall'asse Milano Linate-Roma FCO (flussi prettamente business)


# --- 7. COSTRUZIONE DATASET MENSILE PER L'ANALISI DEL CR2 ---
cr2_data <- df %>%
  group_by(time) %>%
  # 1. Calcolo delle quote di mercato per ogni rotta nel mese
  mutate(route_share = pax / sum(pax, na.rm = TRUE)) %>%
  # 2. Aggregazione per calcolare gli indici strutturali
  summarise(
    pax_total = sum(pax, na.rm = TRUE),
    pax_lcc = sum(pax[is_lcc_route == 1], na.rm = TRUE),
    n_routes = n_distinct(route), # Capillarità (per Modello 2)
    # CR2: Somma delle quote delle 2 rotte più trafficate
    CR2 = sum(sort(route_share, decreasing = TRUE)[1:2], na.rm = TRUE),
    # Gini del network: Misura della disuguaglianza distributiva
    Gini_network = ineq(pax, type = "Gini") 
  ) %>%
  # 3. Trasformazioni e driver per i due modelli
  arrange(time) %>% # Necessario per calcolare correttamente il lag di 'growth'
  mutate(
    year = year(time),
    month = month(time),
    # Variabile Dipendente Trasformata (Oliveira style)
    logit_CR2 = log(CR2 / (1 - CR2)),
    
    # --- VARIABILI MODELLO 1 (Baseline) ---
    daily_pax = pax_total / 30,         # Dimensione mercato (H1)
    daily_pax_sq = daily_pax^2,         # Relazione a U (H1) [15]
    growth = log(pax_total) - dplyr::lag(log(pax_total)), # Reattività (Entry-attraction)
    lcc_share = pax_lcc / pax_total,    # Penetrazione Low-Cost
    
    # --- VARIABILI MODELLO 2 (Enhanced) ---
    # n_routes e Gini_network sono già stati calcolati nel summarise
    dummy_alitalia_2017 = if_else(year == 2017, 1, 0), # Shock Alitalia
    dummy_linate_bridge = if_else(year == 2019 & month %in% c(7,8,9), 1, 0) # Shock Linate
  ) %>%
  ungroup()


# --- 8. STIMA DEI MODELLI DI ROBUSTEZZA SUL CR2 ---

# Modello CR2 1: Impatto delle Low-Cost e della Crescita (Baseline)
model_cr2_eurostat_1 <- lm(logit_CR2 ~ daily_pax + daily_pax_sq + Gini_network + 
                       + growth + lcc_share, 
                    data = cr2_data)

summary(model_cr2_eurostat_1)
# INTERPRETAZIONE (MODEL_CR2_EUROSTAT_1):
# - lcc_share (-2.3930***) ha un impatto negativo e altamente significativo: l'espansione dei vettori Low-Cost 
#   agisce come la principale forza competitiva in grado di erodere la quota di mercato delle rotte leader
# - daily_pax e growth non risultano significativi: la quota del duopolio leader è stabile e non fluttua 
#   con le variazioni congiunturali della domanda mensile, mostrando l'assenza di una relazione a "U" 

# Modello CR2 2: Impatto dei Controlli Strutturali e degli Shock (Enhanced)
model_cr2_eurostat_2 <- lm(logit_CR2 ~ daily_pax + daily_pax_sq + Gini_network + n_routes +
                           dummy_alitalia_2017 + dummy_linate_bridge, 
                         data = cr2_data)

summary(model_cr2_eurostat_2)
# INTERPRETAZIONE (MODEL_CR2_EUROSTAT_2):
# - n_routes (-0.0017***) è negativo e altamente significativo, confermando che l'espansione della capillarità 
#   del network erode sistematicamente la quota del duopolio leader
# - dummy_linate_bridge (-0.1845***) ha un impatto negativo enorme e significativo. Questo dimostra che la 
#   chiusura temporanea di Linate nel 2019, azzerando l'asse Linate-Roma, ha scardinato forzatamente la dominanza 
#   del vertice, redistribuendo temporaneamente il traffico sul resto del sistema
# - dummy_alitalia_2017 non risulta statistica significativa: la crisi di Alitalia non ha indebolito il duopolio 
#   di vertice, ma ha solo ridistribuito le quote interne tra vettori sulla medesima rotta 

# Salva i modelli mensili Eurostat (con shock e CR2)
stargazer(model_network, model_enhanced, model_cr2_eurostat_1, model_cr2_eurostat_2, type = "text",
          title = "Modelli di Rete Mensili e Analisi di Robustezza (Eurostat)",
          out = "output/tables/regressioni_network_mensile.txt")

