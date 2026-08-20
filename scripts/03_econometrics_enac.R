
# Questo script analizza i driver della domanda e i fattori di dominanza 
# dei singoli scali nel decennio 2010-2019.

# --- 1. SET-UP DEI DATI PANEL ---
# Trasformo il dataset in un panel econometrico per catturare la variabilità "within"
pdata <- pdata.frame(data, index = c("airport", "year"))


# --- 2. MODELLAZIONE DEL TRAFFICO PASSEGGERI (DRIVER DELLA DOMANDA) ---

# Modello Baseline con Effetti Fissi Twoways (Within)
model_fe <- plm(
  log_pax ~ log_movements + log_cargo,
  data = pdata,
  model = "within",
  effect = "twoways"
)
summary(model_fe)
# INTERPRETAZIONE:
# - log_movements (0.743***) è altamente significativo: i movimenti sono il driver principale. Un aumento del 10% dei voli genera un incremento del 7.4% dei passeggeri
# - log_cargo non è significativo: conferma la netta separazione operativa tra mercato merci e passeggeri in Italia 
# - R² (Within) = 0.48: il modello spiega il 48% della variabilità interna agli scali

# Modello con Effetti Fissi + Persistenza della Congestione [4]
model_lag <- plm(
  log_pax ~ log_movements + log_cargo + congestion_lag,
  data = pdata,
  model = "within",
  effect = "twoways"
)
summary(model_lag)
# INTERPRETAZIONE:
# - log_movements sale a 0.83*** (elasticità vicina a 1) 
# - congestion_lag (16.45***) è positivo e altamente significativo, confermando l'effetto persistenza: l'intensità di traffico passata attrae stabilmente nuova domanda [22, 23].
# - R² (Within) sale al 69%, confermando l'importanza di includere la dinamica temporale [18].


# --- 3. ANALISI DELLE VARIABILI TIME-INVARIANT (MODELLI OLS) ---

# Modello OLS Baseline (Senza Effetti Fissi) 
model <- lm(log_pax ~ log_movements + congestion_lag + hub + lcc, data = data)
summary(model)
# NOTA: La dummy 'hub' perde significatività una volta controllata la dimensione operativa, 
# evidenziando che è la scala dei voli offerti a determinare la vera centralità 

# Modello OLS con termini di Interazione (Analisi delle Elasticità) 
model_interation <- lm(log_pax ~ log_movements * hub + log_movements * lcc, data = data)
summary(model_interation)
# INTERPRETAZIONE:
# - Negli scali regionali standard l'elasticità è pari a 1.069*** (l'offerta crea la domanda) 
# - Negli Hub l'elasticità è inferiore (1.069 - 0.405 = 0.66*), indicando una domanda più rigida e satura 
# - NOTA DI ROBUSTEZZA: Modello prevalentemente descrittivo a causa della forte collinearità tra scala operativa e categorie strutturali 


# --- 4. MODELLI DI DOMINANZA SULLA QUOTA DI MERCATO (STILE OLIVEIRA) ---

# Modello Oliveira-style sulla Market Share Trasformata (Logit)
model_share_oliveira <- plm(logit_share ~ daily_pax + daily_pax_sq + congestion_lag + hub, 
                            data = pdata, 
                            model = "within", 
                            effect = "twoways")
summary(model_share_oliveira)
# INTERPRETAZIONE:
# - daily_pax (1.03e-04***) ha segno positivo e daily_pax_sq (-5.00e-10***) ha segno negativo: 
#   suggeriscono una relazione non lineare (a parabola rovesciata) a livello di singolo scalo 
# - congestion_lag (7.97e-03***) è positivo e significativo: gli scali storicamente più congestionati 
#   tendono a incrementare la propria quota, confermando l'ipotesi di barriera strategica (H2) 

# Modello di Quota di Mercato con Interazioni
model_share_interaction <- plm(logit_share ~ daily_pax + daily_pax_sq + 
                                 congestion_lag + (hub * daily_pax) + (lcc * daily_pax), 
                               data = pdata, 
                               model = "within", 
                               effect = "twoways")
summary(model_share_interaction)
# INTERPRETAZIONE:
# - I termini di interazione daily_pax:hub (-0.066*) e daily_pax:lcc (-0.056**) sono negativi: 
#   Hub e poli Low-Cost sono mercati ormai "maturi" e la loro quota risponde in modo meno che 
#   proporzionale a incrementi di traffico rispetto agli scali minori 

# Salva una tabella comparativa di tutti i modelli di domanda passeggeri
stargazer(model_fe, model_lag, model, model_interation, type = "text",
          title = "Modelli Econometrici sulla Domanda (ENAC)",
          out = "output/tables/regressioni_domanda_aeroporti.txt")


