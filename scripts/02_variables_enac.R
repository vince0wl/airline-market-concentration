# calcolo delle quote di mercato e delle trasformazioni (pax, lag, logit, congestione)

data <- data %>%
  # Calcolo della quota di mercato nazionale (Market Share) per anno
  group_by(year) %>% 
  mutate(share = pax / sum(pax, na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  # Operazioni a livello di singolo aeroporto (Lag e trasformazioni)
  arrange(airport, year) %>%
  group_by(airport) %>%
  mutate(
    # Trasformazioni logaritmiche standard per volumi
    lag_pax = lag(pax),
    log_lag_pax = log(lag_pax),
    log_pax = log(pax),
    log_movements = log(movements),
    log_cargo = log(cargo + 1), 
    
    # Proxy Congestione (H2)
    congestion = pax / movements,
    congestion_lag = lag(congestion),
    
    # Trasformazione Logit per la variabile dipendente (da Oliveira)
    logit_share = log(share / (1 - share)),
    
    # Variabili per testare H1 (Economie di densità)
    daily_pax = pax / 365,
    daily_pax_sq = (pax / 365)^2
  ) %>%
  # 3. Pulizia e filtraggio
  filter(share > 0 & share < 1) %>% # Necessario per il logit [10]
  ungroup()


#creo variabile hub
data <- data %>%
  mutate(
    hub = ifelse(airport %in% c("Roma Fiumicino",
                                "Milano Malpensa",
                                "Milano Linate"), 1, 0)
  )

#creo variabile LCC
data <- data %>%
  mutate(
    lcc = ifelse(airport %in% c("Bergamo Orio al Serio",
                                "Bologna Borgo Panigale",
                                "Napoli Capodichino",
                                "Roma Ciampino",
                                "Pisa S. Giusto"), 1, 0)
  )
