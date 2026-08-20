# Questo script genera tutti i grafici della tesi, salvandoli automaticamente 
# in formato .png ad alta definizione nella cartella output/plots/

# NOTA DI SICUREZZA: Definiamo queste variabili per evitare errori nella legenda della Curva di Lorenz
anni <- c(2010, 2019)
cols <- c("blue", "red")


# --- 1. Curva di Lorenz (Confronto 2010 vs 2019) ---
# Diciamo a R di aprire un file .png vuoto nella nostra cartella di destinazione
png("output/plots/lorenz_curve.png", width = 800, height = 600, res = 120)

# Disegniamo la curva
plot(Lc(data$pax[data$year == 2010]), col="blue", main="Curva di Lorenz: 2010 vs 2019")
lines(Lc(data$pax[data$year == 2019]), col="red")
legend("topleft", legend=paste("Anno", anni), col=cols, lty=1, lwd=2, bty="n")

# Chiudiamo e salviamo il file sul computer
dev.off() 


# --- 2. Scatter Plot con Fit Quadratico (Dimensione Mercato vs HHI) ---
ggplot(model_data_HHI, aes(x = daily_pax, y = logit_HHI)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "darkred") +
  labs(title = "Relazione tra Dimensione Mercato e HHI")

# ggsave() cattura l'ultimo ggplot generato e lo scrive su disco
ggsave("output/plots/scatter_dimensione_hhi.png", width = 8, height = 6, dpi = 150)


# --- 3. Linee Temporali: HHI vs Gini (Dataset ENAC + Assaeroporti) ---
ggplot(model_data_HHI, aes(x = year)) +
  geom_line(aes(y = HHI * 10, color = "HHI (riscaldato)")) + # Moltiplicato per 10 per visibilità
  geom_line(aes(y = Gini, color = "Gini")) +
  labs(y = "Indice", 
       x = "Data (Anno)",
       title = "Evoluzione HHI vs Gini (2010-2019)",
       subtitle = "Dataset assa + ENAC") +
  theme_minimal()

ggsave("output/plots/hhi_vs_gini_enac.png", width = 8, height = 6, dpi = 150)


# --- 4. Grafico di correlazione: Numero di Rotte vs HHI Mensile (Eurostat) ---
ggplot(network_enhanced, aes(x = n_routes, y = HHI_network)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) +
  labs(title = "Correlazione tra Capillarità del Network e Concentrazione",
       subtitle = "All'aumentare del numero di rotte attive, l'HHI tende a diminuire",
       x = "Numero di Rotte Attive (mensili)", 
       y = "HHI del Network Nazionale") +
  theme_minimal()

ggsave("output/plots/capillarita_network.png", width = 8, height = 6, dpi = 150)


# --- 5. Stagionalità della Concentrazione ---
network_enhanced %>%
  mutate(month_label = month(time, label = TRUE)) %>%
  ggplot(aes(x = month_label, y = HHI_network, fill = month_label)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Stagionalità della Concentrazione del Network",
       subtitle = "L'HHI tende a calare nei mesi estivi (più rotte point-to-point)",
       x = "Mese", y = "HHI Network") +
  theme_minimal()

ggsave("output/plots/stagionalita_hhi.png", width = 8, height = 6, dpi = 150)


# --- 6. Linee Temporali: HHI vs Gini Mensile (Eurostat) ---
ggplot(model_df, aes(x = time)) +
  geom_line(aes(y = HHI_network * 10, color = "HHI (riscaldato)")) + # Moltiplicato per 10 per visibilità
  geom_line(aes(y = Gini_network, color = "Gini")) +
  labs(y = "Indice", 
       x = "Data (Mese/Anno)",
       title = "Evoluzione HHI vs Gini (2010-2019)",
       subtitle = "Dataset Eurostat") +
  theme_minimal()

ggsave("output/plots/hhi_vs_gini_eurostat.png", width = 8, height = 6, dpi = 150)

cat("\n[OK] Tutti i grafici sono stati salvati correttamente nella cartella 'output/plots/'!\n")
