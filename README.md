# Analisi Empirica della Concentrazione e Polarizzazione del Settore Aereo Italiano (2010-2019) ✈️📊

Questo repository contiene il codice sorgente R e la metodologia statistica sviluppata per analizzare l'evoluzione competitiva e la struttura del network del trasporto aereo in Italia nel decennio precedente alla crisi pandemica.

Lo studio mette a confronto due forze contrapposte: l'**effetto attrazione all'entrata** (che favorisce la deconcentrazione del mercato tramite l'espansione dei vettori Low-Cost) e l'**investimento strategico in capacità degli Hub** (che protegge la dominanza spaziale dei principali scali tramite la congestione e la saturazione delle frequenze).

## 🔬 Metodologia Econometrica Implementata

I modelli si basano sulla letteratura dell'Economia Industriale (Sutton 1991, Demsetz 1973, Oliveira 2016). Nel codice sono state implementate le seguenti soluzioni statistiche:

1. **Trasformazione Logit delle Variabili Limitate (LDV):** Per modellare la quota di mercato (`share`) e l'indice di concentrazione (`HHI`), vincolati per definizione nell'intervallo (0,1), è stata applicata la trasformazione logit:
   $$\text{logit}(HHI) = \ln\left(\frac{HHI}{1 - HHI}\right)$$
   Ciò consente di mappare i dati sulla retta reale ed evitare stime distorte o previsioni teoricamente impossibili.
   
2. **Modelli Panel con Effetti Fissi Twoways (Within):** Utilizzati per catturare la variabilità interna (within-panel) controllando per le caratteristiche specifiche invarianti nel tempo dei singoli aeroporti (es. posizione geografica) e gli shock temporali comuni.
   
3. **Correzione di Newey-West (HAC - Heteroskedasticity and Autocorrelation Consistent):** Applicata sia con lag = 2 (per il dataset annuale ENAC) che con lag = 4 (per la serie storica mensile Eurostat) per garantire la validità scientifica dell'inferenza (p-value robusti) in presenza di eteroschedasticità e autocorrelazione dei residui.

4. **Coefficiente di Gini e Curva di Lorenz (ineq):** Utilizzati come indicatori di disuguaglianza spaziale per verificare il "Paradosso Italiano": la convivenza tra un calo della concentrazione media delle rotte (HHI) e una crescente polarizzazione del traffico effettivo su pochissimi poli dominanti (Hub e basi LCC).

## 📦 Pacchetti R Utilizzati
- `tidyverse` (dplyr, ggplot2, lubridate) per la pulizia e data manipulation
- `plm` per la stima dei modelli econometrici su dati panel
- `ineq` per il calcolo del Coefficiente di Gini e la Curva di Lorenz
- `sandwich` & `lmtest` per i test dei coefficienti con correzione di Newey-West (HAC)
- `stargazer` per la formattazione professionale delle tabelle di regressione
