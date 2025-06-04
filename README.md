# Analisi Socioeconomica

Questo progetto analizza diversi modelli statistici, partendo da approcci semplici fino a modelli più complessi.  
L'obiettivo è osservare se esistono relazioni significative tra due principali variabili dipendenti — **aspettativa di vita** e **PIL pro capite** — e una serie di indicatori socioeconomici estratti dal dataset WDI.

L’analisi è stata costruita in modo progressivo:
- Si parte da modelli OLS semplici (es. effetto del PIL pro capite sull'aspettativa di vita),
- Fino ad arrivare a modelli multipli che cercano di spiegare il **PIL pro capite** in funzione delle seguenti variabili:

  - Disoccupazione (%)
  - Investimenti interni lordi
  - Spesa per l'istruzione
  - Spesa per la sanità
  - Popolazione
  - Urbanizzazione (%)
  - Consumo di energia pro capite
  - Esportazioni di beni e servizi
  - Investimenti Diretti Esteri (IDE)
  - Servizi di trasporto (% delle esportazioni di servizi commerciali)
  - Tasse sul reddito, sui profitti e sulle plusvalenze
  - Tariffe commerciali sui prodotti manifatturieri

Il dataset utilizzato è **WDIData**, scaricabile da:  
https://www.kaggle.com/datasets/xavier14/wdidata

---

## 📁 Struttura del progetto

- `Data/` – cartella dove inserire il `WDIData.csv`dopo averlo scaricato dal link
- `Output/` – contiene i grafici in formato `.jpg`
- `Script/` – contiene lo script R per l'analisi
- `README.md` – questa guida

---

## ▶️ Esecuzione

1. Scarica il file `WDIData.csv` dal link sopra  
2. Inseriscilo nella cartella `Data/`  
3. Apri il progetto `GDP_R_Analysis.Rproj` in RStudio  
4. Lancia lo script nella cartella `Script/`
---

## 📄 Report

Il report è attualmente in fase di completamento.

