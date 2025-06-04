
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(jtools)
library(purrr)
library(mgcv)
library(ggrepel)
library(gratia)


#1 ESTRAZIONE DATI


Data<- read.csv ("Data/WDIData.csv")


data_long <- Data %>%
  pivot_longer(
    cols = starts_with("X1960"):starts_with("X2018"),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Year = as.integer(gsub("X", "", Year)),
    Value = as.numeric(Value))



economic_indicators <- data_long %>%
  filter(data_long$Indicator.Code %in% c("NY.GDP.MKTP.CD",   # PIL totale
                                 "NY.GDP.PCAP.CD",   # PIL pro capite
                                 "FP.CPI.TOTL.ZG",   # Inflazione (CPI)
                                 "NY.GDP.MKTP.KD.ZG" # Crescita PIL
                                 ))
                                 

occupation_indicators <- data_long %>%
  filter(data_long$Indicator.Code %in% c("SL.UEM.TOTL.ZS", #Tasso di occupazione
                                         "SL.TLF.CACT.FE.ZS", #Occupazione femminile
                                         "SL.TLF.ACTI.ZS" #Partecipazione forza lavoro)
                                         ))
                                         

education_indicators <- data_long %>%
  filter(data_long$Indicator.Code %in% c("SE.ADT.LITR.ZS", #Tasso di alfabetizzazione adulti
                                         "SE.SEC.ENRR" #Iscrizioni alla scuola superiore)
                                         ))
                                    
health_indicators<- data_long %>%
  filter(data_long$Indicator.Code %in% c("SP.DYN.LE00.IN", #Aspettativa di vita alla nascita
                                         "SH.DYN.MORT.IN", #Mortalità infantile
                                         "SH.XPD.CHEX.GD.ZS" #Spesa sanitaria (% PIL))
                                         ))

development_and_environmental_indicators<-data_long %>%
  filter(data_long$Indicator.Code %in% c("EN.ATM.CO2E.PC", #Emissioni CO2 in tonnellate (metri cubi)
                                         "EG.FEC.RNEW.ZS" #Energia da fonti rinnovabili )
                                         ))

economic_indicators %>%
  filter(Indicator.Code=="NY.GDP.PCAP.CD",
         Country.Name %in% c("Italy", "Germany", "France", "Spain", "United Kingdom")) %>%

    ggplot(aes(x=Year, y=Value, color= Country.Name))+ geom_line(size=1)+labs(title="PIL pro capite", x="Anno", y="Valore")+theme_minimal()

#è stato costruito un grafico che presenta la variabile "anni" sull'asse x e quella valore sull'asse y, ad indicare l'andamento del pil pro capite attraverso gli anni, i valori di riferimento sono quelli di Italia, Germania, Francia, Spagna e Regno Unito.
# Si può osservare come l'andamento di tutti e cinque i paesi segua un trend positivo, si può inoltre osservare un andamento piuttosto simile, ad indicare legame decisamente importante tra i valori del Pil pro capite dei 5 paesi. il grafico presenta un'inversione di tendenza poco prima del 2010, in concomitanza della grande crisi del 2008, tra il 2015 e il 2016 sembra iniziare un leggero rialzo in tutti i paesi tranne che per l'inghilterra


# 2 CREAZIONE MODELLO LINEARE SEMPLICE TRA L'ASPETTATIVA DI VITA E IL PIL PRO CAPITE


pib <- economic_indicators %>%
  filter(Year==2015,Indicator.Code=="NY.GDP.PCAP.CD") %>%
  select(Country.Name, PIL = Value)

life <- health_indicators %>%
  filter(Year==2015, Indicator.Code=="SP.DYN.LE00.IN") %>%
  select(Country.Name, LifeExp = Value)

merged <- inner_join(pib, life, by = "Country.Name")

cor(merged$PIL, merged$LifeExp, use = "complete.obs")

scatter.smooth(merged$PIL,  merged$LifeExp, main= "Relazione PIL Pro Capite, Aspettativa di vita", xlab="PIL pro Capite", ylab="Aspettativa di vita")  

#l'indice di correlazione tra i due indici è 0,61. Ciò indica una discreta correlazione positiva.
#dallo scatterplot si può notare una relazione logaritmica e quindi non lineare tra le due variabili
#si creerà un modello lineare per poi verificare se la funzione log sul Pil Pro capite possa efficientare il modello 

P_and_life_model<- lm(LifeExp~PIL, data = merged)

summary(P_and_life_model)                                         

#il test t su entrambi i parametri β0 e β1 suggerisce di rifiutare l'ipotesi nulla, dato che il valore del test in entrambi i tests è circa zero. Nonostante ciò l'R^2 pari a 0.3739 suggerisce un modello poco esplicativo della variabile aspettativa di vita, la variabile PIL pro capite è quindi utile per spiegare la variabile dipendente, ma non sufficiente.

ggplot(merged, aes(x = PIL, y = LifeExp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "PIL pro capite vs Aspettativa di vita (2015)")

#la retta di regressione indica una relazione lineare positiva tra la variabile dipendenge "LifeExp" e la variabile esplicativa "pil procapite", la stima di beta 1 è pari a 0.0002332377, perciò all'aumentare di un'unità del valore del PIL pro capite, l'aspettativa di vita aumenta in media di 0.0002332377 unità. i dati fanno riferimento a tutti i quelli dei paesi disponibili nell'anno 2015

par(mfrow=c(2,2))

plot(P_and_life_model)

#il grafico residuals vs fitted data la forma a U rovesciata succerisce che potrebbe esistere una relazione non lineare tra le due variabili
#il grafico q-q residuals dimostra che i residui si dispongono pressapoco come una distribuzione normale
#nel grafico scale-location è presente un forte outlier (162) che da al grafico una fomra quasi ad imbuto, è consigliabile eliminare l'outlier e controllare se questa forma, che suggerisce mancanza di omoschedasticità, permanga
#nel grafico residual vs leverage individua 85, 164 e 162 come outlier

par(mfrow=c(1,1))


#2.1 CREAZIONE MODELLO LOG


merged1<- merged %>%
  mutate(PIL=log(PIL))

log_P_and_life_model<- lm(LifeExp~PIL, data = merged1)
summary(log_P_and_life_model)
#l'R^2 aumenta sensibilmente da 0.3739 a 0.7099, ad indicare che l'applicazione della funzione log è stata cruciale per rendere il modello più efficiente.

ggplot(merged1, aes(x = PIL, y = LifeExp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "log PIL pro capite vs Aspettativa di vita (2015)")


par(mfrow=c(2,2))

plot(log_P_and_life_model)

#Dai grafici possiamo notare come la linearità del modello in questo caso sia decisamente più probabile.
#Nonostante un discostamento dei residui più piccoli dalla distribuzione normale, il modello si può dire fortemente perfezionato dalla funzione log

par(mfrow=c(1,1))


#2.1 MODELLO SEMPLICE DEL PUNTO 2 PROPOSTO PER ITALIA, FRANCIA, GERMANIA, SPAGNA E REGNO UNITO


pib_5_2000_2018 <- economic_indicators %>%
  filter(Year >= 2000 & Year <= 2018,
         Indicator.Code == "NY.GDP.PCAP.CD",
         Country.Name %in% c("Italy", "France", "Germany", "Spain", "United Kingdom")) %>%
  select(Country.Name, Year, PIL = Value)

life_5_2000_2018 <- health_indicators %>% 
  filter(Year >= 2000 & Year <= 2018,Indicator.Code=="SP.DYN.LE00.IN",
         Country.Name %in% c("Italy", "France", "Germany", "Spain", "United Kingdom")) %>%
  select(Country.Name, Year, LifeExp = Value)

merged_5_2000_2018 <- inner_join(pib_5_2000_2018, life_5_2000_2018, by = c("Country.Name", "Year"))

scatter.smooth(merged_5_2000_2018$PIL ,merged_5_2000_2018$LifeExp, main="Relazione PIL Pro Capite, Aspettativa di vita per Italia, Francia, Spagna, Germania e Regno Unito", xlab="PIL pro Capite", ylab="Aspettativa di vita" )

PIB_life_5_2000_2018_model <- lm(LifeExp ~ PIL, data = merged_5_2000_2018)

summary(PIB_life_5_2000_2018_model)

# il test t su β0 e β1 suggerisce di rifiutare l'ipotesi nulla.
#l'R^2 è pari a 0.04765, un valore decisamente scarso, a sottolineare la necessità di cercare altre variabili esplicative da aggiungere al modello

ggplot(merged_5_2000_2018, aes(x = PIL, y = LifeExp, color = Country.Name)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "PIL pro capite vs Aspettativa di vita (2000–2018)",
       x = "PIL pro capite (USD)",
       y = "Aspettativa di vita") +
  theme_minimal()

par(mfrow=c(2,2))

plot(PIB_life_5_2000_2018_model)

#Il grafico Residual vs Fitted mostra una forma quasi lineare, ciò suggerisce una relazione lineare, ma non è sufficiente per determinarlo certamente.
#Il secondo grafico, Q-Q Residuals mostra che la distribuzione dei residui non segue propriamente quella di una normale, è possibile quindi che gli errori non si distribuiscano come una gaussiana.
#Il grafico Scale-Location mostra una tendenza crescente all'avvicinarsi agli outlier (74,75,72), per poi tornare alla normalità. Questo mostra una forma paraboloide, seppur molto schiacciata, l'omoschedasticità potrebbe non sussistere. Si procede a eliminare gli outlier per ulteriori verifiche. 
#Il grafico Residuals vs Leverage mostra come outlier principali 74, 20 e 84, per il resto i punti si distribuiscono abbastanza uniformemente attorno allo 0

par(mfrow=c(1,1))


#2.1.1 MODELLO DEL PUNTO 2.1 LOG

merged_5_2000_2018_log <- merged_5_2000_2018 %>%
  mutate(PIL= log(PIL))

log_PIB_life_5_2000_2018_model<- lm(LifeExp ~ PIL, data = merged_5_2000_2018_log)
summary(log_PIB_life_5_2000_2018_model)
#la funzione log in questo caso, nonostante il leggero inremento dell'R^2 (0.06548), è scarsamente utile al miglioramento del modello

par(mfrow=c(2,2))

plot(log_PIB_life_5_2000_2018_model)

#si può notare che l'effetto della funzione log sul modello non ha dato l'effetto desiderato, probabilmente esiste una relazione non lineare

par(mfrow=c(1,1))


#3 MODELLO LINEARE ASPETTATIVA DI VITA, PIL PRO CAPITE, SPESA PER LA SANITÀ SU PIL, TASSO DI ALFABETIZZAZIONE PER ITALIA, FRANCIA, GERMANIA, SPAGNA E REGNO UNITO


health_exp<- health_indicators %>%
  filter(Year >= 2000 & Year <= 2018, Indicator.Code=="SH.XPD.CHEX.GD.ZS",
         Country.Name %in% c("Italy", "France", "Germany", "Spain", "United Kingdom")) %>%
  select(Country.Name,HealthExp=Value, Year )

merged2 <- merged_5_2000_2018 %>%
  inner_join(health_exp, by=c("Country.Name","Year")) 

modello_multi2<- lm(LifeExp ~ PIL + HealthExp, data=merged2)
summary(modello_multi2)
#Secondo il test t per entrambe le variabili possiamo rifiutare l'ipotesi nulla e anche nel caso dell'intercetta
#il test f presenta un p value di 0.0056 che ci permette di rifiutare l'ipotesi nulla, ovvero che nessun parametro del modello sia esplicativo
#L'R^2 però è pari a 0.1187, ciò dimostra la scarsità del modello
#nonostante l'adjusted R^2 (0.09719) è più alto di quello del modello a una sola variabile (0.05486)


#3.0.1 AGGIUNTA DI VARIABILI ESPLICATIVE


alfab <- education_indicators %>%
  filter(Year >= 2000 & Year <= 2018, Indicator.Code=="SE.ADT.LITR.ZS",
         Country.Name %in% c("Italy", "France", "Germany", "Spain", "United Kingdom")) %>%
  select(Country.Name, Alfab=Value, Year)

emissioni <- development_and_environmental_indicators %>%
  filter(Year >= 2000 & Year <= 2018, Indicator.Code=="EN.ATM.CO2E.PC",
         Country.Name %in% c("Italy", "France", "Germany", "Spain", "United Kingdom")) %>%
  select(Country.Name, Year, CO2Em=Value)

merged3 <- merged2 %>%
  inner_join(alfab, by=c("Country.Name","Year")) 

modello_multi3<-lm(LifeExp ~ PIL + HealthExp + Alfab, data = merged3)
summary(modello_multi3)
#in questo caso secondo il test t non possiamo rifiutare l'ipotesi nulla per X1 (la variabile pil procapite)
#ipotizzabile che questo avvenga dato che sono stati comparati paesi molto avanzati del continente europeo e che quindi, l'alto livello di ricchezza di questi stati renda quasi superflue le differenze tra i pil pro capite per spiegare le differenti aspettative di vita. 
#Probabilmente questo non varrebbe per paesi con forti differenze economiche
#nonostante ciò l'R^2 è aumentato sensibilmente, il suo valore è pari a 0.8324
#questo aumento però è biassato dalla mancanza di dati, in quanto esistono dati parziali solo per spagna e italia
#si procede a utilizzare un nuovo parametro

emissioni <- development_and_environmental_indicators %>%
  filter(Year >= 2000 & Year <= 2018, Indicator.Code=="EN.ATM.CO2E.PC",
         Country.Name %in% c("Italy", "France", "Germany", "Spain", "United Kingdom")) %>%
  select(Country.Name, Year, CO2Em=Value)

merged3_new<- merged2 %>%
  inner_join(emissioni, by=c("Country.Name", "Year"))

modello_multi3_new <- lm(LifeExp ~ PIL + HealthExp + CO2Em, data = merged3_new)
summary(modello_multi3_new)
#in questo caso l'R^2 è più basso 0.6442, ma in questo caso sono presenti dati sufficienti per poterlo considerare un aumento realistico rispetto al modello con due variabili esplicative (adjusted R^2=0.69292 contro 0.09719)
#ciò ci porta a fare attenzione a che dati vengono inseriti e non solo a che risultati essi ci diano
#paradossalmente il parametro HealthExpenditure non supera il test t, non possiamo rifiutare l'ipotesi nulla
#come nel caso sopracitato è probabile che questo succeda dato l'elevato livello di sviluppo dei paesi, e la scarsa differenza tra essi
#in questo caso, però, il rifiutiamo l'ipotesi nulla sul parametro pil pro capite

p <- effect_plot(modello_multi3_new, pred = PIL, interval = TRUE, plot.points = TRUE, partial.residuals = TRUE)+
  labs(title = "Effetto del PIL sull'Aspettativa di vita")
ggplotly(p)

h<- effect_plot(modello_multi3_new, pred=HealthExp, interval = TRUE, plot.points= TRUE, partial.residuals = TRUE)+
  labs(title = "Effetto della Spesa Sanitaria sull'Aspettativa di vita")
ggplotly(h)

a <- effect_plot(modello_multi3_new, pred= CO2Em, interval=TRUE, plot.points = TRUE, partial.residuals=TRUE)+
  labs(title = "Effetto delle emissioni di CO2 sull'Aspettativa di vita")
ggplotly(a)

subplot(p, h, a, nrows = 3, shareX = FALSE, shareY = FALSE, titleY = TRUE) %>%
  layout(title = list(text = "Effetti comparati", x = 0.5))
#ognuno di questi tre grafici si focalizza su una delle tre variabili indipendenti, considerando le altre come costanti, per analizzare l'effetto della singola variabile


#3.1 GRAFICO ASPETTATIVA DI VITA, PIL, SPESA SANITARIA, EMISSIONI 2014 (ultimi dati completi)


merged3_2014<- merged3_new %>%
  filter(Year==2014)

ggplot(merged3_2014, aes(x = PIL, y = LifeExp, color = HealthExp, size=CO2Em)) +
  geom_point(alpha = 0.7) +
  ggrepel::geom_text_repel(aes(label = Country.Name), size = 3, max.overlaps = 10) +  # Usa il nome della colonna giusta!
  labs(title = "Aspettativa di vita, PIL pro capite, Spesa sanitaria, Emissioni 2014",
       x = "PIL pro capite",
       y = "Aspettativa di vita",
       color = "Spesa sanitaria",
       size = "Emissioni CO2")+
  theme_minimal()



#3.2 ANALISI DEL MEDESIMO MODELLO MA SU I 5 PAESI CON ASPETTATIVA DI VITA PIÙ ALTA CONTRO I 5 CON QUELLA PIÙ BASSA


pib_diff<- economic_indicators %>%
  filter(Indicator.Code=="NY.GDP.PCAP.CD", Year >= 2000 & Year <=2018, 
  Country.Name %in% c("Hong Kong SAR, China","Japan","Macao SAR, China","Switzerland","Spain", "Sierra Leone", "Chad", "Nigeria","Cote d'Ivoire", "Lesotho")) %>%
  select(Country.Name, Year, PILp=Value)

life_diff <- health_indicators %>%
  filter(Indicator.Code=="SP.DYN.LE00.IN", Year >= 2000 & Year <=2018,
  Country.Name %in% c("Hong Kong SAR, China","Japan","Macao SAR, China","Switzerland","Spain", "Sierra Leone", "Chad", "Nigeria","Cote d'Ivoire", "Lesotho")) %>%
  select(Country.Name, Year, LifeExp=Value)

health_exp_diff <- health_indicators %>%
  filter(Indicator.Code=="SH.XPD.CHEX.GD.ZS", Year >= 2000 & Year <=2018,
         Country.Name %in% c("Hong Kong SAR, China","Japan","Macao SAR, China","Switzerland","Spain", "Sierra Leone", "Chad", "Nigeria","Cote d'Ivoire", "Lesotho")) %>%
  select(Country.Name, Year, HealthExp= Value)

emissioni_diff <- development_and_environmental_indicators %>%
  filter(Indicator.Code=="EN.ATM.CO2E.PC", Year >= 2000 & Year <=2018,
         Country.Name %in% c("Hong Kong SAR, China","Japan","Macao SAR, China","Switzerland","Spain", "Sierra Leone", "Chad", "Nigeria","Cote d'Ivoire", "Lesotho")) %>% 
  select(Country.Name, Year, CO2Em= Value)

merged_diff <- pib_diff %>%
  inner_join(life_diff, by = c("Country.Name", "Year")) %>%
  inner_join(health_exp_diff, by = c("Country.Name", "Year")) %>%
  inner_join(emissioni_diff, by = c("Country.Name", "Year"))

modello_diff <- lm(LifeExp ~ PILp + HealthExp + CO2Em, data= merged_diff)
summary(modello_diff)
#anche in questo caso, non possiamo rifiutare l'ipotesi nulla per la spesa per la sanità
#l'R^2 aumenta fino al livello sorprendente di 0.9442
par(mfrow=c(2,2))

plot(modello_diff)
#nonostante ciò plot() suggerisce che probabilmente la relazione è non lineare
par(mfrow=c(1,1))

p_diff <- effect_plot(modello_diff, pred = PILp, interval = TRUE, plot.points = TRUE, partial.residuals = TRUE)+
  labs(title = "Effetto del PIL sull'Aspettativa di vita")
ggplotly(p_diff)

subplot(ggplotly(p), ggplotly(p_diff), nrows = 1, margin = 0.05, shareY = FALSE)
# nel subplot viene mostrata la differenza dell'effetto del pil procapite sui due modelli, il primo che prendeva solo paesi economicamente avanzati (Italia, Francia, Spagna, Regno Unito, Germania), il secondo prende i 5 paesi con aspettativa di vita più alta e i 5 con aspettativa di vita più bassa

h_diff <- effect_plot(modello_diff, pred=HealthExp, interval= TRUE, plot.points = TRUE, partial.residals = TRUE)+
  labs(title = "Effetto Health Expenditure sull'Aspettativa di vita")
subplot(ggplotly(h), ggplotly(h_diff), nrows = 1, margin = 0.05, shareY = FALSE)


#3.3 MODELLO NON LINEARE


mod_gam <- gam(LifeExp ~ s(PILp) + s(HealthExp) + s(CO2Em), data= merged_diff)
summary(mod_gam)
draw(mod_gam) + theme_minimal()

#possiamo dire che tutte le variabili sono esplicative dato che superano ampliamente il test F
#l'R^2 è sorprendentemente alto (0.99)
#si procede a verificare se questo sia dato dalla scarsità di osservazioni presenti (190)


pib_all<- economic_indicators %>%
  filter(Indicator.Code=="NY.GDP.PCAP.CD", Year >= 2000 & Year <=2018) %>%
  select(Country.Name, Year, PILp=Value)

life_all <- health_indicators %>%
  filter(Indicator.Code=="SP.DYN.LE00.IN", Year >= 2000 & Year <=2018) %>%
  select(Country.Name, Year, LifeExp=Value)

health_exp_all <- health_indicators %>%
  filter(Indicator.Code=="SH.XPD.CHEX.GD.ZS", Year >= 2000 & Year <=2018) %>%
  select(Country.Name, Year, HealthExp= Value)

emissioni_all <- development_and_environmental_indicators %>%
  filter(Indicator.Code=="EN.ATM.CO2E.PC", Year >= 2000 & Year <=2018) %>% 
  select(Country.Name, Year, CO2Em= Value)

merged_all<- pib_all %>%
  inner_join(life_all, by=c("Year", "Country.Name")) %>%
  inner_join(health_exp_all, by=c("Year", "Country.Name")) %>%
  inner_join(emissioni_all, by=c("Year", "Country.Name")) 


mod_gam_all<- gam(LifeExp ~ s(PILp) + s(HealthExp) + s(CO2Em), data= merged_all)
summary(mod_gam_all)

#in questo caso possiamo notare una forte diminuzione dell'R^2 (0.693) nonostante sia comunque molto buono, questo ci porta a ipotizzare che il risultato così alto del modello precedente fosse più alto della realtà
#allo stesso tempo in questo caso la presenza di così tante variabili differenti potrebbe creare un bias al ribasso, è possibile che l'R^2 reale si trovi compreso tra questo e quello del modello precedente
draw(mod_gam_all) + theme_minimal()

# si nota una certa somiglianza tra i grafici del modello precedente e questo
#ciò porta a ipotizzare che le osservazioni scelte per il precedente fossero efficaci per spiegare il modello
#si procede a un confronto per verificare le ipotesi 

effects_gam <- smooth_estimates(mod_gam) %>% mutate(model = "Modello: Paesi estremi")
effects_gam_all <- smooth_estimates(mod_gam_all) %>% mutate(model = "Modello: Tutti i paesi")

colnames(effects_gam_all)

effects_all <- bind_rows(effects_gam, effects_gam_all) %>%
  rename(
    smooth = .smooth,
    est = .estimate,
    se = .se
  ) %>%
  mutate(
    x = case_when(
      smooth == "s(PILp)" ~ PILp,
      smooth == "s(HealthExp)" ~ HealthExp,
      smooth == "s(CO2Em)" ~ CO2Em,
      TRUE ~ NA_real_
    )
  )

ggplot(effects_all, aes(x = x, y = est, color = model, fill = model)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = est - se, ymax = est + se), alpha = 0.2, color = NA) +
  facet_wrap(~smooth, scales = "free") +
  labs(
    title = "Confronto degli effetti stimati (GAM)",
    x = "Valore della variabile predittiva",
    y = "Effetto stimato",
    color = "Modello",
    fill = "Modello"
  ) +
  theme_minimal()

# si nota una forte differenza tra le due distribuzioni nel caso delle emissioni di CO2, nel modello dei paesi estremi l'effetto è molto più accentuato, mentre con tutti i paesi è più smussato
# nel caso del parametro "HealthExp" inizialmente i due modelli hanno un andamento crescente simile ma successivame divergono.
# nel caso del PIL pro capite i modelli sono sorprendentemente simili, nonostante si possa notare un effetto più accentuato nel caso dei paesi estremi

#il modello dei paesi più estremi denota degli effetti più marcati, come ci si poteva aspettare potrebbe esasperare la realtà
# il modello che tiene in conto tutti i paesi è probabilmente più verosimile, ma potrebbe rappresentatare effetti più smussati
# tutto sommato la somiglianza dei due modelli, soprattutto nel caso del Pil pro capite, sottolinea che i paesi estremi sono abbastanza indicativi, nonostante l'estrazione di paesi intermedi probabilmente avrebbe migliorato l'efficacia


#4 ANALISI DELLA VARIAZIONE DEL PIL PROCAPITE DEI 10 PAESI CON MAGGIOR DEVIAZIONE STANDARD


gdp_per_capita<- economic_indicators %>%
  filter(Indicator.Code== "NY.GDP.PCAP.CD",
         Year>= 2000 & Year <= 2018)

sd_gdp <- gdp_per_capita %>%
  group_by(Country.Name) %>%
  summarise(DevStandard = sd(Value, na.rm = TRUE)) %>%
  arrange(desc(DevStandard)) %>%
  slice_head(n = 10)  

panel_top10 <- gdp_per_capita %>%
  filter(Country.Name %in% sd_gdp$Country.Name) %>%  
  arrange(Country.Name, Year) %>%
  group_by(Country.Name) %>%
  mutate(Diff_GDP = Value - lag(Value)) %>%  
  ungroup()

plot_top_10 <- ggplot(panel_top10, aes(x = Year, y = Value, color = Country.Name)) +
  geom_line() +
  theme_minimal()

ggplotly(plot_top_10)

#4.1 ESTRAZIONE DATI PER LA CREAZIONE DI UN MODELLO ESPLICATIVO DELLA VARIAZIONE DEL PIL PRO CAPITE

infl_most_var <- economic_indicators %>%
  filter(Indicator.Code=="FP.CPI.TOTL.ZG",
         Country.Name %in% panel_top10$Country.Name,
         Year>=2000 & Year <= 2018) %>%
  select(Country.Name, 
         Inflation= Value,
         Year)

occup_most_var<- occupation_indicators %>%
  filter(Indicator.Code=="SL.UEM.TOTL.ZS",
         Country.Name %in% panel_top10$Country.Name,
         Year>= 2000 & Year<= 2018) %>%
  select(Country.Name,
         OccupationIndex= Value,
         Year)

green_energy_most_var <- development_and_environmental_indicators %>%
  filter(Indicator.Code=="EG.FEC.RNEW.ZS",
         Country.Name %in% panel_top10$Country.Name,
         Year>=2000 & Year <=2018) %>%
  select(Country.Name,
         GreenEn= Value,
         Year)

merged_10_most_var <- reduce(
  list(panel_top10, infl_most_var, occup_most_var, green_energy_most_var),
  inner_join,
  by = c("Country.Name", "Year")
)

#4.2 MODELLO LINEARE PIL PRO CAPITE, INFLAZIONE, TASSO DI OCCUPAZIONE, E ENERGIA DA FONTI RINNOVABILI

modello_most_var<- lm(Diff_GDP~ Inflation+ OccupationIndex + GreenEn , data= merged_10_most_var)
summary(modello_most_var)
#in questo caso, l'unica variabile di cui possiamo rifiutare l'ipotesi nulla sembra essere l'inflazione, che per ovvi motivi ha una forte influenza sulle variazioni del pil 
#l'R^2 è 0.1016, dimostrando che il modello non è sufficientemente esplicativo
#il test f mostra un p value di 0.02593, ciò ci permette di rifiutare l'ipotesi nulla
par(mfrow=c(2,2))
plot(modello_most_var)
#i quattro grafici mostrano una forte distorsione nei valori inferiori, si procederà ad applicare la funzione logaritmica alle variabili per eliminare i valori pari o inferiori a zero e per migliorare la linearità del modello
#in caso la funzione log non sia sufficiente per migliorare il modello si procederà a verificare la non linearità

ggplot(merged_10_most_var, aes(x = Inflation, y = Diff_GDP)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Inflazione vs crescita PIL pro capite")

ggplot(merged_10_most_var, aes(x = OccupationIndex, y = Diff_GDP)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Occupazione vs crescita PIL pro capite")

ggplot(merged_10_most_var, aes(x = GreenEn, y = Diff_GDP)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Energia da fonti rinnovabili vs crescita PIL pro capite")


#4.2.1 MODELLO CON FUNZIONE LOG

merged_10_most_var_log <- merged_10_most_var %>%
  mutate(
    Diff_GDP = Diff_GDP + 1,
    Inflation = Inflation + 1,
    OccupationIndex = OccupationIndex + 1,
    GreenEn = GreenEn + 1
  ) %>%
  filter(
    Diff_GDP > 0,
    Inflation > 0,
    OccupationIndex > 0,
    GreenEn > 0
  )

model_log <- lm(log(Diff_GDP) ~ log(Inflation) + log(OccupationIndex) + log(GreenEn),
                data = merged_10_most_var_log)
summary(model_log)
#l'R^2 è addirittura inferiore rispetto a quello precedente, si rifiuta l'ipotesi nulla solo per B1 (Inflazione) e B2 (Occupazione), si rifiuta l'ipotesi di collinearità
par(mfrow= c(2,2)) 
plot(model_log) 
par(mfrow= c(1,1))
#l'applicazione della funzione log ha decisamente incrementato l'ipotesi di linearità come si può vedere dal primo grafico. in questo caso l'omoschedasticità sembra rispettata come si può vedere da Scale-Location e non si notano particolari outlier
# rimane però una forte distorsione nella distribuzione dei residui rispetto a quella di una normale nei valori inferiori


#4.2.2 MODELLO NON LINEARE


mod_gam2 <- gam(Diff_GDP ~ s(Inflation) + s(OccupationIndex) + s(GreenEn), 
               data = merged_10_most_var)
summary(mod_gam2)
#migliora leggermente l'R^2 0,143, si rifiuta l'ipotesi nulla per tutte le variabili
draw(mod_gam2) + theme_minimal()


#4.3 Ricerca variabili esplicative


indicatori_modello <- c("SL.UEM.TOTL.ZS",  # Unemployment (% of labor force)
                            "NE.GDI.TOTL.ZS",  # Gross Domestic Investment (% of GDP)
                            "SE.XPD.TOTL.GD.ZS",  # Education expenditure (% of GDP)
                            "SH.XPD.CHEX.GD.ZS",  # Health expenditure (% of GDP)
                            "SP.POP.TOTL",  # Population (total)
                            "SP.URB.TOTL.IN.ZS",  # Urban population (% of total)
                            "EG.USE.PCAP.KG.OE",  # Energy use per capita (kg of oil equivalent)
                            "NE.EXP.GNFS.ZS",  # Exports of goods and services (% of GDP)
                            "BX.KLT.DINV.WD.GD.ZS",  # Foreign direct investment (% of GDP)
                            "TX.VAL.TRAN.ZS.WT",  # Transport services (% of commercial service exports)
                            "GC.TAX.YPKG.ZS",  # Taxes on income, profits and capital gains (% of total taxes)
                            "TM.TAX.MANF.WM.AR.ZS"  # Tariff rate on manufactured products (%)
                        )


dati_per_modello <- data_long %>%
  filter(Indicator.Code %in% indicatori_modello,
         Country.Name %in% panel_top10$Country.Name,
         Year >= 2000 & Year <= 2018) %>%
  select(Country.Name, Year, Indicator.Code, Value) %>%
  pivot_wider(names_from = Indicator.Code, values_from = Value)

dati_per_modello <- dati_per_modello %>%
  rename(
    Disoccupazione = SL.UEM.TOTL.ZS,
    Investimenti = NE.GDI.TOTL.ZS,
    Istruzione = SE.XPD.TOTL.GD.ZS,
    Sanità = SH.XPD.CHEX.GD.ZS,
    Popolazione = SP.POP.TOTL,
    Urbanizzazione = SP.URB.TOTL.IN.ZS,
    Energia_per_capite = EG.USE.PCAP.KG.OE,
    Esportazioni = NE.EXP.GNFS.ZS,
    IDE = BX.KLT.DINV.WD.GD.ZS,
    Servizi_trasporto = TX.VAL.TRAN.ZS.WT,
    Tasse = GC.TAX.YPKG.ZS,
    Tariffe_prodotti = TM.TAX.MANF.WM.AR.ZS
  )


# 4.3.1 creazione modello


merged_modello_perfezionato <- dati_per_modello %>%
  inner_join(panel_top10, by=c("Country.Name", "Year"))

modello_perfezionato <- lm(Diff_GDP ~ Disoccupazione + Investimenti + Istruzione + Sanità + Popolazione + Urbanizzazione + Energia_per_capite + Esportazioni + IDE + Servizi_trasporto + Tasse + Tariffe_prodotti, 
                           data = merged_modello_perfezionato )
summary(modello_perfezionato)
par(mfrow=c(2,2))
plot(modello_perfezionato)

#si nota un forte aumento dell'R^2, ma molti dei parametri non superano il valore limite per permettere il rifiuto dell'ipotesi nulla
#si procede a impostare solo i parametri che superano il test
#successivamente verrà fatto un modello per tutti i paesi
#dal plot notiamo una relazione verosimilmente lineare, si individuano quattro outlier principali


#4.3.2 modello con parametri omessi


modello_perfezionato2 <- lm(Diff_GDP ~ Investimenti + Sanità + Urbanizzazione + Servizi_trasporto + Tasse + Tariffe_prodotti,
                            data = merged_modello_perfezionato )
summary(modello_perfezionato2)
#l'R^2 in questo caso cala da 0.4649 a 0.2947, mentre l'adjusted non cala così drasticamente, prima 0.2578 e successivamente 0.2204. Indicando che probabilmente i parametri omessi in questo caso non fossero particolarmente importanti
#in questo caso il parametro Investimenti è l'unico che non supera il test t, nonostante sia molto vicino alla soglia limite
plot(modello_perfezionato2)


#4.4 Modello per tutti i paesi


dati_per_modello_tutti_i_paesi <- data_long %>%
  filter(Indicator.Code %in% indicatori_modello,
         Year >= 2000 & Year <= 2018) %>%
  select(Country.Name, Year, Indicator.Code, Value) %>%
  pivot_wider(names_from = Indicator.Code, values_from = Value)

dati_per_modello_tutti_i_paesi <- dati_per_modello_tutti_i_paesi %>%
  rename(
    Disoccupazione = SL.UEM.TOTL.ZS,
    Investimenti = NE.GDI.TOTL.ZS,
    Istruzione = SE.XPD.TOTL.GD.ZS,
    Sanità = SH.XPD.CHEX.GD.ZS,
    Popolazione = SP.POP.TOTL,
    Urbanizzazione = SP.URB.TOTL.IN.ZS,
    Energia_per_capite = EG.USE.PCAP.KG.OE,
    Esportazioni = NE.EXP.GNFS.ZS,
    IDE = BX.KLT.DINV.WD.GD.ZS,
    Servizi_trasporto = TX.VAL.TRAN.ZS.WT,
    Tasse = GC.TAX.YPKG.ZS,
    Tariffe_prodotti = TM.TAX.MANF.WM.AR.ZS
  )



gdp_per_capita<- gdp_per_capita %>%
  select(Country.Name, Year, GDP_P = Value)
  
merged_tutti<- dati_per_modello_tutti_i_paesi %>%
  inner_join(gdp_per_capita, by = c("Country.Name", "Year"))

modello_perfezionato_tutti <- lm( GDP_P ~ Disoccupazione + Investimenti + Istruzione + Sanità + Popolazione + Urbanizzazione + Energia_per_capite + Esportazioni + IDE + Servizi_trasporto + Tasse + Tariffe_prodotti, 
                           data = merged_tutti )
summary(modello_perfezionato_tutti)
#in questo caso l'R^2 (0.6755) è sensibilmente più alto rispetto al caso dei paesi più estremi
#i parametri che sembrano non superare il test t sono Investimenti, IDE, e Tariffe_prodotti
#interessante notare la relazione positiva tra Tasse sul reddito e Pil Pro Capite



#4.5 visualizzazione effetti singoli parametri



modello_gam_tutti <- gam( GDP_P ~ s(Disoccupazione) + s(Investimenti) + s(Istruzione) + s(Sanità) + s(Popolazione) + s(Urbanizzazione) + s(Energia_per_capite) + s(Esportazioni) + s(IDE) + s(Servizi_trasporto) + s(Tasse) + s(Tariffe_prodotti), 
                      data = merged_tutti )
draw(modello_gam_tutti) + theme_minimal()

