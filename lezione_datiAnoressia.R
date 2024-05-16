#caricamento librerie----

library(readxl)
library(tidyverse)
library(janitor)


library(usethis)
library(gitcreds)
library(git2r)

gitcreds_set()
use_git()
git2r::config(user.name = "AridellOglio")
git2r::config(user.email = "arianna.delloglio2@unibo.it")
use_github()

#prima sezione: esempio matrice --------

#la matrice è un oggetto atomico; se ha bisogno di far convivere oggetti
#di diverso tipo conviene usare dataframe
a <- matrix(c(1,2,3,4,5,6),3,2,byrow=F,dimnames=list(c(1,2,3),c("a","b")))
x <- seq(1,130,6)
x[7] <- NA 

#seconda sezione: lavorare con file Excel ------------

pazienti <- read_excel("DatiAnoressia.xlsx")
pazienti <- clean_names(pazienti)
names(pazienti)
pazienti <- rename(pazienti, BMI = bmi) #per rinominare la singola colonna

#sintassi manipolazione tabelle
#funzione di manipolazione(tabella,comando di modifica)
pazienti <- mutate(pazienti,log_bmi = log(bmi)) #sto aggiungendo una colonna log_bmi
pazienti <- relocate(pazienti, duratamalattia_mesi) #sposta la colonna "durata..." in prima posizione
pazienti <- relocate(pazienti, bmi, .after = duratamalattia_mesi) #sposta la col bmi dopo durata...
pazienti <- mutate(pazienti, somma_inutile=log_bmi+bmi)
pazienti <- mutate(pazienti,
                   sottotipo = recode(sottotipo,
                                                "1" = "Restrittivo",
                                                "2" = "Vomito"),
                   insight = recode(insight,
                                              "1" = "Assente",
                                              "3" = "Parziale",
                                              "4" = "Presente")) 
#terza sessione: report tabellari --------------
table(pazienti$bmi, pazienti$insight)

tabyl(pazienti,bmi)
tabyl(pazienti,bmi,insight)

#quarta sessione: esempi di pipe -------------

pazienti |> tabyl(insight,sottotipo) |> 
  adorn_totals(where = "row")

pazienti |> tabyl(insight,sottotipo) |> 
  adorn_totals(where = "col")

pazienti |> tabyl(insight,sottotipo) |> 
  adorn_totals(where = c("row", "col")) |> 
  adorn_percentages(denominator = "all") |> 
  adorn_pct_formatting()

# quinta sessione: categorizzare una variabile in classi -------------
pazienti <- mutate(pazienti, bmi_gruppi = cut(bmi, breaks = 3)) #crea una colonna con 3 classi di bmi di uguale ampiezza

pazienti <- mutate(pazienti, bmi_gruppi_pariFreq = cut_number(bmi, n = 5))

tabyl(pazienti, bmi_gruppi_pariFreq)

# sesta sessione: selezione di un sottoinsieme di variabili ----------------------
tb_temp <- select(pazienti,sottotipo,bmi)
tb_temp <- select(pazienti,-bmi,-insight,-eta_primo_ricovero_anni)
tb_temp <- filter(pazienti,bmi<13 | bmi>15)
summarise(pazienti,mean(bmi))


#settima sessione: grafici base -----------
hist(pazienti$bmi,
     xlab = "Body Mass Index",
     ylab = "Frequenza assoluta",
     main = "Andamento del BMI")
