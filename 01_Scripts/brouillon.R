options(scipen = 999999)

# Package
library(dplyr)
library(tidyr)

capital <- 200000
tx <- 0.01
duree <- 20

# Formle mensualité : OK
mensualite <- (capital * tx/12) /(1 - (1 + tx/12)**-(12*duree))


# Coût des intérêts
12*duree * mensualite - capital
  

mensualité
946.8

interet
216.67

capital
730.14

# Montant des intérêt pour la 1e mensualité
((1+ tx)**(1/12)-1) * capital
# Montant du capital pour la 1e mensualité
mensualite - ((1+ tx)**(1/12)-1) * capital
# capital restant à rembourser après la 1e mensualité
capital - (mensualite - ((1+ tx)**(1/12)-1) * capital)


# Fonction : tableau amortissement emprunt
simul_emprunt <- function(capital_emprunte, taux, duree){
  
  # Nombre de mensualité
  nb_mensu <- duree * 12
  
  # Calcul de la mensualité
  mt_mensu <- (capital_emprunte * (taux / 12) * ((1+taux/12)**nb_mensu)) / ((1 + taux / 12) ** nb_mensu-1)
  
  # Tableau vide
  df <- tibble(
    id = seq(1,nb_mensu,1),
    capital_initial = numeric(nb_mensu),
    mensualite = numeric(nb_mensu),
    capital = numeric(nb_mensu),
    interet = numeric(nb_mensu),
    capital_restant = numeric(nb_mensu)
    )
  
  # Boucle pour les mensualités
  for (i in 1:nb_mensu){
    df[i,c("id")] <- i
    df[i,c("capital_initial")] <- ifelse(i == 1, capital_emprunte,df[i-1,c("capital_restant")])
    df[i,c("mensualite")] <- mt_mensu
    df[i,c("capital")] <- df[i,c("mensualite")] - ((1+ taux/12)-1) * df[i,c("capital_initial")]
    df[i,c("interet")] <- ((1+ taux/12)-1) * df[i,c("capital_initial")]
    df[i,c("capital_restant")] <- df[i,c("capital_initial")] - df[i,c("capital")]
  }
  
  return(round(df,3))
}

tableau <- simul_emprunt(200000,0.01,20)


sum(tableau$capital)
sum(tableau$interet)


data_test <- pivot_longer(tableau,cols = c("capital","capital_initial","mensualite","interet","capital_restant"),names_to = "type_montant")
ggplot(data = data_test, aes(x = id, y = value, fill = type_montant))+
  geom_col()
