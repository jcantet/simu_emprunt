options(scipen = 999999)

# Package
library(dplyr)
library(tidyr)
library(plotly)

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





# Optimisation de la boucle car trop lente dans sa version initiale ====

    # Version initiale
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


    debut <- Sys.time()
    tableau <- simul_emprunt(200000,0.01,20)
    fin <- Sys.time()
    (temps_v0 <- fin - debut) # 2 seconde
    
    
    
    # Version améliorée 1 --> externalisation en dehors de la boucle de ce qui peut l'être
    simul_emprunt_v1 <- function(capital_emprunte, taux, duree){
      
      # Nombre de mensualité
      nb_mensu <- duree * 12
      
      # Taux périodique
      tx_period <- (1+taux/12)
      
      # Calcul de la mensualité
      mt_mensu <- (capital_emprunte * (taux / 12) * (tx_period**nb_mensu)) / (tx_period ** nb_mensu-1)
      
      # Tableau vide
      df <- tibble(
        id = seq(1,nb_mensu,1),
        capital_initial = numeric(nb_mensu),
        mensualite = mt_mensu,
        capital = numeric(nb_mensu),
        interet = numeric(nb_mensu),
        capital_restant = numeric(nb_mensu)
      )
      
      # Boucle pour les mensualités
      for (i in 1:nb_mensu){
        df[i,c("capital_initial")] <- ifelse(i == 1, capital_emprunte,df[i-1,c("capital_restant")])
        df[i,c("capital")] <- df[i,c("mensualite")] - (tx_period-1) * df[i,c("capital_initial")]
        df[i,c("interet")] <- (tx_period-1) * df[i,c("capital_initial")]
        df[i,c("capital_restant")] <- df[i,c("capital_initial")] - df[i,c("capital")]
      }
      
      return(round(df,3))
    }
    
    debut <- Sys.time()
    tableau <- simul_emprunt_v1(200000,0.01,20)
    fin <- Sys.time()
    (temps_v1 <- fin - debut) # 1.7 secondes
    
    
    
    # Version améliorée 2 --> solution précédente + utilisateur de vecteurs
    simul_emprunt_v2 <- function(capital_emprunte, taux, duree){
      
      # Nombre de mensualité
      nb_mensu <- duree * 12
      
      # Taux périodique
      tx_period <- (1+taux/12)
      
      # Calcul de la mensualité
      mt_mensu <- (capital_emprunte * (taux / 12) * (tx_period**nb_mensu)) / (tx_period ** nb_mensu-1)
      
      # Tableau vide
        id = seq(1,nb_mensu,1)
        capital_initial = numeric(nb_mensu)
        mensualite = rep(mt_mensu,nb_mensu)
        capital = numeric(nb_mensu)
        interet = numeric(nb_mensu)
        capital_restant = numeric(nb_mensu)
        
      
      # Boucle pour les mensualités
      for (i in 1:nb_mensu){
        capital_initial[i] <- ifelse(i == 1,capital_emprunte, capital_restant[i-1])
        capital[i] <- mensualite[i] - (tx_period-1) * capital_initial[i]
        interet[i] <- (tx_period-1) * capital_initial[i]
        capital_restant[i] <- capital_initial[i] - capital[i]
      }
      
      df <- astibble(cbind(id,capital_initial,mensualite,capital,interet,capital_restant))
      return(round(df,3))
    }
    
    debut <- Sys.time()
    tableau <- simul_emprunt_v2(200000,0.01,20)
    fin <- Sys.time()
    (temps_v2 <- fin - debut) # 0.02 secondes
    
    
    
# Test graphique
    data_cout_emprunt = tibble(
      Montant = c("Apport","Capital emprunté","Intérêt"),
      Valeur = c(40000,200000,21000))
    
    ggplot(data_cout_emprunt,aes(x=Montant, y = Valeur, fill = Montant))+
        geom_col()+
        theme_jcan()+
        scale_fill_jcan()+
        geom_text(aes(x = Montant, y = Valeur, label = Valeur), vjust = -0.5)
    
    plot_ly(
      data = data_cout_emprunt,
      x = ~Montant,
      y = ~Valeur,
      type = 'bar',
      color = ~Montant
      ) %>% 
      layout(title = "Ventilation coût crédit",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
  