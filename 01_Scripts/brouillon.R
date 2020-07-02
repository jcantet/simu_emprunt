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
      
      df <- as_tibble(cbind(id,capital_initial,mensualite,capital,interet,capital_restant))
      return(round(df,3))
    }
    
    debut <- Sys.time()
    tableau <- simul_emprunt_v2(200000,0.01,20)
    fin <- Sys.time()
    (temps_v2 <- fin - debut) # 0.02 secondes
    
    
    
# Test graphique ====
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
  

    
    
# Test du tableau avec plusieurs options de durée et de capital pour une même mensualité =====

capital <- 200000
tx <- 0.01
duree <- 20

data_emp <- simul_emprunt_v2(capital,tx,duree)

# Mensualité ciblée :
data_emp$mensualite[1]
# Taux de référence par période
tx_ref <- tibble(
  duree = c(10,15,20,25,30),
  taux = c(0.009, 0.010, 0.010, 0.015, 0.020),
  capital = as.numeric(5),
  interet = as.numeric(5)
)

# On cherche pour la même mensualité, ce qu'on peut emprunter le même capital 

  # Formule mensualité
  mensualite <- (capital * tx/12) / (1 - (1 + tx/12)**-(12*duree))

  # Formule pour récupérer le capital pour une mensualité donnée
  mensualite * (1 - (1 + tx/12)**(-12*duree)) / tx*12
  
  # Transformation en fonction pour le faire boucler sur différentes périodes
  alt_capital <- function(mensualite){
    
    for (i in seq(1,5,1)){
      # Capital possible
      tx_ref[i,c("capital")] <- mensualite * (1 - (1 + tx_ref[i,c("taux")]/12)**(-12*tx_ref[i,c("duree")])) / tx_ref[i,c("taux")]*12
      # Coût intérêt
      tx_ref[i,c("interet")] <-12*tx_ref[i,c("duree")] * mensualite - tx_ref[i,3]
    }
    print(tx_ref)
  }

alt_capital(mensualite)    




# Test du tableau qui pour un capital donné, donne les mensualités possibles sur différents périodes, avec le taux d'endettement en face ====


capital <- 200000
tx <- 0.01
duree <- 20

data_emp <- simul_emprunt_v2(capital,tx,duree)

# Formule mensualité
mensualite <- (capital * tx/12) / (1 - (1 + tx/12)**-(12*duree))

# Transformation en fonction pour boucler sur différentes périodes

  # Taux de référence par période
  tx_ref_mensu <- tibble(
    duree = c(10,15,20,25,30),
    taux = c(0.009, 0.010, 0.010, 0.015, 0.020),
    mensualite <- as.numeric(5),
    interet = as.numeric(5),
    tx_endettement = as.numeric(5)
  )

  alt_mensualite <- function(capital){
    
    for (i in seq(1,5,1)){
      
      # Calcul de la mensualité pour chaque période
      tx_ref_mensu[i,c("mensualite")] <- (capital * tx_ref_mensu[i,c("taux")]/12) / (1 - (1 + tx_ref_mensu[i,c("taux")]/12)**-(12*tx_ref_mensu[i,c("duree")]))
      # Coût intérêt
      tx_ref_mensu[i,c("interet")] <-12*tx_ref_mensu[i,c("duree")] * mensualite - tx_ref_mensu[i,3]
      
    }
    print(tx_ref_mensu)
  }


  alt_mensualite(200000)
  
  
  
# Test de la fonction qui donne pour une mensualité donnée, un capital donné, un taux d'intérêt donné la durée nécessaire
  
  # Fonction pour calcul de durée nécessaire pour un capital donné étant donné la mensualité
  
  capital <- 200000
  tx <- 0.01
  mensualite <- 966
  
  # Formule mensualité qu'il faut transformer pour trouver la durée
  mensualite <- (capital * tx/12) / (1 - (1 + tx/12)**-(12*duree)) 
    # Résultat
    log(-mensualite / (tx/12*capital - mensualite))/log(1 + tx/12)
    
    alt_duree <- function(capital, tx, mensualite){
      
      return(round((log(-mensualite / (tx/12*capital - mensualite))/log(1 + tx/12) / (12)),1))
      
    }
  
    alt_duree(200000,0.01,920)
    
    alt_duree <- function(capital, tx, mensualite){
      tx <- tx / 100
      
      val <- as.integer(round(log( - mensualite / (tx / 12 * capital - mensualite)) / log(1 + tx/12) / (12),0))
      return(val)
    }
    
    typeof(alt_duree(200000,01,920))

    
# Création fonction pour mise en forme tableau
tableau_test <- tibble(
  duree = c(10,15,20,25,30),
  taux = c(0.009, 0.010, 0.010, 0.015, 0.020),
  mensualite = c(500,600,700,1000,1200),
  interet = as.numeric(c(12000.525,15623,12466,12452.55556,36455)),
  tx_endettement = c(0.3350,0.45555,0.46987,0.54,0.6)
)
tableau_test


easy_format <- function(variable, type_out, decimal = 0, suffix = NULL){
  
  # Format pourcent
  if (type_out == "pourcent"){
    variable = paste0(format(x = round(variable*100, decimal)),"%")
  
  # Format milliers
  } else if (type_out == "milliers"){
    variable = paste0(format(x = round(variable, decimal), big.mark = " ", justify = "right"), suffix)
  }

  # Gestion des erreurs : si l'utilisateur rentre un paramètre qui n'est pas prévu
  if ((type_out %in% c("pourcent","milliers")) == FALSE){
  print(paste0("type_out ", type_out," n'existe pas"))
  } else {
    return(variable)
  }
  
}


easy_format(tableau_test$tx_endettement, type_out = "pourcent", decimal = 1)
easy_format(tableau_test$interet, type_out = "milliers", decimal = 3, suffix = " €")


easy_format(tableau_test$taux, type_out = "porcent",decimal = 1)


# Deploiement shiny en ligne ====
install.packages('rsconnect')

rsconnect::setAccountInfo(name='jordan-cantet',
                        token='71EF130FB41AA85438631BEBB72BC858',
                        secret='NU0TO4d61usucZAgcNQKMXrjwb8n4kvypysrYeiQ')   
library(rsconnect)    

rsconnect::deployApp('02_Outputs/simulateur_credit_immo_io')    
    