library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
options(scipen = 9999999)

server <- function(input, output) {
    
    # Fonction emprunt + calcul amortissement
    simul_emprunt <- function(capital_emprunte, taux, duree){
        
        # Nombre de mensualité
        nb_mensu <- duree * 12
        
        # Correction du taux pour le ramener en pourcent
        taux <- taux/100
        
        # Taux périodique
        tx_period <- (1+taux/12)
        
        # Calcul de la mensualité
        mt_mensu <- (capital_emprunte * (taux / 12) * (tx_period**nb_mensu)) / (tx_period ** nb_mensu-1)
        
        # Vecteurs vides
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
    
    
    # Avec utilisation de l'apport
    graphique = reactiveVal()
    tableau = reactiveVal()
    mensualite = reactiveVal()
    interet = reactiveVal()
    cout_emprunt = reactiveVal()
    
    # Variables calculées à partir des paramètres
    observeEvent(input$capital_emprunte | input$taux | input$duree, {
        data = simul_emprunt(input$capital_emprunte - input$apport, input$taux, input$duree)
        tableau(data)
        graphique(ggplot(data, aes(x = id, y = capital_restant)) + geom_point())
        
        
        mt_mensu = data[1,c("mensualite")]
        mensualite(mt_mensu)
        
        interet(mt_mensu * input$duree * 12 - (input$capital_emprunte  - input$apport))
        
    }, ignoreNULL = F)
    
    
    output$cout_emprunt = renderPlot({
        
        data_cout_emprunt = tibble(
            Montant = c("Apport","Capital emprunté","Intérêt"),
            Valeur = as.numeric(c(input$apport, input$capital_emprunte - input$apport, interet())))
        
        ggplot(data = data_cout_emprunt,aes(x = Montant, y = Valeur))+
            geom_col(aes(fill = Montant),show.legend = FALSE)+
            geom_text(aes(label = paste(format(round(Valeur,0),big.mark = " "),"€")), vjust = -0.5)+
            scale_y_continuous(expand=c(0,0,0.08,0))+
            labs(x = NULL, y = NULL)+
            theme(axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_blank())
    })
    
    output$graphique = renderPlot({
        graphique()
    })
    
    output$tableau = renderTable({
        tableau()
    })
    
    output$mensualite = renderInfoBox({
        valueBox(
            "Mensualité",value = paste(format(round(as.numeric(mensualite()),0),big.mark = " "),"€"),
            color = "purple"
        )
    })
    
    output$interet = renderInfoBox({
        valueBox(
            "Total intérêt",value = paste(format(round(as.numeric(interet()),0),big.mark = " "),"€"),
            color = "teal"
        )
    })
    
    output$poids_interet = renderInfoBox({
        valueBox(
            "Total intérêt / capital emprunté",value = scales::percent(as.numeric(interet())/(input$capital_emprunte - input$apport)),
            color = "teal"
        )
    })
    
    # Box sur tx endettement avec apport - format conditionnel au niveau
    output$tx_endettement = renderInfoBox({
        
        if (as.numeric(mensualite()/input$revenu) <= 0.33)
        {
            valueBox(
                "Taux d'endettement",value = scales::percent(as.numeric(mensualite()/input$revenu),accuracy = 0.1),
                color = "green")   
        }
        else if (as.numeric(mensualite()/input$revenu) > 0.5)
        {
            valueBox(
                "Taux d'endettement",value = scales::percent(as.numeric(mensualite()/input$revenu),accuracy = 0.1),
                color = "red")
        } else {
            valueBox(
                "Taux d'endettement",value = scales::percent(as.numeric(mensualite()/input$revenu),accuracy = 0.1),
                color = "orange")
        }
    })
    
    
    
    
    
    
    
    # Sans utilisation de l'apport
    graphique_sa = reactiveVal()
    tableau_sa = reactiveVal()
    mensualite_sa = reactiveVal()
    interet_sa = reactiveVal()
    cout_emprunt_sa = reactiveVal()
    
    # Variables calculées à partir des paramètres
    observeEvent(input$capital_emprunte | input$taux | input$duree, {
        data_sa = simul_emprunt(input$capital_emprunte, input$taux, input$duree)
        tableau(data_sa)
        graphique(ggplot(data_sa, aes(x = id, y = capital_restant)) + geom_point())
        
        
        mt_mensu_sa = data_sa[1,c("mensualite")]
        mensualite_sa(mt_mensu_sa)
        
        interet_sa(mt_mensu_sa * input$duree * 12 - (input$capital_emprunte))
        
    }, ignoreNULL = F)
    
    
    output$cout_emprunt_sa = renderPlot({
        
        data_cout_emprunt_sa = tibble(
            Montant = c("Apport","Capital emprunté","Intérêt"),
            Valeur = as.numeric(c(input$apport - input$apport, input$capital_emprunte, interet_sa())))
        
        ggplot(data = data_cout_emprunt_sa,aes(x = Montant, y = Valeur))+
            geom_col(aes(fill = Montant),show.legend = FALSE)+
            geom_text(aes(label = paste(format(round(Valeur,0),big.mark = " "),"€")), vjust = -0.5)+
            scale_y_continuous(expand=c(0,0,0.08,0))+
            labs(x = NULL, y = NULL)+
            theme(axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_blank())
    })
    
    output$graphique_sa = renderPlot({
        graphique_sa()
    })
    
    output$tableau_sa = renderTable({
        tableau_sa()
    })
    
    output$mensualite_sa = renderInfoBox({
        valueBox(
            "Mensualité",value = paste(format(round(as.numeric(mensualite_sa()),0),big.mark = " "),"€"),
            color = "purple"
        )
    })
    
    output$interet_sa = renderInfoBox({
        valueBox(
            "Total intérêt",value = paste(format(round(as.numeric(interet_sa()),0),big.mark = " "),"€"),
            color = "teal"
        )
    })
    
    output$poids_interet_sa = renderInfoBox({
        valueBox(
            "Total intérêt / capital emprunté",value = scales::percent(as.numeric(interet_sa())/(input$capital_emprunte)),
            color = "teal"
        )
    })
    
    # Box sur tx endettement avec apport - format conditionnel au niveau
    output$tx_endettement_sa = renderInfoBox({
        
        if (as.numeric(mensualite_sa()/input$revenu) <= 0.33)
        {
            valueBox(
                "Taux d'endettement",value = scales::percent(as.numeric(mensualite_sa()/input$revenu),accuracy = 0.1),
                color = "green")   
        }
        else if (as.numeric(mensualite_sa()/input$revenu) > 0.5)
        {
            valueBox(
                "Taux d'endettement",value = scales::percent(as.numeric(mensualite_sa()/input$revenu),accuracy = 0.1),
                color = "red")
        } else {
            valueBox(
                "Taux d'endettement",value = scales::percent(as.numeric(mensualite_sa()/input$revenu),accuracy = 0.1),
                color = "orange")
        }
    })
}