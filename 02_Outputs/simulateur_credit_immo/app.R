library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(jcan)
extrafont::loadfonts(device = "win")
options(scipen = 9999999)

# Créer un tableau intermédiaire pour afficher le coût du crédit emprunt (en €)

## app.R ##


ui <- dashboardPage(
    dashboardHeader(title = "Simulateur crédit immobilier"),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Synthèse", tabName = "Synthèse", icon = icon("dashboard")),
            menuItem("Tab. amortissement", icon = icon("th"), tabName = "tab_amortissement",
                     badgeLabel = "new", badgeColor = "green")
        )
    ),
        
    ## Body content
    dashboardBody(
        tabItems(
            # Onglet 1 : Synthèse
            tabItem(tabName = "Synthèse",
                    h2 = "Synthèse",
                    
                    # Paramètres à rentrer par l'utilisateur en haut de page
                    fluidRow(
                        box(title = "Paramètres", width = 12, solidHeader = TRUE, status = "primary", background = "navy",
                            column(width = 2, numericInput("revenu","Revenu disponible (hors loyer) (€)", min = 1, max = 10000, value = 3000)),
                            column(width = 2, numericInput("apport","Apport (€)", min = 0, max = 150000, value = 40000)),
                            column(width = 2, numericInput("capital_emprunte","Capital nécessaire (€)", min = 0, max = 1000000, value = 200000)),
                            column(width = 2, numericInput("taux","Taux d'intérêt (%)", min = 0, max = 5, value = 1)),
                            column(width = 2, sliderInput("duree","Durée remboursement (an)", min = 5, max = 30, step = 5, value = 20, post = " ans")),
                            column(width = 2, numericInput("assurance","Assurance emprunteur (%)", min = 0, max = 5, value = 0.3)))
                    ),
                    
                    # Résultats de la simulation avec les paramètres renseignés - en prenant en compte l'apport
                    fluidRow(
                        box(title = "Résultats - en utilisant l'apport", width = 12, solidHeader = TRUE, status = "primary",height = 300,
                            column(width = 6, plotOutput("cout_emprunt",height = 240)),
                            column(width = 4,
                                valueBoxOutput(outputId = "tx_endettement", width = 6),
                                valueBoxOutput(outputId = "mensualite",width = 6),
                                valueBoxOutput(outputId = "interet",width = 6),
                                valueBoxOutput(outputId = "poids_interet",width = 6)))
                            ),
                    
                    # Résultats de la simulation avec les paramètres renseignés - sans prendre en compte l'apport
                    fluidRow(
                        box(title = "Résultats - sans utiliser l'apport", width = 12, solidHeader = TRUE, status = "primary",height = 300,
                            column(width = 6, plotOutput("cout_emprunt_sa",height = 240)),
                            column(width = 4,
                                   valueBoxOutput(outputId = "tx_endettement_sa", width = 6),
                                   valueBoxOutput(outputId = "mensualite_sa",width = 6),
                                   valueBoxOutput(outputId = "interet_sa",width = 6),
                                   valueBoxOutput(outputId = "poids_interet_sa",width = 6)))
                        )
                    ),
        
            # Onglet 2 : Tableau d'amortissement
            tabItem(tabName = "tab_amortissement",
                    h2 = "Tableau d'amortissement",
                    fluidRow(
                        box(title = "tableau",
                            tableOutput("tableau"),
                            width = 12)
                        )
                    )
            )
        )
    )





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
            theme_jcan()+
            scale_fill_jcan()+
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
            Valeur = as.numeric(c(input$apport, input$capital_emprunte, interet_sa())))
        
        ggplot(data = data_cout_emprunt_sa,aes(x = Montant, y = Valeur))+
            geom_col(aes(fill = Montant),show.legend = FALSE)+
            geom_text(aes(label = paste(format(round(Valeur,0),big.mark = " "),"€")), vjust = -0.5)+
            theme_jcan()+
            scale_fill_jcan()+
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

shinyApp(ui, server)
