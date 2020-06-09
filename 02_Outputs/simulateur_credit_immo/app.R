library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(jcan)

options(scipen = 999999915)
## app.R ##


ui <- dashboardPage(
    dashboardHeader(title = "Simulateur crédit immobilier"),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Synthèse", tabName = "Synthèse", icon = icon("dashboard")),
            menuItem("Tab. amortissement", icon = icon("th"), tabName = "tab_amortissement",
                     badgeLabel = "new", badgeColor = "green"),
            sliderInput("capital_emprunte","Capital emprunté", min = 0, max = 1000000, step = 10000, value = 200000),
            numericInput("taux","Taux d'intérêt", min = 0, max = 5, value = 1),
            numericInput("duree","Durée remboursement (an)", min = 1, max = 30, value = 20)
        )
    ),
    
    ## Body content
    dashboardBody(
        tabItems(
            tabItem(tabName = "Synthèse",
                    h2 = "Synthèse",
                    fluidRow(
                        box(title = "graphique",
                            plotOutput("graphique"),width = 8,solidHeader = TRUE, status = "primary"),
                        infoBoxOutput("interet")
                )),
        
            tabItem(tabName = "tab_amortissement",
                    h2 = "Tableau d'amortissement",
                    fluidRow(
        
            box(title = "tableau",
                 tableOutput("dataInput"),
                width = 12
                 )
            )))
))



server <- function(input, output) {




    # Fonction emprunt + calcul amortissement
    
    simul_emprunt <- function(capital_emprunte, taux, duree){
        # Nombre de mensualité
        nb_mensu <- duree * 12
        
        # Correction du taux pour le ramener en pourcent
        taux <- taux/100
        
        # Calcul de la mensualité
        mt_mensu <- (capital_emprunte * (taux / 12) * ((1+taux/12)**nb_mensu)) / ((1 + taux / 12) ** nb_mensu-1)
        
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
            df[i,c("capital")] <- df[i,c("mensualite")] - ((1+ taux/12)-1) * df[i,c("capital_initial")]
            df[i,c("interet")] <- ((1+ taux/12)-1) * df[i,c("capital_initial")]
            df[i,c("capital_restant")] <- df[i,c("capital_initial")] - df[i,c("capital")]
        }
        

        return(tibble(round(df,3)))
    }

    
    
    #  Objet réactif
    graphique = reactiveVal()
    dataInput = reactiveVal()
    interet = reactiveVal()
    
    # Calcul à chaque modification de paramétrage
    observeEvent(input$capital_emprunte | input$taux | input$duree, {
        data = tibble(simul_emprunt(input$capital_emprunte,input$taux,input$duree))
        dataInput(data)
        
        # Le graphique n'a pas vraiement d'intérêt, il n'apporte pas d'informations
        # Peut être un graphique récapitulatif par anneé avec le montant de capital remboursé, et d'intérêt payés
        # Avec un graphique qui récapitule à la fin le pourcentage de chaque type de dépenses dans le total du coût de l'emprunt
        graphique(ggplot()+
                      geom_line(data = data %>% select(id, capital_restant), aes(x = id, y = capital_restant))+
                      geom_col(data = pivot_longer(data,cols = c("capital","capital_initial","mensualite","interet","capital_restant"),names_to = "type_montant") %>%
                                   filter(type_montant %in% c("interet","capital")),
                               aes(x = id, y = value, fill = type_montant)))
        
        interet(sum(data$interet))
    }, ignoreNULL = F)
    
    # Rendu du graphique
    output$graphique = renderPlot({
        graphique()
    })
    
    # Rendu du tableau
    output$dataInput = renderTable({
        dataInput()
    })

    # InfoBox : Total intérêt
    output$interet <- renderInfoBox({
        infoBox(
            "Total intérêt", value = HTML(paste(format(interet(),big.mark = " ", scientific = FALSE, digits = 0),br(),"(soit ",scales::percent(interet()/(input$capital_emprunte + interet()))," du montant total à rembourser)")), icon = icon("list"),
            color = "purple"
        )
    })
    
}

shinyApp(ui, server)
