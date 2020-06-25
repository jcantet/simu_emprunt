library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
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