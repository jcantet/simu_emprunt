library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
options(scipen = 9999999)


# Créer un tableau intermédiaire pour afficher le coût du crédit emprunt (en €)

## app.R ##

# I) Interface utilisateur ====
ui <- dashboardPage(
    dashboardHeader(title = "Simulateur crédit immobilier"),
    ## A) Sidebar content ====
    dashboardSidebar(width = 220,
                     sidebarMenu(tags$script(HTML("$('body').addClass('fixed');")), # Barre latérale fixée
                                 # 1) Liste Onglets ====
                                 menuItem("Synthèse", tabName = "Synthèse", icon = icon("dashboard")),
                                 menuItem("Tab. amortissement", icon = icon("th"), tabName = "tab_amortissement",
                                          badgeLabel = "new", badgeColor = "green"),
                                 
                                 # 2) Paramètres à rentrer par l'utilisateur en haut de page ====
                                 numericInput("revenu","Revenu disponible (€)", min = 1, max = 10000, value = 4400),
                                 numericInput("apport","Apport potentiel (€)", min = 0, max = 150000, value = 40000),
                                 numericInput("capital_emprunte","Capital nécessaire (€)", min = 0, max = 1000000, value = 250000),
                                 numericInput("taux","Taux d'intérêt (%)", min = 0, max = 5, value = 1.3, step = 0.1),
                                 numericInput("assurance","Assurance emprunteur (%)", min = 0, max = 5, step = 0.01, value = 0.34),
                                 sliderInput("duree","Durée du prêt (an)", min = 5, max = 30, step = 1, value = 25, post = " ans")
                     )
    ),
    
    ## B) Body content ====
    dashboardBody(
        tabItems(
            # Onglet 1 : Synthèse ====
            tabItem(tabName = "Synthèse",
                    h2 = "Synthèse",
                    
                    # Elements marquants ====
                    fluidRow(
                        # Résultats - sans prendre en compte l'apport ====
                        box(title = "Coût du projet - sans utiliser l'apport", width = 8, solidHeader = TRUE, status = "primary",height = 420,
                            fluidRow(width = 8, 
                                     column(align ="center",width = 8,
                                            plotOutput("cout_emprunt_sa",height = 350)),
                                     column(width = 4,
                                            fluidRow(
                                                valueBoxOutput(outputId = "mensualite_sa",width = 12)),
                                            fluidRow(
                                                valueBoxOutput(outputId = "interet_sa",width = 12))
                            ))),
                        # En bref ====
                        box(title = "En bref", width = 4, solidHeader = TRUE, status = "primary", height = 410,
                            valueBoxOutput(outputId = "mensu_max", width = 12),
                            valueBoxOutput(outputId = "rev_restant_mensu_max", width = 12),
                            # valueBoxOutput(outputId = "rev_apport", width = 12)
                            )),
                    
                    
                    
                    # Résultats des simulations ====
                    fluidRow(
                        # Résultats - en utilisant l'apport ====
                        tabBox(title = "Résultats - en utilisant l'apport", side = "right", selected = "Iso durée", width = 6, height = 420,
                               
                               # Résultats - en prenant en compte l'apport à iso durée ====
                               tabPanel("Iso durée", 
                                        fluidRow(
                                            column(align ="center",width = 12,
                                                   plotOutput("cout_emprunt",height = 240))),
                                        fluidRow(
                                            valueBoxOutput(outputId = "mensualite",width = 6),
                                            valueBoxOutput(outputId = "interet",width = 6),
                                            valueBoxOutput(outputId = "diff_mensu",width = 6))),
                               
                               # Résultats - en prenant en compte l'apport à iso mensualité ====
                               tabPanel("Iso mensualité",
                                        fluidRow(
                                            column(align ="center",width = 12,
                                                   plotOutput("cout_emprunt_im",height = 240))),
                                        fluidRow(
                                            valueBoxOutput(outputId = "duree_im",width = 6),
                                            valueBoxOutput(outputId = "interet_im",width = 6)))  
                        ),
                        # Propositions alternatives ====
                        tabBox(title = "Propositions alternatives", side = "right", selected = "Iso capital", width = 6, height = 420,
                               tabPanel("Iso capital" ,
                                        textOutput("texte_alt_mensu"),
                                        tableOutput(outputId = "tab_alt_mensu")),
                               tabPanel("Iso mensualité", 
                                        textOutput("texte_alt_capital"),
                                        tableOutput(outputId = "tab_alt_capital")))
                    )),
            
            
            # Onglet 2 : Tableau d'amortissement ====
            tabItem(tabName = "tab_amortissement",
                    h2 = "Tableau d'amortissement",
                    
                    # Pour les options (téléchargement, avec ou sans apport) ====
                    fluidRow(
                        box(title = "Options",
                            width = 12,
                            downloadButton("downloadData", "Téléchargement"))),
                    
                    # Affichage du tableau d'amortissement ====
                    fluidRow(
                        box(title = "Tableau d'amortissement mensuel",
                            width = 12,
                            dataTableOutput("tableau_sa")))
            )
        )
    )
)



