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
                                 numericInput("revenu","Revenu disponible (€)", min = 1, max = 10000, value = 4000),
                                 numericInput("apport","Apport (€)", min = 0, max = 150000, value = 40000),
                                 numericInput("capital_emprunte","Capital nécessaire (€)", min = 0, max = 1000000, value = 250000),
                                 numericInput("taux","Taux d'intérêt (%)", min = 0, max = 5, value = 1.3, step = 0.1),
                                 numericInput("assurance","Assurance emprunteur (%)", min = 0, max = 5, step = 0.1, value = 0.3),
                                 sliderInput("duree","Durée remboursement (an)", min = 5, max = 30, step = 1, value = 20, post = " ans"),
                                 numericInput("rendement","Taux de rémunération pour l'apport (%)", min = 0, max = 5, step = 0.1, value = 1.2)
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
                        box(title = "Résultats - sans utiliser l'apport", width = 6, solidHeader = TRUE, status = "primary",height = 410,
                            fluidRow(width = 4, 
                                     column(align ="center",width = 12,
                                            plotOutput("cout_emprunt_sa",height = 240))),
                            fluidRow(width = 4,
                                     valueBoxOutput(outputId = "mensualite_sa",width = 6),
                                     valueBoxOutput(outputId = "interet_sa",width = 6)
                            )),
                        # En bref ====
                        box(title = "En bref", width = 6, solidHeader = TRUE, status = "primary", height = 410,
                            valueBoxOutput(outputId = "mensu_max", width = 12),
                            valueBoxOutput(outputId = "rev_restant_mensu_max", width = 12),
                            valueBoxOutput(outputId = "rev_apport", width = 12))),
                    
                    
                    
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
                        box(title = "tableau",
                            width = 12,
                            tableOutput("tableau_sa")))
            )
        )
    )
)



