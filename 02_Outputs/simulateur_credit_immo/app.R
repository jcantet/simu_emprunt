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




# II) Serveur ====
server <- function(input, output) {
    
    # Infos ====
    taux_marche = c(0.0095, 0.012, 0.013, 0.016, 0.0195)
    
    # Fonctions génériques ====
    
        # Fonction pour mettre en forme les résultats
        easy_format <- function(variable, type_out, decimal = 0, suffix = NULL){
            
            # Format pourcent
            if (type_out == "pourcent"){
                variable = paste0(format(x = round(variable*100, decimal)),"%")
                
                # Format milliers
            } else if (type_out == "milliers"){
                variable = paste0(format(x = round(variable, decimal), big.mark = " "), suffix)
            }
            
            # Gestion des erreurs : si l'utilisateur rentre un paramètre qui n'est pas prévu
            if ((type_out %in% c("pourcent","milliers")) == FALSE){
                print(paste0("type_out ", type_out," n'existe pas"))
            } else {
                return(variable)
            }
            
        }
        
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
        
        # Fonction calcul des mensualités alternatives pour un capital donné
        alt_mensualite <- function(capital, revenu){
            
            # Tableaux de référence
            tx_ref_mensu <- tibble(
                duree = c(10,15,20,25,30),
                taux = taux_marche,
                mensualite = as.numeric(5),
                interet = as.numeric(5),
                tx_endettement = as.numeric(5)
            )
            
            for (i in seq(1,5,1)){
                
                # Calcul de la mensualité pour chaque période
                tx_ref_mensu[i,c("mensualite")] <- (capital * tx_ref_mensu[i,c("taux")]/12) / (1 - (1 + tx_ref_mensu[i,c("taux")]/12)**-(12*tx_ref_mensu[i,c("duree")]))
                # Coût intérêt
                tx_ref_mensu[i,c("interet")] <- 12*tx_ref_mensu[i,c("duree")] * tx_ref_mensu[i,c("mensualite")] - capital
                # Taux d'endettement
                tx_ref_mensu[i,c("tx_endettement")] <- tx_ref_mensu[i,c("mensualite")] / revenu
                
            }
            
            tx_ref_mensu <- tx_ref_mensu %>% mutate(duree = easy_format(duree, type_out = "milliers" ,suffix = " ans"),
                                                    taux = easy_format(taux, type_out = "pourcent",decimal = 2),
                                                    mensualite = easy_format(mensualite, type_out = "milliers",suffix = " €"),
                                                    interet = easy_format(interet,type_out = "milliers",decimal = 0,suffix = " €"),
                                                    tx_endettement = easy_format(tx_endettement, type_out = "pourcent", decimal = 1))
            
            
            names(tx_ref_mensu) <- c("Durée","Taux","Mensualité","Intérêt", "Taux d'endettement")
                
            return(tx_ref_mensu)
        }
        
        # Fonction pour calcul des capitaux alternatifs pour une mensualité donnée
        alt_capital <- function(mensualite){
            
            # Tableaux de référence
            tx_ref_cap <- tibble(
                duree = c(10,15,20,25,30),
                taux = taux_marche,
                capital = as.numeric(5),
                interet = as.numeric(5)
            )
            
            for (i in seq(1,5,1)){
                # Capital possible
                tx_ref_cap[i,c("capital")] <- mensualite * (1 - (1 + tx_ref_cap[i,c("taux")]/12)**(-12*tx_ref_cap[i,c("duree")])) / tx_ref_cap[i,c("taux")]*12
                # Coût intérêt
                tx_ref_cap[i,c("interet")] <-12*tx_ref_cap[i,c("duree")] * mensualite - tx_ref_cap[i,3]
            }
            
            tx_ref_cap <- tx_ref_cap %>% mutate(duree = easy_format(duree, type_out = "milliers", suffix = " ans"),
                                                taux = easy_format(taux, type_out = "pourcent", decimal = 2),
                                                capital = easy_format(capital, type_out = "milliers", suffix = "€"),
                                                interet = easy_format(interet, type_out = "milliers", suffix = "€"))
    
            names(tx_ref_cap) <- c("Durée","Taux","Capital emprunté","Intérêt")
            
            
            return(tx_ref_cap)
        }
        
        # Fonction pour calcul de durée nécessaire pour un capital donné étant donné la mensualité
        alt_duree <- function(capital, tx, mensualite){
            tx <- tx / 100
            val <- as.numeric(round((log(-mensualite / (tx/12*capital - mensualite))/log(1 + tx/12) / (12)),1))
            return(val)
        }
    

    
    # A) Iso durée avec apport ====
    # Obj : Simuler un emprunt avec utilisation de l'apport et durée de remboursement identique au scénario sans apport (variation mensualité)
    graphique = reactiveVal()
    tableau = reactiveVal()
    mensualite = reactiveVal()
    interet = reactiveVal()
    cout_emprunt = reactiveVal()
    
    # 1) Elements calculés à partir des paramètres, et mis à jour dès qu'un paramètre est modifié ====
    observeEvent(input$capital_emprunte | input$taux | input$duree | input$apport, {
        # Résultats de la simulation et attribution à un objet réactif
        data = simul_emprunt(input$capital_emprunte - input$apport, input$taux, input$duree)
        tableau(data)
        
        # Montant de la mensualité pour l'affichage dans infobox et attribution à un objet réactif
        mt_mensu = data[1,c("mensualite")]
        mensualite(mt_mensu)
        
        # Montant total des intérêt pour l'affichage dans infobox et attribution à un objet réactif
        interet(mt_mensu * input$duree * 12 - (input$capital_emprunte  - input$apport))
        
    }, ignoreNULL = F)
    
    # 2) Graphique coût de l'emprunt ====
    output$cout_emprunt = renderPlot({
        
        data_cout_emprunt = tibble(
            Montant = c("Apport","Capital emprunté","Intérêt"),
            Valeur = as.numeric(c(input$apport, input$capital_emprunte - input$apport, interet())))
        
        ggplot(data = data_cout_emprunt,aes(x = Montant, y = Valeur))+
            geom_col(aes(fill = Montant),show.legend = FALSE)+
            geom_text(aes(label = paste(format(round(Valeur,0),big.mark = " "),"€")), vjust = -0.5, size = 4.5)+
            scale_y_continuous(limits = c(0,input$capital_emprunte),expand=c(0,0,0.08,0))+
            labs(x = NULL, y = NULL)+
            theme(axis.text.y = element_blank(),
                  axis.text.x = element_text(face = "bold", size = 13),
                  axis.ticks = element_blank(),
                  panel.grid.major.y = element_line(colour = "grey95"),
                  panel.grid.major.x = element_blank(),
                  panel.background = element_rect(fill = "#FEFBF8"))
    })
    
    # 3) Tableau d'amortissement ====
    output$tableau = renderTable({
        tableau()
    })
    
    # 4) Box montant mensualité et sur tx endettement avec apport - format conditionnel au niveau ====
    output$mensualite = renderInfoBox({
        
        if (as.numeric(mensualite()/input$revenu) <= 0.33)
        {
            valueBox(
                subtitle = paste0("Mensualité, soit un taux d'endettement de ",scales::percent(as.numeric(mensualite()/input$revenu),accuracy = 0.1)),
                value = paste0(format(round(as.numeric(mensualite()),0),big.mark = " ")," €",
                              " (",paste(format(round(as.numeric(mensualite() - mensualite_sa()),0),big.mark = " "),"€"),")"),
                color = "green")   
        }
        else if (as.numeric(mensualite()/input$revenu) > 0.5)
        {
            valueBox(
                subtitle = paste0("Mensualité, soit un taux d'endettement de ",scales::percent(as.numeric(mensualite()/input$revenu),accuracy = 0.1)),
                value = paste0(format(round(as.numeric(mensualite()),0),big.mark = " ")," €",
                               " (",paste(format(round(as.numeric(mensualite() - mensualite_sa()),0),big.mark = " "),"€"),")"),
                color = "red")
        } else {
            valueBox(
                subtitle = paste0("Mensualité, soit un taux d'endettement de ",scales::percent(as.numeric(mensualite()/input$revenu),accuracy = 0.1)),
                value = paste0(format(round(as.numeric(mensualite()),0),big.mark = " ")," €",
                               " (",paste(format(round(as.numeric(mensualite() - mensualite_sa()),0),big.mark = " "),"€"),")"),
                color = "orange")
        }
    })
    
    # 5) Montant total intérêt & poids des intérêts par rapport au capital emprunté====
    output$interet = renderInfoBox({
        valueBox(
            paste0("Total intérêt,",
                   " soit ", scales::percent(as.numeric(interet())/(input$capital_emprunte - input$apport),accuracy = 0.1)," du capital"),
            value = paste0(format(round(as.numeric(interet()),0),big.mark = " "),"€"),
            color = "teal"
        )
    })
    
    
    # B) Iso mensu apport ====
    # Obj : Simuler un emprunt avec utilisation de l'apport et mensualité identique au scénario sans apport (variation durée remboursement)
    graphique_im = reactiveVal()
    tableau_im = reactiveVal()
    mensualite_im = reactiveVal()
    interet_im = reactiveVal()
    cout_emprunt_im = reactiveVal()
    duree_im = reactiveVal()
    

    # 1) Elements calculés à partir des paramètres, et mis à jour dès qu'un paramètre est modifié ====
    observeEvent(input$capital_emprunte | input$taux | input$duree | input$apport, {
        
        # Besoin de simuler sans apport pour obtenir la mensualité cible
            # Résultats de la simulation et attribution à un objet réactif
            data_sa = simul_emprunt(input$capital_emprunte, input$taux, input$duree)
            tableau_sa(data_sa)
            
            # Montant de la mensualité pour l'affichage dans infobox et attribution à un objet réactif
            mt_mensu_sa = data_sa[1,c("mensualite")]
            mensualite_sa(mt_mensu_sa)
        
        
        # Durée alternative pour un emprunt à mensualité identique (scénarion sans apport) et utilisation de l'apport pour réduire le capital emprunté
        duree <- alt_duree(input$capital_emprunte - input$apport, input$taux, mensualite_sa())
        duree_im(duree)
        
        
        
        # Résultats de la simulation et attribution à un objet réactif
        data_im = simul_emprunt(input$capital_emprunte - input$apport, input$taux, duree_im())
        tableau_im(data_im)
        
        # Montant de la mensualité pour l'affichage dans infobox et attribution à un objet réactif
        mt_mensu_im = data_im[1,c("mensualite")]
        mensualite_im(mt_mensu_im)
        
        # Montant total des intérêt pour l'affichage dans infobox et attribution à un objet réactif
        interet_im(mt_mensu_im * duree_im() * 12 - (input$capital_emprunte - input$apport))
        
    }, ignoreNULL = F)
    
    # 2) Graphique coût de l'emprunt ====
    output$cout_emprunt_im = renderPlot({
        
        data_cout_emprunt_im = tibble(
            Montant = c("Apport","Capital emprunté","Intérêt"),
            Valeur = as.numeric(c(input$apport, input$capital_emprunte - input$apport, interet_im())))
        
        ggplot(data = data_cout_emprunt_im,aes(x = Montant, y = Valeur))+
            geom_col(aes(fill = Montant),show.legend = FALSE)+
            geom_text(aes(label = paste(format(round(Valeur,0),big.mark = " "),"€")), vjust = -0.5, size = 4.5)+
            scale_y_continuous(limits = c(0,input$capital_emprunte),expand=c(0,0,0.08,0))+
            labs(x = NULL, y = NULL)+
            theme(axis.text.y = element_blank(),
                  axis.text.x = element_text(face = "bold", size = 13),
                  axis.ticks = element_blank(),
                  panel.grid.major.y = element_line(colour = "grey95"),
                  panel.grid.major.x = element_blank(),
                  panel.background = element_rect(fill = "#FEFBF8"))
    })
    
    # 3) Tableau d'amortissement ====
    output$tableau_im = renderTable({
        tableau_im()
    })
    
    # 4) Box sur montant mensualité et sur tx endettement avec apport - format conditionnel au niveau
    output$mensualite_im = renderInfoBox({
        
        if (as.numeric(mensualite_im()/input$revenu) <= 0.33)
        {
            valueBox(
                subtitle = paste0("Mensualité, soit un taux d'endettement de ",scales::percent(as.numeric(mensualite_im()/input$revenu),accuracy = 0.1)),
                value = paste(format(round(as.numeric(mensualite_im()),0),big.mark = " "),"€"),
                color = "green")   
        }
        else if (as.numeric(mensualite_im()/input$revenu) > 0.5)
        {
            valueBox(
                subtitle = paste0("Mensualité, soit un taux d'endettement de ",scales::percent(as.numeric(mensualite_im()/input$revenu),accuracy = 0.1)),
                value = paste(format(round(as.numeric(mensualite_im()),0),big.mark = " "),"€"),
                color = "red")
        } else {
            valueBox(
                subtitle = paste0("Mensualité, soit un taux d'endettement de ",scales::percent(as.numeric(mensualite_im()/input$revenu),accuracy = 0.1)),
                value = paste(format(round(as.numeric(mensualite_im()),0),big.mark = " "),"€"),
                color = "orange")
        }
    })    
    
    # 5) Montant total intérêt & Poids des intérêts par rapport au capital emprunté ====
    output$interet_im = renderInfoBox({
        valueBox(
            paste0("Total intérêt,",
                   " soit ", scales::percent(as.numeric(interet_im())/(input$capital_emprunte - input$apport),accuracy = 0.1)," du capital"),
            value = paste(format(round(as.numeric(interet_im()),0),big.mark = " "),"€"),
            color = "teal"
        )
    })
    
    # 6) Durée alternative d'emprunt grâce à l'utilisation de l'apport
    output$duree_im = renderInfoBox({
        valueBox(
            "Durée de l'emprunt",value = paste(as.numeric(duree_im()) %/% 1,"ans et ", round((as.numeric(duree_im()) %% 1)*12,0), " mois"),
            color = "purple"
        )
    })
    

    
    
    # C) Sans apport ====
    graphique_sa = reactiveVal()
    tableau_sa = reactiveVal()
    mensualite_sa = reactiveVal()
    interet_sa = reactiveVal()
    cout_emprunt_sa = reactiveVal()
    tab_alt_mensu = reactiveVal()
    tab_alt_capital = reactiveVal()
    
    # 1) Elements calculés à partir des paramètres, et mis à jour dès qu'un paramètre est modifié ====
    observeEvent(input$capital_emprunte | input$taux | input$duree, {
        # Résultats de la simulation et attribution à un objet réactif
        data_sa = simul_emprunt(input$capital_emprunte, input$taux, input$duree)
        tableau_sa(data_sa)
        
        # Montant de la mensualité pour l'affichage dans infobox et attribution à un objet réactif
        mt_mensu_sa = data_sa[1,c("mensualite")]
        mensualite_sa(mt_mensu_sa)
        
        # Montant total des intérêt pour l'affichage dans infobox et attribution à un objet réactif
        interet_sa(mt_mensu_sa * input$duree * 12 - (input$capital_emprunte))
        
        # Simulation pour un même capital emprunté des différentes mensualités possibles (durées de remboursement différentes)
        data_alt_mensu = alt_mensualite(input$capital_emprunte,input$revenu)
        tab_alt_mensu(data_alt_mensu)

        # Simulation pour une même mensualité des différents montants de capital empruntable possible (durées de remboursement différentes)
        data_alt_capital = alt_capital(mt_mensu_sa)
        tab_alt_capital(data_alt_capital)
        
    }, ignoreNULL = F)
    
    # 2) Graphique coût de l'emprunt ====
    output$cout_emprunt_sa = renderPlot({
        
        data_cout_emprunt_sa = tibble(
            Montant = c("Apport","Capital emprunté","Intérêt"),
            Valeur = as.numeric(c(input$apport - input$apport, input$capital_emprunte, interet_sa())))
        
        ggplot(data = data_cout_emprunt_sa,aes(x = Montant, y = Valeur))+
            geom_col(aes(fill = Montant),show.legend = FALSE)+
            geom_text(aes(label = paste(format(round(Valeur,0),big.mark = " "),"€")), vjust = -0.5, size = 4.5)+
            scale_y_continuous(limits = c(0,input$capital_emprunte),expand=c(0,0,0.08,0))+
            labs(x = NULL, y = NULL)+
            theme(axis.text.y = element_blank(),
                  axis.text.x = element_text(face = "bold", size = 13),
                  axis.ticks = element_blank(),
                  panel.grid.major.y = element_line(colour = "grey95"),
                  panel.grid.major.x = element_blank(),
                  panel.background = element_rect(fill = "#FEFBF8"))
    })
    
    # 3) Tableau d'amortissement
    output$tableau_sa = renderTable({
        tableau_sa()
    })
    
    # 4) Box sur montant mensualité et tx endettement avec apport - format conditionnel au niveau
    output$mensualite_sa = renderInfoBox({
        
        if (as.numeric(mensualite_sa()/input$revenu) <= 0.33)
        {
            valueBox(
                subtitle = paste0("Mensualité, soit un taux d'endettement de ", scales::percent(as.numeric(mensualite_sa()/input$revenu),accuracy = 0.1)),
                value = paste(format(round(as.numeric(mensualite_sa()),0),big.mark = " "),"€"),
                color = "green")   
        }
        else if (as.numeric(mensualite_sa()/input$revenu) > 0.5)
        {
            valueBox(
                subtitle = paste0("Mensualité, soit un taux d'endettement de ", scales::percent(as.numeric(mensualite_sa()/input$revenu),accuracy = 0.1)),
                value = paste(format(round(as.numeric(mensualite_sa()),0),big.mark = " "),"€"),
                color = "red")
        } else {
            valueBox(
                subtitle = paste0("Mensualité, soit un taux d'endettement de ", scales::percent(as.numeric(mensualite_sa()/input$revenu),accuracy = 0.1)),
                value = paste(format(round(as.numeric(mensualite_sa()),0),big.mark = " "),"€"),
                color = "orange")
        }
    })
    
    # 5) Montant total intérêt & Poids des intérêts par rapport au capital emprunté ====
    output$interet_sa = renderInfoBox({
        valueBox(
            paste0("Total intérêt,",
                   " soit ", scales::percent(as.numeric(interet_sa())/(input$capital_emprunte),accuracy = 0.1)," du capital"),
            value = paste(format(round(as.numeric(interet_sa()),0),big.mark = " "),"€"),
            color = "teal"
        )
    })

    

    
    
    
    # D) En bref ====
    # Différence d'intérêts totaux entre la solution avec apport et sans apport
    output$diff_tot_int <- renderInfoBox({
        valueBox(
            subtitle = "Différence coût de l'emprunt à iso durée",
            value = paste(format(round(as.numeric(interet_sa() - interet()),0),big.mark = " "),"€"),
            color = "teal"
        )
        
    })
    

    
    
    # Mensualité max compte tenu du revenu
    output$mensu_max <- renderInfoBox({
        valueBox(
            subtitle = "Mensualité théorique max (33% d'endettement)",
            value = paste(format(round(as.numeric(0.33 * input$revenu),0),big.mark = " "),"€"),
            color = "purple"
        )
    })
    
    # Revenu restant après déduction de la mensualité avec l'option 1
    output$rev_restant_mensu_1 <- renderInfoBox({
        valueBox(
            subtitle = "Revenu dispo - sans apport",
            value = paste(format(round(as.numeric(input$revenu - mensualite_sa()),0),big.mark = " "),"€"),
            color = "navy"
        )
    })
    
    
    # Revenu restant après mensualité IM
    output$rev_restant_mensu_im <- renderInfoBox({
        valueBox(
            subtitle = "Revenu dispo après mensualité",
            value = paste(format(round(as.numeric(input$revenu - mensualite_im()),0),big.mark = " "),"€"),
            color = "navy"
        )
    })
    
    # Revenu restant après mensualité iso-durée
    output$rev_restant_mensu <- renderInfoBox({
        valueBox(
            subtitle = "Revenu dispo - avec apport",
            value = paste(format(round(as.numeric(input$revenu - mensualite()),0),big.mark = " "),"€"),
            color = "navy"
        )
    })
    
    # Revenu restant après déduction de la mensualité max
    output$rev_restant_mensu_max <- renderInfoBox({
        valueBox(
            subtitle = "Revenu dispo avec mensualité max",
            value = paste(format(round(as.numeric(input$revenu * 0.67),0),big.mark = " "),"€"),
            color = "navy"
        )
    })
    
    # Revenu généré par le placement de l'apport
    output$rev_apport <- renderInfoBox({
        valueBox(
            subtitle = paste0("Rémunération pour le placement de ",format(round(as.numeric(input$apport),0),big.mark = " ")," € sur la période de l'emprunt"),
            value = paste(format(round(as.numeric(input$apport * (1 + input$rendement/100)**(input$duree) - input$apport),0),big.mark = " "),"€"),
            color = "yellow"
        )
    })
        
    
    
    
    
    # E) Solutions alternatives ====
    
    # Tableau mensualités alternatives pour un même capital (variation durée)
    output$tab_alt_mensu <- renderTable(width = 500, spacing = "l", hover = TRUE, align = "ccrrc",{
        tab_alt_mensu()
    })
    
    # Texte descriptif pour tableau précédent
    output$texte_alt_mensu <- renderText({
        paste("Simulation pour un capital emprunté de ",
              easy_format(input$capital_emprunte,type_out = "milliers", decimal = 0, suffix = " €"),
              ":")
    })
    
    # Tableau des capitaux empruntables pour la mensualité de référence (variation durée)
    output$tab_alt_capital <- renderTable(width = 400, spacing = "l", hover = TRUE, align = "ccrr",{
        tab_alt_capital()
    })
    
    # Texte descriptif pour tableau précédent
    output$texte_alt_capital <- renderText({
        paste("Emprunt possible avec une mensualité de ",
              easy_format(as.numeric(mensualite_sa()),type_out = "milliers", decimal = 0, suffix = " €"),
              ":")
    })
    
    # Téléchargement tableau d'amortissement ====
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("tableau_amortissement", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(tableau_sa(), file, row.names = FALSE)
        }
    )
    
}

shinyApp(ui, server)
