library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
options(scipen = 9999999)


# On peut transformer les graphiques en option

# II) Serveur ====
server <- function(input, output, session) {
    timer <- reactiveTimer(750)
    
    # Infos ====
    # Taux à 10, 15, 20, 25, 30ans observé sur le marché (décembre 2020)
    taux_marche = c(0.0068, 0.0085, 0.0105, 0.0132, 0.0259)
    
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
    simul_emprunt <- function(capital_emprunte, taux, duree, tx_assurance = input$assurance){
        
        # Nombre de mensualité
        nb_mensu <- duree * 12
        
        # Correction du taux d'emprunt pour le ramener en pourcent
        taux <- taux / 100
    
        # Correction du taux d'assurance pour le ramener en pourcent
        tx_assurance <- tx_assurance / 100
        
        # Taux périodique
        tx_period <- (1+taux/12)
        
        # Calcul de la mensualité hors assurance
        mt_mensu <- (capital_emprunte * (taux / 12) * (tx_period**nb_mensu)) / (tx_period ** nb_mensu-1)
        
        # Vecteurs vides
        id_mensu = seq(1,nb_mensu,1)
        mois = rep(1:12, time = duree)
        annee = numeric(nb_mensu)
        capital_initial = numeric(nb_mensu)
        mensualite_ss_ass = rep(mt_mensu,nb_mensu)
        mensualite_yc_ass = numeric(nb_mensu)
        capital = numeric(nb_mensu)
        interet = numeric(nb_mensu)
        assurance = numeric(nb_mensu)
        capital_restant = numeric(nb_mensu)
        
        
        # Boucle pour les mensualités
        for (i in 1:nb_mensu){
            annee[i] = ceiling(id_mensu[i]/12)
            mois[i] <- as.integer(mois[i])
            capital_initial[i] <- ifelse(i == 1,capital_emprunte, capital_restant[i-1])
            capital[i] <- mensualite_ss_ass[i] - (tx_period-1) * capital_initial[i]
            interet[i] <- (tx_period-1) * capital_initial[i]
            assurance[i] <- as.numeric(tx_assurance * capital_emprunte / 12)
            mensualite_yc_ass[i] <- mensualite_ss_ass[i] + assurance[i]
            capital_restant[i] <- capital_initial[i] - capital[i]
        }
        
        df <- as_tibble(cbind(annee,mois,capital_initial,mensualite_ss_ass,mensualite_yc_ass,capital,interet,assurance,capital_restant))
        return(round(df,3))
    }
    
    # Fonction de mise en forme du tableau d'amortissement
    tab_amortissement_mef <- function(tableau){
        tableau_mef <- tableau %>%
            mutate(annee = as.integer(annee),
                   mois = as.integer(mois),
                   capital_initial = easy_format(capital_initial, type_out = "milliers", suffix = " €"),
                   mensualite = easy_format(mensualite_yc_ass, type_out = "milliers", suffix = " €"),
                   capital = easy_format(capital, type_out = "milliers", suffix = " €"),
                   interet = easy_format(interet, type_out = "milliers",decimal = 0,suffix = " €"),
                   assurance = easy_format(assurance, type_out = "milliers",decimal = 0,suffix = " €"),
                   capital_restant = easy_format(capital_restant, type_out = "milliers", suffix = " €")) %>% 
            select(annee, mois, capital_initial, mensualite, capital, interet, assurance, capital_restant)


        names(tableau_mef) <- c("Année","Mois","Capital initial","Mensualité","Part capital", "Part intérêt", "Part assurance", "Capital restant")
        return(tableau_mef)
    }
    
    # Iso-capital : fonction calcul des mensualités alternatives pour un capital donné
    alt_mensualite <- function(capital, revenu, tx_assurance = input$assurance){
        
        # Tableaux de référence
        tx_ref_mensu <- tibble(
            duree = c(10,15,20,25,30),
            taux = taux_marche,
            mensualite_ss_ass = as.numeric(5),
            mensualite_yc_ass = as.numeric(5),
            interet = as.numeric(5),
            tx_endettement = as.numeric(5)
        )
        
        for (i in seq(1,5,1)){
            
            # Calcul de la mensualité pour chaque période (hors assurance)
            tx_ref_mensu[i,c("mensualite_ss_ass")] <- (capital * tx_ref_mensu[i,c("taux")]/12) / (1 - (1 + tx_ref_mensu[i,c("taux")]/12)**-(12*tx_ref_mensu[i,c("duree")]))
            # Calcul de la mensualité pour chaque période (y compris assurance)
            tx_ref_mensu[i,c("mensualite_yc_ass")] <- tx_ref_mensu[i,c("mensualite_ss_ass")] + as.numeric(tx_assurance/100 * capital / 12)
            # Coût intérêt
            tx_ref_mensu[i,c("interet")] <- 12*tx_ref_mensu[i,c("duree")] * tx_ref_mensu[i,c("mensualite_ss_ass")] - capital
            # Taux d'endettement
            tx_ref_mensu[i,c("tx_endettement")] <- tx_ref_mensu[i,c("mensualite_yc_ass")] / revenu
            
        }
        
        tx_ref_mensu <- tx_ref_mensu %>% mutate(duree = easy_format(duree, type_out = "milliers" ,suffix = " ans"),
                                                taux = easy_format(taux, type_out = "pourcent",decimal = 2),
                                                mensualite = easy_format(mensualite_yc_ass, type_out = "milliers",suffix = " €"),
                                                interet = easy_format(interet,type_out = "milliers",decimal = 0,suffix = " €"),
                                                tx_endettement = easy_format(tx_endettement, type_out = "pourcent", decimal = 1)) %>% 
            select(duree,taux,mensualite,interet,tx_endettement)
        
        
        names(tx_ref_mensu) <- c("Durée","Taux","Mensualité","Intérêt","Taux d'endettement")
        
        return(tx_ref_mensu)
    }
    
    # Iso-mensualité : fonction pour calcul des capitaux alternatifs pour une mensualité donnée
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
            tx_ref_cap[i,c("interet")] <- 12*tx_ref_cap[i,c("duree")] * mensualite - tx_ref_cap[i,3]
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
    
    # Fonction pour représenter le coût de l'emprunt
    graphique_cout_total <- function(data, Montant = Montant, Valeur = Valeur, capital_emprunte = input$capital_emprunte){
        
        
        data$fraction <- data$Valeur / sum(data$Valeur) # Pourcentage
        data$ymax <- cumsum(data$fraction) # Pourcentage cumulé (Haut de chaque rectangle)
        data$ymin <- c(0, head(data$ymax, n = -1)) # Position basse de chaque rectangle
        data$labelPosition <- (data$ymax + data$ymin) / 2 # Position des étiquettes
        data$label <- paste0(data$Montant, "\n", 
                             easy_format(data$Valeur,type_out = "milliers", decimal = 0, suffix = "€")) # Mise en forme label
        
        ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Montant)) +
            geom_rect() +
            geom_label( x=4, aes(y=labelPosition, label=label), size = 4.5, label.size = 0) +
            scale_fill_brewer(palette=4) +
            coord_polar(theta="y") +
            xlim(c(2, 4)) +
            theme_void()+
            theme(legend.position = "none")

    }

    # Obj : Simuler un emprunt avec utilisation de l'apport et durée de remboursement identique au scénario sans apport (variation mensualité)
    graphique = reactiveVal()
    tableau = reactiveVal()
    mensualite_ss_ass = reactiveVal()
    mensualite_yc_ass = reactiveVal()
    interet = reactiveVal()
    assurance = reactiveVal()
    cout_emprunt = reactiveVal()
    
    graphique_im = reactiveVal()
    tableau_im = reactiveVal()
    mensualite_im = reactiveVal()
    mensualite_ss_ass_im = reactiveVal()
    mensualite_yc_ass_im = reactiveVal()
    interet_im = reactiveVal()
    assurance_im = reactiveVal()
    cout_emprunt_im = reactiveVal()
    duree_im = reactiveVal()
    
    graphique_sa = reactiveVal()
    tableau_sa = reactiveVal()
    mensualite_ss_ass_sa = reactiveVal()
    mensualite_yc_ass_sa = reactiveVal()
    interet_sa = reactiveVal()
    assurance_sa = reactiveVal()
    cout_emprunt_sa = reactiveVal()
    tab_alt_mensu = reactiveVal()
    tab_alt_capital = reactiveVal()
    
    apport_ajuste = reactiveVal()
    
    # 1) Elements calculés à partir des paramètres, et mis à jour dès qu'un paramètre est modifié ====
    observeEvent(input$capital_emprunte | input$taux | input$duree | input$apport | input$assurance, {
 
        # Le code est stoppé tout pendant que la valeur renseignée pour le capital est nulle
        ifelse(is.na(input$capital_emprunte),
               req(input$capital_emprunte),
               input$capital_emprunte)
        
        
        # Quand le capital emprunté est inférieur à l'apport disponible, l'apport n'est pas consommé entièrement
        apport_ajuste(min(input$apport,input$capital_emprunte))
        
        
        # A) Iso durée avec apport ====
            # Résultats de la simulation et attribution à un objet réactif
            data = simul_emprunt(input$capital_emprunte - apport_ajuste(), input$taux, input$duree)
            
            # Montant de la mensualité y compris assurance pour l'affichage dans infobox et attribution à un objet réactif
            mt_mensu_yc_ass = data[1,c("mensualite_yc_ass")]
            mensualite_yc_ass(mt_mensu_yc_ass)
            
            # Montant de la mensualité hors assurance pour le calcul du montant total des intérêts
            mt_mensu_ss_ass = data[1,c("mensualite_ss_ass")]
            mensualite_ss_ass(mt_mensu_ss_ass)
            
            # Montant total des intérêt pour l'affichage dans infobox et attribution à un objet réactif
            interet(mensualite_ss_ass() * input$duree * 12 - (input$capital_emprunte  - apport_ajuste()))
        
            # Montant de l'assurance
            assurance((input$capital_emprunte - apport_ajuste()) * input$assurance / 100 * input$duree)
        
        # B) Iso mensu apport ====
            # Besoin de simuler sans apport pour obtenir la mensualité cible
            # Résultats de la simulation et attribution à un objet réactif
            data_sa = simul_emprunt(input$capital_emprunte, input$taux, input$duree)
            
            # Montant de la mensualité y compris assurance pour l'affichage dans infobox et attribution à un objet réactif
            mt_mensu_yc_ass_sa = data_sa[1,c("mensualite_yc_ass")]
            mensualite_yc_ass_sa(mt_mensu_yc_ass_sa)
            
            # Durée alternative pour un emprunt à mensualité identique (scénario sans apport) et utilisation de l'apport pour réduire le capital emprunté
            # Ajout du max avec la mensualite_sa pour prévoir les cas où capital_emprunte - apport ajuste est négatif (quand l'utilisateur est en train de saisir une valeur par ex)
            duree <- alt_duree(max(input$capital_emprunte - apport_ajuste(), as.numeric(mensualite_yc_ass_sa())), input$taux, mensualite_yc_ass_sa())
            duree_im(duree)
            
            # Résultats de la simulation et attribution à un objet réactif
            data_im = simul_emprunt(input$capital_emprunte  - apport_ajuste(), input$taux, duree_im())
            tableau_im(data_im)
            
            # Montant de la mensualité pour l'affichage dans infobox et attribution à un objet réactif
            mt_mensu_yc_ass_im = data_im[1,c("mensualite_yc_ass")]
            mensualite_yc_ass_im(mt_mensu_yc_ass_im)
            
            # Montant de la mensualité hors assurance pour le calcul du montant total des intérêts
            mt_mensu_ss_ass_im = data_im[1,c("mensualite_ss_ass")]
            mensualite_ss_ass_im(mt_mensu_ss_ass_im)
            
            # Montant total des intérêt pour l'affichage dans infobox et attribution à un objet réactif
            interet_im(mensualite_ss_ass_im() * duree_im() * 12 - (input$capital_emprunte - apport_ajuste()))
        
            # Montant de l'assurance
            assurance_im((input$capital_emprunte - apport_ajuste()) * input$assurance / 100 * duree_im())
            
        # C) Sans apport ====
            # Résultats de la simulation et attribution à un objet réactif
            data_sa = simul_emprunt(input$capital_emprunte, input$taux, input$duree)
            tableau_sa(data_sa)
            
            # Montant de la mensualité y compris assurance pour l'affichage dans infobox et attribution à un objet réactif
            mt_mensu_yc_ass_sa = data_sa[1,c("mensualite_yc_ass")]
            mensualite_yc_ass_sa(mt_mensu_yc_ass_sa)

            # Montant de la mensualité hors assurance pour le calcul du montant total des intérêts
            mt_mensu_ss_ass_sa = data_sa[1,c("mensualite_ss_ass")]
            mensualite_ss_ass_sa(mt_mensu_ss_ass_sa)
            
            # Montant total des intérêt pour l'affichage dans infobox et attribution à un objet réactif
            interet_sa(mensualite_ss_ass_sa() * input$duree * 12 - (input$capital_emprunte))
            
            # Simulation pour un même capital emprunté des différentes mensualités possibles (durées de remboursement différentes)
            data_alt_mensu = alt_mensualite(input$capital_emprunte,input$revenu)
            tab_alt_mensu(data_alt_mensu)
            
            # Simulation pour une même mensualité des différents montants de capital empruntable possible (durées de remboursement différentes)
            data_alt_capital = alt_capital(mensualite_yc_ass_sa())
            tab_alt_capital(data_alt_capital)

            # Montant de l'assurance
            assurance_sa(input$capital_emprunte * input$assurance / 100 * input$duree)
            
            
    }, ignoreNULL = F)
    
    # 2) Graphique coût de l'emprunt ====
    output$cout_emprunt = renderPlot({
        req(input$capital_emprunte) # Affichage si le capital emprunté n'est pas nul
        data_cout_emprunt = tibble(
            Montant = c("Apport","Capital emprunté","Intérêt", "Assurance"),
            Valeur = as.numeric(c(apport_ajuste(), input$capital_emprunte - apport_ajuste(), interet(), assurance())))
        
        graphique_cout_total(data_cout_emprunt)
    })
    
    output$cout_emprunt_im = renderPlot({
        req(input$capital_emprunte) # Affichage si le capital emprunté n'est pas nul
        data_cout_emprunt_im = tibble(
            Montant = c("Apport","Capital emprunté","Intérêt","Assurance"),
            Valeur = as.numeric(c(apport_ajuste(), input$capital_emprunte - apport_ajuste(), interet_im(), assurance_im())))
        
        graphique_cout_total(data_cout_emprunt_im)
    })
    
    output$cout_emprunt_sa = renderPlot({
        req(input$capital_emprunte) # Affichage si le capital emprunté n'est pas nul
        data_cout_emprunt_sa = tibble(
            Montant = c("Capital emprunté","Intérêt","Assurance"),
            Valeur = as.numeric(c(input$capital_emprunte, interet_sa(), assurance_sa())))
        
        graphique_cout_total(data_cout_emprunt_sa)
        
    })
    
    # 3) Tableau d'amortissement
    output$tableau_sa = renderDataTable(options = list(pageLength = 12,
                                                       dom = 'tp',
                                                       autoWidth = TRUE,
                                                       filter = "top",
                                                       columnDefs = list(className = 'dt-center', width = '200px', targets = c(1, 8))),{
        
        tab_amortissement_mef(tableau_sa())
    })
    
    
    # 4) Box montant mensualité et sur tx endettement avec apport - format conditionnel au niveau ====
    output$mensualite = renderInfoBox({
        
            valueBox(
                subtitle = paste0("Mensualité, soit un taux d'endettement de ",scales::percent(as.numeric(mensualite_yc_ass()/input$revenu),accuracy = 0.1)),
                value = paste0(format(round(as.numeric(mensualite_yc_ass()),0),big.mark = " ")," €",
                               " (",paste(format(round(as.numeric(mensualite_yc_ass() - mensualite_yc_ass_sa()),0),big.mark = " "),"€"),")"),
                color = ifelse(as.numeric(mensualite_yc_ass()/input$revenu) <= 0.33,"green","orange"))
    })
    
    output$mensualite_im = renderInfoBox({
        valueBox(
            subtitle = paste0("Mensualité, soit un taux d'endettement de ",scales::percent(as.numeric(mensualite_yc_ass_im()/input$revenu),accuracy = 0.1)),
            value = paste(format(round(as.numeric(mensualite_yc_ass_im()),0),big.mark = " "),"€"),
            color = ifelse(as.numeric(mensualite_yc_ass_im()/input$revenu) <= 0.33,"green","orange"))   
        
    }) 
    
    output$mensualite_sa = renderInfoBox({
        valueBox(
            subtitle = paste0("Mensualité, soit un taux d'endettement de ", scales::percent(as.numeric(mensualite_yc_ass_sa()/input$revenu),accuracy = 0.1)),
            value = paste(format(round(as.numeric(mensualite_yc_ass_sa()),0),big.mark = " "),"€"),
            color = ifelse(as.numeric(mensualite_yc_ass_sa()/input$revenu) <= 0.33,"green","orange"))   
    })
    
    # 5) Montant total intérêt & poids des intérêts par rapport au capital emprunté====
    # Coût total (renommer les variables si choix retenu)
    output$interet = renderInfoBox({
        valueBox(
            paste0("Coût total pour emprunter un capital de ", easy_format(input$capital_emprunte - apport_ajuste(),type_out = "milliers", decimal = 0, suffix = "€")),
            value = easy_format(interet() + assurance(),type_out = "milliers", decimal = 0, suffix = "€"),
            color = "teal")
    })
    
    # Coût total (renommer les variables si choix retenu)
    output$interet_im = renderInfoBox({
        valueBox(
            paste0("Coût total pour emprunter un capital de ", easy_format(input$capital_emprunte - apport_ajuste(),type_out = "milliers", decimal = 0, suffix = "€")),
            value = easy_format(interet_im() + assurance_im(),type_out = "milliers", decimal = 0, suffix = "€"),
            color = "teal")
    })
    
    # Coût total (renommer les variables si choix retenu)
    output$interet_sa = renderInfoBox({
        valueBox(
            paste0("Coût total pour emprunter un capital de ", easy_format(input$capital_emprunte,type_out = "milliers", decimal = 0, suffix = "€")),
            value = easy_format(interet_sa() + assurance_sa(),type_out = "milliers", decimal = 0, suffix = "€"),
            color = "teal")
    })
    
    # 6) Durée alternative d'emprunt grâce à l'utilisation de l'apport
    output$duree_im = renderInfoBox({
        valueBox(
            "Durée de l'emprunt",value = paste(as.numeric(duree_im()) %/% 1,"ans et ", round((as.numeric(duree_im()) %% 1)*12,0), " mois"),
            color = "purple"
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

    # Revenu restant après déduction de la mensualité max
    output$rev_restant_mensu_max <- renderInfoBox({
        valueBox(
            subtitle = "Revenu dispo avec mensualité max",
            value = paste(format(round(as.numeric(input$revenu * 0.67),0),big.mark = " "),"€"),
            color = "navy"
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
              easy_format(as.numeric(mensualite_yc_ass_sa()),type_out = "milliers", decimal = 0, suffix = " €"),
              ":")
    })
    
    # F) Onglet tableau d'amortissement
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


