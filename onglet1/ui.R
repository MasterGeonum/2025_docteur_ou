onglet1_ui <- fluidPage(
  
  # Onglets
  tabsetPanel(
    id = "tabs_ong1",  
    tabPanel("Localiser les établissements de santé", value = "tab1"),
    tabPanel("Localiser les médecins", value = "tab2"),
  ),
  
  # Affichage de côté
  sidebarLayout(
    sidebarPanel(
      # Afficher les établissements de santé
      conditionalPanel(
        condition = "input.tabs_ong1 == 'tab1'",  
        checkboxGroupInput("type_eta", "Etablissements de santé : ",
                           choices = unique(
                             bpe23 %>%
                               filter(grd_ctg == "Etablissement") %>%  # Filtrer par catégorie
                               pull(libelle)  # Extraire la colonne libelle
                           )
        ), 
        # Texte en italique pour la source des données
        tags$div(
          style = "font-style: italic; margin-top: 10px;margin-bottom: 10px",  # Style en italique avec marge
          "Source : Base Permanente des Équipements 2023"
        ),
        
        # Sélecteur d'arrondissement
        selectInput(
          inputId = "arrondissement_eta", 
          label = "Statistiques par arrondissement et quartiers prioritaires", 
          choices = c(
            "Ville de Lyon",
            sort(unique(arrondissement$NOM_COM.x)),
            paste0("QPV : ", sort(unique(qpv$lib_qp)))), # Ajouter "QPV :" devant chaque lib_qp
          selected = "Ville de Lyon"  # Sélection par défaut
        ),
        # Histogramme des établissements de santé
        plotOutput("histogram_eta")
      ),
      
      # Afficher les médecins
      conditionalPanel(
        condition = "input.tabs_ong1 == 'tab2'",  
        
        # Choisir le jeu de données à explorer : BPE, Annuaire ou Comparaison
        radioButtons("BD_med", "Choisissez la base de données à visualiser :",
                     choices = list("Base permanente des équipements (BPE)" = "BPE",
                                    "Annuaire de santé" = "Annuaire",
                                    "Comparaison des deux" = "Comparaison"),
                     selected = "BPE"),  # Sélection par défaut
        

        # Ajout d'une boîte avec du texte en gras et séparée
        div(
          style = "background-color: lightblue; border: 2px lightblue; padding: 10px; font-weight: bold; margin-bottom: 15px; color: black;",
          "La taille des cercles est proportionelle au nombre de médecins par adresse"
        ),

        
        # Ajouter une ligne horizontale
        HTML("<hr style='height:3px; border-width:0; background-color:#286c7c; margin-top: 20px; margin-bottom: 20px;'>"),
        
        # Afficher les options de BPE seulement si le choix est BPE
        conditionalPanel(
          condition = "input.BD_med == 'BPE'",  
          checkboxGroupInput(
            inputId = "type_med_bpe", 
            label = HTML("<b style='font-size: 16px;color:#286c7c'>Localisation et nombre de médecins</b> <br> Spécialités (BPE) :"),  # Gras et taille de police augmentée
            choices = sort(unique(
              bpe23_agreg %>%
                filter(grd_ctg == "Médecin") %>%  # Filtrer par catégorie
                pull(libelle)  # Extraire la colonne libelle
            ))
          ),
          # Sélecteur d'arrondissements
          selectInput(
            inputId = "arrondissement_bpe", 
            label = "Statistiques par arrondissement et quartier prioritaire", 
            choices = c(
              "Ville de Lyon",
              sort(unique(arrondissement$NOM_COM.x)),
              paste0("QPV : ", sort(unique(qpv$lib_qp)))),
            selected = "Ville de Lyon"  # Sélection par défaut
          ),
          # Histogramme du nombre de médecins bpe
          plotOutput("histogram_med_bpe")
        ), 
        
        # Afficher les options de l'annuaire seulement si le choix est Annuaire
        conditionalPanel(
          condition = "input.BD_med == 'Annuaire'",  
          checkboxGroupInput("type_med_annuaire", 
                             label = HTML("<b style='font-size: 16px;color:#286c7c'>Localisation et nombre de médecins</b> <br> Spécialités (Annuaire Santé): "),
                             choices = sort(unique(annuaire24_agr$libelle))
          ),
          
          # Sélecteur d'arrondissements
          selectInput(
            inputId = "arrondissement_ann", 
            label = "Statistiques par arrondissement et quartier prioritaire", 
            choices = c(
              "Ville de Lyon",
              sort(unique(arrondissement$NOM_COM.x)),
              paste0("QPV : ", sort(unique(qpv$lib_qp)))),
            selected = "Ville de Lyon"  # Sélection par défaut
          ),
          # Histogramme du nombre de médecins bpe
          plotOutput("histogram_med_annuaire")
        ), 
        

        
        
        
        # Afficher les options de comparaison
        conditionalPanel(
          condition = "input.BD_med == 'Comparaison'",  
          radioButtons("type_med_comparaison", 
                       label = HTML("<b style='font-size: 16px;color:#286c7c'>Comparaison des bases de données</b> <br> Spécialités : "),
                       choices = unique(annuaire24_agr$libelle)),
          div(
            style = "gap: 0px; border: 0px solid #ccc; padding: 2px; border-radius: 2px; background-color: #e3f0ef;",
            checkboxInput("show_bpe", "Affichez les médecins : BPE", value = TRUE),
            checkboxInput("show_annuaire", "Affichez les médecins : Annuaire", value = TRUE)
          ),
          # Sélecteur d'arrondissements
          selectInput(
            inputId = "arrondissement_comp", 
            label = "Statistiques par arrondissement", 
            choices = c(
              "Ville de Lyon",
              sort(unique(arrondissement$NOM_COM.x)),
              paste0("QPV : ", sort(unique(qpv$lib_qp)))),
            selected = "Ville de Lyon"  # Sélection par défaut
          ),
          # Histogramme du nombre de médecins bpe
          plotOutput("histogram_comparaison")
        )
      ),
      
      ),  # Fermeture du sidebarPanel
    
    # Zone principale de la carte
    mainPanel(
      tags$style(HTML("
      #map_ong1 {
        height: 80vh !important; /* 80% de la hauteur de la fenêtre */
      }
    ")),
      leafletOutput("map_ong1", height = "100%") # La hauteur est gérée par le CSS
    )
  )  # Fermeture du sidebarLayout
) # Fermeture du fluidPage
