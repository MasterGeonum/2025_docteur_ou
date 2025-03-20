onglet1_bis_ui <- fluidPage(

  tags$style(HTML("
    .italic-text {
      font-style: italic;
      margin-top: 10px;
      margin-bottom: 10px;
      color: #286c7c;
    }
  ")),
  
  # Onglets
  tabsetPanel(
    id = "tabs_ong1bis",  
    tabPanel("Historique de la densité médicale", value = "tab3"),
    tabPanel("Evolution de la densité médicale", value = "tab4")
  ),
  
  # Affichage de côté
  sidebarLayout(
    sidebarPanel(
      
      #Historique des medecins
  
      # Iris : Sélectionner l'année à partir des colonnes qui finissent par 2016, 2018, 2021, 2023
      # Sélecteur d'année
      conditionalPanel(
        condition = "input.tabs_ong1bis == 'tab3'",  
        radioButtons("year", "Sélectionnez une année :", 
                     choices = c("Aucune", "2016", "2018", "2021", "2023"),
                     inline = TRUE,
                     selected = "Aucune")
      ),
      
      # Sélecteurs apparaissant si "Aucune" n'est pas sélectionnée
      conditionalPanel(
        condition = "input.tabs_ong1bis == 'tab3' && input.year != 'Aucune'",  
        selectInput(
          "type_med_iris", 
          "Spécialités :", 
          choices = choices_medecins
        ),
        # Utilisation de class "italic-text" pour l'élément textOutput
        tags$div(class = "italic-text", textOutput("message_gynécologue")),  # Message explicatif avec style
        
        # Texte en italique pour la source des données
        tags$div(
          style = "font-style: italic; margin-top: 10px;margin-bottom: 10px",  # Style en italique avec marge
          "Source : Base Permanente des Équipements 2023"
        ),
        
        # Sélecteur d'arrondissements
        selectInput(
          inputId = "arrondissement_historique", 
          label = "Statistiques par arrondissement", 
          choices = c("Ville de Lyon", sort(unique(arrondissement$NOM_COM.x))),  # Liste des arrondissements
          selected = "Ville de Lyon"  # Sélection par défaut
        ),
        
        # Histogramme
        
        plotOutput("dens_histo")
      ),
      
      # Evolution des médecins
      conditionalPanel(
        condition = "input.tabs_ong1bis == 'tab4'",  
        radioButtons("evo_med", "Spécialités :",
                     choices = subset(choices_medecins, 
                                      names(choices_medecins) != "Gynécologue")
        ),
        
        # Texte en italique pour la source des données
        tags$div(
          style = "font-style: italic; margin-top: 10px;margin-bottom: 10px",  # Style en italique avec marge
          "Source : Base Permanente des Équipements 2023"
        ),
        
        
        # Sélecteur d'arrondissements
        selectInput(
          inputId = "arrondissement_evo", 
          label = "Statistiques par arrondissement", 
          choices = c("Ville de Lyon", sort(unique(arrondissement$NOM_COM.x))),  # Liste des arrondissements
          selected = "Ville de Lyon"  # Sélection par défaut
        ),
        
        # Histogramme
        
        plotOutput("evo_histo")

      )
    ),  # Fermeture du sidebarPanel
    
    # Zone principale de la carte
    mainPanel(
      tags$style(HTML("
      #map_ong1bis {
        height: 80vh !important; /* 80% de la hauteur de la fenêtre */
      }
    ")),
      leafletOutput("map_ong1bis", height = "100%") # La hauteur est gérée par le CSS
    )
  )  # Fermeture du sidebarLayout
) # Fermeture du fluidPage
