
onglet2_ui <- fluidPage(
  
  tabsetPanel(
    id = "tabs_ong2",
    
    # Premier onglet : Accessibilité à l'offre de soin
    tabPanel("Accessibilité locale à l'offre de soin",  # Affichage en ligne
             sidebarLayout(
               sidebarPanel(
                 # Ajout d'une boîte avec du texte en gras et séparée
                 div(
                   style = "background-color: lightblue; border: 2px lightblue; padding: 10px; font-weight: bold; margin-bottom: 15px; color: black;",
                   "Carte du nombre de professionnels de santé dans un rayon donné autour de chaque carreau."
                 ),
                 radioButtons("choix_med", "Spécialités : ",
                              choices = c("Généraliste", sort(unique(bpe23 %>%
                                                                       filter(grd_ctg == "Médecin") %>%
                                                                       pull(libelle)))[!sort(unique(bpe23 %>%
                                                                                                      filter(grd_ctg == "Médecin") %>%
                                                                                                      pull(libelle))) %in% "Généraliste"]),
                              selected = "Généraliste"),  # Option par défaut, tu peux la changer selon ton besoin  # Extraire la colonne libelle
                   # <-- Correction de la parenthèse fermante
                 sliderInput("distance", "Sélectionnez un rayon :", 
                             min = 300, max = 800, value = 500, step = 100),
                 # Texte en italique pour la source des données
                 tags$div(
                   style = "font-style: italic; margin-top: 10px;margin-bottom: 10px",  # Style en italique avec marge
                   "Source : Base Permanente des Équipements 2023"
                 ),
                 plotOutput("graph_pauvrete")  # Ajout du graphique
               ),
               mainPanel(
                 tags$style(HTML("
          #map_annuaire {
            height: 80vh !important; /* 80% de la hauteur de la fenêtre */
          }
        ")),
                 leafletOutput("map_annuaire", height = "100%") # La hauteur est gérée par le CSS
                 
               )
             )
    ),
    
    # Second onglet : Accessibilité locale
    tabPanel("Accessibilité Potentielle Localisée (APL)",
             sidebarLayout(
               sidebarPanel(
                 div(
                   style = "background-color: lightblue; border: 2px lightblue; padding: 10px; font-weight: bold; margin-bottom: 15px; color: black;",
                   "Carte de l'Accessibilité Potentielle Localisée (APL). L’indice d'APL est calculé par carreau et combine à la fois la densité médicale et la pression exercée par la population."
                 ),
                 radioButtons("libelle_indice", "Spécialités : ",
                              choices = c("Généraliste", sort(unique(pression_med$libelle))[!sort(unique(pression_med$libelle)) %in% "Généraliste"]),
                              selected = "Généraliste"  # Sélection par défaut
                 ),
                 sliderInput("scale_selection", "Sélectionnez un rayon :",
                             min = 300, max = 800, value = 500, step = 100),
                 # Texte en italique pour la source des données
                 tags$div(
                   style = "font-style: italic; margin-top: 10px;margin-bottom: 10px",  # Style en italique avec marge
                   "Source : Base Permanente des Équipements 2023"
                 ),
               
               # <-- Correction de la parenthèse fermante
               ),  # <-- Correction pour fermer le sidebarPanel
               mainPanel(
                 tags$style(HTML("
          #map_indice {
            height: 80vh !important; /* 80% de la hauteur de la fenêtre */
          }
        ")),
                 leafletOutput("map_indice", height = "100%") # La hauteur est gérée par le CSS
               )
             )  # <-- Correction pour fermer le sidebarLayout
    )  # <-- Correction pour fermer le tabPanel
  )  # <-- Correction pour fermer le tabsetPanel
)
