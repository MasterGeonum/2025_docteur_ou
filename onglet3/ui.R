ui_3 <- fluidPage(
  # 🏷️ Onglets principaux
  tabsetPanel(
    id = "tabs_ong3",  
    tabPanel("Accessibilité par transport"),
    tabPanel("Disponibilité par jour et heure")
  ),
  
  # **1️⃣ Contenu de l'onglet "Accessibilité par transport"**
  conditionalPanel(
    condition = "input.tabs_ong3 == 'Accessibilité par transport'",
    sidebarLayout(
      sidebarPanel(
        width = 4,  # ✅ 30% de l'écran
        
        # ✅ Type de ressource (Médecins ou Établissements) - Toujours affiché
        radioButtons("etamed", "Type de ressource de santé :", 
                     choices = list("Médecins" = "med", "Établissements de santé" = "eta"),
                     selected = "med"), 
        tags$div(
          style = "font-style: italic; margin-top: 10px;margin-bottom: 10px",  # Style en italique avec marge
          "Source : Base Permanente des Équipements 2023"
        ),
        HTML("<hr style='height:3px; border-width:0; background-color:#286c7c; margin-top: 20px; margin-bottom: 20px;'>"),
        
        
        # ✅ Sélection des spécialités - Affichée uniquement en mode "Carte" et "Médecins"
        conditionalPanel(
          condition = "!input.toggle_vue && input.etamed == 'med'",  
          radioButtons("type_med_bpe", "Spécialités :",
                       choices = unique(
                         bpe23 %>%
                           filter(grd_ctg == "Médecin") %>%  
                           pull(libelle)  
                       ),
                       selected = first(unique(
                         bpe23 %>%
                           filter(grd_ctg == "Médecin") %>%
                           pull(libelle)
                       ))
          )
        ),
        
        # ✅ Sélection du type d'établissement - Affichée uniquement en mode "Carte" et "Établissements"
        conditionalPanel(
          condition = "!input.toggle_vue && input.etamed == 'eta'",  
          radioButtons("type_med_bpe", "Type d'établissement :",
                       choices = unique(
                         bpe23 %>%
                           filter(grd_ctg == "Etablissement") %>%  
                           pull(libelle)  
                       ),
                       selected = first(unique(
                         bpe23 %>%
                           filter(grd_ctg == "Etablissement") %>%
                           pull(libelle)
                       ))
          )
        ),
        
        # ✅ Curseur de distance (Toujours affiché)
        sliderInput("dist_arret", "Sélectionnez un rayon de recherche :", 
                    min = 100, max = 600, value = 300, step = 100),
        
        # ✅ Bouton réinitialiser - Affiché uniquement en mode "Carte"
        conditionalPanel(
          condition = "!input.toggle_vue",
          actionButton("reset_map", "Réinitialiser la sélection", class = "btn btn-warning")
        ),
        
        # ✅ **Ajout du tableau des arrêts affiché uniquement en mode "Carte"**
        conditionalPanel(
          condition = "!input.toggle_vue && output.table_lignes_non_vide",
          h4("Lignes desservies", style = "font-weight: bold; color: #286c7c;"),
          tableOutput("table_lignes")
        ),
        
        
        # ✅ Mini-carte affichée uniquement en mode "Table"
        conditionalPanel(
          condition = "input.toggle_vue",
          h4("Carte des arrêts pour les lignes sélectionnées", style = "font-size: 16px; color: #286c7c; font-weight: bold;"),
          leafletOutput("mini_map", width = "100%", height = "60vh")
        )
      )
      ,
      
      # ✅ Main panel avec carte + tableau + switch input
      mainPanel(
        width = 8,  # ✅ 70% de l'écran
        
        # ✅ Switch Input avec texte aligné
        div(
          style = "margin-top: 20px; display: flex; align-items: center; gap: 10px;",
          span(
            textOutput("toggle_text"), 
            style = "font-size: 14px; font-weight: bold; white-space: nowrap;"
          ),
          switchInput(
            inputId = "toggle_vue", 
            label = NULL,  
            onLabel = "Carte", offLabel = "Table",  
            size = "mini",  
            onStatus = "success", 
            offStatus = "info"
          )
        ),
        
        # ✅ Carte principale (Affichée seulement si toggle OFF)
        conditionalPanel(
          condition = "!input.toggle_vue",
          h4("Cliquez sur une ressource de santé pour afficher les arrêts à proximité, les lignes desservies et la distance à l'arrêt", 
             style = "font-weight: bold; color: #286c7c;"),
          leafletOutput("map", width = "100%", height = "70vh")
        ),
        
        # ✅ Tableau des médecins par ligne (Affiché si toggle ON, avec plus de hauteur)
        conditionalPanel(
          condition = "input.toggle_vue",
          h4(textOutput("titre_tableau_medecins"), style = "margin-top: 20px; font-weight: bold; color: #286c7c;"),
          h6("Cliquez sur les lignes du tableau pour afficher les lignes TCL concernées sur la carte de gauche"),
          selectInput(
            inputId = "filtre_ligne",
            label = "Type de ligne",
            choices = c("Toutes", "Bus Standard", "Tramway", "Bus Fréquent", "Lignes A", 
                        "Lignes Pleine Lune", "Navette Spéciale"),
            selected = "Toutes"
          ),
          div(
            style = "height: 70vh; display: flex; flex-direction: column;",  # ✅ Étend la hauteur
            DTOutput("table_medecins_lignes")
          )
        )
      )
    )
  ),
  
  # **2️⃣ Contenu de l'onglet "Médecins Disponibles"**
  conditionalPanel(
    condition = "input.tabs_ong3 == 'Disponibilité par jour et heure'",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        
        # ✅ Sélection du jour de la semaine
        selectInput("jour", "Jour de la semaine",
                    choices = list(
                      "Qu'importe le jour" = "all",
                      "Semaine (Lundi à Vendredi)" = "week",
                      "Week-end (Samedi & Dimanche)" = "weekend",
                      "Lundi" = 1, "Mardi" = 2, "Mercredi" = 3, 
                      "Jeudi" = 4, "Vendredi" = 5, "Samedi" = 6, "Dimanche" = 7
                    ),
                    selected = "all"),
        
        # ✅ Séparation visuelle
        HTML("<hr style='height:3px; border-width:0; background-color:#286c7c; margin-top: 20px; margin-bottom: 20px;'>"),
        
        # ✅ Graphique interactif
        h4("Horaires de disponibilité des médecins généralistes", style = "font-size: 16px; color: #286c7c; font-weight: bold;"),
        p("Cliquez sur une plage horaire pour afficher les médecins disponibles à celle-ci."),
        
        div(
          style = "margin-bottom: 20px;",
          plotOutput("graph_horaires", height = "480px", width = "100%", click = "graph_horaires_click")
        ),
        tags$div(
          style = "font-style: italic; margin-top: 10px;margin-bottom: 10px",  # Style en italique avec marge
          "Source : Annuaire Santé 2024, professionnels ayant renseigné leurs jours et horaires de disponibilité"
        ),
        
        actionButton("reset_hour", "Réinitialiser l'heure sélectionnée", class = "btn btn-warning")
      ),
      
      # ✅ Main panel avec carte
      mainPanel(
        width = 8,  # ✅ 70% de l'écran
        leafletOutput("map_horaires", width = "100%", height = "80vh"),
        textOutput("message_horaires")
      )
    )
  )
)
