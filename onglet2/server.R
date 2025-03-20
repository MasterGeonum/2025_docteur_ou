


# Calcul du centroïde des carreaux
carreauxinsee <- carreauxinsee %>% mutate(centroid = st_centroid(geometry))



onglet2_server <- function(input, output, session) {
  
  # Vérification et filtrage des médecins selon le type sélectionné
  select_type_med <- reactive({
    req(input$choix_med)  # Vérifier qu'un type est sélectionné
    bpe23 %>%
      filter(libelle == input$choix_med)  
  })
  
 
  # Fonction pour définir les bornes spécifiques selon le type de médecin
  get_bornes_medecin <- reactive({
    req(input$choix_med) 
    
    if (input$choix_med == "Généraliste") {
      return(c(0, 5, 10, 20, 30, 50, 115))  
    } else if (input$choix_med == "Chirurgien-dentiste") {
      return(c(0, 5, 10, 20, 30, 50, 115))  
    } else {
      return(c(0, 2, 7, 15, 25, 40, 75))  
    }
  })
  
  # Mise à jour de LyonCar2 selon la sélection
  LyonCar2_react <- reactive({
    
    TypeMED <- select_type_med()
    req(nrow(TypeMED) > 0)  # Vérifier qu'on a des données
    

    #Filtrer par type de libelle 
    filteredTYPE <- distLong %>%
      filter(libelle == input$choix_med)
    
    # Filtrer par distance
    filteredDist <- filteredTYPE %>%
      filter(dist <= input$distance) %>%
      group_by(noCell) %>%
      summarise(nbequ = n(), .groups = "drop")
    
    # Jointure avec carreauxinsee
    LyonCar2 <- carreauxinsee %>%
      mutate(noCell = row_number()) %>%
      left_join(filteredDist, by = "noCell") %>%
      mutate(nbequ = coalesce(nbequ, 0))  # Remplacer NA par 0
    
    # Sélectionner les carreaux qui sont partiellement ou totalement dans Lyon
    intersect_lyon <- st_intersects(LyonCar2, lyon, sparse = FALSE)
    
    # Garder uniquement les carreaux qui ont une intersection avec Lyon
    LyonCar2 <- LyonCar2[rowSums(intersect_lyon) > 0, ]
    
    return(LyonCar2)
  })
  
  
  # Carte Leaflet
  output$map_annuaire <- renderLeaflet({
    LyonCar2 <- LyonCar2_react()
    MesBornes <- get_bornes_medecin()
    
    # Palette de couleurs
    MesCouleurs <- brewer.pal(n = 7, name = "YlOrBr")
    pal <- colorBin(MesCouleurs, domain = LyonCar2$nbequ, bins = MesBornes)

    
    # Création du titre de la légende
    title_legende <- if (input$choix_med == "Oto-Rhino-Laryngologue (ORL)") {
      paste("Nombre d'", "Oto-Rhino-Laryngologues (ORL)", sep = "")
    } else if (input$choix_med == "Chirurgien-dentiste") {
      paste("Nombre de Chirurgiens-dentistes")
    } else if (input$choix_med == "Ophtalmologiste") {
      paste("Nombre d'Ophtalmologistes")
    } else {
      paste("Nombre de", paste0(input$choix_med, "s"))
    }
    
    # Création de la carte
    leaflet(LyonCar2) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(minZoom = 12, maxZoom = 20)) %>%
      
      
      # Ajout des carreaux avec la densité médicale
      addPolygons(
        fillColor = ~pal(nbequ),
        color = "white",
        weight = 0.3,
        opacity = 1,
        fillOpacity = 0.8,
        # Ajout des popups avec les conditions spécifiées
        popup = ~if (input$choix_med == "Oto-Rhino-Laryngologue (ORL)") {
          paste("Nombre d'", "Oto-Rhino-Laryngologues (ORL)", ": ", nbequ)
        } else if (input$choix_med == "Chirurgien-dentiste") {
          paste("Nombre de Chirurgiens-dentistes : ", nbequ)
        } else if (input$choix_med == "Ophtalmologiste") {
          paste("Nombre d'Ophtalmologistes : ", nbequ)
        } else {
          paste("Nombre de", paste0(input$choix_med, "s"), ": ", nbequ)
        },
        group = "Acessibilité locale à l'offre de soins"  # Groupe pour la gestion des calques
      ) %>%
      
      # Ajout des arrondissements (calque optionnel)
      addPolylines(
        data = arrondissement,
        color = "blue",
        weight = 1.5,
        fillOpacity = 0,
        group = "Arrondissements de Lyon"
      ) %>%
      
      # Ajout de la légende
      addLegend(
        pal = pal, values = LyonCar2$nbequ, 
        title = title_legende,  
        position = "bottomright"
      ) %>%
      
      # Ajout du contrôle des calques en haut à droite
      addLayersControl(
        overlayGroups = c("Acessibilité locale à l'offre de soins", "Arrondissements de Lyon"),
        options = layersControlOptions(collapsed = FALSE) # Affiché par défaut
      ) 
  })
  
  
  ##graphique 
  output$graph_pauvrete <- renderPlot({
    
    # Récupération des données
    LyonCar2 <- LyonCar2_react()
    MesBornes <- get_bornes_medecin()
    
    # Ajout d'une colonne de classe discrétisée
    LyonCar2 <- LyonCar2 %>%
      mutate(classe_nbequ = cut(nbequ, breaks = MesBornes, include.lowest = TRUE, right = FALSE))
    
    # Calcul du pourcentage de pauvreté par classe de médecins
    data_plot <- LyonCar2 %>%
      mutate(pourcentage_pauvrete = (Men_pauv / Men) * 100) %>%  
      group_by(classe_nbequ) %>%
      summarise(pourcentage_pauvrete = mean(pourcentage_pauvrete, na.rm = TRUE), .groups = "drop")
    
    # Création du graphique ggplot
    ggplot(data_plot, aes(x = classe_nbequ, y = pourcentage_pauvrete, fill = classe_nbequ)) +
      geom_col(show.legend = FALSE) +  # Suppression de la légende
      geom_text(aes(label = sprintf("%.1f%%", pourcentage_pauvrete)), 
                vjust = 13, size = 5, fontface = "bold") +  # Affichage du pourcentage sur les barres
      scale_fill_brewer(palette = "YlOrBr") +  # Utilisation de la palette YlOrBr
      labs(title = "Pourcentage de ménages sous le seuil de pauvreté \n en fonction du nombre de médecins",
           x = "Nombre de médecins",
           y = "Pourcentage de ménages sous le seuil de pauvreté (%)") +
      
      # Regroupement des modifications des labels dans une seule transformation
      scale_x_discrete(labels = function(x) {
        x <- gsub("\\[|\\]", "", x)  # Retirer les crochets
        x <- gsub(",", " -", x)  # Remplacer la virgule par un tiret
        x <- gsub("\\)", "", x)  
        return(x)
      }) +
      
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Texte plus grand pour l'axe X
            axis.text.y = element_text(size = 12),  # Texte plus grand pour l'axe Y
            plot.title = element_text(size = 14, face = "bold"))  # Agrandir le titre
    
  })
  

  ## Sélection du type de médecin dans l'onglet 2
  select_med_ind <- reactive({
    req(input$libelle_indice)  # Vérifier qu'un médecin est sélectionné
    return(input$libelle_indice)  # Retourner la valeur
  })
  
  # Mise à jour des données selon la sélection
  IND_react <- reactive({
    req(input$scale_selection)
    
    TypeMED_IND <- select_med_ind()
    
    # Filtrer les données par type de médecin sélectionné
    filtered_IND <- pression_med %>%
      filter(libelle == TypeMED_IND)
    
    req(nrow(filtered_IND) > 0)  # Vérifier qu'il y a des données à afficher
    
    # Sélectionner uniquement les polygones qui INTERSECTENT Lyon
    intersect_lyon <- st_intersects(filtered_IND, lyon, sparse = FALSE)
    filtered_IND <- filtered_IND[rowSums(intersect_lyon) > 0, ]
    
    
    return(filtered_IND)
  })
  
  output$map_indice <- renderLeaflet({
    pression_med2 <- IND_react()
    
    req(nrow(pression_med2) > 0)  # Vérifier qu'il y a des données avant d'afficher
    
    # Déterminer la colonne à afficher en fonction du slider
    colonne_selectionnee <- paste0("sum_poids_med_", input$scale_selection) 
    
    # Vérifier si la colonne sélectionnée existe dans les données
    if (!colonne_selectionnee %in% colnames(pression_med2)) {
      stop("La colonne sélectionnée n'existe pas dans les données.")
    }
    
    # Remplacer les NA par 99 dans la colonne sélectionnée
    pression_med2[[colonne_selectionnee]] <- replace_na(pression_med2[[colonne_selectionnee]], 99)
    
    # Filtrer les valeurs à 99 pour le calcul des quantiles (elles seront affichées séparément)
    valeurs_valides <- pression_med2[[colonne_selectionnee]][pression_med2[[colonne_selectionnee]] != 99]
    
    # Définir les plages de discrétisation en 3 catégories : "Faible", "Moyen", "Fort"
    breaks <- quantile(valeurs_valides, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE, names = FALSE)
  
    # Palette de couleurs : Faible (Vert) -> Moyen (Jaune) -> Fort (Orange)
    color_palette <- colorBin(palette = c("#ef937f", "#efd67f", "#81d3b7"), 
                              domain = valeurs_valides, 
                              bins = breaks, 
                              na.color = "grey")  # Couleur des NA et valeurs 99
    
    # Fonction pour gérer les valeurs 99
    get_color <- function(value) {
      if (value == 99) {
        return("grey")
      } else {
        return(color_palette(value))
      }
    }
    # Création de la carte Leaflet
    leaflet(data = pression_med2) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(minZoom = 12, maxZoom = 20)) %>%  # Fond de carte
      addPolygons(
        fillColor = ~sapply(get(colonne_selectionnee), get_color),  # Appliquer la couleur dynamique
        color = "white",  # Couleur des bords
        weight = 0.5,  # Épaisseur des bords
        opacity = 0.7,  # Opacité des bords
        fillOpacity = 0.8,  # Opacité du remplissage
        group = "APL"  # Groupe pour la gestion des calques

      ) %>%
      
      # Ajouter les arrondissements (calque optionnel)
      addPolylines(
        data = arrondissement,
        color = "blue",
        weight = 1.5,
        fillOpacity = 0,  # Opacité 0 pour ne pas remplir les arrondissements
        group = "Arrondissements de Lyon"
      ) %>%
      
  
      # Ajouter la légende
      addLegend(
        position = "bottomright", 
        colors = c("#ef937f", "#efd67f", "#81d3b7", "grey"),
        labels = c("Mauvaise", "Moyenne", "Bonne", "Pas de données"),
        title = paste("Accessibilité Potentielle Localisée (", input$scale_selection, "m)", sep = "")
      ) %>%
      
      # Ajouter un contrôle pour activer/désactiver les calques
      addLayersControl(
        overlayGroups = c("APL", "Arrondissements de Lyon"),  # Groupes pour les calques
        options = layersControlOptions(collapsed = FALSE)  # Panneau non réduit par défaut
      ) 
      
  })
    
    
}
