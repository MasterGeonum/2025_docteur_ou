onglet1_bis_server <- function(input, output, session) {
  
  #############################################
  ## Réactives pour les données affichées  ####
  #############################################
  
  
  # Historique : Sélection des données pour les polygones IRIS
  selected_data_iris <- reactive({
    med <- input$type_med_iris
    annee <- input$year
    annee_densite_colonne <- paste0("densite_", med, annee)
    iris_data <- bpe_iris %>%
      select(iris_code, NOM_IRI.x, geometry, annee_densite_colonne)
    iris_data <- iris_data[!is.na(iris_data[[annee_densite_colonne]]), ]  # Supprimer les NAs
    return(iris_data)
  })
  
  # Evolution : Sélection des données pour les polygones IRIS
  evo_data_iris <- reactive({
    req(input$evo_med) # Assurez-vous qu'un choix de médecin est sélectionné
    
    med <- input$evo_med
    col_name_16 <- paste0("nb_", med, "2016")
    col_name_23 <- paste0("nb_", med, "2023")
    
    # Calcul de l'évolution (soustraction simple) et ajout de la colonne `reason_zero`
    iris_data <- bpe_iris %>%
      select(iris_code, geometry, NOM_IRI.x, col_name_16, col_name_23) %>% 
      mutate(
        evo_nb = .[[col_name_23]] - .[[col_name_16]], 
        reason_zero = case_when(
          .[[col_name_16]] == 0 & .[[col_name_23]] == 0 ~ "no_medecin", # Aucun médecin
          .[[col_name_16]] == .[[col_name_23]] & .[[col_name_16]] != 0 ~ "stable", # Nombre stable
          evo_nb == 0 ~ "other", # Autres cas où evo_nb = 0
        )) %>% 
        mutate(evo_rel = ((.[[col_name_23]] - .[[col_name_16]])/.[[col_name_23]])*100)
    
    return(iris_data)
  })
  
  #############################################
  ## Réactives pour les histogrammes       ####
  #############################################
  
  #HISTORIQUE 
  
  # Filtrage des données en fonction de l'année et de la spécialité 
  filtered_densite <- reactive({
    req(input$year)  # année
    req(input$type_med_iris)  # spécialité
    
    # Sélectionner les bonnes colonnes
    med <- input$type_med_iris
    annee <- input$year
    annee_nb_colonne <- paste0("nb_", med, annee)
    
    # Calculer la valeur pour Lyon
    densite_lyon = bpe_iris %>%
      st_drop_geometry() %>% 
      select(NOM_COM.x, !!annee_nb_colonne, P20_POP) %>% 
      filter(startsWith(NOM_COM.x, "Lyon")) %>% 
      summarise(densite = (sum(get(annee_nb_colonne), na.rm = TRUE) / sum(P20_POP, na.rm = TRUE)) * 100000)    
    
    # Calculer la valeur par arrondissement
    DF_filtered_densite <- bpe_iris %>%
      st_drop_geometry() %>% 
      select(NOM_COM.x, !!annee_nb_colonne, P20_POP) %>% 
      filter(startsWith(NOM_COM.x, "Lyon")) %>% 
      group_by(NOM_COM.x) %>%
      summarise(densite = (sum(get(annee_nb_colonne))/sum(P20_POP))*100000) 
    
    #Fusionner Lyon et arrondissement
    DF_filtered_densite <- DF_filtered_densite %>%
      add_row(NOM_COM.x = "Ville de Lyon", densite = densite_lyon$densite, P20_POP = densite_lyon$P20_POP )
    
    # Retourner les données filtrées
    DF_filtered_densite
  })
  
  #------------------------------------------------------------------------------------------------------------
  
  #EVOLUTION 
  
  # Filtrage des données en fonction la spécialité 
  filtered_densite_evo <- reactive({

    # Sélectionner les bonnes colonnes
    med <- input$evo_med
    col_name_16 <- paste0("nb_", med, "2016")
    col_name_23 <- paste0("nb_", med, "2023")
    
    # Calculer la valeur pour Lyon
    evo_lyon = bpe_iris %>%
      st_drop_geometry() %>% 
      select(NOM_COM.x, col_name_16, col_name_23) %>% 
      filter(startsWith(NOM_COM.x, "Lyon")) %>% 
      summarise(nb_med_choi2016 = sum(get(col_name_16)),
                nb_med_choi2023 = sum(get(col_name_23))) %>% 
      mutate(evo_nb = nb_med_choi2023 - nb_med_choi2016) %>% 
      mutate(evo_rel = ((nb_med_choi2023 - nb_med_choi2016)/nb_med_choi2023)*100)  
    
    # Calculer la valeur par arrondissement
    evo_arr <- bpe_iris %>%
      st_drop_geometry() %>% 
      select(NOM_COM.x, col_name_16, col_name_23) %>% 
      filter(startsWith(NOM_COM.x, "Lyon")) %>% 
      group_by(NOM_COM.x) %>%
      summarise(nb_med_choi2016 = sum(get(col_name_16)),
                nb_med_choi2023 = sum(get(col_name_23))) %>% 
      mutate(evo_nb = nb_med_choi2023 - nb_med_choi2016) %>% 
      mutate(evo_rel = ((nb_med_choi2023 - nb_med_choi2016)/nb_med_choi2023)*100)
      
      
    #Fusionner Lyon et arrondissement
    evo_arr <- evo_arr %>%
      add_row(NOM_COM.x = "Ville de Lyon", nb_med_choi2016 = evo_lyon$nb_med_choi2016, nb_med_choi2023 = evo_lyon$nb_med_choi2023,
              evo_nb = evo_lyon$evo_nb, evo_rel = evo_lyon$evo_rel) 
    
    # Retourner les données filtrées
    evo_arr
  })
  
  
  #############################################
  ## Code couleurs et fonctions reactives  ####
  #############################################
 
  # Palette de couleurs iris
  color_palette_iris <- reactive({
    med <- input$type_med_iris
    annee <- input$year
    annee_densite_colonne <- paste0("densite_", med, annee)
    
    
    # Extraction des quantiles personnalisés
    dis_filtered <- discretisation %>% 
      filter(libelle == med)
    
    l1 = dis_filtered[[3]]
    l2 = dis_filtered[[4]]
    l3 = dis_filtered[[5]]
    l4 = dis_filtered[[6]]
    l5 = dis_filtered[[7]]
    l6 = dis_filtered[[8]]
    l7 = dis_filtered[[9]]
    
    bins = c(l1, l2, l3, l4, l5, l6, l7)
    
    # Définir les couleurs personnalisées
    custom_colors <- c(
      "#dddddd",  # Gris pour l'intervalle [l1, l2]
      RColorBrewer::brewer.pal(5, "YlGnBu")  # Palette YlGnBu pour les autres intervalles
    )
    
    # Créer une palette de couleurs personnalisée
    colorBin(palette = custom_colors, domain = selected_data_iris()[[annee_densite_colonne]], bins = bins)
  })
  
  #############################################
  ## Initialisation et nettoyage           ####
  #############################################
  
  # Initialisation de la carte vide
  output$map_ong1bis <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(minZoom = 12, maxZoom = 20)) %>%
      setView(lng = 4.85, lat = 45.75, zoom = 12) %>%  # Vue par défaut sans couches ajoutées
      addPolylines(
        data = arrondissement,
        color = "blue",      # Couleur des contours
        weight = 2,          # Épaisseur des contours
        opacity = 1,         # Opacité des contours (1 = opaque)
        group = "Arrondissements de Lyon"
      ) %>%
      addPolylines(
        data = qpv,
        color = "orange",    # Couleur des contours
        weight = 2,          # Épaisseur des contours
        opacity = 1,         # Opacité des contours (1 = opaque)
        group = "Quartiers prioritaires de la politique de la ville"
      ) %>%
      addLayersControl(
        overlayGroups = c("Arrondissements de Lyon", "Quartiers prioritaires de la politique de la ville"),
        options = layersControlOptions(collapsed = FALSE))
  })
    
  # Vider la carte au changement d'onglet
  observeEvent(input$tabs_ong1bis, {
    leafletProxy("map_ong1bis") %>%
      clearControls() %>%  # Supprimer toutes les légende
      clearGroup("IRIS")  %>% 
      clearGroup("Comparaison") 
  })
  

  
  
  #############################################
  ## Observateurs                          ####
  #############################################
  
  
    # Observateur pour les polygones IRIS : historique
    
    # Observateur pour mettre à jour les choix de spécialité en fonction de l'année
    observeEvent(input$year, {
      if (input$year %in% c("2016", "2021")) {
        # Exclure "Gynécologue" pour 2016 et 2021
        updateSelectInput(
          session, 
          "type_med_iris", 
          choices = choices_medecins[choices_medecins != "Gynécologue"]
        )
        # Afficher un message explicatif
        output$message_gynécologue <- renderText({
          "Pas de données pour la spécialité 'Gynécologue' en 2016 et 2021."
        })
      } else {
        # Réinitialiser les choix pour les autres années
        updateSelectInput(
          session, 
          "type_med_iris", 
          choices = choices_medecins
        )
        # Effacer le message explicatif
        output$message_gynécologue <- renderText({
          ""
        })
      }
    })
  
    
    # Observateur pour les polygones IRIS : historique

  observe({
    if (input$tabs_ong1bis == "tab3" && input$year != "Aucune") {  # Vérifier si une année est sélectionnée
      iris_data <- selected_data_iris()
      med <- input$type_med_iris
      annee <- input$year
      col_name <- paste0("densite_", med, annee)
      
      # Vérifier si la colonne existe
      if (col_name %in% colnames(iris_data)) {
        palette <- color_palette_iris()  # Utilisation de la palette dynamique
        
        leafletProxy("map_ong1bis") %>%
          clearGroup("IRIS") %>%
          clearGroup("Arrondissements de Lyon") %>%
          clearGroup("Quartiers prioritaires de la politique de la ville") %>%
          addPolygons(
            data = iris_data,
            fillColor = ~palette(iris_data[[col_name]]),  # Appliquer la palette dynamique
            fillOpacity = 0.7,
            color = "#000000",
            weight = 1,
            popup = ~paste("Nombre de médecins pour 100 000 habitants : ", round(iris_data[[col_name]], 2), "<br>Nom de l'iris:", iris_data$NOM_IRI.x),
            group = "IRIS"
          ) %>%
          addPolylines(
            data = arrondissement,
            color = "blue",      # Couleur des contours
            weight = 2,          # Épaisseur des contours
            opacity = 1,         # Opacité des contours 
            group = "Arrondissements de Lyon"
          ) %>%
          addPolylines(
            data = qpv,
            color = "orange",    # Couleur des contours
            weight = 2,          # Épaisseur des contours
            opacity = 1,         # Opacité des contours 
            group = "Quartiers prioritaires de la politique de la ville"
          ) %>%
          addLayersControl(
            overlayGroups = c("Arrondissements de Lyon", "Quartiers prioritaires de la politique de la ville"),
            options = layersControlOptions(collapsed = FALSE)
          ) %>% 
          addLegend(
            position = "bottomright",
            pal = palette,
            values = iris_data[[col_name]],
            title = HTML("Nombre de médecins pour <br> 100 000 habitants"),
            opacity = 1,
            layerId = "legend_iris"
          )
        
      
          output$dens_histo <- renderPlot({
            req(filtered_densite())  # Assure que les données sont disponibles
            
            # Ajouter une colonne pour le highlight et retirer "Lyon " du début de NOM_COM.x
            filtered_data <- filtered_densite() %>%
              mutate(
                highlight = ifelse(NOM_COM.x == input$arrondissement_historique, "Oui", "Non"),
                fill_color = color_palette_iris()(densite),  # Appliquer la palette de couleurs à `densite`
                NOM_COM.x = str_remove(NOM_COM.x, "^Lyon ")  # Retirer "Lyon " du début de NOM_COM.x
              )
            
            # Créer l'histogramme
            ggplot(filtered_data, aes(x = NOM_COM.x, y = densite)) +
              geom_bar(
                stat = "identity",
                aes(color = highlight, size = highlight, fill = fill_color),  # Contour dynamique et remplissage personnalisé
              ) +
              scale_color_manual(values = c("Oui" = "orange")) +  # Contour orange pour la barre sélectionnée, noir pour les autres
              scale_size_manual(values = c("Oui" = 1.5, "Non" = 0.5)) +  # Contour plus épais pour la barre sélectionnée
              scale_fill_identity() +  # Utiliser les couleurs de `fill_color` directement
              geom_text(aes(label = round(densite)), position = position_stack(vjust = 0.5), colour = "black", size = 5) +  # Ajouter les valeurs de `densite` au-dessus des barres
              labs(
                title = paste("Nombre de médecins de la spécialité\nchoisie pour 100 000 habitants"),
                x = "Arrondissement",
                y = "Nombre"
              ) +
              theme_minimal() +
              theme(
                axis.text.x = element_text(
                  angle = 65, 
                  hjust = 1,
                  size = 12   # Taille du texte
                  )) +
              guides(color = FALSE, size = FALSE)  # Masquer les légendes pour `color` et `size`
          })
        }
    }
    else { 
      leafletProxy("map_ong1bis") %>%
        clearGroup("IRIS")
      }
    })
    

    # Observer le zoom
    observeEvent(input$arrondissement_historique, {
      req(input$arrondissement_historique)  # Assurez-vous qu'il y a un input
      
      if (input$arrondissement_historique == "Ville de Lyon"){
        #Zoom sur la ville de Lyon : zoom initial
        leafletProxy("map_ong1bis") %>%
          setView(lng = 4.85, lat = 45.75, zoom = 12)
      }else{
        
        # Récupérer les coordonnées de l'arrondissement sélectionné
        arrondissement_selected <- arrondissement %>% filter(NOM_COM.x == input$arrondissement_historique)
        
        
        # Calculer la boîte englobante (bbox) de l'arrondissement
        bbox <- st_bbox(arrondissement_selected)
        y <- c(bbox$ymin, bbox$ymax) 
        x <- c(bbox$xmin, bbox$xmax) 
        
        # Zoom sur l'arrondissement
        leafletProxy("map_ong1bis") %>%
          setView(lng = mean(x), lat = mean(y), zoom = 14)
      } 
    })
    
    

  #-------------------------------------------------------------------------------
    # Observateur pour les polygones IRIS : evolution
    observe({
      req(input$tabs_ong1bis == "tab4", input$evo_med != "") 
      
      iris_data <- evo_data_iris() # Table avec une colonne `evo_nb` et `reason_zero`
      med <- input$evo_med
      col_name_16 <- paste0("nb_", med, "2016")
      col_name_23 <- paste0("nb_", med, "2023")
      
      
      # Calcul des centroïdes des polygones IRIS
      centroids <- st_point_on_surface(iris_data$geometry) # Calcul des centroïdes avec `sf`
      
      # Associer les centroïdes aux données `iris_data`
      iris_data_centroids <- st_set_geometry(iris_data, centroids) # Remplace la géométrie par les centroïdes
      
      # Filtrer les centroïdes à afficher (exclure "no_medecin")
      centroids_to_display <- iris_data_centroids %>% 
        filter(reason_zero != "no_medecin")
      
      # Palette de couleurs personnalisée
      get_color <- function(evo_nb, reason_zero) {
        case_when(
          reason_zero == "no_medecin" ~ "#ffffff00", #transparent
          reason_zero == "stable" ~ "#106c72", 
          evo_nb < 0 ~ "#ff772a", # Rouge pour les valeurs négatives
          evo_nb > 0 ~ "#0fbb43", # Vert pour les valeurs positives
          TRUE ~ "black" # Par défaut (ne devrait pas arriver)
        )
      }
      
      # Appliquer la palette de couleurs
      colors <- get_color(iris_data_centroids$evo_nb, iris_data_centroids$reason_zero)
      
      # Définir un rayon minimal pour les cercles où `evo_nb = 0`
      min_radius <- 3 # Rayon minimal pour les cercles où `evo_nb = 0`
      
      # Calcul du rayon des cercles
      radius <- ifelse(
        iris_data_centroids$evo_nb == 0, # Si `evo_nb = 0`
        min_radius, # Utiliser le rayon minimal
        sqrt(abs(iris_data_centroids$evo_nb)) * 4 # Sinon, utiliser le rayon proportionnel
      )
      
      # Mise à jour de la carte avec `leafletProxy`
      leafletProxy("map_ong1bis") %>%
        clearGroup("IRIS") %>% # Nettoyer les cercles précédents
        clearControls() %>% # Nettoyer la légende précédente
        clearGroup("Arrondissements de Lyon") %>%
        clearGroup("Quartiers prioritaires de la politique de la ville") %>%
        
        # Ajouter les contours des polygones IRIS sans remplissage
        addPolygons(
          data = iris_data,
          fillColor = ~case_when(
            reason_zero == "no_medecin" ~ "#dddddd", # Gris pour "no_medecin"
            TRUE ~ NA_character_ # Pas de remplissage pour les autres
          ),
          fillOpacity = ~case_when(
            reason_zero == "no_medecin" ~ 0.7, # Opacité pour les polygones gris
            TRUE ~ 0 # Pas de remplissage pour les autres
          ),
          color = "#acb0b1", # Contour gris
          weight = 1, # Épaisseur du contour
          group = "IRIS"
        ) %>%
        # Ajouter les cercles proportionnels à `evo_nb`
        addCircleMarkers(
          data = iris_data_centroids, 
          radius = radius, # Utiliser le rayon calculé
          color = colors, # Couleur personnalisée
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 1,
          popup = ~paste(
            "Nombre de médecins en 2016:", get(col_name_16), "<br>", 
            "Nombre de médecins en 2023:", get(col_name_23), "<br>", 
            "Variation absolue entre 2016 et 2023:", evo_nb, "<br>", 
            "Variation relative entre 2016 et 2023:", round(evo_rel,2), "%", "<br>",
            "Nom de l'iris:", NOM_IRI.x
          ),
          group = "IRIS"
        ) %>%
        addPolylines(
          data = arrondissement,
          color = "blue",      # Couleur des contours
          weight = 2,          # Épaisseur des contours
          opacity = 1,         # Opacité des contours (1 = opaque)
          group = "Arrondissements de Lyon"
        ) %>%
        addPolylines(
          data = qpv,
          color = "orange",    # Couleur des contours
          weight = 2,          # Épaisseur des contours
          opacity = 1,         # Opacité des contours (1 = opaque)
          group = "Quartiers prioritaires de la politique de la ville"
        ) %>%
        addLayersControl(
          overlayGroups = c("Arrondissements de Lyon", "Quartiers prioritaires de la politique de la ville"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        
        # Ajouter une légende personnalisée
        addLegend(
          position = "bottomright",
          colors = c("#ff772a", "#0fbb43", "#106c72", "#dddddd"), # Couleurs de la légende
          labels = c("Variation négative", "Variation positive", "Stable", "Pas de médecin"), # Étiquettes
          title = "Evolution entre 2016 et 2023",
          opacity = 1,
          layerId = "legend_iris_evo"
        )
    })
    
    output$evo_histo <- renderPlot({
      req(filtered_densite_evo())  # Assure que les données sont disponibles
      
      # Ajouter une colonne pour le highlight et retirer "Lyon " du début de NOM_COM.x
      filtered_data <- filtered_densite_evo() %>%
        mutate(
          highlight = ifelse(NOM_COM.x == input$arrondissement_evo, "Oui", "Non"),
          NOM_COM.x = str_remove(NOM_COM.x, "^Lyon "),  # Retirer "Lyon " du début de NOM_COM.x
          fill_color = case_when(
            evo_rel == Inf ~ "Pas de médecin en 2023",
            evo_rel > 0 ~ "Positif",
            evo_rel < 0 ~ "Négatif",
            TRUE ~ "Autre"
          ),
          label_text = ifelse(is.infinite(evo_rel), "", paste0(round(evo_rel), "%"))        )
      
      # Créer l'histogramme
      ggplot(filtered_data, aes(x = NOM_COM.x, y = evo_rel)) +
        geom_bar(
          stat = "identity",
          aes(fill = fill_color, color = highlight, size = highlight),  # Remplissage et contour dynamiques
        ) +
        scale_fill_manual(values = c(
          "Positif" = "#aad893",  # Vert pour les valeurs positives
          "Négatif" = "#fabb4b"  # Orange pour les valeurs négatives
        )) +
        scale_color_manual(values = c(
          "Oui" = "blue",  # Contour bleu si highlight = "Oui"
          "Non" = "transparent"  # Pas de contour si highlight = "Non"
        )) +
        scale_size_manual(values = c(
          "Oui" = 1.5,  # Épaisseur du contour si highlight = "Oui"
          "Non" = 0  # Pas de contour si highlight = "Non"
        )) +
        geom_text(
          aes(label = label_text),  # Texte dynamique
          position = position_stack(vjust = 0.5), 
          colour = "black", 
          size = 5
        ) +
        labs(
          title = paste("Evolution relative du nombre de médecins\nde la spécialité choisie entre 2016 et 2023"),
          x = "Arrondissement",
          y = "Pourcentage"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(
          angle = 65, 
          hjust = 1,
          size = 12   # Taille du texte
          )) +
        guides(fill = FALSE, color = FALSE, size = FALSE)  # Masquer les légendes
    })
    
    
    
    # Observer le zoom
    observeEvent(input$arrondissement_evo, {
      req(input$arrondissement_evo)  # Assure qu'il y ait  un input
      
      if (input$arrondissement_evo == "Ville de Lyon"){
        #Zoom sur la ville de Lyon : zoom initial
        leafletProxy("map_ong1bis") %>%
          setView(lng = 4.85, lat = 45.75, zoom = 12)
      }else{
        
        # Récupérer les coordonnées de l'arrondissement sélectionné
        arrondissement_selected <- arrondissement %>% filter(NOM_COM.x == input$arrondissement_evo)
        
        
        # Calculer la boîte englobante (bbox) de l'arrondissement
        bbox <- st_bbox(arrondissement_selected)
        y <- c(bbox$ymin, bbox$ymax) 
        x <- c(bbox$xmin, bbox$xmax) 
        
        # Zoom sur l'arrondissement
        leafletProxy("map_ong1bis") %>%
          setView(lng = mean(x), lat = mean(y), zoom = 14)
      } 
    })
}