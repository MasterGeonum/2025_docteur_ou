onglet1_server <- function(input, output, session) {
  
  #############################################
  ## Réactives pour les données affichées  ####
  #############################################
  
  #BPE Medecin
  selected_data_bpe <- reactive({
    req(input$type_med_bpe)  # Assurez-vous qu'au moins un type de médecin est sélectionné
    data <- bpe23_agreg %>% filter(libelle %in% input$type_med_bpe)  # Filtrer par type de médecin
    return(data)
  })
  
  #BPE Annuaire
  selected_data_annuaire <- reactive({
    req(input$type_med_annuaire)
    data <- annuaire24_agr %>% filter(libelle %in% input$type_med_annuaire)  # Utiliser %in% pour plusieurs sélections
    return(data)
  })
  
  #############################################
  ## Réactives pour les histogrammes       ####
  #############################################
  
  # Filtrage des données en fonction de l'arrondissement sélectionné et du type d'équipement
  filtered_eta_data <- reactive({
    req(input$arrondissement_eta)  # Assurez-vous qu'un arrondissement est sélectionné
    req(input$type_eta)  # Assurez-vous qu'au moins un type d'établissement est sélectionné
    
    # Filtrer les données pour l'arrondissement sélectionné ou pour toute la ville
    if (input$arrondissement_eta == "Ville de Lyon") {
      # Cas 1 : "Ville de Lyon"
      filtered_data <- bpe23 %>%
        st_drop_geometry() %>% 
        filter(libelle %in% input$type_eta & LIBCOM %in% unique(str_to_upper(arrondissement$NOM_COM.x))) %>% 
        select(libelle, LIBCOM) %>% 
        group_by(libelle) %>%
        summarise(count = n(), .groups = "drop") %>% 
        mutate(LIBCOM = NA)
      
    } else if (startsWith(input$arrondissement_eta, "Lyon")) {
      # Cas 2 : Commence par "Lyon" (exemple : "Lyon 1er", "Lyon 2e", etc.)
      arr <- str_to_upper(input$arrondissement_eta)
      filtered_data <- bpe23 %>%
        st_drop_geometry() %>% 
        filter(libelle %in% input$type_eta & LIBCOM == arr) %>% 
        select(libelle, LIBCOM) %>% 
        group_by(libelle) %>%
        summarise(count = n(), .groups = "drop")
      
    } else if (startsWith(input$arrondissement_eta, "QPV :")) {
      # Cas 3 : Commence par "QPV :"
      qpv_name <- gsub("^QPV : ", "", input$arrondissement_eta) # Supprimer le préfixe "QPV :"
      filtered_data <- bpe23 %>%
        st_drop_geometry() %>% 
        filter(libelle %in% input$type_eta & lib_qp == qpv_name) %>% 
        select(libelle, lib_qp) %>% 
        group_by(libelle) %>%
        summarise(count = n(), .groups = "drop")
    }
    
    # Ajouter les types d'établissements manquants avec un compte de 0
    all_types <- unique(input$type_eta)
    missing_types <- setdiff(all_types, filtered_data$libelle)
    
    if (length(missing_types) > 0) {
      missing_data <- tibble(
        libelle = missing_types,  # Colonne libelle
        count = 0                 # Colonne count
      )
      filtered_data <- bind_rows(filtered_data, missing_data)
    }
    
    # Retourner les données filtrées
    filtered_data
  })
  
  #### ----------------------------------------------------------------------------------------
  
  # Filtrage des données en fonction de l'arrondissement sélectionné et du type de medecin BPE
  filtered_bpe_data <- reactive({
    req(input$arrondissement_bpe)  # Assurez-vous qu'un arrondissement est sélectionné
    req(input$type_med_bpe)  # Assurez-vous qu'au moins un type de medecin
    
    # Filtrer les données pour l'arrondissement sélectionné ou pour toute la ville
    if (input$arrondissement_bpe == "Ville de Lyon") {
      filtered_data <- bpe23_agreg %>%
        st_drop_geometry() %>% 
        filter(libelle %in% input$type_med_bpe & LIBCOM %in% unique(str_to_upper(arrondissement$NOM_COM.x))) %>% 
        select(libelle, LIBCOM, nb_med) %>% 
        group_by(libelle) %>%
        summarise(count_histo = sum(nb_med, na.rm = TRUE), .groups = "drop") %>% 
        mutate(LIBCOM=NA)
    } 
    else if (startsWith(input$arrondissement_bpe, "Lyon")) {
      arr <- str_to_upper(input$arrondissement_bpe)
      filtered_data <- bpe23_agreg %>%
        st_drop_geometry() %>% 
        filter(libelle %in% input$type_med_bpe & LIBCOM == arr) %>% 
        select(libelle, LIBCOM, nb_med) %>% 
        group_by(libelle) %>%
        summarise(count_histo = sum(nb_med, na.rm = TRUE), .groups = "drop")
    }
    else if (startsWith(input$arrondissement_bpe, "QPV :")) {
      # Cas 3 : Commence par "QPV :"
      qpv_name <- gsub("^QPV : ", "", input$arrondissement_bpe) # Supprimer le préfixe "QPV :"
      filtered_data <- bpe23_agreg %>%
        st_drop_geometry() %>% 
        filter(libelle %in% input$type_med_bpe & lib_qp == qpv_name) %>% 
        select(libelle, lib_qp, nb_med) %>% 
        group_by(libelle) %>%
        summarise(count_histo = sum(nb_med, na.rm = TRUE), .groups = "drop")
    }
    
    # Ajouter les types d'établissements manquants avec un compte de 0
    all_types <- unique(input$type_med_bpe)
    missing_types <- setdiff(all_types, filtered_data$libelle)
    
    if (length(missing_types) > 0) {
      missing_data <- tibble(
        libelle = missing_types,  # Colonne libelle
        count_histo = 0                 # Colonne count_histo
      )
      filtered_data <- bind_rows(filtered_data, missing_data)
    }
    
    # Retourner les données filtrées
    filtered_data
  })
  
  
  #### ----------------------------------------------------------------------------------------
  
  # Filtrage des données en fonction de l'arrondissement sélectionné et du type de medecin ANNUAIRE
  filtered_ann_data <- reactive({
    req(input$arrondissement_ann)  # Assurez-vous qu'un arrondissement est sélectionné
    req(input$type_med_annuaire)  # Assurez-vous qu'au moins un type de medecin
    
    # Filtrer les données pour l'arrondissement sélectionné ou pour toute la ville
    if (input$arrondissement_ann == "Ville de Lyon") {
      filtered_data <- annuaire24_agr %>%
        st_drop_geometry() %>% 
        filter(libelle %in% input$type_med_annuaire & LIBCOM %in% unique(arrondissement$NOM_COM.x)) %>% 
        select(libelle, LIBCOM, nb_med) %>% 
        group_by(libelle) %>%
        summarise(count_histo = sum(nb_med, na.rm = TRUE), .groups = "drop") %>% 
        mutate(LIBCOM=NA)
      
    } else if (startsWith(input$arrondissement_ann, "Lyon")) {
      filtered_data <- annuaire24_agr %>%
        st_drop_geometry() %>% 
        filter(libelle %in% input$type_med_annuaire & LIBCOM == input$arrondissement_ann) %>% 
        select(libelle, LIBCOM, nb_med) %>% 
        group_by(libelle) %>%
        summarise(count_histo = sum(nb_med, na.rm = TRUE), .groups = "drop")
    }
    else if (startsWith(input$arrondissement_ann, "QPV :")) {
      # Cas 3 : Commence par "QPV :"
      qpv_name <- gsub("^QPV : ", "", input$arrondissement_ann) # Supprimer le préfixe "QPV :"
      filtered_data <- annuaire24_agr %>%
        st_drop_geometry() %>% 
        filter(libelle %in% input$type_med_annuaire & lib_qp == qpv_name) %>% 
        select(libelle, lib_qp, nb_med) %>% 
        group_by(libelle) %>%
        summarise(count_histo = sum(nb_med, na.rm = TRUE), .groups = "drop")
    }
    
    
    # Ajouter les types d'établissements manquants avec un compte de 0
    all_types <- unique(input$type_med_annuaire)
    missing_types <- setdiff(all_types, filtered_data$libelle)
    
    if (length(missing_types) > 0) {
      missing_data <- tibble(
        libelle = missing_types,  # Colonne libelle
        count_histo = 0                 # Colonne count_histo
      )
      filtered_data <- bind_rows(filtered_data, missing_data)
    }
    
    # Retourner les données filtrées
    filtered_data
  })
  


  
  #### ----------------------------------------------------------------------------------------
  
  # Filtrage des données en fonction de l'arrondissement sélectionné :  COMPARAISON BPE, ANNUAIRE
  filtered_comp_data <- reactive({
    req(input$arrondissement_comp)  # Assurez-vous qu'un arrondissement est sélectionné
    req(input$type_med_comparaison)  # Assurez-vous qu'un type de médecin est sélectionné

    if (length(input$type_med_comparaison) == 0) {
      return(data.frame())  # Retourner un data.frame vide si aucune sélection
    }
    
    
    # Filtrer les données pour l'arrondissement sélectionné ou pour toute la ville
    if (input$arrondissement_comp == "Ville de Lyon") {
      #annuaire
      filtered_data_ann <- annuaire24_agr %>%
        st_drop_geometry() %>% 
        filter(libelle %in% input$type_med_comparaison & LIBCOM %in% unique(arrondissement$NOM_COM.x)) %>% 
        select(libelle, LIBCOM, nb_med) %>% 
        group_by(libelle) %>%
        summarise(count_ann =sum(nb_med, na.rm = TRUE), .groups = "drop") %>% 
        mutate(LIBCOM=NA)
      #bpe
      filtered_data_bpe <- bpe23_agreg %>%
        st_drop_geometry() %>% 
        filter(libelle %in% input$type_med_comparaison & LIBCOM %in% unique(str_to_upper(arrondissement$NOM_COM.x))) %>% 
        select(libelle, LIBCOM, nb_med) %>% 
        group_by(libelle) %>%
        summarise(count_bpe = sum(nb_med, na.rm = TRUE), .groups = "drop") %>% 
        mutate(LIBCOM=NA)
    } else {
      #annuaire
      filtered_data_ann <- annuaire24_agr %>%
        st_drop_geometry() %>% 
        filter(libelle %in% input$type_med_comparaison & LIBCOM == input$arrondissement_comp) %>% 
        select(libelle, LIBCOM, nb_med) %>% 
        group_by(libelle) %>%
        summarise(count_ann =sum(nb_med, na.rm = TRUE), .groups = "drop")
      #bpe
      arr <- str_to_upper(input$arrondissement_comp)
      filtered_data_bpe <- bpe23_agreg %>%
        st_drop_geometry() %>% 
        filter(libelle %in% input$type_med_comparaison & LIBCOM == arr) %>% 
        select(libelle, LIBCOM, nb_med) %>% 
        group_by(libelle) %>%
        summarise(count_bpe = sum(nb_med, na.rm = TRUE), .groups = "drop")
    }

    # Gérer les valeurs manquantes
    if (nrow(filtered_data_ann) == 0) {
      filtered_data_ann <- tibble(libelle = input$type_med_comparaison, count_ann = 0)
    }
    if (nrow(filtered_data_bpe) == 0) {
      filtered_data_bpe <- tibble(libelle = input$type_med_comparaison, count_bpe = 0)
    }
      
    # Fusionner les données BPE et Annuaire
    filtered_data <- full_join(filtered_data_bpe, filtered_data_ann, by = "libelle") %>%
      pivot_longer(cols = starts_with("count"), names_to = "source", values_to = "count")
    
    # Retourner les données filtrées
    filtered_data
  })
  
  #############################################
  ## Code couleurs et fonctions reactives  ####
  #############################################
 
  

  
  # Palette de couleurs personnalisée pour les établissements de santé
  color_palette_eta <- reactive({
    function(type) {
      couleurs_types[[type]]
    }
  })
  
  # Palette de couleurs personnalisée pour les médecins BPE
  color_palette_bpe <- reactive({
    function(type) {
      couleurs_types[[type]]
    }
  })
  
  # Palette de couleurs personnalisée pour les médecins de l'annuaire
  color_palette_annuaire <- reactive({
    function(type) {
      couleurs_types[[type]]
    }
  })
  
  
  #############################################
  ## Initialisation et nettoyage           ####
  #############################################
  
  # Initialisation de la carte vide
  output$map_ong1 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(minZoom = 12, maxZoom = 20)) %>%
      setView(lng = 4.85, lat = 45.75, zoom = 12) %>%  # Vue par défaut sans couches ajoutées
      addPolylines(
        data = arrondissement,
        color = "blue",      # Couleur des contours
        weight = 2,          # Épaisseur des contours
        opacity = 1,         # Opacité des contours (1 = opaque)
        popup = ~paste("Arrondissements de Lyon"),
        group = "Arrondissements de Lyon"
      ) %>%
      addPolylines(
        data = qpv,
        color = "orange",    # Couleur des contours
        weight = 2,          # Épaisseur des contours
        opacity = 1,         # Opacité des contours (1 = opaque)
        group = "Quartiers prioritaires de la politique de la ville",
        popup = ~paste("QPV :", lib_qp)
        
      ) %>%
      addLayersControl(
        overlayGroups = c("Arrondissements de Lyon", "Quartiers prioritaires de la politique de la ville"),
        options = layersControlOptions(collapsed = FALSE)
        )
  })
    
  # Vider la carte au changement d'onglet
  observeEvent(input$tabs_ong1, {
    leafletProxy("map_ong1") %>%
      clearControls() %>%  # Supprimer toutes les légendes
      clearGroup("Etablissements") %>%
      clearGroup("BPE") %>%
      clearGroup("Annuaire") %>%
      clearGroup("Comparaison") %>% 
      clearGroup("Médecins BPE")  %>% 
      clearGroup("Médecins Annuaire")
  })
  
  # Vider la carte au changement d'onglet, dans l'onglet 2 
  observeEvent(input$BD_med, {
    leafletProxy("map_ong1") %>%
      clearControls() %>%  # Supprimer toutes les légendes
      clearGroup("Etablissements") %>%
      clearGroup("BPE") %>%
      clearGroup("Annuaire") %>%
      clearGroup("Comparaison") %>% 
      clearGroup("Médecins BPE")  %>% 
      clearGroup("Médecins Annuaire")
  })
  
  
  #############################################
  ## Observateurs                          ####
  #############################################
  
  # Observateur pour les établissements de santé
  observe({
    if (input$tabs_ong1 == "tab1") {
      selected_eta <- input$type_eta
      
      # Si la case est décochée, supprimer les marqueurs et la légende
      if (is.null(selected_eta) || length(selected_eta) == 0) {
        leafletProxy("map_ong1") %>%
          clearGroup("Etablissements") %>%
          removeControl(layerId = "legend_eta")
      } else {
        # Si la case est cochée, ajouter les marqueurs et la légende
        eta_data <- bpe23 %>% filter(libelle %in% selected_eta)
        palette_eta <- color_palette_eta()
        
        leafletProxy("map_ong1") %>%
          clearGroup("Etablissements") %>%
          addCircleMarkers(
            data = eta_data,
            color = unlist(lapply(eta_data$libelle, palette_eta)),  # Utiliser la palette personnalisée
            stroke = FALSE,
            fillOpacity = 1,
            radius = 5,
            popup = ~paste("Établissement:", libelle, "<br>Nom:", NOMRS), 
            group = "Etablissements"
          ) %>%
          addLegend(
            position = "bottomright",
            colors = unlist(lapply(selected_eta, palette_eta)),  # Couleurs pour la légende
            labels = selected_eta,  # Étiquettes pour la légende
            title = "Établissements de santé",
            opacity = 1,
            layerId = "legend_eta"
          ) 
        
        output$histogram_eta <- renderPlot({
          req(filtered_eta_data())  # Assurez-vous que les données sont disponibles
          
          # Créer l'histogramme
          ggplot(filtered_eta_data(), aes(x = libelle, y = count, fill = libelle)) +
            geom_bar(stat = "identity", aes(color = count == 0), size = 1.2, show.legend = FALSE) +  # Ajouter une bordure pour les valeurs 0
            geom_text(
              aes(
                label = ifelse(count > max(count) * 0.05, as.character(count), "")  # Afficher le label uniquement si la barre est suffisamment haute
              ),
              position = position_stack(vjust = 0.5),
              colour = "white",
              size = 6
            ) +  # Ajouter les valeurs de `count` au-dessus des barres
            scale_fill_manual(values = couleurs_types) +  # Appliquer la palette de couleurs personnalisée
            scale_color_manual(values = c("TRUE" = "black", "FALSE" = "transparent")) +  # Bordure noire pour les valeurs 0, transparente sinon
            labs(
              title = if (input$arrondissement_eta == "Ville de Lyon") {
                "Nombre d'établissements par type\nDans toute la ville"
              } else {
                paste("Nombre d'établissements par type \n",input$arrondissement_eta)
              },
              x = "Type d'établissement",
              y = "Nombre"
            ) +
            theme_minimal() +
            theme(
              axis.text.x = element_text(
                angle = 60,  # Incliner le texte à 60 degrés
                hjust = 1,   # Aligner le texte à droite
                vjust = 1,   # Ajuster l'alignement vertical
                size = 12,   # Taille du texte
                lineheight = 0.8  # Espacement entre les lignes
              ),
              axis.title.x = element_text(size = 12, face = "bold")  # Personnaliser le titre de l'axe des x
            ) +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 25))  # Retour à la ligne tous les 25 caractères
        })
      }
    }
  })
  
  # Observer le zoom
  observeEvent(input$arrondissement_eta, {
    req(input$arrondissement_eta)  # Assurez-vous qu'il y a un input
    
    if (input$arrondissement_eta == "Ville de Lyon"){
      #Zoom sur la ville de Lyon : zoom initial
      leafletProxy("map_ong1") %>%
        setView(lng = 4.85, lat = 45.75, zoom = 12)
    } 
    
    else if (startsWith(input$arrondissement_eta, "Lyon")) {
    
    # Récupérer les coordonnées de l'arrondissement sélectionné
    arrondissement_selected <- arrondissement %>% filter(NOM_COM.x == input$arrondissement_eta)
    
    
    # Calculer la boîte englobante (bbox) de l'arrondissement
    bbox <- st_bbox(arrondissement_selected)
    y <- c(bbox$ymin, bbox$ymax) 
    x <- c(bbox$xmin, bbox$xmax) 

    # Zoom sur l'arrondissement
    leafletProxy("map_ong1") %>%
      setView(lng = mean(x), lat = mean(y), zoom = 14)
    
    } else if (startsWith(input$arrondissement_eta, "QPV :")) {
      
    # Récupérer le QPV sélectionné
    qpv_selected <- qpv %>% filter(lib_qp == gsub("^QPV : ", "", input$arrondissement_eta))
    
    # Calculer la boîte englobante (bbox) de l'arrondissement
    bbox_qpv <- st_bbox(qpv_selected)
    y <- c(bbox_qpv$ymin, bbox_qpv$ymax) 
    x <- c(bbox_qpv$xmin, bbox_qpv$xmax) 
    
    # Zoom sur l'arrondissement
    leafletProxy("map_ong1") %>%
      setView(lng = mean(x), lat = mean(y), zoom = 15)
    }
})
  
#### ----------------------------------------------------------------------------------------

# Observateur des médecins BPE
  observe({
    if (input$tabs_ong1 == "tab2" && input$BD_med == "BPE") {
      # Vérifier si les cases sont décochées
      if (is.null(input$type_med_bpe) || length(input$type_med_bpe) == 0) {
        # Si les cases sont décochées, supprimer les marqueurs et la légende
        leafletProxy("map_ong1") %>%
          clearGroup(group = "BPE") %>%  # Supprimer les marqueurs du groupe "BPE"
          removeControl(layerId = "legend_bpe")  # Supprimer la légende
      } else {
        # Si les cases sont cochées, ajouter les marqueurs et la légende   
      medecin_data_bpe <- selected_data_bpe()
      
      if (nrow(medecin_data_bpe) > 0) {
        palette_bpe <- color_palette_bpe()
        
        # Créer une liste de couleurs et de labels
        colors_bpe <- unlist(lapply(input$type_med_bpe, palette_bpe))
        labels_bpe <- input$type_med_bpe
        
        leafletProxy("map_ong1") %>%
          clearGroup(group = "BPE") %>%  # Supprimer les anciens marqueurs du groupe "BPE"
          addCircleMarkers(
            data = medecin_data_bpe,
            fillColor = unlist(lapply(medecin_data_bpe$libelle, palette_bpe)),  # Couleur de remplissage
            stroke = TRUE,  # Activer la bordure
            weight = 2,  # Épaisseur de la bordure
            color = "white",  # Couleur de la bordure (blanc)
            fillOpacity = 1,  # Opacité du remplissage
            radius = ~log(nb_med + 1) * 5,  # Rayon proportionnel à nb_med
            popup = ~paste("Spécialité : ", libelle, "<br>Nombre de médecins:", nb_med),  # Popup
            group = "BPE"  # Groupe pour gérer les marqueurs
          ) %>%
          addLegend(
            position = "bottomright",
            colors = colors_bpe,
            labels = labels_bpe,
            title = "Spécialités (BPE)",  
            opacity = 1,
            layerId = "legend_bpe"
          ) 
        
        # Histogramme
        output$histogram_med_bpe <- renderPlot({
          req(filtered_bpe_data())  # Assurez-vous que les données sont disponibles
          
          ggplot(filtered_bpe_data(), aes(x = libelle, y = count_histo, fill = libelle)) +
            geom_bar(stat = "identity", aes(color = count_histo == 0), size = 1.2, show.legend = FALSE) +  # Ajouter une bordure pour les valeurs 0
            geom_text(
              aes(
                label = ifelse(count_histo > max(count_histo) * 0.05, as.character(count_histo), "")  # Afficher le label uniquement si la barre est suffisamment haute
              ),
              position = position_stack(vjust = 0.5),
              colour = "white",
              size = 6
            ) +  # Ajouter les valeurs de `count_histo` au-dessus des barres
            scale_fill_manual(values = couleurs_types) +  # Appliquer la palette de couleurs personnalisée
            scale_color_manual(values = c("TRUE" = "black", "FALSE" = "transparent")) +  # Bordure noire pour les valeurs 0, transparente sinon
            labs(
              title = if (input$arrondissement_bpe == "Ville de Lyon") {
                "Nombre de médecins par type\nDans toute la ville"
              } else {
                paste("Nombre de médecins par type dans\n",input$arrondissement_bpe)
              },
              x = "Type de médecin",
              y = "Nombre"
            ) +
            theme_minimal() +
            theme(
              axis.text.x = element_text(
                angle = 55,  # Incliner le texte à 45 degrés
                hjust = 1,   # Aligner le texte à droite
                vjust = 1,   # Ajuster l'alignement vertical
                size = 12,   # Taille du texte
                lineheight = 0.8  # Espacement entre les lignes
              ),
              axis.title.x = element_text(size = 12, face = "bold")  # Personnaliser le titre de l'axe des x
            ) +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 25))  # Retour à la ligne tous les 25 caractères
        })
      }
      }
    }
  })
  
  # Observer le zoom
  observeEvent(input$arrondissement_bpe, {
    req(input$arrondissement_bpe)  # Assurez-vous qu'il y a un input
    
    if (input$arrondissement_bpe == "Ville de Lyon"){
      #Zoom sur la ville de Lyon : zoom initial
      leafletProxy("map_ong1") %>%
        setView(lng = 4.85, lat = 45.75, zoom = 12)
    }
    else if (startsWith(input$arrondissement_bpe, "Lyon")) {
      
      # Récupérer les coordonnées de l'arrondissement sélectionné
      arrondissement_selected <- arrondissement %>% filter(NOM_COM.x == input$arrondissement_bpe)
      
      
      # Calculer la boîte englobante (bbox) de l'arrondissement
      bbox <- st_bbox(arrondissement_selected)
      y <- c(bbox$ymin, bbox$ymax) 
      x <- c(bbox$xmin, bbox$xmax) 
      
      # Zoom sur l'arrondissement
      leafletProxy("map_ong1") %>%
        setView(lng = mean(x), lat = mean(y), zoom = 14)
    }
    else if (startsWith(input$arrondissement_bpe, "QPV :")) {
      # Récupérer le QPV sélectionné
      qpv_selected <- qpv %>% filter(lib_qp == gsub("^QPV : ", "", input$arrondissement_bpe))
      
      # Calculer la boîte englobante (bbox) de l'arrondissement
      bbox_qpv <- st_bbox(qpv_selected)
      y <- c(bbox_qpv$ymin, bbox_qpv$ymax) 
      x <- c(bbox_qpv$xmin, bbox_qpv$xmax) 
      
      # Zoom sur l'arrondissement
      leafletProxy("map_ong1") %>%
        setView(lng = mean(x), lat = mean(y), zoom = 15)
    }
    
  })

      
#### ----------------------------------------------------------------------------------------
  
  #observateur des médecins de l'annuaire
    observe({
      if (input$tabs_ong1 == "tab2" && input$BD_med == "Annuaire") {
        # Vérifier si les cases sont décochées
        if (is.null(input$type_med_annuaire) || length(input$type_med_annuaire) == 0) {
          # Si les cases sont décochées, supprimer les marqueurs et la légende
          leafletProxy("map_ong1") %>%
            clearGroup(group = "Annuaire") %>%  # Supprimer les marqueurs du groupe 
            removeControl(layerId = "legend_annuaire")  # Supprimer la légende
        } else {
        medecin_data_annuaire <- selected_data_annuaire()
        if (nrow(medecin_data_annuaire) > 0) {
          palette_an <- color_palette_annuaire()
          
          # Créer une liste de couleurs et de labels
          colors_an <- unlist(lapply(input$type_med_annuaire, palette_an))
          labels_an <- input$type_med_annuaire
          
          leafletProxy("map_ong1") %>%
            clearGroup(group = "Annuaire") %>%
            addCircleMarkers(
              data = medecin_data_annuaire,
              fillColor = unlist(lapply(medecin_data_annuaire$libelle, palette_an)),  # Utiliser la palette pour "Annuaire"
              stroke = TRUE,  # Activer la bordure
              weight = 2,  # Épaisseur de la bordure
              color = "white",  # Couleur de la bordure (blanc)
              fillOpacity = 1,
              radius = ~log(nb_med + 1) * 5,
              popup = ~paste("Spécialité :", libelle, "<br>Nombre de médecins:", nb_med),  
              group = "Annuaire"
            ) %>%
            addLegend(
              position = "bottomright",
              colors = colors_an,
              labels = labels_an,
              title = "Spécialités (Annuaire)",
              opacity = 1,
              layerId = "legend_annuaire"
            )
          
          output$histogram_med_annuaire <- renderPlot({
            req(filtered_ann_data())  # Assurez-vous que les données sont disponibles
            
            # Créer l'histogramme
            ggplot(filtered_ann_data(), aes(x = libelle, y = count_histo, fill = libelle)) +
              geom_bar(stat = "identity", aes(color = count_histo == 0), size = 1.2, show.legend = FALSE) +  # Ajouter une bordure pour les valeurs 0
              geom_text(
                aes(
                  label = ifelse(count_histo > max(count_histo) * 0.05, as.character(count_histo), "")  # Afficher le label uniquement si la barre est suffisamment haute
                ),
                position = position_stack(vjust = 0.5),
                colour = "white",
                size = 6
              ) +  # Ajouter les valeurs de `count_histo` au-dessus des barres
              scale_fill_manual(values = couleurs_types) +  # Appliquer la palette de couleurs personnalisée
              scale_color_manual(values = c("TRUE" = "black", "FALSE" = "transparent")) +  # Bordure noire pour les valeurs 0, transparente sinon
              labs(
                title = if (input$arrondissement_bpe == "Ville de Lyon") {
                  "Nombre de médecins par type\nDans toute la ville"
                } else {
                  paste("Nombre de médecins par type dans\n",input$arrondissement_ann)
                },
                x = "Type de médecin",
                y = "Nombre"
              ) +
              theme_minimal() +
              theme(
                axis.text.x = element_text(
                  angle = 55,  # Incliner le texte à 45 degrés
                  hjust = 1,   # Aligner le texte à droite
                  vjust = 1,   # Ajuster l'alignement vertical
                  size = 12,   # Taille du texte
                  lineheight = 0.8  # Espacement entre les lignes
                ),
                axis.title.x = element_text(size = 12, face = "bold")  # Personnaliser le titre de l'axe des x
              ) +
              scale_x_discrete(labels = function(x) str_wrap(x, width = 25))  # Retour à la ligne tous les 25 caractères
          })
        }
        }
      }
    })
  
  # Observer le zoom
  observeEvent(input$arrondissement_ann, {
    req(input$arrondissement_bpe)  # Assurez-vous qu'il y a un input
    
    if (input$arrondissement_ann == "Ville de Lyon"){
      #Zoom sur la ville de Lyon : zoom initial
      leafletProxy("map_ong1") %>%
        setView(lng = 4.85, lat = 45.75, zoom = 12)
    } 
    else if (startsWith(input$arrondissement_ann, "Lyon")) {

      # Récupérer les coordonnées de l'arrondissement sélectionné
      arrondissement_selected <- arrondissement %>% filter(NOM_COM.x == input$arrondissement_ann)
      
      
      # Calculer la boîte englobante (bbox) de l'arrondissement
      bbox <- st_bbox(arrondissement_selected)
      y <- c(bbox$ymin, bbox$ymax) 
      x <- c(bbox$xmin, bbox$xmax) 
      
      # Zoom sur l'arrondissement
      leafletProxy("map_ong1") %>%
        setView(lng = mean(x), lat = mean(y), zoom = 14)
    } 
    
    else if (startsWith(input$arrondissement_ann, "QPV :")) {
      # Récupérer le QPV sélectionné
    qpv_selected <- qpv %>% filter(lib_qp == gsub("^QPV : ", "", input$arrondissement_ann))
      
      
      # Calculer la boîte englobante (bbox) de l'arrondissement
      bbox_qpv <- st_bbox(qpv_selected)
      y <- c(bbox_qpv$ymin, bbox_qpv$ymax) 
      x <- c(bbox_qpv$xmin, bbox_qpv$xmax) 
      
      # Zoom sur l'arrondissement
      leafletProxy("map_ong1") %>%
        setView(lng = mean(x), lat = mean(y), zoom = 16)
    }
  })
  
#### ----------------------------------------------------------------------------------------
    
    # Observer la comparaison
    observe({
      if (input$tabs_ong1 == "tab2" && input$BD_med == "Comparaison"){
        type_med <- input$type_med_comparaison  # Le type de médecin sélectionné
        
        # Filtrage des médecins de BPE23 selon le type sélectionné
        medecin_data_bpe <- bpe23_agreg %>% filter(libelle %in% input$type_med_comparaison)
        
        # Filtrage des médecins de Annuaire24 selon le type sélectionné
        medecin_data_annuaire <- annuaire24_agr %>% filter(libelle %in% input$type_med_comparaison)
        
        # Affichage des médecins de BPE23 sur la carte
          
          leafletProxy("map_ong1") %>%
            clearGroup("Médecins BPE") %>%
            clearGroup("Médecins Annuaire") %>%
            clearControls() %>%  # Supprimer toutes les légendes existantes
            addCircleMarkers(
              data = medecin_data_bpe,
              color = "orange",
              stroke = FALSE,
              fillOpacity = 0.6,
              radius = ~log(nb_med + 1) * 5,
              popup = ~paste("Spécialité (BPE) :", libelle, "<br>Nombre de medecins:", nb_med),
              group = "Médecins BPE"  # Groupe pour les marqueurs BPE
            ) %>% 
            addCircleMarkers(
              data = medecin_data_annuaire,
              color = "blue",
              stroke = FALSE,
              fillOpacity = 0.5,
              radius = ~log(nb_med + 1) * 5,
              popup = ~paste("Spécialité (Annuaire) ", libelle, "<br>Nombre de medecins:", nb_med),
              group = "Médecins Annuaire"  # Groupe pour les marqueurs Annuaire
            ) %>% 
            addLegend(
              position = "bottomright",
              colors = c("orange", "blue"),
              labels = c("Médecins BPE", "Médecins Annuaire"),
              title = "",
              opacity = 1,
              layerId = "legend_comparaison"  # Identifiant unique pour la légende
            ) 
          
          # Masquer ou afficher les groupes en fonction des cases à cocher
          if (!is.null(input$show_bpe) && !input$show_bpe) {
            leafletProxy("map_ong1") %>% hideGroup("Médecins BPE")
          } else {
            leafletProxy("map_ong1") %>% showGroup("Médecins BPE")
          }
          
          if (!is.null(input$show_annuaire) && !input$show_annuaire) {
            leafletProxy("map_ong1") %>% hideGroup("Médecins Annuaire")
          } else {
            leafletProxy("map_ong1") %>% showGroup("Médecins Annuaire")
          }
          
          
          #histogramme comparaison
          output$histogram_comparaison <- renderPlot({
            req(filtered_comp_data())  # Assurez-vous que les données sont disponibles
            
            # Créer l'histogramme
            ggplot(filtered_comp_data(), aes(x = libelle, y = count, fill = source))  +
              geom_bar(stat = "identity", position = "dodge") +  # Barres côte à côte
              scale_fill_manual(values = c("blue", "orange")) +  
              labs(
                title = "Comparaison du nombre de médecins\npar spécialité",
                y = "Nombre",
                x = ""
              ) +
              theme_minimal() +
              theme(
                axis.text.x = element_text(
                  angle = 55,  # Incliner le texte à 45 degrés
                  hjust = 1,   # Aligner le texte à droite
                  vjust = 1,   # Ajuster l'alignement vertical
                  size = 12,   # Taille du texte
                  lineheight = 0.8  # Espacement entre les lignes
                ),
                axis.title.x = element_text(size = 12, face = "bold"),  # Personnaliser le titre de l'axe des x
                legend.position = "none"  # Supprimer la légende
              ) +
              scale_x_discrete(labels = function(x) str_wrap(x, width = 25))  # Retour à la ligne tous les 25 caractères
          })
          
        
      }
    })
  
  # Observer le zoom
  observeEvent(input$arrondissement_comp, {
    req(input$arrondissement_comp)  # Assurez-vous qu'il y a un input
    
    if (input$arrondissement_comp == "Ville de Lyon"){
      #Zoom sur la ville de Lyon : zoom initial
      leafletProxy("map_ong1") %>%
        setView(lng = 4.85, lat = 45.75, zoom = 12)
    }else{
      
      # Récupérer les coordonnées de l'arrondissement sélectionné
      arrondissement_selected <- arrondissement %>% filter(NOM_COM.x == input$arrondissement_comp)
      
      
      # Calculer la boîte englobante (bbox) de l'arrondissement
      bbox <- st_bbox(arrondissement_selected)
      y <- c(bbox$ymin, bbox$ymax) 
      x <- c(bbox$xmin, bbox$xmax) 
      
      # Zoom sur l'arrondissement
      leafletProxy("map_ong1") %>%
        setView(lng = mean(x), lat = mean(y), zoom = 14)
    } 
  })
  
 
}