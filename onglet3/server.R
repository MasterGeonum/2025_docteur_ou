server_3 <- function(input, output, session) {
  
  # Ajout de préfixes aux IDs pour différencier médecins et arrêts
  bpe23$id <- paste0("med_", as.character(seq_len(nrow(bpe23))))
  med_annuaire24$id <- paste0("med_", as.character(seq_len(nrow(med_annuaire24))))
  arret_transport$id <- paste0("arret_", as.character(seq_len(nrow(arret_transport))))
  
  # Variables réactives
  selected_medecin <- reactiveVal(NULL)
  display_arrets <- reactiveVal(FALSE)
  selected_lines <- reactiveVal(NULL)
  
  BPE_filtre <- reactive({
    req(input$type_med_bpe)
    if (input$etamed == "med") {
      bpe23 %>%
        filter(grd_ctg == "Médecin", libelle %in% input$type_med_bpe)
    } else if (input$etamed == "eta") {
      bpe23 %>%
        filter(grd_ctg == "Etablissement", libelle %in% input$type_med_bpe)
    } else {
      bpe23
    }
  })
  output$toggle_text <- renderText({
    if (input$toggle_vue) {
      "Affichez la carte des ressources de santé et arrêts à proximité"
    } else {
      "Affichez le tableau des lignes et ressources de santé à proximité"
    }
  })
  arrets_proches_unique <- reactive({
    req(input$dist_arret)
    buffer_arrets <- st_buffer(arret_transport, dist = input$dist_arret)
    
    # Sélection des médecins sous ce buffer
    medecins_proches <- BPE_filtre()[st_intersects(BPE_filtre(), buffer_arrets, sparse = FALSE), ]
    if (nrow(medecins_proches) == 0) return(NULL)
    
    # Associer chaque arrêt à ses médecins proches et grouper par ligne
    medecins_par_arret <- medecins_proches %>%
      st_drop_geometry() %>%
      group_by(arret_transport$ligne, libelle) %>%
      summarise(Effectif = n(), .groups = "drop") %>%
      pivot_wider(names_from = libelle, values_from = Effectif, values_fill = list(Effectif = 0))
    return(medecins_par_arret)
  })
  
  # Carte principale avec médecins et arrêts
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(minZoom = 12, maxZoom = 20))%>%
      
      # ✅ Ajout des lignes TCL dès le départ
      addPolylines(
        data = lignes,  
        color = ~couleur,  
        weight = 3, opacity = 0.8, smoothFactor = 1
      )
  })
  
  # ✅ Mise à jour dynamique de la carte dès qu'un élément est sélectionné
  observe({
    req(input$tabs_ong3 == "Accessibilité par transport")  # Vérifie que l'onglet est actif
    
    map <- leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      addPolylines(
        data = lignes,  
        color = ~couleur,  
        weight = 3, opacity = 0.8, smoothFactor = 1
      )
    
    if (nrow(BPE_filtre()) > 0) {  
      # ✅ Affichage des médecins ou établissements
      map %>% addCircleMarkers(
        data = BPE_filtre(),  
        layerId = ~id, 
        color = "white",  
        fillColor = ~ifelse(libelle %in% names(couleurs_types), 
                            couleurs_types[libelle], 
                            couleurs_types["Autre"]),
        fillOpacity = 0.8, 
        radius = 5, 
        stroke = TRUE,  
        weight = 1  
      )
    }
  })
  
  
  observe({
    data_legende <- BPE_filtre()
    if (nrow(data_legende) > 0) {
      specialites_presentees <- unique(data_legende$libelle)
      couleurs_presentees <- couleurs_types[specialites_presentees]
      leafletProxy("map") %>%
        clearControls() %>%
        addLegend(position = "bottomright",
                  title = "Spécialité",
                  colors = couleurs_presentees,
                  labels = specialites_presentees,
                  opacity = 1)
    }
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (is.null(click$id)) return()
    
    if (grepl("^med_", click$id)) {  # ✅ Sélection d'un médecin
      medecin_select <- BPE_filtre() %>% filter(id == click$id) %>% slice(1)
      
      if (nrow(medecin_select) == 0) return()
      selected_medecin(medecin_select)
      display_arrets(TRUE)
      
      # ✅ Calcul du buffer autour du médecin sélectionné
      buffer_zone <- st_buffer(medecin_select, dist = input$dist_arret)
      
      if (!inherits(buffer_zone, "sf") || nrow(buffer_zone) == 0) {
        print("Erreur : Buffer non valide")
        return()
      }
      
      # ✅ Trouver les arrêts dans la zone tampon
      arrets_proches <- arret_transport[st_intersects(arret_transport, buffer_zone, sparse = FALSE), ]
      
      if (nrow(arrets_proches) == 0) return()
      
      # ✅ Ajout des distances entre le médecin sélectionné et les arrêts trouvés
      arrets_proches <- arrets_proches %>%
        mutate(distance = as.numeric(st_distance(geometry, st_geometry(medecin_select[1, ]), by_element = FALSE)))
      
      # ✅ Sélectionner l'arrêt le plus proche pour chaque ligne
      arrets_proches_unique <- arrets_proches %>%
        group_by(ligne) %>%
        slice_min(order_by = distance, n = 1) %>%
        ungroup()
      
      # ✅ Supprime les anciens arrêts avant d'en ajouter de nouveaux
      leafletProxy("map") %>%
        clearMarkers() %>%  # ✅ Supprime TOUS les anciens marqueurs
        clearShapes() %>%   # ✅ Supprime les anciens cercles de distance
        addCircleMarkers(
          data = BPE_filtre(),
          layerId = ~id,
          color = "white",
          fillColor = ~ifelse(libelle %in% names(couleurs_types), 
                              couleurs_types[libelle], 
                              couleurs_types["Autre"]),
          fillOpacity = 0.8,
          radius = 5,
          stroke = TRUE,
          weight = 1
        ) %>%
        addPolygons(
          data = buffer_zone,  # ✅ Ajoute le cercle de distance
          color = "#0045ce",
          weight = 2,
          fillColor = "#90e8ff",
          fillOpacity = 0.2
        ) %>%
        addMarkers(
          data = arrets_proches_unique,
          layerId = ~id,
          icon = arret_icon,
          popup = ~paste0("<b>", nom, "</b><br><br>Ligne desservie: ", ligne)
        )
      
      # ✅ Mise à jour immédiate du tableau des arrêts dans le sidebar
      output$table_lignes <- renderTable({
        req(display_arrets())
        
        data.frame(
          "Ligne TCL" = arrets_proches_unique$ligne,
          "Arrêt le plus proche" = arrets_proches_unique$nom,
          "Distance (m)" = round(arrets_proches_unique$distance, 1)
        )
      })
      
      output$table_lignes_non_vide <- reactive({
        req(display_arrets())  # Vérifie si les arrêts doivent être affichés
        return(!is.null(arrets_proches_unique) && nrow(arrets_proches_unique) > 0)  
      })
      
      outputOptions(output, "table_lignes_non_vide", suspendWhenHidden = FALSE)
      
      
    }
  })
  
  #####################################################################################################
  output$titre_tableau_medecins <- renderText({
    paste("Nombre de ressources de santé à moins de", input$dist_arret, "mètres d'une ligne dans la zone d'étude")
  })
  # 📌 Stocker le tableau dans une variable réactive
  tableau_lignes_reactif <- reactive({
    req(input$dist_arret)
    
    # ✅ Mise à jour des ID avec le préfixe "arret_"
    distance_filtered <- distance_df %>%
      filter(distance <= input$dist_arret) %>%
      mutate(id_arret = paste0("arret_", id_arret), id_medecin = paste0("med_", id_medecin))
    
    # ✅ Joindre les informations des médecins et arrêts
    medecins_proches <- distance_filtered %>%
      left_join(bpe23 %>% select(id, grd_ctg, libelle), by = c("id_medecin" = "id")) %>%
      left_join(arret_transport %>% select(id, ligne), by = c("id_arret" = "id"))
    
    # ✅ Filtrer selon le type de ressource sélectionné
    if (input$etamed == "med") {
      medecins_proches <- medecins_proches %>% filter(grd_ctg == "Médecin")
    } else if (input$etamed == "eta") {
      medecins_proches <- medecins_proches %>% filter(grd_ctg == "Etablissement")
    }
    
    # ✅ Extraction du préfixe et du numéro des lignes pour tri
    medecins_proches <- medecins_proches %>%
      mutate(
        ligne_alpha = gsub("[0-9]", "", ligne),  # ✅ Récupère uniquement les lettres (préfixe)
        ligne_num = as.numeric(gsub("[^0-9]", "", ligne))  # ✅ Extrait uniquement les chiffres
      )
    
    # ✅ Application du filtre de type de ligne
    if (input$filtre_ligne != "Toutes") {
      if (input$filtre_ligne == "Bus Standard") {
        medecins_proches <- medecins_proches %>% filter(ligne_alpha == "")
      } else {
        lettre_correspondante <- switch(input$filtre_ligne,
                                        "Tramway" = "T",
                                        "Bus Fréquent" = "C",
                                        "Lignes A" = "A",
                                        "Lignes Pleine Lune" = "PL",
                                        "Navette Spéciale" = "S")
        medecins_proches <- medecins_proches %>% filter(ligne_alpha == lettre_correspondante)
      }
    }
    
    # ✅ Tri des lignes : d'abord par type, ensuite numériquement
    medecins_proches <- medecins_proches %>%
      arrange(
        factor(ligne_alpha, levels = c("", "C", "T", "A", "PL", "S")),  # ✅ Tri dans l'ordre voulu
        ligne_num
      ) %>%
      select(-ligne_num, -ligne_alpha)  # ✅ Supprime les colonnes temporaires
    
    # ✅ Suppression des doublons par ligne (un médecin par ligne)
    medecins_proches_unique <- medecins_proches %>%
      distinct(id_medecin, ligne, .keep_all = TRUE)
    
    # ✅ Renommage pour affichage
    colnames(medecins_proches_unique)[colnames(medecins_proches_unique) == 'ligne'] <- 'Ligne'
    
    # ✅ Création du tableau croisé dynamique (pivot_wider)
    medecins_proches_unique %>%
      count(Ligne, libelle) %>%
      pivot_wider(names_from = libelle, values_from = n, values_fill = list(n = 0))
  })
  
  
  
  
  # ✅ Utiliser `tableau_lignes_reactif()` dans renderDataTable
  output$table_medecins_lignes <- DT::renderDataTable({
    DT::datatable(
      tableau_lignes_reactif(),
      options = list(
        dom = 't',
        scrollX = TRUE,  
        scrollY = "600px",  
        paging = FALSE,  
        ordering = TRUE
      )
    )
  })
  
  
  # ✅ Correction : Observer les sélections dans le tableau sans erreur
  observeEvent(input$table_medecins_lignes_rows_selected, {
    tableau_lignes <- tableau_lignes_reactif()  # 📌 Récupération des données
    
    lignes_selectionnees <- input$table_medecins_lignes_rows_selected
    
    if (!is.null(lignes_selectionnees) && nrow(tableau_lignes) > 0) {
      lignes_selected <- tableau_lignes[lignes_selectionnees, "Ligne", drop = TRUE]
      selected_lines(lignes_selected)
    } else {
      selected_lines(NULL)  # Reset si aucune ligne sélectionnée
    }
  })
  
  
  # ✅ Mini-carte des arrêts des lignes sélectionnées
  # ✅ Initialisation de la minimap avec zoom sur Lyon
  output$mini_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(minZoom = 12, maxZoom = 20))%>%
      setView(lng = 4.8357, lat = 45.7640, zoom = 12)
  })
  
  # ✅ Mise à jour de la minimap en fonction des lignes sélectionnées
  observe({
    lignes_actuelles <- selected_lines()
    
    leafletProxy("mini_map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls()  # Supprime l'ancienne légende pour éviter un doublon
    
    if (!is.null(lignes_actuelles) && length(lignes_actuelles) > 0) {
      arrets_selectionnes <- arret_transport %>%
        filter(ligne %in% lignes_actuelles)
      
      if (nrow(arrets_selectionnes) > 0) {
        
        # ✅ Création des tampons autour des arrêts
        tampons_arrets <- st_buffer(arrets_selectionnes, dist = input$dist_arret)
        
        # ✅ Fusion des tampons pour éviter une surcharge visuelle
        tampon_merged <- st_union(tampons_arrets)
        
        leafletProxy("mini_map") %>%
          addPolygons(
            data = tampon_merged, 
            color = "#0045ce",  
            weight = 2,  
            fillColor = "#90e8ff",  
            fillOpacity = 0.2  
          ) %>%
          addCircleMarkers(
            data = arrets_selectionnes, 
            color = "black",
            fillOpacity = 1,
            radius = 2,
            stroke = FALSE
          ) %>%
          addLegend(
            position = "topright",
            colors = "black",
            labels = "Arrêts TCL",
            opacity = 1
          )
      }
    } else {
      # Si aucune ligne sélectionnée, recentrer la carte et supprimer la légende
      leafletProxy("mini_map") %>%
        setView(lng = 4.8357, lat = 45.7640, zoom = 12) %>%
        clearControls()  # ✅ Supprime la légende si aucun arrêt n'est affiché
    }
  })
  
  
  
  
  ################################################################################################
  # Réactivité pour stocker l'heure sélectionnée
  selected_hour <- reactiveVal(NULL)
  
  # 📌 Définition des jours de la semaine en texte
  jours_noms <- c("le lundi", "le mardi", "le mercredi", "le jeudi", "le vendredi", "le samedi", "le dimanche")
  
  # Mise à jour du graphique en fonction du jour sélectionné
  output$graph_horaires <- renderPlot({
    req(input$jour)
    
    # 📌 Définition des jours filtrés
    jours_filtrage <- switch(input$jour, "all" = 1:7, "week" = 1:5, "weekend" = 6:7, as.numeric(input$jour))
    
    # 📊 ✅ Filtrer UNIQUEMENT les médecins généralistes
    data_graph <- effectifs_par_heure %>%
      filter(jour %in% jours_filtrage, specialite == "Généraliste")
    
    ggplot(data_graph, aes(x = heure, y = effectif, fill = specialite)) +
      
      # 🔶 Rectangle encadrant la colonne sélectionnée
      { if (!is.null(selected_hour())) 
        geom_rect(data = data_graph %>% filter(heure == selected_hour()),
                  aes(xmin = heure - 0.5, xmax = heure + 0.5),
                  ymin = -Inf, ymax = Inf,
                  fill = NA, color = "orange", linewidth = 1.5)
      } +
      
      # 📊 Graphique principal
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = couleurs_types) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 15, face = "bold"), 
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1)  
      ) +
      labs(title = paste("Généralistes ouverts", 
                         ifelse(input$jour == "week", "la semaine",
                                ifelse(input$jour == "weekend", "le week-end",
                                       ifelse(input$jour %in% 1:7, jours_noms[as.numeric(input$jour)], 
                                              "qu'importe le jour")))),
           x = "Heure de la journée",
           y = "Nombre de médecins",
           fill = "Spécialité")
  })
  
  # Ajout d'une interaction : clic sur une heure du graphique pour filtrer les médecins
  observeEvent(input$graph_horaires_click, {
    clicked_hour <- round(input$graph_horaires_click$x)
    if (!is.null(clicked_hour) && clicked_hour >= 0 && clicked_hour <= 23) {
      selected_hour(clicked_hour)
    }
  })
  
  # Bouton pour réinitialiser la sélection de l'heure
  observeEvent(input$reset_hour, {
    selected_hour(NULL)
  })
  
  # Mise à jour de la carte des médecins disponibles en fonction de l'heure sélectionnée
  output$map_horaires <- renderLeaflet({
    req(input$jour)  
    jour_selectionne <- input$jour
    heure_selectionnee <- selected_hour()
    
    # 🔹 Définition du filtre du jour sélectionné
    jours_filtrage <- switch(jour_selectionne, "all" = 1:7, "week" = 1:5, "weekend" = 6:7, as.numeric(jour_selectionne))  
    
    # 🔹 Filtrer uniquement les "Généralistes"
    medecins_disponibles <- med_annuaire24 %>%
      mutate(jour_list = strsplit(jour_consu, ";")) %>%
      unnest(jour_list) %>%
      mutate(jour_list = as.numeric(jour_list)) %>%
      filter(jour_list %in% jours_filtrage, libelle_pr == "Généraliste")
    
    # 🔹 Appliquer une transparence selon l'heure sélectionnée
    if (!is.null(heure_selectionnee)) {
      medecins_disponibles <- medecins_disponibles %>%
        mutate(
          actif_heure = as.numeric(sub(":.*", "", heures_deb)) <= heure_selectionnee &
            as.numeric(sub(":.*", "", heures_fin)) >= heure_selectionnee,
          opacity = ifelse(actif_heure, 0.8, 0.1),  
          stroke_active = ifelse(actif_heure, TRUE, FALSE),
          stroke_weight = ifelse(actif_heure, 1, 0)  
        )
    } else {
      medecins_disponibles <- medecins_disponibles %>%
        mutate(opacity = 0.8, stroke_active = TRUE, stroke_weight = 1)
    }
    
    # 🟢 **Ajout de la correspondance des couleurs**
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(minZoom = 12, maxZoom = 20))%>%
      addCircleMarkers(
        data = medecins_disponibles, 
        layerId = ~id, 
        color = 'black',  
        fillColor = ~ifelse(libelle_pr %in% names(couleurs_types), 
                            couleurs_types[libelle_pr], 
                            couleurs_types["Autre"]),  # 🔹 Applique la couleur de la symbologie définie
        fillOpacity = ~opacity,  
        radius = 5, 
        stroke = ~stroke_active,  
        weight = ~stroke_weight  
      )
  })
  
  
  # Bouton "Réinitialiser"
  observeEvent(input$reset_map, {
    selected_medecin(NULL)
    display_arrets(FALSE)
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      addPolylines(
        data = lignes,  
        color = ~couleur,  
        weight = 3, opacity = 0.8, smoothFactor = 1
      ) %>%
      addCircleMarkers(
        data = BPE_filtre(), 
        layerId = ~id, 
        color = "white",  
        fillColor = ~ifelse(libelle %in% names(couleurs_types), 
                            couleurs_types[libelle], 
                            couleurs_types["Autre"]),
        fillOpacity = 0.8, 
        radius = 5, 
        stroke = TRUE,  
        weight = 1  
      )
    
    leafletProxy("map_horaires") %>%
      clearMarkers() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(minZoom = 12, maxZoom = 20))
  })
}
