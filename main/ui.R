ui <- fluidPage(
  

  # AJOUT CSS
  
  tags$head(
    tags$style(HTML("
    
/* Style pour le bandeau du titre */
        .title-banner {
        background-color: #d4edda; /* Vert clair */
        padding: 5px; /* Espacement intérieur */
        display: flex;
        align-items: center; /* Centrer verticalement */
        justify-content: space-between; /* Espacer le titre et le logo */
        border-radius: 10px; /* Bords arrondis */
        margin-top: 5px; /* Espace sous le bandeau */
        margin-bottom: 5px; /* Espace sous le bandeau */
      }
      
/* Style pour le titre */
      .title-banner h1 {
        font-size: 1.8em; /* Taille du titre */
        margin: 0; /* Supprimer la marge par défaut */
        color: #155724; /* Couleur du texte */
        font-family: Cambria, serif; /* Police Cambria */
        font-weight: bold; /* Rend le texte plus gras */
        text-align: center; /* Centre le texte */
        font-size: 30px; /* Augmente la taille de la police */
      }
      
/* Style pour la partie en italique */
      .title-banner h1 em {
          font-style: italic; /* Texte en italique */
      }

/* Style pour le pictogramme */
      .title-banner img {
        height: 60px; /* Taille du pictogramme */
        width: auto; /* Conserver les proportions */
      }
/*--------------------------------------------------------------------------------*/

/* Cibler spécifiquement les onglets dans le tabsetPanel */
    #onglet.nav.nav-tabs {
      display: flex;
      justify-content: space-evenly; /* Répartir uniformément les onglets */
      border: none;
      gap: 10px; /* Espace entre les boutons */
    }

/* Style des éléments de la barre d'onglets */
    #onglet.nav.nav-tabs > li {
      flex: 1; /* Les boutons prennent tout l'espace disponible */
      text-align: center; /* Centrer le texte */
    }

/* Style des liens dans les onglets */
    #onglet.nav.nav-tabs > li > a {
      background-color: #c5c8c9; /* Couleur de fond gris */
      color: white; /* Texte en blanc */
      border-radius: 12px; /* Bords arrondis */
      padding: 10px 0; /* Ajuster le padding */
      font: Tahoma;
      transition: background-color 0.3s ease; /* Effet de transition */
      border: none; /* Supprimer la bordure par défaut */
      box-sizing: border-box; /* Inclure le padding dans la taille */
      width: 100%; /* Largeur fixe */
      height: auto; /* Hauteur automatique */
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: 1.2vw; /* Taille de police relative à la largeur de la fenêtre */
      white-space: nowrap; /* Empêcher le texte de passer à la ligne */
      overflow: hidden; /* Cacher le texte qui dépasse */
      text-overflow: ellipsis; /* Ajouter des points de suspension si le texte dépasse */
    }

/* Style au survol */
    #onglet.nav.nav-tabs > li > a:hover {
      background-color: #23697c; /* Couleur plus foncée au survol */
    }

/* Style de l'onglet actif */
    #onglet.nav.nav-tabs > li.active > a {
      background-color: #23697c; /* Couleur de fond pour l'onglet actif */
      border: none;
    }

/* Media queries pour les écrans plus petits */
    @media (max-width: 768px) {
      #onglet.nav.nav-tabs > li > a {
        font-size: 2vw; /* Taille de police plus petite pour les petits écrans */
        padding: 8px 0; /* Padding réduit */
      }
    }

    @media (max-width: 480px) {
      #onglet.nav.nav-tabs > li > a {
        font-size: 3vw; /* Taille de police encore plus petite pour les très petits écrans */
        padding: 6px 0; /* Padding encore plus réduit */
      }
    }

/*--------------------------------------------------------------------------------*/

/* Style pour le tabsetPanel dans onglet1_ui (tabs_ong1) */
    #tabs_ong1.nav.nav-tabs {
      display: flex;
      justify-content: space-evenly; /* Répartir uniformément les onglets */
      border: none;
      gap: 10px; /* Espace entre les boutons */
      margin-top: 10px; /* Ajouter un espace en haut */
      margin-bottom: 10px;

    }

    #tabs_ong1.nav.nav-tabs > li {
      flex: 1; /* Les boutons prennent tout l'espace disponible */
      text-align: center; /* Centrer le texte */
    }

    #tabs_ong1.nav.nav-tabs > li > a {
      background-color: #caf0f8; /* Couleur de fond bleu clair */
      color: black; /* Texte en noir */
      border-radius: 12px; /* Bords arrondis */
      padding: 5px 0; /* Ajuster le padding */
      font-weight: bold;
      transition: background-color 0.3s ease; /* Effet de transition */
      border: none; /* Supprimer la bordure par défaut */
      box-sizing: border-box; /* Inclure le padding dans la taille */
      width: 100%; /* Largeur fixe */
      height: auto; /* Hauteur automatique */
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: 1.2vw; /* Taille de police relative à la largeur de la fenêtre */
      white-space: nowrap; /* Empêcher le texte de passer à la ligne */
      overflow: hidden; /* Cacher le texte qui dépasse */
      text-overflow: ellipsis; /* Ajouter des points de suspension si le texte dépasse */
    }

    #tabs_ong1.nav.nav-tabs > li > a:hover {
      background-color: #8fe5f7; /* Couleur plus foncée au survol */
    }

    #tabs_ong1.nav.nav-tabs > li.active > a {
      background-color: #8fe5f7; /* Couleur de fond pour l'onglet actif */
      border: none;
    }

/*--------------------------------------------------------------------------------*/

/* Style pour le tabsetPanel dans onglet1_bis_ui (tabs_ong1bis) */
    #tabs_ong1bis.nav.nav-tabs {
      display: flex;
      justify-content: space-evenly; /* Répartir uniformément les onglets */
      border: none;
      gap: 10px; /* Espace entre les boutons */
      margin-top: 10px; /* Ajouter un espace en haut */
      margin-bottom: 10px;
    }

    #tabs_ong1bis.nav.nav-tabs > li {
      flex: 1; /* Les boutons prennent tout l'espace disponible */
      text-align: center; /* Centrer le texte */
    }

    #tabs_ong1bis.nav.nav-tabs > li > a {
      background-color: #caf0f8; /* Couleur de fond bleu clair */
      color: black; /* Texte en noir */
      border-radius: 12px; /* Bords arrondis */
      padding: 5px 0; /* Ajuster le padding */
      font-weight: bold;
      transition: background-color 0.3s ease; /* Effet de transition */
      border: none; /* Supprimer la bordure par défaut */
      box-sizing: border-box; /* Inclure le padding dans la taille */
      width: 100%; /* Largeur fixe */
      height: auto; /* Hauteur automatique */
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: 1.2vw; /* Taille de police relative à la largeur de la fenêtre */
      white-space: nowrap; /* Empêcher le texte de passer à la ligne */
      overflow: hidden; /* Cacher le texte qui dépasse */
      text-overflow: ellipsis; /* Ajouter des points de suspension si le texte dépasse */
    }

    #tabs_ong1bis.nav.nav-tabs > li > a:hover {
      background-color: #8fe5f7; /* Couleur plus foncée au survol */
    }

    #tabs_ong1bis.nav.nav-tabs > li.active > a {
      background-color: #8fe5f7; /* Couleur de fond pour l'onglet actif */
      border: none;
    }

    /* Media queries pour les écrans plus petits */
    @media (max-width: 768px) {
      #tabs_ong1.nav.nav-tabs > li > a,
      #tabs_ong1bis.nav.nav-tabs > li > a {
        font-size: 2vw; /* Taille de police plus petite pour les petits écrans */
        padding: 8px 0; /* Padding réduit */
      }
    }

    @media (max-width: 480px) {
      #tabs_ong1.nav.nav-tabs > li > a,
      #tabs_ong1bis.nav.nav-tabs > li > a {
        font-size: 3vw; /* Taille de police encore plus petite pour les très petits écrans */
        padding: 6px 0; /* Padding encore plus réduit */
      }
    }

/*--------------------------------------------------------------------------------*/

/* Style pour le tabsetPanel dans onglet2 (tabs_ong2) */
    #tabs_ong2.nav.nav-tabs {
      display: flex;
      justify-content: space-evenly; /* Répartir uniformément les onglets */
      border: none;
      gap: 10px; /* Espace entre les boutons */
      margin-top: 10px; /* Ajouter un espace en haut */
      margin-bottom: 10px;
    }

    #tabs_ong2.nav.nav-tabs > li {
      flex: 1; /* Les boutons prennent tout l'espace disponible */
      text-align: center; /* Centrer le texte */
    }

    #tabs_ong2.nav.nav-tabs > li > a {
       background-color: #caf0f8; /* Couleur de fond bleu clair */
      color: black; /* Texte en noir */
      border-radius: 12px; /* Bords arrondis */
      padding: 5px 0; /* Ajuster le padding */
      font-weight: bold;
      transition: background-color 0.3s ease; /* Effet de transition */
      border: none; /* Supprimer la bordure par défaut */
      box-sizing: border-box; /* Inclure le padding dans la taille */
      width: 100%; /* Largeur fixe */
      height: auto; /* Hauteur automatique */
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: 1.2vw; /* Taille de police relative à la largeur de la fenêtre */
      white-space: nowrap; /* Empêcher le texte de passer à la ligne */
      overflow: hidden; /* Cacher le texte qui dépasse */
      text-overflow: ellipsis; /* Ajouter des points de suspension si le texte dépasse */
    }

    #tabs_ong2.nav.nav-tabs > li > a:hover {
      background-color: #8fe5f7; /* Couleur plus foncée au survol */
    }

    #tabs_ong2.nav.nav-tabs > li.active > a {
      background-color: #8fe5f7; /* Couleur de fond pour l'onglet actif */
      border: none;
    }

    /* Media queries pour les écrans plus petits */
    @media (max-width: 768px) {
      #tabs_ong2.nav.nav-tabs > li > a,
      #tabs_ong2.nav.nav-tabs > li > a {
        font-size: 2vw; /* Taille de police plus petite pour les petits écrans */
        padding: 8px 0; /* Padding réduit */
      }
    }

    @media (max-width: 480px) {
      #tabs_ong2.nav.nav-tabs > li > a,
      #tabs_ong2.nav.nav-tabs > li > a {
        font-size: 3vw; /* Taille de police encore plus petite pour les très petits écrans */
        padding: 6px 0; /* Padding encore plus réduit */
      }
    }
  
  /*--------------------------------------------------------------------------------*/

/* Style pour le tabsetPanel dans onglet3 (tabs_ong3) */
    #tabs_ong3.nav.nav-tabs {
      display: flex;
      justify-content: space-evenly; /* Répartir uniformément les onglets */
      border: none;
      gap: 10px; /* Espace entre les boutons */
      margin-top: 10px; /* Ajouter un espace en haut */
      margin-bottom: 10px;
    }

    #tabs_ong3.nav.nav-tabs > li {
      flex: 1; /* Les boutons prennent tout l'espace disponible */
      text-align: center; /* Centrer le texte */
    }

    #tabs_ong3.nav.nav-tabs > li > a {
       background-color: #caf0f8; /* Couleur de fond bleu clair */
      color: black; /* Texte en noir */
      border-radius: 12px; /* Bords arrondis */
      padding: 5px 0; /* Ajuster le padding */
      font-weight: bold;
      transition: background-color 0.3s ease; /* Effet de transition */
      border: none; /* Supprimer la bordure par défaut */
      box-sizing: border-box; /* Inclure le padding dans la taille */
      width: 100%; /* Largeur fixe */
      height: auto; /* Hauteur automatique */
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: 1.2vw; /* Taille de police relative à la largeur de la fenêtre */
      white-space: nowrap; /* Empêcher le texte de passer à la ligne */
      overflow: hidden; /* Cacher le texte qui dépasse */
      text-overflow: ellipsis; /* Ajouter des points de suspension si le texte dépasse */
    }

    #tabs_ong3.nav.nav-tabs > li > a:hover {
      background-color: #8fe5f7; /* Couleur plus foncée au survol */
    }

    #tabs_ong3.nav.nav-tabs > li.active > a {
      background-color: #8fe5f7; /* Couleur de fond pour l'onglet actif */
      border: none;
    }

    /* Media queries pour les écrans plus petits */
    @media (max-width: 768px) {
      #tabs_ong3.nav.nav-tabs > li > a,
      #tabs_ong3.nav.nav-tabs > li > a {
        font-size: 2vw; /* Taille de police plus petite pour les petits écrans */
        padding: 8px 0; /* Padding réduit */
      }
    }

    @media (max-width: 480px) {
      #tabs_ong3.nav.nav-tabs > li > a,
      #tabs_ong3.nav.nav-tabs > li > a {
        font-size: 3vw; /* Taille de police encore plus petite pour les très petits écrans */
        padding: 6px 0; /* Padding encore plus réduit */
      }
    }

  /*--------------------------------------------------------------------------------*/

/* Gestion de la page d'accueil */
    
/* Conteneur pour le titre et le logo */
      .title-logo-container {
        display: flex;
        align-items: center; /* Aligne verticalement l'image et le texte */
        gap: 20px; /* Espace entre l'image et le texte */
        margin-bottom: 0px; /* Espace entre chaque bloc */
      }
      
/* Ajout de marges autour du tabPanel Introduction */
    .introduction-padding {
      padding-left: 2%;  /* Espace à gauche */
      padding-right: 2%; /* Espace à droite */
      padding-top: 0.5%;   /* Espace en haut */
      padding-bottom: 0.5%;/* Espace en bas */
    }


/* Style du titre h1 */
    h1 {
        color: #286c7c; /* Met le texte en bleu */
        font-family: Cambria, serif; /* Définit la police à Cambria */
        font-weight: bold; /* Rend le texte plus gras */
        text-align: center; /* Centre le texte */
        font-size: 40px; /* Augmente la taille de la police */
    }
      
      /* Style spécifique en italique */
      h1 span {
        font-style: italic; /* Met le texte en italique */
      }
      
/* Style du titre h2 */
    h2 {
        color: #286c7c; /* Met le texte en bleu */
        font-family: Cambria, serif; /* Définit la police à Cambria */
        font-size: 20; /* Augmente la taille de la police */
        margin-top: 5px;
    }

/* Style du titre h3 */
    h3 {
        color: #286c7c; /* Met le texte en bleu */
        font-family: Cambria, serif; /* Définit la police à Cambria */
        font-weight: bold; /* Rend le texte plus gras */
        font-size: 15; /* Augmente la taille de la police */
    }
    
    
/* Style des paragraphes p */
    p {
        color: black; /* Met le texte en noir */
        font-family: Cambria, serif; /* Définit la police à Cambria */
        font-size: 17px; /* Augmente la taille de la police */
    }
    
/* Appliquer le même style aux éléments <li> */
    li {
        color: black; /* Met le texte en noir */
        font-family: Cambria, serif; /* Définit la police à Cambria */
        font-size: 17px; /* Taille de la police */
    }


/* Réduit l'espace avant le deuxième h2 */
      h2 + h2 {
        margin-top: 10px;  /* Ajustez cette valeur pour réduire l'espace avant ce titre */
      }
    
       .image-text-container {
        display: flex;
        align-items: center; /* Aligne verticalement l'image et le texte */
        gap: 20px; /* Espace entre l'image et le texte */
        margin-bottom: 5px; /* Espace entre chaque bloc */
        margin-top: 1px;
      }
      .image-left {
        flex-shrink: 0; /* Empêche l'image de rétrécir */
      }
      .text-right {
        flex-grow: 1; /* Permet au texte de prendre l'espace restant */
      }
      .image-right {
        flex-shrink: 0; /* Empêche l'image de rétrécir */
      }
      .text-left {
        flex-grow: 1; /* Permet au texte de prendre l'espace restant */
      }
      
/* Conteneur pour organiser les onglets en 2 colonnes */
      .column-container {
        display: flex;
        justify-content: space-between; /* Espace entre les deux colonnes */
        flex-wrap: wrap; /* Permet aux éléments de se replier si nécessaire */
      }

      .column-left,
      .column-right {
        flex: 1; /* Les colonnes prennent un espace égal */
        margin-right: 20px; /* Espace entre les colonnes */
        margin-left: 20px;
      }

      .column-left {
        /* S'assure que les onglets 1 et 3 sont à gauche */
        display: flex;
        flex-direction: column;
      }
      
      .column-right {
        /* S'assure que les onglets 2 et 4 sont à droite */
        display: flex;
        flex-direction: column;
      }

  "))
  ),
  
  
  # Bandeau du titre
  div(class = "title-banner",
      h1(em("Docteur où ?"), ": Visualiser l'accès aux soins à Lyon"),
      img(src = "logo_vert.png")
  ),
  
  # Onglets
  tabsetPanel(
    id = "onglet",  
    tabPanel("Introduction", value = "intro", class = "introduction-padding",
             div(class = "title-logo-container",
                 img(src = "logo.png", height = 170, width = 170, class = "image-left"), # Logo à gauche
                 h1("Bienvenue sur l'application ", span("Docteur où ?", class = "italic"), " : Un outil d'aide à l'implantation des médecins à Lyon") # Titre avec "Docteur où ?" en italique
             ),
             div(class = "image-text-container",
                 div(class = "text-left",
                     p("Cette application a été développée pour le service de santé de la Ville de Lyon et propose un tour d’horizon de l’offre de soins du territoire. Conçue comme un outil de planification, elle facilite l'orientation des professionnels de santé dans leur projet d'installation, tout en permettant d'anticiper les besoins futurs en personnels et cabinets médicaux et les problématiques d'accessibilité aux soins pour les habitants."),
                     p("Le terrain d’étude est la ville de Lyon. Pour limiter les effets de frontières, les territoires limitrophes sont intégrés dans un rayon d’un kilomètre autour de la Ville."), 
                     p("Les bases de données utilisées sont principalement la Base Permanente des Equipements (BPE) et l'Annuaire Santé. Elles fournissent les localisations des médecins et/ou établissements de santé. La BPE est plus exhaustive, mais l'Annuaire Santé fournit des informations sur les jours et horaires de disponibilité des médecins.")
                 ),
                 img(src = "lyon.png", height = 50, width = 150, class = "image-right") # Image à droite
             ),   
             
             h2("Fonctionnalités de l'application"),
             div(class = "column-container", 
                 div(class = "column-left", 
                     div(class = "image-text-container",
                         img(src = "localisateur.png", height = 80, width = 80, class = "image-left"), # Image à gauche
                         div(class = "text-left",
                             h3("Onglet 1 : Localisation des établissements et médecins"),
                             p("Ce premier onglet est un état des lieux de l’offre de santé à Lyon et présente la localisation des principaux établissements de santé ainsi que des principales spécialités médicales présentes sur le territoire.")
                         )
                     ),
                     
                     div(class = "image-text-container",
                         img(src = "marcheur.png", height = 80, width = 80, class = "image-left"), # Image à gauche
                         div(class = "text-left",
                             h3("Onglet 3 : Accessibilité locale"),
                             p("Cet onglet permet de visualiser l’accessibilité locale à l’offre de soins lyonnaise selon le type d’offre retenu. Le calcul d’un indice d’accessibilité potentielle localisée permet une prise en compte la pression de population et offre un aperçu des tensions relatives à l’offre de soin au sein de la ville.")
                         )
                     )
                 ),
                 
                 div(class = "column-right", 
                     div(class = "image-text-container",
                         img(src = "augmenter.png", height = 80, width = 80, class = "image-left"), # Image à gauche
                         div(class = "text-left",
                             h3("Onglet 2 : Évolution de la densité médicale"),
                             p("Cet onglet offre une représentation et une analyse de l'évolution de la densité médicale entre 2016 et 2023.")
                         )
                     ),
                     
                     div(class = "image-text-container",
                         img(src = "tcl.jpg", height = 80, width = 100, class = "image-left"), # Image à gauche
                         div(class = "text-left",
                             h3("Onglet 4 : Accès aux soins - transports et temps"),
                             p("Ce dernier onglet présente l’accessibilité en transports en commun aux médecins et permet de visualiser l’offre de soin en fonction des jours de la semaine et des horaires.")
                         )
                     )
                 )
             ),
             
             div(class = "image-text-container",
                 div(class = "text-left",
                     h2("Qui sommes-nous ?"),
                     p("Le développement de cette application s'inscrit dans le cadre d'un partenariat entre deux Masters et a été réalisé par :"),
                     tags$ul(
                       tags$li("Candice Marchand, Coline Puzin et Oscar Uhry, du Master GEONUM (Géomatique/Géographiques numériques, Université Lumière Lyon II)"),
                       tags$li("Flavie Chalaud et Salomé Mesure, du Master SENTINELLES (Santé, Environnement, Informations spatio-temporelles, Université Jean Moulin Lyon III)")
                     )
                 ),
                 img(src = "geonum.png", height = 60, width = 60, class = "image-right"),  # L'image à droite
                 img(src = "sentinelle.png", height = 50, width = 140, class = "image-right")  # L'image à droite
             ),
             h1("BONNE EXPLORATION !")
    ),
    tabPanel("Localisation des établissements et médecins", onglet1_ui),
    tabPanel("Évolution de la densité médicale", onglet1_bis_ui),
    tabPanel("Accessibilité locale", onglet2_ui), 
    tabPanel("Accès aux soins : transports et temps", ui_3) 
  ),
  

)