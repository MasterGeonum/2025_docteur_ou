#Librairies
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(stringr)
library(tidyr)  
library(bslib)
library(units)
library(DT)
library(htmltools)
library(shinyWidgets)

#Chargement de la donnée
annuaire24_agr <- st_read("donnees/annuaire24_agr.geojson")
bpe23 <- st_read("donnees/bpe23.geojson")
bpe23_agreg <- st_read("donnees/bpe_agreg.geojson")
arrondissement <- st_read("donnees/arrondissement.geojson")
qpv <- st_read("donnees/qpv.geojson")
bpe_iris <- st_read("donnees/BPE_millesime_iris.geojson")
arrondissement <- st_read("donnees/arrondissement.geojson")
qpv <- st_read("donnees/qpv.geojson")
discretisation <- read.csv("donnees/discretisation_bpe_iris.csv")
lyon <-st_read("donnees/contours_lyon.geojson")

carreauxinsee <- st_read(dsn="donnees/carreaux_insee.geojson", 
                         stringsAsFactors = FALSE, ) %>% 
  mutate(noCell=row_number())

distLong <- readRDS("donnees/distLong2.rds")
pression_med <- st_read("donnees/indice_pression.geojson")

med_annuaire24 <- st_read("donnees/med_annuaire24.geojson")  
arret_transport <- st_read("donnees/arret_transport.geojson")  
lignes <- st_read("donnees/lignes_TCL.geojson")
distance_df_file <- "donnees/distance_matrix_df.RData"
load(distance_df_file)

# Changement de projection
bpe23 <- st_transform(bpe23, crs = '+proj=longlat +datum=WGS84')
annuaire24_agr <-  st_transform(annuaire24_agr, crs = '+proj=longlat +datum=WGS84')
arrondissement <- st_transform(arrondissement, crs = '+proj=longlat +datum=WGS84')
qpv <- st_transform(qpv, crs = '+proj=longlat +datum=WGS84')
bpe23_agreg <- st_transform(bpe23_agreg, crs = '+proj=longlat +datum=WGS84')
bpe_iris <- st_transform(bpe_iris, crs = '+proj=longlat +datum=WGS84')
carreauxinsee <- st_transform(carreauxinsee, crs = '+proj=longlat +datum=WGS84')
pression_med<- st_transform(pression_med, crs = '+proj=longlat +datum=WGS84')
lyon<- st_transform(lyon, crs = '+proj=longlat +datum=WGS84')

if (st_crs(med_annuaire24)$epsg != 4326) {
  med_annuaire24 <- st_transform(med_annuaire24, 4326)
}
arret_transport <- st_set_crs(arret_transport, 4326)
if (st_crs(lignes)$epsg != 4326) {
  lignes <- st_transform(lignes, 4326)
}
if (!inherits(lignes, "sf")) {
  lignes <- st_as_sf(lignes)
}

# Creation id unique
bpe23$id <- as.character(seq_len(nrow(bpe23)))
med_annuaire24$id <- as.character(seq_len(nrow(med_annuaire24)))
arret_transport$id <- as.character(seq_len(nrow(arret_transport)))
lignes$id <- as.character(seq_len(nrow(lignes)))

# Définition de listes
choices_medecins <- c(
  "Généraliste" = "generaliste",
  "Chirurgien-dentiste" = "dentiste",
  "Gynécologue" = "gynecologue",
  "Ophtalmologiste" = "ophtalmo",
  "Oto-Rhino-Laryngologue (ORL)" = "orl",
  "Pédiatre" = "pediatre",
  "Psychiatre" = "psychiatre"
)

conv_medecins <- c(
  "Généraliste" = "D201",
  "Psychiatre" = "D207",
  "Ophtalmologiste" = "D208",
  "Oto-Rhino-Laryngologue (ORL)" = "D209",
  "Pédiatre" = "D210",
  "Gynécologue" = "D214",
  "Chirurgien-dentiste" = "D221"
)

#Modification des df

annuaire24_agr<- annuaire24_agr %>%
  mutate(LIBCOM = case_when(
    code_posta == "69001" ~ "Lyon 1er Arrondissement",
    code_posta == "69002" ~ "Lyon 2e Arrondissement",
    code_posta == "69003" ~ "Lyon 3e Arrondissement",
    code_posta == "69004" ~ "Lyon 4e Arrondissement",
    code_posta == "69005" ~ "Lyon 5e Arrondissement",
    code_posta == "69006" ~ "Lyon 6e Arrondissement",
    code_posta == "69007" ~ "Lyon 7e Arrondissement",
    code_posta == "69008" ~ "Lyon 8e Arrondissement",
    code_posta == "69009" ~ "Lyon 9e Arrondissement",
    TRUE ~ NA_character_  # Pour les codes postaux non reconnus
  )
)


annuaire24_agr <- annuaire24_agr %>%
  rename("libelle" = "libelle_pr")



bpe23_agreg$libelle <- gsub("Médecin généraliste", "Généraliste", bpe23_agreg$libelle)

annuaire24_agr$libelle <- gsub("Médecin généraliste", "Généraliste", annuaire24_agr$libelle)

med_annuaire24$libelle_pr <- gsub("Médecin généraliste", "Généraliste", med_annuaire24$libelle_pr)

bpe23$libelle <- gsub("Médecin généraliste", "Généraliste", bpe23$libelle)

distLong$libelle <- gsub("Médecin généraliste", "Généraliste", distLong$libelle)


# Correspondance personnalisée entre types et couleurs
couleurs_types <- list(
  # Types d'établissements de santé
  "Etablissement santé court séjour" = "#ee5249",  # Rouge
  "Etablissement santé moyen séjour" = "#ffa533",  # Orange
  "Etablissement santé long séjour" = "#ffd033",  # Orange clair
  "Etablissement psychiatrique" = "#1c1f23",  # noir
  "Maternité" = "#800080",  # Violet
  "Centre de santé" = "#f09aef",  # Rose
  "Structure psychiatrique en ambulatoire"= "#a9a9a9",  # Gris
  "Centre médecine préventive"  = "#67beec",  # Bleu
  "Laboratoire d'analyses et de biologie médicale"= "#0b831d",  #vert foncé
  "Pharmacie"  = "#6bc484",  # vert
  
  # Types de médecins (BPE)
  "Généraliste" = "#6bc484",  # vert
  "Psychiatre" = "#1c1f23",  # noir
  "Ophtalmologiste" = "#ffa533",  # Orange
  "Oto-Rhino-Laryngologue (ORL)" = "#67beec",  # Bleu
  "Pédiatre" = "#800080",  # Violet
  "Gynécologue" = "#f09aef",  # Rose
  "Chirurgien-dentiste" = "#ee5249",  # Rouge
  
  # Types de médecins (Annuaire)
  "Généraliste" = "#6bc484",  # vert
  "Psychiatre"  = "#1c1f23",  # noir
  "Ophtalmologiste"   = "#ffa533",  # Orange
  "Oto-Rhino-Laryngologue (ORL)" = "#67beec",  # Bleu
  "Pédiatre" = "#800080",  # Violet
  "Gynécologue" = "#f09aef",  # Rose
  "Chirurgien-dentiste"  = "#ee5249"  # Rouge
)

rgb_to_hex <- function(rgb_string) {
  rgb_values <- as.numeric(unlist(strsplit(trimws(rgb_string), "[ ;]+")))  
  if (length(rgb_values) == 3 && all(!is.na(rgb_values)) && all(rgb_values >= 0 & rgb_values <= 255)) {
    return(sprintf("#%02X%02X%02X", as.integer(rgb_values[1]), 
                   as.integer(rgb_values[2]), 
                   as.integer(rgb_values[3])))
  } else {
    return("#000000")  # Noir par défaut en cas d'erreur
  }
}
lignes$couleur <- vapply(lignes$couleur, rgb_to_hex, character(1))



# Calculs pour onglet 3

# Pré-calcul des effectifs horaires
medecins_horaires_df <- med_annuaire24 %>%
  filter(!is.na(heures_deb) & !is.na(heures_fin) & !is.na(jour_consu)) %>%
  mutate(
    heure_deb = as.numeric(sub(":.*", "", heures_deb)),  
    heure_fin = as.numeric(sub(":.*", "", heures_fin)),  
    jours = strsplit(jour_consu, ";")
  ) %>% 
  unnest(jours) %>%  # Explose les jours multiples en lignes distinctes
  mutate(jours = as.numeric(jours))

# Générer les effectifs horaires par jour et spécialité
effectifs_par_heure <- expand.grid(heure = 0:23, jour = 1:7, specialite = unique(medecins_horaires_df$libelle_pr)) %>%
  left_join(medecins_horaires_df, by = c("jour" = "jours", "specialite" = "libelle_pr")) %>%
  filter(heure >= heure_deb & heure <= heure_fin) %>%
  count(jour, heure, specialite, name = "effectif")

# Complétion des effectifs manquants par 0
effectifs_par_heure <- tidyr::complete(effectifs_par_heure, jour, heure, specialite, fill = list(effectif = 0))

# Définition de l'icône pour les arrêts de transport
arret_icon <- makeIcon(
  iconUrl = "https://cdn-icons-png.flaticon.com/512/684/684908.png",
  iconWidth = 15, iconHeight = 15
)

# Charger les sous-applications

source("../onglet1/ui.R", local = TRUE)
source("../onglet1/server.R", local = TRUE)

source("../onglet1_bis/ui.R", local = TRUE)
source("../onglet1_bis/server.R", local = TRUE)

source("../onglet2/ui.R", local = TRUE)
source("../onglet2/server.R", local = TRUE)

source("../onglet3/ui.R", local = TRUE)
source("../onglet3/server.R", local = TRUE)
