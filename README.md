# Logiciels Utilisés

## RStudio
- **Version de R :**  
  **4.3.2 (2023-10-31 ucrt)** — "Eyes Holes"  
  **Platform :** x86_64-w64-mingw32/x64 (64-bit)

## Modules R utilisés :
| Package        | Version  | Installation                          | Source / Archives |
|---------------|----------|---------------------------------------|--------------------|
| bslib         | 0.8.0    | `install.packages("bslib")`           | [CRAN](https://cran.r-project.org/package=bslib) |
| dplyr         | 1.1.4    | `install.packages("dplyr")`           | [CRAN](https://cran.r-project.org/package=dplyr) |
| DT            | 0.33     | `install.packages("DT")`              | [CRAN](https://cran.r-project.org/package=DT) |
| ggplot2       | 3.4.4    | `install.packages("ggplot2")`         | [CRAN](https://cran.r-project.org/package=ggplot2) |
| htmltools     | 0.5.8.1  | `install.packages("htmltools")`       | [CRAN](https://cran.r-project.org/package=htmltools) |
| leaflet       | 2.2.2    | `install.packages("leaflet")`         | [CRAN](https://cran.r-project.org/package=leaflet) |
| RColorBrewer  | 1.1-3    | `install.packages("RColorBrewer")`    | [CRAN](https://cran.r-project.org/package=RColorBrewer) |
| sf            | 1.0-19   | `install.packages("sf")`              | [CRAN](https://cran.r-project.org/package=sf) |
| shiny         | 1.10.0   | `install.packages("shiny")`           | [CRAN](https://cran.r-project.org/package=shiny) |
| shinywidgets  | 0.9.0    | `install.packages("shinywidgets")`    | [CRAN](https://cran.r-project.org/package=shinywidgets) |
| stringr       | 1.5.1    | `install.packages("stringr")`         | [CRAN](https://cran.r-project.org/package=stringr) |
| tidyr         | 1.3.1    | `install.packages("tidyr")`           | [CRAN](https://cran.r-project.org/package=tidyr) |
| units         | 0.8-5    | `install.packages("units")`           | [CRAN](https://cran.r-project.org/package=units) |

## Structure du repository
- 4 dossiers "onglets" : contiennent des "sous-applications" RShiny indépendantes
- 1 dossier main : application principale avec import des données, import des librairies, import des sous-applications...
