---
title: "Étude des Performances Énergétiques des Logements - Département 69"
author: "Leonard"
date: "`r Sys.Date()`"
output:
  html_document:
    toc: true
    toc_depth: 2
    theme: lumen
    highlight: tango
    number_sections: true
    df_print: paged
    css: "www/R_Markdown.css"
---

# Introduction

<div class="introduction">
La transition énergétique est un enjeu crucial pour la lutte contre le changement climatique. Les bâtiments résidentiels jouent un rôle clé dans cette transition, et leur performance énergétique est mesurée à travers le Diagnostic de Performance Énergétique (DPE). Le DPE évalue l'efficacité énergétique des logements sur une échelle de A (très performant) à G (peu performant).

Cette étude se concentre sur l'analyse des logements neufs et anciens du département 69 (Rhône). L'objectif est d'examiner les différences de performances énergétiques entre ces deux types de logements, ainsi que d'analyser les corrélations entre les coûts énergétiques et d'autres variables telles que la surface habitable. En identifiant les zones d'amélioration potentielles, cette étude vise à proposer des recommandations pertinentes pour favoriser des pratiques de construction plus durables et énergétiquement efficaces.
</div>

## Objectifs de l'étude

<div class="objectives">
Les principaux objectifs de cette analyse sont :
  
1. **Identifier les tendances énergétiques des logements** en fonction de l'étiquette DPE.
2. **Explorer les corrélations** entre les coûts énergétiques, la surface habitable, et d'autres variables clés.
3. **Proposer des recommandations** en matière de politiques publiques ou de réhabilitation énergétique.
</div>

---

# Données et Méthodologie

<div class="methodology">
Les données utilisées dans cette étude proviennent de l'API ADEME (schéma v3 en <code>snake_case</code>) via deux jeux : <strong>dpe03existant</strong> et <strong>dpe02neuf</strong>. Nous utilisons un filtrage sur le département 69 et réalisons une fusion (ancien + neuf) pour analyser l’ensemble du parc. Les coordonnées sont converties de Lambert-93 vers WGS84 pour la cartographie.
</div>

## Importation et Préparation des Données

<div class="data-preparation">
Deux fonctions paginent l’API (10 000 lignes/page) et renvoient les variables clefs normalisées : <code>numero_dpe</code>, <code>etiquette_dpe</code>, <code>etiquette_ges</code>, <code>code_postal_ban</code>, <code>surface_habitable_logement</code>, <code>cout_chauffage</code>, <code>coordonnee_cartographique_x_ban</code>, <code>coordonnee_cartographique_y_ban</code>, etc.
</div>

```{r setup, echo=FALSE, message=FALSE, warning=FALSE}
# Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(corrplot)
library(httr)
library(jsonlite)
library(sf)
library(knitr)
library(stringr)
library(lubridate)

# --- Constantes API (v3 snake_case) ---
EXISTANTS_URL <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe03existant/lines"
NEUFS_URL     <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe02neuf/lines"

BASE_FIELDS <- c(
  "numero_dpe","etiquette_dpe","etiquette_ges",
  "date_reception_dpe",
  "code_postal_ban","nom_commune_ban","code_departement_ban",
  "surface_habitable_logement","cout_chauffage",
  "coordonnee_cartographique_x_ban","coordonnee_cartographique_y_ban",
  "type_batiment","typologie_logement","periode_construction",
  "modele_dpe","methode_application_dpe"
)

fetch_dataset <- function(base_url, dept = "69", page_size = 10000, max_pages = 200) {
  out <- list(); page <- 1
  repeat {
    q <- list(
      page = page,
      size = page_size,
      select = paste(BASE_FIELDS, collapse = ","),
      q = paste0(dept, "*"),
      q_fields = "code_postal_ban"
    )
    resp <- GET(modify_url(base_url, query = q))
    if (status_code(resp) != 200) {
      message("HTTP ", status_code(resp), " page ", page, " -> arrêt.")
      break
    }
    dat <- fromJSON(content(resp, "text", encoding = "UTF-8"))$result
    if (is.null(dat) || nrow(dat) == 0) break
    out[[length(out)+1]] <- dat
    if (nrow(dat) < page_size || page >= max_pages) break
    page <- page + 1
  }
  if (length(out) == 0) return(tibble())
  bind_rows(out)
}

# Lambert-93 -> WGS84 via sf (évite rgdal/sp)
to_wgs84 <- function(x_l93, y_l93) {
  ok <- !is.na(x_l93) & !is.na(y_l93)
  if (!any(ok)) return(tibble(lon = rep(NA_real_, length(x_l93)), lat = rep(NA_real_, length(y_l93))))
  pts <- st_as_sf(data.frame(x = as.numeric(x_l93[ok]), y = as.numeric(y_l93[ok])), coords = c("x","y"), crs = 2154)
  pts_wgs <- suppressWarnings(st_transform(pts, 4326))
  lonlat <- st_coordinates(pts_wgs)
  lon <- lat <- rep(NA_real_, length(x_l93))
  lon[ok] <- lonlat[,1]; lat[ok] <- lonlat[,2]
  tibble(lon = lon, lat = lat)
}

# --- Téléchargement ---
existants_data <- fetch_dataset(EXISTANTS_URL, "69")
neufs_data     <- fetch_dataset(NEUFS_URL, "69")

existants_data <- existants_data %>% mutate(logement = "ancien")
neufs_data     <- neufs_data %>% mutate(logement = "neuf")

colonnes_communes <- intersect(names(existants_data), names(neufs_data))
merged_data <- bind_rows(existants_data[, colonnes_communes], neufs_data[, colonnes_communes]) %>%
  # types & nettoyage
  mutate(
    date_reception_dpe = suppressWarnings(ymd(date_reception_dpe)),
    surface_habitable_logement = suppressWarnings(as.numeric(surface_habitable_logement)),
    cout_chauffage = suppressWarnings(as.numeric(cout_chauffage))
  ) %>%
  distinct(numero_dpe, .keep_all = TRUE)

# Coordonnées WGS84
coords <- to_wgs84(merged_data$coordonnee_cartographique_x_ban,
                   merged_data$coordonnee_cartographique_y_ban)
merged_data <- bind_cols(merged_data, coords)

# Aperçu
head(merged_data)
```

# Synthèse des Indicateurs Clés de Performance (KPI)

<div class="kpi-section">
Les indicateurs clés de performance (KPI) suivants fournissent une vue d'ensemble des logements dans le département 69. Ils incluent le nombre total d’observations, la part ancien/neuf, et l’étiquette DPE la plus fréquente.
</div>

```{r kpi, echo=FALSE}
context_data <- function(df) {
  total_logements <- nrow(df)
  pct_anciens <- ifelse(total_logements > 0, round(mean(df$logement == "ancien") * 100, 1), 0)
  pct_neufs   <- ifelse(total_logements > 0, round(mean(df$logement == "neuf")   * 100, 1), 0)
  categorie_majoritaire <- ifelse(total_logements > 0,
                                  df %>% count(etiquette_dpe, sort = TRUE) %>% slice(1) %>% pull(etiquette_dpe),
                                  "Aucune")
  list(total_logements = total_logements,
       pct_anciens = pct_anciens,
       pct_neufs = pct_neufs,
       categorie_majoritaire = categorie_majoritaire)
}

kpi <- context_data(merged_data)

kable(
  data.frame(
    Indicateurs = c("Total de logements",
                    "Pourcentage de logements anciens",
                    "Pourcentage de logements neufs",
                    "Étiquette DPE majoritaire"),
    Valeurs = c(kpi$total_logements,
                paste0(kpi$pct_anciens, " %"),
                paste0(kpi$pct_neufs, " %"),
                kpi$categorie_majoritaire)
  ),
  caption = "Tableau récapitulatif des indicateurs clés de performance des logements (Département 69).",
  align = "c"
)
```

# Analyse des Performances Énergétiques

<div class="energy-analysis">
Nous analysons les performances selon l’étiquette DPE, le type de logement (ancien/neuf) et la surface habitable.
</div>

## 4.1 Répartition des coûts énergétiques selon l'étiquette DPE

<div class="energy-cost-analysis">
Le visuel ci-dessous compare la distribution des <em>coûts de chauffage</em> par <strong>étiquette DPE</strong> et par <strong>type de logement</strong>.
</div>

![Coût du chauffage selon l'étiquette DPE](https://www.hebergeur-image.fr/uploads/20241014/4223c89241621638843dc335bc67816b5f1af683.png)

**Analyse du Graphique :**  
<div class="graph-analysis">
Les coûts sont nettement plus élevés pour les classes F et G. Les logements neufs se concentrent sur des coûts plus faibles, traduisant de meilleures performances, tandis que les anciens affichent davantage d’outliers coûteux. Cela plaide pour des programmes de rénovation énergétique ciblés sur le parc ancien le plus énergivore.
</div>

## 4.2 Corrélation entre la surface habitable et le coût du chauffage

<div class="correlation-analysis">
Nous évaluons la relation entre la <em>surface_habitable_logement</em> et le <em>cout_chauffage</em>.
</div>

```{r correlation_plot, echo=FALSE, warning=FALSE, message=FALSE}
corr_df <- merged_data %>%
  select(surface_habitable_logement, cout_chauffage) %>%
  drop_na() %>%
  mutate(across(everything(), as.numeric))

if (nrow(corr_df) >= 5) {
  cor_matrix <- cor(corr_df, use = "complete.obs")
  corrplot(cor_matrix, method = "circle", tl.col = "black", tl.srt = 45,
           title = "Corrélogramme : surface vs coût de chauffage", mar = c(0,0,2,0))
} else {
  cat("Données insuffisantes pour un corrélogramme fiable.")
}
```

**Analyse du Corrélogramme :**  
<div class="corr-analysis">
La corrélation est positive : plus la surface est grande, plus le coût annuel grimpe, toutes choses égales par ailleurs. L’écart à une corrélation parfaite souligne l’impact d’autres facteurs (isolation, systèmes, exposition…).
</div>

# 5 Visualisation Géographique des Logements

<div class="geo-visual">
La carte interactive localise les logements (ancien vs neuf) avec un résumé de leurs caractéristiques.
</div>

```{r leaflet_map, echo=FALSE, warning=FALSE, message=FALSE}
map_df <- merged_data %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  slice_sample(n = min(5000, n()))

if (nrow(map_df) > 0) {
  leaflet(map_df) %>%
    addTiles() %>%
    setView(lng = 4.85, lat = 45.75, zoom = 10) %>%
    addCircleMarkers(
      lng = ~lon, lat = ~lat,
      color = ~ifelse(logement == "ancien", "#E69F00", "#56B4E9"),
      popup = ~paste0(
        "<strong>", nom_commune_ban, "</strong><br>",
        "DPE: ", etiquette_dpe, " — GES: ", etiquette_ges, "<br>",
        "Surface: ", surface_habitable_logement, " m²<br>",
        "Coût chauffage: ", cout_chauffage, " € / an"
      ),
      radius = 5, fillOpacity = 0.7, stroke = FALSE,
      clusterOptions = markerClusterOptions()
    )
} else {
  cat("Pas de points géolocalisés exploitables.")
}
```

# 6 Étude Statistique : Deux Problématiques Spécifiques

## 6.1 Quels types de logements nécessitent le plus de rénovation énergétique ?

<div class="problem-analysis">
On observe la part des classes <strong>F</strong> et <strong>G</strong> par type de logement.
</div>

```{r renovation_analysis, echo=FALSE}
logements_fg <- merged_data %>%
  filter(etiquette_dpe %in% c("F","G")) %>%
  count(logement, name = "nb")

kable(logements_fg,
      caption = "Nombre de logements classés F et G (candidats prioritaires à la rénovation), par type de logement.",
      align = "c")
```

**Interprétation :**  
<div class="interpretation">
Le parc <em>ancien</em> concentre la majorité des logements F-G : il s’agit des cibles prioritaires pour réduire les émissions et la facture énergétique des ménages.
</div>

## 6.2 Les logements neufs sont-ils systématiquement plus économiques ?

<div class="problem-analysis">
Comparaison des distributions du <em>cout_chauffage</em> pour <em>ancien</em> vs <em>neuf</em>.
</div>

```{r cost_distribution, echo=FALSE, warning=FALSE, message=FALSE}
ggplot(merged_data, aes(x = logement, y = cout_chauffage, fill = logement)) +
  geom_violin(trim = FALSE) +
  coord_cartesian(ylim = c(0, 5000)) +
  labs(title = "Distribution des coûts de chauffage par type de logement",
       x = "Type de logement", y = "Coût du chauffage (€ / an)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "top") +
  scale_fill_manual(values = c("ancien" = "#E69F00", "neuf" = "#56B4E9"))
```

**Analyse du Graphique :**  
<div class="graph-analysis">
Les coûts des logements <em>neufs</em> sont en moyenne plus faibles et moins dispersés ; toutefois, des cas coûteux existent encore, soulignant l’importance de la qualité d’exécution et du choix des systèmes. Le parc <em>ancien</em> montre une dispersion nettement plus large, cohérente avec l’hétérogénéité des bâtis et des isolations.
</div>

# Conclusion

<div class="conclusion">
Cette étude a permis de :
<ul>
<li><strong>Mettre en lumière</strong> la distribution des coûts selon l’étiquette DPE : les classes F-G sont nettement plus onéreuses.</li>
<li><strong>Confirmer</strong> la corrélation positive entre surface et coût de chauffage, incitant à des actions d’isolation sur les grandes surfaces mal classées.</li>
<li><strong>Cartographier</strong> la répartition spatiale des logements pour cibler des zones d’intervention.</li>
<li><strong>Prioriser</strong> la rénovation du parc <em>ancien</em>, principal contributeur aux mauvaises performances.</li>
</ul>
Au global, les politiques publiques devraient concentrer subventions et accompagnements sur les logements anciens F-G, tout en garantissant, pour les constructions neuves, une exigence de qualité homogène afin d’éviter les “mauvais élèves”.
</div>
