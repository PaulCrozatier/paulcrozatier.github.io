# ============================================================================
# Module d'export en image (PNG) - Carte AMAP IDF
# ============================================================================
# Fonction unique : exporter_carte()
# Genere une carte ggplot statique avec habillage cartographique complet
# Sortie : chemin vers un fichier PNG temporaire
# ============================================================================

library(ggplot2)
library(ggspatial)
library(sf)
library(grid)
library(cowplot)
library(here)

# L'identite visuelle du reseau — logo et police de marque — n'est pas reprise
# dans cette demonstration. Police systeme, palette neutre.
POLICE <- "sans"

# Operateur utilitaire
`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 0) return(b)
  if (is.character(a) && length(a) == 1 && a == "") return(b)
  a
}

# ----------------------------------------------------------------------------
# exporter_carte
# ----------------------------------------------------------------------------
exporter_carte <- function(mode, donnees, options, fonds) {
  
  # DIAGNOSTIC console
  cat("\n===== EXPORT IMAGE =====\n")
  cat("Mode :", mode, "\n")
  cat("AAC présente :", !is.null(fonds$aac_sf),
      if (!is.null(fonds$aac_sf)) paste(" -", nrow(fonds$aac_sf), "polygones") else "", "\n")
  cat("ZPA présente :", !is.null(fonds$zpa_sf),
      if (!is.null(fonds$zpa_sf)) paste(" -", nrow(fonds$zpa_sf), "polygones") else "", "\n")
  cat("========================\n\n")
  
  # ----- Dimensions selon la taille (paysage) -----
  dim_paysage <- switch(options$taille %||% "A4",
                        "A5" = list(w = 21,   h = 14.8),
                        "A4" = list(w = 29.7, h = 21),
                        "A3" = list(w = 42,   h = 29.7),
                        list(w = 29.7, h = 21)
  )
  
  # ----- Construire la carte ggplot -----
  p <- ggplot() +
    geom_sf(data = fonds$idf_dept, fill = "#f5f5f0", color = "#bbbbbb", size = 0.3)
  
  if (isTRUE(options$afficher_communes) && !is.null(fonds$idf_communes)) {
    p <- p + geom_sf(data = fonds$idf_communes,
                     fill = NA, color = "#e0e0d8", size = 0.15)
  }
  
  p <- p + geom_sf(data = fonds$idf_sf,
                   fill = NA, color = "#2d5016", size = 0.7)
  
  # AAC (si visible dans l'app) - apres contour IDF pour etre dessus
  if (!is.null(fonds$aac_sf)) {
    cat("Rendu AAC en cours...\n")
    p <- p + geom_sf(data = fonds$aac_sf,
                     fill = "#4a9fd4", color = "#1a6fa8",
                     alpha = 0.5, size = 0.5)
  }
  
  # ZPA (si visible dans l'app) - contour brun sans remplissage
  # Les etiquettes ZPA sont ajoutees plus tard (au-dessus de tout)
  if (!is.null(fonds$zpa_sf)) {
    cat("Rendu ZPA en cours...\n")
    p <- p + geom_sf(data = fonds$zpa_sf,
                     fill = NA, color = "#a0522d",
                     size = 0.7)
  }
  
  # ----- Couches de donnees selon le mode + preparer les elements de legende -----
  legende_entries <- list()  # liste de list(color, label, shape) pour la legende
  
  if (mode == "explo") {
    if (!is.null(donnees$amap_vis) && nrow(donnees$amap_vis) > 0) {
      p <- p + geom_sf(data = donnees$amap_vis,
                       color = "#994d00", fill = "#FF8000",
                       size = 2.2, shape = 21, stroke = 0.4)
      legende_entries[[length(legende_entries) + 1]] <-
        list(fill = "#FF8000", color = "#994d00", label = "AMAP")
    }
    if (!is.null(donnees$fermes_vis) && nrow(donnees$fermes_vis) > 0) {
      p <- p + geom_sf(data = donnees$fermes_vis,
                       color = "#1a5c00", fill = "#33CC00",
                       size = 2.2, shape = 21, stroke = 0.4)
      legende_entries[[length(legende_entries) + 1]] <-
        list(fill = "#33CC00", color = "#1a5c00", label = "Ferme")
    }
  } else if (mode == "match") {
    r <- donnees
    if (r$type == "amap") {
      col_avec <- "#4a9fd4"; col_sans <- "#FF8000"; col_fer <- "#33CC00"
      col_anchor <- "#FF8000"
      lab_avec <- "AMAP partenaire"
      lab_sans <- "AMAP libre"
      lab_fer  <- "Ferme potentielle"
      lab_src  <- "AMAP source"
    } else {
      col_avec <- "#FF8000"; col_sans <- "#4a9fd4"; col_fer <- "#e74c3c"
      col_anchor <- "#33CC00"
      lab_avec <- "AMAP déjà prise"
      lab_sans <- "AMAP cible"
      lab_fer  <- "Ferme concurrente"
      lab_src  <- "Ferme source"
    }
    
    # Lignes (couche du bas)
    if (!is.null(r$lignes) && nrow(r$lignes) > 0) {
      lignes_df <- data.frame(
        x = c(r$lignes$lon_f, r$lignes$lon_g),
        y = c(r$lignes$lat_f, r$lignes$lat_g),
        grp = rep(seq_len(nrow(r$lignes)), 2)
      )
      p <- p + geom_path(data = lignes_df,
                         aes(x = x, y = y, group = grp),
                         color = "#888", size = 0.4, alpha = 0.6)
    }
    
    if (!is.null(r$amap_avec) && nrow(r$amap_avec) > 0) {
      p <- p + geom_sf(data = r$amap_avec, fill = col_avec, color = "black",
                       size = 2.2, shape = 21, stroke = 0.3)
      legende_entries[[length(legende_entries) + 1]] <-
        list(fill = col_avec, color = "black", label = lab_avec)
    }
    if (!is.null(r$amap_sans) && nrow(r$amap_sans) > 0) {
      p <- p + geom_sf(data = r$amap_sans, fill = col_sans, color = "black",
                       size = 1.8, shape = 21, stroke = 0.3, alpha = 0.7)
      legende_entries[[length(legende_entries) + 1]] <-
        list(fill = col_sans, color = "black", label = lab_sans)
    }
    if (!is.null(r$fermes_prod) && nrow(r$fermes_prod) > 0) {
      p <- p + geom_sf(data = r$fermes_prod, fill = col_fer, color = "black",
                       size = 2.2, shape = 21, stroke = 0.3)
      legende_entries[[length(legende_entries) + 1]] <-
        list(fill = col_fer, color = "black", label = lab_fer)
    }
    if (!is.null(r$fermes_hors_zone) && nrow(r$fermes_hors_zone) > 0) {
      p <- p + geom_sf(data = r$fermes_hors_zone, fill = col_fer, color = "black",
                       size = 1.6, shape = 21, stroke = 0.2, alpha = 0.4)
    }
    
    # Point d'ancrage (par-dessus tout)
    anchor_pt <- st_sfc(st_point(c(r$anchor$lon, r$anchor$lat)), crs = 4326)
    anchor_sf <- st_sf(geometry = anchor_pt)
    p <- p + geom_sf(data = anchor_sf, fill = col_anchor, color = "black",
                     size = 5, shape = 21, stroke = 1.2)
    legende_entries <- c(list(list(fill = col_anchor, color = "black",
                                   label = lab_src, source = TRUE)),
                         legende_entries)
  }
  
  # Ajout couches surfaciques a la legende
  if (!is.null(fonds$aac_sf)) {
    legende_entries[[length(legende_entries) + 1]] <- list(
      type = "polygon", fill = "#4a9fd4", color = "#1a6fa8",
      label = "AAC"
    )
  }
  if (!is.null(fonds$zpa_sf)) {
    legende_entries[[length(legende_entries) + 1]] <- list(
      type = "polygon", fill = NA, color = "#a0522d",
      label = "ZPA"
    )
  }
  
  # ----- Cadrage -----
  # Ajuster les bounds au ratio de l'image (largeur/hauteur)
  # pour eviter la distorsion : on elargit la dimension la plus courte
  b <- options$bounds
  if (!is.null(b) && !is.null(b$west) && !is.null(b$east) &&
      !is.null(b$south) && !is.null(b$north)) {
    # Ratio de la zone de carte (apres bandeaux haut/bas qui prennent ~12%)
    ratio_image <- dim_paysage$w / (dim_paysage$h * 0.88)
    # cos(lat_moyenne) pour corriger la projection
    lat_moy <- (b$south + b$north) / 2
    cos_lat <- cos(lat_moy * pi / 180)
    largeur_geo <- (b$east - b$west) * cos_lat
    hauteur_geo <- b$north - b$south
    ratio_geo <- largeur_geo / hauteur_geo
    
    if (ratio_geo < ratio_image) {
      # Trop etroit : elargir en longitude
      cible_largeur <- hauteur_geo * ratio_image / cos_lat
      centre_lon    <- (b$west + b$east) / 2
      b$west <- centre_lon - cible_largeur / 2
      b$east <- centre_lon + cible_largeur / 2
    } else {
      # Trop large : elargir en latitude
      cible_hauteur <- largeur_geo / ratio_image
      centre_lat    <- (b$south + b$north) / 2
      b$south <- centre_lat - cible_hauteur / 2
      b$north <- centre_lat + cible_hauteur / 2
    }
    coord_layer <- coord_sf(
      xlim = c(b$west, b$east),
      ylim = c(b$south, b$north),
      expand = FALSE
    )
  } else {
    coord_layer <- coord_sf(expand = FALSE)
  }
  
  # ----- Habillage cartographique (echelle, nord) -----
  p <- p +
    tryCatch(
      annotation_scale(location = "bl", width_hint = 0.15,
                       style = "bar", text_family = POLICE,
                       text_cex = 0.9, bar_cols = c("black", "white"),
                       line_width = 1),
      error = function(e) NULL
    ) +
    tryCatch(
      annotation_north_arrow(location = "tr", which_north = "true",
                             style = north_arrow_minimal(text_family = POLICE, text_size = 10),
                             height = unit(1.1, "cm"), width = unit(1.1, "cm"),
                             pad_x = unit(0.4, "cm"), pad_y = unit(0.4, "cm")),
      error = function(e) NULL
    ) +
    coord_layer +
    theme_void(base_family = POLICE) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "#fafaf7", color = NA),
      plot.margin      = margin(5, 5, 5, 5)
    )
  
  # ----- Composer la page complete avec cowplot -----
  # Layout :
  #   - haut : bandeau avec logo a gauche + titre centre
  #   - milieu : la carte (avec echelle/nord/legende interne)
  #   - bas : pied de page (source + date)
  
  # Bandeau titre. Palette neutre : les couleurs de marque ne sont pas reprises.
  titre_txt <- options$titre %||% ""
  if (nchar(titre_txt) > 0) {
    bandeau_titre <- ggdraw() +
      draw_grob(rectGrob(gp = gpar(fill = "#ece8f6", col = NA))) +
      draw_text(titre_txt, x = 0.5, y = 0.5, size = 22, fontface = "plain",
                family = POLICE, color = "#5b4b8a", hjust = 0.5, vjust = 0.5)
  } else {
    bandeau_titre <- NULL
  }

  # Pied de page : la mention voyage avec l'image, qui sort de l'application.
  pied <- ggdraw() +
    draw_text("Jeu de données fictif - entités entièrement générées, aucune donnée réelle publiée.",
              x = 0.02, y = 0.5, size = 9, family = POLICE,
              color = "#5b4b8a", hjust = 0, vjust = 0.5) +
    draw_text(paste0(if (!is.null(fonds$aac_sf) || !is.null(fonds$zpa_sf)) "Zonages : AESN   -   " else "",
                     format(Sys.Date(), "%d/%m/%Y")),
              x = 0.98, y = 0.5, size = 9, family = POLICE,
              color = "#555555", hjust = 1, vjust = 0.5)
  
  # Legende custom (si activee) - hauteur adaptee au nombre d'entries
  carte_layer <- if (isTRUE(options$afficher_legende) && length(legende_entries) > 0) {
    leg_grob <- build_legend_grob(legende_entries)
    n_entries <- length(legende_entries)
    leg_height <- n_entries * 0.03 + 0.02
    ggdraw(p) +
      draw_grob(leg_grob, x = 0.78, y = 0.05, width = 0.20, height = leg_height)
  } else {
    ggdraw(p)
  }
  
  # Assemblage vertical
  if (!is.null(bandeau_titre)) {
    final <- plot_grid(
      bandeau_titre,
      carte_layer,
      pied,
      ncol = 1,
      rel_heights = c(0.08, 0.88, 0.04)
    )
  } else {
    final <- plot_grid(
      carte_layer,
      pied,
      ncol = 1,
      rel_heights = c(0.96, 0.04)
    )
  }
  final <- final + theme(plot.background = element_rect(fill = "white", color = NA))
  
  # Logo en surimpression au-dessus de tout (coin haut-gauche)
  # Le logo du reseau n'est pas repris : c'est sa marque, pas un element du
  # travail presente ici.
  
  # ----- Generer le PNG -----
  outfile <- tempfile(fileext = ".png")
  ggsave(outfile, plot = final,
         width = dim_paysage$w, height = dim_paysage$h,
         units = "cm", dpi = 200, bg = "white")
  
  outfile
}

# Construire la legende custom sous forme de grob
build_legend_grob <- function(entries) {
  n <- length(entries)
  h_line <- 1 / n
  # Fond transparent sans contour
  bg <- rectGrob(x = 0.5, y = 0.5, width = 1, height = 1,
                 gp = gpar(fill = NA, col = NA))
  grobs <- list(bg)
  for (i in seq_along(entries)) {
    e <- entries[[i]]
    y_pos <- 1 - i * h_line + h_line / 2
    if (isTRUE(e$type == "polygon")) {
      # Carre representatif pour les couches surfaciques
      grobs[[length(grobs) + 1]] <- rectGrob(
        x = 0.1, y = y_pos, width = 0.06, height = h_line * 0.5,
        gp = gpar(fill = e$fill, col = e$color, lwd = 1)
      )
    } else {
      # Cercle pour les couches ponctuelles
      grobs[[length(grobs) + 1]] <- circleGrob(
        x = 0.1, y = y_pos, r = 0.025,
        gp = gpar(fill = e$fill, col = e$color,
                  lwd = if (isTRUE(e$source)) 2 else 1)
      )
    }
    grobs[[length(grobs) + 1]] <- textGrob(
      e$label, x = 0.18, y = y_pos, just = c("left", "center"),
      gp = gpar(fontfamily = POLICE, fontsize = 10, col = "#333")
    )
  }
  gTree(children = do.call(gList, grobs))
}