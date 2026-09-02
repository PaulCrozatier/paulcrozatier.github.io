# ==============================================================================
# APP SHINY — CARTE AMAP IDF — VERSION DE DEMONSTRATION
# ==============================================================================
# Portage de demonstration de l'application reelle. L'interface et les
# traitements sont ceux de l'outil d'origine ; la source de donnees est un jeu
# ENTIEREMENT FICTIF (data/donnees_demo.json), produit par
# R/generer_donnees_demo.R a partir d'une graine fixe.
#
# Aucune donnee reelle du reseau n'est publiee. Les champs permettant de
# reidentifier ou de contacter une structure — code postal, commune, lieu de
# distribution, telephone, courriel, site web — sont absents de la donnee ET
# de ce code. Ils ne peuvent donc pas reapparaitre en rebranchant une base.
# ==============================================================================
library(shiny)
library(sf)
library(dplyr)
library(tidyr)
library(leaflet)
library(tools)
library(openxlsx)
library(here)
library(httr)
library(jsonlite)

source(here::here("R", "02_helpers.R"))
source(here::here("R", "03_productions.R"))
source(here::here("R", "04_export_image.R"))

aac_sf       <- readRDS(here::here("data", "aac.rds")) %>% st_make_valid()
zpa_sf       <- readRDS(here::here("data", "zpa.rds")) %>% st_make_valid()
idf_sf       <- readRDS(here::here("data", "idf.rds")) %>% st_make_valid()
idf_dept     <- readRDS(here::here("data", "idf_dept.rds")) %>% st_make_valid()
# communes : chargement conditionnel (lourd)
chemin_com   <- here::here("data", "communes_idf.rds")
idf_communes <- if (file.exists(chemin_com)) readRDS(chemin_com) %>% st_make_valid() else NULL
# === MODIF CLAUDE (filtre territorial) : arrondissements de Paris ============
chemin_arr   <- here::here("data", "arr_paris.rds")
arr_paris    <- if (file.exists(chemin_arr)) readRDS(chemin_arr) %>% st_make_valid() else NULL

# Construit la table des territoires selectionnables (dept -> communes).
# Cas special 75 : les 20 arrondissements remplacent la commune "Paris".
build_territoires <- function() {
  if (is.null(idf_communes)) return(NULL)
  com <- idf_communes
  # communes hors Paris (INSEE_DEP != 75)
  com_hors_paris <- com %>%
    filter(INSEE_DEP != "75") %>%
    transmute(dep = INSEE_DEP, nom = NOM, type = "commune", geometry)
  # Paris : remplace par arrondissements si dispo, sinon Paris monobloc
  if (!is.null(arr_paris)) {
    paris <- arr_paris %>% transmute(dep = "75", nom = nom_arr, type = "arrondissement", geometry)
  } else {
    paris <- com %>% filter(INSEE_DEP == "75") %>%
      transmute(dep = "75", nom = NOM, type = "commune", geometry)
  }
  rbind(com_hors_paris, paris)
}
territoires_sf <- build_territoires()
deps_dispo <- if (!is.null(territoires_sf)) sort(unique(territoires_sf$dep)) else character(0)
# === FIN MODIF (filtre territorial) =========================================

# ==============================================================================
# CHARGEMENT — jeu de donnees fictif
# ==============================================================================
# Aucune connexion a une base de donnees. La demo lit un fichier local : il n'y
# a ni chaine de connexion, ni identifiant, ni secret dans ce depot.

chemin_donnees <- here::here("data", "donnees_demo.json")
if (!file.exists(chemin_donnees))
  stop("Jeu de demonstration introuvable. Lancer d'abord : Rscript R/generer_donnees_demo.R")

demo <- fromJSON(chemin_donnees, simplifyDataFrame = TRUE)

# Mention reprise telle quelle dans l'interface et en tete des exports.
MENTION_DEMO <- "Jeu de donnees fictif — les entites affichees sont entierement generees. Aucune donnee reelle du reseau n'est publiee. L'interface et les traitements sont ceux de l'outil d'origine."

raw_amap         <- demo$amap
raw_fermes       <- demo$fermes
raw_partenariats <- demo$partenariats

# ==============================================================================
# NETTOYAGE AMAP
# ==============================================================================
# Champs retires par rapport a l'application reelle : code postal, commune,
# nom et identifiant du lieu de distribution, courriel, telephone fixe et
# portable, site web, annee de creation. Ils ne sont ni lus ni nommes ici.
amap <- raw_amap %>%
  mutate(
    id_groupe   = as.character(id_groupe),
    nom_amap    = safe_text(nom_amap),
    statut_amap = safe_text(statut_amap),
    jour        = toTitleCase(tolower(safe_text(jour))),
    jour        = ifelse(jour == "" | is.na(jour), "Inconnu", jour),
    h_debut     = safe_text(h_debut),
    h_fin       = safe_text(h_fin),
    duree       = duree_fmt(h_debut, h_fin),
    nb_adh      = suppressWarnings(as.integer(nb_adh)),
    lat         = as.numeric(lat),
    lon         = as.numeric(lon)
  ) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  select(id_groupe, nom_amap, statut_amap,
         jour, h_debut, h_fin, duree,
         lat, lon, nb_adh) %>%
  distinct(id_groupe, .keep_all = TRUE)

# Productions actives via partenariats
prod_via_partenariats <- raw_partenariats %>%
  filter(is.na(date_fin)) %>%
  transmute(id_ferme = as.character(id_ferme), ids_part = safe_text(id_produits)) %>%
  filter(!is.na(id_ferme), ids_part != "") %>%
  group_by(id_ferme) %>%
  summarise(ids_via_part = paste(ids_part, collapse = "-"), .groups = "drop")

has_prod_sec <- "id_productions_sec" %in% colnames(raw_fermes)

# ==============================================================================
# NETTOYAGE FERMES
# ==============================================================================
fermes <- raw_fermes %>%
  mutate(id_ferme = as.character(id_ferme), lat = as.numeric(lat), lon = as.numeric(lon)) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  mutate(
    nom_ferme         = safe_text(nom_ferme),
    statut_ferme      = safe_text(statut_ferme),
    certif            = safe_text(certif),
    annee_amap        = safe_text(annee_amap),
    ids_prod_amap_raw = safe_text(id_productions_amap),
    ids_prod_main_raw = safe_text(id_productions),
    ids_prod_sec_raw  = if (has_prod_sec) safe_text(id_productions_sec) else ""
  ) %>%
  left_join(prod_via_partenariats, by = "id_ferme") %>%
  mutate(
    ids_via_part = ifelse(is.na(ids_via_part), "", ids_via_part),
    ids_prod_raw = sapply(seq_len(n()), function(i) {
      ids <- unique(c(decode_ids_prod(ids_prod_amap_raw[i]), decode_ids_prod(ids_prod_main_raw[i]),
                      decode_ids_prod(ids_prod_sec_raw[i]),  decode_ids_prod(ids_via_part[i])))
      if (length(ids) == 0) "" else paste(ids, collapse = ",")
    }),
    productions_txt = sapply(ids_prod_raw, function(x) {
      ids <- if (x == "") character(0) else strsplit(x, ",")[[1]]
      if (length(ids) == 0) return("")
      paste(ref_produits$label[ref_produits$id %in% as.integer(ids)], collapse = " | ")
    }),
    non_prod_txt = sapply(ids_prod_raw, function(x) {
      ids <- if (x == "") character(0) else strsplit(x, ",")[[1]]
      absentes <- setdiff(ALL_PROD_IDS, ids)
      if (length(absentes) == 0) return("")
      paste(ref_produits$label[ref_produits$id %in% as.integer(absentes)], collapse = " | ")
    })
  ) %>%
  # Champs retires : code postal, commune, telephone, courriel, annee
  # d'installation. Absents de la donnee comme du code.
  select(id_ferme, nom_ferme, statut_ferme,
         certif, annee_amap,
         productions_txt, non_prod_txt, ids_prod_raw, lat, lon)

# SF
amap_sf   <- st_as_sf(amap,   coords = c("lon","lat"), crs = 4326, remove = FALSE)
fermes_sf <- st_as_sf(fermes, coords = c("lon","lat"), crs = 4326, remove = FALSE)

# Enrichissement spatial : pour chaque entite, trouver l'AAC et la ZPA qui la contient
enrichir_aac_zpa <- function(sf_pts) {
  aac_idx <- st_intersects(sf_pts, aac_sf)
  aac_nom <- sapply(aac_idx, function(i) {
    if (length(i) == 0) "" else as.character(aac_sf$Nom_AAC[i[1]])
  })
  zpa_idx <- st_intersects(sf_pts, zpa_sf)
  zpa_nom <- sapply(zpa_idx, function(i) {
    if (length(i) == 0) "" else as.character(zpa_sf$territoire[i[1]])
  })
  sf_pts$aac_nom <- aac_nom
  sf_pts$zpa_nom <- zpa_nom
  sf_pts
}

amap_sf   <- enrichir_aac_zpa(amap_sf)
fermes_sf <- enrichir_aac_zpa(fermes_sf)

# Reinjecter dans amap et fermes (data.frames sans geom) pour le panneau detail / export
amap   <- amap   %>% left_join(amap_sf   %>% st_drop_geometry() %>% select(id_groupe, aac_nom, zpa_nom), by = "id_groupe")
fermes <- fermes %>% left_join(fermes_sf %>% st_drop_geometry() %>% select(id_ferme,  aac_nom, zpa_nom), by = "id_ferme")

# Partenariats actifs (pour la logique mise en relation)
partenariats_actifs <- raw_partenariats %>%
  filter(is.na(date_fin)) %>%
  transmute(
    id_ferme    = as.character(id_ferme),
    id_groupe   = as.character(id_groupe),
    id_produits = safe_text(id_produits)
  )

paires <- partenariats_actifs %>%
  select(id_ferme, id_groupe) %>%
  distinct()

# ============================================================================
# === MODIF CLAUDE (typo AESN) : typologie BINAIRE ===========================
# Remplace l'ancienne typo 8 classes (F1-F4 / A1-A4) par une classification
# binaire alignee sur le critere de financement AESN :
#   - Ferme  : "oui" si dans la zone (AAC/ZPA), sinon "non"
#   - AMAP   : "oui" si AU MOINS UNE de ses fermes partenaires est dans la zone
#              (recalcul DEPUIS LES PARTENARIATS, definition stricte par lien
#               ferme ; une AMAP dans la zone mais sans partenaire en zone = "non")
# Volume de controle attendu : AAC 137 fermes / 212 AMAP, ZPA 200 / 267.
# ============================================================================
# helper : isTRUE vectorise (NA/FALSE -> FALSE)
isTRUE_vec <- function(x) !is.na(x) & x

calc_typo_aesn <- function(zone_col) {
  fermes_zone <- fermes %>% transmute(id_ferme, ferme_in = (!!sym(zone_col)) != "")
  
  # AMAP : lien = au moins une ferme partenaire dans la zone
  a_lien <- paires %>%
    left_join(fermes_zone, by = "id_ferme") %>%
    group_by(id_groupe) %>%
    summarise(lien = any(ferme_in, na.rm = TRUE), .groups = "drop")
  
  f_typo <- fermes_zone %>%
    transmute(id_ferme, typo = ifelse(ferme_in, "oui", "non"))
  
  a_typo <- amap %>%
    select(id_groupe) %>%
    left_join(a_lien, by = "id_groupe") %>%
    transmute(id_groupe, typo = ifelse(isTRUE_vec(lien), "oui", "non"))
  
  list(fermes = f_typo, amap = a_typo)
}

typo_aac <- calc_typo_aesn("aac_nom")
typo_zpa <- calc_typo_aesn("zpa_nom")

fermes <- fermes %>%
  left_join(typo_aac$fermes %>% rename(typo_aac = typo), by = "id_ferme") %>%
  left_join(typo_zpa$fermes %>% rename(typo_zpa = typo), by = "id_ferme")
amap <- amap %>%
  left_join(typo_aac$amap %>% rename(typo_aac = typo), by = "id_groupe") %>%
  left_join(typo_zpa$amap %>% rename(typo_zpa = typo), by = "id_groupe")

# Garde-fou : valeurs manquantes -> "non"
fermes <- fermes %>% mutate(typo_aac = ifelse(is.na(typo_aac), "non", typo_aac),
                            typo_zpa = ifelse(is.na(typo_zpa), "non", typo_zpa))
amap   <- amap   %>% mutate(typo_aac = ifelse(is.na(typo_aac), "non", typo_aac),
                            typo_zpa = ifelse(is.na(typo_zpa), "non", typo_zpa))
# === FIN MODIF (typo AESN) ==================================================

# Productions par AMAP (via partenariats)
amap_prod <- paires %>%
  inner_join(fermes %>% select(id_ferme, ids_prod_raw), by = "id_ferme") %>%
  group_by(id_groupe) %>%
  summarise(all_ids = paste(ids_prod_raw[ids_prod_raw != ""], collapse = ","), .groups = "drop") %>%
  mutate(
    ids_u = sapply(all_ids, function(x) unique(unlist(strsplit(x, ","))), USE.NAMES = FALSE),
    prod_presentes_txt = sapply(ids_u, function(ids) {
      paste(ref_produits$label[ref_produits$id %in% as.integer(ids)], collapse = " | ")
    }, USE.NAMES = FALSE),
    prod_absentes_txt = sapply(ids_u, function(ids) {
      paste(ref_produits$label[ref_produits$id %in% as.integer(setdiff(ALL_PROD_IDS, ids))], collapse = " | ")
    }, USE.NAMES = FALSE)
  ) %>%
  select(id_groupe, prod_presentes_txt, prod_absentes_txt)

amap    <- amap %>% left_join(amap_prod, by = "id_groupe")
amap_sf <- st_as_sf(amap, coords = c("lon","lat"), crs = 4326, remove = FALSE)

# Jours de livraison par ferme : union des jours de ses AMAP partenaires
ferme_jours <- paires %>%
  inner_join(amap %>% select(id_groupe, jour), by = "id_groupe") %>%
  group_by(id_ferme) %>%
  summarise(jours = list(unique(jour)), .groups = "drop") %>%
  mutate(jours_txt = sapply(jours, function(j) paste(j, collapse = ", ")))

fermes <- fermes %>% left_join(ferme_jours, by = "id_ferme")
fermes$jours[sapply(fermes$jours, is.null)] <- list(character(0))
fermes$jours_txt[is.na(fermes$jours_txt)] <- ""

fermes_sf <- st_as_sf(fermes, coords = c("lon","lat"), crs = 4326, remove = FALSE)

# AMAP -> liste d'IDs de productions (pour la requête)
amap_prod_ids <- paires %>%
  inner_join(fermes %>% select(id_ferme, ids_prod_raw), by = "id_ferme") %>%
  group_by(id_groupe) %>%
  summarise(
    ids_prods = paste(ids_prod_raw[ids_prod_raw != ""], collapse = ","),
    .groups = "drop"
  ) %>%
  mutate(ids_prods_u = sapply(ids_prods, function(x) {
    paste(unique(unlist(strsplit(x, ","))), collapse = ",")
  }, USE.NAMES = FALSE)) %>%
  select(id_groupe, ids_prods_u)

# Jours & palette
ordre_jours    <- c("Lundi","Mardi","Mercredi","Jeudi","Vendredi","Samedi","Dimanche","Inconnu")
jours_presents <- ordre_jours[ordre_jours %in% unique(amap$jour)]
couleurs_jours <- c("#457B9D","#2A9D8F","#E9C46A","#F4A261","#E63946","#264653","#A8DADC","#999999")
palette_jours  <- setNames(rep_len(couleurs_jours, length(jours_presents)), jours_presents)

# ==============================================================================
# === MODIF CLAUDE (bug 2) : helper de normalisation des partenariats =========
# 1 ligne = 1 partenariat = 1 production. Productions eclatees depuis id_produits
# (delimite par "-"). Utilise par les deux exports.
# ==============================================================================
build_partenariats_normalise <- function(ids_f = NULL, ids_g = NULL) {
  df <- partenariats_actifs
  if (!is.null(ids_f)) df <- df %>% filter(id_ferme %in% ids_f)
  if (!is.null(ids_g)) df <- df %>% filter(id_groupe %in% ids_g)
  if (nrow(df) == 0) return(data.frame())
  
  rows <- lapply(seq_len(nrow(df)), function(i) {
    p <- df[i, ]
    prod_ids <- decode_ids_prod(p$id_produits)
    if (length(prod_ids) == 0) prod_ids <- NA_character_
    data.frame(
      id_ferme  = p$id_ferme,
      id_groupe = p$id_groupe,
      Production = unname(vapply(prod_ids, function(pid) {
        if (is.na(pid)) return("(non precise)")
        lbl <- ref_produits$label[ref_produits$id == suppressWarnings(as.integer(pid))]
        if (length(lbl) == 0 || isTRUE(is.na(lbl))) "(inconnu)" else lbl
      }, character(1))),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })
  out <- do.call(rbind, rows)
  
  out %>%
    left_join(fermes %>% select(id_ferme, nom_ferme), by = "id_ferme") %>%
    left_join(amap %>% select(id_groupe, nom_amap, jour), by = "id_groupe") %>%
    transmute(
      `ID Ferme` = id_ferme, `Nom Ferme` = nom_ferme,
      `ID AMAP` = id_groupe, `Nom AMAP` = nom_amap,
      Production,
      `Jour de livraison` = jour
    )
}

# ==============================================================================
# === MODIF CLAUDE (bug 6) : calcul mutualisation pour export =================
# Reproduit la logique d'affichage carte : pour chaque ferme de reference
# (centres des cercles), trouve les fermes voisines dans le rayon et la distance.
# Retourne un data.frame (ou NULL).
# ==============================================================================
calc_mutualisation <- function(r, rayon_mut) {
  if (is.null(rayon_mut) || is.na(rayon_mut) || rayon_mut <= 0) return(NULL)
  fermes_ref <- bind_rows(
    if (!is.null(r$fermes_prod) && nrow(r$fermes_prod) > 0)
      r$fermes_prod %>% st_drop_geometry() else NULL,
    if (!is.null(r$fermes_hors_zone) && nrow(r$fermes_hors_zone) > 0)
      (if (inherits(r$fermes_hors_zone, "sf")) st_drop_geometry(r$fermes_hors_zone) else r$fermes_hors_zone) else NULL,
    if (r$type == "amap" && !is.null(r$source_partners) && nrow(r$source_partners) > 0)
      (if (inherits(r$source_partners, "sf")) st_drop_geometry(r$source_partners) else r$source_partners) else NULL
  )
  if (is.null(fermes_ref) || nrow(fermes_ref) == 0) return(NULL)
  
  centres_sf <- st_as_sf(fermes_ref, coords = c("lon","lat"), crs = 4326, remove = FALSE)
  dists <- st_distance(fermes_sf, centres_sf)              # matrice fermes x centres
  ids_deja <- c(
    if (!is.null(r$fermes_prod)) r$fermes_prod$id_ferme,
    if (!is.null(r$fermes_hors_zone)) r$fermes_hors_zone$id_ferme,
    if (r$type == "amap" && !is.null(r$source_partners)) r$source_partners$id_ferme
  )
  
  out <- list()
  for (j in seq_len(nrow(centres_sf))) {
    d_col <- as.numeric(dists[, j])
    idx_in <- which(d_col <= rayon_mut * 1000)
    for (i in idx_in) {
      f_vois <- fermes_sf[i, ]
      if (f_vois$id_ferme %in% ids_deja) next
      if (f_vois$id_ferme == centres_sf$id_ferme[j]) next
      out[[length(out) + 1]] <- data.frame(
        `Ferme reference` = centres_sf$nom_ferme[j],
        `Ferme voisine`   = f_vois$nom_ferme,
        `Productions voisine` = f_vois$productions_txt,
        `Distance (km)`   = round(d_col[i] / 1000, 2),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}

# ==============================================================================
# EXPORTS
# ==============================================================================

# La mention voyage avec les fichiers telecharges : ils sortent de
# l'application et peuvent ensuite circuler seuls.
ajouter_feuille_mention <- function(wb) {
  addWorksheet(wb, "Avertissement")
  writeData(wb, "Avertissement", data.frame(
    Avertissement = c(
      "JEU DE DONNEES FICTIF",
      "",
      "Les AMAP et fermes de ce fichier sont entierement generees et n'existent pas.",
      "Aucune donnee reelle du reseau n'est publiee.",
      "L'interface, les traitements et le format de cet export sont ceux de l'outil d'origine.",
      "",
      "Export produit par la version de demonstration de la Carte AMAP Ile-de-France."
    ), stringsAsFactors = FALSE))
  addStyle(wb, "Avertissement",
           createStyle(fontSize = 14, textDecoration = "bold",
                       fontColour = "#FFFFFF", fgFill = "#5b4b8a"),
           rows = 2, cols = 1)
  setColWidths(wb, "Avertissement", cols = 1, widths = 95)
  invisible(wb)
}

generer_excel_selection <- function(fermes_vis, amap_vis, panier_data = NULL) {
  wb <- createWorkbook()
  ajouter_feuille_mention(wb)
  get_fermes_de_amap <- function(id_g) {
    ids <- partenariats_actifs %>% filter(id_groupe == id_g) %>% pull(id_ferme) %>% unique()
    if (length(ids) == 0) return("")
    paste(fermes$nom_ferme[fermes$id_ferme %in% ids], collapse = " | ")
  }
  get_amap_de_ferme <- function(id_f) {
    ids <- partenariats_actifs %>% filter(id_ferme == id_f) %>% pull(id_groupe) %>% unique()
    if (length(ids) == 0) return("")
    paste(amap$nom_amap[amap$id_groupe %in% ids], collapse = " | ")
  }
  
  if (nrow(amap_vis) > 0) {
    df_a <- amap_vis %>% st_drop_geometry() %>%
      mutate(`Fermes partenaires` = sapply(id_groupe, get_fermes_de_amap)) %>%
      select(ID = id_groupe, Nom = nom_amap,
             Statut = statut_amap, Jour = jour, Heure = h_debut,
             Duree = duree, Adherents = nb_adh,
             `Productions presentes` = prod_presentes_txt,
             `Productions absentes`  = prod_absentes_txt,
             `Fermes partenaires`,
             AAC = aac_nom, ZPA = zpa_nom, `En AAC` = typo_aac, `En ZPA` = typo_zpa)
    addWorksheet(wb, "AMAP")
    writeDataTable(wb, "AMAP", as.data.frame(df_a), tableStyle = "TableStyleMedium2")
  }
  if (nrow(fermes_vis) > 0) {
    df_f <- fermes_vis %>% st_drop_geometry() %>%
      mutate(`AMAP partenaires` = sapply(id_ferme, get_amap_de_ferme)) %>%
      select(ID = id_ferme, Nom = nom_ferme,
             Statut = statut_ferme,
             Productions = productions_txt, `Prod. absentes` = non_prod_txt,
             Certifications = certif, `En AMAP depuis` = annee_amap,
             `AMAP partenaires`,
             AAC = aac_nom, ZPA = zpa_nom, `En AAC` = typo_aac, `En ZPA` = typo_zpa)
    addWorksheet(wb, "Fermes")
    writeDataTable(wb, "Fermes", as.data.frame(df_f), tableStyle = "TableStyleMedium9")
  }
  
  # === MODIF CLAUDE (bug 2) : feuille Partenariats normalisee ===============
  # 1 ligne = 1 partenariat = 1 production, restreinte aux entites visibles.
  ids_f_vis <- if (nrow(fermes_vis) > 0) fermes_vis$id_ferme else NULL
  ids_g_vis <- if (nrow(amap_vis)   > 0) amap_vis$id_groupe  else NULL
  df_part <- build_partenariats_normalise(ids_f = ids_f_vis, ids_g = ids_g_vis)
  if (nrow(df_part) > 0) {
    addWorksheet(wb, "Partenariats")
    writeDataTable(wb, "Partenariats", as.data.frame(df_part), tableStyle = "TableStyleMedium4")
  }
  # === FIN MODIF (bug 2) ====================================================
  
  # === MODIF CLAUDE (panier) : feuille "Selection" des entites isolees =======
  if (!is.null(panier_data) && length(panier_data) > 0) {
    ids_pf <- vapply(panier_data, function(x) if (x$type == "ferme") x$id else NA_character_, character(1))
    ids_pa <- vapply(panier_data, function(x) if (x$type == "amap")  x$id else NA_character_, character(1))
    ids_pf <- ids_pf[!is.na(ids_pf)]; ids_pa <- ids_pa[!is.na(ids_pa)]
    lignes_sel <- list()
    if (length(ids_pf) > 0) {
      fsel <- fermes %>% filter(id_ferme %in% ids_pf)
      for (i in seq_len(nrow(fsel))) lignes_sel[[length(lignes_sel)+1]] <- data.frame(
        Type = "Ferme", Nom = fsel$nom_ferme[i],
        Statut = fsel$statut_ferme[i], Productions = fsel$productions_txt[i],
        stringsAsFactors = FALSE)
    }
    if (length(ids_pa) > 0) {
      asel <- amap %>% filter(id_groupe %in% ids_pa)
      for (i in seq_len(nrow(asel))) lignes_sel[[length(lignes_sel)+1]] <- data.frame(
        Type = "AMAP", Nom = asel$nom_amap[i],
        Statut = asel$statut_amap[i], Productions = asel$prod_presentes_txt[i],
        stringsAsFactors = FALSE)
    }
    if (length(lignes_sel) > 0) {
      df_sel <- do.call(rbind, lignes_sel)
      addWorksheet(wb, "Selection")
      writeData(wb, "Selection", df_sel)
      # === Style magenta : en-tete colore + bordures ===
      hs <- createStyle(fgFill = "#e6007e", fontColour = "#FFFFFF", textDecoration = "bold",
                        halign = "center", border = "TopBottomLeftRight", borderColour = "#b3005f")
      bs <- createStyle(border = "TopBottomLeftRight", borderColour = "#f3b0d4")
      addStyle(wb, "Selection", hs, rows = 1, cols = seq_len(ncol(df_sel)), gridExpand = TRUE)
      addStyle(wb, "Selection", bs, rows = 2:(nrow(df_sel)+1), cols = seq_len(ncol(df_sel)),
               gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, "Selection", cols = seq_len(ncol(df_sel)), widths = "auto")
    }
  }
  # === FIN MODIF (panier) ===================================================
  
  tmp <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb, tmp, overwrite = TRUE)
  tmp
}

generer_excel_requete <- function(params, amap_avec, amap_sans, fermes_res, lignes_res,
                                  mutualisation = NULL) {
  wb <- createWorkbook()
  ajouter_feuille_mention(wb)
  addWorksheet(wb, "Requete")
  df_req <- data.frame(
    Parametre = c("Type d'ancrage", "Nom du point de depart", "Localisation",
                  "Production recherchee", "Rayon (km)",
                  "Rayon mutualisation (km)", "Date"),
    Valeur    = c(params$type, params$nom, params$loc, params$prod, params$rayon,
                  params$rayon_mut %||% 0, format(Sys.Date(), "%d/%m/%Y"))
  )
  writeDataTable(wb, "Requete", df_req, tableStyle = "TableStyleMedium1")
  
  # === MODIF CLAUDE (typo AESN) : feuille Synthese AESN =======================
  # Compteurs de volume sur le perimetre de la requete (entites affichees).
  ids_f_res <- if (!is.null(fermes_res) && nrow(fermes_res) > 0) fermes_res$id_ferme else character(0)
  ids_a_res <- unique(c(
    if (!is.null(amap_avec) && nrow(amap_avec) > 0) amap_avec$id_groupe else NULL,
    if (!is.null(amap_sans) && nrow(amap_sans) > 0) amap_sans$id_groupe else NULL
  ))
  f_res <- fermes %>% filter(id_ferme %in% ids_f_res)
  a_res <- amap   %>% filter(id_groupe %in% ids_a_res)
  syn <- data.frame(
    Indicateur = c("Fermes (total requete)", "Fermes en AAC", "Fermes en ZPA",
                   "AMAP (total requete)", "AMAP liees a une ferme AAC", "AMAP liees a une ferme ZPA",
                   "Volume AESN AAC (fermes + AMAP)", "Volume AESN ZPA (fermes + AMAP)"),
    Valeur = c(
      nrow(f_res),
      sum(f_res$typo_aac == "oui", na.rm = TRUE),
      sum(f_res$typo_zpa == "oui", na.rm = TRUE),
      nrow(a_res),
      sum(a_res$typo_aac == "oui", na.rm = TRUE),
      sum(a_res$typo_zpa == "oui", na.rm = TRUE),
      sum(f_res$typo_aac == "oui", na.rm = TRUE) + sum(a_res$typo_aac == "oui", na.rm = TRUE),
      sum(f_res$typo_zpa == "oui", na.rm = TRUE) + sum(a_res$typo_zpa == "oui", na.rm = TRUE)
    )
  )
  addWorksheet(wb, "Synthese AESN")
  writeDataTable(wb, "Synthese AESN", syn, tableStyle = "TableStyleMedium7")
  # === FIN MODIF (typo AESN) ==================================================
  
  get_fermes_de_amap <- function(id_g) {
    ids <- partenariats_actifs %>% filter(id_groupe == id_g) %>% pull(id_ferme) %>% unique()
    if (length(ids) == 0) return("")
    paste(fermes$nom_ferme[fermes$id_ferme %in% ids], collapse = " | ")
  }
  get_amap_de_ferme <- function(id_f) {
    ids <- partenariats_actifs %>% filter(id_ferme == id_f) %>% pull(id_groupe) %>% unique()
    if (length(ids) == 0) return("")
    paste(amap$nom_amap[amap$id_groupe %in% ids], collapse = " | ")
  }
  get_fermes_prod_de_amap <- function(id_g, prod_id) {
    if (is.null(prod_id) || prod_id == "") return("")
    ids <- partenariats_actifs %>%
      filter(id_groupe == id_g) %>%
      inner_join(fermes %>% select(id_ferme, ids_prod_raw), by = "id_ferme") %>%
      filter(sapply(ids_prod_raw, function(x) prod_id %in% unlist(strsplit(x, ",")))) %>%
      pull(id_ferme) %>% unique()
    if (length(ids) == 0) return("")
    paste(fermes$nom_ferme[fermes$id_ferme %in% ids], collapse = " | ")
  }
  
  if (nrow(amap_avec) > 0) {
    df1 <- amap_avec %>% st_drop_geometry() %>%
      mutate(`Fermes fournissant cette prod` = sapply(id_groupe,
                                                      function(g) get_fermes_prod_de_amap(g, params$prod_id))) %>%
      select(ID = id_groupe, Nom = nom_amap,
             Statut = statut_amap, Jour = jour, Heure = h_debut,
             Duree = duree, Adherents = nb_adh,
             `Productions presentes` = prod_presentes_txt,
             `Fermes fournissant cette prod`,
             AAC = aac_nom, ZPA = zpa_nom, `En AAC` = typo_aac, `En ZPA` = typo_zpa)
    addWorksheet(wb, "AMAP avec production")
    writeDataTable(wb, "AMAP avec production", as.data.frame(df1), tableStyle = "TableStyleMedium2")
  }
  if (nrow(amap_sans) > 0) {
    df2 <- amap_sans %>% st_drop_geometry() %>%
      mutate(`Partenaires actuels (toutes prod)` = sapply(id_groupe, get_fermes_de_amap)) %>%
      select(ID = id_groupe, Nom = nom_amap,
             Statut = statut_amap, Jour = jour, Heure = h_debut,
             Duree = duree, Adherents = nb_adh,
             `Productions presentes` = prod_presentes_txt,
             `Partenaires actuels (toutes prod)`,
             AAC = aac_nom, ZPA = zpa_nom, `En AAC` = typo_aac, `En ZPA` = typo_zpa)
    addWorksheet(wb, "AMAP sans production")
    writeDataTable(wb, "AMAP sans production", as.data.frame(df2), tableStyle = "TableStyleMedium3")
  }
  if (nrow(fermes_res) > 0) {
    df3 <- fermes_res %>% st_drop_geometry() %>%
      mutate(`AMAP partenaires (toutes prod)` = sapply(id_ferme, get_amap_de_ferme)) %>%
      select(ID = id_ferme, Nom = nom_ferme,
             Statut = statut_ferme,
             Productions = productions_txt, Certifications = certif,
             `En AMAP depuis` = annee_amap,
             `AMAP partenaires (toutes prod)`,
             AAC = aac_nom, ZPA = zpa_nom, `En AAC` = typo_aac, `En ZPA` = typo_zpa)
    addWorksheet(wb, "Fermes")
    writeDataTable(wb, "Fermes", as.data.frame(df3), tableStyle = "TableStyleMedium9")
  }
  
  # === MODIF CLAUDE (bug 2) : Partenariats normalises (1 ligne / prod) =======
  if (!is.null(lignes_res) && nrow(lignes_res) > 0) {
    ids_f <- unique(lignes_res$id_ferme)
    ids_g <- unique(lignes_res$id_groupe)
    df4 <- build_partenariats_normalise(ids_f = ids_f, ids_g = ids_g)
    # ne garder que les paires reellement affichees
    paires_aff <- paste(lignes_res$id_ferme, lignes_res$id_groupe)
    df4 <- df4 %>% filter(paste(`ID Ferme`, `ID AMAP`) %in% paires_aff)
    if (nrow(df4) > 0) {
      addWorksheet(wb, "Partenariats")
      writeDataTable(wb, "Partenariats", as.data.frame(df4), tableStyle = "TableStyleMedium4")
    }
  }
  # === FIN MODIF (bug 2) =====================================================
  
  # === MODIF CLAUDE (bug 6) : feuille Mutualisation ==========================
  if (!is.null(mutualisation) && nrow(mutualisation) > 0) {
    addWorksheet(wb, "Mutualisation")
    writeDataTable(wb, "Mutualisation", as.data.frame(mutualisation), tableStyle = "TableStyleMedium5")
  }
  # === FIN MODIF (bug 6) =====================================================
  
  tmp <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb, tmp, overwrite = TRUE)
  tmp
}

# ==============================================================================
# UI  (identique a la version VM, sauf modifs marquees)
# ==============================================================================

css_app <- '
/* Polices systeme : l identite visuelle du reseau (logo et police de marque)
   n est pas reprise dans cette demonstration. */

*{box-sizing:border-box;margin:0;padding:0}
body{font-family:system-ui,-apple-system,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;overflow:hidden}

/* Cartouche permanent : la mention doit etre visible sans avoir a la chercher. */
#bandeau_demo{position:fixed;top:0;left:0;right:0;height:30px;z-index:2500;
  background:#5b4b8a;color:#fff;display:flex;align-items:center;justify-content:center;
  font-size:12px;font-weight:600;letter-spacing:.3px;box-shadow:0 1px 6px rgba(0,0,0,.25);}
#pied_demo{position:fixed;bottom:0;left:0;right:0;z-index:2400;
  background:rgba(35,30,50,.92);color:#e8e4f2;font-size:11px;line-height:1.5;
  padding:5px 14px;text-align:center;}
body{padding-top:30px}
#panel_left{position:fixed;top:0;left:0;bottom:0;width:340px;background:#fff;z-index:1500;overflow-y:auto;box-shadow:4px 0 20px rgba(0,0,0,.12);transition:transform .3s cubic-bezier(.4,0,.2,1);display:flex;flex-direction:column;}
#panel_left.collapsed{transform:translateX(-340px)}
#panel_header{background:#2d5016;color:white;padding:10px 14px;display:flex;align-items:center;flex-shrink:0}
#panel_header h2{font-size:14px;font-weight:600;margin:0}
#btn_toggle_fixed{position:fixed;top:50%;left:340px;transform:translateY(-50%);width:20px;height:48px;background:#2d5016;color:white;border:none;border-radius:0 6px 6px 0;cursor:pointer;font-size:14px;z-index:2000;transition:left .3s cubic-bezier(.4,0,.2,1);display:flex;align-items:center;justify-content:center;}
body.left-collapsed #btn_toggle_fixed{left:0}
#panel_body{padding:10px 14px;flex:1;overflow-y:auto}
.stitle{font-size:11px;font-weight:700;color:#2d5016;text-transform:uppercase;letter-spacing:.7px;margin:14px 0 6px;padding-bottom:4px;border-bottom:2px solid #c8dbb8}
.stitle:first-child{margin-top:4px}
#panel_body .form-group{margin-bottom:8px}
#panel_body .shiny-input-container{margin-bottom:0;width:100%!important}
#panel_body .radio label,#panel_body .radio-inline{font-size:12px;color:#333}
#panel_body .control-label{font-size:11px;font-weight:600;color:#555;margin-bottom:2px}
.fg{margin-bottom:8px}
.fg>label{display:block;font-size:11px;font-weight:600;color:#555;margin-bottom:2px}
.fg select,.fg input[type=text],.fg input[type=number]{width:100%;height:30px;font-size:12px;padding:2px 8px;border:1px solid #ccc;border-radius:4px;background:white}
.cb_row{display:flex;flex-wrap:wrap;gap:8px;align-items:center;margin:3px 0}
.cb_row label{font-size:12px;color:#333;cursor:pointer;display:flex;align-items:center;gap:3px}
.cb_row input[type=checkbox]{width:15px;height:15px;cursor:pointer}
#compteur_box{background:#f0f4ec;border-radius:6px;padding:8px 10px;margin:10px 0;font-size:11px;color:#555;line-height:1.5}
.btn_exp{display:block;width:100%;padding:8px;font-size:12px;font-weight:600;border:none;border-radius:5px;cursor:pointer;margin-top:6px;text-align:center;background:#c8dbb8;color:#1a3d00}
.btn_exp:hover{background:#a8cc8f}
.btn_action{display:block;width:100%;padding:8px;font-size:12px;font-weight:600;border:none;border-radius:5px;cursor:pointer;margin-top:6px;text-align:center;background:#2d5016;color:white}
.btn_action:hover{background:#1a3d00}
#panel_right{position:fixed;top:0;right:-380px;bottom:0;width:360px;background:#fff;z-index:1500;overflow-y:auto;box-shadow:-4px 0 20px rgba(0,0,0,.15);transition:right .3s cubic-bezier(.4,0,.2,1);}
#panel_right.open{right:0}
#pr_header{background:#f8f8f5;padding:12px 14px;border-bottom:1px solid #e0e0d8;position:sticky;top:0;z-index:2}
#pr_header h3{font-size:15px;color:#2d5016;margin:0 30px 0 0}
#btn_close_r{position:absolute;top:10px;right:12px;background:none;border:none;font-size:22px;cursor:pointer;color:#aaa;line-height:1}
#btn_close_r:hover{color:#333}
#pr_body{padding:10px 14px}
.info_row{margin:3px 0;color:#444;font-size:12px;line-height:1.5}
.info_label{font-weight:600;color:#666;font-size:11px}
.pr_sec{font-size:11px;font-weight:700;color:#888;text-transform:uppercase;letter-spacing:.6px;margin:14px 0 6px;padding-bottom:3px;border-bottom:1px solid #eee}
.pcard{background:#fafaf7;border-radius:6px;padding:7px 10px;margin-bottom:6px;border-left:3px solid #FF8000;font-size:12px;line-height:1.5}
.pcard.ferme{border-left-color:#2d7a00}
.pcard b{display:block;margin-bottom:1px;color:#333}
.pcard .sub{color:#777;font-size:11px}
.nonprod{color:#c0392b;font-size:11px;margin-top:4px;padding:4px 6px;background:#fdf0ef;border-radius:4px}
#carte{position:fixed!important;top:0;bottom:0;left:340px;right:0;z-index:1;transition:left .3s cubic-bezier(.4,0,.2,1),right .3s cubic-bezier(.4,0,.2,1)}
body.left-collapsed #carte{left:0}
body.right-open #carte{right:360px}
#zone_badge{position:fixed;bottom:10px;left:50%;transform:translateX(-50%);background:rgba(45,80,22,.92);color:white;padding:6px 16px;border-radius:16px;font-size:12px;z-index:1600;display:none;pointer-events:none}
#mode_hint{position:fixed;top:10px;left:355px;background:rgba(45,80,22,.92);color:white;padding:6px 12px;border-radius:6px;font-size:12px;z-index:1600;display:none}
.mode_toggle{display:flex;background:#e8efe2;border-radius:20px;padding:3px;margin:4px 0;position:relative;cursor:pointer;user-select:none}
.mode_toggle .opt{flex:1;text-align:center;padding:7px 10px;font-size:12px;font-weight:600;color:#5a7045;border-radius:18px;transition:color .25s ease;z-index:2;position:relative}
.mode_toggle .opt.active{color:white}
.mode_toggle .slider{position:absolute;top:3px;left:3px;width:calc(50% - 3px);height:calc(100% - 6px);background:#2d5016;border-radius:18px;transition:transform .25s cubic-bezier(.4,0,.2,1);z-index:1}
.mode_toggle.right .slider{transform:translateX(100%)}
.match_tab{display:flex;align-items:center;background:#f0f4ec;border-left:3px solid #c8dbb8;border-radius:4px;padding:6px 8px;margin:4px 0;font-size:12px;cursor:pointer;transition:background .15s ease}
.match_tab:hover{background:#e0ead4}
.match_tab.active{background:#c8dbb8;border-left-color:#2d5016;font-weight:600}
.match_tab .label{flex:1;color:#333;outline:none}
.match_tab .label[contenteditable=true]{background:white;padding:2px 4px;border-radius:3px;border:1px solid #2d5016}
.match_tab .close{color:#999;margin-left:8px;font-size:16px;line-height:1;padding:0 4px;border-radius:3px}
.match_tab .close:hover{color:#c0392b;background:#fdf0ef}
/* === MODIF CLAUDE (B3) : encadre indicateurs === */
#indic_box{position:fixed;bottom:30px;left:355px;background:rgba(255,255,255,.95);border-radius:8px;box-shadow:0 2px 12px rgba(0,0,0,.15);padding:10px 12px;z-index:1600;font-size:12px;min-width:170px;transition:left .3s cubic-bezier(.4,0,.2,1);}
body.left-collapsed #indic_box{left:15px}
#indic_header{font-size:11px;font-weight:700;color:#2d5016;text-transform:uppercase;letter-spacing:.5px;margin-bottom:6px;border-bottom:1px solid #c8dbb8;padding-bottom:4px;}
.indic_row{display:flex;justify-content:space-between;gap:12px;line-height:1.7;color:#444;}
.indic_row .val{font-weight:600;color:#2d5016;}
.indic_scope{font-size:10px;color:#888;font-style:italic;margin-bottom:4px;}
.indic_btn{flex:1;padding:5px 6px;font-size:10px;font-weight:600;border:1px solid #c8dbb8;border-radius:4px;background:#f0f4ec;color:#2d5016;cursor:pointer;}
.indic_btn:hover{background:#c8dbb8;}
/* === MODIF CLAUDE (chargement) : spinner global pendant les calculs === */
#busy_overlay{position:fixed;top:14px;left:50%;transform:translateX(-50%);z-index:3000;
  background:rgba(45,80,22,.92);color:#fff;padding:7px 16px;border-radius:18px;font-size:12px;
  font-weight:600;box-shadow:0 2px 10px rgba(0,0,0,.2);display:none;align-items:center;gap:8px;}
#busy_overlay .spin{width:13px;height:13px;border:2px solid rgba(255,255,255,.35);
  border-top-color:#fff;border-radius:50%;animation:busyspin .7s linear infinite;}
@keyframes busyspin{to{transform:rotate(360deg)}}
html.shiny-busy #busy_overlay{display:flex;}
'

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width,initial-scale=1"),
    tags$title("Carte AMAP Ile-de-France — demonstration"),
    tags$style(HTML(css_app))
  ),

  # Cartouche permanent en haut de l'interface.
  tags$div(id = "bandeau_demo", "JEU DE DONNEES FICTIF — demonstration de portfolio"),

  leafletOutput("carte", width = "100%", height = "100vh"),
  
  tags$div(id = "panel_left",
           tags$div(id = "panel_header",
                    tags$h2("AMAP Ile-de-France — demonstration")
           ),
           tags$div(id = "panel_body",
                    
                    tags$div(class = "stitle", "Mode"),
                    tags$div(id = "mode_toggle_wrap", class = "mode_toggle",
                             onclick = "
          var w = document.getElementById('mode_toggle_wrap');
          w.classList.toggle('right');
          var newVal = w.classList.contains('right') ? 'match' : 'explo';
          document.getElementById('mode_opt_explo').classList.toggle('active', newVal === 'explo');
          document.getElementById('mode_opt_match').classList.toggle('active', newVal === 'match');
          Shiny.setInputValue('mode_app', newVal, {priority:'event'});
        ",
                             tags$div(class = "slider"),
                             tags$div(id = "mode_opt_explo", class = "opt active", "Exploration"),
                             tags$div(id = "mode_opt_match", class = "opt", "Mise en relation")
                    ),
                    tags$div(style = "height:8px"),
                    
                    # ============ MODE MISE EN RELATION ============
                    conditionalPanel(condition = "input.mode_app == 'match'",
                                     tags$div(class = "stitle", "1. Source de la demande"),
                                     tags$div(class = "fg",
                                              radioButtons("match_type", NULL,
                                                           choices = c("AMAP" = "amap", "Ferme" = "ferme"),
                                                           selected = "amap", inline = TRUE)
                                     ),
                                     tags$div(class = "stitle", "2. Methode de selection"),
                                     tags$div(class = "fg",
                                              radioButtons("match_methode", NULL,
                                                           choices = c("Entite existante" = "exist", "Point libre (clic carte)" = "libre"),
                                                           selected = "exist", inline = FALSE)
                                     ),
                                     conditionalPanel(condition = "input.match_methode == 'exist'",
                                                      tags$div(class = "fg",
                                                               selectInput("match_entite", "Selectionner", choices = NULL, selected = NULL)
                                                      )
                                     ),
                                     tags$div(class = "stitle", "3. Partenariat recherche"),
                                     tags$div(class = "fg",
                                              selectInput("match_prod", "Production",
                                                          choices  = setNames(as.character(ref_produits$id), ref_produits$label),
                                                          selected = NULL)
                                     ),
                                     tags$div(class = "stitle", "4. Jour de livraison"),
                                     tags$div(class = "fg",
                                              selectInput("match_jour", NULL,
                                                          choices  = c("Tous" = "tous", setNames(jours_presents, jours_presents)),
                                                          selected = "tous")
                                     ),
                                     tags$div(class = "stitle", "5. Zonage"),
                                     tags$div(class = "fg",
                                              numericInput("match_rayon", "Rayon (km) autour de la source", value = 20, min = 1, max = 100)
                                     ),
                                     tags$div(class = "cb_row",
                                              checkboxInput("match_show_lignes", "Afficher les liens ferme-AMAP", value = TRUE)
                                     ),
                                     tags$div(class = "fg",
                                              tags$label("Rayon de mutualisation (km)"),
                                              tags$div(style = "font-size:10px;color:#777;margin-bottom:4px;",
                                                       "Affiche autour de chaque ferme partenaire actuelle un cercle pour identifier des opportunites de mutualisation de tournee. Mettre 0 pour desactiver."),
                                              numericInput("match_rayon_mut", NULL, value = 0, min = 0, max = 50)
                                     ),
                                     tags$div(class = "stitle", "6. Actions"),
                                     tags$button(id = "match_run", class = "btn_action",
                                                 onclick = "Shiny.setInputValue('match_run',Math.random(),{priority:'event'})",
                                                 "Lancer la recherche"),
                                     tags$button(id = "match_save", class = "btn_exp",
                                                 onclick = "Shiny.setInputValue('match_save',Math.random(),{priority:'event'})",
                                                 "Sauvegarder cette recherche"),
                                     tags$button(id = "match_reset", class = "btn_exp",
                                                 onclick = "Shiny.setInputValue('match_reset',Math.random(),{priority:'event'})",
                                                 "Quitter la recherche"),
                                     tags$div(id = "compteur_box_match", textOutput("compteur_match")),
                                     downloadButton("dl_match", "Exporter les resultats (Excel)", class = "btn_exp"),
                                     tags$button(id = "btn_export_img_match", class = "btn_exp",
                                                 onclick = "Shiny.setInputValue('open_export_modal','match',{priority:'event'})",
                                                 "Exporter en image"),
                                     
                                     tags$div(class = "stitle", "Recherches sauvegardees"),
                                     uiOutput("match_tabs_ui"),
                                     
                                     tags$div(class = "stitle", "Itineraire"),
                                     tags$div(class = "fg",
                                              radioButtons("itin_mode", NULL,
                                                           choices = c("Direct (vers une entite cliquee)" = "direct",
                                                                       "Tournee (par toutes les entites)" = "tournee",
                                                                       "Etapes choisies (clic sur la carte)" = "etapes"),
                                                           selected = "direct", inline = FALSE)
                                     ),
                                     # === MODIF CLAUDE (bug 8) : selection manuelle des etapes ==============
                                     conditionalPanel(
                                       condition = "input.itin_mode == 'etapes'",
                                       tags$div(style = "font-size:10px;color:#777;margin-bottom:4px;",
                                                "Activez la selection, puis cliquez les entites a inclure dans l'ordre. Re-cliquer une etape la retire."),
                                       tags$button(id = "itin_select_toggle", class = "btn_exp",
                                                   onclick = "Shiny.setInputValue('itin_select_toggle',Math.random(),{priority:'event'})",
                                                   "Activer / desactiver la selection"),
                                       uiOutput("itin_waypoints_ui")
                                     ),
                                     # === FIN MODIF (bug 8) =================================================
                                     tags$button(id = "itin_run", class = "btn_action",
                                                 onclick = "Shiny.setInputValue('itin_run',Math.random(),{priority:'event'})",
                                                 "Calculer l'itineraire"),
                                     tags$button(id = "itin_clear", class = "btn_exp",
                                                 onclick = "Shiny.setInputValue('itin_clear',Math.random(),{priority:'event'})",
                                                 "Effacer"),
                                     uiOutput("itin_gmaps_ui"),
                                     # === MODIF CLAUDE (GPX) : telechargement itineraire au format GPX ======
                                     conditionalPanel(
                                       condition = "output.itin_has_result",
                                       downloadButton("dl_gpx", "Telecharger l'itineraire (GPX)", class = "btn_exp")
                                     ),
                                     # === FIN MODIF (GPX) ===================================================
                                     tags$div(id = "itin_info_box", uiOutput("itin_info"))
                    ),
                    
                    # ============ MODE EXPLORATION ============
                    conditionalPanel(condition = "input.mode_app == 'explo'",
                                     # === MODIF CLAUDE (passe B1) : filtres actifs + reset ==================
                                     uiOutput("filtres_actifs_ui"),
                                     # === FIN MODIF (passe B1) ==============================================
                                     tags$div(class = "stitle", "Recherche"),
                                     tags$div(class = "fg",
                                              tags$input(id = "recherche", type = "text", placeholder = "Nom d'AMAP ou de ferme...",
                                                         oninput = "Shiny.setInputValue('recherche',this.value,{priority:'event'})")
                                     ),
                                     tags$div(class = "stitle", "Couches"),
                                     tags$div(class = "cb_row",
                                              checkboxInput("show_amap",   "AMAP",   value = TRUE),
                                              checkboxInput("show_fermes", "Fermes", value = TRUE)
                                     ),
                                     # === MODIF CLAUDE (passe B2) : AAC/ZPA sous Couches avec filtres =======
                                     tags$div(class = "cb_row",
                                              checkboxInput("show_aac", "AAC", value = FALSE),
                                              checkboxInput("show_zpa", "ZPA", value = FALSE)
                                     ),
                                     conditionalPanel(
                                       condition = "input.show_aac || input.show_zpa",
                                       tags$div(id = "compteur_box", textOutput("compteur_aesn")),
                                       # === MODIF CLAUDE (clip) : par defaut, seules les entites concernees ==
                                       # s'affichent. Cocher "afficher le reste" reaffiche les autres en gris.
                                       tags$div(class = "fg",
                                                checkboxInput("aesn_show_reste", "Afficher aussi le reste du reseau (en gris)", value = FALSE)
                                       )
                                     ),
                                     # === FIN MODIF (passe B2) =============================================
                                     tags$div(class = "stitle", "Filtres"),
                                     # === MODIF CLAUDE (filtre territorial) : multi-selection ===============
                                     tags$div(class = "fg",
                                              selectInput("filtre_dep", "Departement(s)",
                                                          choices = setNames(deps_dispo, deps_dispo),
                                                          selected = NULL, multiple = TRUE)
                                     ),
                                     conditionalPanel(
                                       condition = "input.filtre_dep != null && input.filtre_dep.length > 0",
                                       tags$div(class = "fg",
                                                selectInput("filtre_com", "Commune(s) / arrondissement(s)",
                                                            choices = NULL, selected = NULL, multiple = TRUE)
                                       )
                                     ),
                                     # === FIN MODIF (filtre territorial) ====================================
                                     tags$div(class = "fg",
                                              selectInput("filtre_jour", "Jour",
                                                          choices  = c("Tous" = "tous", setNames(jours_presents, jours_presents)),
                                                          selected = "tous")
                                     ),
                                     tags$div(class = "fg",
                                              selectInput("filtre_statut", "Statut AMAP",
                                                          choices  = c("Tous" = "tous", "Fonctionne" = "fonctionne", "Complet" = "complet",
                                                                       "En creation" = "creation", "Inactif" = "inactif"),
                                                          selected = "tous")
                                     ),
                                     tags$div(class = "fg",
                                              selectInput("filtre_statut_f", "Statut ferme",
                                                          choices  = c("Tous" = "tous", "Active" = "active", "Inactive" = "inactive"),
                                                          selected = "tous")
                                     ),
                                     tags$div(class = "fg",
                                              selectInput("filtre_prod", "Production presente",
                                                          choices  = c("Toutes" = "toutes",
                                                                       setNames(as.character(ref_produits$id), ref_produits$label)),
                                                          selected = "toutes")
                                     ),
                                     tags$div(class = "fg",
                                              selectInput("filtre_nprod", "Production absente",
                                                          choices  = c("Pas de filtre" = "toutes",
                                                                       setNames(as.character(ref_produits$id),
                                                                                paste0("Sans : ", ref_produits$label))),
                                                          selected = "toutes")
                                     ),
                                     # === MODIF CLAUDE (passe B2) : bloc Zone AAC/ZPA du bas supprime ======
                                     # (deplace sous "Couches" : cases show_aac/show_zpa + filtres rattaches)
                                     tags$div(class = "stitle", "Zonage"),
                                     # === MODIF CLAUDE (bug 3) : bouton "Effacer" retire. ====================
                                     # La zone se vide en effacant le champ rayon (voir observeEvent rayon_km).
                                     tags$div(class = "fg",
                                              tags$label("Rayon (km) - cliquer sur la carte"),
                                              tags$input(id = "rayon_km", type = "number", value = "", min = 1, max = 100,
                                                         placeholder = "ex: 20", style = "width:100%;",
                                                         onchange = "Shiny.setInputValue('rayon_km',this.value,{priority:'event'})")
                                     ),
                                     # === FIN MODIF (bug 3) ==================================================
                                     tags$div(id = "compteur_box", textOutput("compteur")),
                                     # === MODIF CLAUDE (panier) : selection manuelle d'entites =============
                                     tags$div(class = "stitle", "Selection manuelle"),
                                     tags$div(style = "font-size:10px;color:#777;margin-bottom:4px;",
                                              "Isolez des AMAP/fermes (perte d'adhesion, recherche de paniers...). Activez puis cliquez les entites."),
                                     tags$button(id = "panier_toggle", class = "btn_exp",
                                                 onclick = "Shiny.setInputValue('panier_toggle',Math.random(),{priority:'event'})",
                                                 "Activer / desactiver la selection"),
                                     uiOutput("panier_ui"),
                                     # === FIN MODIF (panier) ===============================================
                                     tags$div(class = "stitle", "Export"),
                                     downloadButton("dl_sel", "Exporter la selection (Excel)", class = "btn_exp"),
                                     tags$button(id = "btn_export_img_explo", class = "btn_exp",
                                                 onclick = "Shiny.setInputValue('open_export_modal','explo',{priority:'event'})",
                                                 "Exporter en image"),
                                     tags$div(style = "height:20px")
                    )
           )
  ),
  
  tags$button(id = "btn_toggle_fixed", "\u276E",
              onclick = "
      var collapsed = document.getElementById('panel_left').classList.toggle('collapsed');
      document.body.classList.toggle('left-collapsed');
      document.getElementById('btn_toggle_fixed').textContent = collapsed ? '\u276F' : '\u276E';
      setTimeout(function(){
        document.querySelectorAll('.leaflet-container').forEach(function(m){
          if(m._leaflet_map) m._leaflet_map.invalidateSize();
        });
      }, 320);"
  ),
  
  tags$div(id = "panel_right",
           tags$div(id = "pr_header",
                    tags$h3(id = "pr_title", ""),
                    tags$button(id = "btn_close_r", "\u2715",
                                onclick = "Shiny.setInputValue('close_right',Math.random(),{priority:'event'})")
           ),
           tags$div(id = "pr_body", uiOutput("detail_content"))
  ),
  
  tags$div(id = "zone_badge", ""),
  tags$div(id = "mode_hint", ""),

  # Pied de page permanent. Ce n'est pas un avertissement juridique : c'est la
  # demonstration que le probleme a ete traite par construction.
  tags$div(id = "pied_demo",
           tags$b("Demonstration. "),
           "Les AMAP et fermes affichees sont ", tags$b("entierement generees"),
           " et n'existent pas. Aucune donnee reelle du reseau n'est publiee. ",
           "L'interface, les filtres, la typologie de zonage, les calculs ",
           "d'itineraire et les exports sont ceux de l'outil d'origine."),
  # === MODIF CLAUDE (chargement) : indicateur visible pendant les calculs =====
  tags$div(id = "busy_overlay", tags$span(class = "spin"), tags$span("Calcul en cours...")),
  # === FIN MODIF (chargement) ================================================
  # === MODIF CLAUDE (B3) : encadre indicateurs territoriaux ==================
  conditionalPanel(
    condition = "input.mode_app == 'explo'",
    tags$div(id = "indic_box",
             tags$div(id = "indic_header", "Indicateurs"),
             uiOutput("indicateurs_ui"),
             tags$div(style = "display:flex;gap:4px;margin-top:6px;",
                      tags$button(id = "indic_vue", class = "indic_btn",
                                  onclick = "Shiny.setInputValue('indic_vue',Math.random(),{priority:'event'})",
                                  "Calculer sur la vue"),
                      tags$button(id = "indic_total", class = "indic_btn",
                                  onclick = "Shiny.setInputValue('indic_total',Math.random(),{priority:'event'})",
                                  "Total")
             )
    )
  ),
  # === FIN MODIF (B3) ========================================================
  
  tags$div(id = "popup_overlay",
           style = "position:fixed;top:0;left:0;right:0;bottom:0;background:rgba(0,0,0,.5);z-index:9999;display:flex;align-items:center;justify-content:center;",
           tags$div(style = "background:white;border-radius:10px;padding:28px 32px;max-width:480px;width:90%;box-shadow:0 8px 32px rgba(0,0,0,.2);",
                    tags$h2("Carte AMAP Ile-de-France — version de demonstration",
                            style = "color:#2d5016;font-size:17px;margin-bottom:16px;"),
                    tags$p(tags$b("Jeu de donnees fictif."),
                           " Les AMAP et fermes affichees sont entierement generees et n'existent pas.",
                           " Aucune donnee reelle du reseau n'est publiee. L'interface et les traitements",
                           " sont ceux de l'outil d'origine, developpe en stage de fin d'etudes.",
                           style = "font-size:13px;color:#5b4b8a;margin-bottom:12px;background:#f2effa;padding:8px 10px;border-radius:6px;"),
                    tags$p("Cet outil permet d'explorer un reseau d'AMAP et de fermes partenaires en Ile-de-France.",
                           style = "font-size:13px;color:#444;margin-bottom:12px;"),
                    tags$ul(style = "font-size:12px;color:#444;padding-left:18px;line-height:2;",
                            tags$li("Mode Exploration : filtrer et naviguer dans le reseau"),
                            tags$li("Mode Mise en relation : trouver des partenaires potentiels"),
                            tags$li("Cliquez sur un marqueur pour voir le detail"),
                            tags$li("Exportez votre selection en Excel"),
                            # === MODIF CLAUDE (bug 5) : indication legende ========================
                            # NB : la legende s'affiche en bas a droite. C'est le panneau de DETAIL
                            # (a droite) qui peut la masquer quand il est ouvert -> le fermer (croix)
                            # la fait reapparaitre. A valider avec Paul (sa consigne disait "gauche").
                            tags$li(tags$b("Astuce : "), "en mode Mise en relation, la legende apparait en bas a droite de la carte. Si le panneau de detail (a droite) la cache, fermez-le avec la croix.")
                            # === FIN MODIF (bug 5) ================================================
                    ),
                    tags$button("Commencer",
                                style = "margin-top:16px;width:100%;padding:10px;background:#2d5016;color:white;border:none;border-radius:6px;font-size:14px;font-weight:600;cursor:pointer;",
                                onclick = "document.getElementById('popup_overlay').style.display='none';"
                    )
           )
  ),
  
  tags$script(HTML("
    Shiny.addCustomMessageHandler('open_right',function(x){
      document.getElementById('panel_right').classList.add('open');
      document.body.classList.add('right-open');
      document.getElementById('pr_title').textContent=x.title||'';
      setTimeout(function(){document.querySelectorAll('.leaflet-container').forEach(function(m){if(m._leaflet_map)m._leaflet_map.invalidateSize();});},320);
    });
    Shiny.addCustomMessageHandler('close_right',function(x){
      document.getElementById('panel_right').classList.remove('open');
      document.body.classList.remove('right-open');
      setTimeout(function(){document.querySelectorAll('.leaflet-container').forEach(function(m){if(m._leaflet_map)m._leaflet_map.invalidateSize();});},320);
    });
    Shiny.addCustomMessageHandler('zone_badge_msg',function(x){
      var el=document.getElementById('zone_badge');
      if(x.text){el.textContent=x.text;el.style.display='block';}else{el.style.display='none';}
    });
    Shiny.addCustomMessageHandler('mode_hint_msg',function(x){
      var el=document.getElementById('mode_hint');
      if(x.text){el.textContent=x.text;el.style.display='block';}else{el.style.display='none';}
    });
    Shiny.addCustomMessageHandler('reset_recherche',function(x){
      var el=document.getElementById('recherche');
      if(el){el.value='';Shiny.setInputValue('recherche','',{priority:'event'});}
    });
    Shiny.addCustomMessageHandler('reset_rayon',function(x){
      var el=document.getElementById('rayon_km');
      if(el){el.value='';Shiny.setInputValue('rayon_km','',{priority:'event'});}
    });
    $(document).on('shiny:connected', function(){
      Shiny.setInputValue('mode_app', 'explo');
    });
  "))
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  losange_icons <- function(fill = "#33CC00", border = "#1a5c00",
                            size = 12, border_w = 0.75) {
    box <- size + 6
    half <- box / 2
    r <- size / sqrt(2) * 0.9
    pts <- sprintf("%f,%f %f,%f %f,%f %f,%f",
                   half, half - r, half + r, half, half, half + r, half - r, half)
    n <- max(length(fill), length(border))
    if (length(fill) == 1)   fill   <- rep(fill,   n)
    if (length(border) == 1) border <- rep(border, n)
    urls <- mapply(function(f, b) {
      svg <- sprintf(
        '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d"><polygon points="%s" fill="%s" stroke="%s" stroke-width="%f"/></svg>',
        box, box, pts, f, b, border_w)
      paste0("data:image/svg+xml;utf8,", URLencode(svg, reserved = TRUE))
    }, fill, border, USE.NAMES = FALSE)
    leaflet::icons(iconUrl = urls, iconWidth = box, iconHeight = box,
                   iconAnchorX = half, iconAnchorY = half)
  }
  
  # === MODIF CLAUDE (passe A) : losanges avec opacite + tailles vectorisees ===
  losange_icons_op <- function(fill = "#33CC00", border = "#1a5c00",
                               size = 12, border_w = 0.75, opacity = 1) {
    n <- max(length(fill), length(border), length(size), length(border_w), length(opacity))
    fill     <- rep_len(fill, n);     border   <- rep_len(border, n)
    size     <- rep_len(size, n);     border_w <- rep_len(border_w, n)
    opacity  <- rep_len(opacity, n)
    box_max  <- max(size) + 6
    half     <- box_max / 2
    urls <- vapply(seq_len(n), function(i) {
      r <- size[i] / sqrt(2) * 0.9
      pts <- sprintf("%f,%f %f,%f %f,%f %f,%f",
                     half, half - r, half + r, half, half, half + r, half - r, half)
      svg <- sprintf(
        '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d"><polygon points="%s" fill="%s" fill-opacity="%.2f" stroke="%s" stroke-opacity="%.2f" stroke-width="%f"/></svg>',
        box_max, box_max, pts, fill[i], opacity[i], border[i], opacity[i], border_w[i])
      paste0("data:image/svg+xml;utf8,", URLencode(svg, reserved = TRUE))
    }, character(1))
    leaflet::icons(iconUrl = urls, iconWidth = box_max, iconHeight = box_max,
                   iconAnchorX = half, iconAnchorY = half)
  }
  # === FIN MODIF (passe A) ====================================================
  
  sel              <- reactiveVal(NULL)
  # === MODIF CLAUDE (panier) : panier de selection manuelle ===================
  panier        <- reactiveVal(list())    # list de list(type, id, nom)
  panier_mode   <- reactiveVal(FALSE)     # TRUE = le clic marque/demarque
  # Dessine les entites du panier en magenta, par-dessus (visible dans tous les modes)
  dessiner_panier <- function(proxy) {
    p <- panier()
    if (length(p) == 0) return(invisible())
    ids_pf <- vapply(p, function(x) if (x$type == "ferme") x$id else NA_character_, character(1))
    ids_pa <- vapply(p, function(x) if (x$type == "amap")  x$id else NA_character_, character(1))
    ids_pf <- ids_pf[!is.na(ids_pf)]; ids_pa <- ids_pa[!is.na(ids_pa)]
    if (length(ids_pf) > 0) {
      pf <- fermes_sf %>% filter(id_ferme %in% ids_pf)
      if (nrow(pf) > 0) proxy %>% addMarkers(data = pf, lng = ~lon, lat = ~lat,
                                             icon = losange_icons_op(fill = "#e6007e", border = "#000", size = 20, border_w = 1.5, opacity = 1),
                                             label = ~paste0(nom_ferme, " (selection)"),
                                             layerId = ~paste0("panier_ferme_", id_ferme), group = "panier")
    }
    if (length(ids_pa) > 0) {
      pa <- amap_sf %>% filter(id_groupe %in% ids_pa)
      if (nrow(pa) > 0) proxy %>% addCircleMarkers(data = pa, lng = ~lon, lat = ~lat,
                                                   color = "#000", fillColor = "#e6007e", fillOpacity = 1, radius = 11, weight = 2.5,
                                                   label = ~paste0(nom_amap, " (selection)"),
                                                   layerId = ~paste0("panier_amap_", id_groupe), group = "panier")
    }
    invisible()
  }
  # === FIN MODIF (panier) =====================================================
  # === MODIF CLAUDE (filtre territorial) : cascade multi dept -> communes =====
  observeEvent(input$filtre_dep, {
    deps <- input$filtre_dep
    if (is.null(deps) || length(deps) == 0 || is.null(territoires_sf)) {
      updateSelectInput(session, "filtre_com", choices = character(0), selected = character(0))
      return()
    }
    noms <- sort(unique(territoires_sf$nom[territoires_sf$dep %in% deps]))
    updateSelectInput(session, "filtre_com",
                      choices = setNames(noms, noms), selected = character(0))
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
  # Polygone (union) du/des territoire(s) selectionne(s).
  # Si des communes sont cochees -> union des communes ; sinon union des departements.
  territoire_filtre <- reactive({
    deps <- input$filtre_dep
    if (is.null(deps) || length(deps) == 0 || is.null(territoires_sf)) return(NULL)
    coms <- input$filtre_com
    sub <- if (!is.null(coms) && length(coms) > 0)
      territoires_sf %>% filter(.data$nom %in% coms)
    else
      territoires_sf %>% filter(.data$dep %in% deps)
    if (nrow(sub) == 0) return(NULL)
    st_union(sub)
  })
  # === FIN MODIF (filtre territorial) =========================================
  # === MODIF CLAUDE (passe B2) : zone active derivee des cases show_aac/zpa ====
  # Remplace l'ancien radio aesn_zone. AAC prioritaire si les deux sont cochees.
  aesn_z <- reactive({
    if (isTRUE(input$show_aac)) "aac"
    else if (isTRUE(input$show_zpa)) "zpa"
    else "none"
  })
  # === FIN MODIF (passe B2) ===================================================
  zone_center      <- reactiveVal(NULL)
  match_point      <- reactiveVal(NULL)
  match_result     <- reactiveVal(NULL)
  match_saved      <- reactiveVal(list())
  match_saved_active <- reactiveVal(NULL)
  itin_result      <- reactiveVal(NULL)
  # === MODIF CLAUDE (bug 8) : etats pour la selection manuelle des etapes =====
  itin_waypoints   <- reactiveVal(list())   # liste ordonnee : list(type, id, nom, lon, lat)
  itin_select_mode <- reactiveVal(FALSE)    # TRUE = le clic carte ajoute/retire une etape
  # === FIN MODIF (bug 8) ======================================================
  
  observe({
    if (isTRUE(input$match_type == "amap")) {
      choices <- setNames(amap$id_groupe, amap$nom_amap)
    } else {
      choices <- setNames(fermes$id_ferme, fermes$nom_ferme)
    }
    updateSelectInput(session, "match_entite", choices = choices, selected = NULL)
  })
  
  observe({
    liste_simple <- setNames(as.character(ref_produits$id), ref_produits$label)
    if (isTRUE(input$match_type == "ferme") &&
        isTRUE(input$match_methode == "exist") &&
        !is.null(input$match_entite) && input$match_entite != "") {
      f <- fermes %>% filter(id_ferme == input$match_entite) %>% slice(1)
      if (nrow(f) == 1 && f$ids_prod_raw != "") {
        ids_ferme <- unlist(strsplit(f$ids_prod_raw, ","))
        ids_ferme <- ids_ferme[ids_ferme != ""]
        idx_ferme  <- ref_produits$id %in% as.integer(ids_ferme)
        prod_ferme <- setNames(as.character(ref_produits$id[idx_ferme]), ref_produits$label[idx_ferme])
        prod_autres <- setNames(as.character(ref_produits$id[!idx_ferme]), ref_produits$label[!idx_ferme])
        choices_grp <- list()
        if (length(prod_ferme) > 0)  choices_grp[["Productions de cette ferme"]] <- prod_ferme
        if (length(prod_autres) > 0) choices_grp[["Autres productions"]] <- prod_autres
        updateSelectInput(session, "match_prod", choices = choices_grp)
        return()
      }
    }
    updateSelectInput(session, "match_prod", choices = liste_simple)
  })
  
  observe({
    if (isTRUE(input$mode_app == "match") &&
        isTRUE(input$match_methode == "libre") &&
        is.null(match_point())) {
      session$sendCustomMessage("mode_hint_msg", list(text = "Cliquez sur la carte pour poser l'ancre"))
    } else {
      session$sendCustomMessage("mode_hint_msg", list(text = NULL))
    }
  })
  
  fermer_detail <- function() {
    sel(NULL)
    session$sendCustomMessage("close_right", list())
  }
  
  observeEvent(input$close_right, fermer_detail(), ignoreInit = TRUE)
  
  # === MODIF CLAUDE (bug 3) : observeEvent clear_zone retire (bouton supprime).
  # La zone se vide via le champ rayon (voir observeEvent input$rayon_km).
  
  # === MODIF CLAUDE (bug 3) : vider la zone quand le champ rayon est vide =====
  observeEvent(input$rayon_km, {
    rayon <- suppressWarnings(as.numeric(input$rayon_km %||% ""))
    if (is.na(rayon) || rayon <= 0) {
      zone_center(NULL)
      session$sendCustomMessage("zone_badge_msg", list(text = NULL))
    }
  }, ignoreInit = TRUE)
  # === FIN MODIF (bug 3) ======================================================
  
  observeEvent(input$match_reset, {
    match_result(NULL); match_point(NULL); match_saved_active(NULL); itin_result(NULL)
    fermer_detail()
  }, ignoreInit = TRUE)
  
  observeEvent(input$match_save, {
    r <- match_result()
    if (is.null(r)) { showNotification("Aucune recherche a sauvegarder", type = "warning", duration = 4); return() }
    saved <- match_saved()
    if (length(saved) >= 8) {
      showNotification("Limite de 8 recherches atteinte. Supprimez une recherche avant.", type = "warning", duration = 5); return()
    }
    label <- paste0(r$prod, " - ", r$anchor$nom)
    if (nchar(label) > 35) label <- paste0(substr(label, 1, 32), "...")
    new_id <- paste0("tab_", as.integer(Sys.time()), "_", sample(1000:9999, 1))
    saved[[new_id]] <- list(label = label, result = r)
    match_saved(saved); match_saved_active(new_id)
    showNotification("Recherche sauvegardee", type = "message", duration = 3)
  }, ignoreInit = TRUE)
  
  observeEvent(input$match_tab_select, {
    tab_id <- input$match_tab_select; saved <- match_saved()
    if (!is.null(saved[[tab_id]])) {
      match_result(saved[[tab_id]]$result); match_saved_active(tab_id)
      r <- saved[[tab_id]]$result
      leafletProxy("carte", session) %>% setView(r$anchor$lon, r$anchor$lat, zoom = 11)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$match_tab_remove, {
    tab_id <- input$match_tab_remove; saved <- match_saved()
    saved[[tab_id]] <- NULL; match_saved(saved)
    if (identical(match_saved_active(), tab_id)) { match_saved_active(NULL); match_result(NULL) }
  }, ignoreInit = TRUE)
  
  observeEvent(input$match_tab_rename, {
    info <- input$match_tab_rename
    if (is.null(info$id) || is.null(info$label)) return()
    saved <- match_saved()
    if (!is.null(saved[[info$id]])) { saved[[info$id]]$label <- info$label; match_saved(saved) }
  }, ignoreInit = TRUE)
  
  output$match_tabs_ui <- renderUI({
    saved <- match_saved(); active <- match_saved_active()
    if (length(saved) == 0) {
      return(tags$p(style = "font-size:11px;color:#999;font-style:italic;padding:4px 0;", "Aucune recherche sauvegardee"))
    }
    tagList(lapply(names(saved), function(id) {
      is_active <- !is.null(active) && active == id
      tags$div(class = paste("match_tab", if (is_active) "active" else ""), `data-id` = id,
               tags$span(class = "label", `data-id` = id, contenteditable = "false",
                         ondblclick = paste0("var el=this;el.contentEditable='true';el.focus();"),
                         onblur = paste0("this.contentEditable='false';Shiny.setInputValue('match_tab_rename',{id:'", id, "',label:this.textContent},{priority:'event'});"),
                         onkeydown = "if(event.key==='Enter'){event.preventDefault();this.blur();}",
                         onclick = paste0("if(this.contentEditable!=='true'){Shiny.setInputValue('match_tab_select','", id, "',{priority:'event'});}"),
                         saved[[id]]$label),
               tags$span(class = "close",
                         onclick = paste0("event.stopPropagation();Shiny.setInputValue('match_tab_remove','", id, "',{priority:'event'});"), "\u00D7")
      )
    }))
  })
  
  # ===== ITINERAIRE OSRM =====
  observeEvent(input$itin_clear, {
    itin_result(NULL)
    itin_waypoints(list())          # === MODIF (bug 8) : vide aussi les etapes
    itin_select_mode(FALSE)
  }, ignoreInit = TRUE)
  
  # === MODIF CLAUDE (bug 8) : activation du mode selection d'etapes ===========
  observeEvent(input$itin_select_toggle, {
    new_mode <- !itin_select_mode()
    itin_select_mode(new_mode)
    if (new_mode)
      session$sendCustomMessage("mode_hint_msg", list(text = "Mode etapes : cliquez les entites a inclure"))
    else
      session$sendCustomMessage("mode_hint_msg", list(text = NULL))
  }, ignoreInit = TRUE)
  
  # Retirer une etape via son bouton
  observeEvent(input$itin_wp_remove, {
    key <- input$itin_wp_remove
    wp <- itin_waypoints()
    wp <- wp[vapply(wp, function(x) paste0(x$type, "_", x$id) != key, logical(1))]
    itin_waypoints(wp)
  }, ignoreInit = TRUE)
  
  # Affichage de la liste ordonnee des etapes
  output$itin_waypoints_ui <- renderUI({
    wp <- itin_waypoints()
    actif <- itin_select_mode()
    etat <- if (actif) tags$div(style = "font-size:11px;color:#1a7a1a;font-weight:600;margin:4px 0;",
                                "Selection ACTIVE") else
                                  tags$div(style = "font-size:11px;color:#999;margin:4px 0;", "Selection inactive")
    if (length(wp) == 0)
      return(tagList(etat, tags$p(style = "font-size:11px;color:#999;font-style:italic;", "Aucune etape selectionnee")))
    tagList(etat, lapply(seq_along(wp), function(i) {
      w <- wp[[i]]; key <- paste0(w$type, "_", w$id)
      tags$div(class = "match_tab",
               tags$span(class = "label", paste0(i, ". ", w$nom)),
               tags$span(class = "close",
                         onclick = paste0("Shiny.setInputValue('itin_wp_remove','", key, "',{priority:'event'});"), "\u00D7"))
    }))
  })
  # === FIN MODIF (bug 8) ======================================================
  
  observeEvent(input$itin_run, {
    r <- match_result()
    if (is.null(r)) { showNotification("Lancez d'abord une recherche.", type = "warning", duration = 4); return() }
    mode_itin <- input$itin_mode %||% "direct"
    src <- c(r$anchor$lon, r$anchor$lat)
    if (mode_itin == "etapes") {
      # === MODIF CLAUDE (bug 8) : itineraire suivant les etapes choisies =======
      wp <- itin_waypoints()
      if (length(wp) == 0) {
        showNotification("Aucune etape selectionnee. Activez la selection et cliquez des entites.", type = "warning", duration = 5); return()
      }
      pts <- lapply(wp, function(w) c(w$lon, w$lat))
      waypoints <- c(list(src), pts)   # depart = ancre, puis les etapes dans l'ordre
      endpoint <- "route"              # ordre respecte (pas d'optimisation)
      # === FIN MODIF (bug 8) ===================================================
    } else if (mode_itin == "direct") {
      s <- sel()
      if (is.null(s)) { showNotification("Cliquez d'abord sur une entite cible sur la carte.", type = "warning", duration = 5); return() }
      if (s$type == "ferme") {
        f <- fermes %>% filter(id_ferme == s$id) %>% slice(1); if (nrow(f) == 0) return()
        cible <- c(f$lon, f$lat)
      } else {
        a <- amap %>% filter(id_groupe == s$id) %>% slice(1); if (nrow(a) == 0) return()
        cible <- c(a$lon, a$lat)
      }
      waypoints <- list(src, cible); endpoint <- "route"
    } else {
      if (r$type == "amap") {
        pts_df <- bind_rows(
          r$fermes_prod %>% st_drop_geometry() %>% select(lon, lat),
          if (!is.null(r$fermes_hors_zone) && nrow(r$fermes_hors_zone) > 0)
            (if (inherits(r$fermes_hors_zone,"sf")) st_drop_geometry(r$fermes_hors_zone) else r$fermes_hors_zone) %>% select(lon, lat) else NULL
        )
      } else {
        pts_df <- r$amap_sans %>% st_drop_geometry() %>% select(lon, lat)
      }
      if (nrow(pts_df) == 0) { showNotification("Aucune entite pour la tournee.", type = "warning", duration = 4); return() }
      if (nrow(pts_df) > 50) { showNotification("Tournee limitee aux 50 premieres entites.", type = "warning", duration = 4); pts_df <- pts_df[1:50, ] }
      waypoints <- c(list(src), lapply(seq_len(nrow(pts_df)), function(i) c(pts_df$lon[i], pts_df$lat[i])))
      endpoint <- "trip"
    }
    coords_str <- paste(sapply(waypoints, function(w) paste0(w[1], ",", w[2])), collapse = ";")
    url <- if (endpoint == "trip") {
      paste0("https://router.project-osrm.org/trip/v1/driving/", coords_str, "?source=first&roundtrip=false&geometries=geojson&overview=full")
    } else {
      paste0("https://router.project-osrm.org/route/v1/driving/", coords_str, "?geometries=geojson&overview=full")
    }
    tryCatch({
      withProgress(message = "Calcul de l'itineraire...", value = 0.5, {
        resp <- GET(url, timeout(10))
        if (status_code(resp) != 200) { showNotification("Service d'itineraire indisponible (erreur HTTP).", type = "error", duration = 6); return() }
        data_json <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
        if (data_json$code != "Ok") { showNotification(paste("OSRM :", data_json$code), type = "error", duration = 6); return() }
        if (endpoint == "trip") {
          trip <- data_json$trips[[1]]
          coords <- do.call(rbind, lapply(trip$geometry$coordinates, function(c) c(c[[1]], c[[2]])))
          dist_km <- trip$distance / 1000; dur_min <- trip$duration / 60
        } else {
          route <- data_json$routes[[1]]
          coords <- do.call(rbind, lapply(route$geometry$coordinates, function(c) c(c[[1]], c[[2]])))
          dist_km <- route$distance / 1000; dur_min <- route$duration / 60
        }
        itin_result(list(coords = coords, dist_km = dist_km, dur_min = dur_min, waypoints = waypoints))
      })
    }, error = function(e) {
      showNotification(paste("Erreur itineraire :", conditionMessage(e)), type = "error", duration = 8)
    })
  }, ignoreInit = TRUE)
  
  output$itin_info <- renderUI({
    i <- itin_result(); if (is.null(i)) return(NULL)
    tags$div(style = "background:#e8efe2;border-radius:6px;padding:8px;margin-top:8px;font-size:12px;",
             tags$div(style = "font-weight:600;color:#2d5016;margin-bottom:4px;", "Itineraire calcule"),
             tags$div(sprintf("Distance : %.1f km", i$dist_km)),
             tags$div(sprintf("Duree : %d min", round(i$dur_min))))
  })
  
  output$itin_gmaps_ui <- renderUI({
    i <- itin_result()
    if (is.null(i) || is.null(i$waypoints) || length(i$waypoints) < 2) return(NULL)
    pts_str <- sapply(i$waypoints, function(w) paste0(w[2], ",", w[1]))
    url_gmaps <- paste0("https://www.google.com/maps/dir/", paste(pts_str, collapse = "/"))
    tags$a(href = url_gmaps, target = "_blank", class = "btn_exp",
           style = "display:block;text-align:center;text-decoration:none;background:#4285f4;color:white;",
           "Ouvrir dans Google Maps")
  })
  
  # === MODIF CLAUDE (GPX) : flag + export GPX (compatible OsmAnd, Garmin...) ==
  output$itin_has_result <- reactive({ !is.null(itin_result()) })
  outputOptions(output, "itin_has_result", suspendWhenHidden = FALSE)
  
  output$dl_gpx <- downloadHandler(
    filename = function() paste0("itineraire_amap_", format(Sys.Date(), "%Y%m%d"), ".gpx"),
    content  = function(file) {
      i <- itin_result()
      if (is.null(i)) { showNotification("Aucun itineraire a exporter.", type = "warning", duration = 4); return() }
      esc <- function(x) gsub("&", "&amp;", gsub("<", "&lt;", gsub(">", "&gt;", x)))
      
      # Points d'etape (waypoints) : depart + etapes
      wpts <- ""
      if (!is.null(i$waypoints)) {
        for (k in seq_along(i$waypoints)) {
          w <- i$waypoints[[k]]   # c(lon, lat)
          nom <- if (k == 1) "Depart" else paste0("Etape ", k - 1)
          wpts <- paste0(wpts, sprintf(
            '  <wpt lat="%f" lon="%f"><name>%s</name></wpt>\n', w[2], w[1], esc(nom)))
        }
      }
      # Trace (track) : geometrie complete de la route OSRM
      trkpts <- ""
      if (!is.null(i$coords) && nrow(i$coords) > 0) {
        for (j in seq_len(nrow(i$coords))) {
          trkpts <- paste0(trkpts, sprintf(
            '      <trkpt lat="%f" lon="%f"></trkpt>\n', i$coords[j, 2], i$coords[j, 1]))
        }
      }
      gpx <- paste0(
        '<?xml version="1.0" encoding="UTF-8"?>\n',
        '<gpx version="1.1" creator="Carte AMAP IDF" xmlns="http://www.topografix.com/GPX/1/1">\n',
        sprintf(paste0('  <metadata><name>Itineraire AMAP IDF (demonstration)</name>',
                       '<desc>Jeu de donnees fictif : les etapes de cet itineraire sont ',
                       'des entites generees, aucune donnee reelle du reseau n est publiee.',
                       '</desc><time>%s</time></metadata>\n'),
                format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
        wpts,
        '  <trk><name>Itineraire</name><trkseg>\n', trkpts, '    </trkseg></trk>\n',
        '</gpx>\n')
      writeLines(gpx, file, useBytes = TRUE)
    }
  )
  # === FIN MODIF (GPX) =======================================================
  
  last_marker_click <- reactiveVal(0)
  observeEvent(input$carte_marker_click, { last_marker_click(as.numeric(Sys.time())) }, priority = 10)
  
  observeEvent(input$carte_click, {
    if (as.numeric(Sys.time()) - last_marker_click() < 0.3) return()
    if (isTRUE(input$mode_app == "match") && isTRUE(input$match_methode == "libre")) {
      match_point(list(lat = input$carte_click$lat, lon = input$carte_click$lng)); return()
    }
    rayon <- suppressWarnings(as.numeric(input$rayon_km %||% ""))
    if (!is.na(rayon) && rayon > 0) {
      zone_center(list(lat = input$carte_click$lat, lon = input$carte_click$lng))
      session$sendCustomMessage("zone_badge_msg",
                                list(text = paste0("Zone : ", rayon, " km autour de ",
                                                   round(input$carte_click$lat, 3), ", ", round(input$carte_click$lng, 3))))
    } else {
      if (!is.null(sel())) fermer_detail()
    }
  })
  
  observeEvent(input$carte_marker_click, {
    layer <- input$carte_marker_click$id %||% ""
    if (layer == "") return()
    if      (startsWith(layer, "ferme_")) { type <- "ferme"; id <- sub("ferme_", "", layer) }
    else if (startsWith(layer, "amap_"))  { type <- "amap";  id <- sub("amap_",  "", layer) }
    else return()
    
    # === MODIF CLAUDE (bug 8) : en mode selection d'etapes, le clic ajoute/retire =
    # une etape AU LIEU d'ouvrir le pop-up. Hors de ce mode, comportement normal.
    if (isTRUE(input$mode_app == "match") && isTRUE(input$itin_mode == "etapes") && isTRUE(itin_select_mode())) {
      if (type == "ferme") {
        e <- fermes %>% filter(id_ferme == id) %>% slice(1); if (nrow(e) == 0) return()
        nom <- e$nom_ferme; lon <- e$lon; lat <- e$lat
      } else {
        e <- amap %>% filter(id_groupe == id) %>% slice(1); if (nrow(e) == 0) return()
        nom <- e$nom_amap; lon <- e$lon; lat <- e$lat
      }
      wp <- itin_waypoints(); key <- paste0(type, "_", id)
      deja <- vapply(wp, function(x) paste0(x$type, "_", x$id) == key, logical(1))
      if (any(deja)) {
        wp <- wp[!deja]                                  # re-clic = retirer
      } else {
        wp <- c(wp, list(list(type = type, id = id, nom = nom, lon = lon, lat = lat)))  # ajouter
      }
      itin_waypoints(wp)
      return()   # ne PAS ouvrir le pop-up dans ce mode
    }
    # === FIN MODIF (bug 8) =====================================================
    
    # === MODIF CLAUDE (panier) : en mode selection, le clic marque/demarque ===
    if (isTRUE(input$mode_app == "explo") && isTRUE(panier_mode())) {
      if (type == "ferme") {
        e <- fermes %>% filter(id_ferme == id) %>% slice(1); if (nrow(e) == 0) return()
        nom <- e$nom_ferme
      } else {
        e <- amap %>% filter(id_groupe == id) %>% slice(1); if (nrow(e) == 0) return()
        nom <- e$nom_amap
      }
      p <- panier(); key <- paste0(type, "_", id)
      deja <- vapply(p, function(x) paste0(x$type, "_", x$id) == key, logical(1))
      if (any(deja)) p <- p[!deja] else p <- c(p, list(list(type = type, id = id, nom = nom)))
      panier(p)
      return()   # ne pas ouvrir le pop-up en mode selection
    }
    # === FIN MODIF (panier) ===================================================
    
    s <- sel()
    if (!is.null(s) && s$type == type && s$id == id) { fermer_detail(); return() }
    titre <- if (type == "ferme") fermes$nom_ferme[fermes$id_ferme == id][1] %||% "Ferme"
    else amap$nom_amap[amap$id_groupe == id][1] %||% "AMAP"
    sel(list(type = type, id = id))
    session$sendCustomMessage("open_right", list(title = titre))
  })
  
  # === MODIF CLAUDE (filtre territorial) : observeEvent zoom retire ===========
  # On n'effectue plus de zoom : le contour du territoire est dessine sur la
  # carte (voir bloc "contour territorial" dans le rendu), et le clip s'applique
  # via fermes_f()/amap_f().
  # === FIN MODIF (filtre territorial) =========================================
  
  # --- Recherche (mode exploration) ---
  ids_r <- reactive({
    q <- tolower(trimws(input$recherche %||% ""))
    if (nchar(q) < 2) return(NULL)
    # La recherche ne porte plus que sur les noms : code postal, commune et
    # lieu de distribution sont absents de la donnee comme du code.
    list(
      f = fermes$id_ferme[grepl(q, tolower(fermes$nom_ferme), fixed = TRUE)],
      a = amap$id_groupe[grepl(q, tolower(amap$nom_amap), fixed = TRUE)]
    )
  })
  
  observeEvent(ids_r(), {
    r <- ids_r(); if (is.null(r)) return()
    pts <- bind_rows(
      fermes %>% filter(id_ferme  %in% r$f) %>% select(lon, lat),
      amap   %>% filter(id_groupe %in% r$a) %>% select(lon, lat)
    )
    if (nrow(pts) == 0) return()
    proxy <- leafletProxy("carte", session)
    if (nrow(pts) == 1) proxy %>% setView(pts$lon[1], pts$lat[1], zoom = 13)
    else proxy %>% fitBounds(min(pts$lon) - .02, min(pts$lat) - .02, max(pts$lon) + .02, max(pts$lat) + .02)
  })
  
  zone_filtre <- reactive({
    zc <- zone_center(); rayon <- suppressWarnings(as.numeric(input$rayon_km %||% ""))
    if (!is.null(zc) && !is.na(rayon) && rayon > 0)
      list(centre = st_sfc(st_point(c(zc$lon, zc$lat)), crs = 4326), rayon = rayon * 1000)
    else NULL
  })
  
  # ===== MODE EXPLORATION : filtrage classique =====
  # === MODIF CLAUDE (bug 1) : la recherche ne FILTRE plus l'affichage. ========
  # Elle sert uniquement a zoomer (voir observeEvent(ids_r())). Les lignes
  #   r <- ids_r(); if (!is.null(r)) df <- df %>% filter(...)
  # ont ete retirees de fermes_f() et amap_f() pour que les clics/selections
  # restent visibles meme quand la barre de recherche contient du texte.
  fermes_f <- reactive({
    df <- fermes_sf
    sf <- input$filtre_statut_f %||% "tous"
    pr <- input$filtre_prod     %||% "toutes"
    np <- input$filtre_nprod    %||% "toutes"
    jo <- input$filtre_jour     %||% "tous"
    if (sf != "tous")   df <- df %>% filter(statut_ferme == sf)
    if (pr != "toutes") df <- df %>% filter(sapply(ids_prod_raw, function(x)  pr %in% unlist(strsplit(x, ",")), USE.NAMES = FALSE))
    if (np != "toutes") df <- df %>% filter(sapply(ids_prod_raw, function(x) !np %in% unlist(strsplit(x, ",")), USE.NAMES = FALSE))
    if (jo != "tous")   df <- df %>% filter(sapply(jours, function(j) jo %in% j))
    aesn_z <- aesn_z()
    # === MODIF CLAUDE (clip) : zone active + reste masque -> ne garder que "oui"
    if (aesn_z != "none" && !isTRUE(input$aesn_show_reste)) {
      col_typo <- if (aesn_z == "aac") "typo_aac" else "typo_zpa"
      df <- df %>% filter(.data[[col_typo]] == "oui")
    }
    z <- zone_filtre()
    if (!is.null(z)) df <- df %>% filter(as.numeric(st_distance(geometry, z$centre)) <= z$rayon)
    # === MODIF CLAUDE (filtre territorial) : intersection avec le territoire ===
    terr <- territoire_filtre()
    if (!is.null(terr)) {
      inter <- st_intersects(df, terr, sparse = FALSE)[, 1]
      df <- df[inter, ]
    }
    df
  })
  
  amap_f <- reactive({
    df <- amap_sf
    jo <- input$filtre_jour   %||% "tous"
    st <- input$filtre_statut %||% "tous"
    if (jo != "tous") df <- df %>% filter(jour == !!jo)
    if (st != "tous") df <- df %>% filter(statut_amap == st)
    # === MODIF CLAUDE (fix filtre prod AMAP) : filtre production manquant ======
    # La production d'une AMAP = union des productions de ses fermes partenaires
    # (objet amap_prod_ids$ids_prods_u, le meme que pop-up / mise en relation).
    pr <- input$filtre_prod  %||% "toutes"
    np <- input$filtre_nprod %||% "toutes"
    if (pr != "toutes") {
      ids_ok <- amap_prod_ids$id_groupe[
        vapply(amap_prod_ids$ids_prods_u, function(x) pr %in% unlist(strsplit(x, ",")), logical(1))]
      df <- df %>% filter(id_groupe %in% ids_ok)
    }
    if (np != "toutes") {
      ids_ok <- amap_prod_ids$id_groupe[
        vapply(amap_prod_ids$ids_prods_u, function(x) !(np %in% unlist(strsplit(x, ","))), logical(1))]
      # AMAP sans aucun partenariat : pas dans amap_prod_ids -> considerees "sans" la prod
      df <- df %>% filter(id_groupe %in% ids_ok | !(id_groupe %in% amap_prod_ids$id_groupe))
    }
    # === FIN MODIF (fix filtre prod AMAP) =====================================
    aesn_z <- aesn_z()
    if (aesn_z != "none" && !isTRUE(input$aesn_show_reste)) {
      col_typo <- if (aesn_z == "aac") "typo_aac" else "typo_zpa"
      df <- df %>% filter(.data[[col_typo]] == "oui")
    }
    z <- zone_filtre()
    if (!is.null(z)) df <- df %>% filter(as.numeric(st_distance(geometry, z$centre)) <= z$rayon)
    # === MODIF CLAUDE (filtre territorial) : intersection avec le territoire ===
    terr <- territoire_filtre()
    if (!is.null(terr)) {
      inter <- st_intersects(df, terr, sparse = FALSE)[, 1]
      df <- df[inter, ]
    }
    df
  })
  # === FIN MODIF (bug 1) ======================================================
  
  # === MODIF CLAUDE (bug 1) : surbrillance des resultats de recherche ========
  # IDs a surligner = resultats de recherche OU partenaires de l'entite cliquee.
  ids_recherche <- reactive({ ids_r() })
  
  output$compteur <- renderText({
    paste0("AMAP : ", nrow(amap_f()), " | Fermes : ", nrow(fermes_f()))
  })
  
  # === MODIF CLAUDE (panier) : toggle, retrait, vidage, affichage =============
  observeEvent(input$panier_toggle, {
    nm <- !panier_mode()
    panier_mode(nm)
    session$sendCustomMessage("mode_hint_msg",
                              list(text = if (nm) "Selection manuelle : cliquez les entites a isoler" else NULL))
  }, ignoreInit = TRUE)
  
  observeEvent(input$panier_remove, {
    key <- input$panier_remove
    p <- panier()
    p <- p[vapply(p, function(x) paste0(x$type, "_", x$id) != key, logical(1))]
    panier(p)
  }, ignoreInit = TRUE)
  
  observeEvent(input$panier_clear, { panier(list()) }, ignoreInit = TRUE)
  
  output$panier_ui <- renderUI({
    p <- panier(); actif <- panier_mode()
    etat <- if (actif) tags$div(style = "font-size:11px;color:#e6007e;font-weight:600;margin:4px 0;", "Selection ACTIVE")
    else tags$div(style = "font-size:11px;color:#999;margin:4px 0;", "Selection inactive")
    if (length(p) == 0)
      return(tagList(etat, tags$p(style = "font-size:11px;color:#999;font-style:italic;", "Aucune entite selectionnee")))
    tagList(
      etat,
      tags$div(style = "font-size:11px;font-weight:600;color:#e6007e;margin-bottom:2px;",
               paste0(length(p), " entite(s) selectionnee(s)")),
      lapply(p, function(w) {
        key <- paste0(w$type, "_", w$id)
        tags$div(class = "match_tab",
                 tags$span(class = "label", paste0(if (w$type == "ferme") "[F] " else "[A] ", w$nom)),
                 tags$span(class = "close",
                           onclick = paste0("Shiny.setInputValue('panier_remove','", key, "',{priority:'event'})"), "\u00D7"))
      }),
      tags$button(class = "btn_exp", style = "margin-top:4px;",
                  onclick = "Shiny.setInputValue('panier_clear',Math.random(),{priority:'event'})",
                  "Vider la selection")
    )
  })
  # === FIN MODIF (panier) =====================================================
  
  # === MODIF CLAUDE (B3) : encadre indicateurs (total / emprise carte) ========
  indic_scope <- reactiveVal("total")   # "total" ou "vue"
  observeEvent(input$indic_vue,   { indic_scope("vue") },   ignoreInit = TRUE)
  observeEvent(input$indic_total, { indic_scope("total") }, ignoreInit = TRUE)
  
  output$indicateurs_ui <- renderUI({
    af <- amap_f(); ff <- fermes_f()   # entites passant les filtres courants
    scope <- indic_scope()
    
    # Mode "vue" : restreindre a l'emprise carte au moment du calcul (rectangle)
    libelle_scope <- "Reseau filtre (total)"
    if (scope == "vue") {
      b <- isolate(input$carte_bounds)
      if (!is.null(b)) {
        in_box <- function(df) df %>% filter(lat >= b$south, lat <= b$north,
                                             lon >= b$west,  lon <= b$east)
        af <- in_box(af); ff <- in_box(ff)
        libelle_scope <- "Vue actuelle de la carte"
      }
    }
    
    n_amap <- nrow(af); n_ferme <- nrow(ff)
    # AAC / ZPA : "oui" parmi les entites du scope (fermes + AMAP)
    f_aac <- sum(ff$typo_aac == "oui", na.rm = TRUE); a_aac <- sum(af$typo_aac == "oui", na.rm = TRUE)
    f_zpa <- sum(ff$typo_zpa == "oui", na.rm = TRUE); a_zpa <- sum(af$typo_zpa == "oui", na.rm = TRUE)
    tot <- n_amap + n_ferme
    aac <- f_aac + a_aac; zpa <- f_zpa + a_zpa
    pct <- function(x) if (tot > 0) paste0(" (", round(100 * x / tot), "%)") else ""
    
    # Partenariats : ceux reliant une ferme ET une AMAP du scope
    n_part <- nrow(paires %>% filter(id_ferme %in% ff$id_ferme, id_groupe %in% af$id_groupe))
    
    row <- function(lab, val) tags$div(class = "indic_row", tags$span(lab), tags$span(class = "val", val))
    tagList(
      tags$div(class = "indic_scope", libelle_scope),
      row("AMAP", n_amap),
      row("Fermes", n_ferme),
      row("En AAC", paste0(aac, pct(aac))),
      row("En ZPA", paste0(zpa, pct(zpa))),
      row("Partenariats", n_part)
    )
  })
  # === FIN MODIF (B3) =========================================================
  
  # === MODIF CLAUDE (passe B1) : filtres actifs + reinitialisation ============
  # Recense les filtres ON sous forme de liste (clé interne -> libellé lisible).
  filtres_actifs_list <- reactive({
    L <- list()
    add <- function(key, lab) L[[length(L) + 1]] <<- list(key = key, lab = lab)
    if ((input$recherche %||% "") != "")            add("recherche", paste0("Recherche : ", input$recherche))
    if ((input$filtre_jour %||% "tous") != "tous")  add("filtre_jour", paste0("Jour : ", input$filtre_jour))
    if (!is.null(input$filtre_dep) && length(input$filtre_dep) > 0) {
      lab <- if (!is.null(input$filtre_com) && length(input$filtre_com) > 0)
        paste0(length(input$filtre_com), " commune(s)")
      else paste0("Dept ", paste(input$filtre_dep, collapse = ", "))
      add("territoire", paste0("Territoire : ", lab))
    }
    if ((input$filtre_statut %||% "tous") != "tous")   add("filtre_statut", paste0("Statut AMAP : ", input$filtre_statut))
    if ((input$filtre_statut_f %||% "tous") != "tous") add("filtre_statut_f", paste0("Statut ferme : ", input$filtre_statut_f))
    if ((input$filtre_prod %||% "toutes") != "toutes") {
      lbl <- ref_produits$label[ref_produits$id == suppressWarnings(as.integer(input$filtre_prod))]
      add("filtre_prod", paste0("Production : ", if (length(lbl)) lbl else input$filtre_prod))
    }
    if ((input$filtre_nprod %||% "toutes") != "toutes") {
      lbl <- ref_produits$label[ref_produits$id == suppressWarnings(as.integer(input$filtre_nprod))]
      add("filtre_nprod", paste0("Sans : ", if (length(lbl)) lbl else input$filtre_nprod))
    }
    if (aesn_z() != "none")    add("aesn_zone", paste0("Zone : ", toupper(aesn_z())))
    rayon <- suppressWarnings(as.numeric(input$rayon_km %||% ""))
    if (!is.na(rayon) && rayon > 0)                 add("rayon", paste0("Rayon : ", rayon, " km"))
    if (!is.null(sel()))                            add("selection", "Selection active")
    L
  })
  
  output$filtres_actifs_ui <- renderUI({
    L <- filtres_actifs_list()
    if (length(L) == 0) return(NULL)
    tagList(
      tags$div(class = "stitle", "Filtres actifs"),
      tags$div(style = "display:flex;flex-wrap:wrap;gap:4px;margin-bottom:6px;",
               lapply(L, function(f) {
                 tags$span(
                   style = "display:inline-flex;align-items:center;background:#2d5016;color:#fff;border-radius:12px;padding:2px 8px;font-size:11px;",
                   f$lab,
                   tags$span(style = "margin-left:6px;cursor:pointer;font-weight:bold;",
                             onclick = paste0("Shiny.setInputValue('filtre_remove','", f$key, "',{priority:'event'})"), "\u00D7"))
               })
      ),
      tags$button(class = "btn_exp",
                  style = "background:#c0392b;color:#fff;",
                  onclick = "Shiny.setInputValue('filtres_reset',Math.random(),{priority:'event'})",
                  "Tout reinitialiser")
    )
  })
  
  reset_un_filtre <- function(key) {
    switch(key,
           recherche       = updateTextInput(session, "recherche", value = ""),
           filtre_jour     = updateSelectInput(session, "filtre_jour", selected = "tous"),
           filtre_statut   = updateSelectInput(session, "filtre_statut", selected = "tous"),
           filtre_statut_f = updateSelectInput(session, "filtre_statut_f", selected = "tous"),
           filtre_prod     = updateSelectInput(session, "filtre_prod", selected = "toutes"),
           filtre_nprod    = updateSelectInput(session, "filtre_nprod", selected = "toutes"),
           aesn_zone       = { updateCheckboxInput(session, "show_aac", value = FALSE)
             updateCheckboxInput(session, "show_zpa", value = FALSE) },
           territoire      = { updateSelectInput(session, "filtre_dep", selected = character(0))
             updateSelectInput(session, "filtre_com", selected = character(0)) },
           rayon           = { updateTextInput(session, "rayon_km", value = ""); zone_center(NULL)
             session$sendCustomMessage("zone_badge_msg", list(text = NULL)) },
           selection       = fermer_detail()
    )
    # le champ recherche est un input HTML brut -> forcer la valeur cote JS aussi
    if (key == "recherche")
      session$sendCustomMessage("reset_recherche", list())
    if (key == "rayon")
      session$sendCustomMessage("reset_rayon", list())
  }
  
  observeEvent(input$filtre_remove, { reset_un_filtre(input$filtre_remove) }, ignoreInit = TRUE)
  
  observeEvent(input$filtres_reset, {
    updateSelectInput(session, "filtre_jour", selected = "tous")
    updateSelectInput(session, "filtre_statut", selected = "tous")
    updateSelectInput(session, "filtre_statut_f", selected = "tous")
    updateSelectInput(session, "filtre_prod", selected = "toutes")
    updateSelectInput(session, "filtre_nprod", selected = "toutes")
    updateCheckboxInput(session, "show_aac", value = FALSE)
    updateCheckboxInput(session, "show_zpa", value = FALSE)
    updateSelectInput(session, "filtre_dep", selected = character(0))
    updateSelectInput(session, "filtre_com", selected = character(0))
    updateTextInput(session, "recherche", value = "")
    updateTextInput(session, "rayon_km", value = "")
    zone_center(NULL)
    session$sendCustomMessage("zone_badge_msg", list(text = NULL))
    session$sendCustomMessage("reset_recherche", list())
    session$sendCustomMessage("reset_rayon", list())
    fermer_detail()   # === selection effacee (option b validee par l'utilisateur)
  }, ignoreInit = TRUE)
  # === FIN MODIF (passe B1) ===================================================
  
  # === MODIF CLAUDE (typo AESN) : compteur de volume en zone =================
  output$compteur_aesn <- renderText({
    z <- aesn_z()
    if (z == "none") return("")
    col <- if (z == "aac") "typo_aac" else "typo_zpa"
    nf <- sum(fermes[[col]] == "oui", na.rm = TRUE)
    na <- sum(amap[[col]]   == "oui", na.rm = TRUE)
    paste0("Volume ", toupper(z), " : ", nf, " fermes + ", na, " AMAP = ", nf + na, " entites")
  })
  # === FIN MODIF (typo AESN) =================================================
  
  # ===== MODE MISE EN RELATION : lancement requete =====
  observeEvent(input$match_run, {
    prod_id <- input$match_prod %||% ""
    rayon   <- input$match_rayon %||% 20
    if (prod_id == "" || is.na(rayon) || rayon <= 0) return()
    
    if (isTRUE(input$match_methode == "exist")) {
      ent_id <- input$match_entite %||% ""
      if (ent_id == "") return()
      if (input$match_type == "amap") {
        ref <- amap %>% filter(id_groupe == ent_id) %>% slice(1); if (nrow(ref) == 0) return()
        anchor_lat <- ref$lat; anchor_lon <- ref$lon; anchor_nom <- ref$nom_amap
        # Localisation exprimee en coordonnees : pas de commune ni de code postal.
        anchor_loc <- paste0(round(ref$lat, 4), ", ", round(ref$lon, 4))
      } else {
        ref <- fermes %>% filter(id_ferme == ent_id) %>% slice(1); if (nrow(ref) == 0) return()
        anchor_lat <- ref$lat; anchor_lon <- ref$lon; anchor_nom <- ref$nom_ferme
        anchor_loc <- paste0(round(ref$lat, 4), ", ", round(ref$lon, 4))
      }
    } else {
      pt <- match_point(); if (is.null(pt)) return()
      anchor_lat <- pt$lat; anchor_lon <- pt$lon; anchor_nom <- "Point libre"
      anchor_loc <- paste0(round(pt$lat, 4), ", ", round(pt$lon, 4))
    }
    
    centre <- st_sfc(st_point(c(anchor_lon, anchor_lat)), crs = 4326)
    rayon_m <- rayon * 1000
    
    amap_in_zone   <- amap_sf   %>% filter(as.numeric(st_distance(geometry, centre)) <= rayon_m)
    fermes_in_zone <- fermes_sf %>% filter(as.numeric(st_distance(geometry, centre)) <= rayon_m)
    
    amap_with_ids <- amap_prod_ids %>%
      filter(sapply(ids_prods_u, function(x) prod_id %in% unlist(strsplit(x, ",")), USE.NAMES = FALSE)) %>%
      pull(id_groupe)
    
    amap_avec <- amap_in_zone %>% filter(id_groupe %in% amap_with_ids)
    amap_sans <- amap_in_zone %>% filter(!id_groupe %in% amap_with_ids)
    
    jour_sel <- input$match_jour %||% "tous"
    if (jour_sel != "tous") {
      amap_avec <- amap_avec %>% filter(jour == jour_sel)
      amap_sans <- amap_sans %>% filter(jour == jour_sel)
    }
    
    if (isTRUE(input$match_methode == "exist") && input$match_type == "amap") {
      amap_avec <- amap_avec %>% filter(id_groupe != input$match_entite)
      amap_sans <- amap_sans %>% filter(id_groupe != input$match_entite)
    }
    
    fermes_prod <- fermes_in_zone %>%
      filter(sapply(ids_prod_raw, function(x) prod_id %in% unlist(strsplit(x, ",")), USE.NAMES = FALSE))
    if (isTRUE(input$match_methode == "exist") && input$match_type == "ferme") {
      fermes_prod <- fermes_prod %>% filter(id_ferme != input$match_entite)
    }
    
    lignes <- partenariats_actifs %>%
      filter(sapply(id_produits, function(x) prod_id %in% unlist(strsplit(x, "-")), USE.NAMES = FALSE)) %>%
      filter(id_groupe %in% amap_avec$id_groupe) %>%
      left_join(fermes %>% select(id_ferme, nom_ferme, lat_f = lat, lon_f = lon), by = "id_ferme") %>%
      left_join(amap %>% select(id_groupe, nom_amap, jour, lat_g = lat, lon_g = lon), by = "id_groupe") %>%
      filter(!is.na(lat_f), !is.na(lon_f), !is.na(lat_g), !is.na(lon_g)) %>%
      distinct(id_ferme, id_groupe, .keep_all = TRUE)
    
    fermes_hors_zone <- fermes %>%
      filter(id_ferme %in% lignes$id_ferme, !id_ferme %in% fermes_prod$id_ferme)
    
    source_partners <- data.frame(); source_lines <- data.frame()
    if (isTRUE(input$match_methode == "exist")) {
      ent_id <- input$match_entite
      if (input$match_type == "amap") {
        ids_partners <- partenariats_actifs %>% filter(id_groupe == ent_id) %>% pull(id_ferme) %>% unique()
        ids_to_show <- setdiff(ids_partners, c(fermes_prod$id_ferme, fermes_hors_zone$id_ferme))
        if (length(ids_to_show) > 0) {
          source_partners <- fermes %>% filter(id_ferme %in% ids_to_show) %>% mutate(part_type = "ferme")
          source_lines <- data.frame(lon_f = source_partners$lon, lat_f = source_partners$lat,
                                     lon_g = anchor_lon, lat_g = anchor_lat)
        }
      } else {
        ids_partners <- partenariats_actifs %>% filter(id_ferme == ent_id) %>% pull(id_groupe) %>% unique()
        ids_to_show <- setdiff(ids_partners, c(amap_avec$id_groupe, amap_sans$id_groupe))
        if (length(ids_to_show) > 0) {
          source_partners <- amap %>% filter(id_groupe %in% ids_to_show) %>% mutate(part_type = "amap")
          source_lines <- data.frame(lon_f = anchor_lon, lat_f = anchor_lat,
                                     lon_g = source_partners$lon, lat_g = source_partners$lat)
        }
      }
    }
    
    jitter_pts <- function(df, tol = 0.005) {
      if (nrow(df) == 0) return(df)
      key <- paste0(round(df$lon / tol) * tol, "_", round(df$lat / tol) * tol)
      tab <- table(key); grappes <- names(tab)[tab > 1]
      for (g in grappes) {
        idx <- which(key == g); n <- length(idx)
        rayon_jit <- 0.003
        angles <- seq(0, 2 * pi, length.out = n + 1)[1:n]
        cx <- mean(df$lon[idx]); cy <- mean(df$lat[idx])
        df$lon[idx] <- cx + rayon_jit * cos(angles)
        df$lat[idx] <- cy + rayon_jit * sin(angles)
      }
      df
    }
    
    all_pts <- bind_rows(
      amap_avec %>% st_drop_geometry() %>% transmute(lon, lat, src = "amap_avec", id = id_groupe),
      amap_sans %>% st_drop_geometry() %>% transmute(lon, lat, src = "amap_sans", id = id_groupe),
      fermes_prod %>% st_drop_geometry() %>% transmute(lon, lat, src = "fermes_prod", id = id_ferme),
      fermes_hors_zone %>% transmute(lon, lat, src = "fermes_hors_zone", id = id_ferme)
    )
    all_pts <- jitter_pts(all_pts)
    
    if (nrow(amap_avec) > 0) { m <- all_pts %>% filter(src == "amap_avec")
    amap_avec$lon <- m$lon[match(amap_avec$id_groupe, m$id)]; amap_avec$lat <- m$lat[match(amap_avec$id_groupe, m$id)] }
    if (nrow(amap_sans) > 0) { m <- all_pts %>% filter(src == "amap_sans")
    amap_sans$lon <- m$lon[match(amap_sans$id_groupe, m$id)]; amap_sans$lat <- m$lat[match(amap_sans$id_groupe, m$id)] }
    if (nrow(fermes_prod) > 0) { m <- all_pts %>% filter(src == "fermes_prod")
    fermes_prod$lon <- m$lon[match(fermes_prod$id_ferme, m$id)]; fermes_prod$lat <- m$lat[match(fermes_prod$id_ferme, m$id)] }
    if (nrow(fermes_hors_zone) > 0) { m <- all_pts %>% filter(src == "fermes_hors_zone")
    fermes_hors_zone$lon <- m$lon[match(fermes_hors_zone$id_ferme, m$id)]; fermes_hors_zone$lat <- m$lat[match(fermes_hors_zone$id_ferme, m$id)] }
    
    if (nrow(lignes) > 0) {
      fp <- bind_rows(
        if (nrow(fermes_prod) > 0) fermes_prod %>% st_drop_geometry() %>% transmute(id_ferme, lon_f_new = lon, lat_f_new = lat) else NULL,
        if (nrow(fermes_hors_zone) > 0) fermes_hors_zone %>% transmute(id_ferme, lon_f_new = lon, lat_f_new = lat) else NULL
      )
      if (nrow(fp) > 0) {
        lignes$lon_f <- fp$lon_f_new[match(lignes$id_ferme, fp$id_ferme)] %||% lignes$lon_f
        lignes$lat_f <- fp$lat_f_new[match(lignes$id_ferme, fp$id_ferme)] %||% lignes$lat_f
      }
      if (nrow(amap_avec) > 0) {
        ap <- amap_avec %>% st_drop_geometry() %>% transmute(id_groupe, lon_g_new = lon, lat_g_new = lat)
        lignes$lon_g <- ap$lon_g_new[match(lignes$id_groupe, ap$id_groupe)] %||% lignes$lon_g
        lignes$lat_g <- ap$lat_g_new[match(lignes$id_groupe, ap$id_groupe)] %||% lignes$lat_g
      }
    }
    
    match_result(list(
      type    = input$match_type,
      anchor  = list(lat = anchor_lat, lon = anchor_lon, nom = anchor_nom, loc = anchor_loc,
                     methode = input$match_methode,
                     ent_id = if (input$match_methode == "exist") input$match_entite else NA),
      prod    = ref_produits$label[ref_produits$id == as.integer(prod_id)],
      prod_id = prod_id, rayon = rayon, centre = centre,
      rayon_mut = isolate(input$match_rayon_mut) %||% 0,   # === MODIF (bug 6) : stocke pour l'export
      amap_avec = amap_avec, amap_sans = amap_sans,
      fermes_prod = fermes_prod, fermes_hors_zone = fermes_hors_zone,
      source_partners = source_partners, source_lines = source_lines, lignes = lignes
    ))
    
    rebuild_sf <- function(df) {
      if (nrow(df) == 0) return(df)
      st_as_sf(st_drop_geometry(df), coords = c("lon","lat"), crs = 4326, remove = FALSE)
    }
    amap_avec   <- rebuild_sf(amap_avec); amap_sans <- rebuild_sf(amap_sans)
    fermes_prod <- rebuild_sf(fermes_prod)
    if (nrow(fermes_hors_zone) > 0) fermes_hors_zone <- st_as_sf(fermes_hors_zone, coords = c("lon","lat"), crs = 4326, remove = FALSE)
    
    match_saved_active(NULL)
    leafletProxy("carte", session) %>% setView(anchor_lon, anchor_lat, zoom = 11)
  }, ignoreInit = TRUE)
  
  output$compteur_match <- renderText({
    r <- match_result()
    if (is.null(r)) return("Lancez une requete pour voir les resultats")
    paste0("AMAP avec : ", nrow(r$amap_avec), " | AMAP sans : ", nrow(r$amap_sans), " | Fermes : ", nrow(r$fermes_prod))
  })
  
  # ===== Carte =====
  output$carte <- renderLeaflet({
    leaflet() %>%
      # Fonds sans authentification uniquement : CartoDB Positron, utilise dans
      # la version interne, exige desormais une cle d'API. Un depot public ne
      # doit contenir aucune cle, donc on s'en passe.
      addProviderTiles(providers$OpenStreetMap,      group = "OSM") %>%
      addProviderTiles(providers$OpenTopoMap,        group = "Relief") %>%
      addProviderTiles(providers$Esri.WorldImagery,  group = "Satellite") %>%
      addPolygons(data = idf_sf, fill = FALSE, color = "#2d5016", weight = 2, opacity = 0.6, group = "idf") %>%
      setView(2.35, 48.85, zoom = 9) %>%
      addLayersControl(baseGroups = c("OSM","Relief","Satellite"), options = layersControlOptions(collapsed = TRUE)) %>%
      addScaleBar(position = "bottomleft")
  })
  
  observe({
    # La carte doit exister avant qu'un leafletProxy puisse l'atteindre.
    # Sous WebAssembly (shinylive), cet observateur se declenche avant que le
    # widget leaflet soit initialise : les mises a jour partaient dans le vide
    # et aucun marqueur ne s'affichait. input$carte_bounds n'est emis qu'une
    # fois la carte dessinee : on s'en sert comme signal de disponibilite.
    req(input$carte_bounds)

    proxy <- leafletProxy("carte", session)
    proxy %>% clearGroup("fermes") %>% clearGroup("amap") %>%
      clearGroup("zone_c") %>% clearGroup("aac") %>% clearGroup("zpa") %>%
      clearGroup("territoire") %>% clearGroup("panier") %>%
      clearGroup("match") %>% clearGroup("match_lignes") %>%
      clearGroup("match_anchor") %>% clearGroup("match_point_libre") %>%
      clearGroup("match_source") %>% clearGroup("match_source_lignes") %>%
      clearGroup("itineraire") %>% clearGroup("itin_waypoints") %>% clearGroup("mutualisation") %>%
      clearGroup("mutualisation_fermes") %>%
      removeControl("leg")
    
    # === MODIF CLAUDE (passe B2) : couche AAC dessinee selon le radio aesn_zone
    if (isTRUE(aesn_z() == "aac"))
      proxy %>% addPolygons(data = aac_sf, group = "aac", color = "#4a9fd4", weight = 0,
                            fillColor = "#4a9fd4", fillOpacity = 0.2, label = ~Nom_AAC, options = pathOptions(interactive = FALSE))
    
    # ====== MODE EXPLORATION ======
    if (isTRUE(input$mode_app == "explo")) {
      ff <- fermes_f(); af <- amap_f(); s <- sel()
      rech <- ids_recherche()   # === MODIF (bug 1) : pour surbrillance
      
      zc <- zone_center(); rayon <- suppressWarnings(as.numeric(input$rayon_km %||% ""))
      if (!is.null(zc) && !is.na(rayon) && rayon > 0)
        proxy %>% addCircles(lng = zc$lon, lat = zc$lat, radius = rayon * 1000,
                             color = "#2d5016", weight = 2, fillColor = "#2d5016", fillOpacity = .08,
                             group = "zone_c", options = pathOptions(interactive = FALSE))
      
      # === MODIF CLAUDE (filtre territorial) : contour du territoire choisi =====
      terr <- territoire_filtre()
      if (!is.null(terr))
        proxy %>% addPolygons(data = terr, fill = FALSE, color = "#2d5016",
                              weight = 2.5, opacity = 0.9, group = "territoire",
                              options = pathOptions(interactive = FALSE))
      # === FIN MODIF (filtre territorial) ======================================
      
      aesn_actif <- isTRUE(aesn_z() != "none")
      col_typo <- if (isTRUE(aesn_z() == "aac")) "typo_aac"
      else if (isTRUE(aesn_z() == "zpa")) "typo_zpa" else NULL
      pal_f <- c(oui = "#1a5c00", non = "#cccccc")  # === MODIF (typo AESN) : binaire
      pal_a <- c(oui = "#994d00", non = "#cccccc")  # === MODIF (typo AESN) : binaire
      
      # === MODIF (bug 1) : IDs a surligner = partenaires de l'entite cliquee
      #     OU resultats de recherche. La recherche ne masque plus rien,
      #     elle met seulement en evidence.
      ids_f_hi <- if (!is.null(s) && s$type == "ferme") s$id
      else if (!is.null(s)) paires$id_ferme[paires$id_groupe == s$id]
      else if (!is.null(rech)) rech$f
      else character(0)
      ids_a_hi <- if (!is.null(s) && s$type == "amap") s$id
      else if (!is.null(s)) paires$id_groupe[paires$id_ferme == s$id]
      else if (!is.null(rech)) rech$a
      else character(0)
      
      # === MODIF CLAUDE (passe A) : rendu unifie fermes ======================
      # Une seule passe : couleur FIXE (vert), opacite modulee si filtre AESN
      # actif (hors-zone = translucide), surbrillance clic/recherche par-dessus.
      # Plus de branches exclusives -> le clic/pop-up marche meme en mode AAC/ZPA.
      if (isTRUE(input$show_fermes) && nrow(ff) > 0) {
        # === MODIF CLAUDE (clip) : si zone active, les "non" (affichees seulement
        # quand "afficher le reste" est coche) passent en GRIS + transparent.
        if (aesn_actif) {
          est_oui <- ff[[col_typo]] == "oui"
          op <- ifelse(est_oui, 1, 0.4)
          ff$lab <- paste0(ff$nom_ferme, ifelse(est_oui, " (en zone)", ""))
        } else { est_oui <- rep(TRUE, nrow(ff)); op <- rep(1, nrow(ff)); ff$lab <- ff$nom_ferme }
        hi_mask <- ff$id_ferme %in% ids_f_hi
        has_hi  <- length(ids_f_hi) > 0
        size_v   <- ifelse(hi_mask, 20, 12)
        border_v <- ifelse(hi_mask, "#000", ifelse(est_oui, "#1a5c00", "#999999"))
        bw_v     <- ifelse(hi_mask, 1.5, 0.75)
        fill_v   <- ifelse(hi_mask, "#00cc44", ifelse(est_oui, "#33CC00", "#bbbbbb"))
        op <- ifelse(hi_mask, 1, ifelse(rep(has_hi, nrow(ff)), pmin(op, 0.55), op))
        proxy %>% addMarkers(data = ff, lng = ~lon, lat = ~lat,
                             icon = losange_icons_op(fill = fill_v, border = border_v, size = size_v,
                                                     border_w = bw_v, opacity = op),
                             label = ~lab, layerId = ~paste0("ferme_", id_ferme), group = "fermes")
      }
      # === FIN MODIF (passe A) ===============================================
      
      # === MODIF CLAUDE (passe A) : rendu unifie AMAP ========================
      if (isTRUE(input$show_amap) && nrow(af) > 0) {
        if (aesn_actif) {
          est_oui <- af[[col_typo]] == "oui"
          op <- ifelse(est_oui, 0.9, 0.4)
          af$lab <- paste0(af$nom_amap, ifelse(est_oui, " (liee zone)", ""))
        } else { est_oui <- rep(TRUE, nrow(af)); op <- rep(0.85, nrow(af)); af$lab <- af$nom_amap }
        hi_mask <- af$id_groupe %in% ids_a_hi
        has_hi  <- length(ids_a_hi) > 0
        af$fillc  <- ifelse(hi_mask, "#ff6600", ifelse(est_oui, "#FF8000", "#bbbbbb"))
        af$strokec<- ifelse(hi_mask, "#000", ifelse(est_oui, "#994d00", "#999999"))
        af$radius <- ifelse(hi_mask, 13, 6)
        af$weight <- ifelse(hi_mask, 2.5, 1.5)
        af$op     <- ifelse(hi_mask, 1, ifelse(rep(has_hi, nrow(af)), pmin(op, 0.45), op))
        proxy %>% addCircleMarkers(data = af, lng = ~lon, lat = ~lat,
                                   color = ~strokec, fillColor = ~fillc, fillOpacity = ~op,
                                   radius = ~radius, weight = ~weight,
                                   label = ~lab, layerId = ~paste0("amap_", id_groupe), group = "amap")
      }
      # === FIN MODIF (passe A) ===============================================
      
      # === MODIF CLAUDE (panier) : entites selectionnees en magenta ==========
      dessiner_panier(proxy)
      # === FIN MODIF (panier) ================================================
      
      
    }
    
    # ====== MODE MISE EN RELATION ======
    if (isTRUE(input$mode_app == "match")) {
      r <- match_result(); pt_libre <- match_point()
      if (isTRUE(input$match_methode == "libre") && !is.null(pt_libre) && is.null(r)) {
        proxy %>% addCircleMarkers(lng = pt_libre$lon, lat = pt_libre$lat,
                                   color = "#000", fillColor = "#ffeb3b", fillOpacity = 1, radius = 8, weight = 2,
                                   label = "Point d'ancrage", group = "match_point_libre")
      }
      if (!is.null(r)) {
        proxy %>% addCircles(lng = r$anchor$lon, lat = r$anchor$lat, radius = r$rayon * 1000,
                             color = "#2d5016", weight = 2, fillColor = "#2d5016", fillOpacity = .05,
                             group = "match", options = pathOptions(interactive = FALSE))
        
        if (r$type == "amap") {
          col_avec_fill <- "#4a9fd4"; col_avec_stroke <- "#1a6fa8"
          col_sans_fill <- "#FF8000"; col_sans_stroke <- "#994d00"
          ferme_fill <- "#33CC00"; ferme_color <- "#1a5c00"
        } else {
          col_avec_fill <- "#FF8000"; col_avec_stroke <- "#994d00"
          col_sans_fill <- "#4a9fd4"; col_sans_stroke <- "#1a6fa8"
          ferme_fill <- "#e74c3c"; ferme_color <- "#c0392b"
        }
        
        if (nrow(r$fermes_prod) > 0)
          proxy %>% addMarkers(data = r$fermes_prod, lng = ~lon, lat = ~lat,
                               icon = losange_icons(fill = ferme_fill, border = ferme_color, size = 12),
                               label = ~nom_ferme, layerId = ~paste0("ferme_", id_ferme), group = "match")
        if (nrow(r$fermes_hors_zone) > 0)
          proxy %>% addMarkers(data = r$fermes_hors_zone, lng = ~lon, lat = ~lat,
                               icon = losange_icons(fill = ferme_fill, border = ferme_color, size = 10),
                               label = ~paste0(nom_ferme, " (hors zone)"), layerId = ~paste0("ferme_", id_ferme), group = "match")
        if (nrow(r$amap_avec) > 0)
          proxy %>% addCircleMarkers(data = r$amap_avec, lng = ~lon, lat = ~lat,
                                     color = col_avec_stroke, fillColor = col_avec_fill, fillOpacity = .9, radius = 7, weight = 1.5,
                                     label = ~nom_amap, layerId = ~paste0("amap_", id_groupe), group = "match")
        if (nrow(r$amap_sans) > 0)
          proxy %>% addCircleMarkers(data = r$amap_sans, lng = ~lon, lat = ~lat,
                                     color = col_sans_stroke, fillColor = col_sans_fill, fillOpacity = .55, radius = 6, weight = 1,
                                     label = ~nom_amap, layerId = ~paste0("amap_", id_groupe), group = "match")
        
        rayon_mut <- isolate(input$match_rayon_mut) %||% 0
        if (!is.na(rayon_mut) && rayon_mut > 0) {
          fermes_mut <- bind_rows(
            if (nrow(r$fermes_prod) > 0) r$fermes_prod %>% st_drop_geometry() %>% select(lon, lat) else NULL,
            if (!is.null(r$fermes_hors_zone) && nrow(r$fermes_hors_zone) > 0)
              (if (inherits(r$fermes_hors_zone,"sf")) st_drop_geometry(r$fermes_hors_zone) else r$fermes_hors_zone) %>% select(lon, lat) else NULL,
            if (r$type == "amap" && !is.null(r$source_partners) && nrow(r$source_partners) > 0)
              r$source_partners %>% select(lon, lat) else NULL
          )
          if (nrow(fermes_mut) > 0) {
            proxy %>% addCircles(lng = fermes_mut$lon, lat = fermes_mut$lat, radius = rayon_mut * 1000,
                                 color = "#8e44ad", weight = 1.5, fillColor = "#8e44ad", fillOpacity = 0.08,
                                 group = "mutualisation", options = pathOptions(interactive = FALSE))
            centres_sf <- st_as_sf(fermes_mut, coords = c("lon","lat"), crs = 4326)
            dists <- st_distance(fermes_sf, centres_sf); min_dist <- apply(dists, 1, min)
            fermes_in_mut <- fermes_sf[as.numeric(min_dist) <= rayon_mut * 1000, ]
            ids_deja <- c(r$fermes_prod$id_ferme,
                          if (!is.null(r$fermes_hors_zone)) r$fermes_hors_zone$id_ferme,
                          if (!is.null(r$source_partners) && r$type == "amap") r$source_partners$id_ferme)
            fermes_in_mut <- fermes_in_mut %>% filter(!id_ferme %in% ids_deja)
            if (nrow(fermes_in_mut) > 0)
              proxy %>% addMarkers(data = fermes_in_mut, lng = ~lon, lat = ~lat,
                                   icon = losange_icons(fill = "#d7bde2", border = "#8e44ad", size = 10),
                                   label = ~paste0(nom_ferme, " (dans zone mutualisation)"),
                                   layerId = ~paste0("ferme_", id_ferme), group = "mutualisation_fermes")
          }
        }
        
        if (!is.null(r$source_lines) && nrow(r$source_lines) > 0) {
          for (i in seq_len(nrow(r$source_lines))) {
            sl <- r$source_lines[i, ]
            proxy <- proxy %>% addPolylines(lng = c(sl$lon_f, sl$lon_g), lat = c(sl$lat_f, sl$lat_g),
                                            color = "#666666", weight = 1.5, opacity = .6, dashArray = "5,5", group = "match_source_lignes")
          }
        }
        if (!is.null(r$source_partners) && nrow(r$source_partners) > 0) {
          if (r$type == "amap")
            proxy %>% addMarkers(data = r$source_partners, lng = ~lon, lat = ~lat,
                                 icon = losange_icons(fill = "#cccccc", border = "#444444", size = 10),
                                 label = ~paste0(nom_ferme, " (partenaire actuel)"),
                                 layerId = ~paste0("ferme_", id_ferme), group = "match_source")
          else
            proxy %>% addCircleMarkers(data = r$source_partners, lng = ~lon, lat = ~lat,
                                       color = "#444444", fillColor = "#cccccc", fillOpacity = .7, radius = 5, weight = 1.5,
                                       label = ~paste0(nom_amap, " (partenaire actuel)"),
                                       layerId = ~paste0("amap_", id_groupe), group = "match_source")
        }
        
        if (isTRUE(input$match_show_lignes) && nrow(r$lignes) > 0) {
          for (i in seq_len(nrow(r$lignes))) {
            li <- r$lignes[i, ]
            col_line <- unname(palette_jours[li$jour]); if (is.na(col_line)) col_line <- "#999999"
            proxy <- proxy %>% addPolylines(lng = c(li$lon_f, li$lon_g), lat = c(li$lat_f, li$lat_g),
                                            color = col_line, weight = 2.5, opacity = .7, group = "match_lignes")
          }
        }
        
        anchor_color <- if (r$type == "amap") "#FF8000" else "#33CC00"
        anchor_stroke <- if (r$type == "amap") "#994d00" else "#1a5c00"
        anchor_layer <- if (!is.na(r$anchor$ent_id)) paste0(if (r$type == "amap") "amap_" else "ferme_", r$anchor$ent_id) else NULL
        if (r$type == "amap")
          proxy %>% addCircleMarkers(lng = r$anchor$lon, lat = r$anchor$lat,
                                     color = anchor_stroke, fillColor = anchor_color, fillOpacity = 1, radius = 13, weight = 3,
                                     label = r$anchor$nom, layerId = anchor_layer, group = "match_anchor")
        else
          proxy %>% addMarkers(lng = r$anchor$lon, lat = r$anchor$lat,
                               icon = losange_icons(fill = anchor_color, border = "#000", size = 18, border_w = 1.5),
                               label = r$anchor$nom, layerId = anchor_layer, group = "match_anchor")
        
        picto_amap <- function(col, stroke = "") {
          border <- if (stroke != "") paste0("border:1.5px solid ", stroke, ";") else ""
          sprintf("<span style='display:inline-block;width:16px;height:16px;vertical-align:middle;text-align:center;margin-right:6px;'><span style='display:inline-block;width:12px;height:12px;border-radius:50%%;background:%s;%svertical-align:middle;'></span></span>", col, border)
        }
        picto_ferme <- function(col, stroke = "#1a5c00", size = 10) {
          sprintf("<span style='display:inline-block;width:16px;height:16px;vertical-align:middle;text-align:center;margin-right:6px;'><span style='display:inline-block;width:%dpx;height:%dpx;background:%s;border:1px solid %s;transform:rotate(45deg);vertical-align:middle;'></span></span>", size, size, col, stroke)
        }
        if (r$type == "amap") {
          leg_html <- paste0("<div style='line-height:1.8'>",
                             picto_amap("#FF8000", "#000"), "AMAP source<br>",
                             picto_amap("#4a9fd4"), "AMAP partenaire<br>",
                             picto_amap("#FF8000"), "AMAP libre<br>",
                             picto_ferme("#33CC00"), "Ferme potentielle", "</div>")
        } else {
          leg_html <- paste0("<div style='line-height:1.8'>",
                             picto_ferme("#33CC00", "#000", size = 14), "Ferme source<br>",
                             picto_amap("#FF8000"), "AMAP d\u00e9j\u00e0 prise<br>",
                             picto_amap("#4a9fd4"), "AMAP cible<br>",
                             picto_ferme("#e74c3c"), "Ferme concurrente", "</div>")
        }
        if (!is.null(r$source_partners) && nrow(r$source_partners) > 0) {
          if (r$type == "amap") { lab_part <- "Ferme partenaire actuelle"; picto_p <- picto_ferme("#cccccc", "#444") }
          else { lab_part <- "AMAP partenaire actuelle"; picto_p <- picto_amap("#cccccc", "#444") }
          leg_html <- paste0(leg_html, "<div style='margin-top:6px;padding-top:6px;border-top:1px solid #ddd;line-height:1.8'>",
                             picto_p, lab_part, "<br>",
                             "<span style='display:inline-block;width:18px;height:0;border-top:1.5px dashed #666;margin-right:6px;vertical-align:middle'></span>Lien partenariat", "</div>")
        }
        if (isTRUE(input$match_show_lignes) && nrow(r$lignes) > 0) {
          jours_lignes <- ordre_jours[ordre_jours %in% unique(r$lignes$jour)]
          if (length(jours_lignes) > 0) {
            lignes_html <- paste0("<span style='display:inline-block;width:18px;height:3px;background:",
                                  unname(palette_jours[jours_lignes]), ";margin-right:6px;vertical-align:middle'></span>", jours_lignes, collapse = "<br>")
            leg_html <- paste0(leg_html, "<div style='margin-top:6px;padding-top:6px;border-top:1px solid #ddd;line-height:1.6'>",
                               "<b style='font-size:10px;color:#888'>LIVRAISON</b><br>", lignes_html, "</div>")
          }
        }
        proxy %>% addControl(html = leg_html, position = "bottomright", layerId = "leg")
      }
      # === MODIF CLAUDE (panier) : panier visible aussi en mode mise en relation
      dessiner_panier(proxy)
      # === FIN MODIF (panier) ================================================
    }
    
    # === MODIF CLAUDE (passe B2) : couche ZPA dessinee selon le radio aesn_zone
    if (isTRUE(aesn_z() == "zpa")) {
      proxy %>% addPolygons(data = zpa_sf, group = "zpa", color = "#a0522d", weight = 2.5,
                            fillColor = "#a0522d", fillOpacity = 0, label = ~territoire, options = pathOptions(interactive = FALSE))
    }
    
    i <- itin_result()
    if (!is.null(i) && !is.null(i$coords) && nrow(i$coords) > 0) {
      proxy %>% addPolylines(lng = i$coords[, 1], lat = i$coords[, 2],
                             color = "#4285f4", weight = 5, opacity = 0.85, group = "itineraire")
    }
    
    # === MODIF CLAUDE (bug 8) : marqueurs numerotes des etapes choisies ========
    if (isTRUE(input$mode_app == "match") && isTRUE(input$itin_mode == "etapes")) {
      wp <- itin_waypoints()
      if (length(wp) > 0) {
        for (k in seq_along(wp)) {
          w <- wp[[k]]
          ic <- leaflet::makeIcon(
            iconUrl = paste0("data:image/svg+xml;utf8,",
                             URLencode(sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="26" height="26"><circle cx="13" cy="13" r="11" fill="#4285f4" stroke="#fff" stroke-width="2"/><text x="13" y="17" font-size="13" fill="#fff" text-anchor="middle" font-family="sans-serif" font-weight="bold">%d</text></svg>', k), reserved = TRUE)),
            iconWidth = 26, iconHeight = 26, iconAnchorX = 13, iconAnchorY = 13)
          proxy %>% addMarkers(lng = w$lon, lat = w$lat, icon = ic,
                               label = paste0("Etape ", k, " : ", w$nom),
                               layerId = paste0(w$type, "_", w$id), group = "itin_waypoints")
        }
      }
    }
    # === FIN MODIF (bug 8) =====================================================
  })
  
  # === MODIF CLAUDE (bug 7) : diagnostic de visibilite d'un partenaire ========
  # Pour une entite donnee (partenaire affiche dans le panneau), indique si elle
  # est reellement dessinee sur la carte, et sinon POURQUOI (filtre, hors rayon,
  # couche masquee, coords manquantes). Non destructif : n'affiche rien sur la
  # carte, ajoute juste une ligne de statut dans le panneau de droite.
  statut_visibilite <- function(type, id) {
    mode <- input$mode_app %||% "explo"
    base_tbl <- if (type == "amap") amap else fermes
    idcol    <- if (type == "amap") "id_groupe" else "id_ferme"
    if (!(id %in% base_tbl[[idcol]]))
      return(list(sym = "\u2717", txt = "coordonnees manquantes (entite non cartographiable)", col = "#c0392b"))
    
    if (mode == "explo") {
      couche_on <- if (type == "amap") isTRUE(input$show_amap) else isTRUE(input$show_fermes)
      if (!couche_on)
        return(list(sym = "\u26A0", txt = "couche masquee (case decochee)", col = "#e67e22"))
      ids_vis <- if (type == "amap") amap_f()$id_groupe else fermes_f()$id_ferme
      if (id %in% ids_vis)
        return(list(sym = "\u2713", txt = "visible sur la carte", col = "#1a7a1a"))
      return(list(sym = "\u26A0", txt = "masquee par un filtre ou une zone", col = "#e67e22"))
    } else {
      r <- match_result()
      if (is.null(r)) return(list(sym = "\u2014", txt = "aucune recherche active", col = "#999"))
      ids_vis <- if (type == "amap")
        c(r$amap_avec$id_groupe, r$amap_sans$id_groupe,
          if (!is.null(r$source_partners) && r$type == "ferme") r$source_partners$id_groupe)
      else
        c(r$fermes_prod$id_ferme,
          if (!is.null(r$fermes_hors_zone)) r$fermes_hors_zone$id_ferme,
          if (!is.null(r$source_partners) && r$type == "amap") r$source_partners$id_ferme)
      if (id %in% ids_vis)
        return(list(sym = "\u2713", txt = "visible sur la carte", col = "#1a7a1a"))
      return(list(sym = "\u26A0", txt = "hors rayon ou hors criteres de la recherche", col = "#e67e22"))
    }
  }
  # === FIN MODIF (bug 7) ======================================================
  
  # ===== Panneau detail =====
  output$detail_content <- renderUI({
    s <- sel(); if (is.null(s)) return(NULL)
    if (s$type == "ferme") {
      f <- fermes %>% filter(id_ferme == s$id) %>% slice(1); if (nrow(f) == 0) return(NULL)
      ids_a <- unique(paires$id_groupe[paires$id_ferme == s$id])
      parts <- amap %>% filter(id_groupe %in% ids_a)
      tagList(
        tags$div(class = "pr_sec", "Informations"),
        if (f$statut_ferme != "")
          tags$p(class = "info_row", tags$span(class = "info_label", "Statut : "), f$statut_ferme),
        if (f$certif != "")
          tags$p(class = "info_row", tags$span(class = "info_label", "Certif : "), f$certif),
        if (f$annee_amap != "")
          tags$p(class = "info_row", tags$span(class = "info_label", "En AMAP depuis : "), f$annee_amap),
        if (f$productions_txt != "")
          tagList(tags$div(class = "pr_sec", "Productions"), tags$p(class = "info_row", f$productions_txt)),
        if (f$non_prod_txt != "")
          tagList(tags$div(class = "pr_sec", "Productions absentes"), tags$div(class = "nonprod", f$non_prod_txt)),
        tags$div(class = "pr_sec", paste0("AMAP partenaires (", nrow(parts), ")")),
        if (nrow(parts) > 0)
          tagList(lapply(seq_len(nrow(parts)), function(i) {
            p <- parts[i, ]
            hor <- if (p$h_debut != "") paste0(p$h_debut, if (p$duree != "") paste0(" - ", p$duree)) else ""
            st <- statut_visibilite("amap", p$id_groupe)   # === MODIF (bug 7)
            tags$div(class = "pcard", tags$b(p$nom_amap),
                     tags$span(class = "sub",
                               paste0(p$jour, if (nchar(hor) > 0) paste0(" | ", hor))),
                     tags$div(style = paste0("font-size:10px;margin-top:3px;color:", st$col, ";"),
                              st$sym, " ", st$txt))   # === MODIF (bug 7)
          }))
        else tags$p(style = "color:#aaa;font-size:12px;", "Aucun partenariat")
      )
    } else {
      a <- amap %>% filter(id_groupe == s$id) %>% slice(1); if (nrow(a) == 0) return(NULL)
      ids_f <- unique(paires$id_ferme[paires$id_groupe == s$id])
      parts <- fermes %>% filter(id_ferme %in% ids_f)
      hor_a <- if (a$h_debut != "") paste0(a$h_debut, if (a$duree != "") paste0(" (", a$duree, ")")) else ""
      tagList(
        tags$div(class = "pr_sec", "Informations"),
        # Lieu de distribution, adresse et contacts retires : absents de la
        # donnee comme du code.
        tags$p(class = "info_row", tags$span(class = "info_label", "Distribution : "),
               paste0(a$jour, if (nchar(hor_a) > 0) paste0(" | ", hor_a))),
        if (a$statut_amap != "")
          tags$p(class = "info_row", tags$span(class = "info_label", "Statut : "), a$statut_amap),
        if (!is.na(a$nb_adh))
          tags$p(class = "info_row", tags$span(class = "info_label", "Adherent.es : "), a$nb_adh),
        if (!is.na(a$prod_presentes_txt) && a$prod_presentes_txt != "")
          tagList(tags$div(class = "pr_sec", "Productions presentes"), tags$p(class = "info_row", a$prod_presentes_txt)),
        if (!is.na(a$prod_absentes_txt) && a$prod_absentes_txt != "")
          tagList(tags$div(class = "pr_sec", "Productions absentes"), tags$div(class = "nonprod", a$prod_absentes_txt)),
        tags$div(class = "pr_sec", paste0("Fermes partenaires (", nrow(parts), ")")),
        if (nrow(parts) > 0)
          tagList(lapply(seq_len(nrow(parts)), function(i) {
            p <- parts[i, ]
            st <- statut_visibilite("ferme", p$id_ferme)   # === MODIF (bug 7)
            tags$div(class = "pcard ferme", tags$b(p$nom_ferme),
                     tags$span(class = "sub",
                               if (p$productions_txt != "") p$productions_txt),
                     tags$div(style = paste0("font-size:10px;margin-top:3px;color:", st$col, ";"),
                              st$sym, " ", st$txt))   # === MODIF (bug 7)
          }))
        else tags$p(style = "color:#aaa;font-size:12px;", "Aucun partenariat")
      )
    }
  })
  
  # ===== Exports =====
  # === MODIF CLAUDE (bug 4) : si une entite est selectionnee (clic), l'export ==
  # ne contient QUE cette entite + ses partenaires. Sinon, tout le visible.
  output$dl_sel <- downloadHandler(
    filename = function() paste0("selection_amap_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content  = function(file) {
      tryCatch({
        s <- sel()
        if (!is.null(s)) {
          if (s$type == "ferme") {
            ids_a <- unique(paires$id_groupe[paires$id_ferme == s$id])
            fsub <- fermes_sf %>% filter(id_ferme == s$id)
            asub <- amap_sf   %>% filter(id_groupe %in% ids_a)
          } else {
            ids_f <- unique(paires$id_ferme[paires$id_groupe == s$id])
            asub <- amap_sf   %>% filter(id_groupe == s$id)
            fsub <- fermes_sf %>% filter(id_ferme %in% ids_f)
          }
          tmp <- generer_excel_selection(fsub, asub, panier_data = panier())
        } else {
          tmp <- generer_excel_selection(fermes_f(), amap_f(), panier_data = panier())
        }
        file.copy(tmp, file)
      }, error = function(e) {
        showNotification(paste("Export echoue :", conditionMessage(e)), type = "error", duration = 8)
      })
    }
  )
  # === FIN MODIF (bug 4) ======================================================
  
  output$dl_match <- downloadHandler(
    filename = function() paste0("requete_amap_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content  = function(file) {
      r <- match_result()
      if (is.null(r)) { showNotification("Aucune requete a exporter", type = "warning", duration = 5); return() }
      tryCatch({
        rayon_mut <- r$rayon_mut %||% 0
        params <- list(
          type = if (r$type == "amap") "AMAP" else "Ferme",
          nom = r$anchor$nom, loc = r$anchor$loc, prod = r$prod,
          prod_id = r$prod_id %||% "", rayon = r$rayon, rayon_mut = rayon_mut
        )
        # === MODIF CLAUDE (bug 6) : calcul de la feuille mutualisation =========
        mutu <- tryCatch(calc_mutualisation(r, rayon_mut), error = function(e) NULL)
        tmp <- generer_excel_requete(params, r$amap_avec, r$amap_sans, r$fermes_prod, r$lignes,
                                     mutualisation = mutu)
        file.copy(tmp, file)
      }, error = function(e) {
        showNotification(paste("Export echoue :", conditionMessage(e)), type = "error", duration = 8)
      })
    }
  )
  
  # ===== MODAL EXPORT IMAGE =====
  observeEvent(input$open_export_modal, {
    mode_ctx <- input$open_export_modal
    if (mode_ctx == "match" && is.null(match_result())) {
      showNotification("Lancez d'abord une recherche.", type = "warning", duration = 4); return()
    }
    if (mode_ctx == "explo" && nrow(fermes_f()) == 0 && nrow(amap_f()) == 0) {
      showNotification("Aucune donnee a exporter.", type = "warning", duration = 4); return()
    }
    titre_defaut <- if (mode_ctx == "match") { r <- match_result(); paste0("Mise en relation : ", r$prod) } else "Carte AMAP Ile-de-France"
    showModal(modalDialog(
      title = "Exporter en image", easyClose = TRUE,
      footer = tagList(modalButton("Annuler"), downloadButton("dl_image", "Generer l'image", class = "btn_action")),
      textInput("export_titre", "Titre de la carte", value = titre_defaut),
      checkboxInput("export_legende",  "Afficher la legende",  value = TRUE),
      checkboxInput("export_communes", "Afficher les communes (rendu plus long)", value = FALSE),
      checkboxInput("export_fond_osm", "Afficher le fond de carte (necessite internet)", value = FALSE),
      radioButtons("export_taille", "Taille",
                   choices = c("Petit (A5)" = "A5", "Standard (A4)" = "A4", "Grand (A3)" = "A3"), selected = "A4", inline = TRUE),
      tags$input(type = "hidden", id = "export_mode_ctx", value = mode_ctx)
    ))
  }, ignoreInit = TRUE)
  
  output$dl_image <- downloadHandler(
    filename = function() paste0("carte_amap_", format(Sys.Date(), "%Y%m%d"), ".png"),
    content = function(file) {
      mode_ctx <- isolate(input$open_export_modal)
      bounds   <- isolate(input$carte_bounds)
      options <- list(
        titre = isolate(input$export_titre),
        afficher_legende = isolate(input$export_legende),
        afficher_communes = isolate(input$export_communes),
        afficher_fond_osm = isolate(input$export_fond_osm),
        taille = isolate(input$export_taille), bounds = bounds
      )
      fonds <- list(idf_sf = idf_sf, idf_dept = idf_dept, idf_communes = idf_communes,
                    aac_sf = if (isTRUE(isolate(aesn_z()) == "aac")) aac_sf else NULL,
                    zpa_sf = if (isTRUE(isolate(aesn_z()) == "zpa")) zpa_sf else NULL)
      tryCatch({
        if (mode_ctx == "match") tmp <- exporter_carte("match", match_result(), options, fonds)
        else tmp <- exporter_carte("explo", list(amap_vis = amap_f(), fermes_vis = fermes_f()), options, fonds)
        file.copy(tmp, file); removeModal()
      }, error = function(e) {
        showNotification(paste("Export image echoue :", conditionMessage(e)), type = "error", duration = 10)
      })
    }
  )
}

shinyApp(ui, server)