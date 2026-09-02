#!/usr/bin/env Rscript
# =============================================================================
# GENERATEUR DE DONNEES FICTIVES — demo Carte AMAP IDF
# =============================================================================
# Produit data/donnees_demo.json, jeu ENTIEREMENT INVENTE.
#
# Principes, non negociables :
#   1. Graine fixe -> deux executions produisent un fichier identique a l'octet
#      pres. Aucun horodatage n'entre dans la sortie.
#   2. Volumes et distributions releves sur la base de production, pour que la
#      carte ait la bonne densite. Seuls des AGREGATS ont ete releves.
#   3. Structure spatiale plausible : AMAP concentrees sur le coeur dense,
#      fermes en couronne, partenariats decroissants avec la distance.
#   4. Noms par assemblage de composants inventes. Aucun composant patronymique,
#      aucun toponyme. Le generateur ne PEUT PAS produire un nom de personne.
#   5. Aucun texte libre : hors les noms, toute valeur appartient a une
#      nomenclature fermee. C'est ce qui rend structurellement impossible la
#      presence d'une note interne en prose.
#   6. Identifiants synthetiques (prefixes G/F/P), aucun identifiant d'origine.
#
# Champs volontairement ABSENTS, cote donnee comme cote code : code postal,
# commune, ville, nom et identifiant du lieu de distribution, telephone,
# courriel, site web, et tout champ de texte libre.
#
# Usage : Rscript R/generer_donnees_demo.R
# =============================================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(sf)
})

GRAINE <- 20260829L

racine     <- if (basename(getwd()) == "R") ".." else "."
chemin_idf <- file.path(racine, "data", "idf.rds")
sortie     <- file.path(racine, "data", "donnees_demo.json")

# --- Volumes cibles (releves sur la base reelle) -----------------------------
N_AMAP   <- 405L    # AMAP geolocalisees
N_FERMES <- 400L    # fermes retenues dans l'emprise francilienne
PART_TERMINES <- 0.298   # 717 / 2409

# =============================================================================
# NOMENCLATURES FERMEES
# =============================================================================
# Referentiel des productions : identique a celui de l'application reelle.
REF_PRODUITS <- data.frame(
  id = 1:23,
  label = c(
    "Brebis - Fromages", "Brebis - Agneaux",
    "Cereales - Farines", "Cereales - Huiles", "Cereales - Legumineuses",
    "Cereales - Pain", "Cereales - Pates",
    "Chevres - Fromages", "Chevres - Cabris",
    "Fruits - Pommes/poires/jus", "Fruits - Petits fruits rouges",
    "Legumes - Maraichage", "Legumes - Pommes de terre", "Legumes - Champignons",
    "Miel et produits apicoles", "Oeufs",
    "Plantes aromatiques", "Porc", "Produits de la mer",
    "Vaches - Laitieres", "Vaches - Viande", "Volaille", "Autre"
  ), stringsAsFactors = FALSE
)

# Poids observes des identifiants de production (frequences reelles).
POIDS_PROD <- c("12" = 192, "22" = 31, "10" = 29, "15" = 26, "21" = 26,
                "16" = 23, "23" = 23, "8" = 21, "20" = 13, "6" = 13,
                "3" = 12, "14" = 10, "4" = 8, "1" = 7, "2" = 7,
                "11" = 6, "17" = 6, "5" = 6, "18" = 5, "7" = 2,
                "13" = 1, "19" = 1)

STATUTS_AMAP  <- c(fonctionne = 340, inactif = 57, complet = 23, creation = 3)
STATUTS_FERME <- c(active = 497, inactive = 68)
JOURS         <- c(Mercredi = 129, Jeudi = 127, Mardi = 86, Vendredi = 53,
                   Samedi = 28, Lundi = 24, Dimanche = 2)
# La modalite vide est majoritaire en production (341 / 619) : on la conserve,
# elle fait partie de la realite que la demo doit montrer.
CERTIFS <- setNames(
  c(341, 235, 23, 8, 4, 3, 2, 1, 1),
  c("", "AB", "Autre", "AB (en conversion)", "AB, AB (en conversion)",
    "AB, Autre", "AB, Biocoherence", "AB, Demeter", "AB, Nature et Progres")
)
HEURES_DEBUT  <- c("18h30" = 107, "19h" = 67, "18h" = 49, "18h45" = 18,
                   "19h30" = 17, "17h30" = 16, "18h00" = 15, "19h00" = 15,
                   "19h15" = 14, "18h15" = 10, "19h45" = 9, "17h00" = 6)
HEURES_FIN    <- c("20h" = 108, "19h30" = 77, "20h00" = 35, "19h45" = 34,
                   "20h30" = 34, "19h" = 17, "20h15" = 13, "19h15" = 12,
                   "20h45" = 7, "21h" = 5)

# --- Lexiques de noms --------------------------------------------------------
# Composants strictement non patronymiques et non toponymiques : noms communs
# du vocabulaire agricole, matieres et couleurs. Aucune combinaison ne peut
# former un nom de personne ni une commune existante.
AMAP_TETE <- c("AMAP du Panier", "AMAP des Cageots", "AMAP de la Cagette",
               "AMAP du Cabas", "AMAP de la Gerbe", "AMAP des Semis",
               "AMAP du Sillon", "AMAP de la Glane", "AMAP des Halliers",
               "AMAP du Pressoir", "AMAP de la Grange", "AMAP des Aires",
               "AMAP du Verger", "AMAP de la Serre", "AMAP des Ruches",
               "AMAP de l'Aire", "AMAP du Fournil", "AMAP de la Meule",
               "AMAP des Sillons", "AMAP du Clos")

FERME_TETE <- c("Ferme du Sillon", "Ferme de la Gerbe", "Ferme des Semis",
                "Jardins du Clos", "Jardins de la Glane", "Le Clos des Aires",
                "Les Terres du Pressoir", "La Grange aux Cagettes",
                "Le Verger du Cabas", "La Serre des Halliers",
                "Les Ruches du Sillon", "La Bergerie des Aires",
                "Le Fournil des Semis", "Les Champs du Pressoir",
                "La Chevrerie du Clos", "Le Rucher des Terres",
                "Le Pressoir des Semis", "La Meule du Clos",
                "Les Sillons de la Glane", "Le Domaine des Cagettes")

# Matieres, mineraux et couleurs. Liste filtree deux fois :
#   - aucun prenom francais (ecartes : Garance, Ambre, Jade, Opale, Corail,
#     Amarante, Lilas, Emeraude, Iris, Azur) ;
#   - aucun toponyme (ecartes : Sienne, Marne).
# Le generateur ne peut donc composer ni un nom de personne ni un nom de lieu.
QUALIFIANTS <- c("Safran", "Indigo", "Ocre", "Turquoise", "Cuivre", "Ardoise",
                 "Vermeil", "Cobalt", "Bistre", "Celadon", "Carmin", "Ivoire",
                 "Onyx", "Sable", "Grenat", "Ecarlate", "Fauve", "Pourpre",
                 "Cendre", "Bronze", "Argile", "Basalte", "Granit", "Silex",
                 "Craie", "Schiste", "Gres", "Tuffeau", "Calcaire", "Albatre",
                 "Porphyre", "Malachite", "Vermillon", "Cinabre", "Sanguine",
                 "Sepia", "Outremer", "Anthracite", "Etain", "Laiton", "Zinc",
                 "Quartz", "Mica", "Gneiss", "Chaux", "Suie")

# =============================================================================
# OUTILS
# =============================================================================

# Tirage pondere reproductible dans une nomenclature fermee.
tirer <- function(nomenclature, n) {
  sample(names(nomenclature), n, replace = TRUE,
         prob = as.numeric(nomenclature) / sum(as.numeric(nomenclature)))
}

# Inverse de fonction de repartition, par interpolation lineaire entre les
# quantiles observes. Reproduit le profil reel sans copier aucune valeur.
tirer_quantile <- function(n, p, v) {
  u <- runif(n)
  approx(x = p, y = v, xout = u, rule = 2)$y
}

# Deplacement d'un point d'une distance (km) dans une direction aleatoire.
deplacer <- function(lat0, lon0, d_km, theta) {
  list(lat = lat0 + (d_km * cos(theta)) / 111.32,
       lon = lon0 + (d_km * sin(theta)) / (111.32 * cos(lat0 * pi / 180)))
}

PARIS <- list(lat = 48.8566, lon = 2.3522)

# Tire n points dont la distance au centre suit le profil observe, en ne
# gardant que ceux tombant dans le contour regional.
semer_points <- function(n, p, v, contour) {
  lat <- numeric(0); lon <- numeric(0)
  while (length(lat) < n) {
    k <- (n - length(lat)) * 3L + 50L
    d <- tirer_quantile(k, p, v)
    theta <- runif(k, 0, 2 * pi)
    pts <- deplacer(PARIS$lat, PARIS$lon, d, theta)
    cand <- st_as_sf(data.frame(lat = pts$lat, lon = pts$lon),
                     coords = c("lon", "lat"), crs = 4326, remove = FALSE)
    dedans <- lengths(st_intersects(cand, contour)) > 0
    lat <- c(lat, pts$lat[dedans]); lon <- c(lon, pts$lon[dedans])
  }
  data.frame(lat = round(lat[1:n], 6), lon = round(lon[1:n], 6))
}

# Noms uniques par assemblage. Verifie l'unicite, echoue plutot que de doublonner.
composer_noms <- function(tetes, n) {
  grille <- expand.grid(t = tetes, q = QUALIFIANTS, stringsAsFactors = FALSE)
  possibles <- unique(paste(grille$t, grille$q))
  if (length(possibles) < n * 1.5)
    stop("Lexique trop etroit : ", length(possibles), " combinaisons pour ", n,
         " noms demandes. Elargir AMAP_TETE / FERME_TETE / QUALIFIANTS.")
  sample(possibles, n)
}

# Encodage des productions au format de la base : "-12-16-"
encoder_prod <- function(ids) {
  if (length(ids) == 0) return("")
  paste0("-", paste(sort(as.integer(ids)), collapse = "-"), "-")
}

# =============================================================================
# GENERATION
# =============================================================================
set.seed(GRAINE)

if (!file.exists(chemin_idf))
  stop("Contour regional introuvable : ", chemin_idf)
contour_idf <- st_make_valid(readRDS(chemin_idf))
contour_idf <- st_union(st_transform(contour_idf, 4326))

# --- AMAP --------------------------------------------------------------------
# Profil de distance au centre releve sur la base : q10 3.8 / q50 15.1 / q90 50.7
xy_amap <- semer_points(N_AMAP,
                        p = c(0, .10, .25, .50, .75, .90, 1),
                        v = c(0.4, 3.8, 6.6, 15.1, 30.1, 50.7, 82),
                        contour = contour_idf)

noms_amap <- composer_noms(AMAP_TETE, N_AMAP)

nb_adh <- round(tirer_quantile(N_AMAP,
                               p = c(0, .10, .25, .50, .75, .90, 1),
                               v = c(4, 20, 30, 42, 66, 90, 240)))

amap <- data.frame(
  id_groupe   = sprintf("G%04d", seq_len(N_AMAP)),
  nom_amap    = noms_amap,
  statut_amap = tirer(STATUTS_AMAP, N_AMAP),
  jour        = tirer(JOURS, N_AMAP),
  h_debut     = tirer(HEURES_DEBUT, N_AMAP),
  h_fin       = tirer(HEURES_FIN, N_AMAP),
  nb_adh      = as.integer(nb_adh),
  lat         = xy_amap$lat,
  lon         = xy_amap$lon,
  stringsAsFactors = FALSE
)

# Coherence horaire : l'heure de fin doit suivre l'heure de debut.
en_minutes <- function(h) {
  p <- strsplit(tolower(h), "h")
  vapply(p, function(x) {
    hh <- suppressWarnings(as.integer(x[1]))
    mm <- if (length(x) > 1 && nchar(trimws(x[2])) > 0) suppressWarnings(as.integer(x[2])) else 0L
    if (is.na(hh)) NA_integer_ else hh * 60L + (if (is.na(mm)) 0L else mm)
  }, 1L)
}
incoherent <- en_minutes(amap$h_fin) <= en_minutes(amap$h_debut)
amap$h_fin[incoherent] <- "20h30"

# --- Fermes ------------------------------------------------------------------
# Profil releve : q10 15 / q50 41.7 / q90 77.3 -> couronne peripherique
xy_fermes <- semer_points(N_FERMES,
                          p = c(0, .10, .25, .50, .75, .90, 1),
                          v = c(4, 15, 26.4, 41.7, 63.3, 77.3, 96),
                          contour = contour_idf)

noms_fermes <- composer_noms(FERME_TETE, N_FERMES)

# Productions : 1 a 3 par ferme, tirees selon les frequences observees.
nb_prod_ferme <- sample(1:3, N_FERMES, replace = TRUE, prob = c(0.62, 0.28, 0.10))
prod_ferme <- lapply(seq_len(N_FERMES), function(i) {
  k <- min(nb_prod_ferme[i], length(POIDS_PROD))
  sample(names(POIDS_PROD), k, replace = FALSE,
         prob = as.numeric(POIDS_PROD) / sum(as.numeric(POIDS_PROD)))
})

# Repartition sur les trois colonnes de la base, comme en production.
ids_principales <- vapply(prod_ferme, function(p) encoder_prod(p[1]), character(1))
ids_amap <- vapply(prod_ferme, function(p) if (length(p) > 1) encoder_prod(p[2]) else "", character(1))
ids_sec  <- vapply(prod_ferme, function(p) if (length(p) > 2) encoder_prod(p[3]) else "", character(1))

# annee_amap renseignee pour 35 % des fermes (217 / 619 en production).
a_annee <- runif(N_FERMES) < 0.35
annee_amap <- ifelse(a_annee, as.character(sample(2003:2026, N_FERMES, replace = TRUE)), "")

fermes <- data.frame(
  id_ferme            = sprintf("F%04d", seq_len(N_FERMES)),
  nom_ferme           = noms_fermes,
  statut_ferme        = tirer(STATUTS_FERME, N_FERMES),
  certif              = tirer(CERTIFS, N_FERMES),
  annee_amap          = annee_amap,
  id_productions      = ids_principales,
  id_productions_amap = ids_amap,
  id_productions_sec  = ids_sec,
  lat                 = xy_fermes$lat,
  lon                 = xy_fermes$lon,
  stringsAsFactors = FALSE
)

# --- Partenariats ------------------------------------------------------------
# Nombre de partenaires par AMAP : profil observe (med 4, q1 2, q3 6, max 21).
nb_part <- round(tirer_quantile(N_AMAP,
                                p = c(0, .25, .50, .75, .95, 1),
                                v = c(1, 2, 4, 6, 12, 21)))
nb_part[nb_part < 1] <- 1L

# Matrice des distances AMAP x fermes, en km.
d_km <- function(la1, lo1, la2, lo2) {
  111.32 * sqrt(((lo2 - lo1) * cos(la1 * pi / 180))^2 + (la2 - la1)^2)
}
D <- outer(seq_len(N_AMAP), seq_len(N_FERMES), Vectorize(function(i, j) {
  d_km(amap$lat[i], amap$lon[i], fermes$lat[j], fermes$lon[j])
}))

# Decroissance exponentielle avec la distance. Calibre pour approcher la
# mediane reelle de 33 km, sachant que la demo garde toutes ses fermes dans
# l'emprise francilienne alors que la base reelle en compte 216 hors region,
# qui allongeaient artificiellement la queue de distribution.
LAMBDA <- 45

# Tire k fermes pour une AMAP, en excluant celles deja liees.
tirer_fermes <- function(i, k, exclues) {
  dispo <- setdiff(seq_len(N_FERMES), exclues)
  if (length(dispo) == 0 || k <= 0) return(integer(0))
  k <- min(k, length(dispo))
  poids <- exp(-D[i, dispo] / LAMBDA)
  if (length(dispo) == 1L) return(dispo)
  sample(dispo, k, replace = FALSE, prob = poids)
}

ligne_partenariat <- function(i, j, date_fin) {
  # 1 production dans 84 % des cas, 2 dans 13 %, 3 marginal (profil reel).
  np <- sample(1:3, 1, prob = c(0.84, 0.13, 0.03))
  dispo <- prod_ferme[[j]]
  prods <- if (length(dispo) <= np) dispo else sample(dispo, np)
  data.frame(id_ferme = fermes$id_ferme[j], id_groupe = amap$id_groupe[i],
             id_produits = encoder_prod(prods), date_fin = date_fin,
             stringsAsFactors = FALSE)
}

dates_possibles <- format(seq(as.Date("2019-01-01"), as.Date("2026-06-30"), by = "month"), "%Y-%m-%d")
# 717 termines pour 1692 actifs en production, soit 42,4 % en plus.
RATIO_TERMINES <- 717 / 1692

lignes <- list()
for (i in seq_len(N_AMAP)) {
  actives <- tirer_fermes(i, nb_part[i], integer(0))
  for (j in actives) lignes[[length(lignes) + 1L]] <- ligne_partenariat(i, j, NA_character_)

  # Partenariats passes : d'autres fermes, aujourd'hui detachees du groupe.
  k_fin <- rbinom(1, length(actives), RATIO_TERMINES)
  for (j in tirer_fermes(i, k_fin, actives))
    lignes[[length(lignes) + 1L]] <- ligne_partenariat(i, j, sample(dates_possibles, 1))
}
partenariats <- do.call(rbind, lignes)
partenariats$id_part <- sprintf("P%05d", seq_len(nrow(partenariats)))
partenariats <- partenariats[, c("id_part", "id_ferme", "id_groupe", "id_produits", "date_fin")]

# =============================================================================
# SORTIE
# =============================================================================
# Aucun horodatage : la sortie doit etre identique a l'octet pres d'une
# execution a l'autre.
donnees <- list(
  meta = list(
    version = "1.0",
    graine  = GRAINE,
    avertissement = paste(
      "Jeu de donnees entierement fictif, genere par R/generer_donnees_demo.R.",
      "Aucune donnee reelle du reseau n'est publiee. Les entites affichees",
      "n'existent pas. L'interface et les traitements sont ceux de l'outil",
      "d'origine."
    ),
    volumes = list(amap = N_AMAP, fermes = N_FERMES,
                   partenariats = nrow(partenariats),
                   partenariats_actifs = sum(is.na(partenariats$date_fin))),
    nomenclatures = list(
      productions   = REF_PRODUITS,
      statuts_amap  = sort(names(STATUTS_AMAP)),
      statuts_ferme = sort(names(STATUTS_FERME)),
      jours         = sort(names(JOURS)),
      certifications = sort(unique(unlist(strsplit(names(CERTIFS)[names(CERTIFS) != ""], ", ")))),
      heures_debut  = sort(names(HEURES_DEBUT)),
      heures_fin    = sort(names(HEURES_FIN))
    )
  ),
  amap         = amap,
  fermes       = fermes,
  partenariats = partenariats
)

json <- toJSON(donnees, dataframe = "rows", auto_unbox = TRUE,
               na = "null", digits = 6, pretty = 2)
writeLines(json, sortie, useBytes = TRUE)

cat("Genere :", normalizePath(sortie, winslash = "/"), "\n")
cat("  AMAP               :", nrow(amap), "\n")
cat("  Fermes             :", nrow(fermes), "\n")
cat("  Partenariats       :", nrow(partenariats),
    "dont actifs", sum(is.na(partenariats$date_fin)), "\n")
cat("  Distance ferme-AMAP des partenariats actifs (km), quantiles :\n    ")
act <- partenariats[is.na(partenariats$date_fin), ]
ia <- match(act$id_groupe, amap$id_groupe); if_ <- match(act$id_ferme, fermes$id_ferme)
dd <- d_km(amap$lat[ia], amap$lon[ia], fermes$lat[if_], fermes$lon[if_])
cat(paste(round(quantile(dd, c(.1, .25, .5, .75, .9)), 1), collapse = "  "), "\n")
