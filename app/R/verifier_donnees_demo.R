#!/usr/bin/env Rscript
# =============================================================================
# CONTROLE DE CONFORMITE AVANT PUBLICATION — demo Carte AMAP IDF
# =============================================================================
# LECTURE SEULE. Ce script ne corrige rien : il constate.
# Ne jamais l'assouplir pour faire passer un controle.
#
# Sortie : code 0 si TOUS les controles passent ET qu'aucun n'est "NON EXEC".
#          code 1 sinon.
#
# Usage :
#   Rscript R/verifier_donnees_demo.R
#   Rscript R/verifier_donnees_demo.R --noms-reels=../noms_reels.txt
# =============================================================================

suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly = TRUE)
arg_noms <- sub("^--noms-reels=", "", args[grepl("^--noms-reels=", args)])
fichier_noms_reels <- if (length(arg_noms) == 1) arg_noms else NA_character_

racine   <- if (basename(getwd()) == "R") ".." else "."
f_json   <- file.path(racine, "data", "donnees_demo.json")
f_app    <- file.path(racine, "app.R")
f_gen    <- file.path(racine, "R", "generer_donnees_demo.R")

# --- Journal -----------------------------------------------------------------
resultats <- list()
noter <- function(id, libelle, etat, detail = "") {
  resultats[[length(resultats) + 1L]] <<-
    list(id = id, libelle = libelle, etat = etat, detail = detail)
  symbole <- switch(etat, OK = "  OK    ", ECHEC = "  ÉCHEC ", `NON EXEC` = "  NON EXEC ")
  cat(sprintf("%s %-2s %s\n", symbole, id, libelle))
  if (etat != "OK" && nzchar(detail))
    for (l in strsplit(detail, "\n")[[1]]) cat("           ", l, "\n")
}

cat("=============================================================\n")
cat(" CONTRÔLE DE CONFORMITÉ — jeu de données de démonstration\n")
cat("=============================================================\n\n")

if (!file.exists(f_json)) {
  cat("ÉCHEC BLOQUANT : ", f_json, " introuvable.\n", sep = "")
  quit(status = 1)
}
d <- fromJSON(f_json, simplifyDataFrame = TRUE)

# =============================================================================
# LISTE BLANCHE — seuls ces champs ont le droit d'exister
# =============================================================================
BLANCHE <- list(
  amap = c("id_groupe", "nom_amap", "statut_amap", "jour", "h_debut", "h_fin",
           "nb_adh", "lat", "lon"),
  fermes = c("id_ferme", "nom_ferme", "statut_ferme", "certif", "annee_amap",
             "id_productions", "id_productions_amap", "id_productions_sec",
             "lat", "lon"),
  partenariats = c("id_part", "id_ferme", "id_groupe", "id_produits", "date_fin")
)

# Noms de champs bannis : exclus de la DONNEE comme du CODE.
#
# Deux niveaux, pour etre precis sans rien relacher :
#  - EXACTS      : noms de champs complets. Recherches partout, tels quels.
#  - GENERIQUES  : mots courants qui designent un champ mais servent aussi de
#                  vocabulaire legitime ("type = 'commune'" pour la couche
#                  administrative publique, "url" pour la requete d'itineraire).
#                  Recherches uniquement en acces d'attribut ($champ), ce qui
#                  est la seule forme par laquelle une donnee d'entite se lit.
BANNIS_EXACTS <- c("cp_amap", "cp_ferme", "code_postal", "ville_amap",
                   "ville_ferme", "nom_lieu", "id_lieu", "mail_amap",
                   "mail_ferme", "tel_amap", "tel_ferme", "portable_amap",
                   "siteweb", "site_web", "contact_nom", "contact_prenom",
                   "contact_tel", "contact_mail", "contact_portable",
                   "commentaire_prive", "commentaire_ramap", "motdepasse",
                   "tresorier", "amapien_relais", "annee_creation",
                   "annee_crea", "annee_installation", "annee_install",
                   "coordonnees_gps")
BANNIS_GENERIQUES <- c("cp", "ville", "commune", "lieu", "adresse", "insee",
                       "mail", "email", "courriel", "tel", "telephone",
                       "portable", "url", "contact", "commentaire", "produits",
                       "token")

# Pour les controles portant sur la donnee (C2), tout est banni sans nuance :
# une cle JSON nommee "ville" est une fuite, quel que soit le contexte.
BANNIS <- unique(c(BANNIS_EXACTS, BANNIS_GENERIQUES))

# =============================================================================
# C1 — Aucun champ hors liste blanche
# =============================================================================
faux <- character(0)
for (coll in names(BLANCHE)) {
  if (is.null(d[[coll]])) { faux <- c(faux, paste0(coll, " : collection absente")); next }
  extra <- setdiff(names(d[[coll]]), BLANCHE[[coll]])
  manq  <- setdiff(BLANCHE[[coll]], names(d[[coll]]))
  if (length(extra)) faux <- c(faux, paste0(coll, " : champ non autorise -> ", paste(extra, collapse = ", ")))
  if (length(manq))  faux <- c(faux, paste0(coll, " : champ attendu manquant -> ", paste(manq, collapse = ", ")))
}
noter("C1", "Aucun champ hors liste blanche",
      if (length(faux)) "ECHEC" else "OK", paste(faux, collapse = "\n"))

# =============================================================================
# C2 — Aucun nom de champ banni, a quelque profondeur que ce soit
# =============================================================================
cles_recursives <- function(x, acc = character(0)) {
  if (is.list(x)) {
    n <- names(x)
    if (!is.null(n)) acc <- c(acc, n)
    for (e in x) acc <- cles_recursives(e, acc)
  }
  acc
}
toutes_cles <- unique(tolower(cles_recursives(d)))
cles_interdites <- intersect(toutes_cles, BANNIS)
noter("C2", "Aucun nom de champ banni dans le JSON",
      if (length(cles_interdites)) "ECHEC" else "OK",
      paste("clés trouvées :", paste(cles_interdites, collapse = ", ")))

# =============================================================================
# C3 — Aucun motif de donnee personnelle dans les VALEURS
# =============================================================================
valeurs_texte <- function(x) {
  out <- character(0)
  if (is.data.frame(x)) {
    for (col in names(x)) out <- c(out, as.character(x[[col]]))
  } else if (is.list(x)) {
    for (e in x) out <- c(out, valeurs_texte(e))
  } else if (is.atomic(x)) {
    out <- c(out, as.character(x))
  }
  out
}
vals <- valeurs_texte(d)
vals <- vals[!is.na(vals)]

MOTIFS <- list(
  "adresse de courriel"      = "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}",
  "numéro de téléphone"      = "(^|[^0-9])0[1-9]([ .-]?[0-9]{2}){4}([^0-9]|$)",
  "code postal francilien"   = "(^|[^0-9])(75|77|78|91|92|93|94|95)[0-9]{3}([^0-9]|$)",
  "adresse web"              = "https?://|www\\.",
  "numéro de voie"           = "\\b[0-9]{1,3}(bis|ter)? (rue|avenue|boulevard|impasse|place|chemin|route) "
)
trouves <- character(0)
for (nom in names(MOTIFS)) {
  hits <- unique(vals[grepl(MOTIFS[[nom]], vals, ignore.case = TRUE, perl = TRUE)])
  # Les dates ISO et les coordonnees ne doivent pas etre confondues avec un code postal.
  hits <- hits[!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", hits)]
  hits <- hits[!grepl("^-?[0-9]+\\.[0-9]+$", hits)]
  if (length(hits))
    trouves <- c(trouves, sprintf("%s : %d valeur(s), ex. %s",
                                  nom, length(hits), paste(head(hits, 3), collapse = " | ")))
}
noter("C3", "Aucun motif de donnée personnelle dans les valeurs",
      if (length(trouves)) "ECHEC" else "OK", paste(trouves, collapse = "\n"))

# =============================================================================
# C4 — Aucun texte libre : hors les noms, tout appartient a une nomenclature
# =============================================================================
nomen <- d$meta$nomenclatures
hors <- character(0)
verifier_nomenclature <- function(valeurs, autorisees, etiquette) {
  v <- unique(valeurs[!is.na(valeurs) & valeurs != ""])
  mauvais <- setdiff(v, autorisees)
  if (length(mauvais))
    hors <<- c(hors, sprintf("%s : %s", etiquette, paste(head(mauvais, 5), collapse = ", ")))
}
verifier_nomenclature(d$amap$statut_amap,   nomen$statuts_amap,  "statut_amap")
verifier_nomenclature(d$amap$jour,          nomen$jours,         "jour")
verifier_nomenclature(d$amap$h_debut,       nomen$heures_debut,  "h_debut")
verifier_nomenclature(d$amap$h_fin,         nomen$heures_fin,    "h_fin")
verifier_nomenclature(d$fermes$statut_ferme, nomen$statuts_ferme, "statut_ferme")
certifs_utilisees <- unlist(strsplit(d$fermes$certif[d$fermes$certif != ""], ", "))
verifier_nomenclature(certifs_utilisees, nomen$certifications, "certif")
# Productions : uniquement des identifiants du referentiel.
ids_prod <- unlist(lapply(
  c(d$fermes$id_productions, d$fermes$id_productions_amap,
    d$fermes$id_productions_sec, d$partenariats$id_produits),
  function(s) { if (is.na(s) || s == "") return(character(0))
    x <- strsplit(gsub("^-|-$", "", s), "-")[[1]]; x[x != ""] }))
verifier_nomenclature(ids_prod, as.character(nomen$productions$id), "identifiants de production")
noter("C4", "Aucun texte libre hors les noms",
      if (length(hors)) "ECHEC" else "OK", paste(hors, collapse = "\n"))

# =============================================================================
# C5 — Les noms sont bien des assemblages, et ils sont uniques
# =============================================================================
pb <- character(0)
n_a <- d$amap$nom_amap; n_f <- d$fermes$nom_ferme
if (anyDuplicated(n_a)) pb <- c(pb, "noms d'AMAP en double")
if (anyDuplicated(n_f)) pb <- c(pb, "noms de ferme en double")
if (!all(grepl("^AMAP ", n_a))) pb <- c(pb, "un nom d'AMAP ne suit pas le schéma d'assemblage")
# Aucun nom ne doit contenir de particule patronymique ni de civilite.
if (any(grepl("\\b(M\\.|Mme|Monsieur|Madame|EARL|GAEC|SCEA|SARL)\\b", c(n_a, n_f), ignore.case = TRUE)))
  pb <- c(pb, "forme juridique ou civilité détectée dans un nom")
noter("C5", "Noms uniques, assemblés, sans civilité ni forme juridique",
      if (length(pb)) "ECHEC" else "OK", paste(pb, collapse = "\n"))

# =============================================================================
# C6 — Coordonnees dans l'emprise francilienne
# =============================================================================
lat <- c(d$amap$lat, d$fermes$lat); lon <- c(d$amap$lon, d$fermes$lon)
dehors <- sum(lat < 48.0 | lat > 49.3 | lon < 1.3 | lon > 3.7 | is.na(lat) | is.na(lon))
noter("C6", "Toutes les coordonnées dans l'emprise francilienne",
      if (dehors > 0) "ECHEC" else "OK", paste(dehors, "point(s) hors emprise"))

# =============================================================================
# C7 — Le CODE ne reference aucun champ banni (contrainte 5)
# =============================================================================
# Un champ retire de la donnee mais toujours nomme dans l'application resterait
# une fuite en puissance : il suffirait de rebrancher la base.
if (!file.exists(f_app)) {
  noter("C7", "Le code de l'application ne référence aucun champ banni",
        "NON EXEC", paste("app.R introuvable à", f_app))
} else {
  code <- readLines(f_app, warn = FALSE)
  # On ignore les lignes de commentaire : la mention d'un champ exclu dans une
  # note explicative n'est pas une fuite.
  code_actif <- code[!grepl("^\\s*#", code)]
  hits <- character(0)
  # Noms de champs complets : interdits ou qu'ils apparaissent.
  for (champ in BANNIS_EXACTS) {
    n <- grep(paste0("\\b", champ, "\\b"), code_actif)
    if (length(n)) hits <- c(hits, sprintf("%s : %d occurrence(s), ligne(s) %s",
                                           champ, length(n), paste(head(n, 5), collapse = ", ")))
  }
  # Mots generiques : interdits en acces d'attribut, seule forme par laquelle
  # une donnee d'entite peut etre lue.
  for (champ in BANNIS_GENERIQUES) {
    n <- grep(paste0("\\$", champ, "\\b"), code_actif)
    if (length(n)) hits <- c(hits, sprintf("$%s : %d occurrence(s), ligne(s) %s",
                                           champ, length(n), paste(head(n, 5), collapse = ", ")))
  }
  noter("C7", "Le code de l'application ne référence aucun champ banni",
        if (length(hits)) "ECHEC" else "OK", paste(hits, collapse = "\n"))
}

# =============================================================================
# C8 — Aucun secret ni connexion base dans le code publié
# =============================================================================
fichiers_code <- c(f_app, f_gen, file.path(racine, "R"))
fichiers_code <- unique(unlist(lapply(fichiers_code, function(p) {
  if (dir.exists(p)) list.files(p, pattern = "\\.R$", full.names = TRUE)
  else if (file.exists(p)) p else character(0)
})))
# Ce script porte lui-meme la liste des motifs interdits : s'il s'analysait,
# il se declencherait sur son propre code. On l'exclut du perimetre, et de lui
# seul — aucun autre fichier n'est soustrait au controle.
fichiers_code <- fichiers_code[basename(fichiers_code) != "verifier_donnees_demo.R"]
MOTIFS_SECRET <- c("dbConnect", "RMariaDB", "\\bDBI\\b", "Sys\\.getenv",
                   "readRenviron", "password", "motdepasse", "api_key",
                   "apikey", "openrouteservice", "\\btoken\\b")
secrets <- character(0)
for (f in fichiers_code) {
  l <- readLines(f, warn = FALSE)
  l <- l[!grepl("^\\s*#", l)]
  for (m in MOTIFS_SECRET) {
    k <- grep(m, l, ignore.case = TRUE)
    if (length(k)) secrets <- c(secrets, sprintf("%s : %s ligne(s) %s",
                                                 basename(f), m, paste(head(k, 3), collapse = ", ")))
  }
}
noter("C8", "Aucun secret ni connexion base dans le code publié",
      if (length(secrets)) "ECHEC" else "OK", paste(secrets, collapse = "\n"))

# =============================================================================
# C9 — Non-collision avec les noms reels
# =============================================================================
# Necessite un fichier local contenant les noms reels, un par ligne, produit
# hors du depot. Sans lui, le controle sort en NON EXEC : c'est un manque
# d'information, pas une reussite.
if (is.na(fichier_noms_reels)) {
  noter("C9", "Aucun nom généré ne coïncide avec un nom réel",
        "NON EXEC", "Relancer avec --noms-reels=<fichier> (un nom par ligne, hors dépôt).")
} else if (!file.exists(fichier_noms_reels)) {
  noter("C9", "Aucun nom généré ne coïncide avec un nom réel",
        "NON EXEC", paste("Fichier introuvable :", fichier_noms_reels))
} else {
  reels <- readLines(fichier_noms_reels, warn = FALSE, encoding = "UTF-8")
  normaliser <- function(x) {
    x <- tolower(trimws(x))
    x <- iconv(x, to = "ASCII//TRANSLIT")
    gsub("[^a-z0-9]", "", x)
  }
  r <- unique(normaliser(reels)); r <- r[nzchar(r)]
  gen <- normaliser(c(n_a, n_f))
  coll <- unique(c(n_a, n_f)[gen %in% r])
  noter("C9", sprintf("Aucun nom généré ne coïncide avec un nom réel (%d références)", length(r)),
        if (length(coll)) "ECHEC" else "OK",
        if (length(coll)) sprintf("%d collision(s) : %s\nNe pas corriger à la main : changer GRAINE, régénérer, relancer.",
                                  length(coll), paste(head(coll, 5), collapse = ", ")) else "")
}

# =============================================================================
# VERDICT
# =============================================================================
cat("\n-------------------------------------------------------------\n")
etats <- vapply(resultats, function(x) x$etat, character(1))
n_ok <- sum(etats == "OK"); n_ko <- sum(etats == "ECHEC"); n_ne <- sum(etats == "NON EXEC")
cat(sprintf(" %d OK   |   %d ÉCHEC   |   %d NON EXEC\n", n_ok, n_ko, n_ne))
if (n_ko > 0) {
  cat(" VERDICT : NON PUBLIABLE. Corriger les échecs ci-dessus.\n")
} else if (n_ne > 0) {
  cat(" VERDICT : INCOMPLET. Un contrôle n'a pas pu s'exécuter ;\n")
  cat("           ce n'est pas une réussite. Ne pas publier en l'état.\n")
} else {
  cat(" VERDICT : PUBLIABLE. Tous les contrôles sont passés.\n")
}
cat("-------------------------------------------------------------\n")
quit(status = if (n_ko > 0 || n_ne > 0) 1 else 0)
