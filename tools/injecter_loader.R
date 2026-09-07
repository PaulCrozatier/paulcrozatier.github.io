#!/usr/bin/env Rscript
# =============================================================================
# Reinjecte l'ecran d'attente dans demo/index.html, puis controle l'export.
#
# A LANCER APRES CHAQUE shinylive::export() : l'export regenere entierement
# demo/index.html et efface donc le bloc. Sans cette etape, la demonstration
# repart sur un ecran blanc de ~65 s sans aucune explication, ce qui se lit
# comme un lien mort.
#
#   Rscript -e "shinylive::export('app', 'demo')"
#   Rscript tools/injecter_loader.R
#
# Le script est idempotent : relance sans effet si le bloc est deja present.
# Il se termine en code 1 si l'un des deux controles bloquants echoue.
# =============================================================================

racine  <- if (basename(getwd()) == "tools") ".." else "."
cible   <- file.path(racine, "demo", "index.html")
extrait <- file.path(racine, "tools", "loader.html")

for (f in c(cible, extrait))
  if (!file.exists(f)) stop("Fichier introuvable : ", f)

html <- paste(readLines(cible, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

deja <- grepl('id="pc-loader"', html, fixed = TRUE)

if (!deja) {
  bloc <- paste(readLines(extrait, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  # Metadonnees : l'export laisse lang="en" et le titre generique "Shiny App".
  html <- sub('<html lang="en">', '<html lang="fr">', html, fixed = TRUE)
  html <- sub("<title>Shiny App</title>",
              "<title>Carte AMAP Île-de-France — démonstration</title>",
              html, fixed = TRUE)

  # Les polices du portfolio : l'export ne les charge pas, l'ecran d'attente
  # retomberait sur Georgia et system-ui et ne ressemblerait plus au reste du
  # site. Injectees dans <head> pour qu'elles soient demandees des le depart.
  polices <- paste0(
    '<link rel="preconnect" href="https://fonts.googleapis.com">\n',
    '<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>\n',
    '<link href="https://fonts.googleapis.com/css2?family=Anton',
    '&family=Newsreader:opsz,wght@6..72,400;6..72,600&display=swap" rel="stylesheet">\n'
  )
  if (!grepl("fonts.googleapis.com", html, fixed = TRUE))
    html <- sub("</head>", paste0(polices, "</head>"), html, fixed = TRUE)

  # Le bloc doit venir juste apres <body> pour couvrir l'ecran des la premiere
  # peinture, avant que shinylive n'ait commence a telecharger quoi que ce soit.
  if (!grepl("<body>", html, fixed = TRUE))
    stop("Balise <body> introuvable dans ", cible, " : structure d'export inattendue.")
  html <- sub("<body>", paste0("<body>\n", bloc), html, fixed = TRUE)

  writeLines(html, cible, useBytes = TRUE)
  cat("Ecran d'attente injecte dans", normalizePath(cible, winslash = "/"), "\n")
} else {
  cat("Deja injecte — rien a reinjecter.\n")
}

# =============================================================================
# CONTROLES BLOQUANTS
#
# Les accents de l'application avaient disparu de l'interface sans que personne
# ne s'en apercoive, parce que rien ne les surveillait entre l'ecriture du code
# et la page servie. Ces deux controles ferment cette fenetre : ils tournent a
# chaque export et arretent la chaine plutot que de laisser passer.
# =============================================================================
html <- paste(readLines(cible, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
echecs <- character(0)

# --- C1 : declaration d'encodage ---------------------------------------------
# Sans <meta charset>, le navigateur devine ; sur un fichier UTF-8 servi sans
# en-tete explicite, les accents se transforment en caracteres de remplacement.
if (grepl('charset=["\']?[Uu][Tt][Ff]-?8', html)) {
  cat("  OK     C1  <meta charset> UTF-8 present\n")
} else {
  cat("  ECHEC  C1  aucune declaration charset UTF-8 dans l'export\n")
  echecs <- c(echecs, "C1")
}

# --- C2 : mots temoins sans accent -------------------------------------------
# Chacun de ces mots porte au moins un accent en francais. En trouver un sous sa
# forme nue dans la page servie signale un libelle non accentue, ou une perte
# d'encodage a l'export.
TEMOINS <- c("demonstration", "donnees", "generees", "publiee", "legende",
             "reseau", "Ile-de-France", "entites", "selection", "itineraire")
# On ecarte les commentaires HTML : ils ne s'affichent pas, et les commentaires
# de source de ce depot sont ecrits sans accent par convention. Le controle
# porte sur ce que lit le visiteur, pas sur ce que lit le developpeur.
visible <- gsub("<!--.*?-->", "", html)
trouves <- TEMOINS[vapply(TEMOINS,
                          function(m) grepl(m, visible, fixed = TRUE),
                          logical(1))]
if (length(trouves) == 0) {
  cat("  OK     C2  aucun mot temoin non accentue (", length(TEMOINS),
      " testes )\n", sep = "")
} else {
  cat("  ECHEC  C2  mot(s) temoin(s) non accentue(s) :",
      paste(trouves, collapse = ", "), "\n")
  echecs <- c(echecs, "C2")
}

cat("  taille :", nchar(html), "caracteres\n")

if (length(echecs)) {
  cat("\nEXPORT NON PUBLIABLE :", paste(echecs, collapse = ", "), "\n")
  quit(status = 1)
}
cat("\nExport conforme.\n")
