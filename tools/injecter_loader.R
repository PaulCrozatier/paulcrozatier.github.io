#!/usr/bin/env Rscript
# =============================================================================
# Reinjecte l'ecran d'attente dans demo/index.html.
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
# =============================================================================

racine  <- if (basename(getwd()) == "tools") ".." else "."
cible   <- file.path(racine, "demo", "index.html")
extrait <- file.path(racine, "tools", "loader.html")

for (f in c(cible, extrait))
  if (!file.exists(f)) stop("Fichier introuvable : ", f)

html <- paste(readLines(cible, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

if (grepl('id="pc-loader"', html, fixed = TRUE)) {
  cat("Deja injecte — rien a faire.\n")
  quit(status = 0)
}

bloc <- paste(readLines(extrait, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Metadonnees : l'export laisse lang="en" et le titre generique "Shiny App".
html <- sub('<html lang="en">', '<html lang="fr">', html, fixed = TRUE)
html <- sub("<title>Shiny App</title>",
            "<title>Carte AMAP Ile-de-France \u2014 demonstration</title>",
            html, fixed = TRUE)

# Les polices du portfolio : l'export ne les charge pas, l'ecran d'attente
# retomberait sur Georgia et system-ui et ne ressemblerait plus au reste du
# site. Injectees dans <head> pour qu'elles soient demandees des le depart.
polices <- paste0(
  '<link rel="preconnect" href="https://fonts.googleapis.com">
',
  '<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
',
  '<link href="https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,300;9..144,500',
  '&family=Karla:wght@300;400;600&family=IBM+Plex+Mono:wght@400;500&display=swap" rel="stylesheet">
'
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
cat("  lang       :", if (grepl('lang="fr"', html)) "fr" else "inchange", "\n")
cat("  titre      :", if (grepl("demonstration", html)) "personnalise" else "inchange", "\n")
cat("  taille     :", nchar(html), "caracteres\n")
