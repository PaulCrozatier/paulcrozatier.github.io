# Ce fichier desactive le chargement automatique du repertoire R/ par Shiny.
#
# Depuis Shiny 1.5, tout fichier .R present dans R/ est source au demarrage de
# l'application. Or ce repertoire contient deux utilitaires en ligne de
# commande : le generateur de donnees et le script de controle. Sans ce
# fichier, lancer l'application declenchait une regeneration des donnees puis
# le controle de conformite, dont le quit(status = 1) final tuait le serveur.
#
# app.R charge explicitement ce dont il a besoin :
#   source(here::here("R", "02_helpers.R"))
#   source(here::here("R", "03_productions.R"))
#   source(here::here("R", "04_export_image.R"))
