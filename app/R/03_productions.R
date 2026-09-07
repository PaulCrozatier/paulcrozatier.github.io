# R/03_productions.R
# Référentiel des productions et fonctions de décodage

ref_produits <- data.frame(
  id = 1:23,
  label = c(
    "Brebis - Fromages", "Brebis - Agneaux",
    "Céréales - Farines", "Céréales - Huiles", "Céréales - Légumineuses",
    "Céréales - Pain", "Céréales - Pâtes",
    "Chèvres - Fromages", "Chèvres - Cabris",
    "Fruits - Pommes/poires/jus", "Fruits - Petits fruits rouges",
    "Légumes - Maraîchage", "Légumes - Pommes de terre", "Légumes - Champignons",
    "Miel et produits apicoles", "Oeufs",
    "Plantes aromatiques", "Porc", "Produits de la mer",
    "Vaches - Laitières", "Vaches - Viande", "Volaille", "Autre"
  ), stringsAsFactors = FALSE
)

ALL_PROD_IDS <- as.character(ref_produits$id)

decode_ids_prod <- function(x) {
  x <- as.character(x)
  if (is.na(x) || x == "") return(character(0))
  ids <- unlist(strsplit(gsub("^-|-$", "", x), "-"))
  ids[ids != "" & !is.na(ids)]
}

label_productions <- function(x) {
  ids <- decode_ids_prod(x)
  if (length(ids) == 0) return("")
  noms <- ref_produits$label[ref_produits$id %in% as.integer(ids)]
  if (length(noms) == 0) return("")
  paste(noms, collapse = " | ")
}

label_non_productions <- function(x) {
  ids_presentes <- decode_ids_prod(x)
  ids_absentes <- setdiff(ALL_PROD_IDS, ids_presentes)
  if (length(ids_absentes) == 0) return("")
  noms <- ref_produits$label[ref_produits$id %in% as.integer(ids_absentes)]
  paste(noms, collapse = " | ")
}