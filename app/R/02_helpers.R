# R/02_helpers.R
# Fonctions utilitaires generiques

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || all(is.na(a)) || identical(a, "")) b else a
}

safe_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

heure_en_minutes <- function(h) {
  h <- trimws(as.character(h))
  result <- rep(NA_integer_, length(h))
  ok <- !is.na(h) & h != "" & grepl("h", h, ignore.case = TRUE)
  if (!any(ok)) return(result)
  parts <- strsplit(tolower(h[ok]), "h")
  heures <- suppressWarnings(as.integer(sapply(parts, `[`, 1)))
  mins   <- suppressWarnings(as.integer(sapply(parts, function(p) {
    if (length(p) > 1 && nchar(trimws(p[2])) > 0) trimws(p[2]) else "0"
  })))
  mins[is.na(mins)] <- 0L
  valid <- !is.na(heures)
  result[ok][valid] <- heures[valid] * 60L + mins[valid]
  result
}

duree_fmt <- function(debut, fin) {
  d <- heure_en_minutes(debut)
  f <- heure_en_minutes(fin)
  result <- rep("", length(d))
  ok <- !is.na(d) & !is.na(f) & f > d
  delta <- ifelse(ok, f - d, 0L)
  result[ok] <- ifelse(
    delta[ok] >= 60,
    paste0(delta[ok] %/% 60, "h", sprintf("%02d", delta[ok] %% 60)),
    paste0(delta[ok], " min")
  )
  result
}