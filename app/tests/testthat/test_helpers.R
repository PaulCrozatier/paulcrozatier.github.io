library(testthat)
source(here::here("R", "02_helpers.R"))

# --- heure_en_minutes ---
test_that("heure_en_minutes parse correctement", {
  expect_equal(heure_en_minutes("19h15"), 1155L)
  expect_equal(heure_en_minutes("9h"),    540L)
  expect_equal(heure_en_minutes("20h00"), 1200L)
  expect_true(is.na(heure_en_minutes("")))
  expect_true(is.na(heure_en_minutes(NA)))
  expect_true(is.na(heure_en_minutes("pas une heure")))
})

test_that("heure_en_minutes fonctionne sur un vecteur", {
  res <- heure_en_minutes(c("19h15", NA, "", "9h"))
  expect_equal(res, c(1155L, NA_integer_, NA_integer_, 540L))
})

# --- duree_fmt ---
test_that("duree_fmt calcule correctement", {
  expect_equal(duree_fmt("18h", "20h00"), "2h00")
  expect_equal(duree_fmt("19h15", "20h"),  "45 min")
  expect_equal(duree_fmt("20h", "18h"),    "")
  expect_equal(duree_fmt(NA, "20h"),       "")
})

test_that("duree_fmt fonctionne sur un vecteur", {
  res <- duree_fmt(c("18h", "19h15", NA), c("20h00", "20h", "20h"))
  expect_equal(res, c("2h00", "45 min", ""))
})

# --- safe_text ---
test_that("safe_text remplace NA par chaine vide", {
  expect_equal(safe_text(NA),  "")
  expect_equal(safe_text(NULL), character(0))
  expect_equal(safe_text(c("Paris", NA, "Lyon")), c("Paris", "", "Lyon"))
})