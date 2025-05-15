pacman::p_load(tidyverse)
testset1 <- c(
  "Meier", "Mayer", "Maier", "Meyer", "Mayr",
  "Hans Meier", "Mayerhofen", "Maya", "Mayor"
)
# find all variations of the name "Meier"

testset2 <- c("weight_mm", "height_cm", "age_yr", "temp_c")
# replace _ with space
# replace _ with space and add unit in brackets

testset3 <- c("1980_12_30", "13.04.2005", "2005/04/25", "24121990")
# transform into YYYY-MM-DD

testset4 <- c("pw2000", "That1sb3tt3r", "M@kesSense?", "NoDigits@this1")
# test pwd strength, rules: Upper, lower, special char, number, min 8 char long


# solutions ####
testset1 |> str_detect("M[ea][iy]e?r$")
testset1 |> str_subset("M[ea][iy]e?r$")
testset1 |> str_extract("M[ea][iy]e?r$")

c(testset1, "Hans Meier", "Mayerhofen") |> str_view("M[ea][iy]e?r$", match = NA)


testset2 |> str_replace(pattern = "_", replacement = " ")
testset2 |> str_replace("_(.+)", " [\\1]")
testset2 |> str_view("_(.+)")

testset3 |>
  str_replace_all(
    c(
      "(\\d{4})[_/](\\d{2})[_/](\\d{2})" = "\\1-\\2-\\3", # ymd
      "([0123]\\d)([01]\\d)([12]\\d{3})" = "\\3-\\2-\\1", # dmy date starting with 0-3,month starting with 0-1, year starting with 1-2
      "(\\d{2})\\.(\\d{2})\\.(\\d{4})" = "\\3-\\2-\\1"
    ) # dmy date with dots
  )

testset4[which(
  str_detect(testset4, pattern = "[A-Z]") &
    str_detect(testset4, "[:lower:]") & # [a-z]") &
    str_detect(testset4, "[0-9]") &
    str_detect(testset4, "\\W") & # "[^A-Za-z0-9]") &
    str_detect(testset4, ".{8,}")
)]
testset4[which(
  str_detect(testset4, pattern = "[A-Z]", negate = T) |
    str_detect(testset4, "[a-z]", negate = T) |
    str_detect(testset4, "\\d", negate = T) |
    str_detect(testset4, "[^A-Za-z0-9]", negate = T) |
    str_detect(testset4, ".{8,}", negate = T)
)]


not_passed <- c(
  str_which(testset4, pattern = "[A-Z]", negate = T),
  str_which(testset4, "[:lower:]", negate = T),
  str_which(testset4, "[0-9]", negate = T),
  str_which(testset4, "[^A-Za-z0-9]", negate = T),
  str_which(testset4, ".{8,}", negate = T)
) |>
  unique() |>
  sort()
testset4[not_passed]
testset4[-not_passed]
