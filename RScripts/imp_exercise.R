# To do:
# import 3 sub-tables into 3 variables
# merge tables underneath each other
#    keep track of source
# rename
# fill in treatment
# get rid of double entry

pacman::p_load(conflicted, tidyverse, wrappedtools, readxl)
# conflict_scout()
conflicts_prefer(
  dplyr::filter(),
  dplyr::lag()
)

t1 <- read_excel(
  path = "Data/UntidyImportChallenge.xlsx",
  range = "A4:E11"
)
t2 <- read_excel(
  path = "Data/UntidyImportChallenge.xlsx",
  range = "G4:K12"
)
t3 <- read_excel(
  path = "Data/UntidyImportChallenge.xlsx",
  range = "M4:Q7"
)
# rawdata <- rbind(t1, t2, t3)

rawdata <- bind_rows(
  fast = t1, medium = t2, slow = t3,
  .id = "Tumorgrowth"
) |>
  rename(
    Treatment = `Start-Day :`,
    AnimalCode = `Meas./Treatm.`
  ) |>
  rename_with(
    .fn = ~ paste("Weight [g]", .x),
    .cols = contains(" h")
  ) |>
  fill(Treatment) |>
  # mutate(`delta 24h-0h`=`Weight [g] 24 h`-`Weight [g] 0 h`,
  #        `delta 72h-0h`=`Weight [g] 72 h`-`Weight [g] 0 h`,
  #        `delta 72h-24h`=`Weight [g] 72 h`-`Weight [g] 24 h`) |>
  arrange(AnimalCode) |>
  # filter(!(Tumorgrowth=="fast"&AnimalCode=="C3")) # remove 2nd
  distinct(AnimalCode,
    .keep_all = TRUE
  ) # all cols, not just AnimalCode
