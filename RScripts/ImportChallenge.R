# To do:
# import 3 sub-tables into 3 variables
# merge tables underneath each other
#    keep track of source
# rename
# fill in treatment
# get rid of double entry

pacman::p_load(conflicted,tidyverse,wrappedtools,readxl)
# conflict_scout()
conflicts_prefer(dplyr::filter(),
                 dplyr::lag())

t1 <- read_excel(path = 'Data/UntidyImportChallenge.xlsx',
                 range = 'A4:E11')
t2 <- read_excel(path = 'Data/UntidyImportChallenge.xlsx',
                 range = 'G4:K12')
t3 <- read_excel(path = 'Data/UntidyImportChallenge.xlsx',
                 range = 'M4:Q7')
# rawdata <- rbind(t1, t2, t3)

rawdata <- bind_rows(fast=t1,medium=t2,slow=t3,
                     .id = "Tumorgrowth") |>
  rename(Treatment=`Start-Day :`,
         AnimalCode=`Meas./Treatm.`) |>
  rename_with(.fn = ~paste("Weight [g]", .x),
              .cols =  contains(" h")) |>
  fill(Treatment) |>
  # mutate(`delta 24h-0h`=`Weight [g] 24 h`-`Weight [g] 0 h`,
  #        `delta 72h-0h`=`Weight [g] 72 h`-`Weight [g] 0 h`,
  #        `delta 72h-24h`=`Weight [g] 72 h`-`Weight [g] 24 h`) |>
  arrange(AnimalCode) |>
  # filter(!(Tumorgrowth=="fast"&AnimalCode=="C3")) # remove 2nd
  distinct(AnimalCode,
           .keep_all = TRUE) # all cols, not just AnimalCode


rawdata_long <-
  pivot_longer(data = rawdata,
               cols = contains("Weight"),
               names_to = 'Time',
               values_to = 'Body Weight (g)'
               ) |>
  mutate(`Time (h)`=parse_number(Time))

rawdata_long <-
  pivot_longer(data = rawdata,
               cols = contains("Weight"),
               names_to = 'Time [h]',
               names_pattern = ".+ (\\d+) h",
               values_to = 'Body Weight [g]')


rawdata_long2 <-
  pivot_longer(data = rawdata,
               cols = contains("Weight"),
               names_to = c(".value",'Time [h]'),
               names_pattern = "(.+\\]) (\\d+) h")

waldo::compare(rawdata_long, rawdata_long2)

str_replace(c("Weight 24 h","Weight 72 h"),
           pattern = "(Weight) (\\d+) h",
           replacement = "\\1")

str_replace(c("Weight 24 h","Weight 72 h"),
            pattern = "\\D+ (\\d+) .+",
            replacement = "\\1") |>
  as.numeric()



rawdata_wide <- rawdata_long |>
  pivot_wider(
    names_from = `Time [h]`,
    # names_prefix = "Weight [g] ",
    # names_glue = "{.value} {`Time [h]`}h",
    values_from = `Body Weight [g]`)

ggplot(rawdata_long,aes(x=`Time [h]`,
                        y=`Body Weight [g]`,
                        color=Treatment,
                        linetype=Tumorgrowth))+
  geom_point()+
  geom_line(aes(group=AnimalCode))

ggplot(rawdata_long,aes(x=`Time [h]`,
                        y=`Body Weight [g]`))+
  geom_boxplot()+
  facet_grid(rows=vars(Tumorgrowth))


