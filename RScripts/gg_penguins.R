pacman::p_load(
  conflicted, tidyverse, wrappedtools, # palmerpenguins,
  ggbeeswarm, hrbrthemes
)
conflicts_prefer(dplyr::filter)
data(penguins)
head(penguins)
penguins <- as_tibble(penguins)

# Vizualize the count of sex within species
# boxplot+beeswarm weight vs. species
# boxplot+beeswarm weight vs. species AND sex

# create a scatterplot for flipper length vs. body mass
# add a regression line to that plot
# do the same scatterplot and regression grouped by species and sex

# optionally, define your own colors for species (scale or name or rgb)







# count sex within species
ggplot(penguins, aes(species)) +
  geom_bar(fill = "darkorange2")

ggplot(penguins, aes(species, fill = sex)) +
  geom_bar()
ggplot(penguins, aes(fill = species, x = sex)) +
  geom_bar()


# boxplot+beeswarm weight vs. species
penguins |>
  filter(!is.na(sex)) |>
  ggplot(aes(species, body_mass)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_beeswarm(alpha = .2, cex = .9)


# boxplot+beeswarm weight vs. species AND sex
penguins |>
  filter(!is.na(sex)) |>
  ggplot(mapping = aes(species, body_mass)) +
  geom_boxplot(
    outlier.alpha = 0, alpha = .2,
    aes(fill = sex)
  ) +
  geom_beeswarm(
    alpha = .5, cex = .9,
    # aes(color=sex),
    aes(shape = sex),
    dodge.width = .75
  )
penguins |>
  filter(!is.na(sex)) |>
  ggplot(mapping = aes(species, body_mass, fill = sex)) +
  geom_boxplot(outlier.alpha = 0.5, alpha = 1) +
  geom_beeswarm(
    alpha = 1, cex = 0.9, shape = 21,
    dodge.width = .75
  )

penguins <- drop_na(penguins)

# scatterplot flipper length vs. body mass
penguins |>
  ggplot(aes(body_mass, flipper_len)) +
  geom_point()

# group by species (and sex?)
penguins |>
  ggplot(aes(body_mass, flipper_len, color = species)) +
  geom_point(alpha = .5)
baseplot <-
  penguins |>
  ggplot(aes(body_mass, flipper_len,
    color = species, shape = sex
  )) +
  geom_point(size = 5) +
  scale_color_manual(values = c("royalblue", "orangered4", "#FFA500")) +
  scale_shape_manual(values = c("\u2640", "\u2642"))

baseplot

# regression lines flipper length / body mass
# overall, by species, by species and sex

# regression line

baseplot +
  geom_smooth(method = lm, se = F) +
  geom_smooth(
    method = lm, se = FALSE,
    color = "black",
    aes(group = "all")
  )
baseplot +
  geom_smooth(method = lm, se = F, aes(linetype = sex))
# V1
penguins |>
  ggplot(aes(body_mass, flipper_len,
    color = species
  )) +
  geom_point(size = 5, aes(shape = sex)) + # local aes
  scale_color_manual(values = c("royalblue", "orangered4", "#FFA500")) +
  scale_shape_manual(values = c("\u2640", "\u2642")) +
  geom_smooth(method = lm, se = F)
# V2
baseplot +
  geom_smooth(method = lm, se = F, aes(group = species)) # changing groupings


penguins |>
  ggplot(aes(bill_dep, body_mass,
    color = species
  )) +
  geom_point(size = 5, aes(shape = sex)) + # local aes
  scale_color_manual(values = c("royalblue", "orangered4", "#FFA500")) +
  scale_shape_manual(values = c("\u2640", "\u2642")) +
  geom_smooth(method = lm, se = F, aes(linetype = sex)) +
  geom_smooth(method = "lm", color = "black")


gridplot <- penguins |>
  ggplot(aes(bill_dep, body_mass)) + # , color=sex))+
  geom_point(alpha = .25) +
  geom_smooth() +
  geom_smooth(method = "lm", color = "orangered4", linetype = "dashed") +
  facet_grid(
    rows = vars(species),
    cols = vars(sex),
    margins = TRUE
  )

# define your own colors for species (name or rgb)
penguins |>
  ggplot(aes(body_mass, flipper_len, color = species, shape = sex)) +
  # scale_shape_manual(values=c("\u2640","\u2642"))+
  geom_point(size = 1) +
  geom_smooth(method = lm, se = F) +
  scale_color_manual(values = c("#007FFF", "#FFFF00", "#FFA500")) +
  theme_dark()

penguincolors <- c("#007FFF", "#dde363", "#5f3529")

penguins |>
  ggplot(aes(body_mass, flipper_len, color = species, shape = sex)) +
  # scale_shape_manual(values=c("\u2640","\u2642"))+
  geom_point(size = 1) +
  geom_smooth(method = lm, se = F) +
  scale_color_manual(values = penguincolors)



gridplot
# gridplot+theme_ipsum_rc()
# theme_set(theme_ipsum_rc())
# theme_update(axis.text.x=element_text(face="bold"),
#              axis.title.x=element_text(size=rel(2),
#                                      face="bold"),
#              strip.text.x=element_text(size=rel(1.5),
#                                        face="bold",
#                                        hjust=1),
#              strip.text.y=element_text(size=rel(.8),
#                                        face="bold",
#                                        hjust=.5),
#              strip.background=element_rect(fill='gold'))
# gridplot

# Visualise bill depth vs. body mass, scatter only
# Add / compare linear/non-linear regression
# Facet by sex and species, use margins

penguins |>
  ggplot(aes(body_mass, bill_dep)) +
  geom_point(alpha = .20)
penguins |>
  ggplot(aes(body_mass, bill_dep)) +
  geom_point(alpha = .20) +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  geom_smooth()
penguins |>
  ggplot(aes(body_mass, bill_dep)) +
  geom_point(alpha = .20) +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  geom_smooth() +
  facet_grid(
    rows = vars(species),
    cols = vars(sex),
    margins = TRUE
  )

penguins |>
  filter(species != "Gentoo") |>
  ggplot(aes(body_mass, bill_dep)) +
  geom_point(alpha = .25) +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  geom_smooth() +
  facet_grid(
    rows = vars(species),
    cols = vars(sex),
    margins = TRUE
  )

# Visualize distribution of body mass by species and sex

penguins |>
  ggplot(aes(body_mass, fill = sex)) +
  geom_density(alpha = .5) +
  facet_grid(rows = vars(species))

penguins |>
  ggplot(aes(body_mass, fill = sex)) +
  geom_histogram(alpha = .5, position = "identity") +
  facet_grid(rows = vars(species))

penguins |>
  ggplot(aes(body_mass, fill = sex)) +
  geom_histogram(position = "dodge") +
  facet_grid(rows = vars(species))


penguins |>
  ggplot(aes(species, y = body_mass, color = sex)) +
  geom_violin(position = "dodge", draw_quantiles = c(.25, .5, .75))
