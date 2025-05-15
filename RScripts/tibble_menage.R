pacman::p_load(tidyverse)
# how to fill columns? external or inside tibble?
# how to create elements? paste/rep?

n_elements <- 5*10^3
salt <- paste('Saltgrain',1:n_elements)
menage <- tibble(Saltshaker= salt,
                 Peppercaster=paste('Pepperflake',1:n_elements),
                 chili=rep('Chiliflake',n_elements))
str(menage)

#Address Saltshaker, Salt, and 100 saltgrains ####

menage['Saltshaker']
menage[1]
menage[c(1,3)]
menage  |>  select(Saltshaker)
select(.data = menage,Saltshaker)
menage[['Saltshaker']]
menage$Saltshaker
pull(menage,Saltshaker)
menage[[1]][1:100]
menage[1:100,1] # still not content!
menage |>  slice(1:100)  |>  pull(Saltshaker)
#menage |>  pull(Saltshaker) |>  slice(1:100)
menage |>  slice(1:100)  |>  select(Saltshaker)
menage |>  slice_sample(n = 100)  |>  pull(Saltshaker)
menage |> slice_max(order_by = Saltshaker, n = 100) |> pull(Saltshaker)

