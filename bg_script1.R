library(dotenv)
library(ggplot2, warn.conflicts = FALSE) # Pour les graphiques
library(magrittr, warn.conflicts = FALSE) # Pour l'opérateur pipe : %>%
library(dplyr, warn.conflicts = FALSE) # Pour la manipulation de dataframe
res_simu <- 100 %>%
    replicate(expr = compute_mean_cost_by_time(
        p = 10000, param_sys = parametres)) %>%
    var()