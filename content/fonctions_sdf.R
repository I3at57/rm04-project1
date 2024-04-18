# Fichier contenant l'ensemble des fonctions utiles de calcul de SdF pour
# le projet.

# =============================================================================
# FONCTIONS DE GENERATION DE PANNE ALEATOIRE
# 
# Ces fonctions servent à générer aléatoirement des instants de panne de
# composants.
# =============================================================================
    
component_failure_date <- function(law = "exponential",
                                     param = list(lambda = 0.1)) {
    
    # Fonction qui simule une date de panne pour un seul composant
    # suivant la law indiquée avec les paramètres spécifiés
    # Permet de rendre plus lisible le rapport sans utiliser
    # les fonctions natives de R peut explicites
    
    if ((law == "exponential") | (law == "exp")) {
        
        # Loi Exponentielle
        return(rexp(1,param$lambda))
              
    } else if ((law == "normale") | (law == "norm")) {
        
        # Loi Normale
        return(rnorm(1, mean = param$mu, sd = param$sigma))
        
    } else if ((law == "uniforme") | (law == "unif")) {
        
        # Loi Uniforme
        return(runif(1, min = param$a, max = param$b))
        
    } else if (law == "weibull") {
        
        # Loi de Weibull
        return(rweibull(1, shape = param$beta, scale = param$eta))
        
    } else {
        print("!!! Error in parameter !!!")
        return(-1)
    }
}

# Alias pour la fonction
cfd <- component_failure_date

# =============================================================================
# FONCTIONS DE SYSTEME
# 
# Ces fonctions servent à générer aléatoirement des instants de panne de
# systèmes.
# =============================================================================

generate_components <- function(comp_names = c(),
                       comp_law = c(),
                       comp_parameter = c()) {
    return(tibble(comp_names,comp_law,comp_parameter))
}

system_n_components_parallele_iid <- function(n,
                                              law = "exponential",
                                              param = list(lambda = 0.01)) {
    return(
        list(
            structure = "parallele",
            number_of_component = n,
            components = generate_components(
                comp_names = as.character(1:n),
                comp_law = replicate(n,law),
                comp_parameter = replicate(n,param)
            )
        )
    )
}

system_n_parallel_iid_failure_date <- function(n = 1,
                                               law = "exponential",
                                               param = list(lambda = 0.1)) {
    
    # Génère la panne d'un système de n composants de loi 'law' en
    # structure parallèle.
    
    replicate(n,
              component_failure_date(law = "exponential",
                                     param = param)) %>% 
        max() %>%
        return()
}

system_failure_date <- function(n = 10,
                          lambda = 0.1,
                          struct = "parallele") {
    
    # Fonction qui simule la date de panne d'un système à
    # n composants iid de type parallele ou serie
    
    if (struct == "parallele") {
        return(max(rexp(n,lambda)))
    } else if (struct == "serie") {
        return(min(rexp(n,lambda)))
    } else {
        print("!!! Error in parameter !!!")
        return(-1)
    }
}

MTTF_syst <- function(nbr_simulation = 10000,
                      loi_sys = panne_sys_exp) {
    return(mean(replicate(nbr_simulation,panne_sys_exp())))
}

# =============================================================================
# FONCTIONS D'AFFICHAGE
# 
# Ces fonctions servent à générer des jolis graphiques avec ggplot
# =============================================================================

plot_density <- function(law = "exponential",
                         param = list(lambda = 0.1),
                         x = seq(0,40,0.001),
                         addon = FALSE) {
    
    if ((law == "exponential") | (law == "exp")) {
        
        # Loi Exponentielle
        p <- geom_line(aes(x,dexp(x,param$lambda)))
        
    } else if ((law == "normale") | (law == "norm")) {
        
        # Loi Normale
        p <- geom_line(aes(x,dnorm(x, mean = param$mu, sd = param$sigma)))
        
    } else if ((law == "uniforme") | (law == "unif")) {
        
        # Loi Uniforme sur [a-b]
        p <- geom_line(aes(x,dunif(x, min = param$a, max = param$b)))
        
    } else if (law == "weibull") {
        
        # Loi de Weibull
        p <- geom_line(aes(x,dweibull(x, shape = param$beta, scale = param$eta)))
        
    } else {
        print("!!! Error in parameter !!!")
        return(-1)
    }
    
    if (!addon) {
        p <- ggplot() + p
    }
    
    return(p)
}

# Alias pour la fonction
dplot <- plot_density

plot_distribution <- function(law = "exponential",
                         param = list(lambda = 0.1),
                         x = seq(0,40,0.001),
                         addon = FALSE) {
    
    if ((law == "exponential") | (law == "exp")) {
        
        # Loi Exponentielle
        p <- geom_line(aes(x,pexp(x,param$lambda)))
        
    } else if ((law == "normale") | (law == "norm")) {
        
        # Loi Normale
        p <- geom_line(aes(x,pnorm(x, mean = param$mu, sd = param$sigma)))
        
    } else if ((law == "uniforme") | (law == "unif")) {
        
        # Loi Uniforme sur [a-b]
        p <- geom_line(aes(x,punif(x, min = param$a, max = param$b)))
        
    } else if (law == "weibull") {
        
        # Loi de Weibull
        p <- geom_line(aes(x,pweibull(x, shape = param$beta, scale = param$eta)))
        
    } else {
        print("!!! Error in parameter !!!")
        return(-1)
    }
    
    if (!addon) {
        p <- ggplot() + p
    }
    
    return(p)
}

# Alias pour la fonction
pplot <- plot_distribution

plot_reliability <- function(law = "exponential",
                         param = list(lambda = 0.1),
                         x = seq(0,40,0.001),
                         addon = FALSE) {
    
    if ((law == "exponential") | (law == "exp")) {
        
        # Loi Exponentielle
        p <- geom_line(aes(x,1-pexp(x,param$lambda)))
        
    } else if ((law == "normale") | (law == "norm")) {
        
        # Loi Normale
        p <- geom_line(aes(x,1-pnorm(x, mean = param$mu, sd = param$sigma)))
        
    } else if ((law == "uniforme") | (law == "unif")) {
        
        # Loi Uniforme sur [a-b]
        p <- geom_line(aes(x,1-punif(x, min = param$a, max = param$b)))
        
    } else if (law == "weibull") {
        
        # Loi de Weibull
        p <- geom_line(aes(x,1-pweibull(x, shape = param$beta, scale = param$eta)))
        
    } else {
        print("!!! Error in parameter !!!")
        return(-1)
    }
    
    if (!addon) {
        p <- ggplot() + p
    }
    
    return(p)
}

# Alias pour la fonction
rplot <- plot_reliability

# =============================================================================
# FONCTIONS DE VERIFICATION
# 
# Ces fonctions servent à verifier les resultats obtenus sur les
# differentes fonctions
# =============================================================================

test <- function() {
    t = seq(0,500,0.01)
    exp_density <- dexp(t,0.01)
    simu <- replicate(200,simu_date_pane_composant(param = list(lambda = 0.01)))
    
    ggplot() +
        geom_line(aes(t,exp_density)) +
        geom_point(aes(simu, dexp(simu,0.01)), color = "red", shape = 3)
}