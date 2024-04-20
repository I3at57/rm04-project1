# Réalisation de l'exercice 5 avec Octave pour optimiser les calculs

# Dans octave la fonction `rand(1)` génère un nombre aléatoire entre 0 et 1.

function retval = realisation_aleatoire_weibull ( scale, shape )
  retval = scale * (-1*log(rand(1)))^(1/shape);
endfunction

# avec Octave on définit une structure comme ça
# param = struct ("H",1000,"x",3,"scale",1,"shape",1,"cmp",200,"cmc",1000)

function Cf = realisation_trajectoire_sys1 ( param, info )

    echeancier <- param.x * 1:floor(param.H/param.x);

    if( info )
        printf("=== l'échéancier de maintenance préventive ===\n");
        disp(echeancier)
        printf("=== Début de simulation ===\n");
    endif

    t = 0;
    Cf = 0;
    i = 1;

    while( t < param.H & i < length(echeancier) )
        next_panne = t + realisation_aleatoire_weibull(
          scale = param.scale,
          shape = param.shape
        );
        if( info )
            printf("Temps courant %f\n",t);
            printf("Date de la prochaine panne : %f\n",next_panne);
            printf(
                "Date de la prochaine maintenance préventive : %f\n",
                echeancier[i]
            );
        endif

        if ( next_panne < echeancier[i] )
            if( info ) printf("MC"); endif
            t = next_panne;
            Cf = Cf + param.cmc;
        else
            if( info ) printf("MP"); endif
            t = echeancier[i];
            i = i + 1;
            Cf = Cf + param.cmp;
        endif

        if( info )
            printf("Temps courant %f\n",t);
            printf("Coût actuel %f\n",Cf);
            printf("val de i %d\n",i);
            printf("\n---\n");
        endif
    endwhile
endfunction

compute_mean_cost_by_time <- function(p = 10000, param_sys) {

  # Calcul un coût moyen par unité de temps Cinf(x)
  # On chaque trajectoire est divisée par H, l'horizon de simulation

  frame <- replicate(
    p, realisation_trajectoire_sys1(param = param_sys, info = FALSE))
  frame <- mean(frame/param_sys.H)
  return(frame)
}
plot_mean_cost_evolution <- function(list_x, param, p = 1000) {
  list_simu <- c()
  for (x in list_x) {
    param.x <- x
    list_simu <- c(list_simu,
                   compute_mean_cost_by_time(p = p,
                                             param_sys = param))
  }

  ggplot() +
    geom_line(aes(x = list_x, y = list_simu)) +
    labs(x = "Périodicité des maintenances",
         y = "Coût moyen simulé",
         title = "Évolution du coût moyen simulé suivant x")
}
