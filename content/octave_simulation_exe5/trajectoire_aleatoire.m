# trajectoire aleatoire
source weibull.m

function Cf = trajectoire_aleatoire (
  param = struct ("H",1000,"x",3,"scale",1,"shape",1,"cmp",200,"cmc",1000),
  info = false
  )
  echeancier = param.x * 1:floor(param.H/param.x);
  if( info )
      printf("=== l'échéancier de maintenance préventive ===\n");
      disp(echeancier)
      printf("\n=== Début de simulation ===\n");
  endif

  t = 0;
  Cf = 0;
  i = 1;

  while( t < param.H & i < length(echeancier) )
      next_panne = t + weibull(param.scale,param.shape);
      if( info )
          printf("Temps courant %f\n",t);
          printf("Date de la prochaine panne : %f\n",next_panne);
          printf(
              "Date de la prochaine maintenance préventive : %f\n",
              echeancier(i)
          );
      endif

      if ( next_panne < echeancier(i) )
          if( info ) printf("MC"); endif
          t = next_panne;
          Cf = Cf + param.cmc;
      else
          if( info ) printf("MP"); endif
          t = echeancier(i);
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
