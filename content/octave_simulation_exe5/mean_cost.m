# Mean cost by time
source trajectoire_aleatoire.m

function retval = mean_cost( p = 10000,
  param = struct ("H",1000,"x",3,"scale",1,"shape",1,"cmp",200,"cmc",1000),
  bytime = true );

  trajectoires = [];
  retval = 0;
  for i = 1:p
    trajectoires = [trajectoires, trajectoire_aleatoire(param,false)];
  endfor

  if bytime
    retval = mean(trajectoires/param.H);
  else
    retval = mean(trajectoires);
  endif
endfunction
