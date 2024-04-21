# plot_mean_evolution

source mean_cost.m

function list_simu = plot_mean_evolution ( list_x = 1:10,
  param = struct ("H",1000,"x",0,"scale",1,"shape",1,"cmp",200,"cmc",1000),
  p = 100 )

  list_simu = [];

  for x = list_x
    param.x = x;
    list_simu = [list_simu, mean_cost(p = p, param = param, bytime = true)];
  endfor

endfunction

