source plot_mean_evolution.m

x = 1:0.01:5;
parametres = struct ("H",1000,"x",0,"scale",1,"shape",1,"cmp",200,"cmc",1000);
p = 100;

list_simu = plot_mean_evolution(x, parametres, p);
