source mean_cost.m

info = true;
param = struct ("H",1000,"x",3,"scale",1,"shape",1,"cmp",200,"cmc",1000);

mean_cost(10000,param,true)
