# Fonction aléatoire de Weibull

function retval = weibull( a_scale = 1, a_shape = 1)

  retval = a_scale * ((-1*log(rand(1)))^(1/a_shape));

endfunction

