Atmosphericpressure <- function(z){
  # Atmospheric pressure (P)
  # http://www.fao.org/docrep/X0490E/x0490e07.htm
  P = 101.3 * ( (293 - 0.0065 * z) / 293 ) ^ (5.26)
  return(P)
}
LatentHeatVaporization<- function(K, pressure, r_a){
  # Latent heat of vaporization (l)
  # http://www.fao.org/docrep/X0490E/x0490e07.htm
  CC = 2.45e6 # 2.45MJ/kg
  return(CC)
}
SlopeSatVaporPressure <- function(){
  # Slope of saturation vapour pressure curve (D )
  # http://www.fao.org/docrep/X0490E/x0490e07.htm
  # D slope of saturation vapour pressure curve at air temperature T [kPa °C-1],
  # T air temperature [°C],
  # exp[..] 2.7183 (base of natural logarithm) raised to the power [..].
  delta = 4098 * ( 0.6108* exp(17.27 * T / (T + 237.3))  ) /  ( (T + 237.3)^2 )
  return(delta)
}