# 
#' Calculate infiltration with Green Ampt method
#' \code{GreenAmpt} 
#' @param x  Input data
#' @param filter Data filter
#' @param plot Whether plot the 
#' @return Matrix information, c('ID','Vmin','Vmax', 'Filter')
#' @references Green, W.H. and G. Ampt. 1911. Studies of soil physics, part I – the flow of air and water through soils. J. Ag. Sci. 4:1-24.
#' @export
#' @examples 
#' p = sin( (1:100) / 10 )
#' p[p<0] = 0
GreenAmpt <- function(t, ts, ks, hf){
  dt = ts - t
  dh = hf
  if(dh <=0 ){
    dh = 1e-6
  }
  if(dt <= 0){
    q = 0
  }else{
    d0 = ks * dh / dt
    q = dt * sqrt(d0 / 2 / 1)
  }
  q
}
# 
# func  <- function(p, ks=0.1, thetas = 0.5, def=10){
#   np = length(p)
#   hf = 0;
#   h0 = 0;
#   theta = 1 / def
#   nt = length(p)*2
#   qi = rep(0, nt)
#   hfi= rep(0,nt)
#   for(i in 1:nt ){
#     theta = hf / def
#     q = greenampt(theta, thetas, ks, hf)
#     message(i,'/', nt, '\t', theta)
#     if(i <= np){
#       if(q > p[i] ){
#         q = p[i]
#       }
#     }else{
#       q = 0;
#     }
#     hf = hf + q - ks * 0.01
#     qi[i] = q
#     hfi[i] = hf
#   }
#   r = cbind(p, qi, hfi)
# }
