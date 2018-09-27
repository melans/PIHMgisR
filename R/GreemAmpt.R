# 
#' Calculate infiltration with Green Ampt method
#' \code{GreenAmpt} 
#' @param theta  Soil moisture content
#' @param thetas  Saturated moisture content
#' @param ksat  Saturated hydraulic conductivity
#' @param Hfront Depth of wetting front.
#' @return Infiltration rate 
#' @references Green, W.H. and G. Ampt. 1911. Studies of soil physics, part I – the flow of air and water through soils. J. Ag. Sci. 4:1-24.
#' @export
#' @examples 
#' hf= (1:200) / 100
#' ksat=0.2
#' theta = matrix(seq(0.1,0.4, by=0.05), ncol=1)
#' q_inf=apply(theta,1, FUN=function(x) GreenAmpt(x, 0.5, ksat, hf))
#' col= topo.colors(length(theta) )
#' lty = 1:length(theta) 
#' matplot(type='l', q_inf, -hf, col=col, lty=lty, lwd=2)
#' legend('bottomleft', paste('Theta =', theta), col=col, lty=lty, lwd=2); grid()
#' abline(v=ksat, lwd=3, lty=2)

GreenAmpt <- function(theta, thetas, ksat, Hfront){
  dt = thetas - theta
  dh = Hfront
  dh[dh <= 0 ] = 1e-6
  if(dt <= 0){
    q = 0
  }else{
    d0 = ksat * dh / dt
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
