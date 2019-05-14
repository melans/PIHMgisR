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

GreenAmpt <- function(theta, thetas, ksat, Hfront, y0, Time=0){
  nt=length(Time)
  if(nt <= 1){
    dt = thetas - theta
    dh = Hfront
    dh[dh <= 0.001 ] = .0010
    # if(y0 > ksat) y0 = ksat
    if(dt <= 0){
      q = 0
    }else if(dh <= ksat){
      q = ksat;
    }else{
      q = ksat * (y0 + dh) / (dh * dt)
      # d0 = ksat * dh / dt
      # q = dt * sqrt(d0 / 2 / 1)
    }
    if(q > y0){
      q = y0
    }
    ret = rbind(c(Time, q, theta, 
                  thetas, y0, ksat, Hfront) )
  }else{
    dtime= c(0, diff(Time))
    ret = NULL
    hf=Hfront
    for(it in 1:nt){
      tt = Time[it]
      x=GreenAmpt(theta = theta, thetas=thetas, 
                  ksat=ksat, y0=y0,  Hfront = hf, Time=tt)
      hf = hf + x[2]
      # print(hf)
      ret= rbind(ret, x)
    }
  }
  colnames(ret) = c('Time', 'Infiltration', 'Theta', 
                    'ThetaS', 'Y0', 'Ksat', 'WetFront')
  return (ret)
}
# y0=1:10/100
# theta=0.5 * (0:10)/10
# Ksat = 0.000001 #m/min
# qi=theta * 0
# Time = seq(0,2, length.out = 100)
# 
# norm <- function(x) return (x / max(x, na.rm=T))
# # 
# # x=GreenAmpt(theta=0.2, thetas = 0.5, ksat = Ksat, y0=y0[1], Hfront = 0, Time=Time)
# # Qi = x[,2]
# # WF = x[,7]
# # plot(Time, norm(Qi), type='l',lwd=2, col=2)
# # lines(Time, norm(WF), lwd=2, col=1)
# # grid()
# # stop()
# n1 = length(Time)
# n2 = 7
# n3 = length(y0)
# arr = array(0, dim=c(n1, n2, n3))
# for( i in 1:n3){
#   arr[,,i] = GreenAmpt(theta=0.2, thetas= 0.5, ksat = Ksat, y0=y0[i], Hfront = 0, Time=Time)
# }
# 
# matplot(type='l', Time[1:30], arr[1:30, 2, ], lwd=2,
#         ylim=range(c(arr[,2,], Ksat) ), 
#         col=topo.colors(n3))
# lines(Time, Time*0 + Ksat, lwd=2)
# grid()
# stop()

# plot(Time, (Qi), type='l',lwd=2, col=2, log='x')
# lines(Time, (WF), lwd=2, col=1)

# plot(Time, WF, type='l')
# plot(WF, Qi, lwd=3, lty=2, type='l')
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
