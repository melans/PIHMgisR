#' Calculate the water balance
#' \code{p.waterbalance}
#' @param xl List of data. Five variables are included: prcp (qEleprcp), et0 (qEleet0), et1 (qEleet1), et2 (qEleet2) and discharge (Qrivflx)
#' @export 
p.waterbalance <-function(
  xl=BasicPlot(varname=c(paste0('qEle', c('prcp', 'et0', 'et1', 'et2') )
                         , 'QRivflx'), plot = F, return = T)
){
  
  func <-function(x, w){
    aa= sum(ia)
    y = sweep(x, 2, w, '*')
  }
  ia=getArea()
  aa=sum(ia)
  w = ia/aa
  pr = readriv()
  oid=getOutlets(pr)
  apply.period = apply.yearly
  P = apply.period( func(xl[['qEleprcp']], w ), FUN=sum)
  Q = apply.period(xl[['QRivflx']][,oid], FUN=sum) / aa
  ET0 = apply.period( func(xl[['qEleet0']], w ), FUN=sum)
  ET1 = apply.period( func(xl[['qEleet1']], w ), FUN=sum)
  ET2 = apply.period( func(xl[['qEleet2']], w ), FUN=sum)
  dh = P-Q-ET0-ET1-ET2
  x=cbind(P,Q,ET0, ET1,ET2)
  colnames(x)=c('P','Q','ET0', 'ET1','ET2')
  y = cbind(dh, x)
  colnames(y)=c('DH', 'P','Q','ET0', 'ET1','ET2')
  hydrograph(y)
#   
# df = cbind('Time'=time(x), as.data.frame(x))
# head(df)
#   tx = time(x)
#   dl = reshape2::melt(df, id='Time')
#   head(dl)
#   
#   ggplot(dl, aes(x = Time, y = value , color = variable)) +
#     geom_line() +
#     theme() +
#     scale_fill_distiller(palette = "Spectral")+
#     labs(x = "Date") 
}
#' Convert Time-Series data to data.frame
#' \code{ts2df}
#' @param x Time-Series data.
#' @return data.frame. First column is Time.
#' @export 
ts2df <- function(x){
  d1 = as.data.frame(x)
  colnames(d1) = colnames(x)
  d2 = data.frame('Time'=time(x), d1)
}
