#' Calculate the water balance
#' \code{p.waterbalance}
#' @param xl List of data. Five variables are included: prcp (eleqprcp), et0 (eleqet0), et1 (eleqet1), et2 (eleqet2) and discharge (rivqflx)
#' @param fun function to process the time-series data. Default = apply.daily.
#' @param plot Whether plot the result
#' @return A matrix, contains the colums of water balance factors
#' @export
wb.all <-function(
  xl=BasicPlot(varname=c(paste0('elev', c('prcp', 'et0', 'et1', 'et2') )
                         , 'rivqdown'), plot = FALSE, return = TRUE),

  fun = xts::apply.yearly, plot=TRUE
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
  P = fun( func(xl[['elevprcp']], w ), FUN=sum)
  Q = fun(xl[['rivqdown']][,oid], FUN=sum) / aa
  ET0 = fun( func(xl[['elevet0']], w ), FUN=sum)
  ET1 = fun( func(xl[['elevet1']], w ), FUN=sum)
  ET2 = fun( func(xl[['elevet2']], w ), FUN=sum)
  dh = P-Q-ET0-ET1-ET2
  x=cbind(P,Q,ET0, ET1,ET2)
  colnames(x)=c('P','Q','ET0', 'ET1','ET2')
  y = cbind(dh, x)
  colnames(y)=c('DH', 'P','Q','ET0', 'ET1','ET2')
  if(plot){
    hydrograph(y)
  }
  y
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

#' Calculate the water balance
#' \code{wb.riv}
#' @param xl List of data. Five variables are included: prcp (eleqprcp), et0 (eleqet0), et1 (eleqet1), et2 (eleqet2) and discharge (rivqflx)
#' @param fun function to process the time-series data. Default = apply.daily.
#' @param plot Whether plot the result
#' @return A matrix, contains the colums of water balance factors
#' @export
wb.riv <-function(
  xl=BasicPlot(varname=paste0('rivq', c('sub', 'surf', 'down') ), plot = FALSE, return = TRUE),
  fun = xts::apply.yearly, plot=TRUE
){
  # xl=BasicPlot(varname=paste0('rivq', c('sub', 'surf', 'flx') ) )
  # fun = xts::apply.daily

  func <-function(x, w){
    aa= sum(ia)
    y = sweep(x, 2, w, '*')
  }
  ia=getArea()
  aa=sum(ia)
  pr = readriv()
  oid=getOutlets(pr)
  qr = xl[['rivqdown']][,oid]
  qsf = xl[['rivqsurf']][,]
  qsb = xl[['rivqsub']][,]

  Q = fun(qr, FUN=sum)
  Qsf = fun(qsf, FUN=sum)
  Qsub = fun(qsb, FUN=sum)
  q3=cbind(Q, -Qsf, -Qsub)  / aa
  plot(q3)

  dh = ( Q + Qsf + Qsub ) /aa
  plot(dh)

  x=cbind(dh, q3)
  colnames(x)=c('DH', 'Qout','Qin_sf','Qin_gw')
  if(plot){
    hydrograph(x)
  }
  x
}


#' Calculate the water balance
#' \code{wb.riv}
#' @param xl List of data. Five variables are included: prcp (eleqprcp), et0 (eleqet0), et1 (eleqet1), et2 (eleqet2) and discharge (rivqflx)
#' @param fun function to process the time-series data. Default = apply.daily.
#' @param period Period of the waterbalance
#' @param plot Whether plot the result
#' @return A matrix, contains the colums of water balance factors
#' @export
wb.ele <-function(
  xl=BasicPlot(varname=c(paste0('elev', c('prcp', 'et0', 'et1', 'et2'),
                                'eleq', c('surf', 'sub'),
                                'eley', c('surf', 'unsat', 'gw') ) ) ),
  fun = xts::apply.yearly, period = 'years', plot=TRUE ){
  # xl=BasicPlot(varname=c(paste0('elev', c('prcp', 'et0', 'et1', 'et2') ),
  #                        paste0('eleq', c('surf', 'sub') ),
  #                        paste0('eley', c('surf', 'unsat', 'gw') )  ) )
  # fun = xts::apply.daily
  func <-function(x, w){
    aa= sum(ia)
    y = sweep(x, 2, w, '*')
  }
  fun.first<- function(x, period='month'){
    r <- do.call(rbind, lapply(split(x, period), first))
  }
  fun.last<- function(x, period='month'){
    r <- do.call(rbind, lapply(split(x, period), last))
  }
  ia=getArea()
  aa=sum(ia)
  w=1/ia
  P = fun(xl[['elevprcp']], FUN=mean)
  ET0 = fun( xl[['elevet0']], FUN=mean)
  ET1 = fun( xl[['elevet1']], FUN=mean)
  ET2 = fun( xl[['elevet2']], FUN=mean)
  QS = fun( xl[['eleqsurf']], FUN=mean)
  Qg = fun( xl[['eleqsub']], FUN=mean)
  dys = fun(xts::diff.xts(xl$eleysurf), FUN=colSums, na.rm=T)
  dyg = fun(xts::diff.xts(xl$eleygw), FUN=colSums, na.rm=T)
  dyu = fun(xts::diff.xts(xl$eleyunsat), FUN=colSums, na.rm=T)
  arr=abind::abind(P, ET0, ET1, ET2, QS/aa, Qg/aa, dys, dyu, dyg, along=3)
  dimnames(arr)[[3]] = c('P','ET0','ET1','ET0', 'qs', 'qg','dys','dyu', 'dyg')
  arr
}
