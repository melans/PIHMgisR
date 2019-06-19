#' Calculate the water balance
#' \code{p.waterbalance}
#' @param xl List of data. Five variables are included: prcp (eleqprcp), etic (eleqetic), ettr (eleqettr), etev (eleqetev) and discharge (rivqflx)
#' @param fun function to process the time-series data. Default = apply.daily.
#' @param plot Whether plot the result
#' @return A matrix, contains the colums of water balance factors
#' @export
wb.all <-function(
  xl=BasicPlot(varname=c(paste0('elev', c('prcp', 'etic', 'ettr', 'etev') )
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
  IC = fun( func(xl[['elevetic']], w ), FUN=sum)
  ET = fun( func(xl[['elevettr']], w ), FUN=sum)
  EV = fun( func(xl[['elevetev']], w ), FUN=sum)
  dh = P-Q-IC-EV-ET
  x=cbind(P,Q,IC, ET,EV)
  colnames(x)=c('P','Q','ET_IC', 'ET_TR','ET_EV')
  y = cbind(dh, x)
  colnames(y)=c('DH', 'P','Q','ET_IC', 'ET_TR','ET_EV')
  if(plot){
      PIHMgisR::hydrograph(y)
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
#' @param xl List of data. Five variables are included: prcp (eleqprcp),and discharge ()
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
#' @param xl List of data. Five variables are included: 
#' @param fun function to process the time-series data. Default = apply.daily.
#' @param period Period of the waterbalance
#' @param plot Whether plot the result
#' @return A matrix, contains the colums of water balance factors
#' @export
wb.ele <-function(
  xl=BasicPlot(varname=c(paste0('elev', c('prcp', 'etic', 'ettr', 'etev') ),
                         paste0('eleq', c('surf', 'sub') ),
                         paste0('eley', c('surf', 'unsat', 'gw')  ) ) ),
  fun = xts::apply.yearly, period = 'years', plot=TRUE ){
  # xl=BasicPlot(varname=c(paste0('elev', c('prcp', 'etic', 'ettr', 'etev')),
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
  IC = fun( xl[['elevetic']], FUN=mean)
  ET = fun( xl[['elevettr']], FUN=mean)
  EV = fun( xl[['elevetev']], FUN=mean)
  QS = fun( xl[['eleqsurf']], FUN=mean)
  Qg = fun( xl[['eleqsub']], FUN=mean)
  dys = fun(xts::diff.xts(xl$eleysurf), FUN=colSums, na.rm=T)
  dyg = fun(xts::diff.xts(xl$eleygw), FUN=colSums, na.rm=T)
  dyu = fun(xts::diff.xts(xl$eleyunsat), FUN=colSums, na.rm=T)
  arr=abind::abind(P, IC, ET, EV, QS/aa, Qg/aa, dys, dyu, dyg, along=3)
  dimnames(arr)[[3]] = c('P','ET_IC', 'ET_TR','ET_EV', 'qs', 'qg','dys','dyu', 'dyg')
  arr
}
