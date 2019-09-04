#' extract Coordinates of SpatialLines or  SpatialPolygons
#' \code{pihm.init} 
#' @param ncell Number of triangles in PIHM mesh domain
#' @param nriv Number of river lines
#' @param AqD Default aquifer depth 
#' @param stage Default river stage
#' @param p1 Fraction of unsat zone, [0,1]
#' @param p2 Fraction of Groundwater, [0,1]
#' @return List of initial condition
#' @export
pihm.init <- function(ncell, nriv, AqD = 10, stage = 0.1, p1=0.4, p2 = p1){
  mi = data.frame(cbind(1:ncell, rep(0, ncell), 0, 0,
             AqD * p1, AqD * p2))
  ri = data.frame(cbind(1:nriv, rep(stage, nriv)))
  colnames(mi) = c('Index', 'Canopy', 'Snow', 'Surface', 'Unsat', 'GW')
  colnames(ri) = c('Index', 'Stage')
  ret = list('minit' = mi, 'rinit' = ri)
}
#' Generate the default PIHM model configuration
#' \code{pihm.para} 
#' @param nday Simulation periods (days)
#' @return model configureation, Vector
#' @export
pihmpara <- function( nday = 10){
  dts = c(
    paste0('dt_',
           c(paste0('ye_', c('snow', 'surf', 'unsat', 'gw') ), 
             paste0('Qe_', c('surf', 'sub') ),
             paste0('qe_et'), 
             paste0('qe_', c('prcp', 'infil', 'rech') ),
             paste0('yr_', c('stage')),
             paste0('Qr_', c('down', 'surf', 'sub', 'up'))
           ))
  )
  
  vdt = rep(1440, length(dts))
  
  vn = c('VERBOSE', 'INIT_MODE',
         'ASCII_OUTPUT', 'Binary_OUTPUT', 
         'SpinupDay', 'NUM_OPENMP', 
         'ABSTOL', 'RELTOL', 
         'INIT_SOLVER_STEP', 'MAX_SOLVER_STEP', 'LSM_STEP', 
         'START', 'END', 
         dts)
  v = c(0,  2,
        0, 1,
        0, 8,
        1e-4, 1e-3,
        1e-3, 60, 60,
        0, nday,
        vdt
  )
  v=data.frame(rbind(v))
  names(v) = vn
  v
}
#' Generate the default PIHM model calibration
#' \code{pihmcalib} 
#' @return Default calibration values for PIHM
#' @export
pihmcalib <- function(){
  cn = toupper( c('KSATH', 'KSATV', 'KINF', 'KMACSATH', 'KMACSATV', 'DINF', 'DROOT', 'DMAC',
                  'THETAS','THETAR', 'ALPHA', 'BETA', 'MACVF', 'MACHF', 'VEGFRAC', 'ALBEDO', 'ROUGH',
                  'Aquifer', 
                  'PRCP', 'SFCTMP', 'ETP', 'EC', 'ETT', 'EDIR',
                  'RIV_ROUGH', 'RIV_KH', 'RIV_DPTH',
                  'RIV_WDTH', 'RIV_SINU', 'RIV_CWR', 'RIV_BSLOPE',
                  'Soil_Dgd', 'ImpAF', 'ISMAX') )
  v=data.frame(rbind(rep(1, length(cn))))
  names(v) = cn
  v['SFCTMP'] = 0  
  v['AQUIFER'] = 0  
  v['RIV_DPTH'] = 0  
  v['RIV_WDTH'] = 0  
  v['RIV_BSLOPE'] = 0  
  v
}
