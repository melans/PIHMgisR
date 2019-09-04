#' write the configuration for CMA-ES method
#' \code{write.cmaes} 
#' @param x Configuration of CMA-ES calibration method.
#' @param file full path of file
#' @param backup TRUE/FALSE
#' @return Parameters for CMA-ES configuration
#' @export
write.cmaes <- function(x=NULL, file, backup = TRUE){
  if(is.null(x)){
    cn = c('lambda',
           'ncores',
           'maxgen',
           'stopfitness',
           'sigma',
           'updateic',
           'nspingup',
           'walltime',
           'tsd_obs',
           'path_out'
    )
    x=cbind(48, 48, 100, 0.3, 0.8, 0, 0, 86400)
    x = data.frame(x)
    x=cbind(x, 'xxx', 'cmaes_out')
    names(x)=cn
    write.cmaes(x, file)
  }else{
    write.config(x, file=file, backup = backup)
  }
  return(x)
}

