#' Linefit the observation and simulation
#' \code{LineFit} 
#' @param x Observation data or matrix/data.frame consist of observation and simulation
#' @param y Simulation data
#' @param xlab xlab, default is 'Observation'
#' @param ylab ylab, default is 'Simulation'
#' @param if.fitline Whether plot the fiting line.
#' @param lim limits of x and y axis.
#' @param ... more options for plot()
#' @return Triangle mesh of model domain.
#' @export
#' @examples 
#' obs=rnorm(100)
#' sim=obs+rnorm(100)/2
#' LineFit(cbind(obs,sim))
LineFit <-function(x, y=NULL,
                   xlab='Observation', ylab='Simulation', 
                   if.fitline=TRUE, lim,...){
  colnames(x) = c(xlab, ylab)
  if(is.null(y)){
    df = as.data.frame(x)
  }else{
    df = data.frame(x,y)
  }
  lm_eqn <- function(df){
    m <- stats::lm(x ~ y, df);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                     list(a = format(stats::coef(m)[1], digits = 2), 
                          b = format(stats::coef(m)[2], digits = 2), 
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));                 
  }
  ggplot2::ggplot(data=df, 
                  ggplot2::aes_string(x=xlab, y=ylab)) + 
    ggplot2::geom_smooth(method = "lm", se = T, col=2) +
    ggplot2::geom_point() + 
    ggplot2::geom_abline() + 
    ggplot2::coord_fixed(ratio=1)
}
#' Default Comparison of the observation and simulation
#' \code{QvsO} 
#' @param qq Matrix or data.frame. Column 1 is observation, while colum 2 is simulation.
#' @param sim Simulation time-seris data;
#' @param obs Observation time-seris data;
#' @param ... more options for plot()
#' @export
#' @examples 
#' obs=rnorm(100)
#' sim=obs+rnorm(100)/2
#' QvsO(cbind(obs,sim))
QvsO <-function(qq,sim=qq[,2],obs=qq[,1], ...){
  hydroGOF::ggof(sim=sim,obs=obs,col=c( 'blue','red'),...)
}
