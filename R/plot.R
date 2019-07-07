#' Prepare png figure out.
#' \code{png.control}
#' @param fn Filename
#' @param file Full path of file
#' @param path The path only. Default = anapath in .pihm environment.
#' @param ratio Ratio of width to height.
#' @param ht Height.
#' @param wd Width.
#' @param units Units of width and height.
#' @param res Resolution along height and width.
#' @export
png.control <- function(fn='figure.png',
                        file = file.path(path, fn),
                        path=get('anapath', envir = .pihm),
                        ratio = 9 / 11,
                        ht = 11,
                        wd = ht * ratio,
                        units = 'in', res = 200){
  fd = dirname(file)
  if(!dir.exists(fd)){
    dir.create(fd, showWarnings = F, recursive = T)
  }
  grDevices::png(filename = file, height = ht, width=wd, units = units, res=res)
}
#' Raster plot the mesh data
#' \code{map2d}
#' @param x Vector or Matrix of mesh data. Length of nrow must be equal the number of cells
#' @param sp.riv The SpatialLines of river network. sp.riv=NULL means plot raster only.
#' @return Raster of the x (vector) or the last row of x (matrix)
#' @export
map2d<-function(x=getElevation(),
                sp.riv = NULL ){
  rmask = PIHM.mask()
  if(is.matrix(x) | is.data.frame(x)){
    y = as.numeric(x[nrow(x)])
  }else{
    y=x
  }
  r = MeshData2Raster(y, rmask, stack=FALSE)
  raster::plot(r)
  if(!is.null(sp.riv)){
    #  bgcol= adjustcolor('gray80', alpha.f = 0.8)
    col = grDevices::adjustcolor(sp.riv@data[,'Type'], alpha.f = 0.7)
    lwd=sp.riv@data[,'Type']
    # raster::plot(add=T, sp.riv, col=bgcol, lwd=lwd*3, lty=2)
    raster::plot(add=T, sp.riv, col=col, lwd=lwd)
  }
  graphics::grid()
  r
}

#' Plot hydrograph
#' \code{hydrograph}
#' @param x time-seres matrix. Column 1 is rainfall data.
#' @param legend.position Location to put the legend for discharge.
#' @param unit Unit of the variables.
#' @param col colors of each variable.
#' @param heights Heights of top (rainfall) figure and bottom (discharge) figure
#' @param bg Whether plot the background rectangles.
#' @export
hydrograph <- function(x, legend.position='bottom', unit = rep('', ncol(x)),
                       col = c(3,4), heights = c(3,7), bg=TRUE
                       ){
  time(x) = as.POSIXct(time(x) )
  Time = NULL
  rain = NULL
  varialbe = NULL
  cn=colnames(x)
  pv = as.numeric(x[,1])
  sv = rep(1,length(pv))
  sv[pv<0]=2
  dfp = data.frame('Time' = time(x), 'rain' = pv )
  # head(dfp)

  dfqq =  data.frame('Time' = time(x), x[,-1] )
  dfq = reshape2::melt(dfqq, id='Time')

  plim = range(pv, na.rm = TRUE)
  g.top <- ggplot2::ggplot()
  if(bg){
    r1=rect.ts(x[,1])
    g.top <- g.top+
      ggplot2::geom_rect(data=r1, ggplot2::aes_string(ymin='ys', ymax='ye', xmin='xs',xmax='xe', 
                                                      fill='icol'), alpha =0.1)
  }
  g.top <- g.top +
    ggplot2::coord_cartesian(ylim = plim ) +
    ggplot2::guides(fill = "none") +
    ggplot2::geom_col(data=dfp,ggplot2::aes_string(x = 'Time', y = 'rain'), fill=col[1]) +
    ggplot2::scale_y_continuous(trans = "reverse") +
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
          axis.text.x=ggplot2::element_blank(),
          axis.ticks.x=ggplot2::element_blank()) +
    ggplot2::labs(y = paste( cn[1], unit[1]))

  plim = range(x[,-1], na.rm = TRUE)

  g.bottom <- ggplot2::ggplot()
  if(bg){
    r2=rect.ts(x[,-1])
    g.bottom <- g.bottom+
      ggplot2::geom_rect(data=r2, ggplot2::aes_string(ymin='ys', ymax='ye', xmin='xs',xmax='xe', 
                                                      fill='icol'), alpha =0.1)
  }  
  g.bottom <- g.bottom+
    ggplot2::coord_cartesian(ylim = plim ) +
    ggplot2::guides(fill = "none") +
    ggplot2::geom_line(data=dfq, ggplot2::aes_string(x = 'Time', y = 'value' , color = 'variable')) +
    ggplot2::theme() +
    ggplot2::scale_fill_distiller(palette = "Spectral")+
    ggplot2::labs(x = "Date", y = paste( cn[-1], unit[-1]) )
  if(ncol(x)>2){
    g.bottom  <- g.bottom +
      ggplot2::theme(legend.position=legend.position, legend.direction = 'horizontal',
          legend.title = ggplot2::element_blank())
  }else{
    g.bottom  <- g.bottom +
      ggplot2::theme(legend.position='none')
  }

  gA <- ggplot2::ggplotGrob(g.top)
  gB <- ggplot2::ggplotGrob(g.bottom)
  maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
  gA$widths[2:5] <- as.list(maxWidth)
  gB$widths[2:5] <- as.list(maxWidth)
  p <- gridExtra::grid.arrange(gA, gB, ncol=1,heights = heights )
}

#' Plot xts data..
#' \code{plot_tsd}
#' @param x xts data
#' @param time.col Time interval of banded background. Default is 'year'
#' @examples
#' library(xts)
#' nday = 1000
#' xd=as.POSIXct(as.Date('2000-01-01')+ 1:nday )
#' x=as.xts(sin(1:1000 / 100), order.by=xd)
#' plot_tsd(x)
#' @export
plot_tsd <- function(x, time.col='year'){
  time(x) = as.POSIXct(time(x) )
  tx = time(x)
  if(grepl(time.col,'year')){
    ty = format(tx, '%Y')
  }else{
    ty = format(tx, '%Y%m')
  }
  dd=data.frame(tx,
                year=ty,
                zoo::coredata(x))
  colnames(dd) = c('Time','Col', 'Data')
  rr=rect.ts(x, time.col=time.col)
  plim=range(x)
  print(plim)
  ggplot2::ggplot() +
    ggplot2::geom_rect(data=rr, ggplot2::aes_string(ymin='ys', ymax='ye', xmin='xs',xmax='xe', fill='icol'), alpha =0.2, show.legend = FALSE)+
    ggplot2::coord_cartesian(ylim = plim ) +
    ggplot2::geom_line(dat=dd, ggplot2::aes_string(x='Time', y='Data', col='Col'),show.legend = FALSE)
}
#' Find the rect for banded background.
#' @param x xts data
#' @param time.col Time interval of banded background. Default is 'year'
rect.ts <- function(x, time.col='year'){
  time(x) = as.POSIXct(time(x) )
  tx = time(x)
  if(grepl(time.col,'year')){
    ty = format(tx, '%Y')
  }else{
    ty = format(tx, '%Y%m')
  }
  ylim = range(x)
  xr = as.numeric(sort(unique(ty)))
  ny=length(xr)
  x1=ifelse(length(xr)>1, xr[-length(xr)], xr)
  # xs=as.POSIXct(paste0(x1, '-01-01')  )
  xs=as.POSIXct(paste0(xr, '-01-01')  )
  xs[1] = max(xs[1], xts::first(tx))
  x2=ifelse(length(xr)>1, xr[-1], xr)
  # xe=as.POSIXct(paste0(x2, '-12-31') )
  xe=as.POSIXct(paste0(xr, '-12-31') )
  xe[length(xe)] = min(xe[length(xe)], xts::last(tx))
  ys= rep(floor(min(x) ), max(ny,ny-1) )
  ye= rep(ceiling(max(x)*2 ), max(ny,ny-1) )
  # reshape2::melt(dd)
  # icol=xr[-1]
  icol = rep(1:2, ny)[1:ny]
  rr = data.frame(xs,xe,ys,ye, icol)
  rr
}
