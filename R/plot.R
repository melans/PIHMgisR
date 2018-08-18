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
  png(filename = file, height = ht, width=wd, units = units, res=res)
}
#' Raster plot the mesh data
#' \code{map2d}
#' @param x Vector or Matrix of mesh data. Length of nrow must be equal the number of cells
#' @param sp.riv The SpatialLines of river network. sp.riv=NULL means plot raster only.
#' @return Raster of the x (vector) or the last row of x (matrix)
#' @export  
map2d<-function(x=getElevation(),
                sp.riv = sp.riv2shp() ){
  rmask = PIHM.mask()
  if(is.matrix(x) | is.data.frame(x)){
    y = as.numeric(x[nrow(x)])
  }else{
    y=x
  }
  r = MeshData2Raster(y, rmask)
  raster::plot(r)
  if(is.null(sp.riv)){
    #  bgcol= adjustcolor('gray80', alpha.f = 0.8)
    col = adjustcolor(sp.riv@data[,'Type'], alpha.f = 0.7)
    lwd=sp.riv@data[,'Type']
    # raster::plot(add=T, sp.riv, col=bgcol, lwd=lwd*3, lty=2)
    raster::plot(add=T, sp.riv, col=col, lwd=lwd)
  }
  grid()
  r
}

#' Plot hydrograph
#' \code{hydrograph}
#' @param x time-seres matrix. Column 1 is rainfall data.
#' @param legend.position Location to put the legend for discharge.
#' @param unit Unit of the variables.
#' @param col colors of each variable.
#' @param heights Heights of top (rainfall) figure and bottom (discharge) figure
#' @export  
hydrograph <- function(x, legend.position='bottom', unit = rep('', ncol(x)),
                       col = c(3,4), heights = c(3,7)
                       ){
  cn=colnames(x)
  dfp = data.frame('Time' = time(x), 'rain' = as.numeric(x[,1]) )
  # head(dfp)
  
  dfqq =  data.frame('Time' = time(x), x[,-1] )
  dfq = reshape2::melt(dfqq, id='Time')
  
  g.top <- ggplot(dfp, aes(x = Time, y = rain, ymin=0, ymax=rain) ) +
    geom_col( fill=col[1]) +
    scale_y_continuous(trans = "reverse") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    labs(y = paste( cn[1], unit[1]))
  
  g.bottom <- ggplot(dfq, aes(x = Time, y = value , color = variable)) +
    geom_line() +
    theme() +
    scale_fill_distiller(palette = "Spectral")+
    labs(x = "Date", y = paste( cn[-1], unit[-1]) ) 
  if(ncol(x)>2){
    g.bottom  <- g.bottom + theme(legend.position=legend.position, legend.direction = 'horizontal', 
          legend.title = element_blank())
  }else{
    g.bottom  <- g.bottom + theme(legend.position='none')
  }
  
  gA <- ggplotGrob(g.top)
  gB <- ggplotGrob(g.bottom)
  maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
  gA$widths[2:5] <- as.list(maxWidth)
  gB$widths[2:5] <- as.list(maxWidth)
  p <- gridExtra::grid.arrange(gA, gB, ncol=1,heights = heights )
}
