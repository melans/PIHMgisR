#' Write ESRI shapefile out
#' \code{writeshape} 
#' @param shp Spatial file
#' @param crs projection 
#' @param file file path, without '.shp'.
writeshape <- function(shp,crs = crs(shp), file=NULL){
  if(grepl(class(shp)[1],'SpatialPolygons' ) ){
    shp = methods::as(shp, "SpatialPolygonsDataFrame")
  }else if ( grepl(class(shp)[1],'SpatialLines' )   ){
    shp = methods::as(shp, "SpatialLinesDataFrame")
  }
  if( is.null(file) ){
    # message('No file exported')
  }else{
    path = dirname(file)
    fn = basename(file)
    if(!dir.exists(path)){
      dir.create(path, showWarnings = T, recursive = T)
    }
    raster::crs(shp) = crs;
    prj = sp::proj4string(shp)
    rgdal::writeOGR(obj=shp, driver = 'ESRI Shapefile',
             layer=fn,
             dsn=path, overwrite_layer = T)
    if( !is.na(crs) ){
      fn.prj = file;
      raster::extension(fn.prj) = '.prj'
      invisible(rgdal::showWKT(prj, file = fn.prj))
    }
    message(file, ' is saved')
  }
}
