#' Write ESRI shapefile out
#' \code{writeshape}
#' @param shp Spatial file
#' @param crs projection
#' @param file file path, without '.shp'.
#' @export
#' @examples
#' library(sp)
#' library(rgeos)
#' library(rgdal)
#' sp1 = readWKT("POLYGON((0 0, 0 1, 1 1, 1 0, 0 0))")
#' raster::crs(sp1) =sp::CRS("+init=epsg:4326")
#' writeshape(sp1, file=file.path(tempdir(), 'sp1'))
#' sp2=readOGR(file.path(tempdir(), 'sp1.shp'))
#' plot(sp2)
writeshape <- function(shp, file=NULL, crs = raster::crs(shp)){
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
