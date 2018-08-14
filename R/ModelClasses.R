
#' Class of PIHM.mesh
#' \code{PIHM.mesh} 
#' @slot mesh data.frame or matrix
#' @slot point data.frame or matrix
#' @return Class of PIHM.mesh
#' @export
PIHM.mesh <- methods::setClass("PIHM mesh",
                      slots = c(mesh="data.frame", 
                                point="data.frame"))
#' Class of PIHM.river
#' \code{PIHM.river} 
#' @slot river data.frame or matrix
#' @slot rivertype data.frame or matrix
#' @slot point data.frame or matrix
#' @return Class of PIHM.river
#' @export
PIHM.river <- methods::setClass("PIHM River", 
                      slots = c(river="data.frame", 
                                rivertype="data.frame",
                                point = 'data.frame'))

#' This is data to be included in my package
#' @name sh
#' @docType data
#' @keywords data
NULL

#' This is data to be included in my package
#' @name sac2
#' @docType data
#' @keywords data
NULL