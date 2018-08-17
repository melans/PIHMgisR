#' Read the matrix or data.frame file which has (nrow, ncol) in header
#' \code{read.df} 
#' @param file full path of file
#' @param text text return from readLines(file)
#' @return a list of matrix
#' @export
read.df <-function(file, text = readLines(file)){
  # fn=fin['md.mesh']
  # text=readLines(fn)
  r0 = 1
  nrow = length(text)
  xl = list()
  for(i in 1: 100){
    ndim = as.numeric(utils::read.table(text = text[r0] ))
    nr = ndim[1]
    nc = ndim[2]
    xl[[i]] = as.matrix(utils::read.table(text = text[0:nr + 1 + r0], header = T))
    r0 = r0 + nr + 2;
    if(r0 + 1 > nrow){
      break
    }
  }
  xl
}
#' Read the .mesh file
#' \code{readmesh} 
#' @param file full path of .mesh file
#' @return pm \code{PIHM.mesh}
#' @export
readmesh <-function(file = PIHM.filein()['md.mesh'] ){
  d = read.df(file = file);
  ret <- PIHM.mesh(mesh = data.frame(d[[1]]),
                   point = data.frame(d[[2]]) )
}

#' Read the .att file
#' \code{readmesh} 
#' @param file full path of file
#' @return matrix
#' @export
readatt <-function( file = PIHM.filein()['md.att']){
  x = read.df(file)
  x
}

#' Read the .riv file
#' \code{readriv} 
#' @param file full path of .riv file
#' @return pr \code{PIHM.riv}
#' @export
readriv <-function(file = PIHM.filein()['md.riv'] ){
  d = read.df(file = file);
  ret <- PIHM.river(river = data.frame(d[[1]]), 
                    rivertype = data.frame(d[[2]]),
                    point = data.frame(d[[3]]) )
}

#' Read the .rivchn file
#' \code{readchannel} 
#' @param file full path of .rivchn file
#' @return matrix
#' @export
readchannel <-function(file = PIHM.filein()['md.chn'] ){
  d = read.df(file = file);
  ret <- d
}

#============
#' Read the .para or .calib file
#' \code{readpc} 
#' @param file full path of file
#' @return .para or .calib
#' @export
readpc <- function(file = PIHM.filein()['md.para'] ){
  # file = fin['md.para']
  tline = readLines(file, skipNul = TRUE)
  tmp = which(grepl('[:Alpha:]', tline) | grepl('[:alpha:]', tline))
  
  value = as.data.frame( utils::read.table(text = tline, header=FALSE, row.names= 1) )
  para = unlist(value)
  names(para) = toupper(rownames(value))
  para
}


#============
#' Read the .ic file
#' \code{readic} 
#' @param file full path of file
#' @return .ic
#' @export
readic <- function(
  file = PIHM.filein()['md.ic']){
  # file = fin['md.ic']
  x=read.df(file)
  cn = c('minit', 'rinit', 'linit')
  names(x) = cn[1:length(x)]
  x
}

#' Read the .soil file
#' \code{readsoil} 
#' @param file full path of file
#' @return .soil
#' @export
readsoil <-function(file =  PIHM.filein()['md.soil']){
  x=read.df(file)
  x
}

#============
#' Read the .geol file
#' \code{readgeol} 
#' @param file full path of file
#' @return .geol
#' @export
readgeol <-function(file =  PIHM.filein()['md.geol']){
  x=read.df(file)
  x
}

#============ 
#' Read the .lc file
#' \code{readlc} 
#' @param file full path of file
#' @return .lc
#' @export
readlc <-function( file = PIHM.filein()['md.lc']){
  x=read.df(file)
  x
}
