
#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;

//' Calculate the centroid of polygons, ncol of \code{poly} must be equal or larger than 3.
//' @param poly, polygon consist of \code{points} Index 
//' @param points, M x 2 matrix, (x,y) of points.
//' @return coordinates of the centroids of \code{poly}
// [[Rcpp::export]]
NumericMatrix getCentroid(NumericMatrix poly, NumericMatrix points) {
  int nr = poly.nrow();
  int nc = poly.ncol();
  double x,y;
  NumericMatrix  out(nr, nc);
  int i,j;
  for(i = 0; i < nr; i++){
    x = 0.;
    y = 0.;
    for(j = 0; j < nc; j++){
      x += points(poly(i,j) - 1, 0);
      y += points(poly(i,j) - 1, 1);
    }
    out(i,0) = x / nc;
    out(i,1) = y / nc;
  }
  return out;
}
