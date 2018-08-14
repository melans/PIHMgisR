
#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;

//' Identify whether every row of \code{m} is same as \code{v};
//' @param x required
//' @param m required
//' @return TRUE/FALSE of matching
// [[Rcpp::export]]
LogicalVector rowMatch(NumericVector x, NumericMatrix m) {
  int n = x.size();
  int nr = m.nrow();
  int nc = m.ncol();
  LogicalVector  out(nr, false);
  int i,j;
  if(n != 2 || nc != 2){
    return 0;
  }
  for(i = 0; i < nr; i++){
    // std::cout << m(i, 0) << "\t" << m(i,1) << std::endl;
    // std::cout << x(0) << "\t" << x(1) << std::endl;
    if( m(i, 0) == x[1] && m(i, 1) == x(0) ){
      out(i) = true;
    }else if( m(i, 1) == x[1] && m(i, 0) == x(0)  ){
      out(i) = true;
    }
  }
  return out;
}
