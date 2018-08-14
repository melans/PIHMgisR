#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;

//' Determine the topological relation among triangles. 
//' @param tri triangle defination, m x 3
//' @return topological relation of triangles. m x 4; cols = c(ID, Nabor1, Nabor2, Nabor3)
// [[Rcpp::export]]
NumericMatrix triTopology(NumericMatrix tri) {
  int n = tri.nrow();
  int flag;
  int nid[4] = {0,1,2,0};
  int ed0[2];
  int ed1[2];
  NumericMatrix out(n, 4);
  for(int i=0; i < n; i++){
    for(int j=0; j < 3; j++){
      ed0[0]=  tri(i, nid[j]);
      ed0[1] =  tri(i, nid[j+1]);
      flag = 0;
      for(int ii=0; ii < n && !flag; ii++){
        if(i == ii){
          continue;
        }
        for(int jj=0;jj<3 && !flag;jj++){
          ed1[0]=  tri(ii, nid[jj]);
          ed1[1] =  tri(ii, nid[jj+1]);
          if(ed0[0] ==  ed1[0]){
            if(ed0[1]== ed1[1]){
              flag = ii + 1;
            }
          }else if(ed0[1] ==  ed1[0]){
            if(ed0[0] ==  ed1[1]){
              flag = ii + 1 ;
            }
          }
        }
      }
      out(i, j+1) = flag;
    } // for of three edges.
    out(i, 0) = i;
  }// for of nrow
  return  out;
}