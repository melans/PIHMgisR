// 
// #include <Rcpp.h>
// using namespace Rcpp;
// //' Identify the commond rows in two matrix
// //' @param x,y Matrix
// //' @return TRUE/FALSE of rows in matrix(x) which is common in x and y.
// //' @useDynLib PIHMgisR
// //' @export
// // [[Rcpp::export]]
// LogicalVector CommonRows(NumericMatrix x, NumericMatrix y) {
//   int nr = x.nrow();
//   int nc = x.ncol();
//   int yr = y.nrow();
//   int yc = y.ncol();
//   int flag;
//   
//   LogicalVector  out(nr, false);
//   if(nc != yc){
//     //nothing to do.
//     return(out);
//   }
//   // printf("%d\t%d\n", nr, nc);
//   for(int ix = 0; ix < nr; ix++){
//     flag = 1;
//     for(int iy = 0; iy < yr; iy++){
//       // printf("%d/%d\t%f\t%f\n", i, j, x(i,j), y(i,j));
//       for(int j = 0; j < nc; j++){
//         if( x(ix, j) == y(iy, j) ){
//           // j member match.
//         }else{
//           // doesn't match
//           flag = 0;
//           break;
//         }
//       }
//       if( flag > 0 ){//find match
//         break;
//       }else{
//         //go on for next row in Y;
//       }
//     }
//     if(flag){
//       // success
//       out(ix) = true;
//     }
//   }
//   
//   return out;
// }