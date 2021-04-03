#include <Rcpp.h>
using namespace Rcpp;

//' Hexfind cpp
//'
//' @export
// [[Rcpp::export]]
LogicalVector hexfind_cpp(RawVector x, RawVector block) {
  int xSize = x.size();
  int bSize = block.size();
  LogicalVector res(xSize);
  LogicalVector match(bSize);
  
  for (int i = 0; i < xSize; ++i) {
    for (int j = 0; j < bSize; ++j) {
      match[j] = x[i + j] == block[j];
    }
    if (sum(match) == bSize) {
      res[i] = 1;
    } else {
      res[i] = 0;
    }
  }
  
  return res;
}
