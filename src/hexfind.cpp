#include <Rcpp.h>
using namespace Rcpp;

//' Hexfind cpp
//'
//' @export
// [[Rcpp::export]]
IntegerVector hexfind_cpp(RawVector x, RawVector block) {
  int x_size = x.size();
  int block_size = block.size();
  
  int max_m = 100;
  IntegerVector match_ind(max_m);
  
  bool match;
  int m = 0;
  int count = 0;
  
  for (int i = 0; i < x_size; ++i) {
    count = 0;
    for (int j = 0; j < block_size; ++j) {
      match = x[i + j] == block[j];
      if (!match) break;
      count++;
    }
    if (count == block_size) {
      match_ind[m++] = i + 1;
      if (m > max_m) {
        stop("Error: Too many matches");
        exit(1);
      }
    }
  }
  
  return match_ind[seq(0, m - 1)];
}
