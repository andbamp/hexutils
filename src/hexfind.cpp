#include <Rcpp.h>
using namespace Rcpp;

//' Hexfind cpp
//'
//' @export
// [[Rcpp::export]]
IntegerVector hexfind_cpp(RawVector x, RawVector block) {
  int x_size = x.size();
  int block_size = block.size();
  
  if (block_size > x_size) return NULL;
  IntegerVector match_ind(x_size - block_size + 1);
  int m = 0;
  bool match;
  int count = 0;
  
  for (int i = 0; i < x_size - block_size + 1; ++i) {
    count = 0;
    for (int j = 0; j < block_size; ++j) {
      match = x[i + j] == block[j];
      if (!match) break;
      count++;
    }
    if (count == block_size) {
      match_ind[m++] = i + 1;
    }
  }
  
  if (m == 0) return NULL;
  return match_ind[seq(0, m - 1)];
}
