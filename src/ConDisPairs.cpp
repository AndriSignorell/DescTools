

//   Rewritten by Andri Signorell in C++ 

#include <Rcpp.h>
#include <vector>
using namespace Rcpp;
using namespace std;

// 2D Binary Indexed Tree
class BIT2D {
  public:
    vector<vector<int>> tree;
  int n, m;
  
  BIT2D(int rows, int cols) : n(rows), m(cols) {
    tree.resize(n + 2, vector<int>(m + 2, 0));
  }
  
  void update(int x, int y, int val) {
    for (int i = x + 1; i <= n + 1; i += i & -i) {
      for (int j = y + 1; j <= m + 1; j += j & -j) {
        tree[i][j] += val;
      }
    }
  }
  
  int query(int x, int y) const {
    int sum = 0;
    for (int i = x + 1; i > 0; i -= i & -i) {
      for (int j = y + 1; j > 0; j -= j & -j) {
        sum += tree[i][j];
      }
    }
    return sum;
  }
  
  int range(int x1, int y1, int x2, int y2) const {
    return query(x2, y2) - query(x1 - 1, y2) - query(x2, y1 - 1) + query(x1 - 1, y1 - 1);
  }
};

// [[Rcpp::export]]
List ConDisPairs(IntegerMatrix x) {
  int n = x.nrow(), m = x.ncol();
  IntegerMatrix pi_c(n, m), pi_d(n, m);
  
  BIT2D bit(n, m);
  
  // Fill the BIT
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      if (x(i, j) != 0) {
        bit.update(i, j, x(i, j));
      }
    }
  }
  
  // Compute pi_c and pi_d
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      int a = bit.range(0, 0, i - 1, j - 1);
      int b = bit.range(i + 1, j + 1, n - 1, m - 1);
      pi_c(i, j) = a + b;
      
      int c = bit.range(0, j + 1, i - 1, m - 1);
      int d = bit.range(i + 1, 0, n - 1, j - 1);
      pi_d(i, j) = c + d;
    }
  }
  
  long long C = 0, D = 0;
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      C += static_cast<long long>(pi_c(i, j)) * x(i, j);
      D += static_cast<long long>(pi_d(i, j)) * x(i, j);
    }
  }
  
  return List::create(
    Named("pi.c") = pi_c,
    Named("pi.d") = pi_d,
    Named("C") = C / 2.0,
    Named("D") = D / 2.0
  );
}
