

//   Rewritten by Andri Signorell in C++ 

#include <Rcpp.h>
#include <vector>
#include <unordered_map>
#include <set>
#include <algorithm>
using namespace Rcpp;
using namespace std;

// 2D Binary Indexed Tree (Fenwick Tree)
struct BIT2D {
  int size_x, size_y;
  vector<vector<int>> tree;
  
  BIT2D(int sx, int sy) : size_x(sx), size_y(sy), tree(sx + 2, vector<int>(sy + 2, 0)) {}
  
  void update(int x, int y, int val) {
    for (int i = x + 1; i <= size_x + 1; i += i & -i)
      for (int j = y + 1; j <= size_y + 1; j += j & -j)
        tree[i][j] += val;
  }
  
  int query(int x, int y) const {
    int sum = 0;
    for (int i = x + 1; i > 0; i -= i & -i)
      for (int j = y + 1; j > 0; j -= j & -j)
        sum += tree[i][j];
    return sum;
  }
  
  int query_range(int x1, int y1, int x2, int y2) const {
    return query(x2, y2) - query(x1 - 1, y2) - query(x2, y1 - 1) + query(x1 - 1, y1 - 1);
  }
};

// [[Rcpp::export]]
List ConDisPairsXY(NumericVector x, NumericVector y) {
  int n = x.size();
  if (y.size() != n) stop("x and y must be the same length");
  
  vector<pair<double, double>> valid_pairs;
  for (int i = 0; i < n; ++i) {
    if (!NumericVector::is_na(x[i]) && !NumericVector::is_na(y[i])) {
      valid_pairs.emplace_back(x[i], y[i]);
    }
  }
  
  // Coordinate compression
  set<double> x_set, y_set;
  for (auto& p : valid_pairs) {
    x_set.insert(p.first);
    y_set.insert(p.second);
  }
  
  unordered_map<double, int> x_map, y_map;
  int idx = 0;
  for (double v : x_set) x_map[v] = idx++;
  int x_size = idx;
  idx = 0;
  for (double v : y_set) y_map[v] = idx++;
  int y_size = idx;
  
  // Compress points
  vector<pair<int, int>> points;
  unordered_map<int, int> count_x;
  unordered_map<int, int> count_y;
  unordered_map<long long, int> count_xy;
  
  for (auto& p : valid_pairs) {
    int xi = x_map[p.first];
    int yi = y_map[p.second];
    points.emplace_back(xi, yi);
    count_x[xi]++;
    count_y[yi]++;
    long long key = ((long long)xi << 32) | yi;
    count_xy[key]++;
  }
  
  // Sort and initialize BIT
  sort(points.begin(), points.end());
  BIT2D bit(x_size, y_size);
  
  long long C = 0, D = 0;
  
  for (size_t i = 0; i < points.size(); ++i) {
    int xi = points[i].first;
    int yi = points[i].second;
    
    int concordant = bit.query_range(0, 0, xi - 1, yi - 1)
      + bit.query_range(xi + 1, yi + 1, x_size - 1, y_size - 1);
    
    int discordant = bit.query_range(0, yi + 1, xi - 1, y_size - 1)
      + bit.query_range(xi + 1, 0, x_size - 1, yi - 1);
    
    C += concordant;
    D += discordant;
    
    bit.update(xi, yi, 1);
  }
  
  // Ties X and Y (corrected for double-counted ties)
  long long Ties_X = 0, Ties_Y = 0;
  for (auto& kv : count_x) {
    long long g = kv.second;
    Ties_X += g * (g - 1) / 2;
  }
  for (auto& kv : count_y) {
    long long g = kv.second;
    Ties_Y += g * (g - 1) / 2;
  }
  for (auto& kv : count_xy) {
    long long g = kv.second;
    Ties_X -= g * (g - 1) / 2;
    Ties_Y -= g * (g - 1) / 2;
  }
  
  return List::create(
    Named("C") = C,
    Named("D") = D,
    Named("Ties_X") = Ties_X,
    Named("Ties_Y") = Ties_Y
  );
}
