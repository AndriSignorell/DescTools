
// [[Rcpp::plugins(cpp11)]]


#include <Rcpp.h>
#include <utility>
#include <queue>

using namespace Rcpp;
using namespace std;


// this returns the largest values with respect of ties and their frequencies
// source Nathan Russell
// http://stackoverflow.com/questions/36993935/find-the-largest-n-unique-values-and-their-frequencies-in-r-and-rcpp/36994047?noredirect=1#comment61629124_36994047
// implemented Andri: 13.05.2016


class hist_top {
private:
  struct paired {
    typedef std::pair<double, unsigned int> pair_t;

    pair_t pair;
    unsigned int is_set;

    paired()
      : pair(pair_t()),
        is_set(0)
    {}

    paired(double x)
      : pair(std::make_pair(x, 1)),
        is_set(1)
    {}

    bool operator==(const paired& other) const {
      return pair.first == other.pair.first;
    }

    bool operator==(double other) const {
      return is_set && (pair.first == other);
    }

    bool operator>(double other) const {
      return is_set && (pair.first > other);
    }

    bool operator<(double other) const {
      return is_set && (pair.first < other);
    }

    paired& operator++() {
      ++pair.second;
      return *this;
    }

    paired operator++(int) {
      paired tmp(*this);
      ++(*this);
      return tmp;
    }
  };

  struct greater {
    bool operator()(const paired& lhs, const paired& rhs) const {
      if (!lhs.is_set) return false;
      if (!rhs.is_set) return true;
      return lhs.pair.first > rhs.pair.first;
    }
  };

  typedef std::priority_queue<
    paired,
    std::vector<paired>,
    greater
  > queue_t;

  unsigned int sz;
  queue_t queue;

  void insert(double x) {
    if (queue.empty()) {
      queue.emplace(x);
      return;
    }

    if (queue.top() > x && queue.size() >= sz) return;

    queue_t qtmp;
    bool matched = false;

    while (queue.size()) {
      paired elem = queue.top();
      if (elem == x) {
        qtmp.emplace(std::move(++elem));
        matched = true;
      } else {
        qtmp.emplace(std::move(elem));
      }
      queue.pop();
    }

    if (!matched) {
      if (qtmp.size() >= sz) qtmp.pop();
      qtmp.emplace(x);
    }

    std::swap(queue, qtmp);
  }

public:
  hist_top(unsigned int sz_)
    : sz(sz_),
      queue(queue_t())
  {}

  template <typename InputIt>
  void insert(InputIt first, InputIt last) {
    while (first != last) insert(*first++);
  }

  // Rcpp::List get() const {
  //   Rcpp::NumericVector values(sz);
  //   Rcpp::IntegerVector freq(sz);
  //   R_xlen_t i = 0;
  //
  //   queue_t tmp(queue);
  //   while (tmp.size()) {
  //     values[i] = tmp.top().pair.first;
  //     freq[i] = tmp.top().pair.second;
  //     ++i;
  //     tmp.pop();
  //   }
  //
  //   return Rcpp::List::create(
  //     Rcpp::Named("value") = values,
  //     Rcpp::Named("frequency") = freq);
  // }

  Rcpp::List get() const {
    Rcpp::NumericVector values(sz);
    Rcpp::IntegerVector freq(sz);
    R_xlen_t i = 0;

    queue_t tmp(queue);
    while (tmp.size()) {
      paired p = tmp.top();
      if (p.is_set) {
        values[i] = p.pair.first;
        freq[i] = p.pair.second;
        ++i;
      }
      tmp.pop();
    }

    if (!i) {
      return Rcpp::List::create(
        Rcpp::Named("value") = Rcpp::NumericVector::create(),
        Rcpp::Named("frequency") = Rcpp::IntegerVector::create());
    }

    Rcpp::Range idx = Rcpp::seq(0, i - 1);
    return Rcpp::List::create(
      Rcpp::Named("value") = values[idx],
                                   Rcpp::Named("frequency") = freq[idx]);
  }



};


// [[Rcpp::export]]
Rcpp::List top_n(Rcpp::NumericVector x, int n = 5) {
  hist_top h(n);
  h.insert(x.begin(), x.end());
  return h.get();
}





class hist_bottom {
private:
  struct paired {
    typedef std::pair<double, unsigned int> pair_t;

    pair_t pair;
    unsigned int is_set;

    paired()
      : pair(pair_t()),
        is_set(0)
    {}

    paired(double x)
      : pair(std::make_pair(x, 1)),
        is_set(1)
    {}

    bool operator==(const paired& other) const {
      return pair.first == other.pair.first;
    }

    bool operator==(double other) const {
      return is_set && (pair.first == other);
    }

    bool operator>(double other) const {
      return is_set && (pair.first > other);
    }

    bool operator<(double other) const {
      return is_set && (pair.first < other);
    }

    paired& operator++() {
      ++pair.second;
      return *this;
    }

    paired operator++(int) {
      paired tmp(*this);
      ++(*this);
      return tmp;
    }
  };

  struct less {
    bool operator()(const paired& lhs, const paired& rhs) const {
      if (!lhs.is_set) return false;
      if (!rhs.is_set) return true;
      return lhs.pair.first < rhs.pair.first;
    }
  };

  typedef std::priority_queue<
    paired,
    std::vector<paired>,
    less
  > queue_t;

  unsigned int sz;
  queue_t queue;

  void insert(double x) {
    if (queue.empty()) {
      queue.emplace(x);
      return;
    }

    if (queue.top() < x && queue.size() >= sz) return;

    queue_t qtmp;
    bool matched = false;

    while (queue.size()) {
      paired elem = queue.top();
      if (elem == x) {
        qtmp.emplace(std::move(++elem));
        matched = true;
      } else {
        qtmp.emplace(std::move(elem));
      }
      queue.pop();
    }

    if (!matched) {
      if (qtmp.size() >= sz) qtmp.pop();
      qtmp.emplace(x);
    }

    std::swap(queue, qtmp);
  }

public:
  hist_bottom(unsigned int sz_)
    : sz(sz_),
      queue(queue_t())
  {}

  template <typename InputIt>
  void insert(InputIt first, InputIt last) {
    while (first != last) insert(*first++);
  }

  // Rcpp::List get() const {
  //   Rcpp::NumericVector values(sz);
  //   Rcpp::IntegerVector freq(sz);
  //   R_xlen_t i = 0;
  //
  //   queue_t tmp(queue);
  //   while (tmp.size()) {
  //     values[i] = tmp.top().pair.first;
  //     freq[i] = tmp.top().pair.second;
  //     ++i;
  //     tmp.pop();
  //   }
  //
  //   return Rcpp::List::create(
  //     Rcpp::Named("value") = values,
  //     Rcpp::Named("frequency") = freq);
  // }

  Rcpp::List get() const {
    Rcpp::NumericVector values(sz);
    Rcpp::IntegerVector freq(sz);
    R_xlen_t i = 0;

    queue_t tmp(queue);
    while (tmp.size()) {
      paired p = tmp.top();
      if (p.is_set) {
        values[i] = p.pair.first;
        freq[i] = p.pair.second;
        ++i;
      }
      tmp.pop();
    }

    if (!i) {
      return Rcpp::List::create(
        Rcpp::Named("value") = Rcpp::rev(Rcpp::NumericVector::create()),
        Rcpp::Named("frequency") = Rcpp::rev(Rcpp::IntegerVector::create()));
    }

    Rcpp::Range idx = Rcpp::seq(0, i - 1);
    return Rcpp::List::create(
      Rcpp::Named("value") = Rcpp::rev(values[idx]),
      Rcpp::Named("frequency") = Rcpp::rev(freq[idx]));
  }

};

// [[Rcpp::export]]
Rcpp::List bottom_n(Rcpp::NumericVector x, int n = 5) {
  hist_bottom h(n);
  h.insert(x.begin(), x.end());
  return h.get();
}


// This returns the index of the nth largest/smallest values without respecting ties
// source Romain Francois:
//   http://gallery.rcpp.org/articles/top-elements-from-vectors-using-priority-queue/

// [[Rcpp::export]]
IntegerVector top_i(NumericVector v, unsigned int n)
{
  typedef pair<double, int> Elt;
  priority_queue< Elt, vector<Elt>, greater<Elt> > pq;
  vector<int> result;

  for (int i = 0; i != v.size(); ++i) {
    if (pq.size() < n)
      pq.push(Elt(v[i], i));
    else {
      Elt elt = Elt(v[i], i);
      if (pq.top() < elt) {
        pq.pop();
        pq.push(elt);
      }
    }
  }

  result.reserve(pq.size());
  while (!pq.empty()) {
    result.push_back(pq.top().second + 1);
    pq.pop();
  }

  return wrap(result);
}


// [[Rcpp::export]]
IntegerVector bottom_i(NumericVector v, unsigned int n)
{
  typedef pair<double, int> Elt;
  priority_queue< Elt, vector<Elt>, less <Elt> > pq;
  vector<int> result;

  for (int i = 0; i != v.size(); ++i) {
    if (pq.size() < n)
      pq.push(Elt(v[i], i));
    else {
      Elt elt = Elt(v[i], i);
      if (pq.top() > elt) {
        pq.pop();
        pq.push(elt);
      }
    }
  }

  result.reserve(pq.size());
  while (!pq.empty()) {
    result.push_back(pq.top().second + 1);
    pq.pop();
  }

  return wrap(result);
}



// Source
// https://stackoverflow.com/questions/55212746/rcpp-fast-statistical-mode-function-with-vector-input-of-any-type
// Author: Ralf Stubner, Joseph Wood

// [[Rcpp::plugins(cpp11)]]
#include <unordered_map>

template <int RTYPE>
Vector<RTYPE> fastModeImpl(Vector<RTYPE> x, bool narm){
  if (narm) x = x[!is_na(x)];
  int myMax = 1;
  Vector<RTYPE> myMode(1);
  // special case for factors == INTSXP with "class" and "levels" attribute
  if (x.hasAttribute("levels")){
    myMode.attr("class") = x.attr("class");
    myMode.attr("levels") = x.attr("levels");
  }
  std::unordered_map<typename Rcpp::traits::storage_type<RTYPE>::type, int> modeMap;
  modeMap.reserve(x.size());
  
  for (std::size_t i = 0, len = x.size(); i < len; ++i) {
    auto it = modeMap.find(x[i]);
    
    if (it != modeMap.end()) {
      ++(it->second);
      if (it->second > myMax) {
        myMax = it->second;
        myMode[0] = x[i];
      }
    } else {
      modeMap.insert({x[i], 1});
    }
  }
  
  myMode.attr("freq") = myMax;
  
  return myMode;
}

template <>
Vector<CPLXSXP> fastModeImpl(Vector<CPLXSXP> x, bool narm) {
  stop("Not supported SEXP type!");
}


// [[Rcpp::export(name="fastMode", rng=false)]]
SEXP fastMode( SEXP x, bool narm = false ){
  RCPP_RETURN_VECTOR(fastModeImpl, x, narm);
}



template <int RTYPE>
Vector<RTYPE> fastModeImplX(Vector<RTYPE> x, bool narm){
  if (narm) x = x[!is_na(x)];
  int myMax = 1;
  std::vector<typename Rcpp::traits::storage_type<RTYPE>::type> modes;
  std::unordered_map<typename
    Rcpp::traits::storage_type<RTYPE>::type, int> modeMap;
  modeMap.reserve(x.size());
  
  for (std::size_t i = 0, len = x.size(); i < len; ++i) {
    auto it = modeMap.find(x[i]);
    
    if (it != modeMap.end()) {
      ++(it->second);
      if (it->second > myMax) {
        myMax = it->second;
        modes.clear();
        modes.push_back(x[i]);
      } else if (it->second == myMax) {
        modes.push_back(x[i]);
      }
    } else {
      modeMap.insert({x[i], 1});
    }
  }
  
  Rcpp::Vector<RTYPE> myMode(modes.size());
  std::copy(modes.cbegin(), modes.cend(), myMode.begin());
  // special case for factors == INTSXP with "class" and "levels" attribute
  if (x.hasAttribute("levels")){
    myMode.attr("class") = x.attr("class");
    myMode.attr("levels") = x.attr("levels");
  }
  myMode.attr("freq") = myMax;
  return myMode;
}


template <>
Vector<CPLXSXP> fastModeImplX(Vector<CPLXSXP> x, bool narm) {
  stop("Not supported SEXP type!");
}


// [[Rcpp::export(name="fastModeX", rng=false)]]
SEXP fastModeX( SEXP x, bool narm = false ){
  RCPP_RETURN_VECTOR(fastModeImplX, x, narm);
}

