
// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <boost/integer/common_factor.hpp>
#include <queue>

#include <iostream>
#include <sstream>
#include <bitset>
#include <string>
#include <boost/algorithm/string.hpp>

using namespace std;
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// [[Rcpp::export]]
int compute_GCD(int a, int b){
  return
    //boost::math::gcd(a, b);
    boost::integer::gcd(a,b);
}

// [[Rcpp::export]]
int compute_LCM(int a, int b){
  return
    // boost::math::lcm(a, b);
    boost::integer::lcm(a,b);
}


//[[Rcpp::export]]
IntegerVector divs( int x ){
  IntegerVector d = seq_len( x / 2 );
  IntegerVector out(0);
  for( int i = 0 ; i < d.size(); i++){
    if( x % d[i] == 0 )
      out.push_back( d[i] );
  }
  return out;
}


// // [[Rcpp::export]]
// List n_pow_sum(NumericVector x, double meanx) {
//
//
//   // x must be a nonempty sorted numeric vector with NAs omitted
//
//   double d = x[0] - meanx;
//   double d2 = d*d;
//
//   double sum2 = d2;
//   double sum3 = d2*d;
//   double sum4 = d2*d2;
//
//   int n = x.size();    // the vector size
//   int zn = 0;
//   if(x[0]==0) {zn=1;}; // the number of zeros
//   int un = 1;          // the number of unique values
//
//   for (int ii=1; ii < x.size(); ii++) {
//
//     d = x[ii] - meanx;
//
//     d2 = d*d;
//     sum2 += d2;     // sum of squares
//     sum3 += d2*d;   // sum of 3th powers
//     sum4 += d2*d2;  // sum of 4th powers
//
//     // number of unique values
//     if(x[ii] != x[ii-1]) un += 1;
//
//     // number of zero values
//     if(x[ii] == 0) zn += 1 ;
//
//   }
//
//
//   int ldim = 5;    // dimension of small/large values vectors
//   if(un < ldim) { ldim = un; }
//
//   NumericVector small_val(ldim);    // the 5 smallest values
//   NumericVector small_freq(ldim);   // the frequency of the 5 smallest values
//
//
//   // the 5 smallest values and their frequencies
//
//   small_val[0] = x[0];
//   small_freq[0] = 1;
//
//   int i = 1;
//   int j = 0;
//
//   do {
//     if(x[i] == x[i-1] ){
//       small_freq[j] += 1 ;
//     } else {
// // we have reached the max number of interesting values, so we break
//       if(j==ldim-1) break;
//
//       j += 1 ;
//       small_val[j] = x[i] ;
//       small_freq[j] = 1 ;
//     }
//     i += 1 ;
//
//   } while(i < n);
//
//
//   // the 5 largest values and their frequencies
//
//   NumericVector large_val(ldim);
//   NumericVector large_freq(ldim);
//
//   large_val[0] = x[n-1];
//   large_freq[0] = 1;
//
//   i = n-1;
//   j = 0;
//
//   do {
//
//     if(x[i] == x[i-1] ){
//       large_freq[j] += 1 ;
//     } else {
//       // we have reached the max number of interesting values, so we break
//       if(j==ldim-1) break;
//
//       j += 1 ;
//       large_val[j] = x[i-1] ;
//       large_freq[j] = 1 ;
//     }
//     i -= 1 ;
//
//   } while(i >= 1);
//
//
//   return Rcpp::List::create(Rcpp::Named("sum2", sum2),
//                             Rcpp::Named("sum3", sum3),
//                             Rcpp::Named("sum4", sum4),
//                             Rcpp::Named("zero", zn),
//                             Rcpp::Named("unique", un),
//                             Rcpp::Named("small_val", small_val),
//                             Rcpp::Named("small_freq", small_freq),
//                             Rcpp::Named("large_val", large_val),
//                             Rcpp::Named("large_freq", large_freq)
//   );
//
// }



// [[Rcpp::export]]
List n_pow_sum(NumericVector x) {

  // x must be a nonempty numeric vector with NAs omitted

  std::map<double, int> counts;

  int n = x.size();
  for (int i = 0; i < n; i++) {
    counts[x[i]]++;
  }

  double mean = 0;
  typedef std::map<double, int>::iterator it_type;
  for(it_type iterator = counts.begin(); iterator != counts.end(); iterator++) {

    mean += iterator->second * iterator->first;

    // iterator->first = key
    // iterator->second = value
  }
  mean = mean / n;

  int un = counts.size();
  int zn = 0;
  double d = 0;
  double d2 = 0;

  double sum2 = 0;
  double sum3 = 0;
  double sum4 = 0;


  for(it_type iterator = counts.begin(); iterator != counts.end(); iterator++) {

    d = iterator->first - mean;
    d2 = iterator->second * d * d;

    sum2 += d2;      // sum of squares
    sum3 += d2*d;    // sum of 3th powers
    sum4 += d2*d*d;  // sum of 4th powers

    if(iterator->first == 0) zn = iterator->second;   // number of zero values
  }

  unsigned int ldim = 5;    // dimension of small/large values vectors
  if(counts.size() < ldim) { ldim = counts.size(); }

  NumericVector small_val(ldim);    // the 5 smallest values
  NumericVector small_freq(ldim);   // the frequency of the 5 smallest values
  NumericVector large_val(ldim);    // the 5 smallest values
  NumericVector large_freq(ldim);   // the frequency of the 5 smallest values

  unsigned int i=0;
  for(it_type iterator = counts.begin(); iterator != counts.end(); iterator++, i++) {
    small_val[i] = iterator->first;
    small_freq[i] = iterator->second;
    if(i == ldim -1){
      break;
    }
  }

  i=0;
  typedef std::map<double, int>::reverse_iterator it_rev_type;
  for(it_rev_type iterator = counts.rbegin(); iterator != counts.rend(); iterator++, i++) {
    large_val[i] = iterator->first;
    large_freq[i] = iterator->second;
    if(i == ldim -1){
      break;
    }
  }


  return Rcpp::List::create(
    Rcpp::Named("mean", mean),
    Rcpp::Named("sum2", sum2),
    Rcpp::Named("sum3", sum3),
    Rcpp::Named("sum4", sum4),
    Rcpp::Named("zero", zn),
    Rcpp::Named("unique", un),
    Rcpp::Named("small_val", small_val),
    Rcpp::Named("small_freq", small_freq),
    Rcpp::Named("large_val", large_val),
    Rcpp::Named("large_freq", large_freq)
  );

  // return 0;
}







// [[Rcpp::export]]
std::vector< std::string > conv_DecToBin(std::vector< int > n )
{
  const unsigned g_unMaxBits = 32;

  int len = n.size();
  std::vector< std::string > out(len);

  for( int i=0; i < len; i++ ) {
      bitset<g_unMaxBits> b(n[i]);
      out[i] = b.to_string();
  }

  return out;

}


// fast implementation for Large(x, n)
// http://gallery.rcpp.org/articles/top-elements-from-vectors-using-priority-queue/
// http://lists.r-forge.r-project.org/pipermail/rcpp-devel/2014-January/006976.html
// this works but does not support ties

// #include <queue>
//
// // [[Rcpp::export]]
// std::vector<int> top_i_pq(NumericVector v, unsigned int n)
// {
//
//   typedef pair<double, int> Elt;
//   priority_queue< Elt, vector<Elt>, greater<Elt> > pq;
//   vector<int> result;
//
//   for (int i = 0; i != v.size(); ++i) {
//     if (pq.size() < n)
//       pq.push(Elt(v[i], i));
//     else {
//       Elt elt = Elt(v[i], i);
//       if (pq.top() < elt) {
//         pq.pop();
//         pq.push(elt);
//       }
//     }
//   }
//
//   result.reserve(pq.size());
//   while (!pq.empty()) {
//     result.push_back(pq.top().second + 1);
//     pq.pop();
//   }
//
//   return result ;
//
// }



// coole function, unbedingt wieder reinnehmen
//
// template <int RTYPE>
// class IndexComparator {
// public:
//   typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
//
//   IndexComparator( const Vector<RTYPE>& data_ ) : data(data_.begin()){}
//
//   inline bool operator()(int i, int j) const {
//     return data[i] > data[j] || (data[i] == data[j] && j > i ) ;
//   }
//
// private:
//   const STORAGE* data ;
// } ;
//
// template <>
// class IndexComparator<STRSXP> {
// public:
//   IndexComparator( const CharacterVector& data_ ) : data(data_.begin()){}
//
//   inline bool operator()(int i, int j) const {
//     return (String)data[i] > (String)data[j] || (data[i] == data[j] && j > i );
//   }
//
// private:
//   const SEXP* data ;
// } ;
//
// template <int RTYPE>
// class IndexQueue {
// public:
//   typedef std::priority_queue<int, std::vector<int>, IndexComparator<RTYPE> > Queue ;
//
//   IndexQueue( const Vector<RTYPE>& data_ ) : comparator(data_), q(comparator), data(data_) {}
//
//   inline operator IntegerVector(){
//     int n = q.size() ;
//     IntegerVector res(n) ;
//     for( int i=0; i<n; i++){
//       // +1 for 1-based R indexing
//       res[i] = q.top() + 1;
//       q.pop() ;
//     }
//     return res ;
//   }
//   inline void input( int i){
//     // if( data[ q.top() ] < data[i] ){
//     if( comparator(i, q.top() ) ){
//       q.pop();
//       q.push(i) ;
//     }
//   }
//   inline void pop(){ q.pop() ; }
//   inline void push( int i){ q.push(i) ; }
//
// private:
//   IndexComparator<RTYPE> comparator ;
//   Queue q ;
//   const Vector<RTYPE>& data ;
// } ;
//
//
// template <int RTYPE>
// IntegerVector top_index(Vector<RTYPE> v, int n){
//   int size = v.size() ;
//
//   // not interesting case. Less data than n
//   if( size < n){
//     return seq( 0, n-1 ) ;
//   }
//
//   IndexQueue<RTYPE> q( v )  ;
//   for( int i=0; i<n; i++) q.push(i) ;
//   for( int i=n; i<size; i++) q.input(i) ;
//   return q ;
// }
//
// // [[Rcpp::export]]
// IntegerVector top_index( SEXP x, int n){
//   switch( TYPEOF(x) ){
//   case INTSXP: return top_index<INTSXP>( x, n ) ;
//   case REALSXP: return top_index<REALSXP>( x, n ) ;
//   case STRSXP: return top_index<STRSXP>( x, n ) ;
//   default: stop("type not handled") ;
//   }
//   return IntegerVector() ; // not used
// }
//
//
//
//
//
//
//
//
//
