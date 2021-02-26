
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp2a)]]

// [[Rcpp::export]]
List rcpp_hello_world() {

    CharacterVector x = CharacterVector::create( "foo", "bar", "baz", "zap")  ;
    NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}
