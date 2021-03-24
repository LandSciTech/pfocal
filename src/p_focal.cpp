#include "p_focal.h"

#include <Rcpp.h>
//using namespace Rcpp;

// [[Rcpp::plugins(cpp2a)]]

// [[Rcpp::export(.openmp_self_test_cpp)]]
Rcpp::List openmp_self_test_cpp() {
    auto x = Rcpp::CharacterVector::create(
        "openMP enabled",
        "openMP version",
        "max_threads");

    auto [enabled, version, thread_count] = openmp_self_test();

    auto y = Rcpp::NumericVector::create(enabled, version, thread_count);

    auto z = Rcpp::List::create( x, y );
    return z ;
}


// [[Rcpp::export(.get_weight_fun_list_cpp)]]
Rcpp::CharacterVector get_weight_fun_list_cpp(){
    Rcpp::CharacterVector cv(TRANSFORM_FUNCTION_LIST.size());
    std::copy(TRANSFORM_FUNCTION_LIST.begin(), TRANSFORM_FUNCTION_LIST.end(), cv.begin());
    return cv;
}


// [[Rcpp::export(.get_fun_list_cpp)]]
Rcpp::CharacterVector get_fun_list_cpp(){
    Rcpp::CharacterVector cv(REDUCE_FUNCTION_LIST.size());
    std::copy(REDUCE_FUNCTION_LIST.begin(), REDUCE_FUNCTION_LIST.end(), cv.begin());
    return cv;
}



// [[Rcpp::export(.p_focal_cpp)]]
std::vector<double> p_focal_cpp(Rcpp::NumericMatrix data, std::vector<double> kernel, size_t k_rows, size_t k_cols, size_t k_lays, int transform_fun, int reduce_fun){
    if(!(transform_fun || reduce_fun)){
        return {};
    }

    return {};
}
