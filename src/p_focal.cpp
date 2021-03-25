#include "p_focal.h"

#include <Rcpp.h>
using namespace p_focal;

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
Rcpp::NumericMatrix p_focal_cpp(const Rcpp::NumericMatrix& data, const std::vector<double>& kernel, const size_t k_rows, const size_t k_cols, const size_t k_lays, const int transform_fun, const int reduce_fun, const double default_value){

    //std::cerr << "d_col:"<< data.ncol() << ", drow:" <<  data.nrow() << ", ecol:" <<  k_cols/2 << ", erow:" <<  k_rows/2 << ", default:" <<  default_value << "\n";
    expanded_aligned_data<> e1(&data[0], data.ncol(), data.nrow(), k_cols/2, k_rows/2, default_value);


    expanded_aligned_data<> e2(          data.ncol(), data.nrow(), k_cols/2, k_rows/2, default_value);

    p_conv<TRANSFORM::MULTIPLY, REDUCE::SUM, STRATEGY::BASIC>(e1, e2, &kernel[0], k_cols, k_rows);

    Rcpp::NumericMatrix out(data.nrow(), data.ncol());

    e2.copy_out(&out[0]);
    return out;
}


