#include "p_focal.h"
#include "p_focal_switch_wall.h"

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

// [[Rcpp::export(.p_focal_cpp)]]
Rcpp::NumericMatrix p_focal_cpp(const Rcpp::NumericMatrix& data, const Rcpp::NumericMatrix& kernel, const double default_value, const size_t transform_fun, const size_t reduce_fun, const bool open_mp){
    TRANSFORM tf((TRANSFORM)transform_fun);
    REDUCE rf((REDUCE)reduce_fun);

    bool good = true;

    if(tf >= TRANSFORM::SIZE){
        std::cerr << "\n"
            << "The transform function, ie: 'weight_fun', is not a valid value.\n"
            << "It is " << ((size_t)tf) << " when it must be in the range [0, " << ((size_t)TRANSFORM::SIZE) << ")\n";
        good = false;
    }
    if(rf >= REDUCE::SIZE){
        std::cerr << "\n"
            << "The reduce function, ie: 'fun', is not a valid value.\n"
            << "It is " << ((size_t)rf) << " when it must be in the range [0, " << ((size_t)REDUCE::SIZE) << ")\n";
        good = false;
    }

    if(!good){
        return (0,0);
    }else{
        //std::cout << ((size_t)tf) << ", " << ((size_t)rf) << "\n";
        expanded_aligned_data<> src(&data[0], data.ncol(), data.nrow(), kernel.ncol()/2, kernel.nrow()/2, default_value);
        expanded_aligned_data<> k(&kernel[0], kernel.ncol(), kernel.nrow(), 0, 0, 0);

        Rcpp::NumericMatrix dest(data.nrow(), data.ncol());

        p_focal_switch_wall(src, k, &dest[0], tf, rf, open_mp);
    //    p_conv<TRANSFORM::MULTIPLY, REDUCE::SUM, STRATEGY::BASIC>(src, k, &dest[0]);

        return dest;
    }
}



