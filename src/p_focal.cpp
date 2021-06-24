#include <Rcpp.h>

#include "p_focal.h"
#include "p_focal_narrow.h"

using namespace p_focal;
using namespace Rcpp;

// [[Rcpp::plugins(cpp17)]]

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

// [[Rcpp::export(.p_focal_transform_info_cpp)]]
Rcpp::List p_focal_transform_info_cpp() {
    //p_focal::TRANSFORM_DESCRIPTION
    Rcpp::CharacterVector name_list(static_cast<size_t>(p_focal::TRANSFORM::SIZE));
    Rcpp::CharacterVector desc_list(static_cast<size_t>(p_focal::TRANSFORM::SIZE));

    for(auto t : p_focal::TRANSFORM_DESCRIPTION){
        const auto& [index, name, desc] = t;
        name_list[index] = name;
        desc_list[index] = desc;
    }

    return Rcpp::List::create(name_list, desc_list);
}

// [[Rcpp::export(.p_focal_reduce_info_cpp)]]
Rcpp::List p_focal_reduce_info_cpp() {
    //p_focal::REDUCE_DESCRIPTION
    Rcpp::CharacterVector name_list(static_cast<size_t>(p_focal::REDUCE::SIZE));
    Rcpp::CharacterVector desc_list(static_cast<size_t>(p_focal::REDUCE::SIZE));

    for(auto t : p_focal::REDUCE_DESCRIPTION){
        const auto& [index, name, desc] = t;
        name_list[index] = name;
        desc_list[index] = desc;
    }

    return Rcpp::List::create(name_list, desc_list);
}

// [[Rcpp::export(.p_focal_nan_policy_info_cpp)]]
Rcpp::List p_focal_nan_policy_info_cpp() {
    //p_focal::NAN_POLICY_DESCRIPTION
    Rcpp::CharacterVector name_list(static_cast<size_t>(p_focal::NAN_POLICY::SIZE));
    Rcpp::CharacterVector desc_list(static_cast<size_t>(p_focal::NAN_POLICY::SIZE));

    for(auto t : p_focal::NAN_POLICY_DESCRIPTION){
        const auto& [index, name, desc] = t;
        name_list[index] = name;
        desc_list[index] = desc;
    }

    return Rcpp::List::create(name_list, desc_list);
}

// [[Rcpp::export(.p_focal_mean_divisor_info_cpp)]]
Rcpp::List p_focal_mean_divisor_info_cpp() {
    //p_focal::MEAN_DIVISOR_DESCRIPTION
    Rcpp::CharacterVector name_list(static_cast<size_t>(p_focal::MEAN_DIVISOR::SIZE));
    Rcpp::CharacterVector desc_list(static_cast<size_t>(p_focal::MEAN_DIVISOR::SIZE));

    for(auto t : p_focal::MEAN_DIVISOR_DESCRIPTION){
        const auto& [index, name, desc] = t;
        name_list[index] = name;
        desc_list[index] = desc;
    }

    return Rcpp::List::create(name_list, desc_list);
}



// [[Rcpp::export(.p_focal_variance_info_cpp)]]
Rcpp::List p_focal_variance_info_cpp() {
    auto name_list = Rcpp::CharacterVector::create(
        "FALSE",
        "TRUE");

    auto desc_list = Rcpp::CharacterVector::create(
        "Returns the value at each point",

        "Retruns something like the variance of the intermediate values at each point."
        "First calculate the value as normal. Then find the 'mean' by dividing the value by the mean_divisor. "
        "Then for each intermidiate value, calculate the square of the difference and 'reduce' them using the reduce_function. (ie: sum them if SUM, multiply them if PROD, take their max if MAX etc.)");

    return Rcpp::List::create(name_list, desc_list);
}

// [[Rcpp::export(.p_focal_cpp)]]
Rcpp::NumericMatrix p_focal_cpp(
        const Rcpp::NumericMatrix& data,
        const Rcpp::NumericMatrix& kernel,
        const double edge_value,
        const size_t transform_fun,
        const size_t reduce_fun,
        const size_t nan_policy,
        const size_t mean_divisor,
        const bool variance,
        const bool open_mp){
    TRANSFORM tf((TRANSFORM)transform_fun);
    REDUCE rf((REDUCE)reduce_fun);
    NAN_POLICY nf((NAN_POLICY)nan_policy);
    MEAN_DIVISOR md((MEAN_DIVISOR)mean_divisor);

    bool good = true;

    if(tf >= TRANSFORM::SIZE){
        Rcout << "\n"
                  << "The transform function, ie: 'weight_fun', is not a valid value.\n"
                  << "It is " << ((size_t)tf) << " when it must be in the range [0, " << ((size_t)TRANSFORM::SIZE) << ")\n";
        good = false;
    }
    if(rf >= REDUCE::SIZE){
        Rcout << "\n"
                  << "The reduce function, ie: 'fun', is not a valid value.\n"
                  << "It is " << ((size_t)rf) << " when it must be in the range [0, " << ((size_t)REDUCE::SIZE) << ")\n";
        good = false;
    }
    if(nf >= NAN_POLICY::SIZE){
        Rcout << "\n"
                  << "The nan policy is not a valid value.\n"
                  << "It is " << ((size_t)nf) << " when it must be in the range [0, " << ((size_t)NAN_POLICY::SIZE) << ")\n";
        good = false;
    }
    if(md >= MEAN_DIVISOR::SIZE){
        Rcout << "\n"
                  << "The mean policy is not a valid value.\n"
                  << "It is " << ((size_t)md) << " when it must be in the range [0, " << ((size_t)MEAN_DIVISOR::SIZE) << ")\n";
        good = false;
    }

    if(!good){
        return{0};
    }else{
        //std::cout << ((size_t)tf) << ", " << ((size_t)rf) << ", " << ((size_t)nf) << ", " << ((size_t)md) << ", " << ((size_t)variance) << "\n";

        expanded_aligned_data<> src(&data[0], data.ncol(), data.nrow(), kernel.ncol()/2, kernel.nrow()/2, edge_value);
        expanded_aligned_data<> k(&kernel[0], kernel.ncol(), kernel.nrow(), 0, 0, 0);

        Rcpp::NumericMatrix dest(data.nrow(), data.ncol());

        p_conv<>(src, k, &dest[0], open_mp, tf, rf, nf, md, variance);

        return dest;
    }
}
