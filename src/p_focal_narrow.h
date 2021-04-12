#ifndef P_FOCAL_NARROW_H
#define P_FOCAL_NARROW_H

#include "p_focal.h"

#include <cstdlib>

namespace p_focal{

    template<TRANSFORM TRANSFORM_FUNCTION, REDUCE REDUCE_FUNCTION, NAN_POLICY NAN_P, MEAN_DIVISOR MEAN_D, size_t ALIGNMENT=_P_FOCAL_ALLIGNMENT>
    void p_conv(const expanded_aligned_data<ALIGNMENT>& src, const expanded_aligned_data<ALIGNMENT>& kernel, double* dest, const bool open_mp_requested,
                bool variance){
        if(variance){
            p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_P, MEAN_D, true> (src, kernel, dest, open_mp_requested);
        }else{
            p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_P, MEAN_D, false>(src, kernel, dest, open_mp_requested);
        }

    }

    template<TRANSFORM TRANSFORM_FUNCTION, REDUCE REDUCE_FUNCTION, NAN_POLICY NAN_P, size_t ALIGNMENT=_P_FOCAL_ALLIGNMENT>
    void p_conv(const expanded_aligned_data<ALIGNMENT>& src, const expanded_aligned_data<ALIGNMENT>& kernel, double* dest, const bool open_mp_requested,
                MEAN_DIVISOR mean_d, bool variance){

        switch(mean_d){
            break;case MEAN_DIVISOR::ONE:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_P, MEAN_DIVISOR::ONE>                  (src, kernel, dest, open_mp_requested, variance);

            break;case MEAN_DIVISOR::KERNEL_SIZE:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_P, MEAN_DIVISOR::KERNEL_SIZE>          (src, kernel, dest, open_mp_requested, variance);

            break;case MEAN_DIVISOR::KERNEL_COUNT:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_P, MEAN_DIVISOR::KERNEL_COUNT>         (src, kernel, dest, open_mp_requested, variance);

            break;case MEAN_DIVISOR::KERNEL_SUM:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_P, MEAN_DIVISOR::KERNEL_SUM>           (src, kernel, dest, open_mp_requested, variance);

            break;case MEAN_DIVISOR::KERNEL_ABS_SUM:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_P, MEAN_DIVISOR::KERNEL_ABS_SUM>       (src, kernel, dest, open_mp_requested, variance);

            break;case MEAN_DIVISOR::KERNEL_PROD:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_P, MEAN_DIVISOR::KERNEL_PROD>          (src, kernel, dest, open_mp_requested, variance);

            break;case MEAN_DIVISOR::KERNEL_ABS_PROD:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_P, MEAN_DIVISOR::KERNEL_ABS_PROD>      (src, kernel, dest, open_mp_requested, variance);

            break;case MEAN_DIVISOR::DYNAMIC_COUNT:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_P, MEAN_DIVISOR::DYNAMIC_COUNT>        (src, kernel, dest, open_mp_requested, variance);

            break;case MEAN_DIVISOR::DYNAMIC_SUM:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_P, MEAN_DIVISOR::DYNAMIC_SUM>          (src, kernel, dest, open_mp_requested, variance);

            break;case MEAN_DIVISOR::DYNAMIC_ABS_SUM:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_P, MEAN_DIVISOR::DYNAMIC_ABS_SUM>      (src, kernel, dest, open_mp_requested, variance);

            break;case MEAN_DIVISOR::DYNAMIC_PROD:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_P, MEAN_DIVISOR::DYNAMIC_PROD>         (src, kernel, dest, open_mp_requested, variance);

            break;case MEAN_DIVISOR::DYNAMIC_ABS_PROD:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_P, MEAN_DIVISOR::DYNAMIC_ABS_PROD>     (src, kernel, dest, open_mp_requested, variance);

            break;case MEAN_DIVISOR::DYNAMIC_DATA_SUM:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_P, MEAN_DIVISOR::DYNAMIC_DATA_SUM>     (src, kernel, dest, open_mp_requested, variance);

            break;case MEAN_DIVISOR::DYNAMIC_DATA_ABS_SUM:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_P, MEAN_DIVISOR::DYNAMIC_DATA_ABS_SUM> (src, kernel, dest, open_mp_requested, variance);

            break;case MEAN_DIVISOR::DYNAMIC_DATA_PROD:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_P, MEAN_DIVISOR::DYNAMIC_DATA_PROD>    (src, kernel, dest, open_mp_requested, variance);

            break;case MEAN_DIVISOR::DYNAMIC_DATA_ABS_PROD:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_P, MEAN_DIVISOR::DYNAMIC_DATA_ABS_PROD>(src, kernel, dest, open_mp_requested, variance);


            break; default: std::cerr << "bad mean divisor\n";exit(64);
        }
    }

    template<TRANSFORM TRANSFORM_FUNCTION, REDUCE REDUCE_FUNCTION, size_t ALIGNMENT=_P_FOCAL_ALLIGNMENT>
    void p_conv(const expanded_aligned_data<ALIGNMENT>& src, const expanded_aligned_data<ALIGNMENT>& kernel, double* dest, const bool open_mp_requested,
                NAN_POLICY nan_p, MEAN_DIVISOR mean_d, bool variance){

        switch(nan_p){
            break;case NAN_POLICY::FAST:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_POLICY::FAST>       (src, kernel, dest, open_mp_requested, mean_d, variance);

            break;case NAN_POLICY::NA_RM_FALSE:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_POLICY::NA_RM_FALSE>(src, kernel, dest, open_mp_requested, mean_d, variance);

            break;case NAN_POLICY::NA_RM_TRUE:
                p_conv<TRANSFORM_FUNCTION, REDUCE_FUNCTION, NAN_POLICY::NA_RM_TRUE> (src, kernel, dest, open_mp_requested, mean_d, variance);

            break; default: std::cerr << "bad nan policy\n";exit(64);
        }

    }

    template<TRANSFORM TRANSFORM_FUNCTION, size_t ALIGNMENT=_P_FOCAL_ALLIGNMENT>
    void p_conv(const expanded_aligned_data<ALIGNMENT>& src, const expanded_aligned_data<ALIGNMENT>& kernel, double* dest, const bool open_mp_requested,
                REDUCE reduce_function, NAN_POLICY nan_p, MEAN_DIVISOR mean_d, bool variance){

        switch(reduce_function){
            break;case REDUCE::SUM:
                p_conv<TRANSFORM_FUNCTION, REDUCE::SUM>        (src, kernel, dest, open_mp_requested, nan_p, mean_d, variance);

            break;case REDUCE::ABS_SUM:
                p_conv<TRANSFORM_FUNCTION, REDUCE::ABS_SUM>    (src, kernel, dest, open_mp_requested, nan_p, mean_d, variance);

            break;case REDUCE::PRODUCT:
                p_conv<TRANSFORM_FUNCTION, REDUCE::PRODUCT>    (src, kernel, dest, open_mp_requested, nan_p, mean_d, variance);

            break;case REDUCE::ABS_PRODUCT:
                p_conv<TRANSFORM_FUNCTION, REDUCE::ABS_PRODUCT>(src, kernel, dest, open_mp_requested, nan_p, mean_d, variance);

            break;case REDUCE::MIN:
                p_conv<TRANSFORM_FUNCTION, REDUCE::MIN>        (src, kernel, dest, open_mp_requested, nan_p, mean_d, variance);

            break;case REDUCE::MAX:
                p_conv<TRANSFORM_FUNCTION, REDUCE::MAX>        (src, kernel, dest, open_mp_requested, nan_p, mean_d, variance);

            break; default: std::cerr << "bad reduce function\n";exit(64);
        }
    }

    template<size_t ALIGNMENT=_P_FOCAL_ALLIGNMENT>
    void p_conv(const expanded_aligned_data<ALIGNMENT>& src, const expanded_aligned_data<ALIGNMENT>& kernel, double* dest, const bool open_mp_requested,
                TRANSFORM transform_function, REDUCE reduce_function, NAN_POLICY nan_p, MEAN_DIVISOR mean_d, bool variance){

        switch(transform_function){
            break;case TRANSFORM::MULTIPLY:
                p_conv<TRANSFORM::MULTIPLY>(src, kernel, dest, open_mp_requested, reduce_function, nan_p, mean_d, variance);

            break;case TRANSFORM::ADD:
                p_conv<TRANSFORM::ADD>     (src, kernel, dest, open_mp_requested, reduce_function, nan_p, mean_d, variance);

            break;case TRANSFORM::R_EXP:
                p_conv<TRANSFORM::R_EXP>   (src, kernel, dest, open_mp_requested, reduce_function, nan_p, mean_d, variance);

            break;case TRANSFORM::L_EXP:
                p_conv<TRANSFORM::L_EXP>   (src, kernel, dest, open_mp_requested, reduce_function, nan_p, mean_d, variance);

            break; default: std::cerr << "bad transform function\n";exit(64);
        }
    }

}//namespace p_focal

#endif /*P_FOCAL_NARROW_H*/
