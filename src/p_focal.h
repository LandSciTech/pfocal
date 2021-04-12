#ifndef P_FOCAL_H_
#define P_FOCAL_H_

#ifdef _OPENMP
#include <omp.h>
// [[Rcpp::plugins(openmp)]]
#define _P_FOCAL_OPENMP_ENADLED 1
#else
#define _P_FOCAL_OPENMP_ENADLED 0
#endif

#define _P_FOCAL_ALLIGNMENT 64

#include <tuple>
#include <array>
#include <cstddef>
#include <cstdlib>
#include <stdlib.h>
#include <cmath>
#include <cstring>

#include <iostream>
#include <cassert>
#include <memory>


namespace p_focal{

    template<size_t DATA_ALIGNMENT=_P_FOCAL_ALLIGNMENT>
    struct expanded_aligned_data{
        static_assert(DATA_ALIGNMENT > 0);
        static_assert(!(DATA_ALIGNMENT & (DATA_ALIGNMENT-1)));
        static_assert(DATA_ALIGNMENT >= alignof(double));

        /**
         for erow=2, ecol=3 row=7 col=17 al=4

         00011111111111111111000
         00011111111111111111000
         00011111111111111111000

         00011111111111111111000
         00011111111111111111000
         00011111111111111111000
         00011111111111111111000

         00011111111111111111000
         00000000000000000000000
         00000000000000000000000
         00000000000000000000000

         */

        double* data;
        double* buffer;
        size_t data_size;
        size_t block_size;
        size_t n_col;
        size_t n_row;
        size_t start_position;
        size_t col_size;
        size_t left_cols;
        size_t right_cols;
        size_t extra_rows;

        static constexpr size_t alignment(){return DATA_ALIGNMENT;}

        void copy_in(const double* const incomming_data){
            const size_t n_col = this->n_col;
            const size_t n_row = this->n_row;
            const size_t start_position = this->start_position;
            const size_t col_size = this->col_size;
            double* data = (double*)__builtin_assume_aligned((this->data), alignment());


            #pragma omp parallel for simd aligned(data:alignment())
            for(size_t col = 0; col<n_col; col++){
                double* col_start = (double*)__builtin_assume_aligned((data+start_position+(col_size*col)), alignment());
                memcpy(col_start, incomming_data+(n_row*col), n_row*sizeof(double));
            }
        }

        expanded_aligned_data(const size_t n_col, const size_t n_row, const size_t extra_col, const size_t extra_row, const double default_value){
            this->n_col = n_col;
            this->n_row = n_row;

            const size_t block_size = alignment()/sizeof(double);
            this->block_size = block_size;

            this->left_cols  = extra_col +(size_t)(extra_row != 0);
            this->right_cols = extra_col;

            this->col_size = (((this->n_row+extra_row-1)/block_size)+1)*block_size;
            this->extra_rows = this->col_size-this->n_row;

            this->start_position = this->left_cols*col_size;

            this->data_size = (this->left_cols + this->n_col + this->right_cols)*this->col_size;

            /*
             std::out   << "--------------------------------------------------------------------------\n"
                        << "alignment     :byte alighment of allocation and columns                  :" << alignment()          << "\n"
                        << "sizeof(double):size of the data element type                             :" << sizeof(double)       << "\n"
                        << "block_size    :How many elements per alignment block                     :" << this->block_size     << "\n"
                        << "n_col         :How many columns of input data                            :" << this->n_col          << "\n"
                        << "n_row         :How many rows of input data                               :" << this->n_row          << "\n"
                        << "start_position:The index of the first data element                       :" << this->start_position << "\n"
                        << "col_size      :The offset between colums of data                         :" << this->col_size       << "\n"
                        << "left_cols     :# of columns of the default value to the left of the data :" << this->left_cols      << "\n"
                        << "right_cols    :# of columns of the default value to the right of the data:" << this->right_cols     << "\n"
                        << "extra_rows    :# of rows of default between each column of data          :" << this->extra_rows     << "\n"
                        << "data_size     :# of indexes allocated                                    :" << this->data_size      << "\n"
                        << "--------------------------------------------------------------------------\n";
             */

            this->buffer = (double*)malloc(this->data_size*sizeof(double)+alignment());
            if(!(this->buffer)){
                std::cerr << "Out of memory\n";
                throw "Out of memory";
            }
            //take the first aligned pointer into the buffer
            this->data = (double*)__builtin_assume_aligned((double*)(
                (((uintptr_t)(this->buffer))+alignment()-1) & ~(alignment()-1)
            ), alignment());

            std::fill_n(data, data_size, default_value);
        }

        expanded_aligned_data(const double* const incomming_data, const size_t n_col, const size_t n_row, const size_t extra_col, const size_t extra_row, const double default_value)
            :expanded_aligned_data(n_col, n_row, extra_col, extra_row, default_value)
        {this->copy_in(incomming_data);}


        expanded_aligned_data(expanded_aligned_data<DATA_ALIGNMENT>&&)=default;

        expanded_aligned_data(const expanded_aligned_data<DATA_ALIGNMENT>& other){
            this->data_size      = other.data_size;
            this->block_size     = other.block_size;
            this->n_col          = other.n_col;
            this->n_row          = other.n_row;
            this->start_position = other.start_position;
            this->col_size       = other.col_size;
            this->left_cols      = other.left_cols;
            this->right_cols     = other.right_cols;
            this->e_rows         = other.e_rows;

            this->buffer = (double*)malloc(this->data_size*sizeof(double)+alignment());
            if(!(this->buffer)){
                std::cerr << "Out of memory\n";
                throw "Out of memory";
            }
            //take the first aligned pointer into the buffer
            this->data = (double*)__builtin_assume_aligned((double*)(
                    (((uintptr_t)(this->buffer))+alignment()-1) & ~(alignment()-1)
                ), alignment());

            double* tdata = this->data;
            const double* odata = other.data;
            const size_t block_size = this->block_size;
            const size_t data_size = this->data_size;

            //#pragma omp parallel for simd aligned(tdata:block_size) aligned(odata:block_size)
            //for(size_t i = 0; i< data_size; i++){
            //    tdata[i] = odata[i];
            //}
            memcpy(this->data, other.data, this->data_size);
        }

        ~expanded_aligned_data() noexcept{
            std::free(this->buffer);
            this->buffer = nullptr;
            this->data = nullptr;
        }

        void swap(expanded_aligned_data<DATA_ALIGNMENT>& rhs) noexcept{
            std::swap(this->buffer, rhs.buffer);
            std::swap(this->data, rhs.data);
            std::swap(this->data_size, rhs.data_size);
            std::swap(this->block_size, rhs.block_size);
            std::swap(this->n_col, rhs.n_col);
            std::swap(this->n_row, rhs.n_row);
            std::swap(this->start_position, rhs.start_position);
            std::swap(this->col_size, rhs.col_size);
            std::swap(this->left_cols, rhs.left_cols);
            std::swap(this->right_cols, rhs.right_cols);
            std::swap(this->e_rows, rhs.e_rows);
        }

        void copy_out(double* output) const noexcept{
            const auto* data = this->data+this->start_position;
            const size_t block_size = this->block_size;
            const size_t col_size = this->col_size;
            const size_t n_col = this->n_col;
            const size_t n_row = this->n_row;

            #pragma omp parallel for
            for(size_t col = 0; col<n_col; col++){
                const double* c_data = data+col*col_size;
                for(size_t row=0; row<n_row; row++){
                    output[col*n_row+row] = c_data[row];
                }
            }
        }

    };

    template<size_t DATA_ALIGNMENT>
    inline void swap(expanded_aligned_data<DATA_ALIGNMENT>& lhs, expanded_aligned_data<DATA_ALIGNMENT>& rhs) noexcept{
        lhs.swap(rhs);
    }

    std::tuple<bool, int, int> openmp_self_test(void) noexcept{
        if constexpr(_P_FOCAL_OPENMP_ENADLED){
            return {true, _OPENMP, omp_get_max_threads()};
        }else{
            return {false, 0, 1};
        }
    }

    enum class TRANSFORM : size_t{
        MULTIPLY=0,
        ADD,
        R_EXP,
        L_EXP,
        SIZE
    };

    const std::array<const std::tuple<const size_t, const char*, const char*>, static_cast<size_t>(TRANSFORM::SIZE)> TRANSFORM_DESCRIPTION {
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(TRANSFORM::MULTIPLY), "MULTIPLY", "For data value 'd' and kernal value 'k', intermediate_value = (d * k)"},
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(TRANSFORM::ADD),      "ADD",      "For data value 'd' and kernal value 'k', intermediate_value = (d + k)"},
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(TRANSFORM::R_EXP),    "R_EXP",    "For data value 'd' and kernal value 'k', intermediate_value = (d ^ k)"},
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(TRANSFORM::L_EXP),    "L_EXP",    "For data value 'd' and kernal value 'k', intermediate_value = (k ^ d)"}
    };

    enum class REDUCE : size_t{
        SUM=0,
        ABS_SUM,
        PRODUCT,
        ABS_PRODUCT,
        MIN,
        MAX,
        SIZE
    };

    const std::array<const std::tuple<const size_t, const char*, const char*>, static_cast<size_t>(REDUCE::SIZE)> REDUCE_DESCRIPTION {
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(REDUCE::SUM),         "SUM",         "Accumulator starts at 0. For each intermediate value, in no particular order, acc = ( acc + iv )"                            },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(REDUCE::ABS_SUM),     "ABS_SUM",     "Accumulator starts at 0. For each intermediate value, in no particular order, acc = ( acc + abs(iv) )"                       },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(REDUCE::PRODUCT),     "PRODUCT",     "Accumulator starts at 1. For each intermediate value, in no particular order, acc = ( acc * iv )"                            },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(REDUCE::ABS_PRODUCT), "ABS_PRODUCT", "Accumulator starts at 1. For each intermediate value, in no particular order, acc = ( acc * abs(iv) )"                       },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(REDUCE::MIN),         "MIN",         "Accumulator starts at the highest possible value. For each intermediate value, in no particular order, acc = min( acc , iv )"},
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(REDUCE::MAX),         "MAX",         "Accumulator starts at the lowest possible value. For each intermediate value, in no particular order, acc = max( acc , iv )" }
    };

    enum class NAN_POLICY : size_t{
        FAST=0,
        NA_RM_FALSE,
        NA_RM_TRUE,
        SIZE
    };

    const std::array<const std::tuple<const size_t, const char*, const char*>, static_cast<size_t>(NAN_POLICY::SIZE)> NAN_POLICY_DESCRIPTION {
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(NAN_POLICY::FAST),        "FAST",        "Assume that there will be no NAN values. Will not crash if it is given a NAN, but makes no garuntee other than that."},
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(NAN_POLICY::NA_RM_FALSE), "NA_RM_FALSE", "Will propagate NANs aggressively."                                                                                   },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(NAN_POLICY::NA_RM_TRUE),  "NA_RM_TRUE",  "Will discard NAN values early."                                                                                      }
    };

    enum class MEAN_DIVISOR : size_t{
        ONE=0,
        KERNEL_SIZE,
        KERNEL_COUNT,
        KERNEL_SUM,
        KERNEL_ABS_SUM,
        KERNEL_PROD,
        KERNEL_ABS_PROD,
        DYNAMIC_COUNT,
        DYNAMIC_SUM,
        DYNAMIC_ABS_SUM,
        DYNAMIC_PROD,
        DYNAMIC_ABS_PROD,
        DYNAMIC_DATA_SUM,
        DYNAMIC_DATA_ABS_SUM,
        DYNAMIC_DATA_PROD,
        DYNAMIC_DATA_ABS_PROD,
        SIZE
    };

    const std::array<const std::tuple<const size_t, const char*, const char*>, static_cast<size_t>(MEAN_DIVISOR::SIZE)> MEAN_DIVISOR_DESCRIPTION {
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(MEAN_DIVISOR::ONE),                     "ONE",                     "Does not divide the final value by anything"                                                                                   },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(MEAN_DIVISOR::KERNEL_SIZE),             "KERNEL_SIZE",             "Divide the final value at each point by nrow(k)*ncol(k)"                                                                       },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(MEAN_DIVISOR::KERNEL_COUNT),            "KERNEL_COUNT",            "Divide the final value at each point by sum(+!is.na(k))"                                                                        },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(MEAN_DIVISOR::KERNEL_SUM),              "KERNEL_SUM",              "Divide the final value at each point by sum(k[!is.na(k)])"                                                                     },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(MEAN_DIVISOR::KERNEL_ABS_SUM),          "KERNEL_ABS_SUM",          "Divide the final value at each point by sum(abs(k[!is.na(k)]))"                                                                },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(MEAN_DIVISOR::KERNEL_PROD),             "KERNEL_PROD",             "Divide the final value at each point by prod(k[!is.na(k)])"                                                                    },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(MEAN_DIVISOR::KERNEL_ABS_PROD),         "KERNEL_ABS_PROD",         "Divide the final value at each point by prod(abs(k[!is.na(k)]))"                                                               },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(MEAN_DIVISOR::DYNAMIC_COUNT),           "DYNAMIC_COUNT",           "Divide the final value at each point by sum(!is.na( intermediate_data )), recalculated at every point"                         },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(MEAN_DIVISOR::DYNAMIC_SUM),             "DYNAMIC_SUM",             "Divide the final value at each point by sum(intermediate_data[!is.na( intermediate_data )]), recalculated at every point"      },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(MEAN_DIVISOR::DYNAMIC_ABS_SUM),         "DYNAMIC_ABS_SUM",         "Divide the final value at each point by sum(abs(intermediate_data[!is.na( intermediate_data )])), recalculated at every point" },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(MEAN_DIVISOR::DYNAMIC_PROD),            "DYNAMIC_PROD",            "Divide the final value at each point by prod(intermediate_data[!is.na( intermediate_data )]), recalculated at every point"     },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(MEAN_DIVISOR::DYNAMIC_ABS_PROD),        "DYNAMIC_ABS_PROD",        "Divide the final value at each point by prod(abs(intermediate_data[!is.na( intermediate_data )])), recalculated at every point"},
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(MEAN_DIVISOR::DYNAMIC_DATA_SUM),        "DYNAMIC_DATA_SUM",        "Divide the final value at each point by sum(local_data[!is.na( intermediate_data )]), recalculated at every point"             },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(MEAN_DIVISOR::DYNAMIC_DATA_ABS_SUM),    "DYNAMIC_DATA_ABS_SUM",    "Divide the final value at each point by sum(abs(local_data[!is.na( intermediate_data )])), recalculated at every point"        },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(MEAN_DIVISOR::DYNAMIC_DATA_PROD),       "DYNAMIC_DATA_PROD",       "Divide the final value at each point by prod(local_data[!is.na( intermediate_data )]), recalculated at every point"            },
        std::tuple<const size_t, const char*, const char*>{static_cast<size_t>(MEAN_DIVISOR::DYNAMIC_DATA_ABS_PROD),   "DYNAMIC_DATA_ABS_PROD",   "Divide the final value at each point by prod(abs(local_data[!is.na( intermediate_data )])), recalculated at every point"       }
    };


    template<TRANSFORM TRANSFORM_FUNCTION, REDUCE REDUCE_FUNCTION, NAN_POLICY NAN_P, MEAN_DIVISOR MEAN_D, bool VARIANCE, size_t ALIGNMENT=_P_FOCAL_ALLIGNMENT>
    void p_conv(const expanded_aligned_data<ALIGNMENT>& src, const expanded_aligned_data<ALIGNMENT>& kernel, double* dest, const bool open_mp_requested){

        static_assert(TRANSFORM_FUNCTION < TRANSFORM::SIZE,    "TRANSFORM_FUNCTION out of range");
        static_assert(REDUCE_FUNCTION    < REDUCE::SIZE,       "REDUCE_FUNCTION out of range");
        static_assert(NAN_P              < NAN_POLICY::SIZE,   "NAN_POLICY out of range");
        static_assert(MEAN_D             < MEAN_DIVISOR::SIZE, "MEAN_D out of range");


        if(!_P_FOCAL_OPENMP_ENADLED && open_mp_requested){std::cerr << "You are asking for open_mp, but it was not enabled at compile time\n";};

        //set up compile time values
        static const size_t ALIGNMENT_BYTES = expanded_aligned_data<ALIGNMENT>::alignment();

        static const double ACC_INITIAL_VALUE =
            std::array<const double, static_cast<size_t>(REDUCE::SIZE)>({
                0,                                  //SUM
                0,                                  //ABS_SUM
                1,                                  //PRODUCT
                1,                                  //ABS_PRODUCT
                std::numeric_limits<double>::max(), //MIN
                std::numeric_limits<double>::min()  //MAX
            })[static_cast<size_t>(REDUCE_FUNCTION)];

        //set up once per call values
        const bool open_mp_enabled = _P_FOCAL_OPENMP_ENADLED && open_mp_requested;

        const size_t s_start_position = src.start_position;

        const double* const d_p = (double*)__builtin_assume_aligned((src.data+s_start_position), ALIGNMENT_BYTES);
        const double* const k_p = (double*)__builtin_assume_aligned((kernel.data), ALIGNMENT_BYTES);

        const size_t d_col_size = src.col_size;
        const size_t k_col_size = kernel.col_size;

        const size_t d_cols = src.n_col;
        const size_t d_rows = src.n_row;
        const size_t k_cols = kernel.n_col;
        const size_t k_rows = kernel.n_row;



        //For non-dynamic divisors, calculate them once
        double mean_divider_temp __attribute__((unused));

        if constexpr(MEAN_D == MEAN_DIVISOR::ONE){
            mean_divider_temp = 1;

        }else if constexpr(MEAN_D == MEAN_DIVISOR::KERNEL_SIZE){
            mean_divider_temp = k_cols * k_rows;

        }else if constexpr(
                MEAN_D ==  MEAN_DIVISOR::KERNEL_COUNT   ||
                MEAN_D ==  MEAN_DIVISOR::KERNEL_SUM     ||
                MEAN_D ==  MEAN_DIVISOR::KERNEL_ABS_SUM ||
                MEAN_D ==  MEAN_DIVISOR::KERNEL_PROD    ||
                MEAN_D ==  MEAN_DIVISOR::KERNEL_ABS_PROD){

            //If we are taking a product, we start at 1, and otherwise we must be taking a sum so we start at 0
            mean_divider_temp = +(MEAN_D ==  MEAN_DIVISOR::KERNEL_PROD || MEAN_D ==  MEAN_DIVISOR::KERNEL_ABS_PROD);
            for(size_t k_col=0; k_col<k_cols; k_col++){
                const double* const k_col_p     = (double*)__builtin_assume_aligned((k_p + k_col*k_col_size), ALIGNMENT_BYTES);

                for(size_t k_row=0; k_row < k_rows; k_row++){
                    if(!std::isnan(k_col_p[k_row])){
                        if constexpr(MEAN_D == MEAN_DIVISOR::KERNEL_COUNT){
                            mean_divider_temp += 1;

                        }else if constexpr(MEAN_D == MEAN_DIVISOR::KERNEL_SUM){
                            mean_divider_temp += k_col_p[k_row];

                        }else if constexpr(MEAN_D == MEAN_DIVISOR::KERNEL_ABS_SUM){
                            mean_divider_temp += std::abs(k_col_p[k_row]);

                        }else if constexpr(MEAN_D == MEAN_DIVISOR::KERNEL_PROD || MEAN_D == MEAN_DIVISOR::KERNEL_ABS_PROD){ //do abs at the end
                            mean_divider_temp *= k_col_p[k_row];

                        }else{
                            static_assert((size_t)MEAN_D & 0);
                        }
                    }
                }
            }
        }else{
            mean_divider_temp = 0;
        }

        if constexpr(MEAN_D ==  MEAN_DIVISOR::KERNEL_ABS_PROD){
            mean_divider_temp = std::abs(mean_divider_temp);
        }

        const double mean_divider __attribute__((unused)) = mean_divider_temp;

        //start the main loops

        #pragma omp parallel if(open_mp_enabled)
        for(size_t d_col=0; d_col<d_cols; d_col++){
            const double* const d_col_p = (double*)__builtin_assume_aligned((d_p+d_col*d_col_size), ALIGNMENT_BYTES);
            double* const dest_col_p = dest+d_col*d_rows;

            for(size_t d_row=0; d_row<d_rows; d_row++){

                double acc = ACC_INITIAL_VALUE;
                //if we are taking a product, we start with 1, otherwise we start with 0
                double local_mean_divider_temp __attribute__((unused)) = +(
                        MEAN_D == MEAN_DIVISOR::DYNAMIC_PROD       ||
                        MEAN_D == MEAN_DIVISOR::DYNAMIC_ABS_PROD   ||
                        MEAN_D == MEAN_DIVISOR::DYNAMIC_DATA_PROD  ||
                        MEAN_D == MEAN_DIVISOR::DYNAMIC_DATA_ABS_PROD);

                for(size_t k_col=0; k_col<k_cols; k_col++){
                    //if NA_RM_FALSE, NAN will always win, so we can stop early
                    if constexpr(NAN_P == NAN_POLICY::NA_RM_FALSE){
                        if(std::isnan(acc)){
                            continue;
                        }
                    }

                    const double* const k_col_p     = (double*)__builtin_assume_aligned((k_p    + k_col          *k_col_size), ALIGNMENT_BYTES);
                    const double* const d_col_p_off = (double*)__builtin_assume_aligned((d_col_p+(k_col-k_cols/2)*d_col_size), ALIGNMENT_BYTES);

                    for(size_t k_row=0; k_row < k_rows; k_row++){
                        //if NA_RM_FALSE, NAN will always win, so we can stop early
                        if constexpr(NAN_P == NAN_POLICY::NA_RM_FALSE){
                            if(std::isnan(acc)){
                                continue;
                            }
                        }

                        //fetch the kernel value at this point
                        const double k_val = k_col_p[k_row];
                        if constexpr(NAN_P == NAN_POLICY::NA_RM_TRUE){
                            if(std::isnan(k_val)){
                                continue;
                            }
                        }

                        //fetch the data value at this point
                        const double d_val = d_col_p_off[d_row+k_row-k_rows/2];
                        if constexpr(NAN_P == NAN_POLICY::NA_RM_TRUE){
                            if(std::isnan(d_val)){
                                continue;
                            }
                        }

                        //calculate the intermediate value at this point
                        double intermediate;

                        if constexpr(TRANSFORM_FUNCTION == TRANSFORM::MULTIPLY){
                            intermediate = d_val * k_val;

                        }else if constexpr(TRANSFORM_FUNCTION == TRANSFORM::ADD){
                            intermediate = d_val + k_val;

                        }else if constexpr(TRANSFORM_FUNCTION == TRANSFORM::R_EXP){
                            intermediate = std::pow(d_val, k_val);

                        }else if constexpr(TRANSFORM_FUNCTION == TRANSFORM::L_EXP){
                            intermediate = std::pow(k_val, d_val);

                        }else{
                            static_assert((size_t)TRANSFORM_FUNCTION & 0);
                        }

                        if constexpr(NAN_P == NAN_POLICY::NA_RM_FALSE){
                            if(std::isnan(intermediate)){
                                acc = std::numeric_limits<double>::quiet_NaN();
                                continue;
                            }
                        }else if constexpr(NAN_P == NAN_POLICY::NA_RM_TRUE){
                            if(std::isnan(intermediate)){
                                continue;
                            }
                        }

                        //if we get here, run the reduce function

                        if constexpr(REDUCE_FUNCTION == REDUCE::SUM){
                            acc += intermediate;

                        }else if constexpr(REDUCE_FUNCTION == REDUCE::ABS_SUM){
                            acc += std::abs(intermediate);

                        }else if constexpr(REDUCE_FUNCTION == REDUCE::PRODUCT || REDUCE_FUNCTION == REDUCE::ABS_PRODUCT){ //do the abs at the end
                            acc *= intermediate;

                        }else if constexpr(REDUCE_FUNCTION == REDUCE::MIN){
                            acc = std::min(acc, intermediate);

                        }else if constexpr(REDUCE_FUNCTION == REDUCE::MAX){
                            acc = std::max(acc, intermediate);

                        }else{
                            static_assert((size_t)REDUCE_FUNCTION & 0);

                        }

                        //if we are doing a dynamic divider, update it
                        if constexpr(MEAN_D == MEAN_DIVISOR::DYNAMIC_COUNT){
                            local_mean_divider_temp += 1;

                        }else if constexpr(MEAN_D == MEAN_DIVISOR::DYNAMIC_SUM){
                            local_mean_divider_temp += intermediate;

                        }else if constexpr(MEAN_D == MEAN_DIVISOR::DYNAMIC_ABS_SUM){
                            local_mean_divider_temp += std::abs(intermediate);

                        }else if constexpr(MEAN_D == MEAN_DIVISOR::DYNAMIC_PROD || MEAN_D == MEAN_DIVISOR::DYNAMIC_ABS_PROD){ //do the abs at the end
                            local_mean_divider_temp *= intermediate;

                        }else if constexpr(MEAN_D == MEAN_DIVISOR::DYNAMIC_DATA_SUM){
                            local_mean_divider_temp += d_val;

                        }else if constexpr(MEAN_D == MEAN_DIVISOR::DYNAMIC_DATA_ABS_SUM){
                            local_mean_divider_temp += std::abs(d_val);

                        }else if constexpr(MEAN_D == MEAN_DIVISOR::DYNAMIC_DATA_PROD || MEAN_D == MEAN_DIVISOR::DYNAMIC_DATA_ABS_PROD){ //do the abs at the end
                            local_mean_divider_temp *= d_val;
                        }
                    }//kernel values
                }//kernel cols

                //do the defered abs for foo_abs_product
                if constexpr(REDUCE_FUNCTION == REDUCE::ABS_PRODUCT){
                    acc = std::abs(acc);
                }
                if constexpr(MEAN_D == MEAN_DIVISOR::DYNAMIC_ABS_PROD || MEAN_D == MEAN_DIVISOR::DYNAMIC_DATA_ABS_PROD){
                    local_mean_divider_temp = std::abs(local_mean_divider_temp);
                }

                //combine the global or the local divider
                double mean_d_temp __attribute__((unused));;
                if constexpr(
                        MEAN_D == MEAN_DIVISOR::ONE            ||
                        MEAN_D == MEAN_DIVISOR::KERNEL_SIZE    ||
                        MEAN_D == MEAN_DIVISOR::KERNEL_COUNT   ||
                        MEAN_D == MEAN_DIVISOR::KERNEL_SUM     ||
                        MEAN_D == MEAN_DIVISOR::KERNEL_ABS_SUM ||
                        MEAN_D == MEAN_DIVISOR::KERNEL_PROD    ||
                        MEAN_D == MEAN_DIVISOR::KERNEL_ABS_PROD){
                    mean_d_temp = mean_divider;
                }else{
                    mean_d_temp = local_mean_divider_temp;
                }
                const double mean_d __attribute__((unused)) = mean_d_temp;

                if constexpr(VARIANCE){
                    //we want the variance, not the value
                    const double mean = acc/mean_d;

                    acc = ACC_INITIAL_VALUE;

                    for(size_t k_col=0; k_col<k_cols; k_col++){
                        //if NA_RM_FALSE, NAN will always win, so we can stop early
                        if constexpr(NAN_P == NAN_POLICY::NA_RM_FALSE){
                            if(std::isnan(acc)){
                                continue;
                            }
                        }

                        const double* const k_col_p     = (double*)__builtin_assume_aligned((k_p    + k_col          *k_col_size), ALIGNMENT_BYTES);
                        const double* const d_col_p_off = (double*)__builtin_assume_aligned((d_col_p+(k_col-k_cols/2)*d_col_size), ALIGNMENT_BYTES);

                        for(size_t k_row=0; k_row < k_rows; k_row++){
                            //if NA_RM_FALSE, NAN will always win, so we can stop early
                            if constexpr(NAN_P == NAN_POLICY::NA_RM_FALSE){
                                if(std::isnan(acc)){
                                    continue;
                                }
                            }

                            //fetch the kernel value at this point
                            const double k_val = k_col_p[k_row];
                            if constexpr(NAN_P == NAN_POLICY::NA_RM_TRUE){
                                if(std::isnan(k_val)){
                                    continue;
                                }
                            }

                            //fetch the data value at this point
                            const double d_val = d_col_p_off[d_row+k_row-k_rows/2];
                            if constexpr(NAN_P == NAN_POLICY::NA_RM_TRUE){
                                if(std::isnan(d_val)){
                                    continue;
                                }
                            }

                            //calculate the intermediate value at this point
                            double intermediate;

                            if constexpr(TRANSFORM_FUNCTION == TRANSFORM::MULTIPLY){
                                intermediate = d_val * k_val;

                            }else if constexpr(TRANSFORM_FUNCTION == TRANSFORM::ADD){
                                intermediate = d_val + k_val;

                            }else if constexpr(TRANSFORM_FUNCTION == TRANSFORM::R_EXP){
                                intermediate = std::pow(d_val, k_val);

                            }else if constexpr(TRANSFORM_FUNCTION == TRANSFORM::L_EXP){
                                intermediate = std::pow(k_val, d_val);

                            }else{
                                static_assert((size_t)TRANSFORM_FUNCTION & 0);
                            }

                            if constexpr(NAN_P == NAN_POLICY::NA_RM_FALSE){
                                if(std::isnan(intermediate)){
                                    acc = std::numeric_limits<double>::quiet_NaN();
                                    continue;
                                }
                            }else if constexpr(NAN_P == NAN_POLICY::NA_RM_TRUE){
                                if(std::isnan(intermediate)){
                                    continue;
                                }
                            }

                            intermediate = (intermediate-mean)*(intermediate-mean);

                            //if we get here, run the reduce function

                            if constexpr(REDUCE_FUNCTION == REDUCE::SUM || REDUCE_FUNCTION == REDUCE::ABS_SUM){//intermediate is positive
                                acc += intermediate;

                            }else if constexpr(REDUCE_FUNCTION == REDUCE::PRODUCT || REDUCE_FUNCTION == REDUCE::ABS_PRODUCT){ //intermediate is positive
                                acc *= intermediate;

                            }else if constexpr(REDUCE_FUNCTION == REDUCE::MIN){
                                acc = std::min(acc, intermediate);

                            }else if constexpr(REDUCE_FUNCTION == REDUCE::MAX){
                                acc = std::max(acc, intermediate);

                            }else{
                                static_assert((size_t)REDUCE_FUNCTION & 0);

                            }

                        }//kernel values
                    }//kernel cols
                }//variance
                if constexpr(MEAN_D == MEAN_DIVISOR::ONE){
                    dest_col_p[d_row] = acc;
                }else{
                    dest_col_p[d_row] = acc/mean_d;
                    /*
                    if constexpr(NAN_P == NAN_POLICY::FAST){
                        dest_col_p[d_row] = acc/mean_d;
                    }else if constexpr(NAN_P == NAN_POLICY::NA_RM_TRUE){
                        if(mean_d){
                            dest_col_p[d_row] = acc/mean_d;
                        }else{
                            dest_col_p[d_row] = acc;
                        }
                    }else if constexpr(NAN_P == NAN_POLICY::NA_RM_FALSE){
                        if(mean_d){
                            dest_col_p[d_row] = acc/mean_d;
                        }else{
                            dest_col_p[d_row] = NAN;
                        }
                    }else{
                        static_assert((size_t)NAN_P & 0);
                    }*/
                }
            }//data vals
        }//data cols
    }
}

#endif // P_FOCAL_H_
