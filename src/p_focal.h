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
                #pragma simd aligned(c_data:block_size)
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

    std::tuple<int, int, int> openmp_self_test(void) noexcept{
        if constexpr(_P_FOCAL_OPENMP_ENADLED){
            return {1, _OPENMP, omp_get_max_threads()};
        }else{
            return {0,0,1};
        }
    }

    enum class TRANSFORM : size_t{
        MULTIPLY=0,
        ADD,
        R_EXP,
        L_EXP,
        SIZE
    };

    enum class REDUCE : size_t{
        SUM=0,
        PRODUCT,
        MIN,
        MAX,
        MEAN,
        VARIANCE,
        SIZE
    };

    enum class NAN_POLICY : size_t{
        NOTHING_SPECIAL=0,
        SKIP_NAN_IN_KERNEL,
        SKIP_NAN_IN_DATA,
        SKIP_NAN_IN_KERNEL_OR_DATA,
        SKIP_NAN_IN_INTERMEDIATE,
        SIZE
    };

    enum class MEAN_POLICY : size_t{
        KENEL_SIZE=0,
        SUM_NON_NAN_KERNEL_VALUES,
        MUL_NON_NAN_KERNEL_VALUES,
        COUNT_NON_NAN_KERNEL_VALUES,
        COUNT_NON_NAN_INTERMEDIATE_VALUES,
        SIZE
    };

    template<TRANSFORM TRANSFORM_FUNCTION, REDUCE REDUCE_FUNCTION, NAN_POLICY NAN_P, MEAN_POLICY MEAN_P, size_t ALIGNMENT=_P_FOCAL_ALLIGNMENT>
    void p_conv(const expanded_aligned_data<ALIGNMENT>& src, const expanded_aligned_data<ALIGNMENT>& kernel, double* out_p, bool open_mp_requested){
        static_assert(TRANSFORM_FUNCTION < TRANSFORM::SIZE,   "TRANSFORM_FUNCTION out of range");
        static_assert(REDUCE_FUNCTION    < REDUCE::SIZE,      "REDUCE_FUNCTION out of range");
        static_assert(NAN_P              < NAN_POLICY::SIZE,  "NAN_POLICY out of range");
        static_assert(MEAN_P             < MEAN_POLICY::SIZE, "MEAN_POLICY out of range");

        if(!_P_FOCAL_OPENMP_ENADLED && open_mp_requested){std::cerr << "You are asking for open_mp, but it was not enabled at compile time\n";};

        static const size_t ALIGNMENT_BYTES = expanded_aligned_data<ALIGNMENT>::alignment();
        static const double ACC_INITIAL_VALUE = ((std::array<double, (size_t)REDUCE::SIZE>){0, 1, std::numeric_limits<double>::max(), std::numeric_limits<double>::min(), 0, 0})[(size_t)REDUCE_FUNCTION];

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

        //work out what the divider should be if we were to use the mean for something
        double mean_divider_temp;
        if constexpr(MEAN_P == MEAN_POLICY::KENEL_SIZE){
            mean_divider_temp = k_cols * k_rows;
        }else if constexpr(MEAN_P == MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES || MEAN_P == MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES || MEAN_P == MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES){
            mean_divider_temp = !!(MEAN_P == MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES);
            for(size_t k_col=0; k_col<k_cols; k_col++){
                const double* const k_col_p     = (double*)__builtin_assume_aligned((k_p + k_col*k_col_size), ALIGNMENT_BYTES);
                for(size_t k_row=0; k_row < k_rows; k_row++){
                    if(!std::isnan(k_col_p[k_row])){
                        if constexpr(MEAN_P == MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES){
                            mean_divider_temp += k_col_p[k_row];
                        }else if constexpr(MEAN_P == MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES){
                            mean_divider_temp *= k_col_p[k_row];
                        }else if constexpr(MEAN_P == MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES){
                            mean_divider_temp++;
                        }else{
                            static_assert((size_t)MEAN_P & 0);
                        }
                    }
                }
            }
        }else if constexpr(MEAN_P == MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES){
            mean_divider_temp = 0;
        }else{
            static_assert((size_t)MEAN_P & 0);
        }
        const double mean_divider = mean_divider_temp;


        #pragma omp parallel if(open_mp_enabled)
        for(size_t d_col=0; d_col<d_cols; d_col++){
            const double* const d_col_p = (double*)__builtin_assume_aligned((d_p+d_col*d_col_size), ALIGNMENT_BYTES);
             double* const out_col_p = out_p+d_col*d_rows;

            for(size_t d_row=0; d_row<d_rows; d_row++){

                double acc = ACC_INITIAL_VALUE;
                double local_mean_divider = 0;

                for(size_t k_col=0; k_col<k_cols; k_col++){
                    const double* const k_col_p     = (double*)__builtin_assume_aligned((k_p    + k_col          *k_col_size), ALIGNMENT_BYTES);
                    const double* const d_col_p_off = (double*)__builtin_assume_aligned((d_col_p+(k_col-k_cols/2)*d_col_size), ALIGNMENT_BYTES);

                    for(size_t k_row=0; k_row < k_rows; k_row++){

                        const double k_val = k_col_p[k_row];
                        if constexpr(NAN_P == NAN_POLICY::SKIP_NAN_IN_KERNEL || NAN_P == NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA){
                            if(std::isnan(k_val)){
                                continue;
                            }
                        }

                        const double d_val = d_col_p_off[d_row+k_row-k_rows/2];
                        if constexpr(NAN_P == NAN_POLICY::SKIP_NAN_IN_DATA || NAN_P == NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA){
                            if(std::isnan(d_val)){
                                continue;
                            }
                        }

                        double p;

                        if constexpr(TRANSFORM_FUNCTION == TRANSFORM::MULTIPLY){
                            p = k_val * d_val;
                        }else if constexpr(TRANSFORM_FUNCTION == TRANSFORM::ADD){
                            p = k_val + d_val;
                        }else if constexpr(TRANSFORM_FUNCTION == TRANSFORM::R_EXP){
                            p = pow(d_val, k_val);
                        }else if constexpr(TRANSFORM_FUNCTION == TRANSFORM::L_EXP){
                            p = pow(k_val, d_val);
                        }else{
                            static_assert((size_t)TRANSFORM_FUNCTION & 0);
                        }

                        if constexpr(NAN_P == NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE){
                            if(std::isnan(p)){
                                continue;
                            }
                        }

                        if constexpr(REDUCE_FUNCTION == REDUCE::SUM || REDUCE_FUNCTION == REDUCE::MEAN || REDUCE_FUNCTION == REDUCE::VARIANCE){
                            acc += p;
                        }else if constexpr(REDUCE_FUNCTION == REDUCE::PRODUCT){
                            acc *= p;
                        }else if constexpr(REDUCE_FUNCTION == REDUCE::MIN){
                            //std::cout << d_col << ", " << d_row << ", " << k_col << ", " << k_row << ", " << p << ", " << acc;
                            acc = std::fmin(p, acc);
                            //std::cout << ", " << acc << "\n";
                        }else if constexpr(REDUCE_FUNCTION == REDUCE::MAX){
                            acc = std::fmax(p, acc);
                        }else{
                            static_assert((size_t)REDUCE_FUNCTION & 0);
                        }

                        if constexpr(MEAN_P == MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES){
                            local_mean_divider += !(std::isnan(p));
                        }
                    }
                }
                if constexpr(REDUCE_FUNCTION == REDUCE::SUM || REDUCE_FUNCTION == REDUCE::PRODUCT || REDUCE_FUNCTION == REDUCE::MAX || REDUCE_FUNCTION == REDUCE::MIN){
                    out_col_p[d_row] = acc;
                }else if constexpr(REDUCE_FUNCTION == REDUCE::MEAN){
                    if constexpr(MEAN_P == MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES){
                        out_col_p[d_row] = acc/local_mean_divider;
                    }else{
                        out_col_p[d_row] = acc/mean_divider;
                    }
                }else if constexpr(REDUCE_FUNCTION == REDUCE::VARIANCE){
                    //We now know the mean, we need to re-run finding the square of the difference. We can't do this in one pass
                    double mean_div_temp;

                    if constexpr(MEAN_P == MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES){
                        mean_div_temp = local_mean_divider;
                    }else{
                        mean_div_temp = mean_divider;
                    }
                    const double mean_div = mean_div_temp;

                    const double mean = acc/mean_div;

                    //std::cout << d_col << ", " << d_row << ", " << mean_div << ", " << acc << ", " << mean;

                    double acc2 = 0;

                    for(size_t k_col=0; k_col<k_cols; k_col++){
                        const double* const k_col_p     = (double*)__builtin_assume_aligned((k_p    + k_col          *k_col_size), ALIGNMENT_BYTES);
                        const double* const d_col_p_off = (double*)__builtin_assume_aligned((d_col_p+(k_col-k_cols/2)*d_col_size), ALIGNMENT_BYTES);

                        for(size_t k_row=0; k_row < k_rows; k_row++){

                            const double k_val = k_col_p[k_row];
                            if constexpr(NAN_P == NAN_POLICY::SKIP_NAN_IN_KERNEL || NAN_P == NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA){
                                if(std::isnan(k_val)){
                                    continue;
                                }
                            }

                            const double d_val = d_col_p_off[d_row+k_row-k_rows/2];
                            if constexpr(NAN_P == NAN_POLICY::SKIP_NAN_IN_DATA || NAN_P == NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA){
                                if(std::isnan(d_val)){
                                    continue;
                                }
                            }

                            double p;

                            if constexpr(TRANSFORM_FUNCTION == TRANSFORM::MULTIPLY){
                                p = k_val * d_val;
                            }else if constexpr(TRANSFORM_FUNCTION == TRANSFORM::ADD){
                                p = k_val + d_val;
                            }else if constexpr(TRANSFORM_FUNCTION == TRANSFORM::R_EXP){
                                p = pow(d_val, k_val);
                            }else if constexpr(TRANSFORM_FUNCTION == TRANSFORM::L_EXP){
                                p = pow(k_val, d_val);
                            }else{
                                static_assert((size_t)TRANSFORM_FUNCTION & 0);
                            }
                            //std::cout << ", " << k_col << ", " << k_row << ", " << p;
                            if constexpr(NAN_P == NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE){
                                if(std::isnan(p)){
                                    continue;
                                }
                            }
                            acc2 += (p-mean)*(p-mean);
                        }
                    }


                    out_col_p[d_row] = acc2/mean_div;


                    //std::cout << ", " << acc2 << ", " << acc2/mean_div << "\n";

                }else{
                    static_assert((size_t)REDUCE_FUNCTION & 0);
                }
            }
        }
    }
}

#endif // P_FOCAL_H_
