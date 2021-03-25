#ifndef _P_FOCAL_H_
#define _P_FOCAL_H_

#ifdef _OPENMP
#include <omp.h>
// [[Rcpp::plugins(openmp)]]
#define _P_FOCAL_OPENMP_ENADLED 1
#else
#define _OPENMP_ENADLED 0
#define _P_FOCAL_ALLIGNMENT
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

namespace p_focal{

    template<size_t DATA_ALIGNMENT=_P_FOCAL_ALLIGNMENT>
    struct expanded_aligned_data{

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
        size_t data_size;
        size_t block_size;
        size_t n_col;
        size_t n_row;
        size_t start_position;
        size_t col_size;
        size_t left_cols;
        size_t right_cols;
        size_t extra_rows;

        static consteval size_t alignment(){return std::max(DATA_ALIGNMENT, alignof(double));}

        void copy_in(const double* const incomming_data){
            const size_t n_col = this->n_col;
            const size_t n_row = this->n_row;
            const size_t start_position = this->start_position;
            const size_t col_size = this->col_size;
            double* data = this->data;

            {
                double* col_start;
                size_t col;
                #pragma omp parallel for simd aligned(data:alignment()) aligned(col_start:alignment())
                for(col = 0; col<n_col; col++){
                    col_start = data+start_position+(col_size*col);
                    memcpy(col_start, incomming_data+(n_row*col), n_row*sizeof(double));
                }
            }
        }

        expanded_aligned_data(const size_t n_col, const size_t n_row, const size_t extra_col, const size_t extra_row, const double default_value){
            this->n_col = n_col;
            this->n_row = n_row;

            const size_t block_size = alignment()/sizeof(double);
            this->block_size = block_size;

            this->left_cols  = std::max(extra_col, (size_t)1);
            this->right_cols = extra_col;


            this->col_size = (((this->n_row+extra_row-1)/block_size)+1)*block_size;
            this->extra_rows = this->col_size-this->n_row;


            this->start_position = this->left_cols*col_size;


            this->data_size = (this->left_cols + this->n_col + this->right_cols)*this->col_size;



            std::cerr   << "--------------------------------------------------------------------------\n"
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

            double* data = (double*)std::aligned_alloc(alignment(), this->data_size*sizeof(double));
            if(!data){
                std::cerr << "Out of memory\n";
                throw "Out of memory";
            }
            this->data = data;

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

            this->data = std::aligned_alloc(this->block_size, this->data_size);
            if(!data){
                throw "Out of memory";
            }

            double* tdata = this->data;
            const double* odata = other.data;
            const size_t block_size = this->block_size;
            const size_t data_size = this->data_size;

            //#pragma omp parallel for simd aligned(tdata:block_size) aligned(odata:block_size)
            //for(size_t i = 0; i< data_size; i++){
            //    tdata[i] = odata[i];
            //}
            memcpy(tdata, odata, data_size);
        }

        ~expanded_aligned_data() noexcept{
            std::free(this->data);
            this->data = nullptr;
        }

        void swap(expanded_aligned_data<DATA_ALIGNMENT>& rhs) noexcept{
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
                const auto* c_data = data+col*col_size;
                #pragma simd aligned(c_data:block_size)
                for(size_t row=0; row<n_row; row++){
                    output[col*n_row+row] = c_data[row];
                }
            }
        }

    };

    template<size_t DATA_ALIGNMENT>
    void swap(expanded_aligned_data<DATA_ALIGNMENT>& lhs, expanded_aligned_data<DATA_ALIGNMENT>& rhs) noexcept{
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
        DIVIDE,
        ADD,
        L_EXP,
        R_EXP,
        SIZE
    };
    constexpr std::array<const char*, (size_t)TRANSFORM::SIZE> TRANSFORM_FUNCTION_LIST{"*","/","+","x^w","w^x"};
    static_assert(TRANSFORM_FUNCTION_LIST.back(), "TRANSFORM_FUNCTION_LIST has more space than it has values, fix it's template paramiter");

    enum class REDUCE : size_t{
        SUM=0,
        PRODUCT,
        MIN,
        MAX,
        MEAN,
        SIZE
    };
    constexpr std::array<const char*, (size_t)REDUCE::SIZE> REDUCE_FUNCTION_LIST{"sum","product","min","max","mean"};
    static_assert(REDUCE_FUNCTION_LIST.back(), "REDUCE_FUNCTION_LIST has more space than it has values, fix it's template paramiter");

    enum class STRATEGY : size_t{
        BASIC,
        NAIVE,
        SIZE
    };
    constexpr std::array<const char*, (size_t)STRATEGY::SIZE> STRATEGY_LIST{"naive","naive_p"};
    static_assert(STRATEGY_LIST.back(), "STRATEGY_LIST has more space than it has values, fix it's template paramiter");



    template<TRANSFORM TRANSFORM_FUNCTION, REDUCE REDUCE_FUNCTION, STRATEGY STRAT, size_t ALIGNMENT=_P_FOCAL_ALLIGNMENT>
    void p_conv(const expanded_aligned_data<ALIGNMENT>& src, expanded_aligned_data<ALIGNMENT>& dest, const double* const kernel, const size_t k_cols, const size_t k_rows){
        static_assert(TRANSFORM_FUNCTION < TRANSFORM::SIZE, "TRANSFORM_FUNCTION out of range");
        static_assert(REDUCE_FUNCTION < REDUCE::SIZE, "REDUCE_FUNCTION out of range");
        constinit static const size_t block_size = expanded_aligned_data<ALIGNMENT>::alignment();


        //for each column within the output space
        for(size_t col = 0; col<dest.n_col; col++){
            //for each row within the output space
            for(size_t row = 0; row<dest.n_row; row++){
                double acc = 0;

                //for each column in the kernel
                for(size_t k_col = 0; k_col<k_cols; k_col++){
                    //for each row in the kernel
                    for(size_t k_row = 0; k_row<k_rows; k_row++){
                        acc += src.data[src.start_position + (col+k_col-k_cols/2)*src.col_size + row+k_row-k_rows/2 ]*kernel[k_col*k_rows+k_row];
                    }
                }

                dest.data[dest.start_position + (col*dest.col_size) + row] = acc;
            }
        }

    }
}




#endif // _P_FOCAL_H_
