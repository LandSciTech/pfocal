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

template<size_t DATA_ALIGNMENT=_P_FOCAL_ALLIGNMENT, class DATA_TYPE=double>
struct expanded_aligned_data{
    double* data;
    size_t data_size;
    size_t block_size;
    size_t n_col;
    size_t n_row;
    size_t start_position;
    size_t col_size;
    size_t left_cols;
    size_t right_cols;
    size_t e_rows;

    expanded_aligned_data(const DATA_TYPE* const incomming_data, const size_t n_col, const size_t n_row, const size_t extra_col, const size_t extra_row, const DATA_TYPE default_value){
        this->n_col = n_col;
        this->n_row = n_row;

        const size_t block_size = std::max(DATA_ALIGNMENT, std::alignment_of<DATA_TYPE>());
        this->block_size = block_size;


        this->col_size = (((n_row+extra_row)/this->block_size)+!((n_row+extra_row)%this->block_size))*this->block_size;

        this->left_cols = std::min(extra_col, (size_t)1);
        this->start_position = this->left_cols*col_size;

        this->data_size = start_position+ n_col+extra_col*col_size;

        this->data = std::aligned_alloc(this->block_size, this->data_size);
        if(!this->data){
            throw "Out of memory";
        }

        const auto data = this->data;
        // Lets fill it now

        //left cols
        #pragma omp parallel for simd aligned(data:block_size)
        for(size_t i = 0; i<this->start_position; i++){
            data[i] = default_value;
        }

        //cols with data
        #pragma omp parallel for simd aligned(data:block_size)
        for(size_t col = 0; col<n_col; col++){
            for(size_t row = 0; row<n_row; row++){
                this->data[(this->left_cols+col)*this->col_size + row] = incomming_data[col*n_row+row];
            }
            for(size_t row = n_row; row++; row<this->col_size){
                this->data[(this->left_cols+col)*this->col_size + row] = default_value;
            }
        }

        //right cols
        #pragma omp parallel for simd aligned(data:block_size)
        for(size_t i = this->start_position+(this->n_col*this->col_size); i<this->data_size; i++){
            data[i] = default_value;
        }
    }

    expanded_aligned_data(expanded_aligned_data<DATA_ALIGNMENT, DATA_TYPE>&&)=default;

    expanded_aligned_data(const expanded_aligned_data<DATA_ALIGNMENT, DATA_TYPE>& other){
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

        const auto* tdata = this->data;
        const auto* odata = other.data;
        const size_t block_size = this->block_size;
        const size_t data_size = this->data_size;

        #pragma omp parallel for simd aligned(tdata:block_size) aligned(odata:block_size)
        for(size_t i = 0; i< data_size; i++){
            tdata[i] = odata[i];
        }
    }

    ~expanded_aligned_data() noexcept{
        std::free(this->data);
        this->data = nullptr;
    }

    void swap(expanded_aligned_data<DATA_ALIGNMENT, DATA_TYPE>& rhs) noexcept{
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

    void copy_out(DATA_TYPE* output) const noexcept{
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

template<size_t DATA_ALIGNMENT, class DATA_TYPE>
void swap(expanded_aligned_data<DATA_ALIGNMENT, DATA_TYPE>& lhs, expanded_aligned_data<DATA_ALIGNMENT, DATA_TYPE>& rhs) noexcept{
    lhs.swap(rhs);
}

std::tuple<int, int, int> openmp_self_test(void) noexcept{
    if constexpr(_P_FOCAL_OPENMP_ENADLED){
        return {1, _OPENMP, omp_get_max_threads()};
    }else{
        return {0,0,1};
    }
}


constexpr std::array<const char*, 5> TRANSFORM_FUNCTION_LIST{"+","*","/","x^w","w^x"};
static_assert(TRANSFORM_FUNCTION_LIST.back(), "TRANSFORM_FUNCTION_LIST has more space than it has values, fix it's template paramiter");
constexpr std::array<const char*, 5> REDUCE_FUNCTION_LIST{"sum","product","min","max","mean"};
static_assert(REDUCE_FUNCTION_LIST.back(), "REDUCE_FUNCTION_LIST has more space than it has values, fix it's template paramiter");





#endif // _P_FOCAL_H_
