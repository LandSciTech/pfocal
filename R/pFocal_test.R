
# Testing functions -------------------------------------------------------
# This code is provided for forming a basis to tests and showing readers
# how to compare outputs wit the R implementation.

.implies <- function(a, b)
  a | !b

#for all values, closer to 0 is better
.pFocal_compare <- function(
  data, 
  kernel, 
  edge_value = 0, 
  transform_function = "MULTIPLY", 
  reduce_function = "SUM", 
  mean_divider = "ONE", 
  variance=FALSE){
  
  rv_t <- NA
  cv_t <- NA
  
  rv_n <- NA
  cv_n <- NA
  
  rv_f <- NA
  cv_f <- NA
  
  times <- c(
    system.time(rv_t <- pFocal(data, kernel, edge_value, transform_function, 
                               reduce_function, mean_divider, variance, na.rm=TRUE, 
                               mp=FALSE, debug_use_r_implementation = TRUE)),
    system.time(cv_t <- pFocal(data, kernel, edge_value, transform_function, 
                               reduce_function, mean_divider, variance, na.rm=TRUE, 
                               mp=TRUE, debug_use_r_implementation = FALSE)),
    
    system.time(rv_n <- pFocal(data, kernel, edge_value, transform_function, 
                               reduce_function, mean_divider, variance, na.rm=NA, 
                               mp=FALSE, debug_use_r_implementation = TRUE)),
    system.time(cv_n <- pFocal(data, kernel, edge_value, transform_function, 
                               reduce_function, mean_divider, variance, na.rm=NA, 
                               mp=TRUE, debug_use_r_implementation = FALSE)),
    
    system.time(rv_f <- pFocal(data, kernel, edge_value, transform_function, 
                               reduce_function, mean_divider, variance, na.rm=TRUE, 
                               mp=FALSE, debug_use_r_implementation = TRUE)),
    system.time(cv_f <- pFocal(data, kernel, edge_value, transform_function, 
                               reduce_function, mean_divider, variance, na.rm=TRUE, 
                               mp=TRUE, debug_use_r_implementation = FALSE))
  )
  
  v_t = rv_t+rv_t
  v_n = rv_n+rv_n
  v_f = rv_f+rv_f
  
  v_t[!v_t] <- Inf
  v_n[!v_n] <- Inf
  v_f[!v_f] <- Inf
  
  na_rv_t = is.na(rv_t)
  na_cv_t = is.na(cv_t)
  na_v_t = is.na(v_t)
  
  na_rv_n = is.na(rv_n)
  na_cv_n = is.na(cv_n)
  na_v_n = is.na(v_n)
  
  na_rv_f = is.na(rv_f)
  na_cv_f = is.na(cv_f)
  na_v_f = is.na(v_f)
  
  c(
    transform_function, 
    reduce_function, 
    mean_divider, 
    c("FALSE", "TRUE")[1+variance],
    
    #sum(!.implies(na_rv_t, na_cv_t)),
    #sum(!.implies(na_cv_t, na_rv_t)),
    
    #sum(!.implies(na_rv_f, na_cv_f)),
    #sum(!.implies(na_cv_f, na_rv_f)),
    
    #sum(!.implies(na_rv_n, na_rv_t)),
    #sum(!.implies(na_cv_n, na_cv_t)),
    
    #sum(!.implies(!na_rv_f, !na_rv_n)),
    #sum(!.implies(!na_cv_f, !na_cv_n)),
    
    # sum(abs((rv_t-cv_t)[(!na_rv_t) && (!na_cv_t)])), #abs error of non NA values when na.rm=TRUE
    # sum(abs((rv_t-cv_n)[(!na_rv_n) && (!na_cv_n)])), #abs error of non NA values when na.rm=NA
    # sum(abs((rv_t-cv_f)[(!na_rv_f) && (!na_cv_f)])), #abs error of non NA values when na.rm=FALSE
    #  
    max(c(0, (abs(rv_t-cv_t)/abs(v_t))[(!na_rv_t) & (!na_cv_t) & (!na_v_t)])), #highest reletive error in na.rm=TRUE
    max(c(0, (abs(rv_n-cv_n)/abs(v_n))[(!na_rv_n) & (!na_cv_n) & (!na_v_n)])), #highest reletive error in na.rm=TRUE
    max(c(0, (abs(rv_f-cv_f)/abs(v_f))[(!na_rv_f) & (!na_cv_f) & (!na_v_f)])), #highest reletive error in na.rm=TRUE
    
    #  times
    0)
}

.pFocal_compare_sweep <- function(data, kernel, edge_value){
  
  for(v in 0:1){
    for(m in pFocal_mean_divisor_info()[,1]){
      for(r in pFocal_reduce_info()[,1]){
        for(t in pFocal_transform_info()[,1]){
          o <- .pFocal_compare(data, kernel, edge_value, t, r, m, v)
          if(any(o[5:length(o)] > 0)){
            print(o[1:4])
            print(o[5:length(o)])
          }
        }
      }
    }
  }
}

