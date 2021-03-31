# R_EXP -> data^kernel
# L_EXP -> kernel^data
# R_DIVIDE -> data/kernel
# L_DIVIDE -> kernel/data
pFocal_weight_fun_list <- function(){
  return (c(
    c("MULTIPLY", "ADD", "R_EXP", "L_EXP"), #in C++, do not change the order
    c("L_DIVIDE", "R_DIVIDE", "SUBTRACT")   #not in c++
  ))
}


pFocal_fun_list <- function(){
  return (c(
    c("SUM", "PRODUCT", "MIN", "MAX", "MEAN", "VARIANCE"), #in C++, do not change the order
    c("STANDARD_DEVIATION", "RANGE")                       #not in C++
  ))
}

pFocal_na_policy_list <- function(){
  return(c("NOTHING_SPECIAL",
           "SKIP_NAN_IN_KERNEL",
           "SKIP_NAN_IN_DATA",
           "SKIP_NAN_IN_KERNEL_OR_DATA",
           "SKIP_NAN_IN_INTERMEDIATE"))
}

pFocal_mean_policy_list <- function(){
  return(c("KERNEL_SIZE",
           "SUM_NON_NAN_KERNEL_VALUES",
           "MUL_NON_NAN_KERNEL_VALUES",
           "COUNT_NON_NAN_KERNEL_VALUES",
           "COUNT_NON_NAN_INTERMEDIATE_VALUES"))
}

.pFocal_weight_fun_narrow <- function(f){
  index <- 0
  if(is(f, "numeric")){
    if(0<f && f<=length(pFocal_weight_fun_list())){
      index <- f
    }
  }else{
    u <- toupper(f)
    index <- 0;
    if(u %in% c("MULTIPLY", "MUL", "TIMES", "PROD", "PRODUCT", "*", "D*K", "K*D")){
      index <- match("MULTIPLY", pFocal_weight_fun_list())[1]
      
    }else if(u %in% c("ADD", "SUM", "+", "D+K", "K+D")){
      index <- match("ADD", pFocal_weight_fun_list())[1]
      
    }else if(u %in% c("R_EXP", "EXP", "^", "D^K")){
      index <- match("R_EXP", pFocal_weight_fun_list())[1]
      
    }else if(u %in% c("L_EXP", "K^D")){
      index <- match("L_EXP", pFocal_weight_fun_list())[1]
      
    }else if(u %in% c("L_DIVIDE", "\\", "K/D")){
      index <- match("L_DIVIDE", pFocal_weight_fun_list())[1]
      
    }else if(u %in% c("R_DIVIDE", "/", "D/K")){
      index <- match("R_DIVIDE", pFocal_weight_fun_list())[1]
      
    }else if(u %in% c("SUBTRACT", "-", "D-K")){
      index <- match("SUBTRACT", pFocal_weight_fun_list())[1]
    }
  }
  if(is.na(index)){
    stop("Something is very wrong with '.pFocal_weight_fun_narrow'. Did someone change pFocal_weight_fun_list and/or .pFocal_weight_fun_narrow?")
  }
  return(index)
}


.pFocal_fun_narrow <- function(f){
  index <- 0
  if(is(f, "numeric")){
    if(0<f && f<=length(pFocal_fun_list())){
      index <- f
    }
  }else{
    u <- toupper(f)
    index <- 0;
    if(u %in% c("SUM", "ADD", "+")){
      index <- match("SUM", pFocal_fun_list())[1]
      
    }else if(u %in% c("PRODUCT", "MUL", "TIMES", "*")){
      index <- match("PRODUCT", pFocal_fun_list())[1]
      
    }else if(u %in% c("MIN", "MINIMUM")){
      index <- match("MIN", pFocal_fun_list())[1]
      
    }else if(u %in% c("MAX", "MAXIMUM")){
      index <- match("MAX", pFocal_fun_list())[1]
      
    }else if(u %in% c("MEAN", "AVERAGE")){
      index <- match("MEAN", pFocal_fun_list())[1]
      
    }else if(u %in% c("VARIANCE", "Σ2", "Σ^2", "S2", "S^2", "SD2", "SD^2")){
      index <- match("VARIANCE", pFocal_fun_list())[1]
      
    }else if(u %in% c("STANDARD_DEVIATION", "SD", "Σ")){
      index <- match("STANDARD_DEVIATION", pFocal_fun_list())[1]
      
    }else if(u %in% c("RANGE")){
      index <- match("RANGE", pFocal_fun_list())[1]
      
    }
  }
  if(is.na(index)){
    stop("Something is very wrong with '.pFocal_fun_narrow'. Did someone change pFocal_fun_list and/or .pFocal_fun_narrow?")
  }
  return(index)
}

.pFocal_na_policy_narrow <- function(f){
  index <- 0
  if(is(f, "numeric")){
    if(0<f && f<=length(pFocal_na_policy_list())){
      index <- f
    }
  }else{
    u <- toupper(f)
    index <- 0;
    if(u %in% c("NOTHING_SPECIAL", "", "NONE")){
      index <- match("NOTHING_SPECIAL", pFocal_na_policy_list())[1]
      
    }else if(u %in% c("SKIP_NAN_IN_KERNEL", "SKIP_NA_IN_KERNEL")){
      index <- match("SKIP_NAN_IN_KERNEL", pFocal_na_policy_list())[1]
      
    }else if(u %in% c("SKIP_NAN_IN_DATA", "SKIP_NA_IN_DATA")){
      index <- match("SKIP_NAN_IN_DATA", pFocal_na_policy_list())[1]
      
    }else if(u %in% c("SKIP_NAN_IN_KERNEL_OR_DATA", "SKIP_NA_IN_KERNEL_OR_DATA")){
      index <- match("SKIP_NAN_IN_KERNEL_OR_DATA", pFocal_na_policy_list())[1]
      
    }else if(u %in% c("SKIP_NAN_IN_INTERMEDIATE", "SKIP_NA_IN_INTERMEDIATE")){
      index <- match("SKIP_NAN_IN_INTERMEDIATE", pFocal_na_policy_list())[1]
    
    }
  }
  if(is.na(index)){
    stop("Something is very wrong with '.pFocal_nan_policy_narrow'. Did someone change pFocal_na_policy_list and/or .pFocal_na_policy_narrow?")
  }
  return(index)
}

.pFocal_mean_policy_narrow <- function(f){
  index <- 0
  if(is(f, "numeric")){
    if(0<f && f<=length(pFocal_mean_policy_list())){
      index <- f
    }
  }else{
    u <- toupper(f)
    index <- 0;
    if(u %in% c("KERNEL_SIZE")){
      index <- match("KERNEL_SIZE", pFocal_mean_policy_list())[1]
      
    }else if(u %in% c("SUM_NON_NAN_KERNEL_VALUES", "SUM_NON_NA_KERNEL_VALUES")){
      index <- match("SUM_NON_NAN_KERNEL_VALUES", pFocal_mean_policy_list())[1]
      
    }else if(u %in% c("MUL_NON_NAN_KERNEL_VALUES", "MUL_NON_NA_KERNEL_VALUES")){
      index <- match("MUL_NON_NAN_KERNEL_VALUES", pFocal_mean_policy_list())[1]
      
    }else if(u %in% c("COUNT_NON_NAN_KERNEL_VALUES", "COUNT_NON_NA_KERNEL_VALUES")){
      index <- match("COUNT_NON_NAN_KERNEL_VALUES", pFocal_mean_policy_list())[1]
      
    }else if(u %in% c("COUNT_NON_NAN_INTERMEDIATE_VALUES", "COUNT_NON_NA_INTERMEDIATE_VALUES")){
      index <- match("COUNT_NON_NAN_INTERMEDIATE_VALUES", pFocal_mean_policy_list())[1]
      
    }
  }
  if(is.na(index)){
    stop("Something is very wrong with '.pFocal_nan_policy_narrow'. Did someone change pFocal_mean_policy_list and/or .pFocal_mean_policy_narrow?")
  }
  return(index)
}


#This function does no checking, don't call this one directly
.pFocal <- function(x, w, fun, weight_fun, na_policy, mean_policy, default, mp=TRUE, debug_use_r_implementation=FALSE){
  
  t_fun <- .pFocal_weight_fun_narrow(weight_fun)
  r_fun <- .pFocal_fun_narrow(fun)
  n_pol <- .pFocal_na_policy_narrow(na_policy)
  mean_pol <- .pFocal_mean_policy_narrow(mean_policy)
  
  #print(c(t_fun, r_fun, n_pol, mean_pol))
  
  focal_func <- .p_focal_cpp
  if(debug_use_r_implementation){
    #warn("You are using the R implementation instead of the c++ implementation, this will be far slower than it needs to be")
    focal_func <- .p_focal_r
  }
  
  #We flip the kernel here instead of making the c++ iterate over it backwards
  k <- matrix(rev(w), nrow(w), ncol(w))
  #print(k)
  t_fun_index <- 0;
  r_fun_index <- 0;
  
  if(pFocal_weight_fun_list()[t_fun] %in% c("MULTIPLY", "ADD", "R_EXP", "L_EXP")){
    t_fun_index <- t_fun
    
  }else if(pFocal_weight_fun_list()[t_fun] == "L_DIVIDE"){
    t_fun_index <- match("MULTIPLY", pFocal_weight_fun_list())[1]
    x <- 1/x
  }else if(pFocal_weight_fun_list()[t_fun] == "R_DIVIDE"){
    t_fun_index <- match("MULTIPLY", pFocal_weight_fun_list())[1]
    w <- 1/w
  }else if(pFocal_weight_fun_list()[t_fun] == "SUBTRACT"){
    t_fun_index <- match("ADD", pFocal_weight_fun_list())[1]
    w <- -w
  }else{
    stop("Error, bad weight_function")
  }
  
  if(pFocal_fun_list()[r_fun] %in% c("SUM", "PRODUCT", "MIN", "MAX", "MEAN", "VARIANCE")){
    r_fun_index <- r_fun
    return(focal_func(x, k, default, t_fun_index-1, r_fun_index-1, n_pol-1, mean_pol-1, mp))
    
  }else if(pFocal_fun_list()[r_fun] == "STANDARD_DEVIATION"){
    r_fun_index <- match("VARIANCE", pFocal_fun_list())[1]
    
    v = focal_func(x, k, default, t_fun_index-1, r_fun_index-1, n_pol-1, mean_pol-1, mp)
    v[is.na(v)]=0
    #print(v)
    sd = sqrt(v)
    sd[is.na(v)]=NA
    return(sd)
    
  }else if(pFocal_fun_list()[r_fun] == "RANGE"){
    r_fun_index_1 <- match("MAX", pFocal_fun_list())[1]
    r_fun_index_2 <- match("MIN", pFocal_fun_list())[1]
    
    return(
      focal_func(x, k, default, t_fun_index-1, r_fun_index_1-1, n_pol-1, mean_pol-1, mp) - 
      focal_func(x, k, default, t_fun_index-1, r_fun_index_2-1, n_pol-1, mean_pol-1, mp)
    )
  }else{
    stop("Error, bad function")
  }
}

#This implementation is to test against, and to use as a backup if for whatever reason the c++ implementation does not work
.p_focal_r <- function(d, k, default, t_fun, r_fun, na_pol, mean_pol, omp){
  tf_name <- pFocal_weight_fun_list()[t_fun+1]
  rf_name <- pFocal_fun_list()[r_fun+1]
  na_pol_name <- pFocal_na_policy_list()[na_pol+1]
  mean_pol_name <- pFocal_mean_policy_list()[mean_pol+1]
  
  if(!(tf_name %in% c("MULTIPLY", 
                      "ADD", 
                      "R_EXP", 
                      "L_EXP"))){
    stop("\nThe transform function, ie: 'weight_fun', is not a valid value.\nIt it must be in the range [0, 4)\n")
  }
  if(!(rf_name %in% c("SUM", 
                      "PRODUCT", 
                      "MIN", 
                      "MAX", 
                      "MEAN", 
                      "VARIANCE"))){
    stop("\nThe reduse function, ie: 'fun', is not a valid value.\nIt it must be in the range [0, 6)\n")
  }
  if(!(na_pol_name %in% c("NOTHING_SPECIAL",
                          "SKIP_NAN_IN_KERNEL",
                          "SKIP_NAN_IN_DATA",
                          "SKIP_NAN_IN_KERNEL_OR_DATA",
                          "SKIP_NAN_IN_INTERMEDIATE"))){
    stop("\nThe NA policy is not a valid value.\nIt it must be in the range [0, 5)\n")
  }
  if(!(mean_pol_name %in% c("KERNEL_SIZE",
                            "SUM_NON_NAN_KERNEL_VALUES",
                            "MUL_NON_NAN_KERNEL_VALUES",
                            "COUNT_NON_NAN_KERNEL_VALUES",
                            "COUNT_NON_NAN_INTERMEDIATE_VALUES"))){
    stop("\nThe mean policy is not a valid value.\nIt it must be in the range [0, 5)\n")
  }
  
  
  extra_top <- floor(nrow(k)/2L)
  extra_bottom <- floor(nrow(k)/2L)
  extra_left <- floor(ncol(k)/2L)
  extra_right <- floor(ncol(k)/2L)
  
  data <- matrix(default, extra_top+nrow(d)+extra_bottom, extra_left+ncol(d)+extra_right)
  data[(1+extra_top):(nrow(data)-extra_bottom), (1+extra_left):(ncol(data)-extra_right)] <- d
  
  output_default <- NA
  
  output <- matrix(0, nrow(d), ncol(d))
  
  for(col in 1:ncol(d)){
    for(row in 1:nrow(d)){
      sub_data <- data[(row):(row+extra_top+extra_bottom), (col):(col+extra_left+extra_right)]
      
      acc = if(rf_name %in% c("SUM", "MEAN", "VARIANCE")){
        0
      }else if(rf_name == "PRODUCT"){
        1
      }else if(rf_name == "MIN"){
        .Machine[["double.xmax"]]
      }else if(rf_name == "MAX"){
        .Machine[["double.xmin"]]
      }else{
        stop()
      }
      
      mean_divisor = if(mean_pol_name %in% c("SUM_NON_NAN_KERNEL_VALUES", "COUNT_NON_NAN_KERNEL_VALUES", "COUNT_NON_NAN_INTERMEDIATE_VALUES")){
        0
      }else if(mean_pol_name == "KERNEL_SIZE"){
        length(k)
      }else if(mean_pol_name == "MUL_NON_NAN_KERNEL_VALUES"){
        1
      }else{
        stop()
      }
      
      for(k_col in 1:ncol(k)){
          for(k_row in 1:nrow(k)){
          
          #calculate the value for this part of the kernel at this part of the data
          p <- if(tf_name == "MULTIPLY"){
            sub_data[k_row, k_col]*k[k_row, k_col]
          }else if(tf_name == "ADD"){
            sub_data[k_row, k_col]+k[k_row, k_col]
          }else if(tf_name == "R_EXP"){
            sub_data[k_row, k_col]^k[k_row, k_col]
          }else if(tf_name == "L_EXP"){
            k[k_row, k_col]^sub_data[k_row, k_col]
          }else{
            stop()
          }
          
          #update the mean_divisor value by policy
          if(mean_pol_name == "SUM_NON_NAN_KERNEL_VALUES"){
            if(!is.na(k[k_row, k_col])){
              mean_divisor = mean_divisor+k[k_row, k_col]
            }
          }
          
          if(mean_pol_name == "MUL_NON_NAN_KERNEL_VALUES"){
            if(!is.na(k[k_row, k_col])){
              mean_divisor = mean_divisor*k[k_row, k_col]
            }
          }
          
          if(mean_pol_name == "COUNT_NON_NAN_KERNEL_VALUES"){
            if(!is.na(k[k_row, k_col])){
              mean_divisor = mean_divisor+1
            }
          }
          
          #if we do not skip this result by policy, we update the accumulator
          if((!((na_pol_name %in% c("SKIP_NAN_IN_KERNEL", "SKIP_NAN_IN_KERNEL_OR_DATA")) && (is.na(k[k_row, k_col])))) && 
             (!((na_pol_name %in% c("SKIP_NAN_IN_DATA",   "SKIP_NAN_IN_KERNEL_OR_DATA")) && (is.na(sub_data[k_row, k_col])))) &&
             (!((na_pol_name ==     "SKIP_NAN_IN_INTERMEDIATE")                          && (is.na(p))))){
            
            if(mean_pol_name == "COUNT_NON_NAN_INTERMEDIATE_VALUES"){
              if(!is.na(p)){
                mean_divisor = mean_divisor+1
              }
            }
            
            acc <- if(rf_name %in% c("SUM", "MEAN", "VARIANCE")){
              acc+p
            }else if(rf_name == "PRODUCT"){
              acc*p
            }else if(rf_name == "MIN"){
              if(is.na(p)){
                acc
              }else{
                min(acc, p)
              }
            }else if(rf_name == "MAX"){
              if(is.na(p)){
                acc
              }else{
                max(acc, p)
              }
            }else{
              stop()
            }
            
          }
          
        }
      }
      
      if(is.na(acc) || rf_name %in% c("SUM", "PRODUCT", "MIN", "MAX")){
        output[row, col] <- acc
      }else if(rf_name == "MEAN"){
        output[row, col] <- acc/mean_divisor
      }else if(rf_name == "VARIANCE"){
        
        
        mean <- acc/mean_divisor
        acc2 = 0
        #ps <- c()
        
        for(k_col in 1:ncol(k)){
          for(k_row in 1:nrow(k)){
            #calculate the value for this part of the kernel at this part of the data
            p <- if(tf_name == "MULTIPLY"){
              sub_data[k_row, k_col]*k[k_row, k_col]
            }else if(tf_name == "ADD"){
              sub_data[k_row, k_col]+k[k_row, k_col]
            }else if(tf_name == "R_EXP"){
              sub_data[k_row, k_col]^k[k_row, k_col]
            }else if(tf_name == "L_EXP"){
              k[k_row, k_col]^sub_data[k_row, k_col]
            }else{
              stop()
            }
            #ps <- append(ps, c(k_col-1, k_row-1, p))
            #if we do not skip this result by policy, we update the accumulator
            if((!((na_pol_name %in% c("SKIP_NAN_IN_KERNEL", "SKIP_NAN_IN_KERNEL_OR_DATA")) && (is.na(k[k_row, k_col])))) && 
               (!((na_pol_name %in% c("SKIP_NAN_IN_DATA",   "SKIP_NAN_IN_KERNEL_OR_DATA")) && (is.na(sub_data[k_row, k_col])))) &&
               (!((na_pol_name ==     "SKIP_NAN_IN_INTERMEDIATE")                          && (is.na(p))))){
              
              acc2 <- acc2+((p-mean)*(p-mean))
              
            }
          }
        }
        
        #print(c(col-1, row-1, mean_divisor, acc, mean, ps, acc2, acc2/mean_divisor))
        output[row, col] <- acc2/mean_divisor
      }else{
        stop()
      }
    }
  }
  
  return(output)
}



pFocal.matrix <- function(x, w, fun = "SUM", weight_fun = "MULTIPLY", na_policy="NOTHING_SPECIAL", mean_policy="KERNEL_SIZE", na_flag=NA, mp=TRUE, debug_use_r_implementation=FALSE){

  fun <- .pFocal_fun_narrow(fun)
  weight_fun <- .pFocal_weight_fun_narrow(weight_fun)
  
  if(!is.na(na_flag)){
    x[is.na(x)] <- na_flag
  }
  
  output <- .pFocal(x, w, fun, weight_fun, na_policy, mean_policy, na_flag, mp, debug_use_r_implementation=debug_use_r_implementation)
  
  return(output)
}

pFocal.stars <- function(x, w, fun = "SUM", weight_fun = "MULTIPLY", na_policy="NOTHING_SPECIAL", mean_policy="KERNEL_SIZE", na_flag = NA, mp=TRUE, debug_use_r_implementation=FALSE){

  #some code from github.com/michaeldorman/starsExtra R/focal2.R:focal2
  if(requireNamespace("starsExtra", quietly = TRUE)){
    
    template <- x
    
    x = starsExtra::check_one_attribute(x)
    x = starsExtra::check_2d(x)
    
    input = layer_to_matrix(template, check = FALSE)
    
    output <- pFocal.matrix(x, w, fun, na_policy, mean_policy, weight_fun, na_policy, na_flag, mean_policy, mp, debug_use_r_implementation=debug_use_r_implementation)
    
    output_vector = result[[length(result)]]
    
    # Back to 'stars'
    
    output = t(output)
    template[[1]] = output
    
    # Return
    return(template)
    
  }else{
    stop("We require package 'starsExtra' to process stars data")
  }
}

pFocal <- function(x, w, fun = "SUM", weight_fun = "MULTIPLY", na_policy="NOTHING_SPECIAL", mean_policy="KERNEL_SIZE", na_flag = NA, mp=TRUE, debug_use_r_implementation=FALSE){
  if(is(w, "list")){
    return(pFocal_chain(x, w, fun, weight_fun, na_policy, mean_policy, na_flag, mp, debug_use_r_implementation=debug_use_r_implementation))
  }else if(is(x, "matrix")){
    return(pFocal.matrix(x, w, fun, weight_fun, na_policy, mean_policy, na_flag, mp, debug_use_r_implementation=debug_use_r_implementation))
  }else if(is(x, "stars")){
    return(pFocal.matrix(x, w, fun, weight_fun, na_policy, mean_policy, na_flag, mp, debug_use_r_implementation=debug_use_r_implementation))
  }else{
    stop('unsupported type, x must be a "matrix" or a "stars"')
  }
}


pFocal_chain <- function(x, w, fun = "SUM", weight_fun = "MULTIPLY", na_policy="NOTHING_SPECIAL", mean_policy="KERNEL_SIZE", na_flag = NA, mp=TRUE, debug_use_r_implementation=FALSE){
  output <- x
  for(w_part in w){
    output <- pFocal(output, w_part, fun, weight_fun, na_policy, mean_policy, na_flag, mp, debug_use_r_implementation=debug_use_r_implementation)
  }
  return(output)
}

.pFocal_compare <- function(x, w, fun = "SUM", weight_fun = "MULTIPLY", na_policy="NOTHING_SPECIAL", mean_policy="KERNEL_SIZE", na_flag = NA, mp=TRUE){
  cpp_run <- abs(pFocal(x, w, fun, weight_fun, na_policy, mean_policy, na_flag, mp, debug_use_r_implementation=FALSE))
  r_run <- abs(pFocal(x, w, fun, weight_fun, na_policy, mean_policy, na_flag, mp, debug_use_r_implementation=TRUE))
  
  dif <- abs(r_run-cpp_run)
  max_dif <- max(dif[!is.na(dif)], 0)  
  
  run_sum <- r_run+cpp_run
  
  run_sum[run_sum==0]<-1
  
  rel_dif <- abs((r_run-cpp_run)/(run_sum))
  max_rel_dif <- max(rel_dif[!is.na(rel_dif)], 0)
  #print(max_dif)
  return(c(sum(is.na(r_run)!=is.na(cpp_run)), max_dif, max_rel_dif))
  #if(bad_count){
  #  print("cpp")
  #  print(cpp_run)
  #  print("r")
  #  print(r_run)
  #}
  #return(bad_count)
  #return(c(
  #  max((r_run-cpp_run)/(r_run+cpp_run)), sum(is.na(r_run)!=is.na(cpp_run))
  #  ))
}

.pFocal_compare_sweep <- function(x, w, na_flag = NA, mp=TRUE){
  running_count <- x
  running_count[] <- 0;
  total <- length(pFocal_fun_list())*length(pFocal_weight_fun_list())*length(pFocal_na_policy_list())*length(pFocal_mean_policy_list())
  count <- 0;
  for(r in pFocal_fun_list()){
    for(t in pFocal_weight_fun_list()){
      for(n in pFocal_na_policy_list()){
        for(m in pFocal_mean_policy_list()){
          
          output <- .pFocal_compare(x, w, r, t, n, m, na_flag, mp)
          if(output[1] || output[2] > (1e-12)){
            print(paste(output[1], output[2], output[3], (count/total),r,t,n,m, sep = ', '))
          }
          count <- count+1
          #print(output)
          #if(output){
          #  print(output)
          #  return(output)
          #}
      
      #print(max(this_count))
      #running_count <- max(running_count, this_count)
        }
      }
    }
  }
  #print("Largest difference")
  #print(running_count)
  #return(running_count)
}


