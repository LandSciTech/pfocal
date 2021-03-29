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

pFocal_fun_list <- function(){
  return (c(
    c("SUM", "PRODUCT", "MIN", "MAX", "MEAN", "VARIANCE"), #in C++, do not change the order
    c("STANDARD_DEVIATION", "RANGE")                       #not in C++
  ))
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

#This function does no checking, don't call this one directly
.pFocal <- function(x, w, fun, weight_fun, default, mp=TRUE, debug_use_r_implementation=FALSE){
  
  t_fun <- .pFocal_weight_fun_narrow(weight_fun)
  r_fun <- .pFocal_fun_narrow(fun)
  
  #print(c(t_fun, r_fun))
  
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
    return(focal_func(x, k, default, t_fun_index-1, r_fun_index-1, mp))
    
  }else if(pFocal_fun_list()[r_fun] == "STANDARD_DEVIATION"){
    r_fun_index <- match("VARIANCE", pFocal_fun_list())[1]
    
    v = focal_func(x, k, default, t_fun_index-1, r_fun_index-1, mp)
    v[is.na(v)]=0
    #print(v)
    sd = sqrt(v)
    sd[is.na(v)]=NA
    return(sd)
    
  }else if(pFocal_fun_list()[r_fun] == "RANGE"){
    r_fun_index_1 <- match("MAX", pFocal_fun_list())[1]
    r_fun_index_2 <- match("MIN", pFocal_fun_list())[1]
    
    return(
      focal_func(x, k, default, t_fun_index-1, r_fun_index_1-1, mp) - 
      focal_func(x, k, default, t_fun_index-1, r_fun_index_2-1, mp)
    )
  }else{
    stop("Error, bad function")
  }
}

#This implementation is to test against, and to use as a backup if for whatever reason the c++ implementation does not work
.p_focal_r <- function(d, k, default, t_fun, r_fun, omp){
  tf_name <- pFocal_weight_fun_list()[t_fun+1]
  rf_name <- pFocal_fun_list()[r_fun+1]
  
  if(!(tf_name %in% c("MULTIPLY", "ADD", "R_EXP", "L_EXP"))){
    stop("\nThe transform function, ie: 'weight_fun', is not a valid value.\nIt it must be in the range [0, 4)\n")
  }
  if(!(rf_name %in% c("SUM", "PRODUCT", "MIN", "MAX", "MEAN", "VARIANCE"))){
    stop("\nThe reduse function, ie: 'fun', is not a valid value.\nIt it must be in the range [0, 6)\n")
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
      acc <- NULL
      
      if(t_fun == 0){
        acc <- sub_data*k
      }else if(t_fun == 1){
        acc <- sub_data+k
      }else if(t_fun == 2){
        acc <- sub_data^k
      }else if(t_fun == 3){
        acc <- k^sub_data
      }
      
      if(r_fun == 0){
        output[row, col] <- sum(acc)
      }else if(r_fun == 1){
        output[row, col] <- prod(acc)
      }else if(r_fun == 2){
        output[row, col] <- min(acc)
      }else if(r_fun == 3){
        output[row, col] <- max(acc)
      }else if(r_fun == 4){
        output[row, col] <- sum(acc)/length(k)
      }else if(r_fun == 5){
        m <- sum(acc)/length(k)
        output[row, col] <- sum((acc-m)*(acc-m))/length(k)
      }
    }
  }
  
  return(output)
}



pFocal.matrix <- function(x, w, fun = "SUM", weight_fun = "MULTIPLY", mask = FALSE, na_flag=NA, mp=TRUE, debug_use_r_implementation=FALSE){

  fun <- .pFocal_fun_narrow(fun)
  weight_fun <- .pFocal_weight_fun_narrow(weight_fun)
  
  #.pFocal_checks(w, fun, weight_fun, mask, na_flag)
  
  if(!is.na(na_flag)){
    x[is.na(x)] <- na_flag
  }
  
  output <- .pFocal(x, w, fun, weight_fun, na_flag, mp, debug_use_r_implementation=debug_use_r_implementation)
  
  if(mask){
    output[is.na(x)] <- NA
  }
  
  return(output)
}

pFocal.stars <- function(x, w, fun = "SUM", weight_fun = "MULTIPLY", mask = FALSE, na_flag = NA, mp=TRUE, debug_use_r_implementation=FALSE){

  #some code from github.com/michaeldorman/starsExtra R/focal2.R:focal2
  if(requireNamespace("starsExtra", quietly = TRUE)){
    
    template <- x
    
    x = starsExtra::check_one_attribute(x)
    x = starsExtra::check_2d(x)
    
    input = layer_to_matrix(template, check = FALSE)
    
    output <- pFocal.matrix(x, w, fun, weight_fun, mask, na_flag, mp, debug_use_r_implementation=debug_use_r_implementation)
    
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

pFocal <- function(x, w, fun = "SUM", weight_fun = "MULTIPLY", mask = FALSE, na_flag = NA, mp=TRUE, debug_use_r_implementation=FALSE){
  if(is(w, "list")){
    return(pFocal_chain(x, w, fun, weight_fun, mask, na_flag, mp, debug_use_r_implementation=debug_use_r_implementation))
  }else if(is(x, "matrix")){
    return(pFocal.matrix(x, w, fun, weight_fun, mask, na_flag, mp, debug_use_r_implementation=debug_use_r_implementation))
  }else if(is(x, "stars")){
    return(pFocal.matrix(x, w, fun, weight_fun, mask, na_flag, mp, debug_use_r_implementation=debug_use_r_implementation))
  }else{
    stop('unsupported type, x must be a "matrix" or a "stars"')
  }
}


pFocal_chain <- function(x, w, fun = "SUM", weight_fun = "MULTIPLY", mask = FALSE, na_flag = NA, mp=TRUE, debug_use_r_implementation=FALSE){
  output <- x
  for(w_part in w){
    output <- pFocal(output, w_part, fun, weight_fun, mask, na_flag, mp, ..., debug_use_r_implementation=debug_use_r_implementation)
  }
  return(output)
}

.pFocal_compare <- function(x, w, fun = "SUM", weight_fun = "MULTIPLY", mask = FALSE, na_flag = NA, mp=TRUE){
  cpp_run <- abs(pFocal(x, w, fun, weight_fun, mask, na_flag, mp, debug_use_r_implementation=FALSE))
  r_run <- abs(pFocal(x, w, fun, weight_fun, mask, na_flag, mp, debug_use_r_implementation=TRUE))
  
  return((r_run-cpp_run)/(r_run+cpp_run))
}

.pFocal_compare_sweep <- function(x, w, mask = FALSE, na_flag = NA, mp=TRUE){
  running_count <- x
  running_count[] <- 0;
  for(t in pFocal_weight_fun_list()){
    for(r in pFocal_fun_list()){
      print(paste(t,r, sep = ', '))
      this_count <- .pFocal_compare(x, w, r, t, mask, na_flag, mp)
      print(min(c(abs(this_count[!is.na(this_count)]), c())))
      
      #print(max(this_count))
      #running_count <- max(running_count, this_count)
    }
  }
  #print("Largest difference")
  #print(running_count)
  #return(running_count)
}


