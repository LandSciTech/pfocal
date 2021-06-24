
.p_focal_r <- function(data, kernel, edge_value, transform_fun, reduce_fun, nan_policy, mean_divisor, variance, open_mp){
  #if(open_mp){
  #  warning("You are using the R implementation, open_mp is not implemented here")
  #}
  #print(c(transform_fun, reduce_fun, nan_policy, mean_divisor, +variance))
  
  
  tf <- c(
    function(d, k){d*k}, #MUL
    function(d, k){d+k}, #ADD
    function(d, k){d^k}, #R_EXP
    function(d, k){k^d}  #L_EXP
  )[[transform_fun+1]]
  
  r_init <- c(
    0,                         #SUM
    0,                         #ABS_SUM
    1,                         #PRODUCT
    1,                         #ABS_PRODUCT
    .Machine[["double.xmax"]], #MIN
    .Machine[["double.xmin"]]  #MAX
  )[[reduce_fun+1]]
  
  rf <- c(
    function(acc, v){acc+v},       #SUM
    function(acc, v){acc+abs(v)},  #ABS_SUM
    function(acc, v){acc*v},       #PRODUCT
    function(acc, v){acc*abs(v)},  #ABS_PRODUCT
    function(acc, v){min(acc, v)}, #MIN
    function(acc, v){max(acc, v)}  #MAX
  )[[reduce_fun+1]]
  
  m_init <- c(
    #static mean divider values
    function(...){1},                    #ONE
    function(k){length(c(k))},        #KERNEL_SIZE
    function(k){sum(+!is.na(k))},        #KERNEL_COUNT
    function(k){sum(k[!is.na(k)])},      #KERNEL_SUM
    function(k){sum(abs(k[!is.na(k)]))}, #KERNEL_ABS_SUM
    function(k){prod(k[!is.na(k)])},     #KERNEL_PROD
    function(k){prod(abs(k[!is.na(k)]))},#KERNEL_ABS_PROD
    #Dynamic mean divider initial values
    function(...){0}, #DYNAMIC_COUNT
    function(...){0}, #DYNAMIC_SUM
    function(...){0}, #DYNAMIC_ABS_SUM
    function(...){1}, #DYNAMIC_PROD
    function(...){1}, #DYNAMIC_ABS_PROD
    function(...){0}, #DYNAMIC_DATA_SUM
    function(...){0}, #DYNAMIC_DATA_ABS_SUM
    function(...){1}, #DYNAMIC_DATA_PROD
    function(...){1}  #DYNAMIC_DATA_ABS_PROD
  )[[mean_divisor+1]](k)
  
  mf <- c(
    #Static mean dividers do not change per value
    function(acc, ...){acc}, #ONE
    function(acc, ...){acc}, #KERNEL_SIZE
    function(acc, ...){acc}, #KERNEL_COUNT
    function(acc, ...){acc}, #KERNEL_SUM
    function(acc, ...){acc}, #KERNEL_ABS_SUM
    function(acc, ...){acc}, #KERNEL_PROD
    function(acc, ...){acc}, #KERNEL_ABS_PROD
    #Dynamic means initial values
    function(acc, v, ...){acc + !(is.na(v))},                 #DYNAMIC_COUNT
    function(acc, v, ...){if(is.na(v)){acc}else{acc+v}},      #DYNAMIC_SUM
    function(acc, v, ...){if(is.na(v)){acc}else{acc+abs(v)}}, #DYNAMIC_ABS_SUM
    function(acc, v, ...){if(is.na(v)){acc}else{acc*v}},      #DYNAMIC_PROD
    function(acc, v, ...){if(is.na(v)){acc}else{acc*abs(v)}}, #DYNAMIC_ABS_PROD
    function(acc, d, ...){if(is.na(d)){acc}else{acc+d}},      #DYNAMIC_DATA_SUM
    function(acc, d, ...){if(is.na(d)){acc}else{acc+abs(d)}}, #DYNAMIC_DATA_ABS_SUM
    function(acc, d, ...){if(is.na(d)){acc}else{acc*d}},      #DYNAMIC_DATA_PROD
    function(acc, d, ...){if(is.na(d)){acc}else{acc*abs(d)}}  #DYNAMIC_DATA_ABS_PROD
  )[[mean_divisor+1]]
  
  na_rm_false <- nan_policy == 1
  na_rm_true <- nan_policy == 2
  
  extra_top <- floor(nrow(kernel)/2L)
  extra_bottom <- floor(nrow(kernel)/2L)
  extra_left <- floor(ncol(kernel)/2L)
  extra_right <- floor(ncol(kernel)/2L)
  
  d <- matrix(edge_value, extra_top+nrow(data)+extra_bottom, extra_left+ncol(data)+extra_right)
  d[(1+extra_top):(nrow(d)-extra_bottom), (1+extra_left):(ncol(d)-extra_right)] <- data
  
  output <- matrix(0, nrow(data), ncol(data))
  
  for(col in 1:ncol(data)){
    for(row in 1:nrow(data)){
      
      sub_data <- d[(row):(row+extra_top+extra_bottom), (col):(col+extra_left+extra_right)]
      
      acc <- r_init
      mean_div <- m_init
      
      for(k_col in 1:ncol(kernel)){
        for(k_row in 1:nrow(kernel)){
          
          l_k <- kernel[k_row, k_col]
          l_d <- sub_data[k_row, k_col]
          l_v <- tf(d = l_d, k = l_k)
          
          
          if(na_rm_false && (is.na(acc) || is.na(l_k) || is.na(l_d) || is.na(l_v)) ){
            acc <- NA
          }else if(na_rm_true && (is.na(l_k) || is.na(l_d) || is.na(l_v))){
            #we skip it
          }else{
            acc <- rf(acc=acc, v=l_v)
            mean_div = mf(acc=mean_div, v=l_v, d=l_d)
          }
        }
      }
      
      if(variance){
        mean <- acc/mean_div
        acc <- r_init
        
        for(k_col in 1:ncol(kernel)){
          if(na_rm_false && is.na(acc)){
            break
          }
          
          for(k_row in 1:nrow(kernel)){
            l_k <- kernel[k_row, k_col]
            l_d <- sub_data[k_row, k_col]
            
            if(na_rm_true && (is.na(l_k) || is.na(l_d))){
              next
            }
            
            t_v <- tf(d = l_d, k = l_k)
            
            l_v <- (t_v-mean)*(t_v-mean)
            
            if(na_rm_true && is.na(l_v)){
              next
            }
            
            if(na_rm_false && is.na(l_v)){
              acc <- NA
              break
            }
            
            acc <- rf(acc=acc, v=l_v)
          }
        }
      }
      output[row, col] <- acc/mean_div
    }
  }
  
  return(output)
}





