#some code from github.com/michaeldorman/starsExtra

.pFocal_fun_list <- NULL

pFocal_fun_list <- function(){
  if(is.null(.pFocal_fun_list)){
    .pFocal_fun_list <- .get_fun_list_cpp()
  }
  return(.pFocal_fun_list)
}

.pFocal_weight_fun_list <- NULL

pFocal_weight_fun_list <- function(){
  if(is.null(.pFocal_weight_fun_list)){
    .pFocal_weight_fun_list <- .get_weight_fun_list_cpp()
  }
  return(.pFocal_weight_fun_list)
}


.pFocal_raw <- function(x, w, fun, weight_fun){
  
  return(x+1)
}

.pFocal_checks <- function(w, fun, weight_fun, na.rm, mask, na_flag){
  errors <- c()
  warns <- c()
  
  if(!(length(na_flag)==1)){
    errors <- append(errors, "'na_flag' must have a length of 1")
  }
  if(!(is.numeric(na_flag) | is.na(na_flag))){
    errors <- append(errors, "'na_flag' must be NA or numeric")
  }
  
  if(length(mask) == 0){
    errors <- append(errors, "'mask' must not be empty")
  }else if(!is.logical(mask[1])){
    if(is.logical(!!mask[1])){
      warns <- append(warns, "'mask' is not logical, but is being treated as logical")
    }else{
      errors <- append(errors, "'mask' must be logical")
    }
  }else if(length(mask) > 1){
    warns <- append(warns, "'mask' has a length >1, only the first element will be used")
  }
  
  if(length(na.rm) == 0){
    errors <- append(errors, "'na.rm' must not be empty")
  }else if(!is.logical(na.rm[1])){
    if(is.logical(!!na.rm[1])){
      warns <- append(warns, "'na.rm' is not logical, but is being treated as logical")
    }else{
      errors <- append(errors, "'na.rm' must be logical")
    }
  }else if(length(na.rm) > 1){
    warns <- append(warns, "'na.rm' has a length >1, only the first element will be used")
  }
  
  if(!(fun %in% pFocal_fun_list())){
    errors <- append(errors, paste("function 'fun' not recognised. Please pick from c(\"", paste(pFocal_fun_list(), collapse = '", "'),"\")", sep=''))
  }
  if(!(weight_fun %in% pFocal_weight_fun_list())){
    errors <- append(errors, paste("function 'weight_fun' not recognised. Please pick from c(\"", paste(pFocal_weight_fun_list(), collapse = '", "'),"\")", sep=''))
  }
  
  if(!is(w, "array")){
    errors <- append(errors, "The weights 'w' must be a matrix or an array")
  }else if(any(!is.numeric(w))){
    errors <- append(errors, "The weights 'w' must not include non-numeric values")
  }else{
    w_dim = dim(w)
    
    if(!(length(w_dim) %in% c(2,3))){
      errors <- append(errors, "The weights 'w' must be 2d or 3d")
    }else{
      if(w_dim[1] %% 2 == 0){
        errors <- append(errors, "The weights 'w' has an even number of rows, it must be odd to have a center element")
      }
      if(w_dim[2] %% 2 == 0){
        errors <- append(errors, "The weights 'w' has an even number of columns, it must be odd to have a center element")
      }
      if(length(w_dim) == 3 && w_dim[3] <= 0){
        errors <- append(errors, "There are 0 layers in this set of 'weights', there must be at least 1 if using a 3d set of weights")
      }
    }
  }
  if(length(warns) >0){
    warning(paste(warns, collapse="\n  "))
  }
  
  if(length(errors) >0){
    stop(paste(errors, collapse="\n  "))
  }
}

pFocal.matrix <- function(x, w, fun = "sum", weight_fun = "*", na.rm = FALSE, mask = FALSE, na_flag = NA){
  .pFocal_checks(w, fun, weight_fun, na.rm, mask, na_flag)
  output <- .pFocal_raw(x, w, fun, weight_fun)
  return(matrix(output, nrow(x), ncol(x)))
}

pFocal.stars <- function(x, w, fun = "sum", weight_fun = "*", na.rm = FALSE, mask = FALSE, na_flag = NA){
  if(requireNamespace("starsExtra", quietly = TRUE)){
    type <- "stars"
    x <- starsExtra::check_one_attribute(x)
    x <- starsExtra::check_2d(x)
    m <- starsExtra::starslayer_to_matrix(x, check = FALSE)
    
    #new_x = pFocal(m, w, fun, weight_fun, na.rm = FALSE, mask = FALSE, na_flag = NA)
    stop("Sorry, implementation to come")
  }else{
    stop("We require package 'starsExtra' to process stars data")
  }
}







pFocal <- function(x, w, fun = "mean", weight_fun = "*", na.rm = FALSE, mask = FALSE, na_flag = NA) {
  
  
  w_dim <- dim(w)
  
  w_rows <- w_dim[1]
  w_cols <- w_dim[2]
  w_lays <- if(length(w_dim) == 3) w_dim[3] else 1
  
  if(w_rows %% 2 == 0){stop("The weights 'w' has an even number of rows, it must be odd to have a center element")}
  if(w_cols %% 2 == 0){stop("The weights 'w' has an even number of columns, it must be odd to have a center element")}
  
  m <- NULL
  type <- "";
  if(is(x, "matrix")){
    # just a matrix in
    type <- "simple"
    m <- x
    
  }else if(is(x, "stars")){
    # handle stars data
    if(requireNamespace("starsExtra", quietly = TRUE)){
      type <- "stars"
      x <- starsExtra::check_one_attribute(x)
      x <- starsExtra::check_2d(x)
      m <- starsExtra::starslayer_to_matrix(x, check = FALSE)
    }else{
      stop("We require package 'starsExtra' to process stars data")
    }
  }else{
    stop("We do not know how to handle that type of data")
  }
  
  d_rows <- nrow(m)
  d_cols <- ncol(m)
  
  #extra_cols = 
  
  
  
  
  
  if(mask) output_vector[is.na(input_vector)] = NA
  
  
  
}