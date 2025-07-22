validate_model <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
 if(!("gam" %in% class(x))){
   cli::cli_abort("{.arg {arg}} must be an mgcv model.", ..., call = call)
 }
}

validate_flag <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if(!(is.logical(x) & length(x) == 1)){
    cli::cli_abort("{.arg {arg}} must be `TRUE` or `FALSE`.", ..., call = call)
  }
}

validate_char <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if(!(is.logical(x) & length(x) == 1)){
    cli::cli_abort("{.arg {arg}} must be a character of length 1.", ..., call = call)
  }
}

validate_number <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if(!(is.numeric(x) & length(x) == 1)){
    cli::cli_abort("{.arg {arg}} must be a numeric of length 1.", ..., call = call)
  }
}
