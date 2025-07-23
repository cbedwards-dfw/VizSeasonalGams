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
  if(!(is.character(x) & length(x) == 1)){
    cli::cli_abort("{.arg {arg}} must be a character of length 1.", ..., call = call)
  }
}

validate_optional_char <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if(!is.null(x)){
    if(!(is.logical(x) & length(x) == 1)){
      cli::cli_abort("{.arg {arg}} must be `NULL` or a character of length 1.", ..., call = call)
    }
  }
}

#' Test argument to see if it's a numeric within one more constraints.
#'
#' Can identify lower and/or upper limits, either inclusive (`limits_inclusive`) or exclusive
#' (`limits_exclusive`).
#'
#' @param x item to test
#' @param limits_inclusive Defines inclusive constraints. If provided, numeric vector of length 2, with first value being the lower limit and second value being the upper limit. Use NA to identify a limit that should be ignored. E.g., if a value should be no less than 0 but has no upper limit, use `limits_inclusive = c(0, NA)`.
#' @param limits_exclusive As limits_inclusive, but for exclusive limits (e.g., a value must be greater than 0).
#' @param ... additional arguments
#' @param arg For identifying argument from the calling function
#' @param call For identifying the calling function
#'
#' @return None.
#'
validate_number <- function(x,
                            limits_inclusive = NULL,
                            limits_exclusive = NULL,
                            ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if(!(is.numeric(x) & length(x) == 1)){
    cli::cli_abort("{.arg {arg}} must be a numeric of length 1.", ..., call = call)
  }
  subvalidate_range(x,
                    limits_inclusive = limits_inclusive,
                    limits_exclusive = limits_exclusive,
                    arg = arg,
                    call = call)
}

validate_integer <- function(x,
                            limits_inclusive = NULL,
                            limits_exclusive = NULL,
                            ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if(!(is.numeric(x) & length(x) == 1)){
    cli::cli_abort("{.arg {arg}} must be an integer of length 1.", ..., call = call)
  }
  if(!(x%%1 == 0)){
    cli::cli_abort("{.arg {arg}} must be an integer of length 1.", ..., call = call)
  }
  subvalidate_range(x,
                    limits_inclusive = limits_inclusive,
                    limits_exclusive = limits_exclusive,
                    arg = arg,
                    call = call)
}



## validates range, useful for numbers or integers
subvalidate_range = function(x,
                             limits_inclusive = NULL,
                             limits_exclusive = NULL,
                             arg = arg,
                             call = call){

  if(!is.null(limits_inclusive)){

    if(all(!is.na(limits_inclusive))){
      if(!(x >= limits_inclusive[1] & x<= limits_inclusive[2])){
        cli::cli_abort("{.arg {arg}} must be no less than {limits_inclusive[1]} and no greater than {limits_inclusive[2]}.", call = call)
      }
    }

    if(!is.na(limits_inclusive[1])){
      if(!(x >= limits_inclusive[1])){
        cli::cli_abort("{.arg {arg}} must be no less than {limits_inclusive[1]}", call = call)
      }
    }

    if(!is.na(limits_inclusive[2])){
      if(!(x <= limits_inclusive[2])){
        cli::cli_abort("{.arg {arg}} must be no greater than {limits_inclusive[2]}", call = call)
      }
    }

  }

  if(!is.null(limits_exclusive)){

    if(all(!is.na(limits_exclusive))){
      if(!(x >= limits_exclusive[1] & x<= limits_exclusive[2])){
        cli::cli_abort("{.arg {arg}} must be greater than {limits_exclusive[1]} and less than {limits_exclusive[2]}.", call = call)
      }
    }

    if(!is.na(limits_exclusive[1])){
      if(!(x >= limits_exclusive[1])){
        cli::cli_abort("{.arg {arg}} must be greater than {limits_exclusive[1]}", call = call)
      }
    }

    if(!is.na(limits_exclusive[2])){
      if(!(x <= limits_exclusive[2])){
        cli::cli_abort("{.arg {arg}} must be less than {limits_exclusive[2]}", call = call)
      }
    }

  }

}

validate_scale <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  x = x[1]
  vals_acceptable = c("free", "fixed_column", "fixed_row", "fixed_all", "fixed_combination")
  if(! x %in% vals_acceptable){
    cli::cli_abort("{.arg {arg}} must be one of the following: {glue_char_arg(vals_acceptable)}.", ..., call = call)
  }
  return(x)
}

validate_variable <- function(x, varnames, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if(! is.character(x)){
    cli::cli_abort("{.arg {arg}} must be one of the modeled variables: {glue_char_arg(varnames)}.",
                   ..., call = call)
  }
  if(! x %in% varnames){
    cli::cli_abort("{.arg {arg}} must be one of the modeled variables: {glue_char_arg(varnames)}.",
                   ..., call = call)
  }
}

glue_char_arg <- function(x){
  glue::glue_collapse(glue::double_quote(x), sep = ', ', last = ', or ')
}
