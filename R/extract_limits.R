## if panels are patchwork, we want limits of the top panel (the non-histogram part)
extract_xlim_patchwork <- function(x){
  temp = as.data.frame(t(ggplot2::layer_scales(x[[1]])$x$range$range))
  names(temp) = c("xlim_low", "xlim_high")
  return(temp)
}
extract_ylim_patchwork <- function(x){
  temp = as.data.frame(t(ggplot2::layer_scales(x[[1]])$y$range$range))
  names(temp) = c("ylim_low", "ylim_high")
  return(temp)
}

extract_ylim_coverage_patchwork <- function(x){
  temp = as.data.frame(t(ggplot2::layer_scales(x[[2]])$y$range$range))
  names(temp) = c("ylim_coverage_low", "ylim_coverage_high")
  return(temp)
}

## if panels are not patchwork, they're just a ggplot object
extract_xlim_ggplot <- function(x){
  temp = as.data.frame(t(ggplot2::layer_scales(x)$x$range$range))
  names(temp) = c("xlim_low", "xlim_high")
  return(temp)
}
extract_ylim_ggplot <- function(x){
  temp = as.data.frame(t(ggplot2::layer_scales(x)$y$range$range))
  names(temp) = c("ylim_low", "ylim_high")
  return(temp)
}
