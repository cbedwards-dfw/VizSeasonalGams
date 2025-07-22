#' Simulate fish count data
#'
#' @description
#' Simple function to generate reasonable-shaped creel sample data. Useful for testing / demonstrating plotting functions. This can (and typically should) be used without providing any arguments; several
#' aspects of the simulated data (e.g. # of years simulated, number of sampeles) can be modified with the optional arguments.
#'
#' @param n.per.year A single numeric value defining the number of samples per year. Defaults to 60.
#' @param years A numeric vector defining a "years" factor. Defaults to 2021:2023
#' @param doy.limits A numeric vector defining minimum and maximum day of year to simulate sampling.
#' Length 2. Optional.
#' @param middle.doy A numeric defining the day of year of the general peak of activity. Defaults to 250.
#' @param samples_per_day_limits A numeric vector of length 2. Optional.
#'
#' @returns
#' A data frame with columns for day of year, year, mark stage, year factor,
#' and simulated fish counts.
#'
#' @export
simulate_data = function(n.per.year = 60,
                         years = 2021:2023,
                         doy.limits = c(200, 300),
                         middle.doy = 250,
                         samples_per_day_limits = c(4,10)){

  n.per.year = 60
  years = 2021:2023
  doy = sample(doy.limits[1]:doy.limits[2], size = n.per.year*length(years), replace = TRUE)
  samples_per_day = sample(4:10, size = length(doy), replace = TRUE)

  year.vec = rep(years, times = n.per.year)

  data = tidyr::expand_grid(data.frame(doy = rep(doy, times = samples_per_day),
                                year = rep(year.vec, times = samples_per_day)),
                     mark_stage = as.factor(c("Jack", "AD Adult", "UM Adult"))) |>
    dplyr::mutate(yearfac = as.factor(.data$year))

  ## Generating effects of year
  coefs_yearfac_real = data.frame(yearfac = unique(data$yearfac))
  coefs_yearfac_real$doy_mod_yearfac = stats::rnorm(3, sd = 10)
  coefs_yearfac_real$amp_mod_yearfac = stats::runif(3, min = 0.8, max = 1.2)

  ## effects effects of mark_stage
  coefs_mark_stage_real = data.frame(mark_stage = unique(data$mark_stage))
  coefs_mark_stage_real$doy_mod_mark_stage = stats::rnorm(3, sd = 5)
  coefs_mark_stage_real$amp_mod_mark_stage = stats::runif(3, min = 0.6, max = 1.4)
  ## make UM and AD smaller
  coefs_mark_stage_real$amp_mod_mark_stage[coefs_mark_stage_real$mark_stage == "UM Adult"] =
    coefs_mark_stage_real$amp_mod_mark_stage[coefs_mark_stage_real$mark_stage == "UM Adult"] *.5

  coefs_mark_stage_real$amp_mod_mark_stage[coefs_mark_stage_real$mark_stage == "Jack"] =
    coefs_mark_stage_real$amp_mod_mark_stage[coefs_mark_stage_real$mark_stage == "Jack"] *.3




  ## generate data itself
  data.full = data |>
    dplyr::left_join(coefs_mark_stage_real, by = "mark_stage") |>
    dplyr::left_join(coefs_yearfac_real, by = "yearfac") |>
    dplyr::mutate(amplitude_eff = .data$amp_mod_mark_stage * .data$amp_mod_yearfac,
           doy_eff = .data$doy_mod_mark_stage + .data$doy_mod_yearfac) |>
    dplyr::mutate(expectation = stats::dnorm(doy,
                               mean = middle.doy + .data$doy_eff,
                               sd = 15) * .data$amplitude_eff*200) |>
    dplyr::mutate(fish_count = stats::rpois(length(.data$expectation), lambda = .data$expectation))
  return(data.full |>
           dplyr::select(tidyselect::any_of(c("doy", "year", "mark_stage", "yearfac", "fish_count"))))
}


#' Simulate mgcv gam model for demonstration purposes
#'
#' Uses `simulate_data()`, and fits a negative binomial gam model for response `fish_count`  using
#' predictor variables `mark_stage` ("Jack", "AD Adult", "UM Adult"), `yearfac` (factor version of year), and a smooth across `doy` (numeric for day of year). Output is suitable model for
#' `plot_seasonal_gam_panels()`.
#'
#' @return fitted mgcv model
#' @export
#' @examples
#' \dontrun{
#' model = simulate_gam()
#' model_panels = plot_seasonal_gam_panels(model)
#' wrap_panels(model_panels)
#' }
simulate_gam = function(){
  data_sim = simulate_data()
  res = mgcv::gam(fish_count ~ mark_stage +
              s(doy, by = mark_stage, k = 20) +
              yearfac,
            method = "REML",
            family = "nb",
            data = data_sim)
}
