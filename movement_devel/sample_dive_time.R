sample_dive <- function(n_samp, lmean = -0.2631779, lsd = 0.8344929, t_max = 3.4, t_min = 0.2){

  rlnorm(n_samp, meanlog = lmean, sdlog = lsd) %>%
    ifelse(. > t_max, t_max, .) %>%
    ifelse(. < t_min, t_min, .) %>%
    units::set_units(., min)

}
