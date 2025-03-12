# -----------------------------------------------------------------------------------------------------------------
#' Generating a summary distribution for the dive data
#' This will allow rapid calculations of the energy costs for a given dive budget



# data from Lila
dive_dat <- readRDS("movement_devel/data/DiveLengthsClean.rds")

# contains some anomolies
dive_dat <- dive_dat %>%
  filter(DiveLength > 0)

# fit a lognormal distribution to the dive data - tried others, this looks OK
par_fit <- fitdistrplus::fitdist(as.numeric(dive_dat$DiveLength), "lnorm")

# qualitatively check fit
x <- seq(0, 300, length = 200)
y <- dlnorm(x, meanlog = par_fit$estimate[1], sdlog = par_fit$estimate[2])

hist(as.numeric(seconds(dive_dat$DiveLength)), freq = F)
lines(x, y, col = "blue")

quantile(as.numeric(dive_dat$DiveLength), probs = 0.995)

sample_dive <- function(n_samp, lmean = 3.845265, lsd = 0.8448445, t_max = 240){

  rlnorm(n_samp, meanlog = lmean, sdlog = lsd) %>%
    ifelse(.>t_max, t_max, .)

}

