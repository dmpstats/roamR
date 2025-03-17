# -----------------------------------------------------------------------------------------------------------------
#' Generating a summary distribution for the dive data
#' This will allow rapid calculations of the energy costs for a given dive budget



# data from Lila
dive_dat <- readRDS("movement_devel/data/DiveLengthsClean.rds")

# contains some anomolies
dive_dat <- dive_dat %>%
  filter(DiveLength > 0)

# equations operate on mins
units(dive_dat$DiveLength) <- "mins"

# drop extremes for fitting dist
upper_trunc <- quantile(as.numeric(dive_dat$DiveLength), probs = 0.99)

dive_dat <- dive_dat %>%
  filter(DiveLength <= upper_trunc)


# fit a lognormal distribution to the dive data - tried others, this looks OK
par_fit <- fitdistrplus::fitdist(as.numeric(dive_dat$DiveLength), "lnorm")

# qualitatively check fit
x <- seq(0, 5, length = 200)
y <- dlnorm(x, meanlog = par_fit$estimate[1], sdlog = par_fit$estimate[2])

hist(as.numeric(seconds(dive_dat$DiveLength)), freq = F)
lines(x, y, col = "blue")



sample_dive <- function(n_samp, lmean = -0.2631779, lsd = 0.8344929, t_max = 3.4, t_min = 0.2){

  rlnorm(n_samp, meanlog = lmean, sdlog = lsd) %>%
    ifelse(. > t_max, t_max, .) %>%
    ifelse(. < t_min, t_min, .)

}

qqplot(sample_dive(200), sample(dive_dat$DiveLength, 200))
