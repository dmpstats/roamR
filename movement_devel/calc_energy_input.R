#' Calculation of an approximate energy per unit time foraging

activity_data <- read_csv("vignettes/articles/data/ActivityBudget.csv")

mean_dive <- activity_data %>%
  filter(Behaviour == "Dive") %>%
  summarise(mean = mean(Hours))

mean_dive

mean_day_gain <- 1.97 #g/day

mass_conversion <- 0.072 #g/kJ

energy_unit_feed <- mean_day_gain/mass_conversion/mean_dive$mean

energy_profile <- numeric(1000)

for(i in 1:1000){test <- calc_day_cost(in_agent = simBird, in_species = guill, in_ibm = guill_ibm); energy_profile[i] <- sum(test$day_cost)}

summary(energy_profile)

mean_energy_out <- mean(energy_profile)

mean_energy_in <- (mean_energy_out + mean_day_gain/mass_conversion)/mean_dive$mean
