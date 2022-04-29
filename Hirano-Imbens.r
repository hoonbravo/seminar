install.packages("causaldrf")
library(causaldrf)
data("nmes_data")
dim(nmes_data)
nm = nmes_data

plot(nm$packyears, nm$TOTALEXP)

set.seed(301)
hi_sample <- function(N){
  X1 <- rexp(N)
  X2 <- rexp(N)
  T <- rexp(N, X1 + X2)
  gps <- (X1 + X2) * exp(-(X1 + X2) * T)
  Y <- T + gps + rnorm(N)
  hi_data <- data.frame(cbind(X1, X2, T, gps, Y))
  return(hi_data)
}
hi_sim_data <- hi_sample(1000)
head(hi_sim_data)
sim = hi_sim_data

hi_estimate <- hi_est(Y = Y,
                      treat = T,
                      treat_formula = T ~ X1 + X2,
                      outcome_formula = Y ~ T + I(T^2) + gps + I(gps^2) + T * gps,
                      data = sim,
                      grid_val = quantile(hi_sim_data$T, probs = seq(0, .95, by = 0.01)),
                      treat_mod = "Gamma",
                      link_function = "inverse")

summary(hi_estimate)


pf_estimate <- reg_est(Y = TOTALEXP,
                       treat = packyears,
                       covar_formula = ~ 1,
                       data = full_data_orig,
                       degree = 2,
                       wt = full_data_orig$HSQACCWT,
                       method = "same")