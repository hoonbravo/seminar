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

add_spl_estimate <- add_spl_est(Y = Y,
                                treat = T,
                                treat_formula = T ~ X1 + X2,
                                data = hi_sim_data,
                                grid_val = quantile(hi_sim_data$T,
                                                    probs = seq(0, .95, by = 0.01)),
                                knot_num = 3,
                                treat_mod = "Gamma",
                                link_function = "inverse")
gam_estimate <- gam_est(Y = Y,
                        treat = T,
                        treat_formula = T ~ X1 + X2,
                        data = hi_sim_data,
                        grid_val = quantile(hi_sim_data$T,
                                            probs = seq(0, .95, by = 0.01)),
                        treat_mod = "Gamma",
                        link_function = "inverse")
                        
hi_estimate <- hi_est(Y = Y,
                      treat = T,
                      treat_formula = T ~ X1 + X2,
                      outcome_formula = Y ~ T + I(T^2) +
                        gps + I(gps^2) + T * gps,
                      data = hi_sim_data,
                      grid_val = quantile(hi_sim_data$T,
                                          probs = seq(0, .95, by = 0.01)),
                      treat_mod = "Gamma",
                      link_function = "inverse")

iptw_estimate <- iptw_est(Y = Y,
                          treat = T,
                          treat_formula = T ~ X1 + X2,
                          numerator_formula = T ~ 1,
                          data = hi_sim_data,
                          degree = 2,
                          treat_mod = "Gamma",
                          link_function = "inverse")

t(p_val_bal_cond)
hi_sim_data <- hi_sample(1000)
head(hi_sim_data)

###trial
lmGPS=lm(packyears~AGESMOKE+LASTAGE+MALE, nm)  ##########################
summary(lmGPS)

stddata2=nm %>% 
  mutate_at(
    vars(packyears, AGESMOKE,LASTAGE,MALE,HSQACCWT,TOTALEXP), ##############
    function(x){(x-mean(x))/sd(x)}
  )

lm(AGESMOKE~packyears,nm)$coef %>% round(4) ##low
lm(LASTAGE~packyears,stddata2)$coef %>% round(4) ##low
lm(MALE~packyears,stddata2)$coef %>% round(4) ##low


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


example_data <- sim_data

hi_list <- hi_est(Y = Y,
                  treat = T,
                  treat_formula = T ~ B.1 + B.2 + B.3 + B.4 + B.5 + B.6 + B.7 + B.8,
                  outcome_formula = Y ~ T + I(T^2) + gps + I(gps^2) + T * gps,
                  data = example_data,
                  grid_val = seq(8, 16, by = 1),
                  treat_mod = "Normal")

sample_index <- sample(1:1000, 100)

plot(index$treat,
     index$succ,
     xlab = "T",
     ylab = "Y",
     main = "hi estimate")

lines(seq(0, 8, by = 1),
      hi_estimate$param,
      lty = 2,
      lwd = 2,
      col = "blue")

legend('bottomright',
       "hi estimate",
       lty=2,
       lwd = 2,
       col = "blue",
       bty='Y',
       cex=1)

rm(example_data, hi_list, sample_index)
