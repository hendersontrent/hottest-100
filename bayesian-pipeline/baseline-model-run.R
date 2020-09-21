#-------------------------------------------
# This script sets out to run the baseline
# Bayesian logit model written in Stan
#
# NOTE: This script requires setup.R to
# have been run first
#-------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 18 September 2020
#-------------------------------------------

# Prep data (mean centre and standardise)

model_dat <- the_data %>%
  mutate(plays_c = (log(plays) - mean(log(plays)))/sd(log(plays)),
         likes_c = (log(fb_likes) - mean(log(fb_likes)))/sd(log(fb_likes)),
         days_c = (days_released - mean(days_released))/sd(days_released),
         rank_c = (the_rank - mean(the_rank))/sd(the_rank))
  mutate(australian = as.numeric(australian)) %>%
  mutate(australian = case_when(
    australian == 1 ~ 0,
    TRUE            ~ 1))

# Set up all input parameters for Stan model and put into a list

N <- nrow(model_dat)
x1 <- model_dat$likes_c
x2 <- model_dat$plays_c
x3 <- model_dat$rank_c
x4 <- model_dat$days_c
y <- as.numeric(model_dat$australian) - 1

stan_data <- list(N = N, x1 = x1, x2 = x2, x3 = x3, x4 = x4, y = y)

#--------------------- RUN MODEL -----------------------------------

system.time({
  mod1 <- stan(data = stan_data, 
               file = "bayesian-pipeline/logit.stan",
               iter = 2000,
               chains = 4,
               seed = 123)
})

# High level plots

plot(mod1, pars = c("alpha", "beta"))
traceplot(mod1, pars = c("alpha", "beta")) # Model convergence

#--------------------- MODEL OUTPUTS ---------------------------------

summary(mod1)[["summary"]][c(paste0("beta[",1:4, "]"), "alpha"),]

# Extract fit

ext_fit <- as.data.frame(extract(mod1))

# Make plot

p <- ext_fit %>%
  dplyr::select(c(1:5)) %>%
  gather(key = metric, value = value, 1:5) %>%
  group_by(metric) %>%
  summarise(mean = mean(value),
            upper_975 = quantile(value, 0.975),
            lower_025 = quantile(value, 0.025),
            upper_900 = quantile(value, 0.900),
            lower_100 = quantile(value, 0.100)) %>%
  ungroup() %>%
  mutate(metric = case_when(
    metric == "beta.1"  ~ "Facebook Likes",
    metric == "beta.2"  ~ "Spotify Plays",
    metric == "beta.3"  ~ "Hottest 100 Rank",
    metric == "beta.4"  ~ "Days Since Release",
    metric == "alpha"   ~ "Alpha")) %>%
  ggplot() +
  geom_segment(aes(x = lower_025, y = metric, xend = upper_975, yend = metric), 
               size = 3, colour = "steelblue2", alpha = 0.5) +
  geom_segment(aes(x = lower_100, y = metric, xend = upper_900, yend = metric), 
               size = 5, colour = "steelblue2", alpha = 0.8) +
  geom_point(aes(x = mean, y = metric), size = 5, colour = "#05445E") +
  labs(title = "Bayesian logit model outputs predicting artist nationality",
       subtitle = "Point = Mean Estimate | Dark band = 80% Credible Interval |\nLight band = 95% Credible Interval.",
       x = "Posterior Probability Value",
       y = "Metric",
       caption = "Analysis: Orbisant Analytics.") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
print(p)

#---------------------EXPORT PLOT ----------------------------------

CairoPNG("output/bayes-plot.png", 800, 600)
print(p)
dev.off()

#---------------------EXTRACT PRIORS FOR FUTURE MODEL --------------

alpha_posterior <- ext_fit$alpha
beta_x1_posterior <- ext_fit$beta.1
beta_x2_posterior <- ext_fit$beta.2
beta_x3_posterior <- ext_fit$beta.3
beta_x4_posterior <- ext_fit$beta.4
