##################################################
## preamble (packages and stuff)
##################################################

# install packages from CRAN (unless installed)
pckgs_needed <- c(
  "tidyverse",
  "brms",
  "rstan",
  "rstanarm",
  "remotes",
  "tidybayes",
  "bridgesampling",
  "shinystan",
  "mgcv"
)
pckgs_installed <- installed.packages()[,"Package"]
pckgs_2_install <- pckgs_needed[!(pckgs_needed %in% pckgs_installed)]
if(length(pckgs_2_install)) {
  install.packages(pckgs_2_install)
} 

# install additional packages from GitHub (unless installed)
if (! "aida" %in% pckgs_installed) {
  remotes::install_github("michael-franke/aida-package")
}
if (! "faintr" %in% pckgs_installed) {
  remotes::install_github("michael-franke/faintr")
}
if (! "cspplot" %in% pckgs_installed) {
  remotes::install_github("CogSciPrag/cspplot")
}

# load the required packages
x <- lapply(pckgs_needed, library, character.only = TRUE)
library(aida)
library(faintr)
library(cspplot)

# these options help Stan run faster
options(mc.cores = parallel::detectCores())

# use the CSP-theme for plotting
theme_set(theme_csp())

# global color scheme from CSP
project_colors = cspplot::list_colors() |> pull(hex)
# names(project_colors) <- cspplot::list_colors() |> pull(name)

# setting theme colors globally
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = project_colors)
}
scale_fill_discrete <- function(...) {
   scale_fill_manual(..., values = project_colors)
}

##################################################
## load and prepare data
##################################################

dolphin <- aida::data_MT

dolphin_agg <- dolphin |> 
  # only correct trials
  filter(correct == 1) |> 
  # by-subject medians (for convenience / robustness)
  group_by(subject_id) |> 
  dplyr::summarize(
    AUC = median(AUC, na.rm = TRUE),
    MAD = median(MAD, na.rm = TRUE)) |> 
  ungroup() |> 
  # standardizing measures
  mutate(
    AUC = (AUC - mean(AUC)) / sd(AUC),
    MAD = (MAD - mean(MAD)) / sd(MAD)
  )

##################################################
## plotting
##################################################

dolphin_agg |> 
  ggplot(aes(x = MAD, y = AUC)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm")

##################################################
## fitting a model
##################################################

fit_dolphin <- 
  brms::brm(
    formula = AUC ~ MAD,
    data    = dolphin_agg,
  )

summary(fit_dolphin)

#### Exercise:
## 1. What do the coefficients tell you?
## 2. Check the function documentation of `brms::brm` to find
##    out what the default configuration is for the MCMC sampling
##    (number of chains, iterations, warmup and thinning rate).
## 3. Rerun the model with different parameters for the MCMC sampler
##    so that MCMC sampling with UNreliable. Inspect the warnings
##    and diagnostics in the summary of the fit.

##################################################
## inspect the fit
##################################################

# extract samples for regression coefficients 
# and compute Bayesian summary stats
tidybayes::tidy_draws(fit_dolphin) |> 
  select(starts_with("b_")) |> 
  pivot_longer(cols = everything()) |> 
  group_by(name) |> 
  summarize(
    aida::summarize_sample_vector(value)[-1]
    )

#extracting the samples
tidybayes::tidy_draws(fit_dolphin) |> 
  select(b_Intercept, b_MAD, sigma) 

# explore via shinystan
shinystan::launch_shinystan(fit_dolphin)

#### Exercise:
## Explore the shinystan interface. What do you understand? 
## What don't you understand? What might be useful for you?


##################################################
## plotting w/ package 'bayesplot'
##################################################

plot(fit_dolphin)

bayesplot::mcmc_intervals(
  fit_dolphin, 
  pars = c("b_Intercept", "b_MAD", "sigma"))

# pairs plot
bayesplot::mcmc_pairs(fit_dolphin,
                      pars = c("b_Intercept", "b_MAD", "sigma"))

##################################################
## plotting w/ package 'tidybayes'
##################################################

fit_dolphin |> 
  tidybayes::tidy_draws() |> 
  select(b_Intercept, b_MAD, sigma) |> 
  pivot_longer(cols = everything(),
               names_to = "parameter", values_to = "posterior") |> 
  ggplot(aes(x = posterior, y = parameter, fill = parameter)) + 
  # plot density w/ 91.2345% credible interval
  tidybayes::stat_halfeye(.width = 0.912345) +
  # add axes titles
  xlab("") +
  ylab("") +
  # adjust the x-axis 
  scale_x_continuous(limits = c(-0.25,1.5)) +
  # add line for the value zero
  geom_segment(x = 0, xend = 0, y = Inf, yend = -Inf,
               lty = "dashed") +
  theme(legend.position="none")


##################################################
## samples from the posterior predictive
##################################################

# we use the original x-values to generate "new predictions for old values"
data_input_for_prediction <- tibble(
  MAD = dolphin_agg$MAD
)

# samples from the linear predictor
tidybayes::linpred_draws(
  object  = fit_dolphin,
  newdata = data_input_for_prediction,
  ndraws  = 1
)

# samples of predicted data points
tidybayes::predicted_draws(
  object  = fit_dolphin,
  newdata = data_input_for_prediction,
  ndraws  = 1
)  

#### Exercise:
## Get four samples of possible AUC values for each MAD value 0, 1 and 10.




