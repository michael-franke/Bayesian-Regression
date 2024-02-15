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

#BEFORE
# options(mc.cores = parallel::detectCores(),         
#         brms.backend = "cmdstanr")

# AFTER
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
## assessing priors BEFORE running a model
##################################################

# default priors for the model and the data
brms::get_prior(
  formula = AUC ~ MAD,
  data    = dolphin_agg)


##################################################
## fitting a model /w user-adjusted priors
##################################################

fit_dolphin <- 
  brms::brm(
    formula = AUC ~ MAD,
    data    = dolphin_agg,
    prior   = brms::prior(prior = "normal(0,10)", class = "b")
  )


##################################################
## sampling parameters from the prior distribution 
##################################################

# convenience function
extract_and_summarize <- function(fitted_object) {
  fitted_object |> 
    tidybayes::tidy_draws() |> 
    select("b_Intercept", "b_MAD", "sigma") |>   
    pivot_longer(cols = everything()) |> 
    group_by(name) |> 
    summarize(
      aida::summarize_sample_vector(value)[-1]
    )
}

# 1. update previously fitted model

fit_dolphin_prior_1 <- stats::update(
  fit_dolphin,
  sample_prior = "only"
)

fit_dolphin_prior_1 |> 
  tidybayes::get_variables()

fit_dolphin_prior_1 |> 
  extract_and_summarize()

# 2. sample priors ONLY from scratch

fit_dolphin_prior_2 <- 
  brms::brm(
    formula = AUC ~ MAD,
    data    = dolphin_agg,
    prior   = brms::prior(prior = "normal(0,10)", class = "b"),
    sample_prior = "only"
  )

fit_dolphin_prior_2 |> 
  tidybayes::get_variables()

fit_dolphin_prior_2 |> 
  extract_and_summarize()

# 3. sample priors ADDITIONALLY to priors
#    CAVEAT: different data structure for prior samples!!!
#            b/c we sample BOTH at the same time

fit_dolphin_prior_3 <- 
  brms::brm(
    formula = AUC ~ MAD,
    data    = dolphin_agg,
    prior   = brms::prior(prior = "normal(0,10)", class = "b"),
    sample_prior = "yes"
  )

fit_dolphin_prior_3 |> 
  tidybayes::get_variables()

fit_dolphin_prior_3 |> 
  extract_and_summarize()


##################################################
## samples from the prior predictives
##################################################

# 1. linear predictor prediction

linPred_samples <- fit_dolphin_prior_1 |> 
  tidybayes::add_linpred_draws(
    newdata = dolphin_agg |> select(MAD),
    ndraws = 5
  )

linPred_samples |> 
  ggplot(aes(x = MAD, y = .linpred, group = .draw)) +
  geom_line(color = "gray", alpha = 0.7)

# 2. data predictions

dataPred_samples <- fit_dolphin_prior_1 |> 
  tidybayes::add_predicted_draws(
    newdata = dolphin_agg |> select(MAD),
    ndraws = 3
  ) 

dataPred_samples |> 
  mutate(
    .draw = factor(.draw)
  ) |> 
  ggplot(aes(x = MAD, y = .prediction, color = .draw)) +
  geom_point(alpha = 0.7)


##################################################
## samples from the posterior predictives
##################################################

# 1. linear predictor prediction

linPred_samples <- fit_dolphin |> 
  tidybayes::add_linpred_draws(
    newdata = dolphin_agg |> select(MAD),
    ndraws = 5
  )

linPred_samples |> 
  ggplot(aes(x = MAD, y = .linpred, group = .draw)) +
  geom_line(color = "gray", alpha = 0.7)

# 2. data predictions

dataPred_samples <- fit_dolphin |> 
  tidybayes::add_predicted_draws(
    newdata = dolphin_agg |> select(MAD),
    ndraws = 3
  ) 

dataPred_samples |> 
  mutate(
    .draw = factor(.draw)
  ) |> 
  ggplot(aes(x = MAD, y = .prediction, color = .draw)) +
  geom_point(alpha = 0.7)

