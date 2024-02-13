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
## run a model predicting RT based on correctness
##################################################

data_MT <- aida::data_MT

fit_MT <- 
  brms::brm(
    formula = RT ~ correct,
    data    = data_MT
  )

##################################################
## sample from the posterior predictive
##################################################

tidybayes::predicted_draws(
  object  = fit_MT,
  newdata = tibble(correct = c(1, 0, 1, 0)),
  ndraws  = 1
  )  
  
tidybayes::linpred_draws(
  object  = fit_MT,
  newdata = tibble(correct = c(1, 0)),
  ndraws  = 2
)

tidybayes::epred_draws(
  object  = fit_MT,
  newdata = tibble(correct = c(1, 0)),
  ndraws  = 2
)


##################################################
## sample from the prior predictive
##################################################

fit_MT_prior <- 
  brms::brm(
    formula = RT ~ correct,
    data    = data_MT,
    prior   = prior(student_t(1, 0, 500)),
    sample_prior = "only"
  )

tidybayes::predicted_draws(
  object  = fit_MT_prior,
  newdata = tibble(correct = c(1, 0, 1, 0)),
  ndraws  = 1
)  

tidybayes::linpred_draws(
  object  = fit_MT_prior,
  newdata = tibble(correct = c(1, 0)),
  ndraws  = 2
)

tidybayes::epred_draws(
  object  = fit_MT_prior,
  newdata = tibble(correct = c(1, 0)),
  ndraws  = 2
)

##################################################
## visual posterior predictive checks
##################################################

bayesplot::pp_check(fit_MT, ndraws = 50)

##################################################
## visual PPC with summary statistic
##################################################

predictive_samples <- 
  brms::posterior_predict(
    object = fit_MT, 
    ndraws = 1000)

bayesplot::ppc_stat(
  y    = data_MT$RT, 
  yrep = predictive_samples,
  stat = median)




































