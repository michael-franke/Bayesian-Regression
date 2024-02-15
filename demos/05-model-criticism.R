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
## data we want to analyze
##################################################

aida::data_WorldTemp |> 
  ggplot(aes(x = avg_temp)) + geom_density()


##################################################
## fit a regression model for world-temp
##################################################

# define "opinionated" priors
prior_opinionated <- c(prior("normal(0.2, 0.05)", class = "b"),
                       prior("student_t(3, 8, 5)", class = "Intercept"))

# fit model to data (i.e., obtain samples from the posterior)
fit_posterior <- brm(
  avg_temp ~ year,
  prior = prior_opinionated,
  data = aida::data_WorldTemp
)

# obtain samples from the prior
fit_prior <- stats::update(
  fit_posterior,
  sample_prior = "only"
)

##################################################
## visual PCs
##################################################

# prior PC
brms::pp_check(fit_prior, ndraws = 50)

# posterior PC
brms::pp_check(fit_posterior, ndraws = 50)


## More specific VPCs
predictive_samples <- brms::posterior_predict(fit_posterior, ndraws = 1000)
predictive_samples[1:5, 1:5] 

bayesplot::ppc_stat(
  y    = aida::data_WorldTemp$avg_temp, 
  yrep = predictive_samples,
  stat = sd)

##################################################
## Bayesian $p$-values
##################################################

# get 4000 sets of posterior predictive samples;
#   one set for all data points up to 1800
postPred_y <- 
  tidybayes::predicted_draws(
    object  = fit_posterior,
    newdata = aida::data_WorldTemp |> select(year) |> filter(year <= 1800),
    value   = "avg_temp",
    ndraws  = 4000) |> ungroup() |> 
  select(.draw,year, avg_temp)

# calculate the standard deviation for each set of samples
sd_postPred <- postPred_y |> 
  group_by(.draw) |> 
  summarize(sd_post_pred = sd(avg_temp)) |> 
  pull(sd_post_pred)

# calculate the SD of the $y$-measurements in
#   the data (up to 1800)
sd_data <- aida::data_WorldTemp |> filter(year <= 1800) |> pull(avg_temp) |> sd()

# approx. p-value <- proportion of samples that have
#   value of the test statistic that is more extreme
#   than that of the data
mean(sd_data < sd_postPred)


##################################################
## Likelihood as a test statistics
##################################################


get_LH <- function(avg_temp, ndraws = 1000) {
  LH_ys <- brms::log_lik(
    object  = fit_posterior,
    newdata = tibble(avg_temp = avg_temp, 
                     year = aida::data_WorldTemp$year),
    ndraws  = ndraws)
  mean(matrixStats::rowLogSumExps(LH_ys) - log(dim(LH_ys)[2]))
}

postPred_y <- 
  tidybayes::predicted_draws(
    object  = fit_posterior,
    newdata = aida::data_WorldTemp |> select(year),
    value   = "avg_temp",
    ndraws  = 100) |> ungroup() |> 
  select(.draw, year, avg_temp)

LH_postPred <- postPred_y |> 
  group_by(.draw) |> 
  summarize(LH_post_pred = get_LH(avg_temp)) |> 
  pull(LH_post_pred)

LH_data <- get_LH(aida::data_WorldTemp$avg_temp)

mean(LH_data > LH_postPred)





