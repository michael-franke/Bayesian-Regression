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
    MAD = median(MAD, na.rm = TRUE)) 

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
    data    = dolphin_agg
  )

summary(fit_dolphin)

##################################################
## inspect the fit
##################################################

tidybayes::tidy_draws(fit_dolphin) |> 
  select(starts_with("b_")) |> 
  pivot_longer(cols = everything()) |> 
  group_by(name) |> 
  summarize(
    aida::summarize_sample_vector(value)[-1]
    )


tidybayes::tidy_draws(fit_dolphin) |> 
  select(b_Intercept, b_MAD, sigma) 











































