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
## create artificial data (from robust regr.)
##################################################

set.seed(1970)

# number of observations
N <- 100
# 100 samples from a standard normal
x <- rnorm(N, 0, 1)

intercept <- 2
slope <- 4

# robust regression with a Student's t error distribution
# with 1 degree of freedom
y <- rt(N, df = 1, ncp = slope * x + intercept)

data_robust <- tibble(x = x, y = y)

##################################################
## plot the data
##################################################

qplot(x,y) + 
  geom_smooth(color = project_colors[1], method = "lm") +
  geom_point(color = project_colors[2], size = 2, alpha = 0.8)


##################################################
## normal vs robust regression model
##################################################

fit_n <- brm(
  formula = y ~ x,
  data = data_robust,
  # student prior for slope coefficient
  prior = prior("student_t(1,0,30)", class = "b"),
)

fit_r <- brm(
  formula = y ~ x,
  data = data_robust,
  # student prior for slope coefficient
  prior = prior("student_t(1,0,30)", class = "b"),
  family = student()
)

##################################################
## inspect (joint) summary
##################################################

prep_summary <- function(fit, model) {
  tidybayes::summarise_draws(fit) |> 
    mutate(model = model) |> 
    select(model, variable, q5, mean, q95) |> 
    filter(grepl(variable, pattern = '^b'))  
}

rbind(prep_summary(fit_n, "normal"), prep_summary(fit_r, "robust"))

##################################################
## compare models w/ LOO
##################################################

loo_comp <- loo_compare(list(normal = loo(fit_n), robust = loo(fit_r)))
loo_comp

# Lambert's test
1 - pnorm(-loo_comp[2,1], loo_comp[2,2])


##################################################
## compare models w/ LOO
##################################################

# running these takes a long time
rerun_models <- FALSE

if (rerun_models) {
  # refit normal model
  fit_n_4Bridge <- update(
    fit_n,
    iter = 5e5,
    save_pars = save_pars(all = TRUE)
  )
  # refit robust model
  fit_r_4Bridge <- update(
    fit_r,
    iter = 5e5,
    save_pars = save_pars(all = TRUE)
  )
  normal_bridge <- bridge_sampler(fit_n_4Bridge, silent = T)
  write_rds(normal_bridge, "04-normal_bridge.rds")
  robust_bridge <- bridge_sampler(fit_r_4Bridge, silent = T)  
  write_rds(robust_bridge, "04-robust_bridge.rds")
} else {
  normal_bridge <- read_rds("04-normal_bridge.rds")  
  robust_bridge <- read_rds("04-robust_bridge.rds")
}

# calculating the bridge 
bf_bridge <- bridgesampling::bf(robust_bridge, normal_bridge)
bf_bridge

