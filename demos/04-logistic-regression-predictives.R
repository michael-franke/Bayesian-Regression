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
## plot the data: accuracy by group and condition
##################################################

# extract summary statistics (boostrapped 95% ConfInts)
summary_stats <- aida::data_MT |> 
  group_by(group, condition) |>
  tidyboot::tidyboot_mean(correct) |> 
  rename(accuracy = mean)

summary_stats |>  
  ggplot(aes(x = group, y = accuracy, fill = condition)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                position = position_dodge(0.25), 
                width = 0.1)+
  geom_point(position = position_dodge(0.25), 
             size = 3, 
             shape = 21, 
             colour = "black") +
  geom_line(aes(group=condition, color = condition), 
            position = position_dodge(0.25), 
            size = 1)

#### Exercise
## Based on the plot, for each of the following conjectures can we speculate
## that it is likely true or likely false, or can we not get enough info?
##   1. The 'typical' trials had higher accuracy.
##   2. The 'typical' trials had higher accuracy in the 'click' group.
##   3. The 'typical' trials had higher accuracy in the 'touch' group.
##   4. The difference in 'accuracy' between 'atypical' and 'typical' cases
##      is larger in the 'click' group than in the 'touch' group.

##################################################
## run a Bayesian logistic regression model
##################################################

fit_MT_logistic <- 
  brms::brm(
    formula = correct ~ group * condition,
    data    = aida::data_MT,
    family  = brms::bernoulli()
  )

summary(fit_MT_logistic)

#### Exercise
## 1. Interpret the results shown in the summary table.
## 2. Which, if any, of the conjectures from the previous exercise can be
##    confirmed or refuted based on the results shown in the summary?

##################################################
## extracting posterior samples of the predictor
##  of central tendency (here: estimated accuracy)
##################################################

# extract the posterior samples of the predictor of interest
samples_linear_predictor <- faintr::extract_cell_draws(
  fit = fit_MT_logistic
)

# compute samples of the estimated accuracy
samples_central_tendency <- 
  samples_linear_predictor |> 
  mutate(across(everything(), plogis))
  
#### Exercise
## 1. Test whether the 'typical' trials had higher accuracy than the
##    'atypical' trials.
## 2. Test whether the accuracy in the 'touch:Atypical' condition is credibly 
##    higher than in the 'click:typical' condition.

##################################################
## extracting posterior predictives 
##################################################

# 2 samples for the linear predictor
data_MT |> 
  select(group, condition) |> 
  unique() |> 
  tidybayes::add_linpred_draws(
    fit_MT_logistic,
    ndraws = 2
  )

# 2 samples from the predicted central tendency
data_MT |> 
  select(group, condition) |> 
  unique() |> 
  tidybayes::add_epred_draws(
    fit_MT_logistic,
    ndraws = 2
  )

# 2 samples from the predictive distribution (data samples)
data_MT |> 
  select(group, condition) |> 
  unique() |> 
  tidybayes::add_predicted_draws(
    fit_MT_logistic,
    ndraws = 2
  )
