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
## load, process and plot the data
##################################################

data_polite <- aida::data_polite

data_polite.agg <- 
  data_polite %>% 
  group_by(gender, context, sentence) %>% 
  summarize(mean_frequency = mean(pitch))

data_polite.agg2 <- 
  data_polite %>%
  group_by(gender, context) %>% 
  summarize(mean_frequency = round(mean(pitch), 0))

ggplot(data = data_polite.agg, 
       aes(x = gender, 
           y = mean_frequency, 
           colour = context)) + 
  geom_point(position = position_dodge(0.5), 
             alpha = 0.3, 
             size = 3) +
  geom_point(data = data_polite.agg2, 
             aes(x = gender, 
                 y = mean_frequency, 
                 fill = context),
             position = position_dodge(0.5), 
             pch = 21, 
             colour = "black",
             size = 5) +
  scale_x_discrete(breaks = c("F", "M"),
                   labels = c("female", "male")) +
  scale_y_continuous(expand = c(0, 0), breaks = (c(50,100,150,200,250,300)), limits = c(50,300)) +
  scale_colour_manual(breaks = c("inf", "pol"),
                      labels = c("informal", "polite"),
                      values = c(project_colors[1], project_colors[2])) +
  scale_fill_manual(breaks = c("inf", "pol"),
                    labels = c("informal", "polite"),
                    values = c(project_colors[1], project_colors[2])) +
  ylab("pitch in Hz\n") +
  xlab("\ngender")


##################################################
## fitting a model
##################################################

fit_polite <- 
  brms::brm(
    formula = pitch ~ context * gender,
    data    = data_polite
  )

##################################################
## quick plot of conditional effects
##################################################

# quick-plot for conditional (= "marginal") effects
brms::conditional_effects(fit_polite)
brms::conditional_effects(fit_polite, method = "posterior_predict")

##################################################
## extract and construct by hand
##################################################

samples_per_design_cell <- tidybayes::tidy_draws(fit_polite) |> 
  select(starts_with("b_")) |> 
  mutate(
    female_informal = b_Intercept,
    female_polite   = b_Intercept + b_contextpol,
    male_informal   = b_Intercept + b_genderM,
    male_polite     = b_Intercept + b_contextpol + 
                      b_genderM   + `b_contextpol:genderM`
  ) |> 
  select(5:8)
  
samples_per_design_cell |>   
  pivot_longer(cols = everything()) |> 
  ggplot(aes(x = value, y = name)) +
  tidybayes::stat_halfeye()

#### Exercise
## Suppose we wanted to compare the estimated means for the "male+informal"
## and the "female+polite" cells. How would you do that?
##
## 1. Add a new column to the data frame `samples_per_design_cell` that
##    contains samples of the differences between the two relevant cells:
##    "female+polite" - "male+informal".
## 2. Calculate the proportion of samples where the difference is positive.
## 3. Interpret the result. What did you just calculate? Hint: It's an
##    estimate of a posterior expectation.


##################################################
## use 'faintr' package
##    CAVEATs: 
##      - does not work for all GLMs
##      - only targets the linear predictor (!)
##################################################

# get samples of linear predictor for all cells
faintr::extract_cell_draws(
  fit = fit_polite
)

# compare cells or groups of cells
faintr::compare_groups(
  fit = fit_polite,
  higher = gender == "F" & context == "inf",
  lower  = gender == "M" & context == "pol"
)













































