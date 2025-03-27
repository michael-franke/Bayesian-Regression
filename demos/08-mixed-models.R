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
## data 
##################################################

d_SPR <- read_delim("08-data_SPR-gibsonwu2012.txt", delim = " ") 

#### Exercise
## Try to guess what the information is that is  contained in each column of the data set.

# preprocess and select the relevant data
d_SPR <- d_SPR |> 
  filter(region == "headnoun") |> 
  mutate(so = ifelse(type == "subj-ext", "-1", "1")) |> 
  select(subj, item, so, rt)

#### Exercise
## 1. Look at the data again. What's the information in each column now?
## 2. The following commands give you counts of how many times different levels of factors in the
##    data set co-occur. What do you learn from this? How many items did each subject see in either
##    of the two conditions? Did subjects see each item in both conditions?

xtabs(~ item + so, d_SPR)
xtabs(~ item + subj, d_SPR)
xtabs(~ subj + so, d_SPR)

##################################################
## summary stats & plots
##################################################

d_SPR |>  
  group_by(so) %>% 
  summarize(mean_log_rt = mean(log(rt)))

d_SPR |> 
  ggplot(aes(x = so, y = log(rt))) + 
  geom_violin() + 
  geom_point(position = "jitter", 
             color = project_colors[1])

##################################################
## simple linear nodel w/o  mixed-effects
##################################################

#### Exercise
## 1. Run a simple linear model without mixed effects that predicts log(rt) from so.
## 2. Interpret the results. Is there evidence for an effect of condition?

fit_FE <- brms::brm(log(rt) ~ so, data = d_SPR)
summary(fit_FE)


##################################################
## adding random intercepts
##################################################

#### Exercise
## 1. Run a mixed-effects model that includes random intercepts for item and subject, also called
##    by-subject random intercepts and by-item random intercepts.
## 2. Interpret the results. Is  there evidence for an effect of condition?
## 3. Based on the posterior estimates for the random effects, do you think that including these 
##    random effects is warrated?

fit_RandInt <- brms::brm(log(rt) ~ (1 | subj + item) + so, data = d_SPR) 
summary(fit_RandInt)

##################################################
## full random effect structure
##################################################

#### Exercise
## 1. Run a mixed-effects model that includes random intercepts for item and subject, and 
##    random slopes for so by subject and by item, including their interaction.
## 2. Interpret the results. Is there evidence for an effect of condition?
## 3. Based on the posterior estimates for the random effects, do you think that including these
##    random effects is warrated?

fit_MaxRE <- brms::brm(log(rt) ~ (1 + so | subj + item) + so, data = d_SPR) 
summary(fit_MaxRE)

##################################################
## model comparison
##################################################

#### Exercise
## 1. Compare the models fit_FE, fit_RandInt, and fit_MaxRE using LOO.
##    To do so, you first need to add the LOO estimates to the models using the loo() function.
##    Then, you can compare the models using loo_compare().
## 2. Which model is the best according to LOO? What does this tell you about the importance of
##    including random effects in the model?

loo::loo_compare(loo(fit_FE), loo(fit_RandInt), loo(fit_MaxRE))



