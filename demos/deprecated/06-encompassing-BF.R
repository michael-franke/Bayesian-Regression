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
## run a model with max RE structure
##################################################

fit_polite <- brms::brm(
  formula = pitch ~ gender * context + 
    (1 + gender * context | sentence) + 
    (1 + context | subject),
  data    = data_polite,
  control = list(adapt_delta = 0.99),
  prior   = prior("student_t(1,100,200)")
  )

##################################################
## test whether context matters for female pitch
##################################################

brms::hypothesis(fit_polite, "contextpol - exp(genderM) < 0")

faintr::compare_groups(
  fit_polite,
  higher = gender == "F" & context == "inf",
  lower  = gender == "F" & context == "pol",
  include_bf = TRUE
)


