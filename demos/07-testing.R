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
options(mc.cores = parallel::detectCores(),
        brms.backend = "cmdstanr")

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
## data & hypothesis
##################################################

k <- 7
N <- 24

ROPE <- c(0.49,0.51)

##################################################
## estimation w/ credible intervals
##################################################

# ROPE and CredInt
hdi = HDInterval::hdi(qbeta , shape1 = 8 , shape2 = 18 )
ROPE

# plot
hdiData <- tibble(
  theta = rep(hdi, each = 2),
  post = c(0,dbeta(hdi, 8, 18), 0)
)
expData <- tibble(
  theta = c(8/26,8/26),
  post = c(0,dbeta(8/26, 8, 18 ))
)
tibble(
  theta = seq(0.01,1, by = 0.01),
  posterior = dbeta(seq(0.01,1, by = 0.01), 8, 18 )
) %>% 
  ggplot(aes(x = theta, y = posterior)) + 
  xlim(0,1) + 
  labs(
    x = latex2exp::TeX("Bias $\\theta$"),
    y = latex2exp::TeX("Posterior probability $P_{M}(\\theta \\, | \\, D)$"),
    title = "Posterior"
  ) +
  geom_line(data = hdiData, aes(x = theta, y = post), color = project_colors[2], linewidth = 1.5) +
  geom_label(x = 0.7, y = 0.5, label = "Cred.Int.: 0.14 - 0.48", color = project_colors[2], size = 5) +
  geom_line(data = expData, aes(x = theta, y = post), color = project_colors[1], linewidth = 1.5) +
  geom_label(x = 0.52, y = dbeta(8/26, 8, 18 ), label = "expectation: 0.308", color = project_colors[1], size = 5) +
  geom_line(color = "black", linewidth = 2)


##################################################
## posterior probability
##################################################

# posterior probability of the ROPE
postProb_ROPE <- pbeta(ROPE[2],8,18) - pbeta(ROPE[1],8,18)

# plot
plotData <- tibble(
  theta = seq(0,1, length.out = 200),
  posterior = dbeta(theta, 8, 18)
)
plotData |> 
  ggplot(aes(x = theta, y = posterior)) + 
  geom_ribbon(aes(ymin=0, ymax=posterior), 
              fill=project_colors[2],
              alpha=0.8, 
              data=subset(plotData, theta >= 0.485 & theta <= 0.515)) +
  geom_line(size = 2) +
  xlim(0,1) + 
  geom_label(x = 0.75, y = 0.5, 
             label = str_c("Post. prob. ROPE: ", round(postProb_ROPE, 3)), 
             color = project_colors[2], size = 3) +
  labs(
    x = latex2exp::TeX("Bias $\\theta$"),
    y = latex2exp::TeX("Posterior probability $P_{M}(\\theta \\, | \\, D)$"),
    title = "Posterior"
  ) 


##############################
## Frequentist p-value
##############################

binom.test(7,24,0.5)

n_samples <- 1e+7
k_reps    <- rbinom(n_samples, 24, prob=0.5)
LH_k_reps <- dbinom(k_reps, 24, prob=0.5)
LH_k_obs  <- dbinom(7, 24, prob=0.5)
mean(LH_k_reps <= LH_k_obs)

##############################
## Bayesian p w/o BRMS
##############################

logistic <- function(x) {
  1 / (1 + exp(x))
}

logit <- function(x) {
  log(x / (1-x))
}

epsilon <- 0.0000000000000000001
ROPE = c(0.5 - epsilon, 0.5 + epsilon)

get_prior_sample_theta <- function(n_samples=1) {
  sample_prior_eta   <- runif(n_samples, logit(ROPE[1]), logit(ROPE[2]))
  # sample_prior_eta   <- rep(0,n_samples)
  sample_prior_theta <- logistic(sample_prior_eta)
  # print(sample_prior_theta)
  return(sample_prior_theta)
}

get_prior_sample_k <- function(n_samples=1) {
  sample_prior_theta <- get_prior_sample_theta(n_samples)
  sample_prior_k     <- map_dbl(sample_prior_theta, function(theta) rbinom(1, 24, theta))
  return(sample_prior_k)
}

get_LH_approx <- function(k, n_samples=1) {
  dbinom(k, 24, get_prior_sample_theta(n_samples), log = F) |> mean()
}

n_samples_k         <- 10000
n_samples_LH_approx <- 100

k_reps    <- get_prior_sample_k(n_samples_k)
LH_k_reps <- map_dbl(k_reps, function(k) get_LH_approx(k, n_samples_LH_approx))
LH_k_obs  <- get_LH_approx(k, 500000)
mean(LH_k_reps <= LH_k_obs)

##############################
## Bayesian p w BRMS binomial
##############################

data_24_7_binomial <- 
  tibble(k = 7, N = 24)

# n_samples_k         <- 10e5
n_samples_k         <- 1e+04
n_samples_LH_approx <- 100

fit_logistic_prior <- brms::brm(
  formula = k | trials(N) ~ 1,
  data = data_24_7_binomial,
  family = binomial(link = "logit"),
  # this is an extremely stupid "unBayesian prior" !!!
  #   but it's the hypothesis we are interested in
  prior = brms::prior("uniform(-0.04000533, 0.04000533)", 
                      class = "Intercept", 
                      lb = -0.04000533, ub = 0.04000533),
  sample_prior = "only",
  iter = 5000,
  warmup = 1000
)

# sanity check implied prior for expected value of a single flip
fit_logistic_prior |> 
  tidybayes::add_linpred_draws(newdata = data_24_7_binomial) |> 
  ggplot(aes(x = logistic(.linpred))) + 
  geom_density()

priorPred_samples <- tidybayes::add_predicted_draws(
  fit_logistic_prior,
  newdata = data_24_7_binomial |> select(-k),
  ndraws = n_samples_k,
  value = "k"
) |> ungroup() |> 
  select(.draw, k, N)

# extract likelihood for observations (approximated w/ MC sampling)
get_LH <- function(k, N, ndraws = n_samples_LH_approx) {
  brms::log_lik(
    object  = fit_logistic_prior,
    newdata = tibble(k = k, N = N),
    ndraws  = ndraws) |> 
    exp() |> 
    mean()
}

# get likelihood of predictive samples
# (this may take a while!!)
LH_predictions <- priorPred_samples |> 
  group_by(.draw) |> 
  summarize(LH_post_pred = get_LH(k, N)) |> 
  pull(LH_post_pred)

# get likelihood of data
LH_data <- get_LH(k = 7, N = 24, ndraws = 16000)

# Bayesian $p$-values with LH as test statistic
print(mean(LH_predictions <= LH_data))


##################################################
## Bayes factors w/ Savage-Dickey & conjugacy
##################################################

# set the scene
k <- 7
N <- 24
theta_null <- 0.5
epsilon <- 0.01                 # epsilon margin for ROPE
upper <- theta_null + epsilon   # upper bound of ROPE
lower <- theta_null - epsilon   # lower bound of ROPE
alpha <- 1                      # prior beta parameter
beta  <- 1                      # prior beta parameter
# calculate prior odds of the ROPE-d hypothesis
prior_of_hypothesis <- pbeta(upper, alpha, beta) - pbeta(lower, alpha, beta)
prior_odds <- prior_of_hypothesis / (1 - prior_of_hypothesis)
# calculate posterior odds of the ROPE-d hypothesis
posterior_of_hypothesis <- pbeta(upper, alpha + k, beta + (N-k)) - pbeta(lower, alpha + k, beta + (N-k))
posterior_odds <- posterior_of_hypothesis / (1 - posterior_of_hypothesis)
# calculate Bayes factor
bf_ROPEd_hypothesis <- posterior_odds / prior_odds
bf_ROPEd_hypothesis

##################################################
## Bayes factors w/ Savage-Dickey & log. regress.
##################################################

fit_logistic_posterior <- brms::brm(
  formula = outcome ~ 1, 
  data   = data_24_7,
  family = brms::bernoulli(link = "logit"),
  prior  = brms::prior("normal(0,1.8)", 
                        class = "Intercept"),
  iter = 50000,
  warmup = 1000
)

fit_logistic_prior <- stats::update(
  fit_logistic_posterior, 
  sample_prior = "only",
  iter = 50000,
  warmup = 1000)

# sanity check: prior distribution of central tendency
fit_logistic_prior |> 
  tidybayes::add_epred_draws(newdata = data_24_7) |> 
  ggplot(aes(x = .epred)) + 
  geom_density()

prior_samples_Intercept <- fit_logistic_prior |> 
  tidybayes::tidy_draws()

posterior_samples_Intercept <- fit_logistic_posterior |> 
  tidybayes::tidy_draws()

# prior probs of ROPE and its negation
P_I0_prior <- mean(prior_samples_Intercept >= logit(lower) & prior_samples_Intercept <= logit(upper) )
P_I1_prior <- 1 - P_I0_prior

# posterior probs of ROPE and its negation
P_I0_posterior <- mean(posterior_samples_Intercept >= logit(lower) & posterior_samples_Intercept <= logit(upper) )
P_I1_posterior <- 1 - P_I0_posterior

# Bayes factor
(P_I0_posterior / P_I1_posterior) / (P_I0_prior / P_I1_prior)


##################################################
## LOO-based model comparison
##################################################
  
# alternative model is just the posterior fit
fit_logistic_alternative <- fit_logistic_posterior

# the "null model" assumes that the parameter is in the ROPE
fit_logistic_null <- brms::brm(
  formula = outcome ~ 1, 
  data = data_24_7,
  family = brms::bernoulli(link = "logit"),
  prior = brms::prior("uniform(-0.04000533, 0.04000533)", 
                      class = "Intercept", 
                      lb = -0.04000533, ub = 0.04000533),
  iter = 50000,
  warmup = 1000
)

# compute LOO-CV
loo_comp <- loo_compare(list(
  alternative = loo(fit_logistic_alternative), 
  null = loo(fit_logistic_null)))
loo_comp

# significance test
1 - pnorm(-loo_comp[2,1], loo_comp[2,2])




##################################################
## using BRMS 'hypothesis' function
##################################################

# sample BOTH posterior and prior
fit_logistic_post_and_prior <- brms::brm(
  formula = outcome ~ 1, 
  data = data_24_7,
  family = brms::bernoulli(link = "logit"),
  prior = brms::prior("normal(0,1.8)", 
                      class = "Intercept"),
  iter = 50000,
  warmup = 1000,
  sample_prior = "yes"
)

# point-valued hypothesis test
fit_logistic_post_and_prior |> 
  brms::hypothesis(hypothesis = "Intercept = 0")

# ROPE-valued hypothesis test
fit_logistic_post_and_prior |> 
  brms::hypothesis(hypothesis = "abs(Intercept) < 0.01")









