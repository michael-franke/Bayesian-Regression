data {
  int<lower=1> N; 
  real y[N];      
}
parameters {
  real<lower=0,upper=1> p;         
  ordered[2] mu;             
  vector<lower=0>[2] sigma; 
}
model {
  p ~ beta(1,1);
  mu ~ normal(12, 10);
  sigma ~ lognormal(0, 1);
  for (i in 1:N) {
    vector[2] alpha;
    alpha[1] = log(p)   + normal_lpdf(y[i] | mu[1], sigma[1]);
    alpha[2] = log(1-p) + normal_lpdf(y[i] | mu[2], sigma[2]);
    target += log_sum_exp(alpha);
  }
}
generated quantities{
  real yrep;
  int<lower=0,upper=1> z;
  z = bernoulli_rng(p);
  yrep = normal_rng(mu[z+1], sigma[z+1]);
}
