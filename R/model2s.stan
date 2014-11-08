data {
  int<lower=0> N; 
  int<lower=0> count[N];
  vector[N] tday;
}
parameters {
  real<lower=0> iphi;
  real ktemp;
  real intercept;
}
model {
  vector[N] t; 
  ktemp ~ normal(0, 2);
  intercept ~ normal(6, 3);
  t <- ktemp*tday/10 + intercept;
  iphi ~ normal(0, 1.0);
  count ~ neg_binomial_2_log(t, 1/iphi);
}