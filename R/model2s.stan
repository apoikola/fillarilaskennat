data {
  int<lower=0> N; int<lower=0> Nmain;
  int<lower=0> count[N];
  vector[N] tday;
  matrix[N,Nmain] Xmain;
}
parameters {
  real<lower=0> iphi;
  real ktemp;
  vector[Nmain] kmain;
  real intercept;
}
model {
  vector[N] t; 
  intercept ~ normal(6, 6);
  ktemp ~ normal(0, 2);
  kmain ~ normal(0, 2); // Cauchy here.
  t <- ktemp*tday/10 + Xmain*kmain + intercept;
  iphi ~ normal(0, 1.0);
  count ~ neg_binomial_2_log(t, 1/iphi);
}