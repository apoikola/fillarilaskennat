data {
  int<lower=0> N; int<lower=0> Nsite;
  int<lower=0> count[N];
  vector[N] tday; int site[N]; int weekday[N]; int holiday[N];
}
parameters {
  //vector[N] err;
  real<lower=0> iphi;
  real ktemp;
//  vector[Nsite] ksite;
//  vector[7] kweekday; real kholiday;
  real intercept;
}
model {
  vector[N] t; 
  ktemp ~ normal(0, 2); //kholiday ~ normal(0, 2);
//  kweekday ~ normal(0, 2);  ksite ~ normal(0, 3);
  intercept ~ normal(6, 3);
  // for (i in 1:N) 
  //   t[i] <-  ktemp*tday[i] + //ksite[site[i]] + kweekday[weekday[i]] + kholiday*(holiday[i]==1) +
  //            intercept;
  t <- ktemp*tday/10 + intercept;
  iphi ~ normal(0, 1.0);
  count ~ neg_binomial_2_log(t, 1/iphi);
  // err ~ normal(0, sigma);
  // count ~ poisson_log(t+err);
}