data {
  int<lower=0> N; int<lower=0> Nsite;
  int<lower=0> count[N];
  real tday[N]; int site[N]; int weekday[N]; int holiday[N];
}
parameters {
  //vector<lower=-pi()/2, upper=pi()/2>[N] berr; 
  //real<lower=0> sigma;
  real<lower=0> phi;
  real ktemp;
  vector[Nsite] ksite;
  vector[7] kweekday; real kholiday;
  real intercept;
}
//transformed parameters {
//  vector[N] err;
//  for (i in 1:N) err[i] <- sigma*tan(berr[i]); // cauchy
//}
model {
  vector[N] t;
  for (i in 1:N) 
     t[i] <-  ktemp*tday[i] + ksite[site[i]] + kweekday[weekday[i]] + kholiday*(holiday[i]==1) +
              intercept;
  ktemp ~ normal(0, 2); kweekday ~ normal(0, 2); kholiday ~ normal(0, 2); ksite ~ normal(0, 3);
  // sigma ~ normal(0, 7.0);
  intercept ~ normal(3, 7);
  phi ~ normal(0, 7);
  count ~ neg_binomial_2(exp(t), phi);
}