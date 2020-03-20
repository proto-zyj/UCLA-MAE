#PS2 QUESTION7
library(ggplot2)

##(a) Asset prices

# we normalize expected dividend to 1.
E0_D = matrix(rep(1,5), nrow = 5)

# assume gamma = 0.01
gamma = 0.01

#vcov matrix of D is sigma
sigma = matrix(c(0.040, 0.064, 0.08, 0.016, 0.048,
                 0.064, 0.160, 0.16, 0.032, 0.096,
                 0.080, 0.160, 0.25, 0.040, 0.120,
                 0.016, 0.032, 0.04, 0.010, 0.024,
                 0.048, 0.096, 0.12, 0.024, 0.090),
               nrow = 5, ncol =5, byrow = T)

# N_bar is supply
N_bar = matrix(c(5, 10, 20, 30, 40), nrow = 5)

# rf is risk-free rate
r_f = 0.01

P = (E0_D - gamma * sigma %*% N_bar ) / (1 + r_f)


##(b) Asset expected returns
E0_r = E0_D/P - 1

##(c) The portfolio weight of each asset in the market portfolio
w = N_bar * P / sum(N_bar * P)

##(d) The beta of each stock return with respect to the return of the market portfolio

# calculate the vcov matrix for r
sigma_r = sigma / (P %*% t(P))

# calculate the variance of the return of market portfolio
V_m = t(w)  %*% sigma_r %*% w

# generate the temp matrix for weight for single asset portfolio
w_s = matrix(rep(0,5), nrow = 5)

# get beta
beta = rep(0,5)
for (i in 1:5){
  w_s[i] = 1
  beta[i] = t(w) %*% sigma_r %*% w_s / V_m
  w_s[i] = 0
}

##(e) draw security market line, verify that expected returns lie on the security market line
r_m = t(w) %*% E0_r
portfolio = data.frame("beta" = beta, "return" = E0_r)
ggplot(portfolio, aes(x = beta, y = return)) +
  geom_point(color = "black", size = 3) +
  geom_abline(intercept = r_f,
              slope = r_m - r_f,
              color = "red") +
  ggtitle("Security Market Line") +
  theme(plot.title = element_text(hjust = 0.5))

