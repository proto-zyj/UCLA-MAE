# preparation

### const
T = 365
sigma = 0.5
r = (1 + 0.01)^(1/365) - 1
delta_t = 1 / 365
u = exp(sigma * sqrt(delta_t))
d = exp(-sigma * sqrt(delta_t))
X = 100
A_u = ((1 + r) - d) / ((1 + r) * (u - d)) 
A_d = (u - (1 + r)) / ((1 + r) * (u - d)) 


### price matrix
scenario = function(S_0) {
  M = matrix(0, nrow = T + 1, ncol = T + 1)
  for (i in 1:(T + 1)) {
    for (j in 1:i) {
      M[j, i] = S_0 * u ^ (i - j) * d ^ (j - 1)
    }
  }
  return (M)
}


### matrix of value of call and put
M_call = function(S_0) {
  M = scenario(S_0)
  M[, T + 1] = pmax(M[, T + 1] - X, 0)
  for (j in (T + 1):1) {
    for (i in 1 : j - 1) {
      M[i, j - 1] = A_u * M[i, j] + A_d * M[i + 1, j]
    }
  }
  return(M)
}

M_put = function(S_0) {
  M = scenario(S_0)
  M[, T + 1] = pmax(X - M[, T + 1], 0)
  for (j in (T + 1):1) {
    for (i in 1 : j - 1) {
      M[i, j - 1] = A_u * M[i, j] + A_d * M[i + 1, j]
    }
  }
  return(M)
}


# a,b price of put and call
C_0 = rep(0, 101)
P_0 = rep(0, 101)
for (i in 50:150){
  C_0[i-49] = M_call(i)[1,1]
  P_0[i-49] = M_put(i)[1,1]
}

# c put-call parity
M_option = data.frame("C_0" = C_0, "P_0" = P_0, "S_0" = seq(50,150))
M_option$parity = M_option$C_0 - M_option$S_0 + X * exp(-r * 365) - M_option$P_0
any(M_option$parity < 1e-5) # parity holds

# d
#### const
n = 31 + 28 + 31 + 30 +31 + 30 + 1 # look the price at day 182

## i
#Convert when at day 182, stock price is larger than bond price

## ii
# the value of the price is max(price of stock at day 182, price of bond at day 182)
M_zcb = matrix(0, nrow = n + 1, ncol = n + 1)
for (i in 1:(n + 1)) {
  for (j in 1:i) {
    M_zcb[j, i] = 100 * u ^ (i - j) * d ^ (j - 1)
  }
}

M_zcb[, n + 1] = pmax(100 * exp(-(T - n) * r), M_zcb[, n + 1])
for (j in (n + 1) : 1) {
  for (i in 1 : j - 1) {
      M_zcb[i, j - 1] = A_u * M_zcb[i, j] + A_d * M_zcb[i + 1, j]
  }
}


# iii price of bond at time 0
P_zcb = M_zcb[1,1]

# iv price of the convertion at tim 0
P_convert = M_zcb[1,1] - 100 * exp(-r * 365)



