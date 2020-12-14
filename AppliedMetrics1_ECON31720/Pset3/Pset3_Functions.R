


# 1. For each u recover E(Y(d) | U=u, X=x) (see Heckman Pset 4)
# What is the difference between p / ud
pL = 0.0
pH = 1.0
bins = 5

# Creates pscore domain
p_values = linspace(pL, pH, bins+1)
valuesY1 = zeros(bins+1, 1)
valuesY0 = zeros(bins+1, 1)
ME = zeros(bins+1, 1)

# Get binned results (non parametric MTR)
for (i in 1:(bins+1)){
  pv = p_values[i]
  indexes = ((pv - 1/(2*bins) <= p) & (p <= pv + 1/(2*bins)))[,1]
  df = data[indexes,]
  df1 = df[df$more2kids==1,]
  df0 = df[df$more2kids==0,]
  valuesY1[i,] = mean(df1$worked, na.rm=T)
  valuesY0[i,] = mean(df0$worked, na.rm=T)
  ME[i,] = valuesY1[i,] - valuesY0[i,]
}

ggplot() +
  geom_line(aes(x=p_values, y=ME))


# From E(Y(1) | U <= p) = a1 + b1/2p
# to E(Y(1) | U = u) = a1 + b1 u 
# ie = b1/2 = b1^ => b1 = 2*b1^ 
# b1/2



### TSLS
tsls_results <- tsls_own(data="NA", dep_var = y_data, indep_var = x_data, 
                         treat_var = d_data, instru_var = z_data,
                         robust = FALSE)
