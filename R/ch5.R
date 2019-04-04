
sample_size <- 10000
confidence_interval <- c(0.025, 0.975)

hours1=c(2.11,9.75,13.88,11.3,8.93,15.66,16.38,4.54,8.86,11.94,12.47,11.11,11.65,14.53,9.61,7.38,3.34,9.06,9.45,5.98,7.44,8.5,1.55,11.45,9.73)
# school1.dat=read.table("school1.dat") 
# attach(school1.dat)

mu_0 <- 5
sigma_0_sequare <- 4
kappa_0 <- 1
nu_0 <- 2

y_bar.1 <- mean(hours1)
y_var.1 <- var(hours1) 
y_sd.1 <- sd(hours1)
length.1 <- length(hours1)

kappa_n.1 <- kappa_0 + length.1
mu_n.1 <- (kappa_0 * mu_0 + length.1 * y_bar.1) / kappa_n.1 
nu_n.1 <- nu_0 + length.1 
sigma_n_square.1 <- (1 / nu_n.1) * (nu_0 * sigma_0_sequare + (length.1 - 1) * y_var.1 + ((kappa_0 * length.1) / kappa_n.1) * (y_bar.1 - mu_0)^2)
