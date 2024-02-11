rm(list=ls())

generate_sample <- function(n, mu, sigma){
  y <- rnorm(n, mu, sigma)
  data.frame(y=y)
}

generate_estimate <- function(dataset){
  y <- dataset[['y']]
  n <- dim(dataset)[1]
  sum((y-(sum(y)/n))^2)/(n-1)
}

generate_sample_and_estimate_N_times <- function(N, n, mu, sigma){
  result <- sapply(1:N, function(x) {
    dataset <- generate_sample(n, mu, sigma)
    generate_estimate(dataset)
  })
}


N <- 500
mu <- 5
sigma <- 100
n_list <- ceiling(seq(from=20, to=1000, length.out = 500))


point_estimates_list <- c()
variance_list <- c()
for (n in n_list){
  estimates <- generate_sample_and_estimate_N_times(N, n, mu, sigma)
  point_estimates_list <- append(point_estimates_list, mean(estimates))
  variance_list <- append(variance_list, var(estimates))
}

png(filename="sample_variance_point_estimate.png")
plot(n_list, point_estimates_list)
dev.off()

png(filename="sample_variance_variance.png")
plot(n_list, variance_list)
dev.off()






