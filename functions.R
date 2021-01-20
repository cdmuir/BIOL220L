# Custom function to generate sampling distribution of the sample mean from a numeric vector x with sample size n
sample_mean <- function(x, n, n_sim) {
  
  data.frame(
    sim = gl(n_sim, n),
    Y = sample(x, n * n_sim, replace = TRUE)
  ) %>%
    group_by(sim) %>%
    summarize(Ybar = mean(Y))
  
}


se_mean <- function(x) {
  sd(x) / sqrt(length(x))
}
