library(tidyverse)

set.seed(nchar("tyranny of means") ^ 3)

n_val <- 200


dat <- 
crossing(
  var = c(5, 1),
  mean = c(0, 1)
) |> 
  mutate(y = map2(mean, var, \(x, y) rnorm(n = n_val, mean = x, sd = y))) |> 
  unnest(y)

dat <- 
dat |> 
  mutate(group = if_else(var == 5, "high", "low"))

dat |> 
  ggplot(aes(x = mean, y = y)) +
  geom_jitter(width = 0.05, alpha = 0.2) +
  facet_wrap(~ group)

dat |> 
  filter(group == "low") |> 
  lm(y ~ mean, data = _) |> 
  summary()

dat |> 
  filter(group == "high") |> 
  lm(y ~ mean, data = _) |> 
  summary()
