#' ---
#' title: Hadley Wickham's R for data science chapter 23
#' author: Nicolas Delhomme
#' date: "`r Sys.Date()`"
#' output:
#'  html_document:
#'    toc: true
#'    number_sections: true
#' ---
#' # 23.1

suppressPackageStartupMessages({
  library(tidyverse)
  library(modelr)
  options(na.action = na.warn)
})

#' # 23.2
#' Take a first look at the data
ggplot(sim1, aes(x, y)) + 
  geom_point()

#' The data looks linear, let's try to model it
models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

#' we get 250 models, some good, some bad
ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point() 

#' write the model as a function
model1 <- function(a, data) {
  a[1] + data$x * a[2]
}

#' Select a model as an example
model1(c(7, 1.5), sim1)

#' Create a function to calculate the residuals
measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}

# calculate it for our example
measure_distance(c(7, 1.5), sim1)

# create another function to calculate the distance for any a1 and a2
sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

#' Calculate the distance foe all models
models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models

#' Visualise the ten best models
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )

#' Visualise the data differently, look at the parameters, colored by the distance
ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

#' Use a less random approach, use a rectangular grid for the parameters
grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

#' plot the grid
grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 

#' These models are all a better fit than the previous approach
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )

#' Instead a manual approach, use functions to optimise
best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

#' plot that best fit
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])

#' The fit looks good
#' What we  did is the same as running the lm function
sim1_mod <- lm(y ~ x, data = sim1)

#' Yay! we reproduce the lm function!
coef(sim1_mod)
summary(sim1_mod)

#' ## Exercises
#' ### 23.2.1
#' One downside of the linear model is that it is sensitive to unusual values 
#' because the distance incorporates a squared term. Fit a linear model to the 
#' simulated data below, and visualise the results. Rerun a few times to 
#' generate different simulated datasets. What do you notice about the model?
#' 
#' Let's generate values drown from the t distribution (for the intersect in y)
sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)

#' plot the data
ggplot(sim1a, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#' Create a new funtion to generate the data 
simt <- function(i) {
  tibble(
    x = rep(1:10, each = 3),
    y = x * 1.5 + 6 + rt(length(x), df = 2),
    .id = i
  )
}

#' Generate 12 simulations
sims <- map_df(1:12, simt)

#' Plot them 
ggplot(sims, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "red") +
  facet_wrap(~.id, ncol = 4)

sim_norm <- function(i) {
  tibble(
    x = rep(1:10, each = 3),
    y = x * 1.5 + 6 + rnorm(length(x)),
    .id = i
  )
}

simdf_norm <- map_df(1:12, sim_norm)

ggplot(simdf_norm, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "red") +
  facet_wrap(~.id, ncol = 4)

tibble(
  x = seq(-5, 5, length.out = 100),
  normal = dnorm(x),
  student_t = dt(x, df = 2)
) %>%
  pivot_longer(-x, names_to="distribution", values_to="density") %>%
  ggplot(aes(x = x, y = density, colour = distribution)) +
  geom_line()

pnorm(2, lower.tail = FALSE)

pt(2, df=2, lower.tail = FALSE)

#' 23.2.2 One way to make linear models more robust is to use a different distance 
#' measure. For example, instead of root-mean-squared distance, you could use 
#' mean-absolute distance:

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  mean(abs(diff))
}

#' Use optim() to fit this model to the simulated data above and compare it to 
#' the linear model.

make_prediction <- function(mod, data) {
  mod[1] + mod[2] * data$x
}

#' optimise the model using the mean-absolute distance
best <- optim(c(0, 0), measure_distance, data = sim1a)
best$par

#' do the same for the root-mean-squared distance
measure_distance_ls <- function(mod, data) {
  diff <- data$y - (mod[1] + mod[2] * data$x)
  sqrt(mean(diff^2))
}

#' optimise it
#' 
#' We can observe that the former is more conservative / more robust to outliers
best <- optim(c(0, 0), measure_distance_ls, data = sim1a)
best$par

#' ### 2.23.3
# One challenge with performing numerical optimisation is that it’s only 
# guaranteed to find one local optimum. What’s the problem with optimising a 
# three parameter model like this?

model3 <- function(a, data) {
  a[1] + data$x * a[2] + a[3]
}

#' a function to calculate the distance
measure_distance_3 <- function(a, data) {
  diff <- data$y - model3(a, data)
  sqrt(mean(diff^2))
}

#' optimise the model, initialising with different values
best3a <- optim(c(0, 0, 0), measure_distance_3, data = sim1)
best3a$par

best3b <- optim(c(0, 0, 1), measure_distance_3, data = sim1)
best3b$par

best3c <- optim(c(0, 0, 5), measure_distance_3, data = sim1)
best3c$par

#' ## 23.3
#' create a parameter
grid <- sim1 %>% 
  data_grid(x) 
grid

#' add the prediction
grid <- grid %>% 
  add_predictions(sim1_mod) 
grid

#' visualise it
ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)

#' add the residuals
sim1 <- sim1 %>% 
  add_residuals(sim1_mod)
sim1

#' visualise as a polygon
ggplot(sim1, aes(resid)) + 
  geom_freqpoly(binwidth = 0.5)

#' visualise as a function
ggplot(sim1, aes(x, resid)) + 
  geom_ref_line(h = 0) +
  geom_point() 

#' # Exercises
# 23.3.1. Instead of using lm() to fit a straight line, you can use loess() to fit a 
# smooth curve. Repeat the process of model fitting, grid generation, 
# predictions, and visualisation on sim1 using loess() instead of lm(). 
# How does the result compare to geom_smooth()?

sim1_loess <- loess(y ~ x, data = sim1)
sim1_lm <- lm(y ~ x, data = sim1)

#' grid search for the loess model
grid_loess <- sim1 %>%
  add_predictions(sim1_loess)

sim1 <- sim1 %>%
  add_residuals(sim1_lm) %>%
  add_predictions(sim1_lm) %>%
  add_residuals(sim1_loess, var = "resid_loess") %>%
  add_predictions(sim1_loess, var = "pred_loess")

plot_sim1_loess <-
  ggplot(sim1, aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(x = x, y = pred), data = grid_loess, colour = "red")
plot_sim1_loess

plot_sim1_loess +
  geom_smooth(method = "loess", colour = "blue", se = FALSE, alpha = 0.20)

ggplot(sim1, aes(x = x)) +
  geom_ref_line(h = 0) +
  geom_point(aes(y = resid)) +
  geom_point(aes(y = resid_loess), colour = "red")

# 23.3.2. add_predictions() is paired with gather_predictions() and 
# spread_predictions(). How do these three functions differ?
grid %>%
  add_predictions(sim1_mod, var = "pred_lm") %>%
  add_predictions(sim1_loess, var = "pred_loess")

grid %>%
  gather_predictions(sim1_mod, sim1_loess)

grid %>%
  spread_predictions(sim1_mod, sim1_loess)

grid %>%
  gather_predictions(sim1_mod, sim1_loess) %>% 
  spread(model,pred)


# 23.3.3  What does geom_ref_line() do? What package does it come from? 
# Why is displaying a reference line in plots showing residuals useful and 
# important?
  
# 23.3.4  Why might you want to look at a frequency polygon of absolute 
# residuals? What are the pros and cons compared to looking at the raw residuals?
ggplot(sim1, aes(x = abs(resid))) +
  geom_freqpoly(binwidth = 0.5)

# 23.4
df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
)

model_matrix(df, y ~ x1)

model_matrix(df, y ~ x1 - 1)

model_matrix(df, y ~ 0 + x1)

model_matrix(df, y ~ x1 + x2)

df <- tribble(
  ~ sex, ~ response,
  "male", 1,
  "female", 2,
  "male", 1
)

model_matrix(df, response ~ sex)

ggplot(sim2) + 
  geom_point(aes(x, y))

mod2 <- lm(y ~ x, data = sim2)

grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)
grid

ggplot(sim2, aes(x)) + 
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)

tibble(x = "e") %>% 
  add_predictions(mod2)

ggplot(sim3, aes(x1, y)) + 
  geom_point(aes(colour = x2))

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

grid <- sim3 %>% 
  data_grid(x1, x2) %>% 
  gather_predictions(mod1, mod2)
grid

ggplot(sim3, aes(x1, y, colour = x2)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model)

sim3 <- sim3 %>% 
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, colour = x2)) + 
  geom_point() + 
  facet_grid(model ~ x2)

mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5), 
    x2 = seq_range(x2, 5) 
  ) %>% 
  gather_predictions(mod1, mod2)
grid

seq_range(c(0.0123, 0.923423), n = 5)

seq_range(c(0.0123, 0.923423), n = 5, pretty = TRUE)

x1 <- rcauchy(100)
seq_range(x1, n = 5)
seq_range(x1, n = 5, trim = 0.10)
seq_range(x1, n = 5, trim = 0.25)
seq_range(x1, n = 5, trim = 0.50)
x2 <- c(0, 1)

seq_range(x2, n = 5)
seq_range(x2, n = 5, expand = 0.10)
seq_range(x2, n = 5, expand = 0.25)
seq_range(x2, n = 5, expand = 0.50)

ggplot(grid, aes(x1, x2)) + 
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~ model)

ggplot(grid, aes(x1, pred, colour = x2, group = x2)) + 
  geom_line() +
  facet_wrap(~ model)

ggplot(grid, aes(x2, pred, colour = x1, group = x1)) + 
  geom_line() +
  facet_wrap(~ model)

df <- tribble(
  ~y, ~x,
  1,  1,
  2,  2, 
  3,  3
)
model_matrix(df, y ~ x^2 + x)

model_matrix(df, y ~ I(x^2) + x)

model_matrix(df, y ~ poly(x, 2))

library(splines)
model_matrix(df, y ~ ns(x, 2))

sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)

ggplot(sim5, aes(x, y)) +
  geom_point()

mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) + 
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~ model)

# Exercises

# 23.4.1 What happens if you repeat the analysis of sim2 using a model without 
# an intercept. What happens to the model equation? What happens to the 
# predictions?

mod2a <- lm(y ~ x - 1, data = sim2)
mod2 <- lm(y ~ x, data = sim2)

grid <- sim2 %>%
  data_grid(x) %>%
  spread_predictions(mod2, mod2a)
grid

   
# 23.4.2 Use model_matrix() to explore the equations generated for the models 
# I fit to sim3 and sim4. Why is * a good shorthand for interaction?

x3 <- model_matrix(y ~ x1 * x2, data = sim3)
x3

x4 <- model_matrix(y ~ x1 * x2, data = sim4)
x4

all(x4[["x1"]] * x4[["x2"]] == x4[["x1:x2"]])
  


# 23.4.3  Using the basic principles, convert the formulas in the following two 
# models into functions. (Hint: start by converting the categorical variable 
# into 0-1 variables.)
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

levels(sim3$x2)

model_matrix_mod1 <- function(.data) {
  mutate(.data,
         x2b = as.numeric(x2 == "b"),
         x2c = as.numeric(x2 == "c"),
         x2d = as.numeric(x2 == "d"),
         `(Intercept)` = 1
  ) %>%
    select(`(Intercept)`, x1, x2b, x2c, x2d)
}

model_matrix_mod1(sim3)

# For sim4, which of mod1 and mod2 is better? I think mod2 does a slightly 
# better job at removing patterns, but it’s pretty subtle. Can you come up with 
# a plot to support my claim?

mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

sim4_mods <- gather_residuals(sim4, mod1, mod2)


ggplot(sim4_mods, aes(x = resid, colour = model)) +
  geom_freqpoly(binwidth = 0.5) +
  geom_rug()

ggplot(sim4_mods, aes(x = abs(resid), colour = model)) +
  geom_freqpoly(binwidth = 0.5) +
  geom_rug()

sim4_mods %>%
  group_by(model) %>%
  summarise(resid = sd(resid))


# 23.5

df <- tribble(
  ~x, ~y,
  1, 2.2,
  2, NA,
  3, 3.5,
  4, 8.3,
  NA, 10
)

mod <- lm(y ~ x, data = df)

mod <- lm(y ~ x, data = df, na.action = na.exclude)

nobs(mod)
