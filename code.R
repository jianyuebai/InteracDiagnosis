set.seed(123)

### 1) PERFECT / QUASI SEPARATION (binomial)
make_separation <- function(n = 200) {
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  # Perfect rule: y = 1 if x1 + x2 > 0
  y <- as.integer(x1 + x2 > 0)
  data.frame(y, x1, x2)
}
# glm(y ~ x1 + x2, data = make_separation(), family = binomial())

### 2) MULTICOLLINEARITY (high correlation)
make_multicollinearity <- function(n = 500, rho = -0.99,
                                   beta0 = -0.5, beta1 = 1.5, beta2 = -1.2) {
  # x1 ~ N(0,1); construct x2 with Corr(x1, x2) = rho (not extreme by default)
  x1 <- rnorm(n)
  z  <- rnorm(n)
  x2 <- rho * x1 + sqrt(1 - rho^2) * z  # Corr(x1, x2) ≈ rho
  
  # Logistic model with moderate effects (avoids separation)
  eta <- beta0 + beta1 * x1 + beta2 * x2
  p   <- 1 / (1 + exp(-eta))
  y   <- rbinom(n, 1, p)
  
  data.frame(y, x1, x2)
}
# glm(y ~ x1 + x2, data = make_multicollinearity(), family = binomial())

### 3) SCALING / NUMERICAL INSTABILITY
make_scaling_issue <- function(n = 500) {
  x1 <- rnorm(n, sd = 1e6)   # huge scale
  x2 <- rnorm(n, sd = 1)     # small scale
  eta <- -1 + 1e-6*x1 + 0.5*x2
  p <- 1 / (1 + exp(-eta))
  y <- rbinom(n, 1, p)
  data.frame(y, x1, x2)
}
# glm(y ~ x1 + x2, data = make_scaling_issue(), family = binomial())

### 4) SPARSE CATEGORIES / RARE LEVELS
make_sparse_levels <- function(n_group = 8, per = 20) {
  g <- factor(rep(paste0("L", 1:n_group), times = c(rep(per, n_group - 1), 2)))
  x1 <- rnorm(length(g))
  x2 <- rnorm(length(g))
  eta <- -1 + 0.7*x1 + 0.4*x2 + 3*(g == "L8")  # rare level mostly successes
  p <- 1 / (1 + exp(-eta))
  y <- rbinom(length(g), 1, p)
  data.frame(y, x1, x2, g)
}
# glm(y ~ x1 + x2 + g, data = make_sparse_levels(), family = binomial())

### 5) OVERFLOW IN exp() (“fitted probs 0 or 1”)
make_overflow <- function(n = 300) {
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eta <- 25*x1 + 25*x2     # extremely large linear predictor
  p <- 1 / (1 + exp(-eta))
  y <- rbinom(n, 1, p)
  data.frame(y, x1, x2)
}
# glm(y ~ x1 + x2, data = make_overflow(), family = binomial())

### 6) MIS-SPECIFIED MODELS
# (a) Overdispersed counts fitted by Poisson
make_overdispersed_counts <- function(n = 500) {
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  mu <- exp(0.5 + 0.4*x1 + 0.6*x2)
  phi <- 2
  lambda <- rgamma(n, shape = phi, scale = mu / phi)
  y <- rpois(n, lambda)
  data.frame(y, x1, x2)
}
# glm(y ~ x1 + x2, data = make_overdispersed_counts(), family = poisson())

# (b) Binomial with identity link (misspecified)
make_binomial_identity_misspec <- function(n = 400) {
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eta_true <- -0.5 + 1*x1 + 0.5*x2
  p <- 1 / (1 + exp(-eta_true))
  y <- rbinom(n, 1, p)
  data.frame(y, x1, x2)
}
# glm(y ~ x1 + x2, data = make_binomial_identity_misspec(),
#     family = binomial(link = "identity"))

### 7) OUTLIERS / HIGH LEVERAGE
make_outliers_leverage <- function(n = 300) {
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eta <- -1 + 2*x1 + 1.5*x2
  p <- 1 / (1 + exp(-eta))
  y <- rbinom(n, 1, p)
  # Add extreme leverage points with discordant outcomes
  x1_out <- c(8, 9, 10, -8, -9)
  x2_out <- c(8, 9, 10, -8, -9)
  y_out <- c(0, 0, 0, 1, 1)
  data.frame(
    y = c(y, y_out),
    x1 = c(x1, x1_out),
    x2 = c(x2, x2_out)
  )
}
# glm(y ~ x1 + x2, data = make_outliers_leverage(), family = binomial())

### Build dataset list
ds <- list(
  separation = make_separation(),
  multicollinearity = make_multicollinearity(),
  scaling = make_scaling_issue(),
  sparse_levels = make_sparse_levels(),
  overflow = make_overflow(),
  misspec_overdisp = make_overdispersed_counts(),
  misspec_identity = make_binomial_identity_misspec(),
  outliers = make_outliers_leverage()
)

str(ds, max.level = 1)



# Perfect Separation
separation = ds[['separation']]
# Plot
plot(
  separation$x1, separation$x2,
  col = ifelse(separation$y == 1, "blue", "red"),
  pch = 19,
  main = "Perfect Separation Example",
  xlab = "Predictor x",
  ylab = "Outcome y (0/1)"
)

m <- glm(y ~ x1 + x2, data = separation, family = binomial())
coefs <- coef(m)
alpha <- coefs[1]
beta1 <- coefs[2]
beta2 <- coefs[3]
# Existing scatterplot
plot(
  separation$x1, separation$x2,
  col = ifelse(separation$y == 1, "blue", "red"),
  pch = 19,
  main = "Perfect Separation Example",
  xlab = "Predictor x1",
  ylab = "Predictor x2"
)

# Fit logistic model
m <- glm(y ~ x1 + x2, data = separation, family = binomial())
coefs <- coef(m)

# Compute decision boundary
abline(a = -coefs[1]/coefs[3], b = -coefs[2]/coefs[3], lwd = 2, col = "darkgray")
legend("topleft", legend = c("y = 1", "y = 0", "p = 0.5 boundary"),
       col = c("blue", "red", "darkgray"), pch = c(19,19,NA),
       lty = c(NA, NA, 1), lwd = c(NA, NA, 2), bty = "n")

summary(m)


mod_sep <- glm(y ~ x, data = separation, family = binomial())

# View summary
summary(mod_sep)


# Colinearity
colinearity = ds[['multicollinearity']]
# Plot
plot(
  colinearity$x1, colinearity$x2,
  col = ifelse(colinearity$y == 1, "blue", "red"),
  pch = 19,
  main = "Colinearity Example",
  xlab = "Predictor x1",
  ylab = "Predictor x2"
)

mod_colinearity <- glm(y ~ x1+x2+1, data = colinearity, family = binomial())
summary(mod_colinearity)



# Plot data points
plot(colinearity$x1, colinearity$x2,
     col = ifelse(colinearity$y == 1, "blue", "red"),
     pch = 19,
     xlab = "x1", ylab = "x2",
     main = "Logistic Regression Decision Boundary")

# Fit logistic regression model
m <- glm(y ~ x1 + x2, data = colinearity, family = binomial())

# Extract coefficients
coefs <- coef(m)
alpha <- coefs[1]
beta1 <- coefs[2]
beta2 <- coefs[3]

# Add decision boundary (p = 0.5 line)
abline(a = -alpha/beta2, b = -beta1/beta2, col = "darkgray", lwd = 2)

# Add legend
legend("topleft",
       legend = c("y = 1", "y = 0", "p = 0.5 boundary"),
       col = c("blue", "red", "darkgray"),
       pch = c(19, 19, NA),
       lty = c(NA, NA, 1),
       lwd = c(NA, NA, 2),
       bty = "n")

cor(colinearity$x1, colinearity$x2)         # e.g. -0.7
kappa(model.matrix(m))                      # condition number; >30 = some concern
vif_values <- car::vif(m)                   # Variance Inflation Factor
vif_values


m <- lm(y ~ x1, data = colinearity)
summary(m)


