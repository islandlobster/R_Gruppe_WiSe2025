

# make_log_likelihoods + estimate (mit formalArgs und lfactorial)
make_log_likelihoods <- function(x) {
  N <- length(x)
  y <- round(x)
  max_y <- max(y)
  mean_y <- mean(y)
  max_x <- max(x)
  eps <- 1e-12
  
  # 1) Poisson: function(lambda)
  ll_poisson <- function(lambda) {
    # domain lambda > 0
    if (!(sign(lambda) == 1)) return(-Inf)
    -N * lambda + sum(y) * log(lambda) - sum(lfactorial(y))
  }
  interval_pois <- c(eps, max(1, max_y + 10))
  
  # 2) Binomial: choose n = max(y). function(p)
  n_binom <- max_y
  ll_binom <- function(p) {
    if (!(sign(p) == 1 && sign(1 - p) == 1)) return(-Inf)
    N * lfactorial(n_binom) - sum(lfactorial(y) + lfactorial(n_binom - y)) +
      sum(y) * log(p) + (N * n_binom - sum(y)) * log(1 - p)
  }
  interval_binom <- c(eps, 1 - eps)
  
  # 3) Normal: function(mu, sigma)
  const_norm <- - (N / 2) * log(2 * pi)
  ssq <- function(mu) sum((x - mu)^2)
  ll_normal <- function(mu, sigma) {
    if (!(is.finite(mu) && is.finite(sigma))) return(-Inf)
    if (!(sign(sigma) == 1)) return(-Inf)
    const_norm - N * log(sigma) - (1 / (2 * sigma^2)) * ssq(mu)
  }
  start_norm <- c(mean(x), sd(x))
  if (is.na(start_norm[2]) || start_norm[2] <= 0) start_norm[2] <- 1e-6
  
  # 4) Geometric: function(p) with y discrete
  sum_y <- sum(y)
  ll_geom <- function(p) {
    if (!(sign(p) == 1 && sign(1 - p) == 1)) return(-Inf)
    N * log(p) + sum_y * log(1 - p)
  }
  interval_geom <- c(eps, 1 - eps)
  
  # 5) Uniform on [0, theta]: function(theta)
  ll_unif_theta <- function(theta) {
    if (!is.finite(theta)) return(-Inf)
    if (theta < 0) return(-Inf)
    if (theta < max_x) return(-Inf)
    -N * log(theta)
  }
  interval_theta <- c(max_x, max_x + 10)
  
  out <- list(
    list("Poissonverteilung", ll_poisson, interval_pois),
    list("Binomialverteilung", ll_binom, interval_binom, n_binom),
    list("Normalverteilung", ll_normal, start_norm),
    list("Geometrische Verteilung", ll_geom, interval_geom),
    list("Gleichverteilung [0, theta]", ll_unif_theta, interval_theta)
  )
  names(out) <- c("Poisson", "Binomial", "Normal", "Geometrisch", "Gleichverteilung")
  out
}


estimate <- function(logLs) {
  out <- vector("list", length(logLs))
  names(out) <- names(logLs)
  
  for (i in seq_along(logLs)) {
    entry <- logLs[[i]]
    name <- entry[[1]]
    fn   <- entry[[2]]
    info <- entry[[3]]
    extra <- if (length(entry) >= 4) entry[[4]] else NULL
    
    args <- formalArgs(fn)  # character vector mit formalen Argumentnamen
    
    if (length(args) == 1) {
      # eindimensional: optimize
      interval <- info
      opt <- optimize(f = fn, interval = interval, maximum = TRUE)
      est <- setNames(opt$maximum, args[1])
      res <- list(name = name, estimate = est, optimize_result = opt)
      if (!is.null(extra)) res$extra <- extra
      out[[i]] <- res
    } else {
      # mehr als ein Argument -> benutze optim (minimiere -ll)
      start <- info
      if (!is.numeric(start) || length(start) < length(args)) {
        # wenn Startwerte nicht passend sind, erzeugen wir vernünftige Defaults
        if (name == "Normalverteilung") start <- c(mean = mean(x), sigma = sd(x))
        else start <- rep(1, length(args))
      }
      # wrapper, optimisiert über Vektor par
      fn_to_min <- function(par) {
        # benenne par nach args, rufe dann fn via do.call
        names(par) <- args
        val <- do.call(fn, as.list(par))
        if (!is.finite(val)) return(1e300)
        -val
      }
      # falls ein Parameter "sigma" heißt, setzen wir lower bound > 0
      lower <- rep(-Inf, length(start))
      upper <- rep( Inf, length(start))
      if ("sigma" %in% args) {
        idx <- which(args == "sigma")
        lower[idx] <- 1e-12
      }
      # verwende L-BFGS-B, die par-Länge ist klein
      opt <- optim(par = unname(start), fn = fn_to_min, method = "L-BFGS-B", lower = lower, upper = upper)
      par_hat <- setNames(opt$par, args)
      res <- list(name = name, estimate = par_hat, optim_result = opt)
      if (!is.null(extra)) res$extra <- extra
      out[[i]] <- res
    }
  }
  out
}

# ---------------------------
# Beispiel (kleines N zum Testen)
# ---------------------------
set.seed(1)
N <- 1e5    # zum testen; für finale Ausführung N <- 1e7
x_samp <- runif(N, min = 0, max = 10)
logLs <- make_log_likelihoods(x_samp)
res <- estimate(logLs)

# Beispiel-Ausgabe wie im Aufgabenblatt: Eintrag für Normalverteilung
print(res[[3]]$name)
print(res[[3]]$estimate)

# Weitere Kontrollen (analytische MLEs)
y_samp <- round(x_samp)
cat("Kontrolle analytisch:\n")
cat("Poisson lambda_hat =", mean(y_samp), "\n")
cat("Binomial n =", max(y_samp), "; p_hat =", mean(y_samp) / max(y_samp), "\n")
cat("Geometrisch p_hat =", 1 / (1 + mean(y_samp)), "\n")
cat("Gleichverteilung theta_hat =", max(x_samp), "\n")
cat("Normal mu_hat =", mean(x_samp), " sigma_hat =", sqrt(var(x_samp)), "\n")
