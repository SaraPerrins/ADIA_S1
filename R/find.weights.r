library(CVXR)

my_scale <- function(covariates, target_margins) {
  sigma <- sqrt(colMeans(covariates * covariates) - colMeans(covariates)^2)
  sigma[colMeans(covariates) < .05] <- sqrt(.05 * .95)
  sigma[colMeans(covariates) > .95] <- sqrt(.05 * .95)
  scale(covariates, center = target_margins, scale = sigma)
}


find_weights <- function(id, covariates
                         , lambda = 0.1
                         #' initial set of weights, sampling weights
                         , base.weights   = rep(1 / n, n)
                         , target.margins = colMeans(X)
                         ) {
  #https://web.stanford.edu/~boyd/papers/pdf/cvxr_paper.pdf
  #  see page 18  Kelly gambling
  n     <-  nrow(covariates)
  w     <-  Variable(n)
  x     <-  my_scale(covariates, target.margins)

  objtive       <- Minimize(
      sum((t(x) %*% w)^2) +
      lambda * (w * log(w / base.weights)) # Kullback (1959) entropy divergence
      )

  constraints   <- list(w >= 0, sum(w) == 1)

  problem       <- Problem(objtive, constraints = constraints)

  result        <- solve(problem)

  print(result$status)
  slw <- round(result$getValue(w), 4)
  return(slw)
  return(data.frame(id=id,wb=ebal$w))
}