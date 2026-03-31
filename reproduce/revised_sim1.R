
make_plot1_new <- function(reps = 3){
  
  set.seed(1234)
  
  
  
  par(mfrow = c(4, 3), mar = c(4, 4.1, 3, 2.5), oma = c(0, 0, 0, 0))
  
  
  n = 1000
  ngrp = 100
  ntrue = 10
  nltype = "piecewise"
  sim_col = "nonparm"
  ctype = "AR"
  rho = 0
  pen = "grLasso"
  fam = "gaussian"
  
  r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                     rho = rho)
  X <- r$X
  y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, family = fam)
  grp <- r$grp
  
  fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = fam)
  ls <- seq(0.3*max(fit$lambda), min(fit$lambda), length.out = 100)
  
  rmat <- mmat <- matrix(NA, nrow = reps, ncol = length(ls))
  for(i in 1:reps){
    
    r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                       rho = rho)
    X <- r$X
    y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, family = fam)
    grp <- r$grp
    
    
    fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = fam, lambda = ls)
    
    ## Estimate marginal false discoveries
    mmat[i,] <- grpreg::mfdr(fit, X)$EF
    
    ## Get true FDR
    rmat[i,] = get_truth(fit, ntrue = ntrue)$fp
  }
  
  plot(ls, apply(mmat, 2, mean)*(1 - ntrue/ngrp), type = "l", lwd = 2, col = "blue",
       ylab = "Groups", xlab = "",
       main = "Linear", bty = "n")
  lines(ls, apply(rmat,2, mean), lwd = 2, col = "red")
  
  
  
  n = 1000
  ngrp = 100
  ntrue = 10
  nltype = "piecewise"
  sim_col = "nonparm"
  ctype = "AR"
  rho = 0
  pen = "grLasso"
  fam = "binomial"
  
  r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                     rho = rho)
  X <- r$X
  y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, family = fam)
  grp <- r$grp
  
  fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = fam)
  ls <- seq(1.05*max(fit$lambda), min(fit$lambda), length.out = 100)
  
  rmat <- mmat <- matrix(NA, nrow = reps, ncol = length(ls))
  for(i in 1:reps){
    
    r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                       rho = rho)
    X <- r$X
    y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, family = fam)
    grp <- r$grp
    
    
    fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = fam, lambda = ls)
    
    ## Estimate marginal false discoveries
    mmat[i,] <- grpreg::mfdr(fit, X)$EF
    
    ## Get true FDR
    rmat[i,] = get_truth(fit, ntrue = ntrue)$fp
  }
  
  plot(ls, apply(mmat, 2, mean)*(1 - ntrue/ngrp), type = "l", lwd = 2, col = "blue",
       ylab = "", xlab = "",
       main = "Logistic", bty = "n")
  lines(ls, apply(rmat,2, mean), lwd = 2, col = "red")
  
  n = 1000
  ngrp = 100
  ntrue = 10
  nltype = "piecewise"
  sim_col = "nonparm"
  ctype = "AR"
  rho = 0.0
  pen = "grLasso"
  fam = "survival"
  
  r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                     rho = rho)
  X <- r$X
  y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, family = fam, C = .9)
  grp <- r$grp
  
  fit <- grpsurv(X, y, group = grp, penalty = pen, returnX = TRUE)
  ls <- seq(1*max(fit$lambda), min(fit$lambda), length.out = 100)
  
  rmat <- mmat <- matrix(NA, nrow = reps, ncol = length(ls))
  for(i in 1:reps){
    
    r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                       rho = rho)
    X <- r$X
    y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, family = fam, C = .9)
    grp <- r$grp
    
    
    fit <- grpsurv(X, y, group = grp, penalty = pen, returnX = TRUE,  lambda = ls)
    
    ## Estimate marginal false discoveries
    mmat[i,] <- grpreg::mfdr(fit, X)$EF
    
    ## Get true FDR
    rmat[i,] = get_truth(fit, ntrue = ntrue)$fp
  }
  
  plot(ls, apply(mmat, 2, mean)*(1 - ntrue/ngrp), type = "l", lwd = 2, col = "blue",
       ylab = "", xlab = "",
       main = "Cox", bty = "n")
  lines(ls, apply(rmat,2, mean), lwd = 2, col = "red")
  mtext("Non-linear, Ind", side = 4, cex = 1, las = 3)
  
  ### NONLINEAR CORR
  n = 1000
  ngrp = 100
  ntrue = 10
  nltype = "piecewise"
  sim_col = "nonparm"
  ctype = "AR"
  rho = 0.8
  pen = "grLasso"
  fam = "gaussian"
  
  r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                     rho = rho)
  X <- r$X
  y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, family = fam)
  grp <- r$grp
  
  fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = fam)
  ls <- seq(0.067*max(fit$lambda), min(fit$lambda), length.out = 100)
  
  rmat <- mmat <- matrix(NA, nrow = reps, ncol = length(ls))
  for(i in 1:reps){
    
    r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                       rho = rho)
    X <- r$X
    y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, family = fam)
    grp <- r$grp
    
    
    fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = fam, lambda = ls)
    
    ## Estimate marginal false discoveries
    mmat[i,] <- grpreg::mfdr(fit, X)$EF
    
    ## Get true FDR
    rmat[i,] = get_truth(fit, ntrue = ntrue)$fp
  }
  
  plot(ls, apply(mmat, 2, mean)*(1 - ntrue/ngrp), type = "l", lwd = 2, col = "blue",  xlab = "",
       ylab = "Groups", bty = "n")
  lines(ls, apply(rmat,2, mean), lwd = 2, col = "red")
  
  
  n = 1000
  ngrp = 100
  ntrue = 10
  nltype = "piecewise"
  sim_col = "nonparm"
  ctype = "AR"
  rho = 0.8
  pen = "grLasso"
  fam = "binomial"
  
  r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                     rho = rho)
  X <- r$X
  y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, family = fam)
  grp <- r$grp
  
  fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = fam)
  ls <- seq(0.36*max(fit$lambda), min(fit$lambda), length.out = 100)
  
  rmat <- mmat <- matrix(NA, nrow = reps, ncol = length(ls))
  for(i in 1:reps){
    
    r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                       rho = rho)
    X <- r$X
    y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, family = fam)
    grp <- r$grp
    
    
    fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = fam, lambda = ls)
    
    ## Estimate marginal false discoveries
    mf <- grpreg::mfdr(fit, X)$EF
    mmat[i,] <- c(mf, rep(NA, length(ls) - length(mf)))
    
    ## Get true FDR
    fp <- get_truth(fit, ntrue = ntrue)$fp
    rmat[i,] = c(fp, rep(NA, length(ls) - length(fp)))
  }
  
  plot(ls, apply(mmat, 2, mean)*(1 - ntrue/ngrp),  xlab = "",  ylab = "", type = "l", lwd = 2, col = "blue", bty = "n")
  lines(ls, apply(rmat,2, mean), lwd = 2, col = "red")
  
  n = 1000
  ngrp = 100
  ntrue = 10
  nltype = "piecewise"
  sim_col = "nonparm"
  ctype = "AR"
  rho = 0.8
  pen = "grLasso"
  fam = "survival"
  
  r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                     rho = rho)
  X <- r$X
  y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, family = fam, C = .9)
  grp <- r$grp
  
  fit <- grpsurv(X, y, group = grp, penalty = pen, returnX = TRUE)
  ls <- seq(0.67*max(fit$lambda), min(fit$lambda), length.out = 100)
  
  rmat <- mmat <- matrix(NA, nrow = reps, ncol = length(ls))
  for(i in 1:reps){
    
    r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                       rho = rho)
    X <- r$X
    y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, family = fam, C = .9)
    grp <- r$grp
    
    
    fit <- grpsurv(X, y, group = grp, penalty = pen, returnX = TRUE,  lambda = ls)
    
    ## Estimate marginal false discoveries
    mmat[i,] <- grpreg::mfdr(fit, X)$EF
    
    ## Get true FDR
    rmat[i,] = get_truth(fit, ntrue = ntrue)$fp
  }
  
  plot(ls, apply(mmat, 2, mean)*(1 - ntrue/ngrp), type = "l", lwd = 2, col = "blue",
       ylab = "",  xlab = "", bty = "n")
  lines(ls, apply(rmat,2, mean), lwd = 2, col = "red")
  mtext("Non-linear, Corr", side = 4, cex = 1, las = 3)
  
  
  ### FACTOR GRP ROWS
  n = 1000
  ngrp = 100
  ntrue = 10
  nltype = "factor"
  ctype = "AR"
  rho = 0
  pen = "grLasso"
  fam = "gaussian"
  bb = 6
  k=3
  
  r <- genX_fac_grp(n = n, ngrp = ngrp,
                    k = k,
                    ctype = ctype, rho = rho)
  X <- r$X
  y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = bb/sqrt(n))
  grp <- r$grp
  
  fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = fam)
  ls <- seq(0.6*max(fit$lambda), min(fit$lambda), length.out = 100)
  
  rmat <- mmat <- matrix(NA, nrow = reps, ncol = length(ls))
  for(i in 1:reps){
    
    r <- genX_fac_grp(n = n, ngrp = ngrp,
                      k = k, 
                      ctype = ctype, rho = rho)
    X <- r$X
    y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = bb/sqrt(n))
    grp <- r$grp
    
    fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = fam, lambda = ls)
    
    ## Estimate marginal false discoveries
    mmat[i,] <- grpreg::mfdr(fit, X)$EF
    
    ## Get true FDR
    rmat[i,] = get_truth(fit, ntrue = ntrue)$fp
  }
  
  plot(ls, apply(mmat, 2, mean, na.rm=TRUE)*(1 - ntrue/ngrp), type = "l", lwd = 2, col = "blue",
       ylab = "Groups", xlab = "", bty = "n")
  lines(ls, apply(rmat,2, mean, na.rm=TRUE), lwd = 2, col = "red")
  
  n = 1000
  ngrp = 100
  ntrue = 10
  nltype = "factor"
  ctype = "AR"
  rho = 0
  pen = "grLasso"
  fam = "binomial"
  bb = 6
  k=3
  
  r <- genX_fac_grp(n = n, ngrp = ngrp,
                    k = k,
                    ctype = ctype, rho = rho)
  X <- r$X
  y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = bb/sqrt(n), family = fam)
  grp <- r$grp
  
  fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = fam)
  ls <- seq(1.2*max(fit$lambda), min(fit$lambda), length.out = 100)
  
  rmat <- mmat <- matrix(NA, nrow = reps, ncol = length(ls))
  for(i in 1:reps){
    
    r <- genX_fac_grp(n = n, ngrp = ngrp,
                      k = k,
                      ctype = ctype, rho = rho)
    X <- r$X
    y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = bb/sqrt(n), family = fam)
    grp <- r$grp
    
    fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = fam, lambda = ls)
    
    ## Estimate marginal false discoveries
    mmat[i,] <- grpreg::mfdr(fit, X)$EF
    
    ## Get true FDR
    rmat[i,] = get_truth(fit, ntrue = ntrue)$fp
  }
  
  plot(ls, apply(mmat, 2, mean, na.rm=TRUE)*(1 - ntrue/ngrp),  xlab = "",  ylab = "", type = "l", lwd = 2, col = "blue",  bty = "n")
  lines(ls, apply(rmat,2, mean, na.rm=TRUE), lwd = 2, col = "red")
  
  
  n = 1000
  ngrp = 100
  ntrue = 10
  nltype = "factor"
  ctype = "AR"
  rho = 0
  pen = "grLasso"
  fam = "survival"
  bb = 6
  k=3
  
  r <- genX_fac_grp(n = n, ngrp = ngrp,
                    k = k,
                    ctype = ctype, rho = rho)
  X <- r$X
  y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = bb/sqrt(n), family=fam, C=0.9)
  grp <- r$grp
  
  fit <- grpsurv(X, y, group = grp, penalty = pen, returnX = TRUE)
  ls <- seq(0.9*max(fit$lambda), min(fit$lambda), length.out = 100)
  
  rmat <- mmat <- matrix(NA, nrow = reps, ncol = length(ls))
  for(i in 1:reps){
    
    r <- genX_fac_grp(n = n, ngrp = ngrp,
                      k = k,
                      ctype = ctype, rho = rho)
    X <- r$X
    y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = bb/sqrt(n), family=fam, C=0.9)
    grp <- r$grp
    
    
    fit <- grpsurv(X, y, group = grp, penalty = pen, returnX = TRUE,  lambda = ls)
    
    ## Estimate marginal false discoveries
    mmat[i,] <- grpreg::mfdr(fit, X)$EF
    
    ## Get true FDR
    rmat[i,] = get_truth(fit, ntrue = ntrue)$fp
  }
  
  plot(ls, apply(mmat, 2, mean)*(1 - ntrue/ngrp), type = "l", lwd = 2, col = "blue",
       ylab = "",  xlab = "", bty = "n")
  lines(ls, apply(rmat,2, mean), lwd = 2, col = "red")
  mtext("Factor, Ind", side = 4, cex = 1, las = 3)
  
  ## CORR FACTOR GRPS
  n = 1000
  ngrp = 100
  ntrue = 10
  nltype = "factor"
  ctype = "AR"
  rho = 0.9
  pen = "grLasso"
  fam = "gaussian"
  bb = 6
  k=3
  
  r <- genX_fac_grp(n = n, ngrp = ngrp,
                    k = k,
                    ctype = ctype, rho = rho)
  X <- r$X
  y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = bb/sqrt(n))
  grp <- r$grp
  
  fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = fam)
  ls <- seq(0.6*max(fit$lambda), min(fit$lambda), length.out = 100)
  
  rmat <- mmat <- matrix(NA, nrow = reps, ncol = length(ls))
  for(i in 1:reps){
    
    r <- genX_fac_grp(n = n, ngrp = ngrp,
                      k = k, type = "correlated",
                      ctype = ctype, rho = rho)
    X <- r$X
    y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = bb/sqrt(n))
    grp <- r$grp
    
    fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = fam, lambda = ls)
    
    ## Estimate marginal false discoveries
    mmat[i,] <- grpreg::mfdr(fit, X)$EF
    
    ## Get true FDR
    rmat[i,] = get_truth(fit, ntrue = ntrue)$fp
  }
  
  plot(ls, apply(mmat, 2, mean, na.rm=TRUE)*(1 - ntrue/ngrp), type = "l", lwd = 2, col = "blue",
       ylab = "Groups",
       xlab = expression(lambda), bty = "n")
  lines(ls, apply(rmat,2, mean, na.rm=TRUE), lwd = 2, col = "red")
  
  n = 1000
  ngrp = 100
  ntrue = 10
  nltype = "factor"
  ctype = "AR"
  rho = 0.9
  pen = "grLasso"
  fam = "binomial"
  bb = 6
  k=3
  
  r <- genX_fac_grp(n = n, ngrp = ngrp,
                    k = k,
                    ctype = ctype, rho = rho)
  X <- r$X
  y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = bb/sqrt(n), family = fam)
  grp <- r$grp
  
  fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = fam)
  ls <- seq(0.9*max(fit$lambda), min(fit$lambda), length.out = 100)
  
  rmat <- mmat <- matrix(NA, nrow = reps, ncol = length(ls))
  for(i in 1:reps){
    
    r <- genX_fac_grp(n = n, ngrp = ngrp,
                      k = k, type = "correlated",
                      ctype = ctype, rho = rho)
    X <- r$X
    y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = bb/sqrt(n), family = fam)
    grp <- r$grp
    
    fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = fam, lambda = ls)
    
    ## Estimate marginal false discoveries
    mmat[i,] <- grpreg::mfdr(fit, X)$EF
    
    ## Get true FDR
    rmat[i,] = get_truth(fit, ntrue = ntrue)$fp
  }
  
  plot(ls, apply(mmat, 2, mean, na.rm=TRUE)*(1 - ntrue/ngrp), type = "l", lwd = 2, col = "blue",
       xlab = expression(lambda), ylab = "", bty = "n")
  lines(ls, apply(rmat,2, mean, na.rm=TRUE), lwd = 2, col = "red")
  
  
  n = 1000
  ngrp = 100
  ntrue = 10
  nltype = "factor"
  ctype = "AR"
  rho = 0.9
  pen = "grLasso"
  fam = "survival"
  bb = 6
  k=3
  
  r <- genX_fac_grp(n = n, ngrp = ngrp,
                    k = k, type = "correlated",
                    ctype = ctype, rho = rho)
  X <- r$X
  y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = bb/sqrt(n), family=fam, C=0.9)
  grp <- r$grp
  
  fit <- grpsurv(X, y, group = grp, penalty = pen, returnX = TRUE)
  ls <- seq(0.7*max(fit$lambda), min(fit$lambda), length.out = 100)
  
  rmat <- mmat <- matrix(NA, nrow = reps, ncol = length(ls))
  for(i in 1:reps){
    
    r <- genX_fac_grp(n = n, ngrp = ngrp,
                      k = k,
                      ctype = ctype, rho = rho)
    X <- r$X
    y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = bb/sqrt(n), family=fam, C=0.9)
    grp <- r$grp
    
    
    fit <- grpsurv(X, y, group = grp, penalty = pen, returnX = TRUE,  lambda = ls)
    
    ## Estimate marginal false discoveries
    mmat[i,] <- grpreg::mfdr(fit, X)$EF
    
    ## Get true FDR
    rmat[i,] = get_truth(fit, ntrue = ntrue)$fp
  }
  
  plot(ls, apply(mmat, 2, mean)*(1 - ntrue/ngrp), type = "l", lwd = 2, col = "blue",
       ylab = "",
       xlab = expression(lambda), bty = "n")
  lines(ls, apply(rmat,2, mean), lwd = 2, col = "red")
  mtext("Factor, Corr", side = 4, cex = 1, las = 3)
  
  
  
 # par(mai=c(0,0,0,0))
 # plot.new()
 # legend("center", legend = c("Expected False Selections (mFDR)", "Empirical False Sections"), 
 #        lwd = 2, col = c("blue", "red"), bty = "n", ncol = 2)
}

make_plot1_new()
