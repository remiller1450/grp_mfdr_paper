###############################################################################
###
###      Function to make Figure 1
###
###############################################################################


create_fig1 <- function(){
  
  ### ROW 1 - low p
  
  ## panel 1,1 - % error = 6.9%
  n200_grp40 = get_plot_for_fig1(reps = 100, n = 200, ngrp = 40, ntrue = 2, lmin = 0,
                                 nltype = "piecewise", sim_col = "nonparm",
                                 ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian",
                                 lam_max_mult = 0.6)$plot + ggtitle("n = 200")
  
  ## panel 1,2 - % error = 3.0%
  n400_grp40 = get_plot_for_fig1(reps = 100, n = 400,  ngrp = 40, ntrue = 2,  lmin = 0,
                                 nltype = "piecewise", sim_col = "nonparm",
                                 ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian",
                                 lam_max_mult = 0.6)$plot + ggtitle("n = 400")
  
  ## panel 1,3 - % error = 0.05
  n800_grp40 = get_plot_for_fig1(reps = 100, n = 800,  ngrp = 40, ntrue = 2,  lmin = 0,
                                 nltype = "piecewise", sim_col = "nonparm",
                                 ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian",
                                 lam_max_mult = 0.6)$plot + ggtitle("n = 800")
  
  
  # grid.arrange(n200_grp40, n400_grp40, n800_grp40)
  
  
  ## ROW 2 - high p
  
  ## panel 2,1 
  n200_grp80 = get_plot_for_fig1(reps = 100, n = 200, ngrp = 80, ntrue = 4, lmin = 0.001,
                                 nltype = "piecewise", sim_col = "nonparm",
                                 ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian",
                                 lam_max_mult = 0.6)$plot
  
  
  ## panel 2,2
  n400_grp80  = get_plot_for_fig1(reps = 100, n = 400, ngrp = 80, ntrue = 4, lmin = 0.001,
                                  nltype = "piecewise", sim_col = "nonparm",
                                  ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian",
                                  lam_max_mult = 0.6)$plot
  
  
  ## panel 2,3
  n800_grp80  = get_plot_for_fig1(reps = 100, n = 800, ngrp = 80, ntrue = 4, lmin = 0.001,
                                  nltype = "piecewise", sim_col = "nonparm",
                                  ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian",
                                  lam_max_mult = 0.6)$plot 
  
  
  # grid.arrange(n200_grp40, n400_grp40, n800_grp40,
  #              n200_grp80, n400_grp80, n800_grp80,
  #              nrow = 2)
  
  
  
  
  ## ROW 3 - highest p
  
  ## panel 3,1 
  n200_grp200 = get_plot_for_fig1(reps = 100, n = 200, ngrp = 200, ntrue = 10, lmin = 0.00001,
                                  nltype = "piecewise", sim_col = "nonparm",
                                  ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian",
                                  lam_max_mult = 0.6)$plot 
  
  
  ## panel 3,2
  n400_grp200  =  get_plot_for_fig1(reps = 100, n = 400, ngrp = 200, ntrue = 10, lmin = 0.00001,
                                    nltype = "piecewise", sim_col = "nonparm",
                                    ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian",
                                    lam_max_mult = 0.6)$plot
  
  
  
  ## panel 3,3
  n800_grp200  =  get_plot_for_fig1(reps = 100, n = 800, ngrp = 200, ntrue = 10, lmin = 0.00001,
                                    nltype = "piecewise", sim_col = "nonparm",
                                    ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian",
                                    lam_max_mult = 0.6)$plot
  
  
  grid.arrange(n200_grp40 + labs(y = "Groups (of 40)"), n400_grp40, n800_grp40,
               n200_grp80 + labs(y = "Groups (of 80)"), n400_grp80, n800_grp80,
               n200_grp200 + labs(y = "Groups (of 200)"), n400_grp200, n800_grp200,
               nrow = 3)
  
  
  ## Hacky way to get a common legend
  dummy_df <- data.frame(x = 1, y = 1, Group = c("Estimated False Positives", "Empirical False Positives"))
  
  dummy_plot <- ggplot(dummy_df, aes(x, y, color = Group)) +
    geom_line() + geom_point() + scale_color_manual(values = c("blue","red")) +
    guides(color = guide_legend(nrow = 1)) + theme_minimal() + labs(color = "") +
    theme(legend.direction = "horizontal",legend.box = "horizontal",legend.position = "bottom")
  

  
  my_legend <- get_legend(dummy_plot)
  
  
  final_fig1 = grid.arrange(n200_grp40 + labs(y = "Groups (of 40)"), n400_grp40, n800_grp40,
                            n200_grp80 + labs(y = "Groups (of 80)"), n400_grp80, n800_grp80,
                            n200_grp200 + labs(x= expression(lambda), y = "Groups (of 200)"), 
                            n400_grp200 + labs(x= expression(lambda)), n800_grp200 + labs(x= expression(lambda)),
                            my_legend,
                            layout_matrix = rbind(c(1, 2, 3),
                                                  c(4, 5, 6),
                                                  c(7, 8, 9),
                                                  c(10, 10, 10)),
                            heights = c(10,10,10,1))
  
  return(final_fig1)
}

################################################################################
###
###   Function to make Figure 2
###
################################################################################

create_fig2 <- function(reps = 100){
  
  set.seed(1234)
  
  n = 1000
  ngrp = 80
  ntrue = 4
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
  ls <- seq(0.65*max(fit$lambda), min(fit$lambda), length.out = 100)
  
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
  
  plot_df_lr <- data.frame(
    ls   = ls,
    expected = apply(mmat, 2, mean),
    actual  = apply(rmat, 2, mean)
  )
  
  diff_auc_lr = AUC(plot_df_lr$ls, plot_df_lr$expected)/AUC(plot_df_lr$ls, plot_df_lr$actual)
  
  
  ## Logistic Reg
  
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
  
  plot_df_log <- data.frame(
    ls   = ls,
    expected = apply(mmat, 2, mean),
    actual  = apply(rmat, 2, mean)
  )
  
  
  diff_auc_log = AUC(plot_df_log$ls, plot_df_log$expected)/AUC(plot_df_log$ls, plot_df_log$actual)
  
  
  ### Cox Reg
  
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
  
  plot_df_surv <- data.frame(
    ls   = ls,
    expected = apply(mmat, 2, mean),
    actual  = apply(rmat, 2, mean)
  )
  
  
  diff_auc_surv = AUC(plot_df_surv$ls, plot_df_surv$expected)/AUC(plot_df_surv$ls, plot_df_surv$actual)
  
  ## Make Plots
  plr = ggplot(plot_df_lr, aes(x = ls)) + 
    geom_line(aes(y = expected), color = "red", lwd = 1.2) +
    geom_line(aes(y = actual), color = "blue", lwd = 1.2) + 
    labs(x = expression(lambda), y = "Groups (of 80)", title = "Linear")  + scale_y_continuous(limits = c(0,ngrp)) + 
    annotate("text", x = 0.05, y = ngrp*0.9, label = paste("area ratio:", round(diff_auc_lr,3)), vjust = 1) + theme_bw()
  
  plog = ggplot(plot_df_log, aes(x = ls)) + 
    geom_line(aes(y = expected), color = "red", lwd = 1.2) +
    geom_line(aes(y = actual), color = "blue", lwd = 1.2) + 
    labs(x = expression(lambda), y = "", title = "Logistic")  + scale_y_continuous(limits = c(0,ngrp)) + 
    annotate("text", x = 0.023, y = ngrp*0.9, label = paste("area ratio:", round(diff_auc_log,3)), vjust = 1) + theme_bw()
  
  psurv = ggplot(plot_df_surv, aes(x = ls)) + 
    geom_line(aes(y = expected), color = "red", lwd = 1.2) +
    geom_line(aes(y = actual), color = "blue", lwd = 1.2) + 
    labs(x = expression(lambda), y = "", title = "Cox")  + scale_y_continuous(limits = c(0,ngrp)) + 
    annotate("text", x = median(ls)*1.2, y = ngrp*0.9, label = paste("area ratio:", round(diff_auc_surv,3)), vjust = 1) + theme_bw()
  
  
  ## Repeat the hacky way to get a common legend
  dummy_df <- data.frame(x = c(1,1), y = c(1,1), Group = c("Estimated False Positives", "Empirical False Positives"))
  
  dummy_plot <- ggplot(dummy_df, aes(x, y, color = Group)) +
    geom_line() + geom_point() + scale_color_manual(values = c("blue","red")) +
    guides(color = guide_legend(nrow = 1)) + theme_minimal() + labs(color = "") +
    theme(legend.direction = "horizontal",legend.box = "horizontal",legend.position = "bottom")
  
  get_legend <- function(myplot){
    tmp <- ggplot_gtable(ggplot_build(myplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  
  my_legend <- get_legend(dummy_plot)
  
  ## Final combined plot
  final_fig2 = grid.arrange(plr, plog, psurv,
                            my_legend,
                            layout_matrix = rbind(c(1, 2, 3),
                                                  c(4, 4, 4)),
                            heights = c(3,1))
  
  return(final_fig2)
  
}


################################################################################
###
###   Function to make Figure 3
###
################################################################################


create_fig3 <- function(reps = 300){
  
  set.seed(123)
  
  pp = c(100,200,500,800,1200)
  diff_aucs = diff_aucs_corr = diff_aucs_fac = diff_aucs_fac6 =
  diff_aucs_mcp = diff_aucs_corr_mcp = diff_aucs_fac_mcp = diff_aucs_fac6_mcp =  numeric(length(pp))
  nreps = reps
  
  for (i in 1:length(pp)){
    
    ### Grp Lasso
    
    temp_res_fac = get_plot_for_fig1(reps = 100, n = pp[i], ngrp = 80, ntrue = 2, lmin = 0,
                                     nltype = "piecewise", sim_col = "factor", k = 3,
                                     ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian",
                                     lam_max_mult = 1)
    
    diff_aucs_fac[i] = temp_res_fac$auc
    
    temp_res_fac6 = get_plot_for_fig1(reps = 100, n = pp[i], ngrp = 80, ntrue = 2, lmin = 0,
                                      nltype = "piecewise", sim_col = "factor", k = 6,
                                      ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian",
                                      lam_max_mult = 1)
    
    diff_aucs_fac6[i] = temp_res_fac6$auc
    
    temp_res_corr = get_plot_for_fig1(reps = nreps, n = pp[i],  ngrp = 80, ntrue = 2,  lmin = 0,
                                      nltype = "piecewise", sim_col = "nonparm",
                                      ctype = "AR", rho = 0.8, pen = "grLasso", fam = "gaussian",
                                      lam_max_mult = 1) 
    
    diff_aucs_corr[i] = temp_res_corr$auc
    
    temp_res = get_plot_for_fig1(reps = nreps, n = pp[i],  ngrp = 80, ntrue = 2,  lmin = 0,
                                 nltype = "piecewise", sim_col = "nonparm",
                                 ctype = "AR", rho = 0.0, pen = "grLasso", fam = "gaussian",
                                 lam_max_mult = 1) 
    
    diff_aucs[i] = temp_res$auc
    
    ### Grp MCP
    
    temp_res_facm = get_plot_for_fig1(reps = 100, n = pp[i], ngrp = 80, ntrue = 2, lmin = 0,
                                     nltype = "piecewise", sim_col = "factor", k = 3,
                                     ctype = "AR", rho = 0, pen = "grMCP", fam = "gaussian",
                                     lam_max_mult = 1)
    
    diff_aucs_fac_mcp[i] = temp_res_facm$auc
    
    temp_res_fac6m = get_plot_for_fig1(reps = 100, n = pp[i], ngrp = 80, ntrue = 2, lmin = 0,
                                      nltype = "piecewise", sim_col = "factor", k = 6,
                                      ctype = "AR", rho = 0, pen = "grMCP", fam = "gaussian",
                                      lam_max_mult = 1)
    
    diff_aucs_fac6_mcp[i] = temp_res_fac6m$auc
    
    temp_res_corrm = get_plot_for_fig1(reps = nreps, n = pp[i],  ngrp = 80, ntrue = 2,  lmin = 0,
                                      nltype = "piecewise", sim_col = "nonparm",
                                      ctype = "AR", rho = 0.8, pen = "grMCP", fam = "gaussian",
                                      lam_max_mult = 1) 
    
    diff_aucs_corr_mcp[i] = temp_res_corrm$auc
    
    temp_resm = get_plot_for_fig1(reps = nreps, n = pp[i],  ngrp = 80, ntrue = 2,  lmin = 0,
                                 nltype = "piecewise", sim_col = "nonparm",
                                 ctype = "AR", rho = 0.0, pen = "grMCP", fam = "gaussian",
                                 lam_max_mult = 1) 
    
    diff_aucs_mcp[i] = temp_resm$auc
  }
  
  plot_df_gl = data.frame(values = c(diff_aucs, diff_aucs_corr, diff_aucs_fac, diff_aucs_fac6),
                       Scenario = rep(c("Piecewise (Independence)", "Piecewise (Autoregressive)", "Factor (k=3)", "Factor (k=6)"), each = length(diff_aucs)),
                       n = rep(pp, times = 4), Penalty = "Group Lasso")
  
  plot_df_mcp = data.frame(values = c(diff_aucs_mcp, diff_aucs_corr_mcp, diff_aucs_fac_mcp, diff_aucs_fac6_mcp),
                          Scenario = rep(c("Piecewise (Independence)", "Piecewise (Autoregressive)", "Factor (k=3)", "Factor (k=6)"), each = length(diff_aucs)),
                          n = rep(pp, times = 4), Penalty = "Group MCP")
  
  plot_df = rbind(plot_df_gl, plot_df_mcp)
  
  p1 = ggplot(plot_df_gl, aes(x = n, y = values, color = Scenario)) + geom_point() + geom_line() + scale_x_continuous(breaks = seq(0, 1200, by = 200)) +
    scale_y_continuous(breaks = seq(1.0, 1.2, by = 0.05), limits = c(1.0,1.2)) +
    theme_bw() + labs(y = "AUC Ratio", color = "", title = "Group Lasso") + geom_hline(yintercept = 1, lty = 2)
  
  p2 = ggplot(plot_df_mcp, aes(x = n, y = values, color = Scenario)) + geom_point() + geom_line() + scale_x_continuous(breaks = seq(0, 1200, by = 200)) +
    scale_y_continuous(breaks = seq(1.0, 1.2, by = 0.05)) +
    theme_bw() + labs(y = " ", color = "", title = "Group MCP") + geom_hline(yintercept = 1, lty = 2) +
    theme(legend.position = "none")
  
  dummy_plot <- p1 +
    theme(legend.direction = "horizontal",legend.box = "horizontal",legend.position = "bottom")
  my_legend <- get_legend(dummy_plot)
  
  final_fig3 = grid.arrange(p1 + theme(legend.position = "none"), 
                            p2,
                            my_legend,
                            layout_matrix = rbind(c(1, 2),
                                                  c(3, 3)),
                            heights = c(4,1))
  
  
  return(final_fig3)
}
  
###############################################################################
###
###      Helper to make plots used in Figure 1
###
###############################################################################
  
  
get_plot_for_fig1 = function(reps = 100, n = 200, ngrp = 100, ntrue = 10, k=3, nltype = "piecewise", sim_col = "nonparm", lmin = 0.01,
                               ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian", x_lab = "", y_lab = "", lam_max_mult = 1){
    
    set.seed(1234)
    
    if(sim_col == "nonparm"){
      r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                         rho = rho)
      X <- r$X
      y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, family = fam)
      grp <- r$grp
    } else if(sim_col == "factor"){
      r <- genX_fac_grp(n = n, ngrp = ngrp,
                        k = k,
                        ctype = ctype, rho = rho)
      X <- r$X
      y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = 5/sqrt(n), family = fam)
      grp <- r$grp
    }
    
    
    fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = fam)
    ls <- seq(lam_max_mult*max(fit$lambda), lmin, length.out = 100)
    
    rmat <- mmat <- matrix(NA, nrow = reps, ncol = length(ls))
    i = 1
    while(i <= reps){
      
      
      if(sim_col == "nonparm"){
        r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                           rho = rho)
        X <- r$X
        y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, family = fam)
        grp <- r$grp
      } else if(sim_col == "factor"){
        r <- genX_fac_grp(n = n, ngrp = ngrp,
                          k = k,
                          ctype = ctype, rho = rho)
        X <- r$X
        y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = 5/sqrt(n), family = fam)
        grp <- r$grp
      }
      
      fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = fam, lambda = ls)
      
      expected_fd = grpreg::mfdr(fit, X)$EF
      
      if(length(expected_fd) != length(ls)){
       next  
      }
      
      mmat[i,] <- expected_fd
      rmat[i,] = get_truth(fit, ntrue = ntrue)$fp
      i = i+1
    }
    
    
    plot_df <- data.frame(
      ls   = ls,
      expected = apply(mmat, 2, mean),
      actual  = apply(rmat, 2, mean)
    )
    
    
    mean_per_error = with(plot_df, mean((expected - actual)/expected, na.rm = TRUE))
    # print(mean_per_error)
    
    diff_auc = AUC(plot_df$ls, plot_df$expected)/AUC(plot_df$ls, plot_df$actual)
    # print(diff_auc)
    
    pp = ggplot(plot_df, aes(x = ls)) + 
      geom_line(aes(y = expected), color = "red", lwd = 1.2) +
      geom_line(aes(y = actual), color = "blue", lwd = 1.2) + 
      labs(x = x_lab, y = y_lab)  + scale_y_continuous(limits = c(0,ngrp)) + 
      annotate("text", x = median(ls)*1.2, y = ngrp*0.9, label = paste("area ratio:", round(diff_auc,3)), vjust = 1) + theme_bw()
    
    output = list(auc = diff_auc,
                  plot = pp)
    
    return(output)
}




################################################################################
###
###   Function to generate factor groups (potentially w/ correlations)
###
################################################################################

genX_fac_grp <- function(n, ngrp, k = 3, probs = NULL, 
                         type = "independent", ctype = NULL,
                         rho = 0){
  
  ## Decide if balanced probs or unequal distrib
  if(is.null(probs)){
    probs <- rep(1/k, k)
    }
  
  ## Create X and rawX
  X <- NULL
  rawX <- NULL
  
  ## Create group IDs
  grp <- sort(rep(1:ngrp, k))
  
  ## Generate either independent or non-independent groups
  if(type == "independent"){
  for(i in 1:ngrp){
    tgrp <- sample(as.factor(1:k), size = n, prob = probs, replace = TRUE)
    rawX <- cbind(rawX, tgrp)
    
    tx <- model.matrix(~ -1 + ., data = data.frame(tgrp))
    colnames(tx) <- paste0("grp",i,"lvl",1:k)
    X <- cbind(X, tx)
    }
  } else if(type == "correlated"){
   ## Generate correlated MVNorm values
    if(ctype == "AR"){
      RHO <- matrix(rho^(0:(ngrp-1)), ngrp, ngrp, byrow=TRUE)
      S <- bandSparse(ngrp, k=0:(ngrp-1), diagonals=RHO, symmetric=TRUE)
      R <- chol(S)
      tX <- as.matrix(matrix(rnorm(n*ngrp), n, ngrp) %*% R)
    } else if (ctype == "Exch"){
      S <- matrix(rho, ngrp, ngrp)
      diag(S) <- 1
      R <- chol(S)
      tX <- as.matrix(matrix(rnorm(n*ngrp), n, ngrp) %*% R)
    }
    
    ## Convert MVN values to factors using "probs"
    cpts <- rbind(-Inf, apply(tX, 2, quantile, probs = cumsum(probs)))
    cpts[k+1,] <- Inf
    
    for(i in 1:ngrp){
      tgrp <- cut(tX[,i], breaks = cpts[,i], labels = 1:k)
      rawX <- cbind(rawX, tgrp)
      
      tx <- model.matrix(~ -1 + ., data = data.frame(tgrp))
      colnames(tx) <- paste0("grp",i,"lvl",1:k)
      X <- cbind(X, tx)
     }
    }
  
  return(list(X = X, grp = grp, rawX = rawX))
}

################################################################################
###
###   Function to estimate Marginal false discoveries
###
################################################################################


get_EFD <- function(fit, sig = 1, X = NULL, y = NULL){
  
  ## Setups
  g <- fit$XG$g
  ngrp <- length(unique(g))
  k <- as.vector(table(g))
  nl <- length(fit$lambda)
  fd <- numeric(nl)
  XX <- fit$XG$X
  
  ## Linear reg
  if(fit$family == "gaussian"){
  
  ## Estimate sigma (if needed)
  if(is.null(sig)){
    cvf <- cv.grpreg(X = X, y = y, grp = fit$group)
    sig = min(cvf$cve)
  }
  
  ## Calculate MFDR at each lambda value
  for(l in 1:nl){
  
    ## Add up expected contribution of each grp
    for(i in 1:ngrp){
      fd[l] = fd[l] + pchisq(fit$n*k[i]*fit$lambda[l]^2/sig^2, 
                             df = k[i], lower.tail = FALSE)
    }
  }
 } else if (fit$family == "binomial"){
   
  ## First calculate W for each lambda
   for(l in 1:nl){
     P <- predict(fit,X = X, lambda = fit$lambda[l], type = "response")  
     W <- diag(as.vector(P*(1 - P)))  

     ## Add up expected contribution of each grp
     for(j in 1:ngrp){
       st <- (fit$lambda[l]^2*n^2*k[j]^2)/(sum(diag(t(XX[,g == j]) %*% W %*% XX[,g == j])))
       fd[l] <- fd[l] + pchisq(st, df = k[j], lower.tail = FALSE)
     }
   }
 }
    
  return(fd)
}


################################################################################
###
###   Function used to generate X for non-parm regression
###
################################################################################

genX_nonparm <- function(n, ngrp, ctype = "AR",
                         rho = 0){

## Create X 
X <- rawX <- NULL  
  
if(ctype == "AR"){
  RHO <- matrix(rho^(0:(ngrp-1)), ngrp, ngrp, byrow=TRUE)
  S <- bandSparse(ngrp, k=0:(ngrp-1), diagonals=RHO, symmetric=TRUE)
  R <- chol(S)
  rawX <- as.matrix(matrix(rnorm(n*ngrp), n, ngrp) %*% R)
} else if (ctype == "Exch"){
  S <- matrix(rho, ngrp, ngrp)
  diag(S) <- 1
  R <- chol(S)
  rawX <- as.matrix(matrix(rnorm(n*ngrp), n, ngrp) %*% R)
}

for(i in 1:ncol(rawX)){
X <- cbind(X, bs(rawX[,i]))
}

grp <- sort(rep(1:ngrp, 3))

return(list(X = X, rawX = rawX, grp = grp))
}

################################################################################
###
###   Function used to calculate number of selected groups at each lambda
###
################################################################################

get_S <- function(fit){
  
  ## Setups
  ngrp <- length(unique(fit$group))
  gid <- 1:ngrp
  nl <- length(fit$lambda)
  betas <-  fit$beta[-1,]
  S = numeric(nl)
  
  ## Find S
  for(l in 1:nl){
    for(i in 1:ngrp){
      S[l] = S[l] + ifelse(sum(betas[fit$group == gid[i],l]) != 0, 1, 0)
    }
  }
  
  return(S)
}


################################################################################
###
###      Function used to generate outcomes from factor groups
###
################################################################################

genY_factor <- function(X, grp, ntrue, beta = NULL, family = "gaussian",
                        bmin = -2, bmax = 2, sigma = 1, C = 0){
  
Xtrue = X[,grp %in% 1:ntrue]

if (is.null(beta)){
   b = runif(ncol(Xtrue), bmin, bmax)
} else {
   b = sample(c(beta,-beta), size = ncol(Xtrue), replace = TRUE)
}

if(family == "gaussian"){
  y <- Xtrue %*% b + rnorm(nrow(X), 0, sd = sigma)
} else if (family == "binomial") {
  py <- 1/(1 + exp(-Xtrue %*% b))
  y = rbinom(length(py),1,py)
} else if (family == "survival") {
  haz = exp(Xtrue %*% b)
  y = rexp(length(haz), haz)

  
  ## Random censoring of C% of data
  c = rbinom(n, 1, p = (1-C))
  ct = cbind(runif(sum(c == 0), min = 0, max = y[which(c == 0)]), y[which(c == 0)])
  y[which(c == 0)] = ct[,1]
  y = Surv(time = y, event = c)
}

return(y)
}

################################################################################
###
###      Function to calculate the true group-level FDR
###
################################################################################


get_truth <- function(fit, ntrue){
  
  ## Beta matrix
  bm <- fit$beta[-1,]
  
  ## True positives
  true_ids = match(1:ntrue, fit$group)
  tp = apply(bm[true_ids,] != 0, 2, sum)
  
  ## False negatives
  fn = apply(bm[true_ids,] == 0, 2, sum)
  
  ## False positives
  mg = max(as.numeric(fit$group))
  false_ids = match((ntrue + 1):mg, fit$group)
  fp = apply(bm[false_ids,] != 0, 2, sum)
  
  ## True negatives
  tn = apply(bm[false_ids,] == 0, 2, sum)
  
  ## FP rate
  real_fdr = fp/(fp + tp)
  real_fdr[is.nan(real_fdr)] = 0
  
  return(list(tp = tp, fn = fn,
              fp = fp, tn = tn,
              real_fdr = real_fdr))
}


################################################################################
###
###  Function to generate non-linear relationships for non-parametric regression
###
################################################################################


genY_nonlin <- function(rawX, ntrue, type = "quadratic", 
                        sigma = 1, family = "gaussian", pbeta = 10, C = 0){
  
  ## Setup
  n = nrow(rawX)
  
  piece <- function(x){
    x[which(x < 0)] <- 0
    return(pbeta/sqrt(n)*x)
  }
  
  quadp <- function(x){
    return(pbeta/sqrt(n)*(x^2)) 
  }
  
if(family == "gaussian"){
  if(ntrue == 0){
    y = rnorm(n, sd = sigma)
  } else if(type == "quadratic"){
    y = rowSums(apply(rawX[,1:ntrue], 2, quadp)) + 
      rnorm(n, sd = sigma)
  } else if (type == "piecewise"){
    y = rowSums(apply(rawX[,1:ntrue], 2, piece)) + 
      rnorm(n, sd = sigma)
  } else if (type == "sin"){
    y = rowSums(apply(rawX[,1:ntrue], 2, sin)) + 
      rnorm(n, sd = sigma)
  }
} else if (family == "binomial"){
  if(ntrue == 0){
    y = rbinom(n, 1, prob = 0.5)
  } else if(type == "quadratic"){
    eta =  rowSums(apply(rawX[,1:ntrue], 2, quadp))
    y = rbinom(n, 1, prob =  1/(1 + exp(-eta)))
  } else if (type == "piecewise"){
    eta = rowSums(apply(rawX[,1:ntrue], 2, piece))
    y = rbinom(n, 1, prob =  1/(1 + exp(-eta)))
  } else if (type == "sin"){
    eta = rowSums(apply(rawX[,1:ntrue], 2, sin))
    y = rbinom(n, 1, prob =  1/(1 + exp(-eta)))
  }
} else if (family == "survival"){
  if(ntrue == 0){
    y = rexp(n)
  } else if(type == "quadratic"){
    haz =  exp(rowSums(apply(rawX[,1:ntrue], 2, quadp)))
    y = rexp(n, haz)
  } else if (type == "piecewise"){
    haz = exp(rowSums(apply(rawX[,1:ntrue], 2, piece)))
    y = rexp(n, haz)
  } else if (type == "sin"){
    haz = exp(rowSums(apply(rawX[,1:ntrue], 2, sin)))
    y = rexp(n, haz)
  }
  
  ## Random censoring of C% of data
  c = rbinom(n, 1, p = (1-C))
  ct = cbind(runif(sum(c == 0), min = 0, max = y[which(c == 0)]), y[which(c == 0)])
  y[which(c == 0)] = ct[,1]
  y = Surv(time = y, event = c)
}
  
  
  return(y)
}


################################################################################
###
###  Sim Study #1 - MFDR accuracy vs. n (same func for both independence and cor)
###
################################################################################


sim1 <- function(nreps, n, k = 3, ngrp, ntrue, sim_col = "factor",
                 type = "independent", ctype = NULL, rho = 0,
                 nltype = "quadratic", estimate_sig = FALSE, pen = "grLasso", bb = 6){


## Calibrate a suitable lambda sequence
if(sim_col == "factor"){
  r <- genX_fac_grp(n = n, ngrp = ngrp,
                    k = k, type = type,
                    ctype = ctype, rho = rho)
  X <- r$X
  y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = bb/sqrt(n))
  grp <- r$grp
} else if (sim_col == "nonparm"){
  r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                     rho = rho)
  X <- r$X
  y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype)
  grp <- r$grp
}

fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE)
tmfdr <- mfdr(fit)$mFDR
lmin <- min(which(tmfdr > 0.8)) + 1
lmax <- max(max(which(tmfdr < 0.001)) - 1, 1)
lseq <- seq(fit$lambda[lmax], fit$lambda[lmin], length.out = 100)

## Objects to store results
sm <- rfp <- estfp <- matrix(NA, nrow = nreps, ncol = length(lseq))

## Repeat for nreps
for(rep in 1:nreps){
  
if(sim_col == "factor"){
  r <- genX_fac_grp(n = n, ngrp = ngrp,
                  k = k, type = type,
                  ctype = ctype, rho = rho)
  X <- r$X
  y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = bb/sqrt(n))
  grp <- r$grp
} else if (sim_col == "nonparm"){
  r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                           rho = rho)
  X <- r$X
  y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype)
  grp <- r$grp
}

fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, lambda = lseq)

## Estimate marginal false discoveries
estfp[rep,] <- grpreg::mfdr(fit)$EF*(1 - ntrue/ngrp)

## Store selected groups
sm[rep,] <- get_S(fit)

## Store false positives
rfp[rep,] = get_truth(fit, ntrue = ntrue)$fp
}

## Return results
return(list(estfp = estfp, sm = sm, rfp = rfp, lseq = lseq, ffit = fit))
}

################################################################################
###
###  Sim Study #2 - MFDR accuracy vs. n for Logistic Regression
###
################################################################################

sim2 <- function(nreps, n, k = 3, ngrp, ntrue, sim_col = "factor",
                 type = "independent", ctype = NULL, rho = 0,
                 nltype = "quadratic", pen = "grLasso"){
  
    res_est5 <- res_t5 <- res_est10 <- res_t10 <-
    res_est15 <- res_t15 <- res_est20 <- res_t20 <-
    incor <- numeric(nreps)

for(rep in 1:nreps){
  
  if(sim_col == "factor"){
    r <- genX_fac_grp(n = n, ngrp = ngrp,
                      k = k, type = type,
                      ctype = ctype, rho = rho)
    X <- r$X
    y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, family = "binomial", beta = 10/sqrt(n))
    grp <- r$grp
  } else if (sim_col == "nonparm"){
    r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                       rho = rho)
    X <- r$X
    y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, family = "binomial")
    grp <- r$grp
  }
  
  ## Track incidental correlations
  #cX <- cor(X)
  #for (i in 1:ngrp){
  #  cX[grp == i, grp == i] = 0
  #}
  #incor[rep] <- mean(abs(cX))
  
  ## Fit model using grpreg
  fit <- grpreg(X, y, group = grp, penalty = pen, returnX = TRUE, family = "binomial")
  
  ## Estimate marginal false discoveries
  MFDR<- grpreg::mfdr(fit, X)$mFDR
  
  ## Get true FDR
  RMFDR = get_truth(fit, ntrue = ntrue)$real_fdr
  
  res_est5[rep] = MFDR[max(which(MFDR <= .05))]
  res_t5[rep] = RMFDR[max(which(MFDR <= .05))]
  
  res_est10[rep] = MFDR[max(which(MFDR <= .10))]
  res_t10[rep] = RMFDR[max(which(MFDR <= .10))]
  
  res_est15[rep] = MFDR[max(which(MFDR <= .15))]
  res_t15[rep] = RMFDR[max(which(MFDR <= .15))]
  
  res_est20[rep] = MFDR[max(which(MFDR <= .20))]
  res_t20[rep] = RMFDR[max(which(MFDR <= .20))]
  
}

mean_errs <- c(mean(res_est5 - res_t5), mean(res_est10 - res_t10),
               mean(res_est15 - res_t15), mean(res_est20 - res_t20),
               mean(incor))

## 5th position in avg inc cor 
return(mean_errs)
}

################################################################################
###
###  Local Group-level mfdr (Extra -- might not use)
###
################################################################################

loc_grp_mfdr <- function(fit, lambda, sig = NULL,
                         method=c('ashr', 'kernel'), ...) {
  
  # Determine method, if missing
  if (!inherits(fit, 'grpreg')) stop('"fit" must be a grpreg object', call.=FALSE)
  if (missing(method)) {
    if (requireNamespace('ashr', quietly=TRUE)) {
      method <- 'ashr'
    } else {
      method <- 'kernel'
      if (is.null(getOption('grp.ashr.warn'))) {
        message('Using a basic kernel estimate for local fdr; consider installing the ashr package for more accurate estimation.  See ?local_mfdr')
        options(grp.ashr.warn = FALSE)
      }
    }
  }
  
  if (is.null(fit$X)) {
    stop("For this procedure you must use the argument 'returnX=TRUE' when fitting your grpreg model", call.=FALSE)
  }
  
  ## General setup
  yy <- fit$y
  XX <- fit$XG$X
  sc <- fit$XG$scale
  cn <- fit$XG$center
  n <- nrow(XX)
  p <- ncol(XX)
  grp <- fit$XG$g
  ngrp <- length(unique(grp))
  beta <- coef(fit, lambda = lambda)
  S <- predict(fit, type = "ngroups", lambda = lambda)
  lid <- which(fit$lambda == lambda)
  pen.idx <- fit$XG$m > 0
  
  
  ## Calculation (linear)
  
  ## Get standardized coefs
  ind <- !sapply(attr(XX, "T"), is.null)
  TT <- Matrix::bdiag(attr(XX, "T")[ind])
  bb <- round(MASS::ginv(as.matrix(TT)) %*% (beta[-1]*sc), 20)
    
  ## Estimate sigma^2 (if not provided)
  if(is.null(sig)){
  esig2 <- fit$loss[lid]/(n - fit$df[lid])
  } else {
  esig2 <- sig^2
  }
  
  ## Calculate statistics
  z <- numeric(ngrp)
  for(j in 1:ngrp){
    pr <- yy - XX[,grp != j] %*% bb[grp != j]
    tj <- (1/(n*esig2))*crossprod(t(XX[,grp == j]) %*% pr)
    z[j] <- qnorm(pchisq(tj, df = sum(grp == j), log.p = TRUE), log.p = TRUE)
  }
  
  # Calculate locfdr
  if (method=='ashr') {
    ash_fit <- ashr::ash(z[pen.idx], rep(1, sum(pen.idx)), optmethod='mixEM')
    est.gam <- ashr::get_lfdr(ash_fit)
  } else {
    f <- density(z[pen.idx])
    ff <- approxfun(f$x, f$y)
    est.gam <- pmin(dnorm(z[pen.idx], 0, 1)/ff(z[pen.idx]), 1)
  }    
  
  bn <-  predict(fit, type = "norm", lambda = lambda)
  return(data.frame(grp = 1:ngrp,
                    coefnorm = round(bn,3), 
                    z = round(z,3), 
                    mfdr = round(est.gam,5)))  
}

###########################################
###
###         Function to make Plot #1
###
###########################################

make_plot1 <- function(reps = 200){
  
  set.seed(1234)
  
  layout(matrix(c(1,2,3,4,4,4), ncol=3, byrow=TRUE), heights=c(4, 0.6))
  par(mar = c(4, 4.1, 3, 2.5), oma = c(0,0,0,0))
  
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
       ylab = "Groups",
       xlab = expression(lambda),
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
       ylab = "",
       xlab = expression(lambda),
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
       ylab = "",
       xlab = expression(lambda),
       main = "Cox", bty = "n")
  lines(ls, apply(rmat,2, mean), lwd = 2, col = "red")
  
  par(mai=c(0,0,0,0))
  plot.new()
  legend("center", legend = c("Expected False Selections (mFDR)", "Empirical False Sections"), lwd = 2, 
         col = c("blue", "red"), bty = "n", ncol = 2)
}

#####################################
###
###    Function to make Plot #2
###
#####################################

make_plot2 <- function(reps = 200){
  
  set.seed(1234)
  
  layout(matrix(c(1,2,3,4,4,4), ncol=3, byrow=TRUE), heights=c(4, 0.6))
  par(mar = c(4, 4.1, 3, 2.5), oma = c(0,0,0,0))
  
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

  plot(ls, apply(mmat, 2, mean)*(1 - ntrue/ngrp), type = "l", lwd = 2, col = "blue",
       ylab = "Groups",
       xlab = expression(lambda), main = "Linear", bty = "n")
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
  
  plot(ls, apply(mmat, 2, mean)*(1 - ntrue/ngrp), type = "l", lwd = 2, col = "blue",
       ylab = "Groups",
       xlab = expression(lambda), main = "Logistic", bty = "n")
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
       ylab = "",
       xlab = expression(lambda), main = "Cox", bty = "n")
  lines(ls, apply(rmat,2, mean), lwd = 2, col = "red")
  
  
  par(mai=c(0,0,0,0))
  plot.new()
  legend("center", legend = c("Expected False Selections (mFDR)", "Empirical False Sections"), 
         lwd = 2, col = c("blue", "red"), bty = "n", ncol = 2)
}

####################################################
##
##    helper function to get legend from ggplot
##
####################################################

get_legend <- function(myplot){
  tmp <- ggplot_gtable(ggplot_build(myplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


#######################################
##
##      Function to make Plot 3
##
#######################################

make_plot3 <- function(reps = 500){

  set.seed(1234)
  nseq = c(150,300,500,750,1000,1300,1600)
  thr = 0.15
  tfdr_diff1 = tfdr_se1 = tfdr_diff2 = tfdr_se2 = tfdr_diff3 = tfdr_se3 = numeric(length(nseq))
  
  for(nn in 1:length(nseq)){
    sres1 <- sim1(nreps = reps, n = nseq[nn], k = 3, ngrp = 100, ntrue = 10)
    efdr_seq <- apply(sres1[[1]], 2, mean)/apply(sres1[[2]], 2, mean)
    thr.id <- max(which(efdr_seq < thr))
    tfdr_diff1[nn] <-  (apply(sres1[[3]], 2, mean)/apply(sres1[[2]], 2, mean))[thr.id] - efdr_seq[thr.id]
    tfdr_se1[nn] <- apply(sres1[[3]]/sres1[[2]], 2, sd, na.rm = TRUE)[thr.id]/sqrt(reps)
    
    sres1 <- sim1(nreps = reps, n = nseq[nn], k = 6, ngrp = 100, ntrue = 10)
    efdr_seq <- apply(sres1[[1]], 2, mean)/apply(sres1[[2]], 2, mean)
    thr.id <- max(which(efdr_seq < thr))
    tfdr_diff2[nn] <-  (apply(sres1[[3]], 2, mean)/apply(sres1[[2]], 2, mean))[thr.id] - efdr_seq[thr.id]
    tfdr_se2[nn] <- apply(sres1[[3]]/sres1[[2]], 2, sd, na.rm = TRUE)[thr.id]/sqrt(reps)
    
    sres1 <- sim1(nreps = reps, n = nseq[nn], ngrp = 100, ntrue = 10, nltype = "piecewise", sim_col = "nonparm", ctype = "AR", rho = 0)
    efdr_seq <- apply(sres1[[1]], 2, mean)/apply(sres1[[2]], 2, mean)
    thr.id <- max(which(efdr_seq < thr))
    tfdr_diff3[nn] <-  (apply(sres1[[3]], 2, mean)/apply(sres1[[2]], 2, mean))[thr.id] - efdr_seq[thr.id]
    tfdr_se3[nn] <- apply(sres1[[3]]/sres1[[2]], 2, sd, na.rm = TRUE)[thr.id]/sqrt(reps)
  }
  
  ## Setup plot Layout
  layout(matrix(c(1,2,3,3), ncol=2, byrow=TRUE), heights=c(4, 0.6))
  par(mar = c(5.1, 4.1, 4.1, 2.1), oma = c(0,0,0,0))
  
  ## Left plot (group Lasso)
  plot(nseq, tfdr_diff1, col = 2,
       ylim = c(-0.15,0.013), lwd = 1.6, type = "b", ylab = "Estimated - Empirical FDR",
       xlab = "n", main = "Group Lasso", bty = "n")
  arrows(x0=nseq, y0= tfdr_diff1 - tfdr_se1, x1=nseq, y1= tfdr_diff1 + tfdr_se1, code=3, angle=90, length=0.05, col=2, lwd=1.6)
  abline(h = 0, lty = 2, lwd = 1.6)
  lines(nseq, tfdr_diff2, type = "b", lwd = 1.6, pch = 2, col = 3)
  arrows(x0=nseq, y0= tfdr_diff2 - tfdr_se2, x1=nseq, y1= tfdr_diff2 + tfdr_se2, code=3, angle=90, length=0.05, col=3, lwd=1.6)
  lines(nseq, tfdr_diff3, type = "b", lwd = 1.6, pch = 4, col = 4)
  arrows(x0=nseq, y0= tfdr_diff3 - tfdr_se3, x1=nseq, y1= tfdr_diff3 + tfdr_se3, code=3, angle=90, length=0.05, col=4, lwd=1.6)
  
  
  for(nn in 1:length(nseq)){
    sres1 <- sim1(nreps = reps, n = nseq[nn], k = 3, ngrp = 100, ntrue = 10, pen = "grMCP")
    efdr_seq <- apply(sres1[[1]], 2, mean)/apply(sres1[[2]], 2, mean)
    thr.id <- max(which(efdr_seq < thr))
    tfdr_diff1[nn] <-  (apply(sres1[[3]], 2, mean)/apply(sres1[[2]], 2, mean))[thr.id] - efdr_seq[thr.id]
    tfdr_se1[nn] <- apply(sres1[[3]]/sres1[[2]], 2, sd, na.rm = TRUE)[thr.id]/sqrt(reps)
    
    sres1 <- sim1(nreps = reps, n = nseq[nn], k = 6, ngrp = 100, ntrue = 10, pen = "grMCP")
    efdr_seq <- apply(sres1[[1]], 2, mean)/apply(sres1[[2]], 2, mean)
    thr.id <- max(which(efdr_seq < thr))
    tfdr_diff2[nn] <-  (apply(sres1[[3]], 2, mean)/apply(sres1[[2]], 2, mean))[thr.id] - efdr_seq[thr.id]
    tfdr_se2[nn] <- apply(sres1[[3]]/sres1[[2]], 2, sd, na.rm = TRUE)[thr.id]/sqrt(reps)
    
    sres1 <- sim1(nreps = reps, n = nseq[nn], ngrp = 100, ntrue = 10, nltype = "piecewise",
                  sim_col = "nonparm", ctype = "AR", rho = 0, pen = "grMCP")
    efdr_seq <- apply(sres1[[1]], 2, mean)/apply(sres1[[2]], 2, mean)
    thr.id <- max(which(efdr_seq < thr))
    tfdr_diff3[nn] <-  (apply(sres1[[3]], 2, mean)/apply(sres1[[2]], 2, mean))[thr.id] - efdr_seq[thr.id]
    tfdr_se3[nn] <- apply(sres1[[3]]/sres1[[2]], 2, sd, na.rm = TRUE)[thr.id]/sqrt(reps)
  }
  
  ## Right plot (group MCP)
  plot(nseq, tfdr_diff1, col = 2,
       ylim = c(-0.15,0.013), lwd = 1.6, type = "b", ylab = " ",
       xlab = "n", main = "Group MCP", bty = "n")
  arrows(x0=nseq, y0= tfdr_diff1 - tfdr_se1, x1=nseq, y1= tfdr_diff1 + tfdr_se1, code=3, angle=90, length=0.05, col=2, lwd=1.6)
  abline(h = 0, lty = 2, lwd = 1.6)
  lines(nseq, tfdr_diff2, type = "b", lwd = 1.6, pch = 2, col = 3)
  arrows(x0=nseq, y0= tfdr_diff2 - tfdr_se2, x1=nseq, y1= tfdr_diff2 + tfdr_se2, code=3, angle=90, length=0.05, col=3, lwd=1.6)
  lines(nseq, tfdr_diff3, type = "b", lwd = 1.6, pch = 4, col = 4)
  arrows(x0=nseq, y0= tfdr_diff3 - tfdr_se3, x1=nseq, y1= tfdr_diff3 + tfdr_se3, code=3, angle=90, length=0.05, col=4, lwd=1.6)
  
  par(mai=c(0,0,0,0))
  plot.new()
  legend("center", legend = c("Factors (k = 3)",
                              "Factors (k = 6)",
                              "Non-parametric"),
         pch = c(1,2,4), col = c(2,3,4), bty = "n", ncol = 3)
  
}


#######################################
##
##      Function to make Plot 4
##
#######################################

make_plot4 <- function(reps = 100){
  
  
  #######################
  ## FIRST SET -> No Corr
  #######################
  
  n = 200
  ngrp = 100
  k = 3
  ntrue = 10
  ctype = "AR"
  rho = 0.0
  pen = "grLasso"
  fam = "gaussian"
  thr = 0.1
  pbeta = 1.1*c(2,4,6,8,10)/sqrt(n)
  
  tp_mfdr_gl = matrix(NA, nrow = reps, ncol = length(pbeta))
  tp_mfdr_gmcp = matrix(NA, nrow = reps, ncol = length(pbeta))
  tp_fs_aic = matrix(NA, nrow = reps, ncol = length(pbeta))
  tp_ls_fdr = matrix(NA, nrow = reps, ncol = length(pbeta))
  tp_ss_fdr = matrix(NA, nrow = reps, ncol = length(pbeta))
  lspv = numeric(ngrp)
  
  for(i in 1:reps){
    
    r <- genX_fac_grp(n = n, ngrp = ngrp, k = k, ctype = ctype, rho = rho)
    X <- r$X
    rawX <- r$rawX
    grp <- r$grp
    
    for(pb in 1:length(pbeta)){
      
      y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = pbeta[pb])
      
      ### MFDR (group MCP)
      fit <- grpreg(X, y, group = grp, returnX = TRUE, family = fam, penalty = "grMCP")
      mfdr_res <- grpreg::mfdr(fit)
      mfdr_mod <- max(which(mfdr_res$mFDR <= thr))
      
      truth <- get_truth(fit, ntrue)
      tp_mfdr_gmcp[i,pb] <- truth$tp[mfdr_mod]
      
      ### MFDR (group lasso)
      fit <- grpreg(X, y, group = grp, returnX = TRUE, family = fam, penalty = "grLasso")
      mfdr_res <- grpreg::mfdr(fit)
      mfdr_mod <- max(which(mfdr_res$mFDR <= thr))
      
      truth <- get_truth(fit, ntrue)
      tp_mfdr_gl[i,pb] <- truth$tp[mfdr_mod]
      
      ## Selective Inference (forward sel)
      fs_res <- groupfs(X, y, grp, 13, sigma = NULL, k = 2, intercept = TRUE, aicstop = 2, verbose = FALSE)
      fs_pv <- groupfsInf(fs_res, verbose = FALSE)
      fs_id <- forwardStop(fs_pv$pv, alpha=0.1)
      if(fs_id > 0){
        tp_fs_aic[i,pb] = sum(fs_pv$vars[1:fs_id] <= ntrue)
      } else {
        tp_fs_aic[i,pb] = 0
      }
      
      ## Data-splitting
      idx <- sample(n, n/2)
      X1 <- X[idx,]
      y1 <- y[idx] 
      y2 <- y[-idx]
      
      p1fit <- cv.grpreg(X1, y2, group = grp, returnX = TRUE, family = fam, penalty = "grLasso")
      cvl_id <- which(p1fit$fit$lambda == p1fit$lambda.min)
      p1sel <- unique(grp[which(p1fit$fit$beta[-1,cvl_id] != 0)])
      
      if(length(p1sel) == 0){
        tp_ss_fdr[i,pb] = 0
      } else if (length(p1sel) == 1){
        x2 = rawX[-idx,p1sel]
        p2fit <- lm(y2 ~ factor(x2))
        p2test <- anova(p2fit)$`Pr(>F)`[1]
        if(p2test < thr & p1sel <= ntrue){
          tp_ss_fdr[i,pb] = 1
        } else {
          tp_ss_fdr[i,pb] = 0
        }
      } else {
        X2 <- data.frame(apply(rawX[-idx,p1sel], 2, factor))
        names(X2) <- p1sel
        p2fit <- lm(y2 ~ . , data = X2)
        p2fdr <- p.adjust(anova(p2fit)$`Pr(>F)`[1:ncol(X2)], method = "fdr")
        p2signif <- p1sel[which(p2fdr < thr)]
        tp_ss_fdr[i,pb] = sum(p2signif <= ntrue)
      }
      
      
      ## Large-scale testing
      for(j in 1:ngrp){
        tfit <- lm(y ~ X[,which(grp == j)])
        lspv[j] <- anova(tfit)$'Pr(>F)'[1]
      }
      tp_ls_fdr[i,pb] = sum(p.adjust(lspv, method = "fdr")[1:ntrue] <= thr)
      
    }
    print(paste0("Completed = ", 100*i/reps, "%"))
  }
  
  mc_tpr = apply(tp_mfdr_gmcp, 2, mean, na.rm = TRUE)
  gl_tpr = apply(tp_mfdr_gl, 2, mean, na.rm = TRUE)
  si_tpr = apply(tp_fs_aic, 2, mean, na.rm = TRUE)
  ss_tpr = apply(tp_ss_fdr, 2, mean, na.rm = TRUE)
  ls_tpr = apply(tp_ls_fdr, 2, mean, na.rm = TRUE)
  
  pf3_df = data.frame(beta = rep(pbeta, times = 5), 
                      tp = c(mc_tpr, gl_tpr, si_tpr,
                             ss_tpr, ls_tpr),
                      method = rep(c("mFDR (Grp MCP)", "mFDR (Grp lasso)",
                                     "Selective Inference (FS)", 
                                     "Sample Splitting",
                                     "Univariate Testing"), each = 5),
                      scenario = "Factor (k=3)")
  
  pf3 = ggplot(pf3_df, aes(x = beta, y = tp, color = method)) + geom_point() + geom_line() + theme_bw() + 
    labs(x = expression(beta), y = "True Positives (of 10)", color = " ", title = "Categorical (k=3)") + 
    scale_color_manual(
      values = c(
        "mFDR (Grp MCP)"            = "#0072B2", # Blue
        "mFDR (Grp lasso)"          = "#56B4E9", # Light blue
        
        "Selective Inference (FS)"  = "#CC79A7", # Purple
        "Selective Inference (lin)" = "#AA4C8F", # Darker purple
        
        "Sample Splitting"          = "#009E73", # Green
        "Univariate Testing"        = "#6DBE9A", # Light green
        
        "Knock-off Filter (lin)"    = "#E69F00"  # Orange
      )
    )
  
  # layout(matrix(c(1,2,3,4,4,4), ncol=3, byrow=TRUE), heights=c(4, 0.6))
  # par(mar = c(5.1, 4.1, 4.1, 2.1), oma = c(0,0,0,0))
  # 
  # plot(pbeta, mc_tpr, type = "b", lty = 2, ylim = c(0,10), 
  #      bty = "n", xlab = expression(beta), ylab = "True Positives (of 10)", main = "Factors (k = 3)")
  # lines(pbeta, gl_tpr, type = "b")
  # lines(pbeta, si_tpr, col = "green", type = "b")
  # lines(pbeta, ss_tpr, col = "red", type = "b")
  # lines(pbeta, ls_tpr, col = "purple", type = "b")
  # lines(pbeta, si_tpr, col = "blue", type = "b")
  
  
  
  n = 200
  ngrp = 100
  ntrue = 10
  nltype = "quadratic"
  sim_col = "nonparm"
  ctype = "AR"
  rho = 0.0
  pen = "grLasso"
  fam = "gaussian"
  thr = 0.1
  pbeta = c(2,4,6,8,10)/1.3
  
  ## Storage objects
  tp_mfdr_gl_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  tp_mfdr_gmcp_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  tp_fs_aic_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  tp_ls_fdr_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  tp_ss_fdr_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  lspv_q = numeric(ngrp)
  tp_si_pv_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  tp_si_sp_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  tp_si_msp_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  tp_si_ct_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  tp_kn_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  
  for(i in 1:reps){
    
    r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                       rho = rho)
    X <- r$X
    grp <- r$grp
    
    for(pb in 1:length(pbeta)){
      
      y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, pbeta = pbeta[pb])
      
      ### MFDR (group MCP)
      fit <- grpreg(X, y, group = grp, returnX = TRUE, family = fam, penalty = "grMCP")
      mfdr_res <- grpreg::mfdr(fit)
      mfdr_mod <- max(which(mfdr_res$mFDR <= thr))
      
      truth <- get_truth(fit, ntrue)
      tp_mfdr_gmcp_q[i,pb] <- truth$tp[mfdr_mod]
      
      ### MFDR (group lasso)
      fit <- grpreg(X, y, group = grp, returnX = TRUE, family = fam, penalty = "grLasso")
      mfdr_res <- grpreg::mfdr(fit)
      mfdr_mod <- max(which(mfdr_res$mFDR <= thr))
      
      truth <- get_truth(fit, ntrue)
      tp_mfdr_gl_q[i,pb] <- truth$tp[mfdr_mod]
      
      ## Selective Inference (forward sel)
      fs_res <- groupfs(X, y, grp, 13, sigma = NULL, k = 2, intercept = TRUE, aicstop = 2, verbose = FALSE)
      fs_pv <- groupfsInf(fs_res, verbose = FALSE)
      fs_id <- forwardStop(fs_pv$pv, alpha=thr)
      if(fs_id > 0){
        tp_fs_aic_q[i,pb] = sum(fs_pv$vars[1:fs_id] <= ntrue)
      } else {
        tp_fs_aic_q[i,pb] = 0
      }
      
      ## Selective Inference (linear)
      fit.lar <- tryCatch(lar(rawX,y, maxsteps = 17), error=function(e) NULL)
      sig.est <- tryCatch(estimateSigma(rawX, y), error=function(e) NULL)
      res <- tryCatch(larInf(fit.lar, sigma = sig.est$sigmahat, k = 15), error=function(e) NULL)
      
      
      if (!is.null(res)){
        step.pv <- forwardStop(res$pv, alpha=thr)
        step.spacing <- forwardStop(res$pv.spacing, alpha=thr)
        step.modspacing <- forwardStop(res$pv.modspac, alpha=thr)
        step.covtest <- forwardStop(res$pv.covtest, alpha=thr)
        
        ### Results for main SI test
        if (step.pv == 0){
          true.pv <- 0
        } else {
          true.pv <- sum(res$vars[1:step.pv] %in% 1:ntrue)
        }
      } else {
        ## If Sel Inf fails, then there are zero true positives for that dataset
        true.pv <- 0
      }  
      
      ## Store Sel Inf results
      tp_si_pv_q[i,pb] = true.pv
      
      ### Knock-off Filter
      suppressWarnings(knres <- knockoff.filter(X = rawX, y = y, fdr = thr))
      tp_kn_q[i,pb] <- sum(knres$selected %in% 1:ntrue) # true pos
      
      ## Data-splitting
      idx <- sample(n, n/2)
      X1 <- X[idx,]
      y1 <- y[idx] 
      y2 <- y[-idx]
      
      p1fit <- cv.grpreg(X1, y2, group = grp, returnX = TRUE, family = fam, penalty = "grLasso")
      cvl_id <- which(p1fit$fit$lambda == p1fit$lambda.min)
      p1sel <- unique(grp[which(p1fit$fit$beta[-1,cvl_id] != 0)])
      
      if(length(p1sel) == 0){
        tp_ss_fdr_q[i,pb] = 0
      } else if (length(p1sel) == 1){
        x2 = rawX[-idx,p1sel]
        p2fit <- lm(y2 ~ factor(x2))
        p2test <- anova(p2fit)$`Pr(>F)`[1]
        if(p2test < thr & p1sel <= ntrue){
          tp_ss_fdr_q[i,pb] = 1
        } else {
          tp_ss_fdr_q[i,pb] = 0
        }
      } else {
        X2 <- data.frame(apply(rawX[-idx,p1sel], 2, factor))
        names(X2) <- p1sel
        p2fit <- lm(y2 ~ . , data = X2)
        p2fdr <- p.adjust(anova(p2fit)$`Pr(>F)`[1:ncol(X2)], method = "fdr")
        p2signif <- p1sel[which(p2fdr < thr)]
        tp_ss_fdr_q[i,pb] = sum(p2signif <= ntrue)
      }
      
      
      ## Large-scale testing
      for(j in 1:ngrp){
        tfit <- lm(y ~ X[,which(grp == j)])
        lspv_q[j] <- anova(tfit)$'Pr(>F)'[1]
      }
      tp_ls_fdr_q[i,pb] = sum(p.adjust(lspv_q, method = "fdr")[1:ntrue] <= thr)
      
    }
    print(paste0("Completed = ", 100*i/reps, "%"))
  }
  

  mc_tpr = apply(tp_mfdr_gmcp_q, 2, mean)
  gl_tpr = apply(tp_mfdr_gl_q, 2, mean)
  si_tpr = apply(tp_fs_aic_q, 2, mean)   ## Slightly jitter results that are near zero 
  ss_tpr = apply(tp_ss_fdr_q, 2, mean) + rnorm(ncol(tp_ss_fdr_q), 0, 0.005)
  ls_tpr = apply(tp_ls_fdr_q, 2, mean) 
  sipv_tpr = apply(tp_si_pv_q, 2, mean) + rnorm(ncol(tp_ss_fdr_q), 0, 0.005)
  kn_tpr = apply(tp_kn_q, 2, mean) + rnorm(ncol(tp_ss_fdr_q), 0, 0.005)
  
  pq_df = data.frame(beta = rep(pbeta, times = 7)/sqrt(n), 
                      tp = c(mc_tpr, gl_tpr, si_tpr,
                             ss_tpr, ls_tpr, sipv_tpr, kn_tpr),
                      method = rep(c("mFDR (Grp MCP)", "mFDR (Grp lasso)",
                                     "Selective Inference (FS)", 
                                     "Sample Splitting",
                                     "Univariate Testing",
                                     "Selective Inference (lin)",
                                     "Knock-off Filter (lin)"), each = 5),
                      scenario = "Quadratic")
  
  pq = ggplot(pq_df, aes(x = beta, y = tp, color = method)) + geom_point() + geom_line() + theme_bw() + 
    labs(x = expression(beta), color = " ", y = " ", title = "Quadratic") + 
    scale_color_manual(
      values = c(
        "mFDR (Grp MCP)"            = "#0072B2", # Blue
        "mFDR (Grp lasso)"          = "#56B4E9", # Light blue
        
        "Selective Inference (FS)"  = "#CC79A7", # Purple
        "Selective Inference (lin)" = "#AA4C8F", # Darker purple
        
        "Sample Splitting"          = "#009E73", # Green
        "Univariate Testing"        = "#6DBE9A", # Light green
        
        "Knock-off Filter (lin)"    = "#E69F00"  # Orange
      )
    )
  
  
  
  # plot(pbeta, mc_tpr, type = "b", lty = 2, ylim = c(0,10), 
  #      bty = "n", xlab = expression(beta), ylab = "", main = "Quadratic")
  # lines(pbeta, gl_tpr, type = "b")  
  # lines(pbeta, ss_tpr, col = "red", type = "b")
  # lines(pbeta, ls_tpr, col = "purple", type = "b")
  # lines(pbeta, si_tpr, col = "blue", type = "b")
  # lines(pbeta, sipv_tpr, col = "brown", type = "b")
  # lines(pbeta, kn_tpr, col = "orange", type = "b")

  #### Non-parm (piecewise)
  
  n = 200
  ngrp = 100
  ntrue = 10
  nltype = "piecewise"
  sim_col = "nonparm"
  ctype = "AR"
  rho = 0.0
  pen = "grLasso"
  fam = "gaussian"
  thr = 0.1
  pbeta = c(2,4,6,8,10)*1.4
  
  
  ## Storage objects
  tp_mfdr_gl_l = matrix(NA, nrow = reps, ncol = length(pbeta))
  tp_mfdr_gmcp_l = matrix(NA, nrow = reps, ncol = length(pbeta))
  tp_fs_aic_l = matrix(NA, nrow = reps, ncol = length(pbeta))
  tp_ls_fdr_l = matrix(NA, nrow = reps, ncol = length(pbeta))
  tp_ss_fdr_l = matrix(NA, nrow = reps, ncol = length(pbeta))
  lspv_l = numeric(ngrp)
  tp_si_pv_l = matrix(NA, nrow = reps, ncol = length(pbeta))
  tp_kn_l = matrix(NA, nrow = reps, ncol = length(pbeta))
  
  for(i in 1:reps){
    
    r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                       rho = rho)
    X <- r$X
    grp <- r$grp
    
    for(pb in 1:length(pbeta)){
      
      y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, pbeta = pbeta[pb])
      
      ### MFDR (group MCP)
      fit <- grpreg(X, y, group = grp, returnX = TRUE, family = fam, penalty = "grMCP")
      mfdr_res <- grpreg::mfdr(fit)
      mfdr_mod <- max(which(mfdr_res$mFDR <= thr))
      
      truth <- get_truth(fit, ntrue)
      tp_mfdr_gmcp_l[i,pb] <- truth$tp[mfdr_mod]
      
      ### MFDR (group lasso)
      fit <- grpreg(X, y, group = grp, returnX = TRUE, family = fam, penalty = "grLasso")
      mfdr_res <- grpreg::mfdr(fit)
      mfdr_mod <- max(which(mfdr_res$mFDR <= thr))
      
      truth <- get_truth(fit, ntrue)
      tp_mfdr_gl_l[i,pb] <- truth$tp[mfdr_mod]
      
      ## Selective Inference (forward sel)
      fs_res <- groupfs(X, y, grp, 13, sigma = NULL, k = 2, intercept = TRUE, aicstop = 2, verbose = FALSE)
      fs_pv <- groupfsInf(fs_res, verbose = FALSE)
      fs_id <- forwardStop(fs_pv$pv, alpha=thr)
      if(fs_id > 0){
        tp_fs_aic_l[i,pb] = sum(fs_pv$vars[1:fs_id] <= ntrue)
      } else {
        tp_fs_aic_l[i,pb] = 0
      }
      
      ## Selective Inference (linear)
      fit.lar <- tryCatch(lar(rawX,y, maxsteps = 17), error=function(e) NULL)
      sig.est <- tryCatch(estimateSigma(rawX, y), error=function(e) NULL)
      res <- tryCatch(larInf(fit.lar, sigma = sig.est$sigmahat, k = 15), error=function(e) NULL)
      
      
      if (!is.null(res)){
        step.pv <- forwardStop(res$pv, alpha=thr)
        step.spacing <- forwardStop(res$pv.spacing, alpha=thr)
        step.modspacing <- forwardStop(res$pv.modspac, alpha=thr)
        step.covtest <- forwardStop(res$pv.covtest, alpha=thr)
        
        ### Results for each
        if (step.pv == 0){
          true.pv <- 0
        } else {
          true.pv <- sum(res$vars[1:step.pv] %in% 1:ntrue)
        }
      } else {
        ## If Sel Inf fails, then there are zero true positives for that dataset
        true.pv <- 0
      }  
      
      ## Store Sel Inf results
      tp_si_pv_l[i,pb] = true.pv
      
      ### Knock-off Filter
      suppressWarnings(knres <- knockoff.filter(X = rawX, y = y, fdr = thr))
      tp_kn_l[i,pb] <- sum(knres$selected %in% 1:ntrue) # true pos
      
      ## Data-splitting
      idx <- sample(n, n/2)
      X1 <- X[idx,]
      y1 <- y[idx] 
      y2 <- y[-idx]
      
      p1fit <- cv.grpreg(X1, y2, group = grp, returnX = TRUE, family = fam, penalty = "grLasso")
      cvl_id <- which(p1fit$fit$lambda == p1fit$lambda.min)
      p1sel <- unique(grp[which(p1fit$fit$beta[-1,cvl_id] != 0)])
      
      if(length(p1sel) == 0){
        tp_ss_fdr_l[i,pb] = 0
      } else if (length(p1sel) == 1){
        x2 = rawX[-idx,p1sel]
        p2fit <- lm(y2 ~ factor(x2))
        p2test <- anova(p2fit)$`Pr(>F)`[1]
        if(p2test < thr & p1sel <= ntrue){
          tp_ss_fdr_l[i,pb] = 1
        } else {
          tp_ss_fdr_l[i,pb] = 0
        }
      } else {
        X2 <- data.frame(apply(rawX[-idx,p1sel], 2, factor))
        names(X2) <- p1sel
        p2fit <- lm(y2 ~ . , data = X2)
        p2fdr <- p.adjust(anova(p2fit)$`Pr(>F)`[1:ncol(X2)], method = "fdr")
        p2signif <- p1sel[which(p2fdr < thr)]
        tp_ss_fdr_l[i,pb] = sum(p2signif <= ntrue)
      }
      
      
      ## Large-scale testing
      for(j in 1:ngrp){
        tfit <- lm(y ~ X[,which(grp == j)])
        lspv_l[j] <- anova(tfit)$'Pr(>F)'[1]
      }
      tp_ls_fdr_l[i,pb] = sum(p.adjust(lspv_l, method = "fdr")[1:ntrue] <= thr)
      
    }
    print(paste0("Completed = ", 100*i/reps, "%"))
  }
  
  
  mc_tpr = apply(tp_mfdr_gmcp_l, 2, mean)
  gl_tpr = apply(tp_mfdr_gl_l, 2, mean)
  si_tpr = apply(tp_fs_aic_l, 2, mean)  ## Slightly jitter methods near zero
  ss_tpr = apply(tp_ss_fdr_l, 2, mean) + rnorm(ncol(tp_ss_fdr_l), 0, 0.005)
  ls_tpr = apply(tp_ls_fdr_l, 2, mean)
  sipv_tpr = apply(tp_si_pv_l, 2, mean) + rnorm(ncol(tp_ss_fdr_l), 0, 0.005)
  kn_tpr = apply(tp_kn_l, 2, mean) + rnorm(ncol(tp_ss_fdr_l), 0, 0.005)
  
  
  pnl_df = data.frame(beta = rep(pbeta, times = 7)/sqrt(n), 
                     tp = c(mc_tpr, gl_tpr, si_tpr,
                            ss_tpr, ls_tpr, sipv_tpr, kn_tpr),
                     method = rep(c("mFDR (Grp MCP)", "mFDR (Grp lasso)",
                                    "Selective Inference (FS)", 
                                    "Sample Splitting",
                                    "Univariate Testing",
                                    "Selective Inference (lin)",
                                    "Knock-off Filter (lin)"), each = 5),
                     scenario = "Quadratic")
  
  pnl = ggplot(pnl_df, aes(x = beta, y = tp, color = method)) + geom_point() + geom_line() + theme_bw() + 
    labs(x = expression(beta), color = " ", y = " ", title = "Piecewise") + 
    scale_color_manual(
      values = c(
        "mFDR (Grp MCP)"            = "#0072B2", # Blue
        "mFDR (Grp lasso)"          = "#56B4E9", # Light blue
        
        "Selective Inference (FS)"  = "#CC79A7", # Purple
        "Selective Inference (lin)" = "#AA4C8F", # Darker purple
        
        "Sample Splitting"          = "#009E73", # Green
        "Univariate Testing"        = "#6DBE9A", # Light green
        
        "Knock-off Filter (lin)"    = "#E69F00"  # Orange
      )
    )
  
  
  
  # 
  # 
  # plot(pbeta, mc_tpr, type = "b", lty = 2, ylim = c(0,10), 
  #      bty = "n", xlab = expression(beta), ylab = "", main = "Piecewise Linear")
  # lines(pbeta, gl_tpr, type = "b")
  # lines(pbeta, ss_tpr, col = "red", type = "b")
  # lines(pbeta, ls_tpr, col = "purple", type = "b")
  # lines(pbeta, si_tpr, col = "blue", type = "b")
  # lines(pbeta, sipv_tpr, col = "brown", type = "b")
  # lines(pbeta, kn_tpr, col = "orange", type = "b")
  # 
  # 
  # par(mai=c(0,0,0,0))
  # plot.new()
  # legend("center", legend = c("mFDR (Grp MCP)", "mFDR (Grp lasso)", "Sel Inf (FS)", "Sample Splitting",
  #                             "Large-Scale Testing", "Sel Inf (Linear)", "Knockoff (Linear)"),
  #         lty = c(2,1,1,1,1,1,1),
  #         col = c("black", "black", "blue", "red", "purple", "brown", "orange"), bty = "n", ncol = 4)
  
  
  
 
  ## Old plot code below ->
  
  # layout(matrix(c(1,2,3,4,4,4), ncol=3, byrow=TRUE), heights=c(4, 0.6))
  # par(mar = c(5.1, 4.1, 4.1, 2.1), oma = c(0,0,0,0))
  # 
  # plot(pbeta, mc_tpr, type = "b", lty = 2, ylim = c(0,10), 
  #      bty = "n", xlab = expression(beta), ylab = "True Positives (of 10)", main = "Factors (k = 3)")
  # lines(pbeta, gl_tpr, type = "b")
  # lines(pbeta, si_tpr, col = "green", type = "b")
  # lines(pbeta, ss_tpr, col = "red", type = "b")
  # lines(pbeta, ls_tpr, col = "purple", type = "b")
  # lines(pbeta, si_tpr, col = "blue", type = "b")
  
  ###################################################
  ## SECOND ROW -> AR rho = 0.9
  ###################################################
  
  # 
  # n = 200
  # ngrp = 100
  # k = 3
  # ntrue = 10
  # ctype = "AR"
  # rho = 0.9
  # pen = "grLasso"
  # fam = "gaussian"
  # thr = 0.1
  # pbeta = c(2,4,6,8,10)/sqrt(n)
  # 
  # tp_mfdr_gl = matrix(NA, nrow = reps, ncol = length(pbeta))
  # tp_mfdr_gmcp = matrix(NA, nrow = reps, ncol = length(pbeta))
  # tp_fs_aic = matrix(NA, nrow = reps, ncol = length(pbeta))
  # tp_ls_fdr = matrix(NA, nrow = reps, ncol = length(pbeta))
  # tp_ss_fdr = matrix(NA, nrow = reps, ncol = length(pbeta))
  # lspv = numeric(ngrp)
  # 
  # for(i in 1:reps){
  #   
  #   r <- genX_fac_grp(n = n, ngrp = ngrp, k = k, ctype = ctype, rho = rho)
  #   X <- r$X
  #   rawX <- r$rawX
  #   grp <- r$grp
  #   
  #   for(pb in 1:length(pbeta)){
  #     
  #     y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = pbeta[pb])
  #     
  #     ### MFDR (group MCP)
  #     fit <- grpreg(X, y, group = grp, returnX = TRUE, family = fam, penalty = "grMCP")
  #     mfdr_res <- grpreg::mfdr(fit)
  #     mfdr_mod <- max(which(mfdr_res$mFDR <= thr))
  #     
  #     truth <- get_truth(fit, ntrue)
  #     tp_mfdr_gmcp[i,pb] <- truth$tp[mfdr_mod]
  #     
  #     ### MFDR (group lasso)
  #     fit <- grpreg(X, y, group = grp, returnX = TRUE, family = fam, penalty = "grLasso")
  #     mfdr_res <- grpreg::mfdr(fit)
  #     mfdr_mod <- max(which(mfdr_res$mFDR <= thr))
  #     
  #     truth <- get_truth(fit, ntrue)
  #     tp_mfdr_gl[i,pb] <- truth$tp[mfdr_mod]
  #     
  #     ## Selective Inference (forward sel)
  #     fs_res <- groupfs(X, y, grp, 13, sigma = NULL, k = 2, intercept = TRUE, aicstop = 2, verbose = FALSE)
  #     fs_pv <- groupfsInf(fs_res, verbose = FALSE)
  #     fs_id <- forwardStop(fs_pv$pv, alpha=0.1)
  #     if(fs_id > 0){
  #       tp_fs_aic[i,pb] = sum(fs_pv$vars[1:fs_id] <= ntrue)
  #     } else {
  #       tp_fs_aic[i,pb] = 0
  #     }
  #     
  #     ## Data-splitting
  #     idx <- sample(n, n/2)
  #     X1 <- X[idx,]
  #     y1 <- y[idx] 
  #     y2 <- y[-idx]
  #     
  #     p1fit <- cv.grpreg(X1, y2, group = grp, returnX = TRUE, family = fam, penalty = "grLasso")
  #     cvl_id <- which(p1fit$fit$lambda == p1fit$lambda.min)
  #     p1sel <- unique(grp[which(p1fit$fit$beta[-1,cvl_id] != 0)])
  #     
  #     if(length(p1sel) == 0){
  #       tp_ss_fdr[i,pb] = 0
  #     } else if (length(p1sel) == 1){
  #       x2 = rawX[-idx,p1sel]
  #       p2fit <- lm(y2 ~ factor(x2))
  #       p2test <- anova(p2fit)$`Pr(>F)`[1]
  #       if(p2test < thr & p1sel <= ntrue){
  #         tp_ss_fdr[i,pb] = 1
  #       } else {
  #         tp_ss_fdr[i,pb] = 0
  #       }
  #     } else {
  #       X2 <- data.frame(apply(rawX[-idx,p1sel], 2, factor))
  #       names(X2) <- p1sel
  #       p2fit <- lm(y2 ~ . , data = X2)
  #       p2fdr <- p.adjust(anova(p2fit)$`Pr(>F)`[1:ncol(X2)], method = "fdr")
  #       p2signif <- p1sel[which(p2fdr < thr)]
  #       tp_ss_fdr[i,pb] = sum(p2signif <= ntrue)
  #     }
  #     
  #     
  #     ## Large-scale testing
  #     for(j in 1:ngrp){
  #       tfit <- lm(y ~ X[,which(grp == j)])
  #       lspv[j] <- anova(tfit)$'Pr(>F)'[1]
  #     }
  #     tp_ls_fdr[i,pb] = sum(p.adjust(lspv, method = "fdr")[1:ntrue] <= thr)
  #     
  #   }
  #   print(paste0("Completed = ", 100*i/reps, "%"))
  # }
  # 
  # mc_tpr = apply(tp_mfdr_gmcp, 2, mean, na.rm = TRUE)
  # gl_tpr = apply(tp_mfdr_gl, 2, mean, na.rm = TRUE)
  # si_tpr = apply(tp_fs_aic, 2, mean, na.rm = TRUE)
  # ss_tpr = apply(tp_ss_fdr, 2, mean, na.rm = TRUE)
  # ls_tpr = apply(tp_ls_fdr, 2, mean, na.rm = TRUE)
  # 
  # pf3_df_corr = data.frame(beta = rep(c(2,4,6,8,10), times = 5), 
  #                     tp = c(mc_tpr, gl_tpr, si_tpr,
  #                            ss_tpr, ls_tpr),
  #                     method = rep(c("mFDR (Grp MCP)", "mFDR (Grp lasso)",
  #                                    "Selective Inference (FS)", 
  #                                    "Sample Splitting",
  #                                    "Univariate Testing"), each = 5),
  #                     scenario = "Factor (k=3)")
  # 
  # pf3_corr = ggplot(pf3_df_corr, aes(x = beta, y = tp, color = method)) + geom_point() + geom_line() + theme_bw() + 
  #   labs(x = expression(beta), y = "True Positives (of 10)", color = " ", title = "Categorical (k=3)")
  # 
  # 
  # n = 200
  # ngrp = 100
  # ntrue = 10
  # nltype = "quadratic"
  # sim_col = "nonparm"
  # ctype = "AR"
  # rho = 0.9
  # pen = "grLasso"
  # fam = "gaussian"
  # thr = 0.1
  # pbeta = c(2,4,6,8,10)
  # 
  # ## Storage objects
  # tp_mfdr_gl_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  # tp_mfdr_gmcp_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  # tp_fs_aic_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  # tp_ls_fdr_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  # tp_ss_fdr_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  # lspv_q = numeric(ngrp)
  # tp_si_pv_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  # tp_si_sp_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  # tp_si_msp_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  # tp_si_ct_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  # tp_kn_q = matrix(NA, nrow = reps, ncol = length(pbeta))
  # 
  # for(i in 1:reps){
  #   
  #   r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
  #                      rho = rho)
  #   X <- r$X
  #   grp <- r$grp
  #   
  #   for(pb in 1:length(pbeta)){
  #     
  #     y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, pbeta = pbeta[pb])
  #     
  #     ### MFDR (group MCP)
  #     fit <- grpreg(X, y, group = grp, returnX = TRUE, family = fam, penalty = "grMCP")
  #     mfdr_res <- grpreg::mfdr(fit)
  #     mfdr_mod <- max(which(mfdr_res$mFDR <= thr))
  #     
  #     truth <- get_truth(fit, ntrue)
  #     tp_mfdr_gmcp_q[i,pb] <- truth$tp[mfdr_mod]
  #     
  #     ### MFDR (group lasso)
  #     fit <- grpreg(X, y, group = grp, returnX = TRUE, family = fam, penalty = "grLasso")
  #     mfdr_res <- grpreg::mfdr(fit)
  #     mfdr_mod <- max(which(mfdr_res$mFDR <= thr))
  #     
  #     truth <- get_truth(fit, ntrue)
  #     tp_mfdr_gl_q[i,pb] <- truth$tp[mfdr_mod]
  #     
  #     ## Selective Inference (forward sel)
  #     fs_res <- groupfs(X, y, grp, 13, sigma = NULL, k = 2, intercept = TRUE, aicstop = 2, verbose = FALSE)
  #     fs_pv <- groupfsInf(fs_res, verbose = FALSE)
  #     fs_id <- forwardStop(fs_pv$pv, alpha=thr)
  #     if(fs_id > 0){
  #       tp_fs_aic_q[i,pb] = sum(fs_pv$vars[1:fs_id] <= ntrue)
  #     } else {
  #       tp_fs_aic_q[i,pb] = 0
  #     }
  #     
  #     ## Selective Inference (linear)
  #     fit.lar <- tryCatch(lar(rawX,y, maxsteps = 17), error=function(e) NULL)
  #     sig.est <- tryCatch(estimateSigma(rawX, y), error=function(e) NULL)
  #     res <- tryCatch(larInf(fit.lar, sigma = sig.est$sigmahat, k = 15), error=function(e) NULL)
  #     
  #     
  #     if (!is.null(res)){
  #       step.pv <- forwardStop(res$pv, alpha=thr)
  #       step.spacing <- forwardStop(res$pv.spacing, alpha=thr)
  #       step.modspacing <- forwardStop(res$pv.modspac, alpha=thr)
  #       step.covtest <- forwardStop(res$pv.covtest, alpha=thr)
  #       
  #       ### Results for main SI test
  #       if (step.pv == 0){
  #         true.pv <- 0
  #       } else {
  #         true.pv <- sum(res$vars[1:step.pv] %in% 1:ntrue)
  #       }
  #     } else {
  #       ## If Sel Inf fails, then there are zero true positives for that dataset
  #       true.pv <- 0
  #     }  
  #     
  #     ## Store Sel Inf results
  #     tp_si_pv_q[i,pb] = true.pv
  #     
  #     ### Knock-off Filter
  #     suppressWarnings(knres <- knockoff.filter(X = rawX, y = y, fdr = thr))
  #     tp_kn_q[i,pb] <- sum(knres$selected %in% 1:ntrue) # true pos
  #     
  #     ## Data-splitting
  #     idx <- sample(n, n/2)
  #     X1 <- X[idx,]
  #     y1 <- y[idx] 
  #     y2 <- y[-idx]
  #     
  #     p1fit <- cv.grpreg(X1, y2, group = grp, returnX = TRUE, family = fam, penalty = "grLasso")
  #     cvl_id <- which(p1fit$fit$lambda == p1fit$lambda.min)
  #     p1sel <- unique(grp[which(p1fit$fit$beta[-1,cvl_id] != 0)])
  #     
  #     if(length(p1sel) == 0){
  #       tp_ss_fdr_q[i,pb] = 0
  #     } else if (length(p1sel) == 1){
  #       x2 = rawX[-idx,p1sel]
  #       p2fit <- lm(y2 ~ factor(x2))
  #       p2test <- anova(p2fit)$`Pr(>F)`[1]
  #       if(p2test < thr & p1sel <= ntrue){
  #         tp_ss_fdr_q[i,pb] = 1
  #       } else {
  #         tp_ss_fdr_q[i,pb] = 0
  #       }
  #     } else {
  #       X2 <- data.frame(apply(rawX[-idx,p1sel], 2, factor))
  #       names(X2) <- p1sel
  #       p2fit <- lm(y2 ~ . , data = X2)
  #       p2fdr <- p.adjust(anova(p2fit)$`Pr(>F)`[1:ncol(X2)], method = "fdr")
  #       p2signif <- p1sel[which(p2fdr < thr)]
  #       tp_ss_fdr_q[i,pb] = sum(p2signif <= ntrue)
  #     }
  #     
  #     
  #     ## Large-scale testing
  #     for(j in 1:ngrp){
  #       tfit <- lm(y ~ X[,which(grp == j)])
  #       lspv_q[j] <- anova(tfit)$'Pr(>F)'[1]
  #     }
  #     tp_ls_fdr_q[i,pb] = sum(p.adjust(lspv_q, method = "fdr")[1:ntrue] <= thr)
  #     
  #   }
  #   print(paste0("Completed = ", 100*i/reps, "%"))
  # }
  # 
  # 
  # mc_tpr = apply(tp_mfdr_gmcp_q, 2, mean)
  # gl_tpr = apply(tp_mfdr_gl_q, 2, mean)
  # si_tpr = apply(tp_fs_aic_q, 2, mean)   ## Slightly jitter results that are near zero 
  # ss_tpr = apply(tp_ss_fdr_q, 2, mean) + rnorm(ncol(tp_ss_fdr_q), 0, 0.005)
  # ls_tpr = apply(tp_ls_fdr_q, 2, mean) 
  # sipv_tpr = apply(tp_si_pv_q, 2, mean) + rnorm(ncol(tp_ss_fdr_q), 0, 0.005)
  # kn_tpr = apply(tp_kn_q, 2, mean) + rnorm(ncol(tp_ss_fdr_q), 0, 0.005)
  # 
  # pq_df_corr = data.frame(beta = rep(c(2,4,6,8,10), times = 7), 
  #                    tp = c(mc_tpr, gl_tpr, si_tpr,
  #                           ss_tpr, ls_tpr, sipv_tpr, kn_tpr),
  #                    method = rep(c("mFDR (Grp MCP)", "mFDR (Grp lasso)",
  #                                   "Selective Inference (FS)", 
  #                                   "Sample Splitting",
  #                                   "Univariate Testing",
  #                                   "Selective Inference (lin)",
  #                                   "Knock-off Filter (lin)"), each = 5),
  #                    scenario = "Quadratic")
  # 
  # pq_corr = ggplot(pq_df_corr, aes(x = beta, y = tp, color = method)) + geom_point() + geom_line() + theme_bw() + 
  #   labs(x = expression(beta), color = " ", y = " ", title = "Quadratic")
  # 
  # 
  # 
  # # plot(pbeta, mc_tpr, type = "b", lty = 2, ylim = c(0,10), 
  # #      bty = "n", xlab = expression(beta), ylab = "", main = "Quadratic")
  # # lines(pbeta, gl_tpr, type = "b")  
  # # lines(pbeta, ss_tpr, col = "red", type = "b")
  # # lines(pbeta, ls_tpr, col = "purple", type = "b")
  # # lines(pbeta, si_tpr, col = "blue", type = "b")
  # # lines(pbeta, sipv_tpr, col = "brown", type = "b")
  # # lines(pbeta, kn_tpr, col = "orange", type = "b")
  # 
  # #### Non-parm (piecewise)
  # 
  # n = 200
  # ngrp = 100
  # ntrue = 10
  # nltype = "piecewise"
  # sim_col = "nonparm"
  # ctype = "AR"
  # rho = 0.9
  # pen = "grLasso"
  # fam = "gaussian"
  # thr = 0.1
  # pbeta = c(2,4,6,8,10)
  # 
  # 
  # ## Storage objects
  # tp_mfdr_gl_l = matrix(NA, nrow = reps, ncol = length(pbeta))
  # tp_mfdr_gmcp_l = matrix(NA, nrow = reps, ncol = length(pbeta))
  # tp_fs_aic_l = matrix(NA, nrow = reps, ncol = length(pbeta))
  # tp_ls_fdr_l = matrix(NA, nrow = reps, ncol = length(pbeta))
  # tp_ss_fdr_l = matrix(NA, nrow = reps, ncol = length(pbeta))
  # lspv_l = numeric(ngrp)
  # tp_si_pv_l = matrix(NA, nrow = reps, ncol = length(pbeta))
  # tp_kn_l = matrix(NA, nrow = reps, ncol = length(pbeta))
  # 
  # for(i in 1:reps){
  #   
  #   r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
  #                      rho = rho)
  #   X <- r$X
  #   grp <- r$grp
  #   
  #   for(pb in 1:length(pbeta)){
  #     
  #     y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, pbeta = pbeta[pb])
  #     
  #     ### MFDR (group MCP)
  #     fit <- grpreg(X, y, group = grp, returnX = TRUE, family = fam, penalty = "grMCP")
  #     mfdr_res <- grpreg::mfdr(fit)
  #     mfdr_mod <- max(which(mfdr_res$mFDR <= thr))
  #     
  #     truth <- get_truth(fit, ntrue)
  #     tp_mfdr_gmcp_l[i,pb] <- truth$tp[mfdr_mod]
  #     
  #     ### MFDR (group lasso)
  #     fit <- grpreg(X, y, group = grp, returnX = TRUE, family = fam, penalty = "grLasso")
  #     mfdr_res <- grpreg::mfdr(fit)
  #     mfdr_mod <- max(which(mfdr_res$mFDR <= thr))
  #     
  #     truth <- get_truth(fit, ntrue)
  #     tp_mfdr_gl_l[i,pb] <- truth$tp[mfdr_mod]
  #     
  #     ## Selective Inference (forward sel)
  #     fs_res <- groupfs(X, y, grp, 13, sigma = NULL, k = 2, intercept = TRUE, aicstop = 2, verbose = FALSE)
  #     fs_pv <- groupfsInf(fs_res, verbose = FALSE)
  #     fs_id <- forwardStop(fs_pv$pv, alpha=thr)
  #     if(fs_id > 0){
  #       tp_fs_aic_l[i,pb] = sum(fs_pv$vars[1:fs_id] <= ntrue)
  #     } else {
  #       tp_fs_aic_l[i,pb] = 0
  #     }
  #     
  #     ## Selective Inference (linear)
  #     fit.lar <- tryCatch(lar(rawX,y, maxsteps = 17), error=function(e) NULL)
  #     sig.est <- tryCatch(estimateSigma(rawX, y), error=function(e) NULL)
  #     res <- tryCatch(larInf(fit.lar, sigma = sig.est$sigmahat, k = 15), error=function(e) NULL)
  #     
  #     
  #     if (!is.null(res)){
  #       step.pv <- forwardStop(res$pv, alpha=thr)
  #       step.spacing <- forwardStop(res$pv.spacing, alpha=thr)
  #       step.modspacing <- forwardStop(res$pv.modspac, alpha=thr)
  #       step.covtest <- forwardStop(res$pv.covtest, alpha=thr)
  #       
  #       ### Results for each
  #       if (step.pv == 0){
  #         true.pv <- 0
  #       } else {
  #         true.pv <- sum(res$vars[1:step.pv] %in% 1:ntrue)
  #       }
  #     } else {
  #       ## If Sel Inf fails, then there are zero true positives for that dataset
  #       true.pv <- 0
  #     }  
  #     
  #     ## Store Sel Inf results
  #     tp_si_pv_l[i,pb] = true.pv
  #     
  #     ### Knock-off Filter
  #     suppressWarnings(knres <- knockoff.filter(X = rawX, y = y, fdr = thr))
  #     tp_kn_l[i,pb] <- sum(knres$selected %in% 1:ntrue) # true pos
  #     
  #     ## Data-splitting
  #     idx <- sample(n, n/2)
  #     X1 <- X[idx,]
  #     y1 <- y[idx] 
  #     y2 <- y[-idx]
  #     
  #     p1fit <- cv.grpreg(X1, y2, group = grp, returnX = TRUE, family = fam, penalty = "grLasso")
  #     cvl_id <- which(p1fit$fit$lambda == p1fit$lambda.min)
  #     p1sel <- unique(grp[which(p1fit$fit$beta[-1,cvl_id] != 0)])
  #     
  #     if(length(p1sel) == 0){
  #       tp_ss_fdr_l[i,pb] = 0
  #     } else if (length(p1sel) == 1){
  #       x2 = rawX[-idx,p1sel]
  #       p2fit <- lm(y2 ~ factor(x2))
  #       p2test <- anova(p2fit)$`Pr(>F)`[1]
  #       if(p2test < thr & p1sel <= ntrue){
  #         tp_ss_fdr_l[i,pb] = 1
  #       } else {
  #         tp_ss_fdr_l[i,pb] = 0
  #       }
  #     } else {
  #       X2 <- data.frame(apply(rawX[-idx,p1sel], 2, factor))
  #       names(X2) <- p1sel
  #       p2fit <- lm(y2 ~ . , data = X2)
  #       p2fdr <- p.adjust(anova(p2fit)$`Pr(>F)`[1:ncol(X2)], method = "fdr")
  #       p2signif <- p1sel[which(p2fdr < thr)]
  #       tp_ss_fdr_l[i,pb] = sum(p2signif <= ntrue)
  #     }
  #     
  #     
  #     ## Large-scale testing
  #     for(j in 1:ngrp){
  #       tfit <- lm(y ~ X[,which(grp == j)])
  #       lspv_l[j] <- anova(tfit)$'Pr(>F)'[1]
  #     }
  #     tp_ls_fdr_l[i,pb] = sum(p.adjust(lspv_l, method = "fdr")[1:ntrue] <= thr)
  #     
  #   }
  #   print(paste0("Completed = ", 100*i/reps, "%"))
  # }
  # 
  # 
  # mc_tpr = apply(tp_mfdr_gmcp_l, 2, mean)
  # gl_tpr = apply(tp_mfdr_gl_l, 2, mean)
  # si_tpr = apply(tp_fs_aic_l, 2, mean)  ## Slightly jitter methods near zero
  # ss_tpr = apply(tp_ss_fdr_l, 2, mean) + rnorm(ncol(tp_ss_fdr_l), 0, 0.005)
  # ls_tpr = apply(tp_ls_fdr_l, 2, mean)
  # sipv_tpr = apply(tp_si_pv_l, 2, mean) + rnorm(ncol(tp_ss_fdr_l), 0, 0.005)
  # kn_tpr = apply(tp_kn_l, 2, mean) + rnorm(ncol(tp_ss_fdr_l), 0, 0.005)
  # 
  # 
  # pnl_df_corr = data.frame(beta = rep(c(2,4,6,8,10), times = 7), 
  #                     tp = c(mc_tpr, gl_tpr, si_tpr,
  #                            ss_tpr, ls_tpr, sipv_tpr, kn_tpr),
  #                     method = rep(c("mFDR (Grp MCP)", "mFDR (Grp lasso)",
  #                                    "Selective Inference (FS)", 
  #                                    "Sample Splitting",
  #                                    "Univariate Testing",
  #                                    "Selective Inference (lin)",
  #                                    "Knock-off Filter (lin)"), each = 5),
  #                     scenario = "Quadratic")
  # 
  # pnl_corr = ggplot(pnl_df_corr, aes(x = beta, y = tp, color = method)) + geom_point() + geom_line() + theme_bw() + 
  #   labs(x = expression(beta), color = " ", y = " ", title = "Piecewise")
  # 
  
  ### Combine for plot
  
  my_legend <- get_legend(pnl + theme(legend.direction = "horizontal",legend.box = "horizontal",legend.position = "bottom") +
                            guides(color = guide_legend(nrow = 2))) 
  
  
  final_fig4 = grid.arrange(pf3 + theme(legend.position = "none"),
                            pq + theme(legend.position = "none"),
                            pnl + theme(legend.position = "none"),
                           # pf3_corr + theme(legend.position = "none"),
                           # pq_corr + theme(legend.position = "none"),
                          #  pnl_corr + theme(legend.position = "none"),
                            my_legend,
                            layout_matrix = rbind(c(1, 2, 3),
                           #                       c(4, 5, 6),
                                                  c(4, 4, 4)),
                            heights = c(4,1))
  
  return(final_fig4)
  
}


#######################################
##
##      Function to make Table 1
##
#######################################


make_table1 <- function(reps = 100){
  
set.seed(12345)

n = 200
ngrp = 100
ntrue = 10
nltype = "piecewise"
sim_col = "nonparm"
ctype = "AR"
rho = 0.0
pen = "grLasso"
fam = "gaussian"
thr1 = 0.1
thr2 = 0.2
pbeta = c(0.25, 0.5, 0.75)


tp_mfdr_gmcp_l1 = tp_mfdr_gmcp_l2 = tp_mfdr_gmcp_cv = tp_mfdr_gmcp_cv1se = matrix(NA, nrow = reps, ncol = length(pbeta))
fp_mfdr_gmcp_l1 = fp_mfdr_gmcp_l2 = fp_mfdr_gmcp_cv = fp_mfdr_gmcp_cv1se = matrix(NA, nrow = reps, ncol = length(pbeta))
fdr_mfdr_gmcp_l1 = fdr_mfdr_gmcp_l2 = fdr_mfdr_gmcp_cv = fdr_mfdr_gmcp_cv1se = matrix(NA, nrow = reps, ncol = length(pbeta))

tp_mfdr_gl_l1 = tp_mfdr_gl_l2 = tp_mfdr_gl_cv = tp_mfdr_gl_cv1se = matrix(NA, nrow = reps, ncol = length(pbeta))
fp_mfdr_gl_l1 = fp_mfdr_gl_l2 = fp_mfdr_gl_cv = fp_mfdr_gl_cv1se = matrix(NA, nrow = reps, ncol = length(pbeta))
fdr_mfdr_gl_l1 = fdr_mfdr_gl_l2 = fdr_mfdr_gl_cv = fdr_mfdr_gl_cv1se = matrix(NA, nrow = reps, ncol = length(pbeta))


for(i in 1:reps){
  
  r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                     rho = rho)
  X <- r$X
  grp <- r$grp
  
  
  for(pb in 1:length(pbeta)){
    
    y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, pbeta = pbeta[pb])
    
    ### Grp MCP
    cv.fit <- cv.grpreg(X, y, group = grp, returnX = TRUE, family = fam, penalty = "grMCP")
    fit <- cv.fit$fit
    mfdr_res <- grpreg::mfdr(fit)
    mfdr_l1 <- max(which(mfdr_res$mFDR <= thr1))
    mfdr_l2 <- max(which(mfdr_res$mFDR <= thr2))
    cvl <- which(cv.fit$lambda  == cv.fit$lambda.min)
    cv1se <- min(which(cv.fit$cve < cv.fit$cve[cvl] + cv.fit$cvse[cvl]))
    truth <- get_truth(fit, ntrue)
    
    
    tp_mfdr_gmcp_l1[i,pb] <- truth$tp[mfdr_l1]
    fp_mfdr_gmcp_l1[i,pb] <- truth$fp[mfdr_l1]
    fdr_mfdr_gmcp_l1[i,pb] <- truth$real_fdr[mfdr_l1]
    
    
    tp_mfdr_gmcp_l2[i,pb] <- truth$tp[mfdr_l2]
    fp_mfdr_gmcp_l2[i,pb] <- truth$fp[mfdr_l2]
    fdr_mfdr_gmcp_l2[i,pb] <- truth$real_fdr[mfdr_l2]
    
    
    tp_mfdr_gmcp_cv[i,pb] <- truth$tp[cvl]
    fp_mfdr_gmcp_cv[i,pb] <- truth$fp[cvl]
    fdr_mfdr_gmcp_cv[i,pb] <- truth$real_fdr[cvl]
    
    tp_mfdr_gmcp_cv1se[i,pb] <- truth$tp[cv1se]
    fp_mfdr_gmcp_cv1se[i,pb] <- truth$fp[cv1se]
    fdr_mfdr_gmcp_cv1se[i,pb] <- truth$real_fdr[cv1se]
    
    ## Grp Lasso
    cv.fit <- cv.grpreg(X, y, group = grp, returnX = TRUE, family = fam, penalty = "grLasso")
    fit <- cv.fit$fit
    mfdr_res <- grpreg::mfdr(fit)
    mfdr_l1 <- max(which(mfdr_res$mFDR <= thr1))
    mfdr_l2 <- max(which(mfdr_res$mFDR <= thr2))
    cvl <- which(cv.fit$lambda  == cv.fit$lambda.min)
    cv1se <- min(which(cv.fit$cve < cv.fit$cve[cvl] + cv.fit$cvse[cvl]))
    truth <- get_truth(fit, ntrue)
    
    
    tp_mfdr_gl_l1[i,pb] <- truth$tp[mfdr_l1]
    fp_mfdr_gl_l1[i,pb] <- truth$fp[mfdr_l1]
    fdr_mfdr_gl_l1[i,pb] <- truth$real_fdr[mfdr_l1]
    
    
    tp_mfdr_gl_l2[i,pb] <- truth$tp[mfdr_l2]
    fp_mfdr_gl_l2[i,pb] <- truth$fp[mfdr_l2]
    fdr_mfdr_gl_l2[i,pb] <- truth$real_fdr[mfdr_l2]
    
    
    tp_mfdr_gl_cv[i,pb] <- truth$tp[cvl]
    fp_mfdr_gl_cv[i,pb] <- truth$fp[cvl]
    fdr_mfdr_gl_cv[i,pb] <- truth$real_fdr[cvl]
    
    tp_mfdr_gl_cv1se[i,pb] <- truth$tp[cv1se]
    fp_mfdr_gl_cv1se[i,pb] <- truth$fp[cv1se]
    fdr_mfdr_gl_cv1se[i,pb] <- truth$real_fdr[cv1se]
    
  }
  
}


resdf_tp <- data.frame(
  beta = rep(pbeta, 8),
  lambda = rep(c("Grp MCP (10%)", "Grp MCP (20%)", "Grp MCP (CV)", "Grp MCP (1se)",
                 "Grp lasso (10%)", "Grp lasso(20%)", "Grp lasso (CV)", "Grp lasso (1se)"), each = length(pbeta)),
  true_pos = c(apply(tp_mfdr_gmcp_l1, 2, mean), apply(tp_mfdr_gmcp_l2, 2, mean), apply(tp_mfdr_gmcp_cv, 2, mean), apply(tp_mfdr_gmcp_cv1se, 2, mean),
               apply(tp_mfdr_gl_l1, 2, mean), apply(tp_mfdr_gl_l2, 2, mean), apply(tp_mfdr_gl_cv, 2, mean), apply(tp_mfdr_gl_cv1se, 2, mean))
)

resdf_fp <- data.frame(
  beta = rep(pbeta, 8),
  lambda = rep(c("Grp MCP (10%)", "Grp MCP (20%)", "Grp MCP (CV)", "Grp MCP (1se)",
                 "Grp lasso (10%)", "Grp lasso(20%)", "Grp lasso (CV)", "Grp lasso (1se)"), each = length(pbeta)),
  false_pos = c(apply(fp_mfdr_gmcp_l1, 2, mean), apply(fp_mfdr_gmcp_l2, 2, mean), apply(fp_mfdr_gmcp_cv, 2, mean), apply(fp_mfdr_gmcp_cv1se, 2, mean),
                apply(fp_mfdr_gl_l1, 2, mean), apply(fp_mfdr_gl_l2, 2, mean), apply(fp_mfdr_gl_cv, 2, mean), apply(fp_mfdr_gl_cv1se, 2, mean))
)

resdf_fdr <- data.frame(
  beta = rep(pbeta, 8),
  lambda = rep(c("Grp MCP (10%)", "Grp MCP (20%)", "Grp MCP (CV)", "Grp MCP (1se)",
                 "Grp lasso (10%)", "Grp lasso(20%)", "Grp lasso (CV)", "Grp lasso (1se)"), each = length(pbeta)),
  real_fdr = c(apply(fdr_mfdr_gmcp_l1, 2, mean), apply(fdr_mfdr_gmcp_l2, 2, mean), apply(fdr_mfdr_gmcp_cv, 2, mean), apply(fdr_mfdr_gmcp_cv1se, 2, mean),
               apply(fdr_mfdr_gl_l1, 2, mean), apply(fdr_mfdr_gl_l2, 2, mean), apply(fdr_mfdr_gl_cv, 2, mean), apply(fdr_mfdr_gl_cv1se, 2, mean))
)

long_tp = tidyr::spread(resdf_tp, lambda, true_pos)
long_fp = tidyr::spread(resdf_fp, lambda, false_pos)
long_fdr = tidyr::spread(resdf_fdr, lambda, real_fdr)

temptab1 <- rbind(round(long_tp[,c("Grp MCP (10%)", "Grp MCP (1se)", "Grp MCP (CV)", 
                                   "Grp lasso (10%)",  "Grp lasso (1se)", "Grp lasso (CV)")], 1),
                  round(long_fdr[,c("Grp MCP (10%)",  "Grp MCP (1se)", "Grp MCP (CV)", 
                                    "Grp lasso (10%)", "Grp lasso (1se)", "Grp lasso (CV)")], 3))

tab_pl <- cbind(data.frame(Var = c("True Positives", rep(" ", length(pbeta[-1])), "Empirical FDR", rep(" ", length(pbeta[-1]))),
                           Beta = rep(round(pbeta,2),2)), temptab1)



n = 200
ngrp = 100
ntrue = 10
nltype = "quadratic"
sim_col = "nonparm"
ctype = "AR"
rho = 0.0
fam = "gaussian"
thr1 = 0.1
thr2 = 0.2
pbeta = c(0.15,0.2,0.25)


tp_mfdr_gmcp_l1 = tp_mfdr_gmcp_l2 = tp_mfdr_gmcp_cv = tp_mfdr_gmcp_cv1se = matrix(NA, nrow = reps, ncol = length(pbeta))
fp_mfdr_gmcp_l1 = fp_mfdr_gmcp_l2 = fp_mfdr_gmcp_cv = fp_mfdr_gmcp_cv1se = matrix(NA, nrow = reps, ncol = length(pbeta))
fdr_mfdr_gmcp_l1 = fdr_mfdr_gmcp_l2 = fdr_mfdr_gmcp_cv = fdr_mfdr_gmcp_cv1se = matrix(NA, nrow = reps, ncol = length(pbeta))

tp_mfdr_gl_l1 = tp_mfdr_gl_l2 = tp_mfdr_gl_cv = tp_mfdr_gl_cv1se = matrix(NA, nrow = reps, ncol = length(pbeta))
fp_mfdr_gl_l1 = fp_mfdr_gl_l2 = fp_mfdr_gl_cv = fp_mfdr_gl_cv1se = matrix(NA, nrow = reps, ncol = length(pbeta))
fdr_mfdr_gl_l1 = fdr_mfdr_gl_l2 = fdr_mfdr_gl_cv = fdr_mfdr_gl_cv1se = matrix(NA, nrow = reps, ncol = length(pbeta))


for(i in 1:reps){
  
  r <-  genX_nonparm(n = n, ngrp = ngrp, ctype = ctype,
                     rho = rho)
  X <- r$X
  grp <- r$grp
  
  
  for(pb in 1:length(pbeta)){
    
    y <- genY_nonlin(rawX = r$rawX, ntrue = ntrue, type = nltype, pbeta = pbeta[pb])
    
    ### Grp MCP
    cv.fit <- cv.grpreg(X, y, group = grp, returnX = TRUE, family = fam, penalty = "grMCP")
    fit <- cv.fit$fit
    mfdr_res <- grpreg::mfdr(fit)
    mfdr_l1 <- max(which(mfdr_res$mFDR <= thr1))
    mfdr_l2 <- max(which(mfdr_res$mFDR <= thr2))
    cvl <- which(cv.fit$lambda  == cv.fit$lambda.min)
    cv1se <- min(which(cv.fit$cve < cv.fit$cve[cvl] + cv.fit$cvse[cvl]))
    truth <- get_truth(fit, ntrue)
    
    
    tp_mfdr_gmcp_l1[i,pb] <- truth$tp[mfdr_l1]
    fp_mfdr_gmcp_l1[i,pb] <- truth$fp[mfdr_l1]
    fdr_mfdr_gmcp_l1[i,pb] <- truth$real_fdr[mfdr_l1]
    
    
    tp_mfdr_gmcp_l2[i,pb] <- truth$tp[mfdr_l2]
    fp_mfdr_gmcp_l2[i,pb] <- truth$fp[mfdr_l2]
    fdr_mfdr_gmcp_l2[i,pb] <- truth$real_fdr[mfdr_l2]
    
    
    tp_mfdr_gmcp_cv[i,pb] <- truth$tp[cvl]
    fp_mfdr_gmcp_cv[i,pb] <- truth$fp[cvl]
    fdr_mfdr_gmcp_cv[i,pb] <- truth$real_fdr[cvl]
    
    tp_mfdr_gmcp_cv1se[i,pb] <- truth$tp[cv1se]
    fp_mfdr_gmcp_cv1se[i,pb] <- truth$fp[cv1se]
    fdr_mfdr_gmcp_cv1se[i,pb] <- truth$real_fdr[cv1se]
    
    ## Grp Lasso
    cv.fit <- cv.grpreg(X, y, group = grp, returnX = TRUE, family = fam, penalty = "grLasso")
    fit <- cv.fit$fit
    mfdr_res <- grpreg::mfdr(fit)
    mfdr_l1 <- max(which(mfdr_res$mFDR <= thr1))
    mfdr_l2 <- max(which(mfdr_res$mFDR <= thr2))
    cvl <- which(cv.fit$lambda  == cv.fit$lambda.min)
    cv1se <- min(which(cv.fit$cve < cv.fit$cve[cvl] + cv.fit$cvse[cvl]))
    truth <- get_truth(fit, ntrue)
    
    
    tp_mfdr_gl_l1[i,pb] <- truth$tp[mfdr_l1]
    fp_mfdr_gl_l1[i,pb] <- truth$fp[mfdr_l1]
    fdr_mfdr_gl_l1[i,pb] <- truth$real_fdr[mfdr_l1]
    
    
    tp_mfdr_gl_l2[i,pb] <- truth$tp[mfdr_l2]
    fp_mfdr_gl_l2[i,pb] <- truth$fp[mfdr_l2]
    fdr_mfdr_gl_l2[i,pb] <- truth$real_fdr[mfdr_l2]
    
    
    tp_mfdr_gl_cv[i,pb] <- truth$tp[cvl]
    fp_mfdr_gl_cv[i,pb] <- truth$fp[cvl]
    fdr_mfdr_gl_cv[i,pb] <- truth$real_fdr[cvl]
    
    tp_mfdr_gl_cv1se[i,pb] <- truth$tp[cv1se]
    fp_mfdr_gl_cv1se[i,pb] <- truth$fp[cv1se]
    fdr_mfdr_gl_cv1se[i,pb] <- truth$real_fdr[cv1se]
    
  }
  
}


resdf_tp <- data.frame(
  beta = rep(pbeta, 8),
  lambda = rep(c("Grp MCP (10%)", "Grp MCP (20%)", "Grp MCP (CV)", "Grp MCP (1se)",
                 "Grp lasso (10%)", "Grp lasso(20%)", "Grp lasso (CV)", "Grp lasso (1se)"), each = length(pbeta)),
  true_pos = c(apply(tp_mfdr_gmcp_l1, 2, mean), apply(tp_mfdr_gmcp_l2, 2, mean), apply(tp_mfdr_gmcp_cv, 2, mean), apply(tp_mfdr_gmcp_cv1se, 2, mean),
               apply(tp_mfdr_gl_l1, 2, mean), apply(tp_mfdr_gl_l2, 2, mean), apply(tp_mfdr_gl_cv, 2, mean), apply(tp_mfdr_gl_cv1se, 2, mean))
)

resdf_fp <- data.frame(
  beta = rep(pbeta, 8),
  lambda = rep(c("Grp MCP (10%)", "Grp MCP (20%)", "Grp MCP (CV)", "Grp MCP (1se)",
                 "Grp lasso (10%)", "Grp lasso(20%)", "Grp lasso (CV)", "Grp lasso (1se)"), each = length(pbeta)),
  false_pos = c(apply(fp_mfdr_gmcp_l1, 2, mean), apply(fp_mfdr_gmcp_l2, 2, mean), apply(fp_mfdr_gmcp_cv, 2, mean), apply(fp_mfdr_gmcp_cv1se, 2, mean),
                apply(fp_mfdr_gl_l1, 2, mean), apply(fp_mfdr_gl_l2, 2, mean), apply(fp_mfdr_gl_cv, 2, mean), apply(fp_mfdr_gl_cv1se, 2, mean))
)

resdf_fdr <- data.frame(
  beta = rep(pbeta, 8),
  lambda = rep(c("Grp MCP (10%)", "Grp MCP (20%)", "Grp MCP (CV)", "Grp MCP (1se)",
                 "Grp lasso (10%)", "Grp lasso(20%)", "Grp lasso (CV)", "Grp lasso (1se)"), each = length(pbeta)),
  real_fdr = c(apply(fdr_mfdr_gmcp_l1, 2, mean), apply(fdr_mfdr_gmcp_l2, 2, mean), apply(fdr_mfdr_gmcp_cv, 2, mean), apply(fdr_mfdr_gmcp_cv1se, 2, mean),
               apply(fdr_mfdr_gl_l1, 2, mean), apply(fdr_mfdr_gl_l2, 2, mean), apply(fdr_mfdr_gl_cv, 2, mean), apply(fdr_mfdr_gl_cv1se, 2, mean))
)

long_tp = tidyr::spread(resdf_tp, lambda, true_pos)
long_fp = tidyr::spread(resdf_fp, lambda, false_pos)
long_fdr = tidyr::spread(resdf_fdr, lambda, real_fdr)

temptab2 <- rbind(round(long_tp[,c("Grp MCP (10%)", "Grp MCP (1se)", "Grp MCP (CV)", 
                                   "Grp lasso (10%)",  "Grp lasso (1se)", "Grp lasso (CV)")], 1),
                  round(long_fdr[,c("Grp MCP (10%)",  "Grp MCP (1se)", "Grp MCP (CV)", 
                                    "Grp lasso (10%)", "Grp lasso (1se)", "Grp lasso (CV)")], 3))

tab_q <- cbind(data.frame(Var = c("True Positives", rep(" ", length(pbeta[-1])), "Empirical FDR", rep(" ", length(pbeta[-1]))),
                          Beta = rep(round(pbeta,2),2)), temptab2)



n = 200
ngrp = 100
ntrue = 10
k = 3
ctype = "AR"
rho = 0.0
fam = "gaussian"
thr1 = 0.1
thr2 = 0.2
pbeta = c(0.2, 0.3, 0.4)

tp_mfdr_gmcp_l1 = tp_mfdr_gmcp_l2 = tp_mfdr_gmcp_cv = tp_mfdr_gmcp_cv1se = matrix(NA, nrow = reps, ncol = length(pbeta))
fp_mfdr_gmcp_l1 = fp_mfdr_gmcp_l2 = fp_mfdr_gmcp_cv = fp_mfdr_gmcp_cv1se = matrix(NA, nrow = reps, ncol = length(pbeta))
fdr_mfdr_gmcp_l1 = fdr_mfdr_gmcp_l2 = fdr_mfdr_gmcp_cv = fdr_mfdr_gmcp_cv1se = matrix(NA, nrow = reps, ncol = length(pbeta))

tp_mfdr_gl_l1 = tp_mfdr_gl_l2 = tp_mfdr_gl_cv = tp_mfdr_gl_cv1se = matrix(NA, nrow = reps, ncol = length(pbeta))
fp_mfdr_gl_l1 = fp_mfdr_gl_l2 = fp_mfdr_gl_cv = fp_mfdr_gl_cv1se = matrix(NA, nrow = reps, ncol = length(pbeta))
fdr_mfdr_gl_l1 = fdr_mfdr_gl_l2 = fdr_mfdr_gl_cv = fdr_mfdr_gl_cv1se = matrix(NA, nrow = reps, ncol = length(pbeta))


for(i in 1:reps){
  
  r <- genX_fac_grp(n = n, ngrp = ngrp, k = k, ctype = ctype, rho = rho)
  X <- r$X
  grp <- r$grp
  
  
  for(pb in 1:length(pbeta)){
    
    y <- genY_factor(X = X, grp = r$grp, ntrue = ntrue, beta = pbeta[pb])
    
    
    ### Grp MCP
    cv.fit <- cv.grpreg(X, y, group = grp, returnX = TRUE, family = fam, penalty = "grMCP")
    fit <- cv.fit$fit
    mfdr_res <- grpreg::mfdr(fit)
    mfdr_l1 <- max(which(mfdr_res$mFDR <= thr1))
    mfdr_l2 <- max(which(mfdr_res$mFDR <= thr2))
    cvl <- which(cv.fit$lambda  == cv.fit$lambda.min)
    cv1se <- min(which(cv.fit$cve < cv.fit$cve[cvl] + cv.fit$cvse[cvl]))
    truth <- get_truth(fit, ntrue)
    
    
    tp_mfdr_gmcp_l1[i,pb] <- truth$tp[mfdr_l1]
    fp_mfdr_gmcp_l1[i,pb] <- truth$fp[mfdr_l1]
    fdr_mfdr_gmcp_l1[i,pb] <- truth$real_fdr[mfdr_l1]
    
    
    tp_mfdr_gmcp_l2[i,pb] <- truth$tp[mfdr_l2]
    fp_mfdr_gmcp_l2[i,pb] <- truth$fp[mfdr_l2]
    fdr_mfdr_gmcp_l2[i,pb] <- truth$real_fdr[mfdr_l2]
    
    
    tp_mfdr_gmcp_cv[i,pb] <- truth$tp[cvl]
    fp_mfdr_gmcp_cv[i,pb] <- truth$fp[cvl]
    fdr_mfdr_gmcp_cv[i,pb] <- truth$real_fdr[cvl]
    
    tp_mfdr_gmcp_cv1se[i,pb] <- truth$tp[cv1se]
    fp_mfdr_gmcp_cv1se[i,pb] <- truth$fp[cv1se]
    fdr_mfdr_gmcp_cv1se[i,pb] <- truth$real_fdr[cv1se]
    
    ## Grp Lasso
    cv.fit <- cv.grpreg(X, y, group = grp, returnX = TRUE, family = fam, penalty = "grLasso")
    fit <- cv.fit$fit
    mfdr_res <- grpreg::mfdr(fit)
    mfdr_l1 <- max(which(mfdr_res$mFDR <= thr1))
    mfdr_l2 <- max(which(mfdr_res$mFDR <= thr2))
    cvl <- which(cv.fit$lambda  == cv.fit$lambda.min)
    cv1se <- min(which(cv.fit$cve < cv.fit$cve[cvl] + cv.fit$cvse[cvl]))
    truth <- get_truth(fit, ntrue)
    
    
    tp_mfdr_gl_l1[i,pb] <- truth$tp[mfdr_l1]
    fp_mfdr_gl_l1[i,pb] <- truth$fp[mfdr_l1]
    fdr_mfdr_gl_l1[i,pb] <- truth$real_fdr[mfdr_l1]
    
    
    tp_mfdr_gl_l2[i,pb] <- truth$tp[mfdr_l2]
    fp_mfdr_gl_l2[i,pb] <- truth$fp[mfdr_l2]
    fdr_mfdr_gl_l2[i,pb] <- truth$real_fdr[mfdr_l2]
    
    
    tp_mfdr_gl_cv[i,pb] <- truth$tp[cvl]
    fp_mfdr_gl_cv[i,pb] <- truth$fp[cvl]
    fdr_mfdr_gl_cv[i,pb] <- truth$real_fdr[cvl]
    
    tp_mfdr_gl_cv1se[i,pb] <- truth$tp[cv1se]
    fp_mfdr_gl_cv1se[i,pb] <- truth$fp[cv1se]
    fdr_mfdr_gl_cv1se[i,pb] <- truth$real_fdr[cv1se]
    
  }
  
}


resdf_tp <- data.frame(
  beta = rep(pbeta, 8),
  lambda = rep(c("Grp MCP (10%)", "Grp MCP (20%)", "Grp MCP (CV)", "Grp MCP (1se)",
                 "Grp lasso (10%)", "Grp lasso(20%)", "Grp lasso (CV)", "Grp lasso (1se)"), each = length(pbeta)),
  true_pos = c(apply(tp_mfdr_gmcp_l1, 2, mean), apply(tp_mfdr_gmcp_l2, 2, mean), apply(tp_mfdr_gmcp_cv, 2, mean), apply(tp_mfdr_gmcp_cv1se, 2, mean),
               apply(tp_mfdr_gl_l1, 2, mean), apply(tp_mfdr_gl_l2, 2, mean), apply(tp_mfdr_gl_cv, 2, mean), apply(tp_mfdr_gl_cv1se, 2, mean))
)

resdf_fp <- data.frame(
  beta = rep(pbeta, 8),
  lambda = rep(c("Grp MCP (10%)", "Grp MCP (20%)", "Grp MCP (CV)", "Grp MCP (1se)",
                 "Grp lasso (10%)", "Grp lasso(20%)", "Grp lasso (CV)", "Grp lasso (1se)"), each = length(pbeta)),
  false_pos = c(apply(fp_mfdr_gmcp_l1, 2, mean), apply(fp_mfdr_gmcp_l2, 2, mean), apply(fp_mfdr_gmcp_cv, 2, mean), apply(fp_mfdr_gmcp_cv1se, 2, mean),
                apply(fp_mfdr_gl_l1, 2, mean), apply(fp_mfdr_gl_l2, 2, mean), apply(fp_mfdr_gl_cv, 2, mean), apply(fp_mfdr_gl_cv1se, 2, mean))
)

resdf_fdr <- data.frame(
  beta = rep(pbeta, 8),
  lambda = rep(c("Grp MCP (10%)", "Grp MCP (20%)", "Grp MCP (CV)", "Grp MCP (1se)",
                 "Grp lasso (10%)", "Grp lasso(20%)", "Grp lasso (CV)", "Grp lasso (1se)"), each = length(pbeta)),
  real_fdr = c(apply(fdr_mfdr_gmcp_l1, 2, mean), apply(fdr_mfdr_gmcp_l2, 2, mean), apply(fdr_mfdr_gmcp_cv, 2, mean), apply(fdr_mfdr_gmcp_cv1se, 2, mean),
               apply(fdr_mfdr_gl_l1, 2, mean), apply(fdr_mfdr_gl_l2, 2, mean), apply(fdr_mfdr_gl_cv, 2, mean), apply(fdr_mfdr_gl_cv1se, 2, mean))
)

long_tp = tidyr::spread(resdf_tp, lambda, true_pos)
long_fp = tidyr::spread(resdf_fp, lambda, false_pos)
long_fdr = tidyr::spread(resdf_fdr, lambda, real_fdr)

temptab3 <- rbind(round(long_tp[,c("Grp MCP (10%)", "Grp MCP (1se)", "Grp MCP (CV)", 
                                   "Grp lasso (10%)",  "Grp lasso (1se)", "Grp lasso (CV)")], 1),
                  round(long_fdr[,c("Grp MCP (10%)",  "Grp MCP (1se)", "Grp MCP (CV)", 
                                    "Grp lasso (10%)", "Grp lasso (1se)", "Grp lasso (CV)")], 3))

tab_k3 <- cbind(data.frame(Var = c("True Positives", rep(" ", length(pbeta[-1])), "Empirical FDR", rep(" ", length(pbeta[-1]))),
                           Beta = rep(round(pbeta,2),2)), temptab3)

final_tab <- cbind(data.frame(Scenario = c("Factors (k=3)", rep(" ", 2*length(pbeta)-1),
                                           "Piecewise Linear", rep(" ", 2*length(pbeta)-1),
                                           "Quadratic", rep(" ", 2*length(pbeta)-1))) ,rbind(tab_k3, tab_pl, tab_q))

gridExtra::grid.table(final_tab)
}