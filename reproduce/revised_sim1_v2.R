library(ggplot2)
library(DescTools)
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
for(i in 1:reps){
  
  
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
  
  ## Estimate marginal false discoveries
  mmat[i,] <- grpreg::mfdr(fit, X)$EF
  
  ## Get true FDR
  rmat[i,] = get_truth(fit, ntrue = ntrue)$fp
  
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

get_legend <- function(myplot){
  tmp <- ggplot_gtable(ggplot_build(myplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

my_legend <- get_legend(dummy_plot)


final_fig1 = grid.arrange(n200_grp40 + labs(y = "Groups (of 40)"), n400_grp40, n800_grp40,
               n200_grp80 + labs(y = "Groups (of 80)"), n400_grp80, n800_grp80,
               n200_grp200 + labs(y = "Groups (of 200)"), n400_grp200, n800_grp200,
               my_legend,
               layout_matrix = rbind(c(1, 2, 3),
                                     c(4, 5, 6),
                                     c(7, 8, 9),
                                     c(10, 10, 10)),
               heights = c(10,10,10,1))

ggsave("Fig1.png", final_fig1, width = 7, height = 7, units = "in", dpi = 300)


###################################################################################
## PLOT 2 - GLMs and Cox
###################################################################################



##################################################################################
## PLOT 2 - AR corr w/ rho = 0.9
###################################################################################

## panel 1,1 - % error = 6.9%
n200_grp40 = get_plot_for_fig1(reps = 100, n = 200, ngrp = 40, ntrue = 2, lmin = 0,
                               nltype = "piecewise", sim_col = "nonparm",
                               ctype = "AR", rho = 0.9, pen = "grLasso", fam = "gaussian",
                               lam_max_mult = 0.33) + ggtitle("n = 200")

## panel 1,2 - % error = 3.0%
n400_grp40 = get_plot_for_fig1(reps = 100, n = 400,  ngrp = 40, ntrue = 2,  lmin = 0,
                               nltype = "piecewise", sim_col = "nonparm",
                               ctype = "AR", rho = 0.9, pen = "grLasso", fam = "gaussian",
                               lam_max_mult = 0.33) + ggtitle("n = 400")

## panel 1,3 - % error = 0.05
n800_grp40 = get_plot_for_fig1(reps = 100, n = 800,  ngrp = 40, ntrue = 2,  lmin = 0,
                               nltype = "piecewise", sim_col = "nonparm",
                               ctype = "AR", rho = 0.9, pen = "grLasso", fam = "gaussian",
                               lam_max_mult = 0.6) + ggtitle("n = 800")


# grid.arrange(n200_grp40, n400_grp40, n800_grp40)


## ROW 2 - high p

## panel 2,1 
n200_grp80 = get_plot_for_fig1(reps = 100, n = 200, ngrp = 80, ntrue = 4, lmin = 0.001,
                               nltype = "piecewise", sim_col = "nonparm",
                               ctype = "AR", rho = 0.9, pen = "grLasso", fam = "gaussian",
                               lam_max_mult = 0.6)


## panel 2,2
n400_grp80  = get_plot_for_fig1(reps = 100, n = 400, ngrp = 80, ntrue = 4, lmin = 0.001,
                                nltype = "piecewise", sim_col = "nonparm",
                                ctype = "AR", rho = 0.9, pen = "grLasso", fam = "gaussian",
                                lam_max_mult = 0.6) 


## panel 2,3
n800_grp80  = get_plot_for_fig1(reps = 100, n = 800, ngrp = 80, ntrue = 4, lmin = 0.001,
                                nltype = "piecewise", sim_col = "nonparm",
                                ctype = "AR", rho = 0.9, pen = "grLasso", fam = "gaussian",
                                lam_max_mult = 0.6) 


# grid.arrange(n200_grp40, n400_grp40, n800_grp40,
#              n200_grp80, n400_grp80, n800_grp80,
#              nrow = 2)




## ROW 3 - highest p

## panel 3,1 
n200_grp200 = get_plot_for_fig1(reps = 100, n = 200, ngrp = 200, ntrue = 10, lmin = 0.00001,
                                nltype = "piecewise", sim_col = "nonparm",
                                ctype = "AR", rho = 0.9, pen = "grLasso", fam = "gaussian",
                                lam_max_mult = 0.6) 


## panel 3,2
n400_grp200  =  get_plot_for_fig1(reps = 100, n = 400, ngrp = 200, ntrue = 10, lmin = 0.00001,
                                  nltype = "piecewise", sim_col = "nonparm",
                                  ctype = "AR", rho = 0.9, pen = "grLasso", fam = "gaussian",
                                  lam_max_mult = 0.6)



## panel 3,3
n800_grp200  =  get_plot_for_fig1(reps = 100, n = 800, ngrp = 200, ntrue = 10, lmin = 0.00001,
                                  nltype = "piecewise", sim_col = "nonparm",
                                  ctype = "AR", rho = 0.9, pen = "grLasso", fam = "gaussian",
                                  lam_max_mult = 0.6)


grid.arrange(n200_grp40, n400_grp40, n800_grp40,
             n200_grp80, n400_grp80, n800_grp80,
             n200_grp200, n400_grp200, n800_grp200,
             nrow = 3)


### OLD STUFF

get_plot_for_fig1(reps = 10, n = 200, ngrp = 40, ntrue = 2,
                  nltype = "piecewise", sim_col = "nonparm",
                  ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian",
                  lam_max_mult = 0.3) + ggtitle("High n, Ind")



get_plot_for_fig1(reps = 2, n = 1000, ngrp = 200, ntrue = 10,
                  nltype = "piecewise", sim_col = "nonparm",
                  ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian",
                  lam_max_mult = 0.3) + ggtitle("High n, Ind")


## Fix n:ngrp ratio at 5:1, ntrue % at 5%

## High n - linear - piecewise - ind
p1 = get_plot_for_fig1(reps = 100, n = 1000, ngrp = 200, ntrue = 10,
                  nltype = "piecewise", sim_col = "nonparm",
                  ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian",
                  lam_max_mult = 0.3) + ggtitle("High n, Ind")

## Low n - linear - piecewise - ind
p2 = get_plot_for_fig1(reps = 100, n = 200, ngrp = 40, ntrue = 2,
                  nltype = "piecewise", sim_col = "nonparm",
                  ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian",
                  lam_max_mult = 0.5) + ggtitle("Low n, Ind")


## High n - linear - piecewise - AR 0.8
p3 = get_plot_for_fig1(reps = 100, n = 1000, ngrp = 200, ntrue = 10,
                       nltype = "piecewise", sim_col = "nonparm",
                       ctype = "AR", rho = 0.9, pen = "grLasso", fam = "gaussian",
                       lam_max_mult = 0.08) + ggtitle("High n, AR")

## Low n - linear - piecewise - AR 0.8
p4 = get_plot_for_fig1(reps = 100, n = 200, ngrp = 40, ntrue = 2,
                       nltype = "piecewise", sim_col = "nonparm",
                       ctype = "AR", rho = 0.9, pen = "grLasso", fam = "gaussian",
                       lam_max_mult = 0.33) + ggtitle("Low n, AR")

## High n - factor - ind
p5 = get_plot_for_fig1(reps = 100, n = 1000, ngrp = 200, ntrue = 10, 
                       nltype = "piecewise", sim_col = "factor", k = 3,
                       ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian",
                       lam_max_mult = 0.7) + ggtitle("High n, Ind, Factor")

## Low n - factor - ind
p6 = get_plot_for_fig1(reps = 100, n = 200, ngrp = 40, ntrue = 2,
                       nltype = "piecewise", sim_col = "factor", k = 3,
                       ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian",
                       lam_max_mult = 0.55) + ggtitle("Low n, Ind, Factor")


## High n - factor - AR
p7 = get_plot_for_fig1(reps = 100, n = 1000, ngrp = 40, ntrue = 2, lmin = 0,
                       nltype = "piecewise", sim_col = "factor", k = 3,
                       ctype = "AR", rho = 0, pen = "grLasso", fam = "gaussian",
                       lam_max_mult = 0.6) + ggtitle("High n, AR, Factor")

## Low n - factor - AR
p8 = get_plot_for_fig1(reps = 100, n = 200, ngrp = 40, ntrue = 2,
                       nltype = "piecewise", sim_col = "factor", k = 3,
                       ctype = "AR", rho = 0.9, pen = "grLasso", fam = "gaussian",
                       lam_max_mult = 0.55) + ggtitle("Low n, AR, Factor")



###############
## NEW FIG 2
###############

pp = c(100,200,500,800,1200)
pen = "grMCP"
diff_aucs = diff_aucs_corr = diff_aucs_fac = diff_aucs_fac6 = numeric(length(pp))
nreps = 10
for (i in 1:length(pp)){
temp_res_fac = get_plot_for_fig1(reps = 100, n = pp[i], ngrp = 80, ntrue = 2, lmin = 0,
                  nltype = "piecewise", sim_col = "factor", k = 3,
                  ctype = "AR", rho = 0, pen = pen, fam = "gaussian",
                  lam_max_mult = 1)

diff_aucs_fac[i] = temp_res_fac$auc

temp_res_fac6 = get_plot_for_fig1(reps = 100, n = pp[i], ngrp = 80, ntrue = 2, lmin = 0,
                                 nltype = "piecewise", sim_col = "factor", k = 6,
                                 ctype = "AR", rho = 0, pen = pen, fam = "gaussian",
                                 lam_max_mult = 1)

diff_aucs_fac6[i] = temp_res_fac6$auc

temp_res_corr = get_plot_for_fig1(reps = nreps, n = pp[i],  ngrp = 80, ntrue = 2,  lmin = 0,
                  nltype = "piecewise", sim_col = "nonparm",
                  ctype = "AR", rho = 0.8, pen = pen, fam = "gaussian",
                  lam_max_mult = 1) 

diff_aucs_corr[i] = temp_res_corr$auc

temp_res = get_plot_for_fig1(reps = nreps, n = pp[i],  ngrp = 80, ntrue = 2,  lmin = 0,
                             nltype = "piecewise", sim_col = "nonparm",
                             ctype = "AR", rho = 0.0, pen = pen, fam = "gaussian",
                             lam_max_mult = 1) 

diff_aucs[i] = temp_res$auc
}

plot_df = data.frame(values = c(diff_aucs, diff_aucs_corr, diff_aucs_fac, diff_aucs_fac6),
                     Scenario = rep(c("Piecewise", "Piecewise (w/ Corr)", "Factor (k=3)", "Factor (k=6)"), each = length(diff_aucs)),
                     n = rep(pp, times = 4))

ggplot(plot_df, aes(x = n, y = values, color = Scenario)) + geom_point() + geom_line() +
  theme_light() + labs(y = "AUC Ratio") + geom_hline(yintercept = 1, lty = 2)




pp = c(100,200,500,800,1200)
diff_aucs_bin = diff_aucs_corr_bin = diff_aucs_fac_bin = diff_aucs_fac6_bin = numeric(length(pp))
nreps = 20
for (i in 1:length(pp)){
  
  lm = ifelse(pp[i] < 200, 0.0001, 0)
  
  temp_res_fac = get_plot_for_fig1(reps = 100, n = pp[i], ngrp = 80, ntrue = 2, lmin = lm, 
                                   nltype = "piecewise", sim_col = "factor", k = 3,
                                   ctype = "AR", rho = 0, pen = "grLasso", fam = "binomial",
                                   lam_max_mult = 1)
  
  diff_aucs_fac_bin[i] = temp_res_fac$auc
  
  temp_res_fac6 = get_plot_for_fig1(reps = 100, n = pp[i], ngrp = 80, ntrue = 2, lmin = lm,
                                    nltype = "piecewise", sim_col = "factor", k = 6,
                                    ctype = "AR", rho = 0, pen = "grLasso", fam = "binomial",
                                    lam_max_mult = 1)
  
  diff_aucs_fac6_bin[i] = temp_res_fac6$auc
  
  temp_res_corr = get_plot_for_fig1(reps = nreps, n = pp[i],  ngrp = 80, ntrue = 2,  lmin = lm,
                                    nltype = "piecewise", sim_col = "nonparm",
                                    ctype = "AR", rho = 0.8, pen = "grLasso", fam = "binomial",
                                    lam_max_mult = 1) 
  
  diff_aucs_corr_bin[i] = temp_res_corr$auc
  
  temp_res = get_plot_for_fig1(reps = nreps, n = pp[i],  ngrp = 80, ntrue = 2,  lmin = lm,
                               nltype = "piecewise", sim_col = "nonparm",
                               ctype = "AR", rho = 0.0, pen = "grLasso", fam = "binomial",
                               lam_max_mult = 1) 
  
  diff_aucs_bin[i] = temp_res$auc
}

