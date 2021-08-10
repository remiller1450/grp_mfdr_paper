setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Requires GEOquery library
#install.packages("BiocManager")
#BiocManager::install("GEOquery")
#BiocManager::install("snpStats")
library(GEOquery)
library(splines)
library(grpreg)
library(snpStats)

## Case Study

## Pre-processing on spira data
load("C:\\Users\\millerr33\\Documents\\GitHub\\grp_mfdr_paper\\reproduce\\data\\spira-geo.RData")

ys <- pData(geo[[1]])[,1]
y <- as.numeric(regexpr('NOT', ys) == -1)
X2 <- t(exprs(geo[[1]]))  
rawX <- apply(X2, 2, as.numeric)  ## Make gene expressions numeric

for(i in 1:ncol(rawX)){
  X <- cbind(X, bs(rawX[,i]))
}

k <- ncol(X)/ncol(rawX)
grp <- sort(rep(1:ncol(rawX), k))

## Fit models
fit <- cv.grpreg(X, y, grp, family = "binomial", penalty = "grLasso", returnX = TRUE)

##  Results (grp lasso)
mfdr_res <- grpreg::mfdr(fit$fit, X)
print(mfdr_res[mfdr_res$mFDR < .15,])
fit$pe[max(which(fit$lambda >= .0620))]

mfdr_res[which.min(fit$pe),]




## Cross-validation results
predict(fit$fit, lambda = fit$lambda.min, type = "ngroups")
min(fit$pe)

### What if only linear relationships are considered?
fitl <- cv.ncvreg(rawX, y, family = "binomial", penalty = "lasso", returnX = TRUE)
fitl$pe[which(fitl$lambda == fitl$lambda.min)]
min(fitl$pe)

fitml <- cv.ncvreg(rawX, y, family = "binomial", penalty = "MCP", returnX = TRUE)


## mFDR
lfmfdr = ncvreg::mfdr(fitl$fit)
lfmfdr[which(lfmfdr$mFDR < 0.15),]
fitl$pe[which(fitl$lambda > 0.1420)]


## Cross-validation results
predict(fitl$fit, lambda = fitl$lambda.min, type = "nvars")

## Large scale testing
pvl <- pvg <- numeric(ncol(rawX))
int_only <- glm(y ~ 1, family = "binomial")
for(i in 1:ncol(rawX)){
  tft <- glm(y ~ rawX[,i], family = "binomial")
  pvl[i] <- summary(tft)$coefficients[2,4]
  
  tft_g <- glm(y ~ X[,which(grp == i)], family = "binomial")
  pvg[i] <- anova(int_only, tft_g, test = "LRT")$`Pr(>Chi)`[2]
}

adj_pvl = p.adjust(pvl, method = "fdr")
sum(adj_pvl < 0.1)
adj_pvl[which.min(abs(adj_pvl - 0.1))]

adj_pvg = p.adjust(pvg, method = "fdr")
sum(adj_pvg < 0.1)
adj_pvg[which.min(abs(adj_pvg - 0.1))]


## Results Plot

## Create and Export Fig 3
png("Fig3.png", h= 3.5, w=8, units = 'in', res = 300)

layout(matrix(c(1,2,3,3), ncol=2, byrow=TRUE), heights=c(4, 0.6))

plot(fit, log.l = FALSE, type = "pred", vertical.line = FALSE, ylab = "MCE",
     selected = FALSE, main = "Cross-validated Misclassification Error")
abline(v = fit$lambda[which.min(fit$pe)], lty = 3, lwd = 1.5)
abline(v = fit$lambda[which.min(mfdr_res$mFDR < 0.1)], lty = 2, lwd = 1.5)


fef <- loess(mfdr_res$EF ~ fit$lambda, span = .1)
plot(fit$lambda, predict(fef), type = "l", col = "blue", lwd = 2,
     xlim = c(max(fit$lambda),0),
     xlab = expression(lambda),
     ylab = "Features",
     main = "Model Selections",
     bty = "n")
lines(fit$lambda, mfdr_res$S, col = "red", lwd = 2)
abline(v = fit$lambda[which.min(fit$pe)], lty = 3, lwd = 1.5)
abline(v = fit$lambda[which.min(mfdr_res$mFDR < 0.1)], lty = 2, lwd = 1.5)

par(mai=c(0,0,0,0))
plot.new()
legend("center", legend = c("10% mFDR", "Cross-validation", "Expected false discoveries", "Selections"), 
       lty = c(2,3,1,1), bty = "n", col = c(1,1,"blue", "red"), lwd = 1.5, ncol = 2)
dev.off()




plot(fit, log.l = FALSE, type = "pred", vertical.line = FALSE)
