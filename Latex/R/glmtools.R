# Handling convergence warnings:
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html

# A faster BOBYQA optimizer.
library(nloptr)
defaultControl <- list(algorithm="NLOPT_LN_BOBYQA",xtol_rel=1e-6,maxeval=1e5)
nloptwrap2 <- function(fn,par,lower,upper,control=list(),...) {
  for (n in names(defaultControl)) 
    if (is.null(control[[n]])) control[[n]] <- defaultControl[[n]]
  res <- nloptr(x0=par,eval_f=fn,lb=lower,ub=upper,opts=control,...)
  with(res,list(par=solution,
                fval=objective,
                feval=iterations,
                conv=if (status>0) 0 else status,
                message=message))
}


# Function to get prop of correct predictions.
corr.prop <- function(model, observed, cutoff) {
  preds <- ifelse(predict(model) < cutoff, 0, 1)
  cmat <- table(preds, observed)
  sum(diag(cmat))/sum(cmat)
}


# Function to get LRT for (G)LMMMs with bootstrapped CIs. This takes AGES!
lmer.modelcomparison <- function(model, regressors, formula.target, ci = 0.95, nsim = 4, print.updated = F, print.diag = F) {
  require(pbkrtest)
  
  result = NULL
  
  for (i in 1:length(regressors)) {
    formula.parts <- c(formula.target, regressors[1:i-1], regressors[i+1:length(regressors)])
    formula.parts <- formula.parts[which(!is.na(formula.parts))]
    formula <- as.formula(paste(formula.parts, collapse="+"))
    
    model.0 <- update(model, formula = formula)
    pbmc <- PBmodcomp(model, model.0, nsim = nsim)
    
    if (print.diag) {
      cat("\n\n\n")
      print(regressors[i])
      print(pbmc)
    }
    
    res.tmp <- data.frame(Regressor = regressors[i], PB.p = pbmc$test$p.value[2],
                          LRT.p = pbmc$test$p.value[1], LR = unname(pbmc$LRTstat[1]),
                          Df = unname(pbmc$LRTstat[2]))
    if (is.null(result))
      result <- res.tmp
    else
      result <- rbind(result, res.tmp)
    
    if (print.updated) print(result)
  }
  result
}



join.factors <- function(...) {
  unlist(list(...))
}

factorify <- function(f) {
  levelsize <- function(l) { s = length(which(f==l)) }
  f <- factor(f)
  o = levels(f)[order(unlist(lapply(levels(f), levelsize)), decreasing = T)]
  f <- factor(f, o)
}

# Sort factor to put most r(esponse)=0 in f(actor) first,
factorify.to.zero <- function(f, r, dec=F) {
  f <- factor(f)
  r <- factor(r)
  levs <- colnames(table(r, f))[order(table(r, f)[1,]/table(r, f)[2,],decreasing=dec)]
  f <- factor(f, levels=levs)
}


siglev <- function(p)
{
  if (p <= 0.1)
    if (p <= 0.05)
      if (p <= 0.01)
        if (p <= 0.001)
          sig <- "***"
  else
    sig <- "**"
  else
    sig <- "*"
  else
    sig <- "."
  else
    sig <- ""
}

siglev <- function(p)
{
  if (p <= 0.1)
    if (p <= 0.05)
      if (p <= 0.01)
        if (p <= 0.001)
          sig <- "***"
  else
    sig <- "**"
  else
    sig <- "*"
  else
    sig <- "."
  else
    sig <- ""
}

# This functions makes a plot of the COEFFICIENTS of significant factors.
plot.coef <- function(glm, alpha = 0.05, pr="Pr(>|z|)", cex.labels = 1, pos.labels = 0.4, ...)
{
  # Extract p values and coefs.
  p <- coef(summary(glm))[,pr]
  c <- coef(glm)
  
  # Separate intercept from other coefs.
  p.int <- as.numeric(p[1])
  c.int <- as.numeric(c[1])
  p <- p[-1]
  c <- c[-1]
  
  # Find the order in which to put coefficients.
  c <- c[which(p <= alpha)]
  p <- p[which(p <= alpha)]
  
  # Find the order according to coef.
  order <- order(c)
  c <- c[order]
  p <- p[order]
  names <- names(c)
  
  # Put intercept and coefs back together.
  names <- c("(Intercept)", names)
  vals <- c(round(c.int, 3), round(c, 3))
  
  c <- c(as.numeric(c.int), as.numeric(c))
  p <- c(as.numeric(p.int), as.numeric(p))
  vals <- paste(vals, lapply(p, siglev), sep=" ")
  names <- paste(names, vals, sep="\n")
  
  range <- max(c)-min(c)
  ylim <- c(min(c)-0.25*range, max(c)+0.25*range)
  bars <- barplot(c, ylim=ylim, col=c("white", rep("lightgray", length(c)-1)), ...)
  pos <- ifelse(c > 0, c+pos.labels*cex.labels, c-pos.labels*cex.labels)
  text(bars, pos, names, cex=cex.labels, srt=75)
}

# This functions makes a plot of the ODDS RATIOS of significant factors.
plot.or <- function(glm, alpha = 0.05, pr="Pr(>|z|)", cex.labels = 1, pos.labels = 3, ...)
{
  # Extract p values and coefs.
  p <- coef(summary(glm))[,pr]
  c <- exp(coef(glm))
  
  # Separate intercept from other coefs.
  p.int <- as.numeric(p[1])
  c.int <- as.numeric(c[1])
  p <- p[-1]
  c <- c[-1]
  
  # Extract the significant coefficients.
  c <- c[which(p <= alpha)]
  p <- p[which(p <= alpha)]
  
  # Find the order according to coef.
  order <- order(c)
  c <- c[order]
  p <- p[order]
  names <- names(c)
  
  # Put intercept and coefs back together.
  names <- c("(Intercept)", names)
  vals <- c(round(c.int, 3), round(c, 3))
  
  c <- c(as.numeric(c.int), as.numeric(c))
  p <- c(as.numeric(p.int), as.numeric(p))
  vals <- paste(vals, lapply(p, siglev), sep=" ")
  names <- paste(names, vals, sep="\n")
  
  range <- max(c)-min(c)
  ylim <- c(0, max(c)+0.25*range)
  bars <- barplot(c, ylim=ylim, col=c("white", rep("lightgray", length(c)-1)), ...)
  pos <- ifelse(c > 0, c+pos.labels*cex.labels, c-pos.labels*cex.labels)
  text(bars, pos, names, cex=cex.labels)
}



# Plots the intercept from model for the number most frequent levels in data.
# NOTE! This works only with VI models, not with VS or VIVS models. But
# it can be adapted to handle such models.
ranef.plot <- function(model, effect, number = -1, intervals = 0.95, ...) {
  require(lme4)
  
  # Get conditional modes.
  .ranef <- ranef(model, condVar = TRUE, drop = T)[[effect]]
  
  # Get a subsample of conditional modes if necessary.
  if (number > 0 & length(.ranef) > number)
    .select <- sample(1:length(.ranef), number)
  else
    .select <- 1:length(.ranef)
  
  # Order indices of the selection by conditional modes.
  .select <- .select[order(.ranef[.select])]
  
  # Create full data frame.
  # Note: postVar gives you the conditional variance. Hence, we need
  # to calculate the stdev and derive a proper n% interval from it.
  .condvar <- attributes(.ranef)$postVar
  .qn <- qnorm(1-(1-intervals)/2)
  .df <- cbind(.ranef[.select],
               .ranef[.select]-.qn*sqrt(.condvar[.select]),
               .ranef[.select]+.qn*sqrt(.condvar[.select])
  )
  
  # Plot.
  dotchart(.df[,1], labels = rownames(.df), xlim = c(min(.df[,2]), max(.df[,3])), ...)
  
  # Add the prediction intervals.
  if (!is.null(intervals)) {
    for (.i in 1:nrow(.df)) lines(c(.df[.i,2], .df[.i,3]), c(.i,.i), lwd=2)
  }
  
  # Return the data frame with the selection.
  .df
}


lr.test <- function(glm, glm0)
{
  ll <- -2*logLik(glm)
  ll0 <- -2*logLik(glm0)
  lr <- ll0 - ll
  df <- glm$rank - glm0$rank
  p <- 1-pchisq(lr, df)
  list(lr=as.numeric(lr), df=as.numeric(df), p=as.numeric(p))
}

phi.glm <- function(glm)
{
  sum(resid(glm, type="pearson")^2 / df.residual(glm))
}
