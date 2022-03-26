library(lme4)
library(fmsb)
library(MuMIn)

source('../../Latex/R/glmtools.R')

# Format numbers for use in running text.
nice.float <- function(x, d = 3) formatC(x, format="f", big.mark=",", digits = d)
nice.int <- function(n) formatC(n, format="d", big.mark=",")

concordance <- all

save.cols <-c("Noun", "Verb", "Compound", "Relation", "Linkbinary")
conditions <- list(
  list(sep = "q_np_sep_all",       joint = "q_np_joint_all",       vari = "NP"),
  list(sep = "q_particip_sep_all", joint = "q_particip_joint_all", vari = "Participle"),
  list(sep = "q_prog_sep_all",     joint = "q_prog_joint_all",     vari = "Progressive"),
  list(sep = "q_infzu_sep_all",    joint = "q_infzu_joint_all",    vari = "Infinitive")
)

concordance.glmm <- NULL

for (cond in conditions) {
  print(cond$vari)
  .adder <- cbind(
    concordance[, save.cols],
    data.frame(
      Separate = concordance[, cond$sep],
      Joint    = concordance[, cond$joint],
      Context  = rep(cond$vari, nrow(concordance))
    )
  )
  print(nrow(.adder))
  if (is.null(concordance.glmm)) concordance.glmm <- .adder
  else concordance.glmm <-  rbind(concordance.glmm, .adder)
}

concordance.glmm$Relation <- factor(concordance.glmm$Relation, levels = c("Object", "Undetermined", "Adjunct"))
concordance.glmm$Context <- factor(concordance.glmm$Context, levels = c("Infinitive", "Participle", "NP", "Progressive"))
concordance.glmm$FullLink <- concordance.glmm$Link
concordance.glmm$Link <- concordance.glmm$Linkbinary

corpus.glmm <- glmer(cbind(Joint, Separate)~Context+Relation+Link+(1|Compound),
                     data=concordance.glmm, family=binomial,
                     na.action = na.fail, control=glmerControl(optimizer="nloptwrap2", optCtrl=list(maxfun=2e5))
)

summary(corpus.glmm)
corpus.fixefs <- fixef(corpus.glmm)
nrow(concordance.glmm)






corpus.fixefs <- fixef(corpus.glmm)
corpus.confints <- confint(corpus.glmm)
# corpus.confints <- cbind(corpus.fixefs, corpus.fixefs) # Fake CIs for quick recompiles.
corpus.glmm.r2 <- r.squaredGLMM(corpus.glmm)




format.ranef <- function(glmm, ranef) {
  require(lme4)
  .vcov   <- as.data.frame(VarCorr(glmm))
  list(Name = ranef, Intercept = .vcov[which(.vcov$grp == ranef), "vcov"], sd = .vcov[which(.vcov$grp == ranef), "sdcor"])
}

# Build the table.
corpus.ct <- cbind(corpus.fixefs, corpus.confints[2:8,])
colnames(corpus.ct) <- c("Estimate", "CI low", "CI high")
ranef.nv <- format.ranef(corpus.glmm, "Compound")
corpus.r2.txt <- paste0("Nakagawa \\& Schielzeth's \\CM{R^2_m=", nice.float(corpus.glmm.r2[1,1]), "} and \\CM{R^2_c=", nice.float(corpus.glmm.r2[1,2]), "}")
ranef.txt <- paste0("Random effect for N+V lemma: \\CM{Intercept=", nice.float(ranef.nv$Intercept), "}, \\CM{sd=", nice.float(ranef.nv$sd),
                    "}")
corpus.ctxt <- xtable(corpus.ct, digits = 3,
                      caption = paste0("Coefficient table for the binomial GLMM modelling the corpus data with 95\\% profile likelihood ratio confidence intervals. The horizontal line separates first-level and second-level effects. Weighting was used to account for the bias in models on proportion data. ", ranef.txt, ". The intercepts model the fixed effects Relation~=~Argument. " , corpus.r2.txt),
                      label = "tab:corpusglmm")

# Experimental function to fix variable names.
lme4.pretty <- function(s) {
  s <- gsub("([a-z])([A-Z])", "\\1 = \\2", s)
  s <- gsub("Object", "Argument", s)
  gsub("Adjunct", "Oblique", s)
}

# Print the table.
print(corpus.ctxt,
      include.rownames=T,
      floating = T,
      table.placement = '!htbp',
      booktabs = T,
      scalebox = 1,
      hline.after = c(-1,1,4,7),
      sanitize.text.function = lme4.pretty
)
