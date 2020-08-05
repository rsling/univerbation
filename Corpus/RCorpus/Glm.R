library(lme4)
library(fmsb)
library(MuMIn)

source('../../Latex/glmtools.R')

# Format numbers for use in running text.
nice.float <- function(x, d = 3) formatC(x, format="f", big.mark=",", digits = d)
nice.int <- function(n) formatC(n, format="d", big.mark=",")

if (save.persistent) sink("Results/glmm.txt")

# One of those weird binomial GLMs on proportions.
Glm <- glm(cbind(Joint, Separate)~Relation+Linkbinary, data=all, family=binomial)
print(summary(Glm))
print(NagelkerkeR2(Glm))

### Oh yeah, swamp me in your dirty random effects!
Glmm <- glmer(cbind(Joint, Separate)~Relation+Linkbinary+(1|Compound), data=all, family=binomial,
              na.action = na.fail)
print(summary(Glmm))
print(r.squaredGLMM(Glmm))

if (save.persistent) sink()


# Make nice table to be used in knitr.
require(xtable)

# Helper function.
format.ranef <- function(glmm, ranef) {
  require(lme4)
  .vcov   <- as.data.frame(VarCorr(glmm))
  list(Name = ranef, Intercept = .vcov[which(.vcov$grp == ranef), "vcov"], sd = .vcov[which(.vcov$grp == ranef), "sdcor"])
}

Glmm.fixefs <- fixef(Glmm)
Glmm.confints <- confint(Glmm)
corpus.ct <- nice.float(cbind(Glmm.fixefs, Glmm.confints[2:5,]))
colnames(corpus.ct) <- c("Estimate", "CI low", "CI high")
rownames(corpus.ct)
corpus.ctx <- xtable(corpus.ct,
                    caption = "Coefficient table for the corpus GLMM",
                    label = "tab:corpusglmm")

ranef.v <- format.ranef(Glmm, "Verb")
ranef.n <- format.ranef(Glmm, "Noun")
ranef.txt <- paste0("Random effects for lemmas: ",
                    ranef.v$Name, " with Intercept=", nice.float(ranef.v$Intercept), ", sd=", nice.float(ranef.v$sd),
                    " and ", ranef.n$Name, " with Intercept=", nice.float(ranef.n$Intercept), ", sd=", nice.float(ranef.n$sd), ".")
print(corpus.ctx,
      include.rownames=T,
      floating = T,
      table.placement = 'h!',
      booktabs = T,
      scalebox = 1,
      hline.after = c(-1,1,4),
      sanitize.text.function = function(x){x},
)


set.seed(3478)
ranef.plot(Glmm, effect = "Compound", number = 20)

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
