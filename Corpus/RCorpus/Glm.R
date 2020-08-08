library(lme4)
library(fmsb)
library(MuMIn)

source('../../Latex/glmtools.R')

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
