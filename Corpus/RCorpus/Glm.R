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
              na.action = na.fail, control=glmerControl(optimizer="nloptwrap2", optCtrl=list(maxfun=2e5)))
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
