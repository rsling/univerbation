library(lme4)
library(fmsb)
library(MuMIn)

# One of those weird binomial GLMs on proportions.
Glm <- glm(cbind(Joint, Separate)~Valency+Relation+Linkbinary, data=all, family=binomial)
print(summary(Glm))
print(NagelkerkeR2(Glm))

### Oh yeah, swamp me in your dirty random effects!
Glmm <- glmer(cbind(Joint, Separate)~Valency+Relation+Linkbinary+(1|Verb)+(1|Noun), data=all, family=binomial)
print(summary(Glmm))
print(r.squaredGLMM(Glmm))
