library(lme4)
library(car)
library(vcd)

rm(list=ls())
cat("\014")
graphics.off()

setwd("~/Linguistics/Incubator/Schreibung/RExperiment")

conditions <- rep(c("Prepart", "Inf", "Part", "Prog"), 2)
lexicals <- c("Bergsteigen", "Platzmachen", "Probehören", "Teetrinken",
              "Spaßhaben", "Seilspringen", "Mutmachen", "Bogenschießen")
attracts <- c(0.05623067, -0.07883408, 0.043379037, -0.05475554,
              -0.08254289, 2.79e-02, -0.06317342, 0.06083703)
attracts.f <- c("Hi", "Lo", "Hi", "Lo",
                "Lo", "Hi", "Lo", "Hi")

results.w1 <- read.csv(file = "Ergebnisse_Woche1.csv", sep = "\t",
                    colClasses = c("factor"))
results.w2 <- read.csv(file = "Ergebnisse_woche2.csv", sep = "\t",
                    colClasses = c("factor"))
results <- rbind(results.w1, results.w2[,colnames(results.w1)])


# Remove remarks and secondary experiment.
remover <- c("Anmerkungen", paste0("Parenth", 1:6))
results <- results[, -which(names(results) %in% remover)]

print(summary(results))

# Descriptive plots.
par(mfrow=c(4, 2))
for (i in 1:8) {
  plot(results[, paste0("Univerb", i, "F")], main = conditions[i])
}
par(mfrow=c(1, 1))


# Turn into usable data frame.
cases <- NULL
for (r in 1:nrow(results)) {
  cat("\n", as.character(results[r, 1]))
  .result <- cbind(t(results[r, paste0("Univerb", 1:8, "F")]), 
                   as.character(results[r, 1]), as.character(results[r, 2]), as.character(results[r, 3]))
  if (is.null(cases))
    cases <- .result
  else
    cases <- rbind(cases, .result)
}
cases <- as.data.frame(cases)
rownames(cases) <- c()
cases <- cbind(cases, rep(conditions, nrow(results)), rep(lexicals, nrow(results)), rep(attracts.f, nrow(results)), rep(attracts, nrow(results)))
colnames(cases) <- c("Univerbation", "Participant", "Age", "Gender", "Condition", "Item", "Attraction", "AttractionNum")

# GLM is nonsense b/o collinearity.
# model <- glmer(Univerbation~Attraction+(1|Condition), data = cases, family = binomial(link=logit))
# model.0 <- glmer(Univerbation~Attraction, data = cases, family = binomial(link=logit))
# model.1 <- glmer(Univerbation~1+(1|Condition), data = cases, family = binomial(link=logit))
# 
# print(anova(model, model.0))
# print(anova(model, model.1))
# 
# print(summary(model))

# Informative plot.

the.table <- structable(Univerbation ~ Attraction + Condition, data = cases)
the.table <- as.table(the.table)
# lab <-ifelse(the.table < 5, NA, paste0(round(the.table/32*100, 0), "%"))
lab <-ifelse(the.table <= 5, NA, the.table)

# pdf("responses.pdf")
the.plot <- mosaic(the.table, pop = F, direction = c("h", "v", "h"),
                   labeling = T, colorize = T,
                   gp = gpar(fill=c("lightyellow", "lightgreen")),
                   set_labels=list(Univerbation = c("No", "Yes"), Attraction = c("High", "Low"),
                                   Condition = rev(c("Prog.", "Prp+Clt.", "Part.", "Inf."))))
tmparray <- as.table(the.table)
labeling_cells(text = lab)(tmparray)
# dev.off()

