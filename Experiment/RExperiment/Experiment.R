library(lme4)
library(car)
library(vcd)
library(effects)
library(lattice)
library(MuMIn)

rm(list=ls())
cat("\014")
graphics.off()

setwd("~/Workingcopies/Univerbation/Experiment/RExperiment")
out.dir <- "/Users/user/Workingcopies/Univerbation/Experiment/RExperiment/Results/"

source("glmtools.R")
source("highstat.R")

save.persistent <- F

conditions <- rep(c("Prepart", "Inf", "Part", "Prog"), 2)
lexicals <- c("Bergsteigen", "Platzmachen", "Probehören", "Teetrinken",
              "Spaßhaben", "Seilspringen", "Mutmachen", "Bogenschießen")


# Pull attraction from corpus results.
load(file = "../../Corpus/Results/corpus.Rdata")
lexicals.df <- data.frame(Compound = lexicals)
attracts <- join(lexicals.df, all)[, "all.assocs"]
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


# Turn into usable data frame.
cases <- NULL
for (r in 1:nrow(results)) {
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
cases$Univerbation <- as.factor(cases$Univerbation)


# Reorder factor for nicer effect plot.
cases$Condition <- factor(cases$Condition, levels = c("Inf", "Part", "Prog", "Prepart"))
cases$Condition <- revalue(cases$Condition, c("Inf"="Infinitive", "Part"="Participle", "Prepart"="Clitic", "Prog"="Progressive"))


# Store the data frame for archiving.
save(cases, file = paste0(out.dir, "experiment.Rdata"))


### Informative plot ###

the.table <- structable(Univerbation ~ Attraction + Condition, data = cases)
the.table <- as.table(the.table)
# lab <-ifelse(the.table < 5, NA, paste0(round(the.table/32*100, 0), "%"))
lab <-ifelse(the.table <= 5, NA, the.table)

if (save.persistent) pdf(file = paste0(out.dir, "responses.pdf"))
the.plot <- mosaic(the.table, pop = F, direction = c("h", "v", "h"),
                   labeling = T, colorize = T,
                   shade = T,
                   gp = gpar(fill=c("lightyellow", "lightgreen")),
                   set_labels=list(Univerbation = c("No", "Yes"), Attraction = c("High", "Low"),
                                   Condition = rev(c("Progressive", "Clitic", "Participle", "Infinitive"))))
tmparray <- as.table(the.table)
labeling_cells(text = lab)(tmparray)
if (save.persistent) dev.off()



### GLMM ###

univerbate.glmer <- glmer(Univerbation ~ AttractionNum + Condition + (1 | Participant) + (1 | Item),
      data = cases, family = binomial(link = "logit"),
      na.action = na.fail, nAGQ=0,
      control=glmerControl(optimizer="nloptwrap2", optCtrl=list(maxfun=2e5)))
print(summary(univerbate.glmer))
suppressWarnings(print(r.squaredGLMM(univerbate.glmer)))


# Effect plots.
fx <- c("Condition", "AttractionNum")
fx.xlabs <- c("Context", "Lexical")
fx.ylabs <- rep("Probability of univerbation", length(fx))
fx.mains <- c("Context", "Lexical")

for (i in 1:length(fx)) {
  p <- plot(effect(fx[i], univerbate.glmer, KR = T), rug=F, colors = c("black", "darkorange"),
            main=fx.mains[i],
            ylab=fx.ylabs[i],
            xlab=fx.xlabs[i]
  )
  
  if (save.persistent) {
    trellis.device(device = "pdf", file = paste0(out.dir, fx[i], ".pdf"))
    trellis.par.set(list(axis.text = list(cex = 1.25)))
    trellis.par.set(list(par.ylab.text = list(cex = 1.25)))
    trellis.par.set(list(par.xlab.text = list(cex = 1.25)))
  }
  print(p)
  if (save.persistent) dev.off()  
}

