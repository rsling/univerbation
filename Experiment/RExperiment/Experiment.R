library(lme4)
library(car)
library(vcd)
library(effects)
library(lattice)
library(MuMIn)
library(plyr)

rm(list=ls())
cat("\014")
graphics.off()

setwd("~/Workingcopies/Univerbation/Experiment/RExperiment")
out.dir <- "~/Workingcopies/Univerbation/Experiment/RExperiment/Results/"

source("glmtools.R")
source("highstat.R")

save.persistent <- F

conditions <- rep(c("Prepart", "Inf", "Part", "Prog"), 2)
lexicals <- c("Bergsteigen", "Platzmachen", "Probehören", "Teetrinken",
              "Spaßhaben", "Seilspringen", "Mutmachen", "Bogenschießen")


# Pull attraction from corpus results.
load(file = "../../Corpus/RCorpus/Results/corpus.Rdata")
concordance <- all
lexicals.df <- data.frame(Compound = lexicals)
attracts <- join(lexicals.df, concordance)[, "all.assocs"]
attracts.f <- ifelse(attracts < -0.003, "Lo", "Hi")

results.w1 <- read.csv(file = "../Data/Ergebnisse_Woche1.csv", sep = "\t",
                    colClasses = c("factor"))
results.w2 <- read.csv(file = "../Data/Ergebnisse_woche2.csv", sep = "\t",
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
colnames(cases) <- c("Univerbation", "Participant", "Gender", "Age", "Condition", "Item", "Attraction", "AttractionNum")
cases$Univerbation <- as.factor(cases$Univerbation)


# Reorder factor for nicer effect plot.
#cases$Condition <- factor(cases$Condition, levels = c("Inf", "Part", "Prog", "Prepart"))
#cases$Condition <- revalue(cases$Condition, c("Inf"="Infinitive", "Part"="Participle", "Prepart"="Clitic", "Prog"="Progressive"))


# Store the data frame for archiving.
save(cases, file = paste0(out.dir, "experiment.Rdata"))


### Informative plot ###
#cases$Condition <- factor(cases$Condition, levels = c("Infinitive", "Participle", "Clitic", "Progressive"))
cases$Attraction <- revalue(cases$Attraction, c("Hi"="High", "Lo"="Low"))
cases$Univerbation <- revalue(cases$Univerbation, c("1"="Yes", "0"="No"))

the.table <- structable(Univerbation ~ Attraction + Condition, data = cases)
the.table <- as.table(the.table)
# lab <-ifelse(the.table < 5, NA, paste0(round(the.table/32*100, 0), "%"))
lab <-ifelse(the.table <= 5, NA, the.table)

if (save.persistent) pdf(file = paste0(out.dir, "Responses.pdf"))
the.plot <- mosaic(the.table, pop = F, direction = c("h", "v", "h"),
                   labeling = T, colorize = T,
                   shade = T,
                   gp = gpar(fill=c(rep("lightyellow", 8), rep("lightgreen", 8)))
                  )
tmparray <- as.table(the.table)
labeling_cells(text = lab)(tmparray)
if (save.persistent) dev.off()



### GLMM ###

univerbate.glmer <- glmer(Univerbation ~ AttractionNum + Condition + (1 | Participant),
      data = cases, family = binomial(link = "logit")
      , na.action = na.fail,
      control=glmerControl(optimizer="nloptwrap2", optCtrl=list(maxfun=2e5))
      )
print(summary(univerbate.glmer))
suppressWarnings(print(r.squaredGLMM(univerbate.glmer)))


# Effect plots.
p <- plot(effect("AttractionNum", univerbate.glmer, KR = T), rug=F, colors = c("black", "darkorange"),
          main="",
          ylab="P",
          xlab="Attraction"
)
if (save.persistent) {
    trellis.device(device = "pdf", file = paste0(out.dir, fx[i], ".pdf"))
    trellis.par.set(list(axis.text = list(cex = 1.25)))
    trellis.par.set(list(par.ylab.text = list(cex = 1.25)))
    trellis.par.set(list(par.xlab.text = list(cex = 1.25)))
  }
  print(p)
if (save.persistent) dev.off()  


fx <- effect("Condition", univerbate.glmer, KR = T)

#fx$variables$Condition$levels <- c("Inf.", "Part.", "Clitic", "Prog.")
#levels(fx$x$Condition) <- c("Clitic", "Inf.", "Part.", "Prog.")

plot(fx, rug=F, colors = c("black", "darkorange"),
          main="",
          ylab="P",
          xlab="Condition"
)



designtable <- cases[1:8, c("Condition", "Item", "AttractionNum")]
designtable <- designtable[order(designtable$AttractionNum),]
designtable <- cbind(designtable, data.frame(Attraction=c(rep("Low",4), rep("High", 4))))
designtable <- designtable[order(designtable$Condition),]



length(levels(as.factor(cases$Participant)))
summary(as.integer(cases$Age))[1]
summary(as.integer(cases$Age))[3]
summary(as.integer(cases$Age))[6]
length(levels(results.w1$ID))
length(levels(results.w2$ID))
