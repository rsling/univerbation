library(lme4)
library(car)
library(vcd)
library(effects)
library(lattice)


rm(list=ls())
cat("\014")
graphics.off()

setwd("~/Workingcopies/Univerbation/Experiment/RExperiment")

out.dir <- "/Users/user/Workingcopies/Univerbation/Experiment/RExperiment/output/"

source("glmtools.R")
source("highstat.R")

save.persistent <- F

# conditions <- rep(c("Prepart", "Inf", "Part", "Prog"), 2)
conditions <- rep(c("PräpMitArtikel", "Infinitiv", "Partizip", "Progressiv"), 2)

lexicals <- c("Bergsteigen", "Platzmachen", "Probehören", "Teetrinken",
              "Spaßhaben", "Seilspringen", "Mutmachen", "Bogenschießen")
attracts <- c(0.05623067, -0.07883408, 0.043379037, -0.05475554,
              -0.08254289, 2.79e-02, -0.06317342, 0.06083703)
# attracts.f <- c("Hi", "Lo", "Hi", "Lo",
#                 "Lo", "Hi", "Lo", "Hi")
attracts.f <- c("Hoch", "Niedrig", "Hoch", "Hoch",
                "Niedrig", "Hoch", "Niedrig", "Hoch")

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
#  cat("\n", as.character(results[r, 1]))
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
# colnames(cases) <- c("Univerbation", "Participant", "Age", "Gender", "Condition", "Item", "Attraction", "AttractionNum")
colnames(cases) <- c("Univerbierung", "ProbandIn", "Alter", "Gender", "Morphosyntax", "Item", "LexTendenzDiskret", "LexTendenz")

# Informative plot.

# the.table <- structable(Univerbation ~ Attraction + Condition, data = cases)
the.table <- structable(Univerbierung ~ LexTendenzDiskret + Morphosyntax, data = cases)
the.table <- as.table(the.table)
# lab <-ifelse(the.table < 5, NA, paste0(round(the.table/32*100, 0), "%"))
lab <-ifelse(the.table <= 5, NA, the.table)

if (save.persistent) pdf(file = paste0(out.dir, "responses.pdf"))
the.plot <- mosaic(the.table, pop = F, direction = c("h", "v", "h"),
                   labeling = T, colorize = T,
                   shade = T,
                   gp = gpar(fill=c("lightyellow", "lightgreen")),
                   set_labels=list(Univerbation = c("No", "Yes"), Attraction = c("High", "Low"),
                                   Condition = rev(c("Progressiv", "Präp.+Artikel", "Partizip", "Infinitiv"))))
tmparray <- as.table(the.table)
labeling_cells(text = lab)(tmparray)
if (save.persistent) dev.off()


### GLMM ###

univerbate.glmer <- glmer(Univerbation ~ AttractionNum + Condition + (1 | Participant) + (1 | Item),
      data = cases, family = binomial(link = "logit"),
      na.action = na.fail, nAGQ=0,
      control=glmerControl(optimizer="nloptwrap2", optCtrl=list(maxfun=2e5)))

print(summary(univerbate.glmer))

# Effect plots.

fx <- c("Condition", "AttractionNum")
fx.xlabs <- c("Morphosyntaktischer Kontext", "Lexikalische Univerbierungtendenz (aus DECOW16A)")
fx.ylabs <- rep("Wahrscheinlichkeit der Univerbierung", length(fx))
fx.mains <- c("Effekt des morphosyntaktischen Kontexts", "Effekt der lexikalischen Univerbierungstendenz")

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


# # Raneff plot.
# opts.dotchart <- list(pch=19, col="black", cex=1, xlab="Prediction of conditional mode")
# 
# if(save.plots) pdf("ranef_selection.pdf")
#   do.call(ranef.plot, c(list(univerbate.glmer, "Participant", 50, main = "Participant random effects with 95% prediction intervals"), opts.dotchart))
# if(save.plots) dev.off()
