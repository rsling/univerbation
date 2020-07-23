library(lme4)
library(car)
library(vcd)
library(effects)
library(lattice)
library(pbkrtest)
library(MuMIn)
library(beanplot)

rm(list=ls())

set.seed(923)

cat("\014")
graphics.off()

setwd("~/Workingcopies/Univerbation/Experiment/RExperiment")
out.dir <- "/Users/user/Workingcopies/Univerbation/Experiment/RExperiment/output/"

source("glmtools.R")
source("highstat.R")

save.persistently <- T
modcomp.nsim      <- 500 # 500 for production (488 data points).
boot.nsim         <- 500 # 500 for production (488 data points).
do.modelselection <- T
do.bootstrap      <- F


# To reliably set the font in R plots.
library(showtext)
font_paths("/Library/Fonts")
font_add("Libertinus Sans",
         regular    = "LinBiolinum_R.otf",
         bold       = "LinBiolinum_RB.otf",
         italic     = "LinBiolinum_RI.otf"
         )
showtext_auto()

# Reset output file.
if (save.persistently) {
  sink(paste0(out.dir, "results.txt"), append = F)
  cat("\n\nUniverbation of V+N syntagmas (Schäfer & Sayatz)\n\n")
  sink()
}


# Create annotations.
conditions <- rep(c("PrpMitArtikel", "Infinitiv", "Partizip", "Progressiv"), 2)
lexicals <- c("Bergsteigen", "Platzmachen", "Probehören", "Teetrinken",
              "Spaßhaben", "Seilspringen", "Mutmachen", "Bogenschießen")
attracts <- c(0.05623067, -0.07883408, 0.043379037, -0.05475554,
              -0.08254289, 2.79e-02, -0.06317342, 0.06083703)
attracts.f <- c(rep(c("Hoch", "Tief"), 2),
                rep(c("Tief", "Hoch"), 2))
government <- factor(c("Opak", "Objekt", "Opak", "Objekt",
                       "Objekt", "Opak", "Objekt", "Opak"))


# Read data.
results.w1 <- read.csv(file = "Ergebnisse_Woche1.csv", sep = "\t",
                    colClasses = c("factor"))
results.w2 <- read.csv(file = "Ergebnisse_woche2.csv", sep = "\t",
                    colClasses = c("factor"))
results <- rbind(results.w1, results.w2[,colnames(results.w1)])


# Remove remarks and secondary experiment.
remover <- c("Anmerkungen", paste0("Parenth", 1:6))
results <- results[, -which(names(results) %in% remover)]


# Save descriptive.
if (save.persistently) sink(paste0(out.dir, "results.txt"), append = T)
print(summary(results))
if (save.persistently) sink()


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
cases <- cbind(cases, rep(conditions, nrow(results)), rep(lexicals, nrow(results)),
               rep(attracts.f, nrow(results)), rep(attracts, nrow(results)),
               rep(government, nrow(results)))
colnames(cases) <- c("Univerbierung", "Probandin", "Gender", "Alter", "Morphosyntax",
                     "Stimulus", "LexTendenzDiskret", "LexTendenz", "Relation")


# Reorder and rename factor levels for nicer display.
cases$Morphosyntax <- factor(cases$Morphosyntax, levels(cases$Morphosyntax)[c(1,2,4,3)])
levels(cases$Univerbierung) <- c("Nein", "Ja")


# Descriptive plot: government and morphosyntax.

# Reorder factors acc. to predictions.
cases$Morphosyntax <- factor(cases$Morphosyntax, levels = c("Infinitiv", "Partizip", "Progressiv", "PrpMitArtikel"))

the.table <- structable(Univerbierung ~ Relation + Morphosyntax, data = cases)
the.table <- as.table(the.table)
lab <-ifelse(the.table < 10, NA, paste0(round(the.table/61*100, 0), "%"))

if (save.persistently) pdf(file = paste0(out.dir, "responses.pdf"))
the.plot <- mosaic(the.table, direction = c("h", "v", "h"),
                   pop = F,
                   labeling = T, colorize = T,
                   shade = T,
                   gp = gpar(fill=c("lightyellow", "lightblue")),
                   set_labels=list(Univerbation = c("No", "Yes"), Attraction = c("High", "Low"),
                                   Condition = rev(c("Prp. + Artikel", "Progressiv", "Partizip", "Infinitiv")))
)
tmparray <- as.table(the.table)
labeling_cells(text = lab)(tmparray)
if (save.persistently) dev.off()


# Descriptive plot: "syntactic" relationship.
if (save.persistently) pdf(file = paste0(out.dir, "lexGovernment.pdf"))
beanplot(attracts~government,
         bw="nrd0", 
         what = c(1,1,1,1),
         method = "jitter", 
         col = "lightyellow",
         axes = T,
         main=paste("N-V-Relation und lexikalische Tendenz\n(für 8 Items, Big Data-Punktschätzungen)"),
         xlab = "Relation: direktes Objekt",
         ylab = "lexikalische Tendenz zur Univerbierung",
         cex.main = 1
         )
if (save.persistently) dev.off()


if (save.persistently) pdf(file = paste0(out.dir, "lexPref.pdf"))
pal <- colorRampPalette(c("#000080", "#008521"))(8)
attrax <- attracts[order(attracts)]
lexix <- lexicals[order(attracts)]
govz <- government[order(attracts)]
dither <- rep(c(-0.1, 0.1), 2)
plot(0, xlim = c(0,1), frame.plot = F,
     ylim = c(min(attrax)*1.3, max(attrax)*1.3),
     xaxt = "n", cex = 0,
     xlab = "N-V-Relation",
     ylab = "Anziehung bzw. Abstoßung durch Univerbierung",
     main = "",# "Die acht Items und ihre Affinität zur Univerbierung",
     cex.axis = 1.25,
     cex.lab = 1.25,
     cex.main = 1.5
     )
axis(side = 1, at = c(0.25, 0.75), labels = c("objektartig", "opak"),
     cex.axis = 1.25)
abline(h = 0, col = "gray", lty = 2, lwd = 2)
text(x = 0.25+dither, y = attrax[which(govz=="Objekt")] - c(0.008, 0, 0, 0),
     labels = lexix[which(govz=="Objekt")],
     col = pal[1:4],
     cex = 1.5)
text(x = 0.75+dither, y = attrax[which(govz=="Opak")] + c(0, 0, 0, 0.008),
     labels = lexix[which(govz=="Opak")],
     col = pal[5:8],
     cex = 1.5)
if (save.persistently) dev.off()


### GLMM (and drop-models for model selection). ###
univerbate.glmer <- glmer(Univerbierung ~ LexTendenz + Morphosyntax + (1 | Probandin),
                          data = cases, family = binomial(link = "logit"),
                          na.action = na.fail, nAGQ=0,
                          control=glmerControl(optimizer="nloptwrap2", optCtrl=list(maxfun=2e5)))
univerbate.glmer.0 <- glmer(Univerbierung ~ Morphosyntax + (1 | Probandin),
                            data = cases, family = binomial(link = "logit"),
                            na.action = na.fail, nAGQ=0,
                            control=glmerControl(optimizer="nloptwrap2", optCtrl=list(maxfun=2e5)))
univerbate.glmer.1 <- glmer(Univerbierung ~ LexTendenz + (1 | Probandin),
                            data = cases, family = binomial(link = "logit"),
                            na.action = na.fail, nAGQ=0,
                            control=glmerControl(optimizer="nloptwrap2", optCtrl=list(maxfun=2e5)))


# Report model and R2.
if (save.persistently) sink(paste0(out.dir, "results.txt"), append = T)
cat("\n\nGLMM SUMMARY\n")
print(summary(univerbate.glmer))
cat("\n\nR-SQUARED\n")
print(r.squaredGLMM(univerbate.glmer))
cat("\nWITHOUTH LexTendenz\n")
print(r.squaredGLMM(univerbate.glmer.0))
cat("\nWITHOUT Morphosyntax\n")
print(r.squaredGLMM(univerbate.glmer.1))
cat("\n\n")
if (save.persistently) sink()


# Bootstrap or Wald CIs.
if (do.bootstrap) {
  opts.ci.95 <- list(level = 0.95, method = "boot", boot.type = "perc", nsim = boot.nsim)
  univerbate.ci.95  <- do.call(confint.merMod, c(opts.ci.95, list(object = univerbate.glmer, parm = names(fixef(univerbate.glmer)))))
} else {
  univerbate.ci.95 <- confint(univerbate.glmer, parm="beta_", method="Wald")
}
univerbate.ci.95  <- univerbate.ci.95[nrow(univerbate.ci.95):1,]  # Reverse order of CIs.


# Print CIs (Wald or boot).
if (save.persistently) sink(paste0(out.dir, "results.txt"), append = T)
cat("\n\n ", ifelse(do.bootstrap, "BOOT", "WALD"), " CONFIDENCE INTERVALSs\n")
print(univerbate.ci.95)
cat("\n\n")
if (save.persistently) sink()


# Model "selection".
if (do.modelselection) {
  if (save.persistently) sink(paste0(out.dir, "results.txt"), append = T)
  cat("\n\nMODEL SELECTION\n\n")
  print(PBmodcomp(univerbate.glmer, univerbate.glmer.0), nsim = modcomp.nsim)
  cat("\n\n")
  print(PBmodcomp(univerbate.glmer, univerbate.glmer.1), nsim = modcomp.nsim)
  cat("\n\n")
  if (save.persistently) sink()
}


# Plot coefficients.
univerbate.fixeffs <- rev(fixef(univerbate.glmer)[2:length(fixef(univerbate.glmer))])
x.lower <- min(univerbate.ci.95[,1])*1.05
x.upper <- max(univerbate.ci.95[,2])*1.05

if (save.persistently) pdf(paste(out.dir, "fixeffs.pdf", sep=""))
dotchart(univerbate.fixeffs, pch=20,
         xlim = c(x.lower, x.upper),
         lcolor = "gray",
         cex = 1.2,
         main=paste("Coefficient estimates\n with 95% Wald CI", sep=""))
lines(c(0,0), c(0,length(univerbate.ci.95)), col="gray")

for (i in 1:nrow(univerbate.ci.95)) {
  points(univerbate.fixeffs[i], i, pch=18, cex=1.5, col="black")
  lines(univerbate.ci.95[i,c(1,2)], c(i,i), col="black", lwd=2)
}
if (save.persistently) dev.off()


# Effect plots.
fx <- c("Morphosyntax", "LexTendenz")
fx.xlabs <- c("Morphosyntaktischer Kontext", "Lexikalische Univerbierungtendenz (aus DECOW16A)")
fx.ylabs <- rep("Wahrscheinlichkeit der Univerbierung", length(fx))
fx.mains <- c("Effekt des morphosyntaktischen Kontexts", "Effekt der lexikalischen Univerbierungstendenz")

for (i in 1:length(fx)) {
  p <- plot(effect(fx[i], univerbate.glmer, KR = T), rug=F, colors = c("black", "darkorange"),
            main="", # fx.mains[i]
            ylab=fx.ylabs[i],
            xlab=fx.xlabs[i]
  )
  
  if (save.persistently) {
    trellis.device(device = "pdf", file = paste0(out.dir, fx[i], ".pdf"))
    trellis.par.set(list(axis.text = list(cex = 1.5)))
    trellis.par.set(list(par.ylab.text = list(cex = 1.5)))
    trellis.par.set(list(par.xlab.text = list(cex = 1.5)))
  }
  print(p)
  if (save.persistently) dev.off()  
}


# Plot per-participant preferences.
particips <- table(table(cases$Univerbierung, cases$Probandin)["Ja",])
myRamp <- colorRampPalette(c("lightblue", "darkgreen"))(8)
if (save.persistently) pdf(file = paste0(out.dir, "individuals.pdf"))
  bp <- barplot(particips,
       col = myRamp[1],
       main = "",# "Tendenz der Proband*innen zur Univerbierung",
       xlab = "Anzahl der Univerbierungen (von 8)",
       ylab = "Anzahl der Proband*innen mit diesem Verhalten",
       ylim = c(0,20),
       cex.axis = 1.5,
       cex.names = 1.5,
       cex.lab = 1.5,
       cex.main = 1.5)
  text(bp, y = particips+1, labels = particips, cex = 1.5)
if (save.persistently) dev.off()


# Save workspace.
if (save.persistently) save(list = ls(), file=paste(out.dir, "workspace.RData", sep=""))