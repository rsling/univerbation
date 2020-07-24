rm(list = ls(all=T))

setwd("~/Workingcopies/Univerbation/Corpus")
save.persistent <- F
min.count       <- 10
num             <- 20
show.results    <- 'all'
out.dir         <- 'Results/'

corpus.size     <- 13950853989  # Subcorpus size 'Sätze' in DECOW16A.

all <- read.csv2(file = "Frequencies/decow16a/all.csv", sep = '\t', header = T)

# Indices for the ones to exclude from 'am' progressive and P-clitic counts.
ignore.am <- c(
  which(all$Noun=="Computer" & all$Verb=="spielen"),
  which(all$Noun=="Daumen" & all$Verb=="lutschen"),
  which(all$Noun=="Schluss" & all$Verb=="machen"),
  which(all$Noun=="Kopf" & all$Verb=="machen"),
  which(all$Noun=="Platz" & all$Verb=="machen"),
  which(all$Noun=="Handy" & all$Verb=="telefonieren"),
  which(all$Noun=="Eis" & all$Verb=="schlecken"),
  which(all$Noun=="Kopf" & all$Verb=="drehen")
)

all$FLogPerMillion <- apply(all[,12:46], 1, function(n) { log(sum(n)/corpus.size*10^6) })


