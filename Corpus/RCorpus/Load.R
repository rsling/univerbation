rm(list = ls(all=T))

setwd("~/Workingcopies/Univerbation/Corpus/RCorpus")
save.persistent <- F
min.count       <- 10
num             <- 20
show.results    <- 'all'
out.dir         <- 'Results/'

corpus.size     <- 13950853989  # Subcorpus size 'Sätze' in DECOW16A.

all <- read.csv2(file = "../Data/concordance.csv", sep = '\t', header = T)

# Remove ones with "Erroneous" annotation.
all <- all[-which(all$Relation=="Erroneous"),]

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

# Get relative frequency.
all$FLogPerMillion <- apply(all[,16:50], 1, function(n) { log(sum(n)/corpus.size*10^6) })

# Get total sums for separate and joint spelling.
all$Separate <- apply(all[,grep("_sep_", colnames(all))], 1, sum)
all$Joint    <- apply(all[,grep("_joint_", colnames(all))], 1, sum)

# Get a binary linking element vartiable.
all$Linkbinary <- ifelse(all$Linking=="0", "No", "Yes")
