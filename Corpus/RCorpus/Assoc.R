require(vioplot)
source('./vioplot2.R')
source('./lingassoc.R')
require(beanplot)


# Get overall univerbation strength.
all.assocs <- apply(all[, c("Joint", "Separate")], 1, function(row) {assoc(row[1], row[2], sum(all$Joint), sum(all$Separate), min.count, method="Odds")})
all        <- cbind(all, all.assocs)


# Purge those which have no defined overall assoc strength.
all <- all[-which(is.na(all$all.assocs)),]


# Sort frame by overall assoc strength.
all <- all[order(all$all.assocs),]


obj <- all[which(all$Relation == "Object"), "all.assocs"]
adj <- all[which(all$Relation == "Adjunct"), "all.assocs"]

pdf("logodds.pdf")
plot(density(obj), main="Associations with log odds ratios",
     ,col="darkgreen", lty=2, lwd=3
     #, ylim=c(0,60), xlim=c(-0.05, 0.05)
     , xlab =""
     )
lines(density(adj)
      ,col="black", lty=1, lwd=3
      )
legend("topleft", legend = c("Oblique", "Argument"), lwd=3, col = c("black", "darkgreen"))
dev.off()
