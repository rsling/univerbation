require(vioplot)
source('./vioplot2.R')
source('./lingassoc.R')
require(beanplot)


# Get overall univerbation strength.
all.assocs <- apply(all[, c("Joint", "Separate")], 1, function(row) {assoc(row[1], row[2], sum(all$Joint), sum(all$Separate), min.count)})
all        <- cbind(all, all.assocs)


# Purge those which have no defined overall assoc strength.
all <- all[-which(is.na(all$all.assocs)),]

all$q_np_sep_all <- apply(all[,grep("q_np.*_sep_", colnames(all))], 1, sum)
all$q_np_joint_all <- apply(all[,grep("q_np.*_joint_", colnames(all))], 1, sum)
all$q_particip_sep_all <- apply(all[,grep("q_particip.*_sep_", colnames(all))], 1, sum)
all$q_particip_joint_all <- apply(all[,grep("q_particip.*_joint_", colnames(all))], 1, sum)
all$q_prog_sep_all <- apply(all[,grep("q_prog.*_sep_", colnames(all))], 1, sum)
all$q_prog_joint_all <- apply(all[,grep("q_prog.*_joint_", colnames(all))], 1, sum)
all$q_infzu_sep_all <- apply(all[,grep("q_infzu.*_sep_", colnames(all))], 1, sum)
all$q_infzu_joint_all <- apply(all[,grep("q_infzu.*_joint_", colnames(all))], 1, sum)



# Caclulate per-Cx associtations.


np.all.joint.sum <- sum(all$q_np_joint_all)
np.all.sep.sum   <- sum(all$q_np_sep_all)
all$np.all.assocs    <- apply(all[, c("q_np_joint_all", "q_np_sep_all")], 1, function(row) {assoc(row[1], row[2], np.all.joint.sum, np.all.sep.sum, min.count)})

particip.joint.sum  <- sum(all$q_particip_joint_all)
particip.sep.sum    <- sum(all$q_particip_sep_all)
all$particip.assocs <- apply(all[, c("q_particip_joint_all", "q_particip_sep_all")], 1, function(row) {assoc(row[1], row[2], particip.joint.sum, particip.sep.sum, min.count)})

prog.joint.sum  <- sum(all$q_prog_joint_all)
prog.sep.sum    <- sum(all$q_prog_sep_all)
all$prog.assocs <- apply(all[, c("q_prog_joint_all", "q_prog_sep_all")], 1, function(row) {assoc(row[1], row[2], prog.joint.sum, prog.sep.sum, min.count)})
# prog.assocs[ignore.am] <- NaN

infzu.joint.sum  <- sum(all$q_infzu_joint_all)
infzu.sep.sum    <- sum(all$q_infzu_sep_all)
all$infzu.assocs <- apply(all[, c("q_infzu_joint_all", "q_infzu_sep_all")], 1, function(row) {assoc(row[1], row[2], infzu.joint.sum, infzu.sep.sum, min.count)})


# Sort frame by overall assoc strength.
all <- all[order(all$all.assocs),]




### Plot the association scores.

if (save.persistent) pdf("Results/densities_cramer.pdf")
density.opts <- list(lwd = 2)
plot(density(all$all.assocs),
  ylim = c(0, 60), axes = F, xlab = "",
  lwd = density.opts$lwd,
  col = "black", lty = 1,
  main = "Distribution of association measures",
  )

lines(density(na.omit(all$np.all.assocs)),
      lwd = density.opts$lwd,
      col = "darkblue", lty = 2)
lines(density(na.omit(all$prog.assocs)),
      lwd = density.opts$lwd,
      col = "darkgreen", lty = 3)
lines(density(na.omit(all$particip.assocs)),
      lwd = density.opts$lwd,
      col = "darkred", lty = 4)
lines(density(na.omit(all$infzu.assocs)),
      lwd = density.opts$lwd,
      col = "darkorange", lty = 5)
axis(1)
axis(2)
legend("topright", bty = "n", lwd = 2, col = c("black", "darkblue", "darkgreen", "darkred", "darkorange"), lty = 1:5,
       legend = c(paste0("All (n=", length(which(!is.na(all$all.assocs))), ")"),
                  paste0("NPs (n=", length(which(!is.na(c(all$np.det.assocs, all$np.clt.assocs, all$np.ndt.assocs)))), ")"),
                  paste0("Prog. (n=", length(which(!is.na(all$prog.assocs))), ")"),
                  paste0("Part. (n=", length(which(!is.na(all$particip.assocs))), ")"),
                  paste0("Inf. (n=", length(which(!is.na(all$infzu.assocs))), ")"))
)
if (save.persistent) dev.off()





save(all, file="Results/corpus.Rdata")
