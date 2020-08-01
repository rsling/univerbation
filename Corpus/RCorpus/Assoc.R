require(vioplot)
source('./vioplot2.R')
source('./lingassoc.R')
require(beanplot)


# Get overall univerbation strength.
all.assocs <- apply(all[, c("Joint", "Separate")], 1, function(row) {assoc(row[1], row[2], sum(all$Joint), sum(all$Separate), min.count)})
all        <- cbind(all, all.assocs)


# Purge those which have no defined overall assoc strength.
all <- all[-which(is.na(all$all.assocs)),]


# Caclulate per-Cx associtations.
np.det.joint     <- all$q_np_det_joint_cap + all$q_np_det_joint_nocap
np.det.joint.sum <- sum(np.det.joint)
np.det.sep       <- all$q_np_det_sep_cap + all$q_np_det_sep_nocap
np.det.sep.sum   <- sum(np.det.sep)
np.det           <- cbind(np.det.joint, np.det.sep)
np.det.assocs    <- apply(np.det, 1, function(row) {assoc(row[1], row[2], np.det.joint.sum, np.det.sep.sum, min.count)})

np.clt.joint     <- all$q_np_apprart_joint_cap + all$q_np_apprart_joint_nocap
np.clt.joint.sum <- sum(np.clt.joint)
np.clt.sep       <- all$q_np_apprart_sep_cap + all$q_np_apprart_sep_nocap
np.clt.sep.sum   <- sum(np.clt.sep)
np.clt           <- cbind(np.clt.joint, np.clt.sep)
np.clt.assocs    <- apply(np.clt, 1, function(row) {assoc(row[1], row[2], np.clt.joint.sum, np.clt.sep.sum, min.count)})
np.clt.assocs[ignore.am] <- NaN

np.ndt.joint     <- all$q_np_nodet_joint_cap + all$q_np_nodet_joint_nocap
np.ndt.joint.sum <- sum(np.ndt.joint)
np.ndt.sep       <- all$q_np_nodet_sep_cap + all$q_np_nodet_sep_nocap
np.ndt.sep.sum   <- sum(np.ndt.sep)
np.ndt           <- cbind(np.ndt.joint, np.ndt.sep)
np.ndt.assocs    <- apply(np.ndt, 1, function(row) {assoc(row[1], row[2], np.ndt.joint.sum, np.ndt.sep.sum, min.count)})

particip.joint     <- all$q_particip_joint_cap + all$q_particip_joint_nocap
particip.joint.sum <- sum(particip.joint)
particip.sep       <- all$q_particip_sep_cap + all$q_particip_sep_nocap
particip.sep.sum   <- sum(particip.sep)
particip           <- cbind(particip.joint, particip.sep)
particip.assocs    <- apply(particip, 1, function(row) {assoc(row[1], row[2], particip.joint.sum, particip.sep.sum, min.count)})

prog.joint     <- all$q_prog_joint_cap + all$q_prog_joint_nocap
prog.joint.sum <- sum(prog.joint)
prog.sep       <- all$q_prog_sep_cap + all$q_prog_sep_nocap
prog.sep.sum   <- sum(prog.sep)
prog           <- cbind(prog.joint, prog.sep)
prog.assocs    <- apply(prog, 1, function(row) {assoc(row[1], row[2], prog.joint.sum, prog.sep.sum, min.count)})
prog.assocs[ignore.am] <- NaN

infzu.joint     <- all$q_infzu_joint_cap + all$q_infzu_joint_nocap
infzu.joint.sum <- sum(infzu.joint)
infzu.sep       <- all$q_infzu_sep_cap + all$q_infzu_sep_nocap
infzu.sep.sum   <- sum(infzu.sep)
infzu           <- cbind(infzu.joint, infzu.sep)
infzu.assocs    <- apply(infzu, 1, function(row) {assoc(row[1], row[2], infzu.joint.sum, infzu.sep.sum, min.count)})


# Put all data in one frame.
all <- data.frame(all, np.det.assocs, np.clt.assocs, np.ndt.assocs, particip.assocs, prog.assocs, infzu.assocs)

# Sort frame by overall assoc strength.
all <- all[order(all$all.assocs),]




### Plot the association scores.
### Also comparative plots between overall and Cx-specific score.

plot(all$all.assocs, pch=20, ylim = c(-0.3, 0.3), col="darkgreen",
     main="Associations in all contexts", xlab="", ylab="Association")

z.score <- function(v1, v2) {
  .v1 <- v1[which(!is.na(v1) & !is.na(v2))]
  .v2 <- v2[which(!is.na(v1) & !is.na(v2))]
  .v12 <- .v1-.v2
  list(mean = round(mean(.v12), 3), sd = round(sd(.v12), 3), min = round(min(.v12),3), max = round(max(.v12), 3))
}


z.np.det <- z.score(all$all.assocs, all$np.det.assocs)
plot(all$np.det.assocs, pch=20, ylim = c(-0.3, 0.3), col="darkgreen",
     main="Associations in NPs with determiner", xlab="", ylab="Association",
     sub = paste0("µ=", z.np.det$mean, ", sd=", z.np.det$sd, ", min=", z.np.det$min, ", max=", z.np.det$max))
points(
  0:(nrow(all)-1),
  ifelse(is.na(all$np.det.assocs), -1, all$all.assocs),
  pch=20, cex=0.5, col = ifelse(is.na(all$np.det.assocs), "white", "darkorange"))


z.np.clt <- z.score(all$all.assocs, all$np.clt.assocs)
plot(all$np.clt.assocs, pch=20, ylim = c(-0.3, 0.3), col="darkgreen",
     main="Associations in NPs with prepositional determiner", xlab="", ylab="Association",
     sub = paste0("µ=", z.np.clt$mean, ", sd=", z.np.clt$sd, ", min=", z.np.clt$min, ", max=", z.np.clt$max))
points(
  0:(nrow(all)-1),
  ifelse(is.na(all$np.clt.assocs), -1, all$all.assocs),
  pch=20, cex=0.5, col = ifelse(is.na(all$np.clt.assocs), "white", "darkorange"))


z.np.ndt <- z.score(all$all.assocs, all$np.ndt.assocs)
plot(all$np.ndt.assocs, pch=20, ylim = c(-0.3, 0.3), col="darkgreen",
     main="Associations in NPs without a determiner", xlab="", ylab="Association",
     sub = paste0("µ=", z.np.ndt$mean, ", sd=", z.np.ndt$sd, ", min=", z.np.ndt$min, ", max=", z.np.ndt$max))
points(
  0:(nrow(all)-1),
  ifelse(is.na(all$np.ndt.assocs), -1, all$all.assocs),
  pch=20, cex=0.5, col = ifelse(is.na(all$np.ndt.assocs), "white", "darkorange"))


z.particip <- z.score(all$all.assocs, all$particip.assocs)
plot(all$particip.assocs, pch=20, ylim = c(-0.3, 0.3), col="darkgreen",
     main="Associations of participles", xlab="", ylab="Association",
     sub = paste0("µ=", z.particip$mean, ", sd=", z.particip$sd, ", min=", z.particip$min, ", max=", z.particip$max))
points(
  0:(nrow(all)-1),
  ifelse(is.na(all$particip.assocs), -1, all$all.assocs),
  pch=20, cex=0.5, col = ifelse(is.na(all$particip.assocs), "white", "darkorange"))


z.prog <- z.score(all$all.assocs, all$prog.assocs)
plot(all$prog.assocs, pch=20, ylim = c(-0.3, 0.3), col="darkgreen",
     main="Associations of progressives", xlab="", ylab="Association",
     sub = paste0("µ=", z.prog$mean, ", sd=", z.prog$sd, ", min=", z.prog$min, ", max=", z.prog$max))
points(
  0:(nrow(all)-1),
  ifelse(is.na(all$prog.assocs), -1, all$all.assocs),
  pch=20, cex=0.5, col = ifelse(is.na(all$prog.assocs), "white", "darkorange"))


z.infzu <- z.score(all$all.assocs, all$infzu.assocs)
plot(all$infzu.assocs, pch=20, ylim = c(-0.3, 0.3), col="darkgreen",
     main="Associations of infinitives", xlab="", ylab="Association",
     sub = paste0("µ=", z.infzu$mean, ", sd=", z.infzu$sd, ", min=", z.infzu$min, ", max=", z.infzu$max))
points(
  0:(nrow(all)-1),
  ifelse(is.na(all$infzu.assocs), -1, all$all.assocs),
  pch=20, cex=0.5, col = ifelse(is.na(all$infzu.assocs), "white", "darkorange"))


puppies.and.kittens <- rbind(z.np.det, z.np.clt, z.np.ndt, z.particip, z.prog, z.infzu)



### Make beanplots of the distributions of association measures.

cut.off.point <- 0.15

beanplot(
  c(all$np.det.assocs[which(all$np.det.assocs > -(cut.off.point) & all$np.det.assocs < cut.off.point)],
    all$np.clt.assocs[which(all$np.clt.assocs > -(cut.off.point) & all$np.clt.assocs < cut.off.point)],
    all$np.ndt.assocs[which(all$np.ndt.assocs > -(cut.off.point) & all$np.ndt.assocs < cut.off.point)]),
  all$prog.assocs[which(all$prog.assocs > -(cut.off.point) & all$prog.assocs < cut.off.point)],
  all$particip.assocs[which(all$particip.assocs > -(cut.off.point) & all$particip.assocs < cut.off.point)],
  all$infzu.assocs[which(all$infzu.assocs > -(cut.off.point) & all$infzu.assocs < cut.off.point)],
         what=c(1,1,1,0), ylim = c(-0.15, 0.15),
         names = c("NP", "Participle", "Progressive", "Infinitive"),
         col = list(c("yellow", "darkgray", "black", "black"),
                    #c("yellow", "darkgray", "black", "black"),
                    #c("yellow", "darkgray", "black", "black"),
                    c("lightgreen", "darkgray", "black", "black"),
                    c("darkgreen", "darkgray", "black", "black"),
                    c("darkgreen", "darkgray", "black", "black")))



### Plot distributions of association measures.

if (save.persistent) pdf("densities_cramer.pdf")
density.opts <- list(lwd = 2)
plot(density(all$all.assocs[which(all$all.assocs > -(cut.off.point) & all$all.assocs < cut.off.point)]),
  ylim = c(0, 60), axes = F, xlab = "",
  lwd = density.opts$lwd,
  col = "black", lty = 1,
  main = "Distribution of association measures",
  )

lines(density(c(all$np.det.assocs[which(all$np.det.assocs > -(cut.off.point) & all$np.det.assocs < cut.off.point)],
                all$np.clt.assocs[which(all$np.clt.assocs > -(cut.off.point) & all$np.clt.assocs < cut.off.point)],
                all$np.ndt.assocs[which(all$np.ndt.assocs > -(cut.off.point) & all$np.ndt.assocs < cut.off.point)])),
      lwd = density.opts$lwd,
      col = "darkblue", lty = 2)
lines(density(all$prog.assocs[which(all$prog.assocs > -(cut.off.point) & all$prog.assocs < cut.off.point)]),
      lwd = density.opts$lwd,
      col = "darkgreen", lty = 3)
lines(density(all$particip.assocs[which(all$particip.assocs > -(cut.off.point) & all$particip.assocs < cut.off.point)]),
      lwd = density.opts$lwd,
      col = "darkred", lty = 4)
lines(density(all$infzu.assocs[which(all$infzu.assocs > -(cut.off.point) & all$infzu.assocs < cut.off.point)]),
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



### The same, but with per-group NA removal.

all.redux <- all[which((!is.na(all$np.det.assocs) | !is.na(all$np.clt.assocs) | !is.na(all$np.ndt.assocs))
                       & !is.na(all$particip.assocs)
                       & !is.na(all$infzu.assocs)
                       & !is.na(all$prog.assocs)
),]

if (save.persistent) pdf("densities_reduced.pdf")
density.opts <- list(lwd = 2)
plot(density(all.redux$all.assocs[which(all.redux$all.assocs > -(cut.off.point) & all.redux$all.assocs < cut.off.point)]),
     ylim = c(0, 60), axes = F, xlab = "",
     lwd = density.opts$lwd,
     col = "black", lty = 1,
     main = "Distribution of association measures",
)

lines(density(c(all.redux$np.det.assocs, all.redux$np.clt.assocs, all.redux$np.ndt.assocs)),
      lwd = density.opts$lwd,
      col = "darkblue", lty = 2)
lines(density(all.redux$prog.assocs[which(all.redux$prog.assocs > -(cut.off.point) & all.redux$prog.assocs < cut.off.point)]),
      lwd = density.opts$lwd,
      col = "darkgreen", lty = 3)
lines(density(all.redux$particip.assocs[which(all.redux$particip.assocs > -(cut.off.point) & all.redux$particip.assocs < cut.off.point)]),
      lwd = density.opts$lwd,
      col = "darkred", lty = 4)
lines(density(all.redux$infzu.assocs[which(all.redux$infzu.assocs > -(cut.off.point) & all.redux$infzu.assocs < cut.off.point)]),
      lwd = density.opts$lwd,
      col = "darkorange", lty = 5)
axis(1)
axis(2)
legend("topright", bty = "n", lwd = 2, col = c("black", "darkblue", "darkgreen", "darkred", "darkorange"), lty = 1:5,
       legend = c(paste0("All (n=", length(which(!is.na(all.redux$all.assocs))), ")"),
                  paste0("NPs (n=", length(which(!is.na(c(all.redux$np.det.assocs, all.redux$np.clt.assocs, all.redux$np.ndt.assocs)))), ")"),
                  paste0("Prog. (n=", length(which(!is.na(all.redux$prog.assocs))), ")"),
                  paste0("Part. (n=", length(which(!is.na(all.redux$particip.assocs))), ")"),
                  paste0("Inf. (n=", length(which(!is.na(all.redux$infzu.assocs))), ")"))
)
if (save.persistent) dev.off()

save(all, file="Results/corpus.Rdata")

