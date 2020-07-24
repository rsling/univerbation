
assoc <- function(f1, f2, cs1, cs2, min.count=5, smooth=1) {
  if (f1+f2 < min.count) r <- NaN
  else {
    r <- 0
    f1  <- f1 + smooth
    f2  <- f2 + smooth
    cs1 <- cs1 + smooth
    cs2 <- cs2 + smooth
    x2 <- chisq.test(matrix(c(f1,f2,cs1-f1,cs2-f2), nrow = 2), correct = T)
    r <- as.numeric(sqrt(x2$statistic/(cs1+cs2))) * -sign(x2$expected-x2$observed)[1,1]
    r
  }
}


# Get overall univerbation strength.
all$sep    <- apply(all[,grep("_sep_", colnames(all))], 1, sum)
all$joint  <- apply(all[,grep("_joint_", colnames(all))], 1, sum)
all.assocs <- apply(all[, c("joint", "sep")], 1, function(row) {assoc(row[1], row[2], sum(all$joint), sum(all$sep), min.count)})
all        <- cbind(all, all.assocs)

# Purge thise which have no defined overall assoc strength.
all <- all[-which(is.na(all$all.assocs)),]

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


plot(all$all.assocs, pch=20, ylim = c(-0.3, 0.3), col="darkgreen",
     main="Associations in all contexts", xlab="", ylab="Association")

z.score <- function(v1, v2) {
  .v1 <- v1[which(!is.na(v1) & !is.na(v2))]
  .v2 <- v2[which(!is.na(v1) & !is.na(v2))]
  list(mean = round(mean(.v1-.v2), 3), sd = round(sd(.v1-.v2), 3))
}


z.np.det <- z.score(all$all.assocs, all$np.det.assocs)
plot(all$np.det.assocs, pch=20, ylim = c(-0.3, 0.3), col="darkgreen",
     main="Associations in NPs with determiner", xlab="", ylab="Association",
     sub = paste0("µ=", z.np.det$mean, ", sd=", z.np.det$sd))
points(
  0:(nrow(all)-1),
  ifelse(is.na(all$np.det.assocs), -1, all$all.assocs),
  pch=20, cex=0.5, col = ifelse(is.na(all$np.det.assocs), "white", "darkorange"))


z.np.clt <- z.score(all$all.assocs, all$np.clt.assocs)
plot(all$np.clt.assocs, pch=20, ylim = c(-0.3, 0.3), col="darkgreen",
     main="Associations in NPs with prepositional determiner", xlab="", ylab="Association",
     sub = paste0("µ=", z.np.clt$mean, ", sd=", z.np.clt$sd))
points(
  0:(nrow(all)-1),
  ifelse(is.na(all$np.clt.assocs), -1, all$all.assocs),
  pch=20, cex=0.5, col = ifelse(is.na(all$np.clt.assocs), "white", "darkorange"))


z.np.ndt <- z.score(all$all.assocs, all$np.ndt.assocs)
plot(all$np.ndt.assocs, pch=20, ylim = c(-0.3, 0.3), col="darkgreen",
     main="Associations in NPs without a determiner", xlab="", ylab="Association",
     sub = paste0("µ=", z.np.ndt$mean, ", sd=", z.np.ndt$sd))
points(
  0:(nrow(all)-1),
  ifelse(is.na(all$np.ndt.assocs), -1, all$all.assocs),
  pch=20, cex=0.5, col = ifelse(is.na(all$np.ndt.assocs), "white", "darkorange"))


z.particip <- z.score(all$all.assocs, all$particip.assocs)
plot(all$particip.assocs, pch=20, ylim = c(-0.3, 0.3), col="darkgreen",
     main="Associations of participles", xlab="", ylab="Association",
     sub = paste0("µ=", z.particip$mean, ", sd=", z.particip$sd))
points(
  0:(nrow(all)-1),
  ifelse(is.na(all$particip.assocs), -1, all$all.assocs),
  pch=20, cex=0.5, col = ifelse(is.na(all$particip.assocs), "white", "darkorange"))


z.prog <- z.score(all$all.assocs, all$prog.assocs)
plot(all$prog.assocs, pch=20, ylim = c(-0.3, 0.3), col="darkgreen",
     main="Associations of progressives", xlab="", ylab="Association",
     sub = paste0("µ=", z.prog$mean, ", sd=", z.prog$sd))
points(
  0:(nrow(all)-1),
  ifelse(is.na(all$prog.assocs), -1, all$all.assocs),
  pch=20, cex=0.5, col = ifelse(is.na(all$prog.assocs), "white", "darkorange"))



# plot(np.det.assocs[order(np.det.assocs)], pch=20,
#     main="Associations for NPs with D", xlab="", ylab="Association")
# plot(np.clt.assocs[order(np.clt.assocs)], pch=20,
#      main="Associations for NPs with P-clitic", xlab="", ylab="Association")
# plot(np.ndt.assocs[order(np.ndt.assocs)], pch=20, ylim = c(-0.1, 0.1),
#      main="Associations for bare Ns", xlab="", ylab="Association")
# plot(particip.assocs[order(particip.assocs)], pch=20, ylim = c(-0.1, 0.1),
#      main="Associations for participle", xlab="", ylab="Association")
# plot(prog.assocs[order(prog.assocs)], pch=20, ylim = c(-0.1, 0.1),
#      main="Associations for 'am' progressive", xlab="", ylab="Association")
# plot(infzu.assocs[order(infzu.assocs)], pch=20, ylim = c(-0.1,0.05),
#      main="Associations for 'zu' infinitive", xlab="", ylab="Association")




