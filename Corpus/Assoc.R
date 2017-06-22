
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

np.det.joint     <- all$q_np_det_joint_cap + all$q_np_det_joint_nocap
np.det.joint.sum <- sum(np.det.joint)
np.det.sep       <- all$q_np_det_sep_cap + all$q_np_det_sep_nocap
np.det.sep.sum   <- sum(np.det.sep)
np.det           <- cbind(np.det.joint, np.det.sep)
np.det.assocs    <- apply(np.det, 1, function(row) {assoc(row[1], row[2], np.det.joint.sum, np.det.sep.sum, min.count)})

if (save.persistent) pdf(paste0(out.dir, 'det.pdf'))
plot(np.det.assocs, pch=20,
     main="Associations for NPs with D", xlab="", ylab="Association")
if (save.persistent) dev.off()

np.clt.joint     <- all$q_np_apprart_joint_cap + all$q_np_apprart_joint_nocap
np.clt.joint.sum <- sum(np.clt.joint)
np.clt.sep       <- all$q_np_apprart_sep_cap + all$q_np_apprart_sep_nocap
np.clt.sep.sum   <- sum(np.clt.sep)
np.clt           <- cbind(np.clt.joint, np.clt.sep)
np.clt.assocs    <- apply(np.clt, 1, function(row) {assoc(row[1], row[2], np.clt.joint.sum, np.clt.sep.sum, min.count)})
np.clt.assocs[ignore.am] <- NaN

if (save.persistent) pdf(paste0(out.dir, 'clt.pdf'))
plot(np.clt.assocs, pch=20, ylim = c(-0.1,0.1),
     main="Associations for NPs with P-clitic", xlab="", ylab="Association")
if (save.persistent) dev.off()

np.ndt.joint     <- all$q_np_nodet_joint_cap + all$q_np_nodet_joint_nocap
np.ndt.joint.sum <- sum(np.ndt.joint)
np.ndt.sep       <- all$q_np_nodet_sep_cap + all$q_np_nodet_sep_nocap
np.ndt.sep.sum   <- sum(np.ndt.sep)
np.ndt           <- cbind(np.ndt.joint, np.ndt.sep)
np.ndt.assocs    <- apply(np.ndt, 1, function(row) {assoc(row[1], row[2], np.ndt.joint.sum, np.ndt.sep.sum, min.count)})

if (save.persistent) pdf(paste0(out.dir, 'ndt.pdf'))
plot(np.ndt.assocs, pch=20, ylim = c(-0.1, 0.1),
     main="Associations for bare Ns", xlab="", ylab="Association")
if (save.persistent) dev.off()

particip.joint     <- all$q_particip_joint_cap + all$q_particip_joint_nocap
particip.joint.sum <- sum(particip.joint)
particip.sep       <- all$q_particip_sep_cap + all$q_particip_sep_nocap
particip.sep.sum   <- sum(particip.sep)
particip           <- cbind(particip.joint, particip.sep)
particip.assocs    <- apply(particip, 1, function(row) {assoc(row[1], row[2], particip.joint.sum, particip.sep.sum, min.count)})

if (save.persistent) pdf(paste0(out.dir, 'particip.pdf'))
plot(particip.assocs, pch=20, ylim = c(-0.1, 0.1),
     main="Associations for participle", xlab="", ylab="Association")
if (save.persistent) dev.off()

prog.joint     <- all$q_prog_joint_cap + all$q_prog_joint_nocap
prog.joint.sum <- sum(prog.joint)
prog.sep       <- all$q_prog_sep_cap + all$q_prog_sep_nocap
prog.sep.sum   <- sum(prog.sep)
prog           <- cbind(prog.joint, prog.sep)
prog.assocs    <- apply(prog, 1, function(row) {assoc(row[1], row[2], prog.joint.sum, prog.sep.sum, min.count)})
prog.assocs[ignore.am] <- NaN

if (save.persistent) pdf(paste0(out.dir, 'prog.pdf'))
plot(prog.assocs, pch=20, ylim = c(-0.1, 0.1),
     main="Associations for 'am' progressive", xlab="", ylab="Association")
if (save.persistent) dev.off()

infzu.joint     <- all$q_infzu_joint_cap + all$q_infzu_joint_nocap
infzu.joint.sum <- sum(infzu.joint)
infzu.sep       <- all$q_infzu_sep_cap + all$q_infzu_sep_nocap
infzu.sep.sum   <- sum(infzu.sep)
infzu           <- cbind(infzu.joint, infzu.sep)
infzu.assocs    <- apply(infzu, 1, function(row) {assoc(row[1], row[2], infzu.joint.sum, infzu.sep.sum, min.count)})

if (save.persistent) pdf(paste0(out.dir, 'infzu.pdf'))
plot(infzu.assocs, pch=20, ylim = c(-0.1,0.05),
     main="Associations for 'zu' infinitive", xlab="", ylab="Association")
if (save.persistent) dev.off()

all <- data.frame(all, np.det.assocs, np.clt.assocs, np.ndt.assocs, particip.assocs, prog.assocs, infzu.assocs)

if (save.persistent) pdf(paste0(out.dir, 'clt.det.pdf'))
plot(np.clt.assocs, np.det.assocs, pch=20, xlim=c(-0.1,0.1), ylim=c(-0.15,0.15),
     main="Correlation between NP with P-clitic and D",
     xlab="P-clitic", ylab="D")
if (save.persistent) dev.off()

if (save.persistent) pdf(paste0(out.dir, 'clt.ndt.pdf'))
plot(np.clt.assocs, np.ndt.assocs, pch=20, xlim=c(-0.1,0.1), ylim=c(-0.15,0.15),
     main="Correlation between NP with P-clitic and bare N",
     xlab="P-clitic", ylab="bare N")
if (save.persistent) dev.off()


if (save.persistent) pdf(paste0(out.dir, 'clt.prog.pdf'))
plot(np.clt.assocs, prog.assocs, pch=20, xlim=c(-0.1,0.1), ylim=c(-0.15,0.15),
     main="Correlation between NP with P-clitic and 'am' progressive",
     xlab="P-clitic", ylab="'am' progressive")
if (save.persistent) dev.off()


if (save.persistent) pdf(paste0(out.dir, 'clt.infzu.pdf'))
plot(np.clt.assocs, infzu.assocs, pch=20, xlim=c(-0.1,0.1), ylim=c(-0.15,0.15),
     main="Correlation between NP with P-clitic and 'zu' infinitive",
     xlab="P-clitic", ylab="'zu' infinitive")
if (save.persistent) dev.off()


if (save.persistent) pdf(paste0(out.dir, 'prog.infzu.pdf'))
plot(prog.assocs, infzu.assocs, pch=20, xlim=c(-0.1,0.1), ylim=c(-0.15,0.15),
     main="Correlation between 'am' progressive and 'zu' infinitive",
     xlab="'am' progressive", ylab="'zu' infinitive")
if (save.persistent) dev.off()


print.prefs <- function(df, col, show.cols, show.results, num, cx, effect) {
  if (show.results %in% c('extreme', 'for', 'all')) {
    univ.sel <- df[which(df[, col] > 0),]
    univ.sel.order <- order(abs(univ.sel[, col]), decreasing = T)
    cat('\n\nPREFERENCE FOR', effect, ' IN: ', cx, '\n\n')
    print(head(univ.sel[univ.sel.order, show.cols], n = num))
  }
  if (show.results %in% c('extreme', 'against', 'all')) {
    univ.sel <- df[which(df[, col] < 0),]
    univ.sel.order <- order(abs(univ.sel[, col]), decreasing = T)
    cat('\n\nPREFERENCE AGAINST', effect, ' IN: ', cx, '\n\n')
    print(head(univ.sel[univ.sel.order, show.cols], n = num))
  }
  if (show.results %in% c('zero', 'all')) {
    univ.sel <- df[which(!is.nan(df[, col])),]
    cat('\n\nNO PREFERENCE FOR/AGAINST', effect, ' IN: ', cx, '\n\n')
    print(univ.sel[tail(order(abs(univ.sel[, col]), decreasing = T), n = num), show.cols,])
  }
}

if (save.persistent) sink(paste0(out.dir, 'results.txt'))
cat('\n\n=======================================================================\n')
print.prefs(df = all, col = 'np.det.assocs', show.results = show.results,
            show.cols = c("Noun", "Linking", "Verb", "np.det.assocs", "FLogPerMillion"),
            num = num, cx = "NP with D", effect = "UNIVERBATION")
cat('\n\n=======================================================================\n')
print.prefs(df = all, col = 'np.clt.assocs', show.results = show.results,
            show.cols = c("Noun", "Linking", "Verb", "np.clt.assocs", "FLogPerMillion"),
            num = num, cx = "NP with P-clitic", effect = "UNIVERBATION")
cat('\n\n=======================================================================\n')
print.prefs(df = all, col = 'np.ndt.assocs', show.results = show.results,
            show.cols = c("Noun", "Linking", "Verb", "np.ndt.assocs", "FLogPerMillion"),
            num = num, cx = "bare N", effect = "UNIVERBATION")
cat('\n\n=======================================================================\n')
print.prefs(df = all, col = 'particip.assocs', show.results = show.results,
            show.cols = c("Noun", "Linking", "Verb", "particip.assocs", "FLogPerMillion"),
            num = num, cx = "Participle", effect = "UNIVERBATION")
cat('\n\n=======================================================================\n')
print.prefs(df = all, col = 'prog.assocs', show.results = show.results,
            show.cols = c("Noun", "Linking", "Verb", "prog.assocs", "FLogPerMillion"),
            num = num, cx = "'am' Progressive", effect = "UNIVERBATION")
cat('\n\n=======================================================================\n')
print.prefs(df = all, col = 'infzu.assocs', show.results = show.results,
            show.cols = c("Noun", "Linking", "Verb", "infzu.assocs", "FLogPerMillion"),
            num = num, cx = "'zu' Infinitive", effect = "UNIVERBATION")
cat('\n\n=======================================================================\n')
if (save.persistent) sink()