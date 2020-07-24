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
