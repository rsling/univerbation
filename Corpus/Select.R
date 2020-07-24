


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


print.prefs <- function(df, col, show.cols, show.results, num, cx, effect, freq.cutoff = -1) {

  if (freq.cutoff > 0) {
    .fc <- round(nrow(df)*freq.cutoff, 0)
    df <- df[order(df$FLogPerMillion, decreasing = T)[1:.fc],]
  }

  .result <- NULL

  if (show.results %in% c('extreme', 'for', 'all')) {
    univ.sel <- df[which(df[, col] > 0),]
    univ.sel.order <- order(abs(univ.sel[, col]), decreasing = T)
    cat('\n\nPREFERENCE FOR', effect, ' IN: ', cx, '\n\n')
    .tmp <- head(univ.sel[univ.sel.order, show.cols], n = num)
    print(.tmp)
    .result <- .tmp
  }

  if (show.results %in% c('zero', 'all')) {
    univ.sel <- df[which(!is.nan(df[, col])),]
    cat('\n\nNO PREFERENCE FOR/AGAINST', effect, ' IN: ', cx, '\n\n')
    .tmp <- univ.sel[tail(order(abs(univ.sel[, col]), decreasing = T), n = num), show.cols,]
    .tmp <- .tmp[order(.tmp[, col], decreasing = T),]
    print(.tmp)
    if (is.null(.result)) .result <- .tmp
    else .result <- rbind(.result, .tmp)
  }

  if (show.results %in% c('extreme', 'against', 'all')) {
    univ.sel <- df[which(df[, col] < 0),]
    univ.sel.order <- order(abs(univ.sel[, col]), decreasing = T)
    cat('\n\nPREFERENCE AGAINST', effect, ' IN: ', cx, '\n\n')
    .tmp <- head(univ.sel[univ.sel.order, show.cols], n = num)
    .tmp <- .tmp[order(.tmp[, col], decreasing = T),]
    print(.tmp)
    if (is.null(.result)) .result <- .tmp
    else .result <- rbind(.result, .tmp)
  }
  .result
}

if (save.persistent) sink(paste0(out.dir, 'results.txt'))
cat('\n\n=======================================================================\n')
tmp <- print.prefs(df = all, col = 'all.assocs', show.results = "all",
                   show.cols = c("Noun", "Linking", "Verb", "all.assocs", "FLogPerMillion"),
                   num = num, cx = "All contexts", effect = "UNIVERBATION", freq.cutoff = 0.2)
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
