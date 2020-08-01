
save.persistent <- FALSE

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
print.prefs(df = all, col = 'np.det.assocs', show.results = show.results,
            show.cols = c("Compound", "np.det.assocs", "FLogPerMillion", "Relation", "Valency"),
            num = num, cx = "NP with D", effect = "UNIVERBATION")
cat('\n\n=======================================================================\n')
print.prefs(df = all, col = 'np.clt.assocs', show.results = show.results,
            show.cols = c("Compound", "np.clt.assocs", "FLogPerMillion", "Relation", "Valency"),
            num = num, cx = "NP with P-clitic", effect = "UNIVERBATION")
cat('\n\n=======================================================================\n')
print.prefs(df = all, col = 'np.ndt.assocs', show.results = show.results,
            show.cols = c("Compound", "np.ndt.assocs", "FLogPerMillion", "Relation", "Valency"),
            num = num, cx = "bare N", effect = "UNIVERBATION")
cat('\n\n=======================================================================\n')
print.prefs(df = all, col = 'particip.assocs', show.results = show.results,
            show.cols = c("Compound", "particip.assocs", "FLogPerMillion", "Relation", "Valency"),
            num = num, cx = "Participle", effect = "UNIVERBATION")
cat('\n\n=======================================================================\n')
print.prefs(df = all, col = 'prog.assocs', show.results = show.results,
            show.cols = c("Compound", "prog.assocs", "FLogPerMillion", "Relation", "Valency"),
            num = num, cx = "'am' Progressive", effect = "UNIVERBATION")
cat('\n\n=======================================================================\n')
print.prefs(df = all, col = 'infzu.assocs', show.results = show.results,
            show.cols = c("Compound", "infzu.assocs", "FLogPerMillion", "Relation", "Valency"),
            num = num, cx = "'zu' Infinitive", effect = "UNIVERBATION")
cat('\n\n=======================================================================\n')
tmp <- print.prefs(df = all, col = 'all.assocs', show.results = "all",
                   show.cols = c("Compound", "all.assocs", "FLogPerMillion", "Relation", "Valency"),
                   num = num, cx = "All contexts", effect = "UNIVERBATION", freq.cutoff = 0.2)
cat('\n\n=======================================================================\n')
if (save.persistent) sink()
