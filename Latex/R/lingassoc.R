### Calculate linguistic association measures.

assoc <- function(f1, f2, cs1, cs2, min.count=5, smooth=1, method="Cramer") {
  if (f1+f2 < min.count) r <- NaN
  else {
    r <- 0
    f1  <- f1 + smooth
    f2  <- f2 + smooth
    cs1 <- cs1 + smooth
    cs2 <- cs2 + smooth
    if (method=="Cramer") {
      .x2 <- chisq.test(matrix(c(f1,f2,cs1-f1,cs2-f2), nrow = 2), correct = T)
      .r <- as.numeric(sqrt(.x2$statistic/(cs1+cs2))) * -sign(.x2$expected-.x2$observed)[1,1]
    } else if (method=="CramerMC") {
      .x2 <- chisq.test(matrix(c(f1,f2,cs1-f1,cs2-f2), nrow = 2), simulate.p.value = T, B = sum(c(f1,f2,cs1-f1,cs2-f2)%/%10), correct = T)
      .r <- as.numeric(sqrt(.x2$statistic/(cs1+cs2))) * -sign(.x2$expected-.x2$observed)[1,1]
    } else if (method=="Fisher") {
      .f <- fisher.test(matrix(c(f1,f2,cs1-f1,cs2-f2), nrow = 2))
      .x2 <- chisq.test(matrix(c(f1,f2,cs1-f1,cs2-f2), nrow = 2), correct = T)
      .r <- as.numeric(.f$p.value) * -sign(.x2$expected-.x2$observed)[1,1]
    } else if (method=="Odds") {
      .f <- fisher.test(matrix(c(f1,f2,cs1-f1,cs2-f2), nrow = 2))
      .x2 <- chisq.test(matrix(c(f1,f2,cs1-f1,cs2-f2), nrow = 2), correct = T)
      .r <- as.numeric(.f$estimate) * -sign(.x2$expected-.x2$observed)[1,1]
    }
    .r
  }
}



format.assocs <- function(df, col, show.cols, show.results, num, cx, effect, freq.cutoff = -1, quiet = T) {

  if (freq.cutoff > 0) {
    .fc <- round(nrow(df)*freq.cutoff, 0)
    df <- df[order(df$FLogPerMillion, decreasing = T)[1:.fc],]
  }

  .result <- NULL

  if (show.results %in% c('extreme', 'for', 'all')) {
    univ.sel <- df[which(df[, col] > 0),]
    univ.sel.order <- order(abs(univ.sel[, col]), decreasing = T)
    if (!quiet) cat('\n\nPREFERENCE FOR', effect, ' IN: ', cx, '\n\n')
    .tmp <- head(univ.sel[univ.sel.order, show.cols], n = num)
    if (!quiet) print(.tmp)
    .result <- .tmp
  }

  if (show.results %in% c('zero', 'all')) {
    univ.sel <- df[which(!is.nan(df[, col])),]
    if (!quiet) cat('\n\nNO PREFERENCE FOR/AGAINST', effect, ' IN: ', cx, '\n\n')
    .tmp <- univ.sel[tail(order(abs(univ.sel[, col]), decreasing = T), n = num), show.cols,]
    .tmp <- .tmp[order(.tmp[, col], decreasing = T),]
    if (!quiet) print(.tmp)
    if (is.null(.result)) .result <- .tmp
    else .result <- rbind(.result, .tmp)
  }

  if (show.results %in% c('extreme', 'against', 'all')) {
    univ.sel <- df[which(df[, col] < 0),]
    univ.sel.order <- order(abs(univ.sel[, col]), decreasing = T)
    if (!quiet) cat('\n\nPREFERENCE AGAINST', effect, ' IN: ', cx, '\n\n')
    .tmp <- head(univ.sel[univ.sel.order, show.cols], n = num)
    .tmp <- .tmp[order(.tmp[, col], decreasing = T),]
    if (!quiet) print(.tmp)
    if (is.null(.result)) .result <- .tmp
    else .result <- rbind(.result, .tmp)
  }
  .result
}
