### Calculate linguistic association measures.

assoc <- function(f1, f2, cs1, cs2, min.count=5, smooth=1, method="Odds") {
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
      .r <- as.numeric(.f$p.value) * -sign(.x2$expected-.x2$observed)[1,1]
    } else if (method=="Odds") {
      .t <- matrix(c(f1,f2,cs1-f1,cs2-f2), nrow = 2)
      .r <- log((.t[1,1]/.t[1,2])/(.t[2,1]/.t[2,2]))
    }
    .r
  }
}
