tpr <- function(cm){
  cm[2, 2]/(sum(cm[, 2]))
}

tnr <- function(cm){
  cm[1, 1]/(sum(cm[, 1]))
}

ppv <- function(cm){
  cm[2, 2]/(sum(cm[2, ]))
}

f_score <- function(cm){
  (2*tpr(cm) * ppv(cm))/(tpr(cm) + ppv(cm))
}

kappa <- function(cm){
  n <- sum(cm)
  p_o <- sum(diag(cm))/n
  p_rpa <- (sum(cm[2, ]))/n * (sum(cm[, 2]))/n
  p_rna <- (sum(cm[1, ]))/n * (sum(cm[, 1]))/n
  p_r <- p_rpa + p_rna
  (p_o - p_r)/(1 - p_r)
}