#' Calculate relative expression from Ct values
#'
#' @param exp a data frame
#' @param group group column
#' @param gene gene column
#' @param ct ct value column
#' @param gene_ctrl contrl gene
#' @param sample_ctrl contrl group
#'
#' @return a data frame of exp
#'
#' @examples none
rq_calcu <- function(exp, group = "group", gene = "gene", ct = "ct", gene_ctrl, sample_ctrl){

  colnames(exp)[colnames(exp) == group] <- "group"
  colnames(exp)[colnames(exp) == gene] <- "gene"
  colnames(exp)[colnames(exp) == ct] <- "ct"

  expList <- split(exp, ~sample)
  expList <- lapply(expList, function(x) {
    ct_mean_ctrl <- mean(x$ct[x$gene == gene_ctrl])
    x <- x[x$gene != gene_ctrl,]
    x$dct <- x$ct- ct_mean_ctrl
    x
  })
  expList <- Reduce(rbind, expList)

  expList <- split(expList, ~gene)
  expList <- lapply(expList, function(y) {
    dct_mean_ctrl <- mean(y$dct[y$group == sample_ctrl])
    y$ddct <- y$dct- dct_mean_ctrl
    y$exp <- 2^(y$ddct*-1)
    rq_mean_ctrl <- mean(y$exp[y$group == sample_ctrl])
    y$exp <- y$exp/rq_mean_ctrl
    y
  })

  expList <- Reduce(rbind, expList)
  expList
}
