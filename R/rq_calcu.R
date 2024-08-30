
rq_calcu <- function(exp, group = "group", gene = "gene", ct = "ct", gene_ctrl, sample_ctrl){

  colnames(exp)[colnames(exp) == group] <- "group"
  colnames(exp)[colnames(exp) == gene] <- "gene"
  colnames(exp)[colnames(exp) == ct] <- "ct"

  expList <- split(exp, ~group)
  expList <- lapply(expList, function(x) {
    ct_mean_ctrl <- mean(x$ct[x$gene == gene_ctrl])
    x <- x[x$gene != gene_ctrl,]
    x$dct <- x$ct- ct_mean_ctrl
    x
  })
  expList <- Reduce(rbind, expList)

  expList <- expList <- split(expList, ~gene)
  expList <- lapply(expList, function(y) {
    dct_mean_ctrl <- mean(y$dct[y$group == sample_ctrl])
    y$ddct <- y$dct- dct_mean_ctrl
    y$rq <- 2^(y$ddct*-1)
    rq_mean_ctrl <- mean(y$rq[y$group == sample_ctrl])
    y$rq <- y$rq/rq_mean_ctrl
    y
  })

  expList <- Reduce(rbind, expList)
  expList
}
