
get_formula_left <- function(formula){
  deparse(formula[[2]])
}

get_formula_right <- function(formula){
  attr(stats::terms(formula), "term.labels")
}

reconstruct_dataframe <- function(dat, formula){
  dat <- as.data.frame(dat)
  data.frame(exp = dat[,get_formula_left(formula)],
             group = dat[,get_formula_right(formula)])
}

p_line_x <- function(dat, comparisons){
  levels <- levels(dat$group)
  p_value_x <- lapply(comparisons, function(x) match(x, levels)) %>%
    Reduce(rbind,.) %>%
    magrittr::set_colnames(c("x1", "x2")) %>%
    magrittr::set_rownames(1:nrow(.))
  p_value_x
}

p_line_y <- function(dat, comparisons, nudge.p){
  levels <- levels(dat$group)
  comparisons <- lapply(comparisons, function(x) match(x, levels))
  maxs <- dat %>% dplyr::group_by(group) %>% dplyr::summarise(max = max(exp))
  while(length(nudge.p) < length(comparisons)){
    nudge.p <- c(nudge.p,nudge.p)
  }
  nudge.p <- nudge.p[1:length(comparisons)]

  p.y <- c()
  for (i in 1:length(comparisons)) {
    compar <- comparisons[[i]]
    max <- maxs$max[maxs$group %in% levels[compar[1]:compar[2]]] %>% max()*nudge.p[i]
    p.y[i] <- max
    maxs$max[maxs$group %in% levels[compar[1]:compar[2]]] <- max
  }
  p.y
}

p_value_format <- function(p){
  for (i in 1:length(p)) {
    if(p[i] < 0.001){
      p[i] <- "p < 0.001"
    }else{
      p[i] <- paste0("p = ", round(as.numeric(p[i]), 4))
    }
    }
  p
}
