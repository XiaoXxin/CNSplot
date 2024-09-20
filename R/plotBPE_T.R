plotBPE_T <- function(dat,
                      exp = "exp",
                      group = "group",
                      levels,
                      comparison,
                      style = 2,
                      plot.B = T,
                      lab.x = "",
                      lab.y = "",
                      title = "",
                      text.x,
                      size.text.x = 10,
                      size.text.y = 10,
                      size.point = 2,
                      size.p = 4,
                      angle.x = 0,
                      breaks.y = NULL,
                      start.y = 0,
                      fill = "white",
                      jitter.width = 0.1,
                      aspect.ratio = 0.5){

  colnames(dat)[colnames(dat) == exp] <- "exp"
  colnames(dat)[colnames(dat) == group] <- "group"

  # isolate interest gene and group
  dat_sub <- dat[dat$group %in% levels,]
  dat_sub$group <- factor(dat_sub$group, levels = levels)

  # stat
  res_t <- rstatix::t_test(dat_sub, exp ~ group)
  res_t$groups <- paste(res_t$group1, res_t$group2, sep = "-")

  # comparison <- list(c(2, 3))

  res_t <- lapply(comparison, function(x) {
    tList <- res_t[grepl(levels[x[1]], res_t$groups) & grepl(levels[x[2]], res_t$groups), ]
    tList$ppos <- mean(c(x[1],x[2]))
    tList
  }) %>% Reduce(rbind, .)

  # p value position
  max <- max(dat_sub$exp)*1.1
  res_t$y = cumprod(c(max, rep(1.1, length(comparison)-1)))

  # p value format
  if(nrow(res_t)>1){
    for (i in 1:nrow(res_t)) {
    if(as.numeric(res_t$p.adj[i]) < 0.001){
      res_t$p.adj[i] <- "p < 0.001"
    }else{
      res_t$p.adj[i] <- paste0("p = ", round(as.numeric(res_t$p.adj[i]), 4))
    }
  }
  }else{
    if(as.numeric(res_t$p[1]) < 0.001){
      res_t$p[1] <- "p < 0.001"
    }else{
      res_t$p[1] <- paste0("p = ", round(as.numeric(res_t$p[1]), 4))
    }
    res_t$p.adj <- res_t$p
  }


  if(style == 1){
    res_t$vline = res_t$y
    res_t$vline_end = res_t$y
  }
  if(style == 2){
    res_t$vline = res_t$y+0.02
    res_t$vline_end = res_t$y-0.02
  }

  dat_sum <- dat_sub %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(mean = mean(exp),
                     n = length(exp),
                     sd = sd(exp),
                     sem = sd/sqrt(n))

  while(length(fill) < nrow(dat_sum)){
    fill <- c(fill,fill)
  }
  fill <- fill[1:nrow(dat_sum)]

  if(is.null(text.x)){text.x = levels}


  p <- ggplot(dat_sum, aes(x = group, y = mean, fill = group))+
    labs(x= lab.x, y = lab.y)+
    guides(fill = "none")+
    ggtitle(title)+
    scale_x_discrete(labels = text.x)+
    scale_y_continuous(expand = c(0,0), n.breaks = breaks.y)+
    scale_fill_manual(values = fill)+
    theme_bw()+
    theme(aspect.ratio = aspect.ratio,
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank(),
          axis.text.x = element_text(size = size.text.x, angle = angle.x, vjust = 0.5),
          axis.text.y = element_text(size = size.text.y))+
    coord_fixed(ylim = c(start.y, max(res_t$y)*1.1))

  if(plot.B){
    p <- p+geom_bar(stat = "identity", color = "black", alpha = 0.6, width=0.6)+
      geom_errorbar(aes(x = group, ymin = mean-sem, ymax = mean+sem),
                    width = 0.3, color = "black", alpha = 0.8, linewidth = 1)
  }else{
    p <- p+geom_errorbar(aes(x = group, ymin = mean, ymax = mean),
                         width = 0.5, color = "black", alpha = 0.8, linewidth = 1)+
      geom_errorbar(aes(x = group, ymin = mean-sem, ymax = mean+sem),
                    width = 0.3, color = "black", alpha = 0.8, linewidth = 1)
  }

  p <- p+geom_jitter(data = dat, aes(y = exp),
                     shape = 21, size = size.point, show.legend = FALSE, color = "black",
                     width = jitter.width)

  # stat
  p <- p+geom_segment(data = res_t, aes(x = group1, xend = group2, y = y, yend = y),
                      linewidth = 0.3, inherit.aes = F)+
    geom_segment(data = res_t, aes(x = group1, xend = group1, y = vline, yend = vline_end),
                 linewidth = 0.3, inherit.aes = F)+
    geom_segment(data = res_t, aes(x = group2, xend = group2, y = vline, yend = vline_end),
                 linewidth = 0.3, inherit.aes = F)+
    geom_text(data = res_t, aes(x = ppos-0.2, y = y*1.02, label = p.adj),
              size = size.p, vjust = 0, hjust = 0, nudge_x = 0, inherit.aes = F)

  p

}
