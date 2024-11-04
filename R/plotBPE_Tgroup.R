plotBPE_Tgroup <- function(dat,
                      formula,
                      color,
                      plot.B = T,
                      lab.x = "",
                      lab.y = "",
                      title = "",
                      text.x,
                      size.text.x = 10,
                      size.text.y = 10,
                      size.point = 2,
                      size.p = 4,
                      nudge.p = 1.3,
                      angle.p = 45,
                      angle.x = 0,
                      label.group,
                      breaks.y = NULL,
                      expand.up = 1.5,
                      expand.down = 1.5,
                      start.y = 0,
                      fill = "white",
                      aspect.ratio = 0.5){

  color <- dat[,color]
  dat <- reconstruct_dataframe(dat, formula = formula)
  dat$color <- color

  exp_sub <- split(dat, dat$group)

  exp_sum <- lapply(exp_sub, function(x) {
    dplyr::group_by(x, color) %>%
      dplyr::summarise(mean = mean(exp),
                       n = length(exp),
                       sd = sd(exp),
                       sem = sd/sqrt(n)) %>%
      dplyr::mutate(group = unique(x$group))
  }) %>% Reduce(rbind,.)


  res_t <- lapply(exp_sub, function(x) {
    rstatix::t_test(x, exp ~ color)
  }) %>% Reduce(rbind,.)

  res_t <- data.frame(p = res_t$p,
                      x = 1:nrow(res_t),
                      line.x = 1:nrow(res_t)-0.25,
                      line.x.end = 1:nrow(res_t)+0.25)

  for (i in 1:nrow(res_t)) {
    if(res_t$p[i] < 0.001){
      res_t$p[i] <- "p < 0.001"
    }else{
      res_t$p[i] <- paste0("p = ", res_t$p[i])
    }
  }

  exp_sub <- Reduce(rbind,exp_sub)


  max <- max(exp_sub$exp)
  min <- min(exp_sub$exp)
  p <- ggplot(exp_sum, aes(x = group, y = mean, fill = color))+
    geom_errorbar(aes(x = group, ymin=mean-sem, ymax=mean+sem),
                  position = position_dodge(1), width=0.3, colour="black", alpha=0.9, linewidth =1)+
    geom_errorbar(aes(x = group, ymin=mean, ymax=mean),
                  position = position_dodge(1), width=0.5, alpha=1, linewidth = 1.6)+
    geom_point(data = exp_sub, aes(y = exp), shape = 21, size = 2, color = "black",
               position = position_jitterdodge(0.2, dodge.width = 1))+
    geom_segment(data = res_t, aes(x = line.x, xend = line.x.end, y = max*1.05, yend = max*1.05),
                 linewidth = 0.3, inherit.aes = F)+
    geom_text(data = res_t, aes(x = x, y = max*nudge.p, label = p),
              size = size.p, vjust = -0.5, hjust = 0.5, nudge_x = 0, angle = angle.p, inherit.aes = F)+
    ggtitle(title)+
    scale_x_discrete(expand = c(0,0))+
    scale_fill_manual(values = fill, labels = label.group)+
    labs(x= lab.x, y = lab.y)+
    theme_bw()+
    theme(aspect.ratio = aspect.ratio,
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank(),
          axis.text.x = element_text(size = size.text.x, angle = angle.x, vjust = 0.5),
          axis.text.y = element_text(size = size.text.y))+
    coord_fixed(ylim = c(min-expand.down, max+expand.up))


  p

}
