#' A function to draw bar (B) plot with point (P) and error (E) bars
#'
#' @param dat a data frame
#' @param levels level 1 must be the control group
#' @param comparison a numeric vector indicates which groups are used to compare with control
#' @param style 1 or 2
#' @param plot.B boolean values determining if bar plot should be drawn
#' @param lab.x label of x-axis
#' @param lab.y label of y-axis
#' @param title title
#' @param text.x x-axis text
#' @param size.text.x size of x-axis text
#' @param size.text.y size of y-axis text
#' @param size.point size of point
#' @param size.p size of p values
#' @param angle.x angle of x-axis text
#' @param breaks.y breaks of y-axis
#' @param fill fill colors
#' @param jitter.width jitter width of the points
#' @param aspect.ratio ratio of the x axis and y axis
#'
#' @return a ggplot2 plot
#' @export
#'
#' @examples
plotBPE_Dunn <- function(dat,
                       levels,
                       comparison,
                       style,
                       plot.B = T,
                       lab.x = "",
                       lab.y = "",
                       title = "",
                       text.x,
                       size.text.x = 10,
                       size.text.y = 10,
                       size.point = 2,
                       size.p = 3,
                       angle.x = 0,
                       breaks.y = NULL,
                       fill,
                       jitter.width = 0.1,
                       aspect.ratio = 0.5){

  # isolate interest gene and group
  dat_sub <- dat[dat$group %in% levels,]
  dat_sub$group <- factor(dat_sub$group, levels = levels)

  # stat
  res_dunn <- rstatix::dunn_test(dat_sub, p.adjust.method = "BH", exp ~ group)
  res_dunn <- res_dunn[res_dunn$group1 == levels[1],]
  res_dunn <- res_dunn[match(levels[comparison], res_dunn$group2),]
  res_dunn$ppos <- (1+comparison)/2

  # p value position
  max <- max(dat_sub$exp)*1.1
  res_dunn$y = cumprod(c(max, rep(1.1, length(comparison)-1)))

  # p value format
  for (i in 1:nrow(res_dunn)) {
    if(as.numeric(res_dunn$p.adj[i]) < 0.001){
      res_dunn$p.adj[i] <- "p < 0.001"
    }else{
      res_dunn$p.adj[i] <- paste0("p = ", round(as.numeric(res_dunn$p.adj[i]), 4))
    }
  }

  if(style == 1){
    res_dunn$vline = res_dunn$y
    res_dunn$vline_end = res_dunn$y
  }
  if(style == 2){
    res_dunn$vline = res_dunn$y+0.02
    res_dunn$vline_end = res_dunn$y-0.02
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
    coord_fixed(ylim = c(0, max(res_dunn$y)*1.1))

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
  p <- p+geom_segment(data = res_dunn, aes(x = group1, xend = group2, y = y, yend = y),
                      linewidth = 0.3, inherit.aes = F)+
    geom_segment(data = res_dunn, aes(x = group1, xend = group1, y = vline, yend = vline_end),
                 linewidth = 0.3, inherit.aes = F)+
    geom_segment(data = res_dunn, aes(x = group2, xend = group2, y = vline, yend = vline_end),
                 linewidth = 0.3, inherit.aes = F)+
    geom_text(data = res_dunn, aes(x = ppos-0.2, y = y*1.02, label = p.adj),
              size = size.p, vjust = 0, hjust = 0, nudge_x = 0, inherit.aes = F)

  p

}
