#' A function to draw bar (B) plot with point (P) and error (E) bars
#'
#' @param dat a data frame
#' @param formula a formula
#' @param comparisons A list of length-2 vectors specifying the groups of interest to be compared
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
#' @param aspect.ratio ratio of the x axis and y axis
#' @param nudge.p nudge of p values
#'
#' @return a ggplot2 plot
#' @export
#'
#' @examples
plotBPE_Dunn <- function(dat,
                         formula,
                         comparisons,
                         plot.B = T,
                         lab.x = "",
                         lab.y = "",
                         title = "",
                         text.x,
                         size.text.x = 10,
                         size.text.y = 10,
                         size.point = 2,
                         size.p = 3,
                         nudge.p = 1.1,
                         angle.x = 0,
                         breaks.y = NULL,
                         start.y = 0,
                         fill = "white",
                         aspect.ratio = 0.5){

  dat <- reconstruct_dataframe(dat, formula = formula)

  res_dunn <- rstatix::dunn_test(dat, p.adjust.method = "BH", exp ~ group)
  levels <- levels(dat$group)
  res_dunn <- res_dunn[res_dunn$group1 == levels[1],]
  res_dunn <- res_dunn[match(comparisons, res_dunn$group2),]

  comparisons <- data.frame(group1 = levels[1], group2 = comparisons) %>% split(~group2)
  comparisons <- comparisons[res_dunn$group2]
  res_dunn$y = p_line_y(dat, comparisons, nudge.p)

  if(nrow(res_dunn)>1){
    res_dunn <- cbind(res_dunn, p_line_x(dat, comparisons))
  }else{
    res_dunn$x1 <- 1
    res_dunn$x2 <- grep(res_dunn$group2, levels)
  }

  if("p.adj" %in% colnames(res_dunn)){
    res_dunn$p.layout <- p_value_format(res_dunn$p.adj)
  }else{
    res_dunn$p.layout <- p_value_format(res_dunn$p)
  }

  res_dunn$x <- rowMeans(res_dunn[,c("x1","x2")])


  dat_sum <- dat %>%
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
    coord_fixed(ylim = c(start.y, max(res_dunn$y)*1.1))

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

  p <- p+ggbeeswarm::geom_beeswarm(data = dat, aes(y = exp), cex=2,priority='density',
                                   shape = 21, size = size.point, show.legend = FALSE,
                                   color = "black")
  # stat
  p <- p+geom_segment(data = res_dunn, aes(x = x1, xend = x2, y = y, yend = y),
                      linewidth = 0.3, inherit.aes = F)+
    geom_text(data = res_dunn, aes(x = x-0.2, y = y*1.02, label = p.layout),
              size = size.p, vjust = 0, hjust = 0, nudge_x = 0, inherit.aes = F)

  p

}
