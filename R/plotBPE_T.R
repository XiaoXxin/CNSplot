#' @include utilities.R
#' A function to draw bar (B) plot with point (P) and error (E) bars with t-test
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
#' @param start.y start of y-axis
#' @param fill fill colors
#' @param jitter.width jitter width of the points
#' @param aspect.ratio ratio of the x axis and y axis
#' @param nudge.p nudge of p values
#'
#' @return a ggplot2 plot
#' @export
#'
#' @examples
plotBPE_T <- function(dat,
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
                      size.p = 4,
                      nudge.p = 1.1,
                      angle.x = 0,
                      breaks.y = NULL,
                      start.y = 0,
                      fill = "white",
                      jitter.width = 0.1,
                      aspect.ratio = 0.5){

  dat <- reconstruct_dataframe(dat, formula = formula)

  res_t <- rstatix::t_test(dat, exp~group, comparisons)

  res_t$y = p_line_y(dat, comparisons, nudge.p)

  res_t <- cbind(res_t, p_line_x(dat, comparisons))

  if("p.adj" %in% colnames(res_t)){
    res_t$p.layout <- p_value_format(res_t$p.adj)
  }else{
    res_t$p.layout <- p_value_format(res_t$p)
  }

  res_t$x <- rowMeans(res_t[,c("x1","x2")])

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

  p <- p+geom_segment(data = res_t, aes(x = x1, xend = x2, y = y, yend = y),
                      linewidth = 0.3, inherit.aes = F)+
    geom_text(data = res_t, aes(x = x-0.2, y = y*1.02, label = p.layout),
              size = size.p, vjust = 0, hjust = 0, nudge_x = 0, inherit.aes = F)

  p

}
