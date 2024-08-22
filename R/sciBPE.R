#' A function to draw bar (B) plot with point (P) and error (E) bars
#'
#' @param dat a data frame
#' @param plot_B boolean values determining if bar plot should be drawn
#' @param levels levels of samples
#' @param lab.sample labels of samples
#' @param lab.x label of x-axis
#' @param lab.y label of y-axis
#' @param size.text.x size of x-axis text
#' @param size.text.y size of y-axis text
#' @param size.point size of point
#' @param title title
#' @param fill fill colors
#' @param ylims two numeric values, specifying the lower limit and the upper limit of the scale
#' @param aspect.ratio ratio of the x axis and y axis
#'
#' @return a ggplot2 plot
#' @export
#'
#' @examples
sciBPE <- function(dat,
                   plot_B = T,
                   levels,
                   lab.sample = NULL,
                   lab.x = "",
                   lab.y = "",
                   size.text.x = 10,
                   size.text.y = 10,
                   size.point = 2,
                   title = "",
                   fill = "black",
                   ylims,
                   aspect.ratio = 0.5){
  dat <- gather(dat, group, exp) %>% mutate(group = factor(group, levels = levels))

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

  if(is.null(lab.sample)){lab.sample = levels}

  p <- ggplot(dat_sum, aes(x = group, y = mean, fill = group))+
    labs(x= lab.x, y = lab.y)+
    guides(fill = "none")+
    ggtitle(title)+
    scale_x_discrete(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    scale_fill_manual(values = fill, labels = lab.sample)+
    theme_bw()+
    theme(aspect.ratio = 0.5,
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank(),
          axis.text.x = element_text(size = size.text.x),
          axis.text.y = element_text(size = size.text.y))+
    coord_fixed(ylim = c(ylims[1], ylims[2]))

  if(plot_B){
    p <- p+geom_bar(position = position_dodge(1), stat = "identity", color = "black", alpha = 0.6, width=0.6)+
      geom_errorbar(aes(x = group, ymin = mean-sem, ymax = mean+sem),
                         position = position_dodge(1), width = 0.3, color = "black", alpha = 0.5, linewidth = 1)
  }else{
    p <- p+geom_errorbar(aes(x = group, ymin = mean, ymax = mean),
                         position = position_dodge(1), width = 0.5, color = "black", alpha = 0.5, linewidth = 1)+
      geom_errorbar(aes(x = group, ymin = mean-sem, ymax = mean+sem),
                    position = position_dodge(1), width = 0.3, color = "black", alpha = 0.5, linewidth = 1)
  }


  p <- p+geom_point(data = dat, aes(y = exp), shape = 21, size = size.point, show.legend = FALSE, color = "black",
               position = position_jitterdodge(0.2, dodge.width = 1))

  p


}
