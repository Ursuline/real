library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(viridis)
#library(hrbrthemes)

plot_dir <- 'plots/'

theme_get()

theme_update(plot.title         = element_text(hjust = 0.5,
                                               size  = 12,
                                               face = 'bold'),
             panel.grid.minor.x = element_blank())

output_plot <- function(filename){
  filename <- paste(plot_dir, filename, sep='')
  ggsave(filename, width = 30, height = 20, dpi=600, units = "cm")
  dev.off()
}

to_png_old <- function(filename, width, height, res){
  file <- paste(plot_dir, filename,'.png', sep='')
  print(file)
  dev.copy(png,
           file = file,
           width = width, height = height, res=res, units = "cm")
  dev.off()
}

to_png <- function(filename, g, width, height, res){
  ggsave(paste(filename, '.png'),
         path = plot_dir,
         plot = last_plot(),
         scale = 1,
         width = width,
         height = height,
         units = 'cm',
         dpi = res)
  dev.off()
}

# Plot title building routine
build_title <- function(prefix, commune, year){
  title <- paste(prefix, commune, year)
  return(title)
}

# Plot filename building routine
build_filename <- function(prefix, commune, year){
  name <- paste(prefix, '_', commune, '_', year, sep='')
  return(name)
}

# Plot filename building routine
build_caption <- function(nsales, max_vf){
  cap <- paste('(',nsales, ' transactions \u2264 ', format(max_vf, digits=0, scientific = FALSE, big.mark=" "),'\u20ac)', sep='')
  return(cap)
}

# Correlation plots
correlation_plot <- function(df.cor, commune, year){
  p <- chart.Correlation(df.cor, histogram=TRUE, pch=19)
  filename <- build_filename('correlation', commune, year)

  res <- cor.mtest(df.cor, conf.level = .99)
  corrplot(cor(df.cor),
           type = "upper",
           order = "hclust",
           p.mat = res$p,
           insig = "blank",
           col = brewer.pal(n = 8, name = "RdBu"))
  to_png(filename, 30, 20, 360)
  return(p)
}

# Volume plot
volume_plot <- function(df, colName, title, x_label, y_label, caption, filename){
  nudge_x <- .125
  nudge_y <- -20
  p <- ggplot(df, aes(x=as.factor(!! sym(colName)),  fill=as.factor(!! sym(colName)))) +
    geom_bar(stat="count") +
    labs(title = title,
         caption = caption,
         x = x_label,
         y = y_label)+
    guides(fill=FALSE)+
    geom_text(
      aes(label=stat(count), group=1),
      stat='count',
      nudge_x = -nudge_x,
      nudge_y = nudge_y,
      size=4,
      color='white'
    ) +
    geom_text(
      aes(label=sprintf(" (%1.1f%%)", stat(prop)*100), group=1),
      stat='count',
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      size=4,
      color='white'
    ) +
    scale_fill_brewer(palette='Dark2')
  print(p)
  to_png(filename, 40, 30, 360)
  return(p)
}

# Histogram plots
plot_histogram <- function(df, colname, title, x_label, y_label, caption, mean_value, median_value, mode_value, max_value, nbins, filename) {
  fill     <- 'skyblue3'
  color    <- 'grey'
  mean_color   <- 'brown3'
  median_color <- 'yellow'
  mode_color   <- 'blue4'
  x_offset <- max_value/9

  variable <- df[ , colname]

  p <- df %>%
    ggplot(aes(variable)) +
    geom_histogram(stat = "bin",
                   bins = nbins,
                   aes(fill = I(fill), color = I(color))) +
    labs (
      title = title,
      x = x_label,
      y = y_label,
      caption = caption
    ) +
    scale_x_continuous(labels = comma) +
    geom_vline(xintercept = mean_value, lwd=1, linetype = "dashed", color=mean_color) +
    annotate("text",
             label = paste('Mean:', format(mean_value, digits=0, scientific = FALSE, justify = "r", big.mark=" "), sep=''),
             x = mean_value + x_offset,
             y = 5,
             color = mean_color) +
    geom_vline(xintercept = median_value, lwd=1, linetype='dashed', color=median_color) +
    annotate("text",
             label = paste('Median:', format(median_value, digits=0, scientific = FALSE, justify = "r", big.mark=" "), sep=''),
             x = median_value - x_offset,
             y = 9,
             color = median_color) +
    geom_vline(xintercept = mode_value, lwd=1, linetype='dashed', color=mode_color) +
    annotate("text",
             label = paste('Mode:', format(mode_value, digits=0, scientific = FALSE, justify = "r", big.mark=","), sep=''),
             x = mode_value - x_offset,
             y = 13,
             color = mode_color)
  print(p)
  to_png(filename, 40, 30, 360)
  return(p)
}

facet_summary_data <- function(df, colName, facetName, title, x_label, y_label, caption, filename, regression_flag) {
  color <- 'skyblue3'

  p <- df %>%
    ggplot(aes_string(x='Month', y=colName)) +
    geom_line(color = color, group = 1, size = .75) +
    geom_point(size = 2, color = color) +
    scale_y_continuous(labels = comma) +
    expand_limits(y=0) +
    scale_x_continuous(breaks = seq(1, 12))

  if (regression_flag == TRUE){
    y_max <- max(df[colName])
    regression_color <- 'tomato'
    line_size <- .75
    p <- p +
      geom_smooth(method = lm, se = FALSE, color = regression_color, size = line_size) +
      stat_cor(label.x = 6.5, label.y = -10, color = regression_color ,size=3) +
      stat_regline_equation(label.x = 1, label.y = -10, color = regression_color, size=3)
  }

  if(colName %in% c('med', 'avg')){ # include +/- sdev on plot
    y_max <- max(df$max)
    sd_color <- 'green'
    line_size <- .75
    p <- p +
      geom_line(data=df, aes(x=Month, y=avg+sd), color = sd_color, group = 1, size = line_size, linetype="dashed") +
      geom_line(data=df, aes(x=Month, y=avg-sd), color = sd_color, group = 1, size = line_size, linetype="dashed")
  }
  if(facetName == 'Code.postal') p <- p + facet_wrap(~ Code.postal)
  if(facetName == 'Surface.cat') p <- p + facet_wrap(~ Surface.cat)
  p <- p +
    labs(title = title,
         caption = caption,
         x = x_label,
         y = y_label)
  print(p)
  to_png(filename, 40, 30, 360)
  return(p)
}

# Plot sequential summary data
plot_summary_data <- function(df, groupName, colName, title, x_label, y_label, caption, filename, regression_flag) {
  color <- 'skyblue3'

  if(groupName == 'Surface.cat'){
    levels_ = df$Surface.cat %>% levels()
    df$Surface.cat <- factor(df$Surface.cat,
                             levels = levels_)
  }

  p <- df %>%
    ggplot(aes_string(x=groupName, y=colName)) +
    geom_line(color = color, group = 1, size = .75) +
    geom_point(size = 2, color = color) +
    labs(title = title,
         caption = caption,
         x = x_label,
         y = y_label)+
    scale_y_continuous(labels = comma) +
    expand_limits(y=0)

  if (groupName == 'Month'){
    p <- p +
      scale_x_continuous(breaks = seq(1, 12))
  }

  if (regression_flag == TRUE){
    y_max <- max(df[colName])
    regression_color <- 'tomato'
    line_size <- .75
    p <- p +
      geom_smooth(method = lm, se = FALSE, color = regression_color, size = line_size, linetype='dashed') +
      stat_cor(label.x = 7, label.y = y_max/10, color = regression_color) +
      stat_regline_equation(label.x = 1, label.y = y_max/10, color = regression_color)
  }

  if(colName %in% c('med', 'avg')){ # include +/- sdev on plot
    y_max <- max(df$max)
    sd_color <- 'green4'
    line_size <- .75
    p <- p +
      geom_line(data=df, aes(x=!!sym(groupName), y=avg+sd), color = sd_color, group = 1, size = line_size, linetype="dotted") +
      geom_line(data=df, aes(x=!!sym(groupName), y=avg-sd), color = sd_color, group = 1, size = line_size, linetype="dotted")
  }
  print(p)
  #output_plot(filename)
  to_png(filename, 40, 30, 360)
  return(p)
}

plot_summary_data2 <- function(df, groupName1, groupName2, colName, title, x_label, y_label, caption, filename) {
  color <- 'skyblue3'
  size <- 1.25

  if(groupName2 == 'Surface.cat'){
    levels_ = df$Surface.cat%>% levels()
    df$Surface.cat <- factor(df$Surface.cat,
                             levels = levels_)
  }

  p <- df %>%
    ggplot(aes_string(x=groupName1, y=colName, color=groupName2)) +
    geom_line(size=size) +
    geom_point(color='grey', size=size) +
    ggtitle(title) +
    labs(x=x_label,
         y=y_label,
         title=title,
         caption=caption)+
    scale_y_continuous(labels = comma)

  if (groupName1 == 'Month'){
    p <- p +
      scale_x_continuous(breaks = seq(1, 12))
  }
  print(p)
  to_png(filename, 40, 30, 360)
  return(p)
}

# Bar plot 1 variable
bar_plot <- function(df, xvar, cvalue, title, xlabel, ylabel, caption, filename){
p <- df %>%
  ggplot(aes(x=as.factor(!!sym(xvar)), y=!!sym(cvalue), fill=as.factor(!!sym(xvar)))) +
  geom_bar(stat='identity') +
  labs(title = title,
       caption = caption,
       x = xlabel,
       y = ylabel) +
  guides(fill=FALSE) +
  geom_text(aes(label = format(as.numeric(..y..), nsmall=0, big.mark=" ")),
            size = 4,
            color = 'white',
            hjust = 0.5,
            vjust = 3) +
  geom_errorbar(aes(ymin=!!sym(cvalue)-sd, ymax=!!sym(cvalue)+sd),
                width=.2,
                position=position_dodge(.9))+
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette='Dark2')
  print(p)
  to_png(filename, 40, 30, 360)
  return(p)
}

# Violin plot 1 variable
violin_plot <- function(df, xvar, yvar, title, xlabel, ylabel, caption, filename, facet_flag){
  p <- df %>%
    ggplot(aes(x=as.factor(!!sym(xvar)), y=!!sym(yvar), fill=as.factor(!!sym(xvar)))) +
    geom_violin() +
    labs(title = title,
         caption = caption,
         x = xlabel,
         y = ylabel) +
    guides(fill=FALSE) +
    scale_y_continuous(labels = comma) +
    geom_boxplot(width = 0.2) +
    scale_fill_brewer(palette='Dark2')
  #+ theme_ipsum()

  if(!facet_flag){
    caption <- build_caption(nsales, max_vf)
    p <- p + geom_jitter(height = 0, width = 0.2, size=.5, alpha=.5, colour="grey")
  }
  else if(facet_flag){
    p <- p + facet_wrap(~Surface.cat)
  }
  #p <- p + facet_wrap(~Code.postal)
  print(p)
  to_png(filename, 40, 30, 360)
  return(p)
}

# Test whether distribution is normal and make QQ plot
normality_test <- function(column, title){
  test <- shapiro.test(column)
  print(test)
  if(test$p.value > 0.05) print("Cannot reject normal distribution")
  else print("Normal distribution rejected")

  qqnorm(column, main = title);
  qqline(column, col = 2)
}