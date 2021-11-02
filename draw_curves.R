source("C:/Users/sprin/Documents/R/scripts/load_data.R")


draw_average_curves <- function(data) {

  data <- load_data(data)
  
  data <- data %>% filter(!is.na(f0))

  data$norm.time <- (data$rounded_time/max(data$rounded_time-min(data$rounded_time)))*max(data$rounded_time)

  length(levels(data$filename))

  
  # f0 filenames  
  p.f0 <- ggplot()+
    geom_smooth(data=data,aes(x=norm.time,y=f0,color=tone, linetype = type),size=1.2,method = "loess") +
    scale_color_brewer(palette="Set1") + xlab("Normalised Time") + ylab("F0 (Hz)")+
    scale_x_continuous(labels = scales::percent)+ 
    scale_linetype_manual(values = c('solid','dotted'))+
    expand_limits(y=c(100,200),na.rm="TRUE") +
    theme_pubr()+
    theme(legend.position = 'bottom')+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    scale_fill_grey()+
    geom_smooth(data=s.data,aes(x=norm.time,y=f0,color=tone, linetype = type), size=1.2,method = "loess") + facet_wrap(.~tone, ncol=2)
  
  
  # f0.erb filenames
  p.f0.erb <- ggplot()+
    geom_smooth(data=data,aes(x=norm.time,y=f0.erb,color=tone, linetype = type), size=1.2,method = "loess") +
    scale_color_brewer(palette="Set1") + xlab("Normalised Time") + ylab("F0 (ERB)")+
    scale_x_continuous(labels = scales::percent)+ 
    scale_linetype_manual(values = c('solid','dotted'))+
    expand_limits(y=c(4,8),na.rm="TRUE") +
    theme_pubr()+
    theme(legend.position = 'bottom')+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    scale_fill_grey()+
    geom_smooth(data=s.data,aes(x=norm.time,y=f0.erb,color=tone, linetype = type), size=1.2,method = "loess") + 
    facet_wrap(tone~., ncol=2)
  return (p.f0, p.f0.erb)
}



draw_compare_interpolation <- function(data) {
  
  data <- load_data(data)
  
  for (c in levels(data$filename)){
    data.utterance <- subset(data, data$filename== c)
    data.utterance$f0_interpolate <- na_interpolation(data.utterance$f0, 
                                                      "stine")
    
    p.original <- ggplot() +
      geom_point(data=data.utterance, aes(time, f0), color='black', size = 0.5) +
      # stat_smooth(data=data.utterance, aes(time, f0),method = lm, formula = y ~ splines::bs(x, df =18), color='purple', size = 0.5) +
      theme_minimal() +
      ggtitle("original")
    ##
    
    
    p.interpolate <- ggplot() +
      geom_point(data=data.utterance, aes(time, f0), color='grey', size = 0.5) +
      geom_point(data=data.utterance, aes(time, f0_interpolate), color='black', size = 0.5) +
      geom_point(data=subset(data.utterance, is.na(data.utterance$f0)), 
                 aes(time, f0_interpolate), color='red', size = 0.5) + 
      theme_minimal()+
      ggtitle("stine interpolation")
    
    plot.comparison <- ggarrange(p.original, p.interpolate, nrow=2, ncol=1, align = c("h"))
    plot.comparison

  }
  # 
  return (plot.comparison)
}



draw_compare_interpolation_2 <- function(data1, data2, method1 = "method1", method2 = "method2") {
  
  data1 <- load_data(data1)
  data2 <- load_data(data2)
  
  for (c in levels(data1$filename)){
    data1.utterance <- subset(data1, data$filename== c)
    data.utterance$f0_interpolate <- na_interpolation(data.utterance$f0, 
                                                      "stine")
    
    p.original <- ggplot() +
      geom_point(data=data1.utterance, aes(time, f0), color='black', size = 0.5) +
      # stat_smooth(data=data.utterance, aes(time, f0),method = lm, formula = y ~ splines::bs(x, df =18), color='purple', size = 0.5) +
      theme_minimal() +
      ggtitle(method1)
    ##
    
    data2.utterance <- subset(data2, data2$filename== c)
    p.interpolate <- ggplot() +
      geom_point(data=data2.utterance, aes(time, f0), color='grey', size = 0.5) +
      # geom_point(data=data2.utterance, aes(time, f0_interpolate), color='black', size = 0.5) +
      # geom_point(data=subset(data2.utterance, is.na(data.utterance$f0)), 
      #            aes(time, f0_interpolate), color='red', size = 0.5) + 
      theme_minimal()+
      ggtitle(method2)
    
    plot.comparison <- ggarrange(p.original, p.interpolate, nrow=2, ncol=1, align = c("h"))
    plot.comparison
    
  }
  # 
  return (plot.comparison)
}

