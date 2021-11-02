pirate_pdf <- function(output_file, input_data, dv, Tune, Tone, title, x_lab, y_lab, y_lim, lab_location, lab_size) {
  library(yarrr)
  pirate_data <- pirateplot(formula = dv ~ Tune + Tone,
                            plot = FALSE,
                            data = input_data,
                            sortx = "alphabetical",
                            decreasing = TRUE)
  cairo_pdf(output_file, width = 10, height = 8)
  pirateplot(formula = dv ~ Tune + Tone,
             data = input_data,
             main = title,
             xlab = x_lab,
             ylab = y_lab,
             ylim = y_lim,
             cex.lab = 1.2,
             cex.axis = 1.2,
             theme = 2,  # Start with theme 2
             pal = 'basel',
             # pal = c('black', "grey35"), # for black and white figures
             inf.f.o = 0, # Turn off inf fill
             inf.b.o = 0, # Turn off inf border
             point.o = .2,   # Turn up points
             bar.f.o = .5, # Turn up bars
             bean.f.o = .4, # Light bean filling
             bean.b.o = .2, # Light bean border
             avg.line.o = 1, # Turn off average line
             point.col = "black") # Black points
  # sortx = "alphabetical",
  # decreasing = FALSE)
  
  
  
  
  text(x = 1, y=pirate_data$summary$avg[1]+ lab_location, labels=paste(format(round(pirate_data$summary$avg[1], 2), nsmall = 2)), cex=lab_size) 
  text(x = 2, y=pirate_data$summary$avg[2]+ lab_location, labels=paste(format(round(pirate_data$summary$avg[2], 2), nsmall = 2)), cex=lab_size) 
  
  text(x = 4, y=pirate_data$summary$avg[3]+ lab_location, labels=paste(format(round(pirate_data$summary$avg[3], 2), nsmall = 2)), cex=lab_size) 
  text(x = 5, y=pirate_data$summary$avg[4]+ lab_location, labels=paste(format(round(pirate_data$summary$avg[4], 2), nsmall = 2)), cex=lab_size) 
  
  text(x = 7, y=pirate_data$summary$avg[5]+ lab_location, labels=paste(format(round(pirate_data$summary$avg[5], 2), nsmall = 2)), cex=lab_size) 
  text(x = 8, y=pirate_data$summary$avg[6]+ lab_location, labels=paste(format(round(pirate_data$summary$avg[6], 2), nsmall = 2)), cex=lab_size) 
  
  text(x = 10, y=pirate_data$summary$avg[7]+ lab_location, labels=paste(format(round(pirate_data$summary$avg[7], 2), nsmall = 2)), cex=lab_size) 
  text(x = 11, y=pirate_data$summary$avg[8]+ lab_location, labels=paste(format(round(pirate_data$summary$avg[8], 2), nsmall = 2)), cex=lab_size) 
  
  dev.off()
}


# function: pirate_png
pirate_png <- function(output_file, input_data, dv, Tune, Tone, title, x_lab, y_lab, y_lim, lab_location, lab_size) {
  
  library(yarrr)
  pirate_data <- pirateplot(formula = dv ~ Tune + Tone,
                            plot = FALSE,
                            data = input_data,
                            sortx = "alphabetical",
                            decreasing = T)
  
  png(output_file, width=20,height=20, units="cm", res=300)
  
  pirateplot(formula = dv ~ Tune + Tone,
             data = input_data,
             main = title,
             xlab = x_lab,
             ylab = y_lab,
             ylim = y_lim,
             cex.lab = 1.5,
             cex.axis = 1.5,
             theme = 2,  # Start with theme 2
             # pal = c('black', "grey35"), # for black and white figures
             pal = 'basel',
             inf.f.o = 0, # Turn off inf fill
             inf.b.o = 0, # Turn off inf border
             point.o = .2,   # Turn up points
             bar.f.o = .5, # Turn up bars
             bean.f.o = .4, # Light bean filling
             bean.b.o = .2, # Light bean border
             avg.line.o = 1, # Turn off average line
             point.col = "black") # Black points
  # sortx = "alphabetical",
  # decreasing = FALSE)
  
  text(x = 1, y=pirate_data$summary$avg[1]+ lab_location, labels=paste(format(round(pirate_data$summary$avg[1], 2), nsmall = 2)), cex=lab_size) 
  text(x = 2, y=pirate_data$summary$avg[2]+ lab_location, labels=paste(format(round(pirate_data$summary$avg[2], 2), nsmall = 2)), cex=lab_size) 
  
  text(x = 4, y=pirate_data$summary$avg[3]+ lab_location, labels=paste(format(round(pirate_data$summary$avg[3], 2), nsmall = 2)), cex=lab_size) 
  text(x = 5, y=pirate_data$summary$avg[4]+ lab_location, labels=paste(format(round(pirate_data$summary$avg[4], 2), nsmall = 2)), cex=lab_size) 
  
  text(x = 7, y=pirate_data$summary$avg[5]+ lab_location, labels=paste(format(round(pirate_data$summary$avg[5], 2), nsmall = 2)), cex=lab_size) 
  text(x = 8, y=pirate_data$summary$avg[6]+ lab_location, labels=paste(format(round(pirate_data$summary$avg[6], 2), nsmall = 2)), cex=lab_size) 
  
  text(x = 10, y=pirate_data$summary$avg[7]+ lab_location, labels=paste(format(round(pirate_data$summary$avg[7], 2), nsmall = 2)), cex=lab_size) 
  text(x = 11, y=pirate_data$summary$avg[8]+ lab_location, labels=paste(format(round(pirate_data$summary$avg[8], 2), nsmall = 2)), cex=lab_size) 
  
  dev.off()
}
