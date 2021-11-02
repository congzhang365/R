report_lmer <- function(model) {
  df <- model[1:6]
  df <- round(df, 2)
  
  result.df <- data.frame(effect = character(), result=character())
  for (i in 1:length(df$`Pr(>F)`)){
    if (df$`Pr(>F)`[i] < 0.001 ){
      # df <- format(round(df, 2), nsamll =2)
      effect <- rownames(df)[i]
      result <- paste0("χ²(", df$NumDF[i], ") = ", df$`F value`[i], ", p < 0.001 ", "***")
      result.print <- paste(effect, result, sep = " ")
      print(result.print)
      
      result.line <- cbind(effect, result)
      result.df <- rbind(result.df, result.line)
    }
    else if (df$`Pr(>F)`[i] <= 0.01 & df$`Pr(>F)`[i] > 0.001) {
      effect <- rownames(df)[i]
      result <- paste0("χ²(", df$NumDF[i],") = ", df$`F value`[i], ", p = ", df$`Pr(>F)`[i], " **" ) 
      result.print <- paste(effect, result, sep = " ")
      print(result.print)
      
      result.line <- cbind(effect, result)
      result.df <- rbind(result.df, result.line)      
    }
    else if (df$`Pr(>F)`[i] <= 0.05 & df$`Pr(>F)`[i] > 0.01) {
      effect <- rownames(df)[i]
      result <- paste0("χ²(", df$NumDF[i],") = ", df$`F value`[i], ", p = ", df$`Pr(>F)`[i], " *" ) 
      result.print <- paste(effect, result, sep = " ")
      print(result.print)
      
      result.line <- cbind(effect, result)
      result.df <- rbind(result.df, result.line)      
    }
    else if (df$`Pr(>F)`[i] > 0.05) {
      effect <- rownames(df)[i]
      result <- paste0("χ²(", df$NumDF[i],") = ", df$`F value`[i], ", p = ", df$`Pr(>F)`[i], ", n.s." ) 
      result.print <- paste(effect, result, sep = " ")
      print(result.print)
      
      result.line <- cbind(effect, result)
      result.df <- rbind(result.df, result.line)      
    }
  }
  return(result.df)
  
}
