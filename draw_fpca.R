draw_fpca_average_curve <- function(curves, time, filenameId, max, type, fig_path, tone_choice, y_fd, pcafdPar, reg) {
  duration <- as.vector(with(curves, tapply(time,filenameId,max)))
  # get a vector with type for each token
  tune <- as.vector(with(curves, tapply(as.character(type),filenameId,"[",1)))
  
  
  png(paste0(fig_path, tone_choice, "_average_curves.png"),width=16,height=11.4, units="cm", res=200)
  #par(mfrow=c(1,1))
  plot(c(0,mean(duration)),c(3,9),type='n',xlab='Normalised time',ylab='F0 (ERB)',main = '',las=1,cex.axis=1.5,cex.lab=1.5)
  pcafd_Q <- pca.fd(y_fd[tune=="Q"], nharm=3, pcafdPar)
  lines(pcafd_Q$meanfd, col = "#D55E00", lwd =3, lty =1 )
  pcafd_S <- pca.fd(y_fd[tune=="S"], nharm=3, pcafdPar)
  lines(pcafd_S$meanfd, col = "#009E73", lwd =3, lty = 2)
  legend(-0.01,8,c("Q","S"),lty=c(1,2), lwd=c(2.9,2.5), col=c("#D55E00","#009E73"))
  abline(v=reg$land[2], lty = "dotted",lwd = 2)
  dev.off()
}