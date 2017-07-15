stuve_diagram <-
function(Pres,Temp,TempD=NA,XLIM=c(-80,45),YLIM=c(1050,100)){
  
  lines=export_lines()
  #data(adiabat_x_T,adiabat_y_T,adiabat_z_T,theta_x_T,theta_y_T,wsat_x_T,wsat_y_T,wsat_z_T)
  plot(lines[["adiabat_x_T"]][1,],lines[["adiabat_y_T"]][1,],
       type="l",xlim=XLIM,ylim=YLIM,log="y",col="blue",
       xlab="\u00B0C",ylab="hPa",lty=2,axes=FALSE,xaxs = "i",yaxs = "i")
  
  
  #pseudoadiabatic lines
  for(i in 2:dim(lines[["adiabat_x_T"]])[1]){
    lines(lines[["adiabat_x_T"]][i,],lines[["adiabat_y_T"]][i,],col="blue",lty=2)
  }
  
  #theta lines (dry adiabats)
  for(i in 1:dim(lines[["theta_x_T"]])[1]){
    lines(lines[["theta_x_T"]][i,],lines[["theta_y_T"]][i,],col="green")
  }
  
  #wsat lines (constant mixing ratio)
  for(i in 1:dim(lines[["wsat_x_T"]])[1]){
    lines(lines[["wsat_x_T"]][i,],lines[["wsat_x_T"]][i,],col=rgb(238/255,119/255,0/255),lty=2)
  }
  
  #x axis
  axis(1)
  grid()
  
  # labels in the y axis
  Levels_y<-c(1000,925,850,700,500,400,300,250,200,150,100)
  NN<-which(Levels_y<=YLIM[1] & Levels_y>=YLIM[2])
  Levels_y<-Levels_y[NN]
  axis(2,at=Levels_y,labels=FALSE)
  text(y = Levels_y, par("usr")[1], labels =Levels_y, 
       srt =0, pos = 2, xpd = TRUE)
  abline(h=Levels_y,col=rgb(209/255,209/255,209/255),lty=3)
  
  #labels for wsat lines
  #text(x=c(-20),y=90,labels="g/kg",
  #     pos=NULL,srt=0, adj=1, xpd=TRUE,col=rgb(238/255,119/255,0/255),cex=1)
  mtext("g/kg     ",line=1,side=3,col=rgb(238/255,119/255,0/255),cex=1)
  N<-which(lines[["wsat_y_T"]][1,]==YLIM[2])
  NN<-which(lines[["wsat_x_T"]][,N]>=(XLIM[1]+5) & lines[["wsat_x_T"]][,N]<=(XLIM[2]-0.01))
  text(x=lines[["wsat_x_T"]][NN,N]-0.01, 
       y=lines[["wsat_y_T"]][NN,N]+5,
       labels=lines[["wsat_z_T"]][NN], 
       pos=NULL,srt=0, adj=1, xpd=TRUE,col=rgb(238/255,119/255,0/255),cex=0.75)
  
  #labels for pseudoadiaabtic lines
  #text(x=c(-10),y=90,labels="C",
  #     pos=NULL,srt=0, adj=1, xpd=TRUE,col="blue",cex=1.5)
  mtext("          \u00B0C",col="blue",cex=1,line=1)
  N<-which(lines[["adiabat_y_T"]][1,]==YLIM[2])
  NN<-which(lines[["adiabat_x_T"]][,N]>=(XLIM[1]+5) & lines[["adiabat_x_T"]][,N]<=(XLIM[2]-0.01))
  text(x=lines[["adiabat_x_T"]][NN,N]+1.5, 
       y=lines[["adiabat_y_T"]][NN,N]+12,
       labels=lines[["adiabat_z_T"]][NN], 
       pos=NULL,srt=0, adj=1, xpd=TRUE,col="blue",cex=0.75)
  
  #sounding
  lines(Temp,Pres,lwd=2)
  #dew temperature
  if(is.na(TempD[1])==FALSE){
    lines(TempD,Pres,lwd=2,col="red")
  }
  box()
  
  #record the plot in an object
  p <- recordPlot()
  return(p)
}
