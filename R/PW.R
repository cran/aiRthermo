PW <-
function(w,PRES,Psurf,consts=export_constants()){
  Q=w2q(w)
  if(PRES[1]<PRES[2]){
      PRES<-PRES[length(PRES):1]
      w<-w[length(w):1]
  }
  dP<-rep(NA,length(Q))
  #First level, from MSL to midpoint between two lowest leveles
  dP[1]<-Psurf-(PRES[1]+PRES[2])/2
  #Middle levels
  for(i in 2:(length(Q)-1)){
    dP[i]<-(PRES[i-1]-PRES[i+1])/2
  }
  #Last level
  dP[length(Q)]<-PRES[length(Q)-1]-PRES[length(Q)]
  PW<-sum(Q*dP/consts["g"],na.rm=TRUE)
  return(as.double(PW))
}
