equivalentPotentialTemperature <-
function(P,Temp,w,TLCL,consts=export_constants()){
	theta=PT2Theta(P,Temp,w,consts)
	kd=0.2854
	thetaDL=theta*((theta/TLCL)^(0.28*w)*(1+w/consts["epsilon"])^kd)
	L0star=2.56313e6
	L1star=1754.
	Lstar=L0star+L1star*(TLCL-consts["T0"])
	cpd=1005.7
	k2=1.137e6
	thetaE=thetaDL*exp((Lstar+k2*w)*w/(cpd*TLCL))
	return(as.double(thetaE))
}
