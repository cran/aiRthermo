moistCv <-
function( w , consts=export_constants()){
		   return(as.double(consts["cv"]*(1+0.97*w2q(w))))
}
