load.mgh <-
function(input.file) {
	
	to.read <- file(input.file,"rb")

	v <- readBin(to.read,integer(), endian = "big")
	ndim1 <- readBin(to.read,integer(), endian = "big")
	ndim2 <- readBin(to.read,integer(), endian = "big") 
	ndim3 <- readBin(to.read,integer(), endian = "big") 
	nframes <- readBin(to.read,integer(), endian = "big")
	type <- readBin(to.read,integer(), endian = "big") 
	dof <- readBin(to.read,integer(), endian = "big")
	
	close(to.read)

	to.read <- file(input.file,"rb")
	dump <- readBin(to.read,double(),size = 4,n = 71, endian = "big")
	x <- readBin(to.read,double(),size = 4, n = ndim1*ndim2*nframes, endian = "big")
	close(to.read)
	
	list(x = x,v = v,ndim1 = ndim1,ndim2 = ndim2,ndim3 = ndim3,nframes = nframes,type = type,dof = dof)

}
