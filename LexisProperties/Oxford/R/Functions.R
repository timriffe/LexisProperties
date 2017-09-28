#get packages sorted out
if( !"colorspace" %in% rownames(installed.packages())){
	install.packages("colorspace")
}
# this is installed for certain grayscale tools in it.
# too bad we end up with this huge package just for that,
# but really the functions are convenient.
if( !"spatstat" %in% rownames(installed.packages())){
	install.packages("spatstat")
}

library(colorspace)
library(spatstat)


AssignColour <- function (x) {
	if (x == "A") result <- "#D23737"
	if (x == "P") result <- "#3191C9"
	if (x == "C") result <- "#D2BC2D"
	if (x == "T") result <- "#4EC93B"
	if (x == "D") result <- "#881F93"
	if (x == "L") result <- "#C5752B"
	return(result)
}
# darken by changing luminance
darkenhex <- function(hexcols,fac=.3){
	# from @Roland:
	#https://stackoverflow.com/questions/30219738/is-there-a-way-to-programmatically-darken-the-color-given-rgb-values
	require(colorspace)
	zz   <- textConnection(hexcols)
	cols <- readhex(file = zz,	class = "RGB")
	close(zz)
# transform to hue/lightness/saturation colorspace
	cols <- as(cols, "HLS")
	cols@coords[, "L"] <- pmax(0, cols@coords[, "L"] - fac)
	cols <- as(cols, "RGB")
	hex(cols)
}
# a gives a number from 0 to 1. 0 is black, 1 is white. vectorized
howdark <- function(col){
	colorspace::hex2RGB(spatstat::to.grey(col))@coords[, 1]
}
# darkens a color to a specific grayscale target darkness. not vectorized
darken.to <- function(hexcol,target=.2){
	require(colortools)
	require(spatstat)
	
	# a function to optimize the ideal darkening factor
	mintarg <- function(fac,col,target){
		hexcol <- darkenhex(hexcol,fac)
		abs(target - howdark(hexcol))
	}
	# get darkening factor
	fac    <- optimize(mintarg,c(-.8,.8),col=hexcol,target=target)$minimum
	# now darken by that much...
	colout <- darkenhex(hexcol,fac)
	colout
}

# ---------------------

