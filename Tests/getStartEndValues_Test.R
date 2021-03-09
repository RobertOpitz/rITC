
getStartEndValues <- function( input.matrix ){
	startEndValues <- rep( 0, length( input.matrix[,1] ) + 1 )
	print(startEndValues)
	for (i in seq(along = input.matrix[1,]) ){
		print(paste("i=",i))
		startEndValues[i+1] <- length( na.omit( input.matrix[,i] ) ) + startEndValues[i]
	}
	return(startEndValues)
}

test <- matrix(1:(4*3), 4, 3)
test[4,1] <- NA
test[3:4,3] <- NA

print(test)
test2 <- na.omit( as.vector(test) )
print(test2)

getStartEndValues <- getStartEndValues(test)
print( getStartEndValues )

for (i in seq(along = test[1,]) ){
	start <- getStartEndValues[i] + 1
	end <- getStartEndValues[i+1]
	print(paste(start,end))
	print(test2[start:end])
}
	