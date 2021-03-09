plotRegressionOutput <- cmpfun(

	f = function(output, devSetNumber = 3){
	
			dev.set(devSetNumber)
			
			def.par <- par(no.readonly = TRUE)

			layout( matrix(c(1, 2)), heights = c(3, 2) )
				
			#===Begin: plot titration data===
			# set general plot symbols
			plotColor <- rep( "black", length(output$MR.matrix[1,]) )
			plotSymbol <- rep(1, length(output$MR.matrix[1,]) )
			plotType <- rep("p", length(output$MR.matrix[1,]) )

			# set plot symbols for active plot
			if ( output$activeTitrationPlot != "showAll" ){
				plotColor[as.numeric(output$activeTitrationPlot)] <- "red"
				plotSymbol[as.numeric(output$activeTitrationPlot)] <- 16
				plotType[as.numeric(output$activeTitrationPlot)] <- "b"
			}
			
			# get min and max value for ylim
			minValue <- min( output$Q.norm.matrix, output$Q.theo, output$DH.at0, output$QBackground, output$QRawTitration, na.rm=TRUE )
			maxValue <- max( output$Q.norm.matrix, output$Q.theo, output$DH.at0, output$QBackground, output$QRawTitration, na.rm=TRUE )
			#print( paste( "min, max", minValue, maxValue) )
			#print(output$QBackground)
			
			# plot
			plot(	output$MR.matrix[,1], output$Q.norm.matrix[,1],
	    			xlab = "Molar Ratio",
	 				ylab = "Q (kJ/mol per injectant)",
	 				xlim = c( 0.0, max(output$MR.matrix, na.rm = TRUE) ),
	 				ylim = c( minValue, maxValue ),
	 				col = plotColor[1],
	 				pch = plotSymbol[1]
	 		)
	 			 		
	 		# plot more data, if necessary
	 		if ( length(dataSets[[1]]) > 1 ){ 
	    		for ( i in 2:length(dataSets[[1]]) ){
	    			# plot more data
					points(output$MR.matrix[ ,i], output$Q.norm.matrix[ ,i], col = plotColor[i], pch = plotSymbol[i] )
	    		}
	    		#lines( c(0.0, output$MR.matrix[ ,1]), c(output$DH.at0[1], output$curve[,1]), col="red")
	 		#}else{
	 			#lines( c(0.0, output$MR.matrix), c(output$DH.at0, output$curve), col="red")
	 		}
	 		# plot bad data
	 		points( output$MR.bad, output$Q.bad, col = "black", pch = 4 )
	 		
	 		# plot theo. curve
	 		lines( c(0.0, output$MR.matrix[ ,1]), c(output$DH.at0[1], output$curve[,1]), col="red")
	 		#===End: plot titration data===
	 		
	 		#===Begin: plot background data===
	 		if ( length( dataSets[[2]] ) > 0 ){
	 			
	 			# set plot symbols
	 			plotColorBackground <- rep( "black", length( dataSets[[2]] ) )
				plotSymbolBackground <- rep( 2, length( dataSets[[2]] ) )
	 			
	 			# set plot symbols for active plot
	 			if (  output$activeBackgroundPlot != "showAll" ){
					plotColorBackground[as.numeric(output$activeBackgroundPlot)] <- "red"
					plotSymbolBackground[as.numeric(output$activeBackgroundPlot)] <- 17
				}
					 			
	 			# plot points 		
	 			for (i in seq( along = dataSets[[2]] ) ){
					points( output$molarRatioBackground[,i], output$QBackground[,i], pch = plotSymbolBackground[i], col = plotColorBackground[i] )
				}
				
				# plot line
				lines( output$molarRatioBackground[,1], output$QBackgroundAverage, col = "green" )
				
	 		}
	 		#===End: plot background data===
	 		
	 		#===Begin: plot Raw Heats per Injectant for Titration===
	 		# würg-a-raund
	 		if ( invalid(output$subtractBackground) ){
	 			subtractBackground <- FALSE
	 		}else{
	 			subtractBackground <- output$subtractBackground
	 		}
	 		
			if ( subtractBackground == TRUE &  length( dataSets[[2]] ) > 0 ){
				for ( i in seq(along=dataSets[[1]] ) ){
					points( output$MR.matrix[ ,i], output$QRawTitration[ ,i], pch = 6 )
				}
				lines( output$MR.matrix[ ,1], output$QRawTitrationAverage, col = "blue" )
			}
			#===End: plot Raw Heats per Injectant for Titration===
	 			 		
	 		#===Begin: plot standardized residuals===
	 		tValue <- qt(0.975, output$residuals$DoF)
	 		plot( na.omit(output$residuals$x[,1]), na.omit(output$residuals$y[,1]),
	 			  ylim = c( min(na.omit(as.vector(output$residuals$y)), -tValue), max(na.omit(as.vector(output$residuals$y)), tValue)  ),
	 			  xlim = c( 0.0, max( na.omit(as.vector(output$residuals$x)) ) ),
	 			  xlab = "Molar Ratio",
	 			  ylab = "standardized residuals e_i / s",
	 			  col  = plotColor[1],
	 			  pch = plotSymbol[1],
	 			  type = plotType[1] 
	 			)
	 		
	 		if ( length(dataSets[[1]]) > 1 ){ 
	    		for (i in 2:length(dataSets[[1]]) ){
	    			points(	na.omit(output$residuals$x[ ,i]), na.omit(output$residuals$y[,i]), 
	    					col = plotColor[i], 
	    					pch = plotSymbol[i],
	    					type = plotType[i]
	    				  )
	    		}
	    	}
	 		
	 		abline(h = 0.0)
	 		abline(h = tValue, lty = "dashed")
	 		abline(h = -tValue, lty = "dashed")
	 		#===End: plot standardized residuals===
	 		
	 		par(def.par)
	},
	option = list(optimize = 3, supressAll = TRUE)
)