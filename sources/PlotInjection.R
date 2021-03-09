plotInjection <- cmpfun(
	f = function(dataSet){
			
		if ( activeDataSet[tob] > 0 ){
			
			injec.nb <- dataSet$injecNb
			
			min.value <- 0.999 * min( dataSet$injecQMatrix[1, injec.nb], dataSet$injecQMatrix[ dataSet$injecLength[injec.nb], injec.nb] ) + 0.01 * dataSet$viewportRange[injec.nb]
			max.value <- 1.001 * max( dataSet$injecQMatrix[, injec.nb], na.rm=TRUE ) + 0.01 * dataSet$viewportRange[injec.nb]
	
			plot( na.omit( dataSet$injecTimeMatrix[,injec.nb]), na.omit( dataSet$injecQMatrix[,injec.nb] ),
			     type = "l",
			     xlab = "time in sec",
			     ylab = "µcal/sec",
			     main = paste("Input File Name -> ", dataSet$fileNameShort, "\n\n", injec.nb - 1, ". Injection"),
			     ylim = c(min.value, max.value)
			)
			
			if (injec.nb != 1){ # draw blue line for background, red line for integration range, dashed line for separation of both
			
			   # fit background
			   fitBackground(tob, activeDataSet[tob], injec.nb)
			   
			   # get filter period
			   filterPeriod <- dataSet$filterPeriod[injec.nb - 1]
			   
			   # first point a
			   bg.points.time.a <- dataSet$injecTimeMatrix[1, injec.nb]
			   bg.points.Q.a <- dataSet$bgFitValues[3, injec.nb - 1] * bg.points.time.a^2 + dataSet$bgFitValues[2, injec.nb - 1] * bg.points.time.a + dataSet$bgFitValues[1, injec.nb - 1]
			   
			   # compute background line
			   bgLineStartX <- dataSet$injecTimeMatrix[1, injec.nb] + filterPeriod * dataSet$injecLength[injec.nb] * dataSet$integrationRange[injec.nb]
			   bgLineEndX <- dataSet$injecTimeMatrix[ dataSet$injecLength[injec.nb], injec.nb ]
			   if (bgLineStartX >= bgLineEndX) bgLineStartX <- bgLineEndX 
			   bgLineX <- seq( from = bgLineStartX, to = bgLineEndX, by = 1 )
			   bgLineY <- bgLineX^2 * dataSet$bgFitValues[3, injec.nb] + bgLineX * dataSet$bgFitValues[2, injec.nb] + dataSet$bgFitValues[1, injec.nb]
		
			   
			   # estimated background for injection
			   lines( c(bg.points.time.a, bgLineX[1]), c(bg.points.Q.a, bgLineY[1]), col = "red", lwd = 2)
			   
			   # fitted background
			   lines( bgLineX, bgLineY, col="blue", lwd = 2 )
			
			   # integration range
			   abline(v = bgLineStartX, lty = 5)
			   	
			}else{
				# fitted background
			   #abline(a = dataSet$bgFitValues[1,1], b = dataSet$bgFitValues[2,1], col="blue", lwd=2)
			   #x <- dataSet$injecTimeMatrix[ 1:dataSet$injecLength[1], 1]
			   x <- dataSet$injecTimeMatrix[ 1:dataSet$injecLength[injec.nb], injec.nb]
			   y <- x^2 * dataSet$bgFitValues[3,1] + x * dataSet$bgFitValues[2,1] + dataSet$bgFitValues[1,1]
			   lines(x, y, col = "blue", lwd = 2)
			}
		}	
	},
	options = list(optimize = 3, supressAll = TRUE)
)