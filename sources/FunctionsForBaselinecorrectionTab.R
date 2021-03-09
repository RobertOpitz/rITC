# functions for baseline correction tab
#======================================================================================================

setIntegrationRange <- function(range){
	if ( length( dataSets[[tob]] ) > 0 ){
	  dataSetNb <- activeDataSet[tob]
	  dataSets[[tob]][[dataSetNb]]$integrationRange[ dataSets[[tob]][[dataSetNb]]$injecNb ] <<- range$getValue()
	  plotInjection( dataSets[[tob]][[dataSetNb]] )
	}
}

#======================================================================================================

setViewportRange <- function(range){
	if ( length( dataSets[[tob]] ) > 0 ){
	  dataSetNb <- activeDataSet[tob]
	  dataSets[[tob]][[dataSetNb]]$viewportRange[ dataSets[[tob]][[dataSetNb]]$injecNb ] <<- range$getValue()
	  plotInjection( dataSets[[tob]][[dataSetNb]] )
	}
}

#======================================================================================================

nextInjection <- function(widget){
	if ( length( dataSets[[tob]] ) > 0 ){
   	   
   	   data.set.nb <- activeDataSet[tob] 
   	   
	   if ( dataSets[[tob]][[data.set.nb]]$injecNb < dataSets[[tob]][[data.set.nb]]$maxInjecNb){
	     dataSets[[tob]][[data.set.nb]]$injecNb <<- dataSets[[tob]][[data.set.nb]]$injecNb + 1
	   }else{
	     dataSets[[tob]][[data.set.nb]]$injecNb <<- dataSets[[tob]][[data.set.nb]]$maxInjecNb
	   }
	
	   injec.nb <- dataSets[[tob]][[data.set.nb]]$injecNb
	
	   # Schieber Wert voreinstellen
	   sliderIntegrationRange$setValue( dataSets[[tob]][[data.set.nb]]$integrationRange[ injec.nb ] )

	   # Port View Schieber Wert voreinstellen
	   sliderViewportRange$setValue( dataSets[[tob]][[data.set.nb]]$viewportRange[ injec.nb ] )
	   
	   # plot next injection  
	   plotInjection( dataSets[[tob]][[data.set.nb]] )
   }
}

#======================================================================================================

previuosInjection <- function(widget){
	if ( length( dataSets[[tob]] ) > 0 ){
		
   	   data.set.nb <- activeDataSet[tob]
	   
	   if (dataSets[[tob]][[data.set.nb]]$injecNb > 1){
	     dataSets[[tob]][[data.set.nb]]$injecNb <<- dataSets[[tob]][[data.set.nb]]$injecNb - 1
	   }else{
	     dataSets[[tob]][[data.set.nb]]$injecNb <<- 1
	   }
	   
	   injec.nb <- dataSets[[tob]][[data.set.nb]]$injecNb
	   
	   # Schieber Wert voreinstellen
	   sliderIntegrationRange$setValue( dataSets[[tob]][[data.set.nb]]$integrationRange[ injec.nb ] )

	   # Port View Schieber Wert voreinstellen
	   sliderViewportRange$setValue( dataSets[[tob]][[data.set.nb]]$viewportRange[ injec.nb ] )
	   
	   # plot previous injection
	   plotInjection( dataSets[[tob]][[data.set.nb]] )
   }
}

#======================================================================================================

showThermogram <- function(widget){
	if ( length( dataSets[[tob]] ) > 0 ){

		dev.set(2)
		dataSet <- dataSets[[tob]][[ activeDataSet[tob] ]]
		
		plot(	na.omit( as.vector(dataSet$injecTimeMatrix ) ), na.omit( as.vector(dataSet$injecQMatrix) ),
		        type = "l",
				xlim = c( 0, max( dataSet$injecTimeMatrix, na.rm = TRUE ) ),
				ylim = c( min( dataSet$injecQMatrix, na.rm = TRUE ), max( dataSet$injecQMatrix, na.rm = TRUE ) ),
				xlab = "time in sec",
				ylab = "µcal/sec",
				#fin = c(10,5),
				main = paste("Input File Name -> ", dataSet$fileNameShort, "\n\n Thermogram")
		)
		
		for (i in 2:(length( dataSet$injecLength ))){

			# first point a
		    bg.points.time.a <- dataSet$injecTimeMatrix[1,i]
		    bg.points.Q.a <- dataSet$bgFitValues[3,i-1] * bg.points.time.a^2 + dataSet$bgFitValues[2,i-1] * bg.points.time.a + dataSet$bgFitValues[1,i-1]
			    
		    # second point b
		    #filterPeriod <- dataSet$filterPeriod[i-1]
		    bg.points.time.b <- round( dataSet$injecTimeMatrix[1,i] + dataSet$filterPeriod[i-1] * dataSet$injecLength[i] * dataSet$integrationRange[i] )
		    bg.points.Q.b <- dataSet$bgFitValues[3,i] * bg.points.time.b^2 + dataSet$bgFitValues[2,i] * bg.points.time.b + dataSet$bgFitValues[1,i]
		    
		    lines( c(bg.points.time.a, bg.points.time.b), c(bg.points.Q.a, bg.points.Q.b), col="red" )
		}
	}
}

#======================================================================================================

showTemperatureCourse <- function(widget){
	if ( length( dataSets[[tob]] ) > 0 ){
		
		dataSet <- dataSets[[tob]][[ activeDataSet[tob] ]]
		
		injec.time.Matrix.temp <- as.vector( dataSet$injecTimeMatrix )
		temperature.course.temp <- as.vector( dataSet$temperatureCourse )
								
		plot(	na.omit(as.vector(injec.time.Matrix.temp)),na.omit(as.vector(temperature.course.temp)),
		        type = "l",
				xlab = "time in sec",
				ylab = "cell temperature in °C",
				main = paste("Input File Name -> ", dataSet$fileNameShort, "\n\n Temperature course; mean temperature -> ", round( dataSet$meanTemperature, 2 ), "°C")
		)
		abline(h = dataSet$meanTemperature, lty=2)
	}
}

#======================================================================================================

showIntegratedHeats <- function(widget){
	
	if ( length( dataSets[[tob]] ) > 0) {
			    
	    dataSets[[tob]][[ activeDataSet[tob] ]]$integratedHeats <<- computeIntegrationOfHeats( dataSets[[tob]][[ activeDataSet[tob] ]] )
	    
	    plot( 1:dataSets[[tob]][[ activeDataSet[tob] ]]$nbOfInjections, dataSets[[tob]][[ activeDataSet[tob] ]]$integratedHeats, 
	          xlab = "injection number", 
	          ylab = "µJ",
	          main = paste("Input File Name -> ", dataSets[[tob]][[ activeDataSet[tob] ]]$fileNameShort, "\n\n integrated Heats"),
	          type = "p",
	          pch = 19
	        )
   }
}

#======================================================================================================
