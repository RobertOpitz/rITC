plotFinalFigure <- function( dataSets ){

	computeAllPureQ <- function( dataSet ){
	
		# Zeit für den Hintergrund des Delays
		time.background <- dataSet$injecTimeMatrix[ 1:dataSet$injecLength[1], 1 ]
		#print("0. Injection")
		#print(time.background)
		
		# theoretische Werte für den Hintergrund
		Q.background <- time.background^2 * dataSet$bgFitValues[3,1] + time.background * dataSet$bgFitValues[2,1] + dataSet$bgFitValues[1,1]
		
		# Speichere den Endpunkt der theo Baseline im Wert c
		c <- c( time.background[length(time.background)], Q.background[length(Q.background)] )
		
		# Ziehe die Baseline von der gemessenen Baseline ab
		pureQ <- dataSet$injecQMatrix[ 1:dataSet$injecLength[1], 1 ] - Q.background
		
		# Speichere das Zeit in einem Vektor
		pureQ.time <- time.background
				
		for ( i in 2:dataSet$maxInjecNb ){
		
			# Ermittle die Zeitpunkte xt für den Hintergrund der iten Injektion
			start <- round( dataSet$injecLength[i] * dataSet$integrationRange[i] )
			end <- dataSet$injecLength[i]
			time.background <- dataSet$injecTimeMatrix[ start:end, i ]
		
			# Berechne mit xt und den Koeffizienten der iten Injektion den
		 	# theoretischen Hintergrund Qtheo
			Q.background <- time.background^2 * dataSet$bgFitValues[3, i] + time.background * dataSet$bgFitValues[2, i] + dataSet$bgFitValues[ 1, i ]
		
			# Übertrage den letzten Datenpunkt von Qtheo auf a
			a <- c( time.background[length(time.background)], Q.background[length(Q.background)] )
		
			# Übertrage den ersten Datenpunkt von Qtheo nach b
			b <- c( time.background[1], Q.background[1] )
		
			# Berechne mit b und c den Anstieg m und Schnittpunkt mit der
			# y-Achse der Geraden, die als Baseline für den Reaktionspeak 
			# dient.
			slope <- (b[2] - c[2]) / (b[1] - c[1])
			intercept <- b[2] - slope * b[1]
		
			# Ziehe die Gerade vom Reaktionspeakbereich ab (innerhalb der 
			# Zeitgrenzen die die Punkte b und c liefern).
			start <- start - 1
			time.integration <- dataSet$injecTimeMatrix[ 1:start, i ]
			#print(paste(i, "injection, integrationsbereich"))
			#print(time.integration)
			Q.integration <- slope * time.integration + intercept
			pureQ <- c( pureQ, dataSet$injecQMatrix[ 1:start, i ] - Q.integration )
			pureQ.time <- c( pureQ.time, time.integration )
		
		
			# Ziehe den theoretischen Hintergrund Qtheo vom gemessenen
		 	# Hintergund Q für die Zeitpunkte xt ab und speichere das Ergebnis in einem Vektor.
		 	start <- start + 1
			pureQ <- c( pureQ, dataSet$injecQMatrix[ start:end, i ] - Q.background )
			pureQ.time <- c( pureQ.time, time.background )
			#print(paste(i, "injection, Hintergrund"))
			#print(time.background)
					
			# Ersetze den Wert von c mit dem Wert von a.
			c <- a	
		}
		
		return( cbind( pureQ.time, pureQ ) )
	
	}
	
	# 
	minValue <- 0
	maxValue <- 0
	
	# Berechne bereinigtes Thermogramm für Titration
	Qresult.titration <- list()
	for ( i in seq(along = dataSets[[1]]) ){
		
		# compute thermogram without baseline
		Qresult.titration[[i]] <- computeAllPureQ( dataSets[[1]][[i]] )
		
		# find minimum
		temp <- min( Qresult.titration[[i]][ ,2], na.rm=TRUE )
		if (temp < minValue) minValue <- temp
		
		# find maximum
		temp <- max( Qresult.titration[[i]][ ,2], na.rm=TRUE )
		if (temp > maxValue) maxValue <- temp
	}
	
	# Berechne bereinigtes Thermogramm für den Hintergrund
	Qresult.background <- list()
	for ( i in seq(along = dataSets[[2]]) ){
		
		# compute thermogram without baseline
		Qresult.background[[i]] <- computeAllPureQ( dataSets[[2]][[i]] )
		
		# find minimum
		temp <- min( Qresult.background[[i]][ ,2], na.rm=TRUE )
		if (temp < minValue) minValue <- temp
		
		# find maximum
		temp <- max( Qresult.background[[i]][ ,2], na.rm=TRUE )
		if (temp > maxValue) maxValue <- temp
	}
	
	# plotte bereinigtee Thermogramme
	for ( i in seq(along = Qresult.titration) ){
		plot( 	Qresult.titration[[i]][ ,1], Qresult.titration[[i]][ ,2], 
				type = "l",
				main = "Titration", 
				xlab = "Time (sec)", ylab = "q (µcal/sec)",
				ylim = c( minValue, maxValue )
			)
	}
	
	for ( i in seq(along = Qresult.background) ){
		plot( 	Qresult.background[[i]][ ,1], Qresult.background[[i]][ ,2], 
				type = "l", 
				main = "Background",
				xlab = "Time (sec)", ylab = "q (µcal/sec)",
				ylim = c( minValue, maxValue )
			)
	}	

}