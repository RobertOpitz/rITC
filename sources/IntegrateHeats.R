computeIntegrationOfHeats <- cmpfun(

	f = function( dataSet ){
	
		QiResults <- rep(0, dataSet$nbOfInjections)
	
		for (i in seq(length = dataSet$nbOfInjections) ){
	
			# first point a
			bg.points.time.a <- dataSet$injecTimeMatrix[1, i + 1]
			bg.points.Q.a <- dataSet$bgFitValues[3, i] * bg.points.time.a^2 + dataSet$bgFitValues[2, i] * bg.points.time.a + dataSet$bgFitValues[1, i]
	
			# second point b
			filterPeriod <- dataSet$filterPeriod[i]
			bg.points.time.b <- round(dataSet$injecTimeMatrix[1, i + 1] + filterPeriod * dataSet$injecLength[i + 1] * dataSet$integrationRange[i + 1])
			bg.points.Q.b <- dataSet$bgFitValues[3, i + 1] * bg.points.time.b^2 + dataSet$bgFitValues[2, i + 1] * bg.points.time.b + dataSet$bgFitValues[1, i + 1]
	
			# loop for integration of ith injection
			slope <- (bg.points.Q.b - bg.points.Q.a)/(bg.points.time.b - bg.points.time.a)
			intercept <- bg.points.Q.a - slope * bg.points.time.a
	
			integration.range <- round(dataSet$injecLength[i + 1] * dataSet$integrationRange[i + 1])
			for (j in seq(length = integration.range) ){
				ti <- dataSet$injecTimeMatrix[j + 1, i + 1]
				ti.minus.one <- dataSet$injecTimeMatrix[j, i + 1]
	
				qi <- dataSet$injecQMatrix[j + 1, i + 1] - (slope * ti + intercept)
				qi.minus.one <- dataSet$injecQMatrix[j, i + 1] - (slope * ti.minus.one + intercept)
				QiResults[i] <- QiResults[i] + 0.5 * (ti - ti.minus.one) * (qi.minus.one + qi)
			}
		}
		
		return(4.1868 * QiResults)
	},
	options = list(optimze = 3, supressAll = TRUE)
)