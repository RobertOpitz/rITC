regressionOnetoOneModel <- cmpfun(

	f = function(input.data){
	
		# maximum number of Iterations for data fit
		max.Iterations <- 1000
		
		# constants
		R <- 8.3144621 # gas constant in [J/(mol*K)]
				
		##===Begin: internal functions=================================================================================
		
		getStartEndValues <- function( input.matrix ){
			startEndValues <- rep( 0, length( input.matrix[1,] ) + 1 )
			for (i in seq(along = input.matrix[1,]) ){
				startEndValues[i+1] <- length( na.omit( input.matrix[,i] ) ) + startEndValues[i]
			}
			return(startEndValues)
		}
		
		#-------------------------------------------------------------------------------------------------------------
		
		# get all the good or bad data, it's your choice
		fetch.data <- function( input.data, Bad.data, yn ){

			# 1.) how man entries with good data
			k <- 0
			for (i in seq(along = input.data) ){
				if ( Bad.data[i] == yn ){ 
					k <- k + 1
				}
			}
		
			# 2.) create vector for good data
			input.data.good <- rep(NA,k)
		
			# 3.) fill DQ.good with good data
			k <- 0
			for (i in seq(along = input.data) ){
				if ( Bad.data[i] == yn ){ 
					k <- k + 1
					input.data.good[k] <- input.data[i]
				}
			}
			
			# 4.) return good data
			return(input.data.good)
		}
		
		#-------------------------------------------------------------------------------------------------------------	
			
		# theorectical curve for P + L <=> PL
		itc.theo.curve <- function(P0, L0, dVi, V0, Kd, DH, Q.dil){
			
			Q.theo <- rep( NA, length(P0) )
				
			temp <- P0 + L0 + Kd
			temp2 <- pmax( temp^2 - 4.0 * P0 * L0, 0.0 )
			PL <- 0.5 * ( temp - sqrt( temp2 ) )
									
			for (i in seq(along = P0)){
				# compare: ITC Data Analysis in Origin -- Tutorial Guide version 7.0 - January 2004; MircoCal
				if (i == 1){
					Q.theo[1] <- DH * ( PL[1] * ( V0 + 0.5 * dVi[1] ) + Q.dil * dVi[1] )
				}else{
					Q.theo[i] <- DH * ( ( PL[i] * ( V0 + 0.5 * dVi[i] ) - PL[(i-1)] * ( V0 - 0.5*dVi[i] ) ) + Q.dil * dVi[i] )
				}
			}
					
			return(Q.theo)
		}
		
		#-------------------------------------------------------------------------------------------------------------	
			
		# model function for P + L <=> PL for non-linear fit
		itc.curve <- function(P0, L0, dVi, Bad.data, V0, Kd, DH, N, Q.dil, NorM, startEndValues, m, nb.ds){
			
			Q.theo <- rep( NA, length(P0) )
						
			for (j in seq(length = nb.ds) ){
				
				start <- startEndValues[j] + 1
				end <- startEndValues[j+1]
				
				#===Begin: compute PL===
				P0.temp <- P0[start:end]
				L0.temp <- L0[start:end]
				
				if (NorM == "N"){
					# N*P0
					P0.temp <- N * P0.temp
				}else if(NorM == "M"){
					# M*L0
					L0.temp <- N * L0.temp
				}
	
				Q.theo[start:end] <- itc.theo.curve( P0.temp, L0.temp, dVi[start:end], V0[j], Kd, DH, Q.dil )

			}
			
			# reduce data set
			Q.theo <- fetch.data( Q.theo, Bad.data, FALSE )
		
			return( Q.theo )
		}
		
		#-------------------------------------------------------------------------------------------------------------

		excludeOutliers <- function( residuals, degreeOfFreedom, BadData ){
						
			# t*3.09024; i.e. 99.9 %
			# t*3.719; i.e. 99.99 %
			qtmax <- qt( 0.9999, df = (degreeOfFreedom - 1) )

			j <- 0
			outliersDetected <- FALSE
			for ( i in seq(along = BadData) ){
				if (BadData[i] == FALSE){
					j <- j + 1
					if ( abs( ( mean(residuals[-j]) - residuals[j] ) / sd(residuals[-j]) ) >= qtmax ){
						#print(paste(j, abs( ( mean(residuals[-j]) - residuals[j] ) / sd(residuals[-j]) ), qtmax ))
						BadData[i] <- TRUE
						outliersDetected <- TRUE
					}
				}
	
			}
			
			return( list(newBadData = BadData, outliersDetected = outliersDetected) )
		}
		
		#-------------------------------------------------------------------------------------------------------------
					
		computeStandarizedResiduals <- function( residuals, molarRatio, degreeOfFreedom, BadData ){
			
			x <- matrix(NA, nrow = length(molarRatio[,1]), ncol = length(molarRatio[1,]) )
			y <- x
			a <- sqrt( (length(residuals) - 2.0 )/degreeOfFreedom )
			
			temp <- residuals/(a * sd(residuals))
						
			k <- 0
			h <- 0
			for ( j in seq(along=x[1,]) ){
				for (i in seq(along=x[,1]) ){
					k <- k + 1
					if ( BadData[k] == FALSE ){
						h <- h + 1
						y[i,j] <- temp[h]
						x[i,j] <- molarRatio[i,j]
					}
				}
			}
				
			return( list( molarRatio = x, residuals = y ) )
		}
		
		#-------------------------------------------------------------------------------------------------------------
				
		# compute free energy in kJ/mol
		comp.DG <- function(Kd, Kd.sd, Kd.CI, Temperature){
			R <- 8.3144621 # gas constant in [J/(mol*K)]
			RT <- R * (Temperature + 273.15)
			M <- 1000000.0 # 1 M in [µM]
			DG <- RT*log(Kd/M)
			DG.sd <- RT*Kd.sd/Kd
			DG.CI <- c( RT*log(Kd.CI[1]/M), RT*log(Kd.CI[2]/M) )
	
			return(list(DG.est = DG, DG.sd = DG.sd, DG.CI = DG.CI))
		}
		
		#-------------------------------------------------------------------------------------------------------------	
			
		computeLackOfFit <- function( MR, Qexp, Qtheo, Bad, numberOfEstimatedParamter, tol = 0.001 ){
			
			m <- NULL
			#===Begin: create input matrix======================
			for (i in seq(along=MR[1,])){
				m <- rbind( m, cbind( MR[ ,i], Qexp[ ,i], Qtheo[ ,i] ) )
			}
			m <- cbind( m, Bad )
			
			m <- m[ order( m[ ,1] ), ]
			#===End: create input matrix========================
			
			#===Begin: grouping=================================
			newValueFound <- FALSE
			i <- 0
			j <- 0
			maxi <- length(m[ ,1])
			QExpGroup <- list()
			tempExp <- NULL
			QTheoGroup <- list()
			tempTheo <- NULL
			repeat{
					
				i <- i + 1
				
				if ( newValueFound == FALSE ){
					if ( m[i,4] == FALSE ){
						newValueFound <- TRUE
						newValue <- as.numeric( m[i,1] )
						tempExp <- c(tempExp, as.numeric( m[i,2] ) )
						tempTheo <- c(tempTheo, as.numeric( m[i,3] ) )
					}
				}else{
					if ( abs( newValue - as.numeric( m[i,1] ) ) < tol ){
						if ( m[i,4] == FALSE ){
							tempExp <- c(tempExp, as.numeric( m[i,2] ) )
							tempTheo <- c(tempTheo, as.numeric( m[i,3] ) )
						}
					}else{
						j <- j + 1
						QExpGroup[[j]] <- tempExp
						QTheoGroup[[j]] <- tempTheo
												
						tempExp <- NULL
						tempTheo <- NULL
						
						newValueFound <- FALSE
						i <- i - 1
					}
				}
				
				if (i >= maxi){
					j <- j + 1
					QExpGroup[[j]] <- tempExp
					QTheoGroup[[j]] <- tempTheo
					break
				}
			}
			#===End: grouping=================================
			
			#===Begin: compute Lack-of-Fit====================
			numberOfAveragedDataPoints <- length(QExpGroup)
			numberOfTotalDataPoints <- length(m[,1])
			
			DegreeOfFreedom <- numberOfAveragedDataPoints - numberOfEstimatedParamter
			NumberOfUngroupedDataPoints <- numberOfTotalDataPoints - numberOfAveragedDataPoints
			
			if ( DegreeOfFreedom <= 0 ){
				return( list( 	FValue = NA, 
								PValue = NA, 
								everythingOK = FALSE, 
								lyrics = paste("Degree of Freedom is to small :", DegreeOfFreedom) 
							) 
						)
			}
			
			if ( NumberOfUngroupedDataPoints <= 0 ){
				return( list( 	FValue = NA, 
								PValue = NA, 
								everythingOK = FALSE, 
								lyrics = paste("No grouped data points :", NumberOfUngroupedDataPoints) 
							) 
						)
			}
			
			lackOfFitSumOfSquares <- 0.0
			pureErrorSumOfSquares <- 0.0
			for ( i in seq(along = QExpGroup) ){
				QgroupMean <- mean( QExpGroup[[i]] )
				lackOfFitSumOfSquares <- lackOfFitSumOfSquares + length( QExpGroup[[i]] ) * ( QgroupMean - mean( QTheoGroup[[i]] ) )^2
				
				pureErrorSumOfSquares_temp <- 0.0
				for ( j in seq(along=QExpGroup[[i]]) ){
					pureErrorSumOfSquares_temp <- pureErrorSumOfSquares_temp + ( QExpGroup[[i]][j] - QgroupMean )^2
				}
				pureErrorSumOfSquares <- pureErrorSumOfSquares + pureErrorSumOfSquares_temp
			}
			
			lackOfFitSumOfSquares <- lackOfFitSumOfSquares / DegreeOfFreedom
			pureErrorSumOfSquares <- pureErrorSumOfSquares / NumberOfUngroupedDataPoints
			
			FValue <- lackOfFitSumOfSquares / pureErrorSumOfSquares
			PValue <- pf( FValue, DegreeOfFreedom, NumberOfUngroupedDataPoints, lower.tail=FALSE )
			#===End: compute Lack-of-Fit======================
			
			return( list( 	FValue = as.numeric(FValue), 
							PValue = as.numeric(PValue), 
							everythingOK = TRUE, 
							lyrics = paste("No errors.")
						) 
					)
		}
		##===End: internal functions========================================================================================
		
		##===Begin: initialize=====================================================================================
		
	    Pmax <- input.data$Pmax  # µM
		Lmax <- input.data$Lmax  # µM
		V0 <- input.data$V0      # µL
		NorM <- input.data$NorM  #
		
		Q.matrix <- input.data$Q.matrix
		dVi.matrix <- input.data$dVi.matrix
				
		# create data matrices and data vectors
		m <- length(Q.matrix[,1])
		nb.ds <- length(Q.matrix[1,])
		
		P0.matrix <- matrix(NA, nrow = m, ncol = nb.ds)
		L0.matrix <- matrix(NA, nrow = m, ncol = nb.ds)
		MR.matrix <- matrix(NA, nrow = m, ncol = nb.ds)
		
		# fill data fields
		for (i in seq(length = nb.ds) ){ 					
			# compute P0 and L0 according to MicroCal	
			# compare: ITC Data Analysis in Origin -- Tutorial Guide version 7.0 - January 2004; MircoCal
		    DV <- 0.0
			for (j in seq(length = length( na.omit( Q.matrix[ ,i] ) )) ){
				DV <- DV + dVi.matrix[j,i]
				temp <- 0.5 * DV/V0[i]
				P0 <- Pmax[i] * (1.0 - temp)/(1.0 + temp)
				L0 <- Lmax[i] * (DV/V0[i]) * (1.0 - temp)
				P0.matrix[j,i] <- P0
				L0.matrix[j,i] <- L0
				MR.matrix[j,i] <- L0/P0
			}
		}
			
		##===End: initialize=======================================================================================
		
		##===Begin: data fit=======================================================================================

		P0.vector <- na.omit( as.vector(P0.matrix) )
		L0.vector <- na.omit( as.vector(L0.matrix) )
		dVi.vector <- na.omit( as.vector(dVi.matrix) )
		
		startEndValues <- getStartEndValues( Q.matrix )
		
		Q.vector <- na.omit( as.vector(Q.matrix) )
		Q.good.temp <- Q.vector
		Bad.data <- rep( FALSE, length(Q.vector) )
		
		# starting values
		Kd.start <- 0.0001
		Q.dil.start <- 0.0
		N.start <- 1.0
		
		# do the data fit
		repeat{
			if (NorM != "0"){
				print("Model mit Conc.Corr.")
				itc.fit <- try( nls(Q.good.temp ~ itc.curve(P0.vector, L0.vector, dVi.vector, Bad.data, V0, Kd, 1.0, N, Q.dil, NorM, startEndValues, m, nb.ds), 
									trace = TRUE, 
									start = list( Kd = Kd.start, Q.dil = Q.dil.start, N = N.start ),
									algorithm = "plinear",
									control = list(maxiter = max.Iterations)
							       )
						  	  )
			}else{
				itc.fit <- try( nls(Q.good.temp ~ itc.curve(P0.vector, L0.vector, dVi.vector, Bad.data, V0, Kd, 1.0 ,0.0, Q.dil, NorM, startEndValues, m, nb.ds), 
									trace = TRUE, 
									start = list(Kd = Kd.start, Q.dil = Q.dil.start),
									algorithm = "plinear",
									control = list(maxiter = max.Iterations) 
							       )
						   	  )
			}
			
			if ( is.character(itc.fit) ){
				cat( "Data fit FAILED! Check model, starting values and data." )
				return( "Data fit FAILED! Check model, starting values and data." )
			}else{
				excludeOutliersOutput <- excludeOutliers( residuals(itc.fit), df.residual(itc.fit), Bad.data )
				if ( excludeOutliersOutput$outliersDetected == TRUE ){
					Bad.data <- excludeOutliersOutput$newBadData
					Q.good.temp <- fetch.data( Q.vector, Bad.data, FALSE )
				}else{
					break
				}
			}
		}
		##===End: data fit=========================================================================================
		
		##===Begin: get everything=================================================================================
			
		itc.fit.summary <- summary(itc.fit)
		
		Kd.est <- itc.fit.summary$coef[1,1]
		Q.dil.est <- itc.fit.summary$coef[2,1] 
		if (NorM != "0"){
			N.est <- itc.fit.summary$coef[3,1]
			DH.est <- itc.fit.summary$coef[4,1]
		}else{
			DH.est <- itc.fit.summary$coef[3,1]
		}
		
		#test <- update(itc.fit, algorithm = "default", start = list(Kd = Kd.est, DH = DH.est, Q.dil = Q.dil.est, N = N.est) )
		#print(test)
		
		# refit with estimated values for Kd, DH, Q.dil (and N or M) with algorithm "default" to get the right itc.fit-object for the confidence intervalls
		# of DH; with the object of the "plinear" fit, DH is not avaiable for function "confint"
		if (NorM != "0"){		
			itc.fit <- try( nls(Q.good.temp ~ itc.curve(P0.vector, L0.vector, dVi.vector, Bad.data, V0, Kd, DH, N, Q.dil, NorM, startEndValues, m, nb.ds), 
								trace = FALSE, 
								start = list(Kd = Kd.est, DH = DH.est, Q.dil = Q.dil.est, N = N.est),
								algorithm = "default",
								control = list(maxiter=max.Iterations)
							   )
					      )
		}else{
			itc.fit <- try( nls(Q.good.temp ~ itc.curve(P0.vector, L0.vector, dVi.vector, Bad.data, V0, Kd, DH, 1.0, Q.dil, NorM, startEndValues, m, nb.ds), 
								trace = FALSE, 
								start = list(Kd = Kd.est, DH = DH.est, Q.dil = Q.dil.est),
								algorithm = "default",
								control = list(maxiter=max.Iterations)
							   )
					      )
		}
		
		# check if Q.dil is neglebile
		# QDilP <- itc.fit.summary$coef[3,4]
		# if (QDilP >= 0.1) {
		#   if (NorM != "0"){
		#      update( itc.fit, Q.good.temp ~ itc.curve(P0.vector, L0.vector, dVi.vector, Bad.data, V0, Kd, DH, N, 0.0, NorM, m, nb.ds),
		#      start = list(Kd = Kd.est, DH = DH.est, N = N.est) )
		#	}else{
		#	
		#   }	
		#}
		
		if (is.character(itc.fit)){
			cat( "Data fit FAILED! Check model, starting values and data." )
			return( "Data fit FAILED! Check model, starting values and data." )
		}
		
		itc.fit.summary <- summary(itc.fit)
		itc.fit.sandwich.summary <- coeftest( itc.fit, vcov = sandwich )
			
		Kd.est <- itc.fit.summary$coef[1,1]
		Kd.sd <- itc.fit.sandwich.summary[1,2]
		DH.est <- itc.fit.summary$coef[2,1]
		DH.sd <- itc.fit.sandwich.summary[2,2]
		Q.dil.est <- itc.fit.summary$coef[3,1] 
		if (NorM != "0"){
			N.est <- itc.fit.summary$coef[4,1]
			N.sd <- itc.fit.sandwich.summary[4,2]
		}
	
		# 95 % confidence intervall for Kd, DH and N or M
		Kd.CI95 <- tryCatch( confint(itc.fit, parm = "Kd", level = 0.95), error = function(e) c(NA, NA) )
		DH.CI95 <- tryCatch( confint(itc.fit, parm = "DH", level = 0.95), error = function(e) c(NA, NA) )
		if (NorM != "0"){ 
		  N.CI95 <- tryCatch( confint(itc.fit, parm = "N", level = 0.95), error = function(e) c(NA, NA) )
		}else{
		  N.CI95 <- c(NA, NA)
		}
				
		# compute DG.est, SE and CI for DG
		DG <- comp.DG( Kd.est, Kd.sd, c( Kd.CI95[1], Kd.CI95[2] ), input.data$temperature )
		
		results_vcov <- sandwich(itc.fit)
		
		# compute -T*DS
		TDS <- DG$DG.est / 1000.0 - 1000.0 * DH.est
		# compute SE for -T*DS
		KdDHsd <- 1000000.0 * results_vcov[1,2]
		TDS.sd <- sqrt( DG$DG.sd^2 + (1000000.0 * DH.sd)^2 - 2.0 * R * (input.data$temperature + 273.15) * KdDHsd/Kd.est )
	
		# compute c-value and DH at a molar ratio of zero
		if (NorM == "N"){
			c.value <- N.est * min(Pmax) / Kd.est
			DH.at0 <- 1000.0 * ( DH.est * ( N.est * Pmax / (N.est * Pmax + Kd.est) + Q.dil.est / Lmax ) )
		}else if(NorM == "M"){
			c.value <- min(Pmax) / Kd.est
			DH.at0 <- 1000.0 * ( DH.est * ( Pmax/(Pmax + Kd.est) + Q.dil.est / (N.est * Lmax) ) )
		}else{
			c.value <- min(Pmax)/Kd.est
			DH.at0 <- 1000.0 * ( DH.est * ( Pmax/(Pmax + Kd.est) + Q.dil.est / Lmax ) )
		}
		
		# compute Heats per mol Injectant
		Q.theo <- matrix(NA, nrow = m, ncol = nb.ds)
		for (i in seq(length = nb.ds) ){ 
			if ( NorM == "N"){
			   	Q.theo[ ,i] <- itc.theo.curve( N.est * P0.matrix[,i], L0.matrix[,i], dVi.matrix[,i], V0[i], Kd.est, DH.est, Q.dil.est )
			   	Q.theo[ ,i] <- 1000.0 * Q.theo[ ,i] / ( dVi.matrix[ ,i] * Lmax[i] )
			   	Q.matrix[1:m,i] <- 1000.0 * Q.matrix[1:m,i] / ( dVi.matrix[1:m,i] * Lmax[i] )	   	
			}else if ( NorM == "M" ){
			   	Q.theo[ ,i] <- itc.theo.curve( P0.matrix[,i], N.est * L0.matrix[,i], dVi.matrix[,i], V0[i], Kd.est, DH.est, Q.dil.est )
				Q.theo[ ,i] <- 1000.0 * Q.theo[ ,i] / ( dVi.matrix[ ,i] * N.est * Lmax[i] )
				Q.matrix[1:m,i] <- 1000.0 * Q.matrix[1:m,i] / ( dVi.matrix[1:m,i] * N.est * Lmax[i] )
			}else{
			   Q.theo[1:m,i] <- itc.theo.curve( P0.matrix[,i], L0.matrix[,i], dVi.matrix[,i], V0[i], Kd.est, DH.est, Q.dil.est )
			   Q.theo[ ,i] <- 1000.0 * Q.theo[,i] / ( dVi.matrix[1:m,1] * Lmax[i] )
			   Q.matrix[1:m,i] <- 1000.0 * Q.matrix[1:m,i] / ( dVi.matrix[1:m,i] * Lmax[i] )
			}	
		}
		
		#===Begin: compute lack-of-fit===============================================================================
		if ( nb.ds > 1 ){
			
			if (NorM != "0"){ 
				NumberOfEstimatedParameter <- 4
			}else{
				NumberOfEstimatedParameter <- 3
			}
			lackOfFit <- computeLackOfFit( MR.matrix, Q.matrix, Q.theo, Bad.data, NumberOfEstimatedParameter )

			if ( lackOfFit$everythingOK == FALSE ) print( lackOfFit$lyrics )
			
			FValue <- lackOfFit$FValue
			PValue <- lackOfFit$PValue
			
		}else{
			FValue <- NA
			PValue <- NA
		}
		#===end: compute lack-of-fit================================================================================
		
		# compute plot of theo. itc curve
		#curve.plot <- plot.itc.curve( MR.matrix, dVi.matrix, V0, Pmax, Lmax, Kd.est, DH.est, N.est, NorM, Q.dil.est )
		#print(curve.plot)
		
		# compute standardized residuals
		standardizedResiduals <- computeStandarizedResiduals( residuals(itc.fit), MR.matrix, df.residual(itc.fit), Bad.data )
		
		# create bad DQ.good
		#Q.bad <- fetch.data( Q.vector, Bad.data, TRUE )
		Q.bad <- fetch.data( na.omit( as.vector( Q.matrix ) ), Bad.data, TRUE )
		MR.bad <- fetch.data( na.omit( as.vector(MR.matrix) ), Bad.data, TRUE )
		#print(cbind(MR.bad,Q.bad))
		
		output <- list( summary = itc.fit.sandwich.summary, 
						Kd.CI95 = Kd.CI95, DH.CI95 = DH.CI95, N.CI95 = N.CI95,  
						MR.matrix = MR.matrix, 
						Q.norm.matrix = Q.matrix, 
						DH.at0 = DH.at0,
						MR.bad = MR.bad, 
						Q.bad = Q.bad, 
						curve = Q.theo, 
						DG = DG$DG.est/1000.0, DG.sd = DG$DG.sd/1000.0, DG.CI = DG$DG.CI/1000.0, 
						TDS = TDS, TDS.sd = TDS.sd/1000, 
						c.value = c.value, 
						#fittedCurve = curve.plot,
						lackOfFit = list(FValue = FValue, PValue = PValue), 
						residuals = list( x = standardizedResiduals$molarRatio, y = standardizedResiduals$residuals, DoF = df.residual(itc.fit) ) 
					  )
				
		return(output)
	},
	options = list(optimize = 3, supressAll = TRUE)
)