# functions for competition regression
#=======================================================================================================================================	

# expectation function for reaction: P + A <=> PA; P + B <=> PB
# according to Wang, FEBS Letters, 360, 1995, 111 -- 114
itc.curve <- cmpfun(

	f = function( P0, A0, B0, dVi, Bad.data, V0, Pmax, Bmax, Kd.PA, Kd.PB, DH.PA, DH.PB, N, Q.dil, NorM, m, nb.ds, fetch.data ){
	
		Q.theo <- rep(NA,(m*nb.ds))
			
		for (j in seq(length = nb.ds) ){
				
			start = (m*(j-1)+1)
			end = m*j

			P0.temp <- c( Pmax[j], P0[start:end] )
			A0.temp <- c( 0.0, A0[start:end] )
			B0.temp <- c( Bmax, B0[start:end] )
			dVi.temp <- dVi[start:end]
				
			if ( identical(NorM, "N") ){
				# N*P0
				P0.temp <- N * P0.temp
			}else if( identical(NorM, "M") ){
				# M*A0
				A0.temp <- N * A0.temp
			}
	
			a <- Kd.PA + Kd.PB + A0.temp + B0.temp - P0.temp
			b <- Kd.PB * (A0.temp - P0.temp) + Kd.PA * (B0.temp - P0.temp) + Kd.PA * Kd.PB
			c <- (-1.0) * Kd.PA * Kd.PB * P0.temp
						
			theta <- acos( (-2.0 * a^3 + 9.0 * a * b - 27.0 * c)/(2.0 * sqrt( ( a^2 - 3.0 * b )^3) ) ) / 3.0
					
			d <- 2.0 * sqrt( a^2 - 3.0 * b ) * cos( theta ) - a
	
			PA <- A0.temp * d / ( 3.0 * Kd.PA + d )
		
			PB <- B0.temp * d / ( 3.0 * Kd.PB + d )
						
			#===End: compute PL and PB===	
			# compare: ITC Data Analysis in Origin -- Tutorial Guide version 7.0 - January 2004; MircoCal
			for (i in 1:m){
				Q.theo[(start + i - 1)] <- ( DH.PA * PA[i+1] + DH.PB * PB[i+1] ) * ( V0[j] + 0.5 * dVi.temp[i] ) - ( DH.PA * PA[i] + DH.PB * PB[i] ) * ( V0[j] - 0.5 * dVi.temp[i]) + Q.dil * dVi.temp[i]
			}
		}
		
		# reduce data set
		Q.theo.good <- fetch.data( Q.theo, Bad.data, FALSE )
	
		return(Q.theo)
		
	},
	options = list(optimze = 3, supressAll = TRUE)
)

#=======================================================================================================================================	

compITCDisplacements <- cmpfun(
	
	f = function(input.data){

       # constants
		max.Iterations <- 500
		R <- 8.314462 # gas constant
				
		#===Begin: internal functions===================================================================================
				
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

		# expectation function for apparent reaction: P + A <=> PA
		itc.curve.init <- function(P0, A0, dVi, Bad.data, V0, Kd.app, DH.app, N, Q.dil, NorM, m, nb.ds){
			
			Q.theo <- rep(NA,(m*nb.ds))
			
			for (j in seq(length = nb.ds) ){
				
				start <- (m*(j-1)+1)
				end <- m*j
				
				#===Begin: compute PL===
				P0.temp <- P0[start:end]
				A0.temp <- A0[start:end]
				
				if (NorM == "N"){
					# N*P0
					P0.temp <- N * P0.temp
				}else if(NorM == "M"){
					# M*L0
					A0.temp <- N * A0.temp
				}
		
				temp <- P0.temp + A0.temp + Kd.app
				temp2 <- pmax( temp^2 - 4.0 * P0.temp * A0.temp, 0.0 )
				PL <- 0.5 * ( temp - sqrt( temp2 ) )
				#===End: compute PL===
					
				# compare: ITC Data Analysis in Origin -- Tutorial Guide version 7.0 - January 2004; MircoCal
				Q.theo[start] <- DH.app * ( PL[1] * ( V0[j] + 0.5 * dVi[start] ) + Q.dil * dVi[start] )
				
				for (i in 2:m){
					# compare: ITC Data Analysis in Origin -- Tutorial Guide version 7.0 - January 2004; MircoCal
					Q.theo[(start+i-1)] <- DH.app * ( ( PL[i] * (V0[j] + 0.5 * dVi[(start+i-1)] ) - PL[i-1] * ( V0[j] - 0.5*dVi[(start+i-1)]) ) + Q.dil * dVi[(start+i-1)] )
				}
			}
			
			# reduce data set
		  Q.theo <- fetch.data( Q.theo, Bad.data, FALSE )
			
			return(Q.theo)
		}
		
	   #-------------------------------------------------------------------------------------------------------------

		comp.new.Kd <- function(n, Kd, Kd.sd, temperature){
			
			RT <- 8.314462 * (temperature + 273.15) # gas constant times absolute temperature
			
			DG <- RT * log( Kd / 1000000.0 )
			DG.sd <- RT * Kd.sd/Kd
			
			DG.random <- rnorm( n, mean = DG, sd = DG.sd )
			
			Kd.random <- 1000000.0 * exp( DG.random / RT )
			
			return(Kd.random)
		}
		
       #-------------------------------------------------------------------------------------------------------------
				
		comp.DG <- function(Kd, temperature){
			
			RT <- 8.314462 * (temperature + 273.15) # gas constant times absolute temperature
			
			DG <- RT * log( Kd / 1000000.0 )
			
			return(DG)
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
			
			return( list( newBadData = BadData, outliersDetected = outliersDetected ) )
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
		
		#===End: internal functions=======================================================================================
		
		#===Begin: initialize=============================================================================================
		
		Pmax <- input.data$Pmax # µM
		Amax <- input.data$Amax # µM
		Bmax <- input.data$Bmax # µM
		V0 <- input.data$V0     # µL
		NorM <- input.data$NorM #
		temperature <- input.data$temperature # °C
		
		Kd.PA <- input.data$Kd.PA # µM
		Kd.PA.SD <- input.data$Kd.PA.SD # µM
		DH.PA <- input.data$DH.PA / 1000.0 # kJ/mol
		DH.PA.SD <- input.data$DH.PA.SD / 1000.0 # kJ / mol
			
		Q.matrix <- input.data$Q.matrix 
		dVi.matrix <- input.data$dVi.matrix 
		
		m <- length( Q.matrix[,1] )
		nb.ds <- length( Q.matrix[1,] )
						
		P0.matrix <- matrix(NA, m, nb.ds)
		A0.matrix <- matrix(NA, m, nb.ds)
		B0.matrix <- matrix(NA, m, nb.ds)
		MR.matrix <- matrix(NA, m, nb.ds)
							
		# compute P0 and L0 according to MicroCal	
		# compare: ITC Data Analysis in Origin -- Tutorial Guide version 7.0 - January 2004; MircoCal
		for (i in seq(length = nb.ds)){
			DV <- 0.0
			for (j in seq(length = m) ){
				DV <- DV + dVi.matrix[j,i]
				temp <- 0.5*DV/V0[i]
				P0 <- Pmax[i] * (1.0 - temp)/(1.0 + temp)
				B0 <- Bmax * (1.0 - temp)/(1.0 + temp)
				A0 <- Amax[i] * (DV/V0[i])*(1.0 - temp)
				P0.matrix[j,i] <- P0
				A0.matrix[j,i] <- A0
				B0.matrix[j,i] <- B0
				MR.matrix[j,i] <- A0/P0
			}
		}
		
		#===End: initialize=============================================================================================
		
		#===Begin: data fit=============================================================================================

		P0.vector <- as.vector(P0.matrix) 
		A0.vector <- as.vector(A0.matrix) 
		B0.vector <- as.vector(B0.matrix) 
		dVi.vector <- as.vector(dVi.matrix)
		MR.vector <- as.vector(MR.matrix) 

		Q.vector <- as.vector(Q.matrix)
		Q.good.temp <- Q.vector
		Bad.data <- rep( FALSE, length(Q.vector) )

       # do the data fit
       repeat{
			if (NorM != "0"){
				itc.fit.init <- try( nls(Q.good.temp ~ itc.curve.init(P0.vector, A0.vector, dVi.vector, Bad.data, V0, Kd.app, 1.0, N, Q.dil, NorM, m, nb.ds),
												trace = TRUE,
												start = list( Kd.app = Kd.PA, Q.dil = 0.0, N = 1.0 ),
												algorithm = "plinear",
												control = list(maxiter = max.Iterations)
									) 
								)
								
				if ( is.character( itc.fit.init ) ){
				   cat( "Data fit FAILED! Check model, starting values and data." )
				 return( "Data fit FAILED! Check model, starting values and data." )	
				}
										 
				Kd.app <- as.numeric( coef(itc.fit.init)[1] )
				Q.dil <- as.numeric( coef(itc.fit.init)[2] )
				N.start <- as.numeric( coef(itc.fit.init)[3] )
				DH.app <- as.numeric( coef(itc.fit.init)[4] )
				
				Kd.PB.start <- median( B0.vector * ( Kd.PA / ( Kd.app - Kd.PA ) ) )
				DH.PB.start <- median( ( DH.PA - DH.app ) * ( (Kd.PB.start + B0.vector) / B0.vector ) )
				Q.dil.start <- DH.app * Q.dil
							
				#print( itc.curve(P0.vector, A0.vector, B0.vector, dVi.vector, Bad.data, V0, Pmax, Bmax, Kd.PA, Kd.PB.start, DH.PA, DH.PB.start, N.start, Q.dil.start, NorM, m, nb.ds, fetch.data) )
							
				itc.fit <- try( nlsLM(Q.good.temp ~ itc.curve(P0.vector, A0.vector, B0.vector, dVi.vector, Bad.data, V0, Pmax, Bmax, Kd.PA, Kd.PB, DH.PA, DH.PB, N, Q.dil, NorM, m, nb.ds, fetch.data), 
									trace = TRUE, 
									#start = list( Kd.PB = Kd.PB.start, DH.PB = DH.PB.start, Q.dil = Q.dil.start, N = N.start ),
									start = list( Kd.PB = 0.6, DH.PB = -64/1000, Q.dil = 0.0, N = N.start ),
									lower = c(0.0, -Inf, -Inf, 0.0),
									control = list(maxiter = max.Iterations)
							       )
						  	  )
			}else{
				
				itc.fit.init <- try( nls(Q.good.temp ~	itc.curve.init(P0.vector, A0.vector, dVi.vector, Bad.data, V0, Kd.app, 1.0, 1.0, Q.dil, "0", m, nb.ds),
												trace = TRUE,
												start = list( Kd.app = Kd.PA, Q.dil = 0.0 ),
												algorithm = "plinear",
												control = list(maxiter = max.Iterations)
									) 
								)
								
				if ( is.character( itc.fit.init ) ){
				   cat( "Data fit FAILED! Check model, starting values and data." )
				 return( "Data fit FAILED! Check model, starting values and data." )	
				}
										
				Kd.app <- as.numeric( coef(itc.fit.init)[1] )
				Q.dil <- as.numeric( coef(itc.fit.init)[2] )
				DH.app <- as.numeric( coef(itc.fit.init)[3] )
				
				Kd.PB.start <- median( B0.vector * ( Kd.PA / ( Kd.app - Kd.PA ) ) )
				DH.PB.start <- median( ( DH.PA - DH.app ) * ( (Kd.PB.start + B0.vector) / B0.vector ) )
				Q.dil.start <- DH.app * Q.dil
							
				itc.fit <- try( nlsLM(Q.good.temp ~ itc.curve(P0.vector, A0.vector, B0.vector, dVi.vector, Bad.data, V0, Pmax, Bmax,
														Kd.PA, Kd.PB, DH.PA, DH.PB, 1.0, Q.dil, "0", m, nb.ds, fetch.data), 
								trace = TRUE, 
								#start = list( Kd.PB = 0.6, DH.PB = DH.PB.start, Q.dil = Q.dil.start ),
								start = list( Kd.PB = Kd.PB.start, DH.PB = DH.PB.start, Q.dil = Q.dil.start ),
								lower = c(0.0, -Inf, -Inf),
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
		#===End: data fit===========================================================================================================
		
		itc.fit.summary <- summary(itc.fit)
		itc.fit.sandwich.summary <- coeftest( itc.fit, vcov = sandwich )	  
		
		results_vcov <- sandwich(itc.fit)
		
		Kd.PB.est <- itc.fit.summary$coefficients[1,1]
		print(paste("Kd.PB.est :", Kd.PB.est))
		DH.PB.est <- itc.fit.summary$coefficients[2,1]
		Q.dil.est <- itc.fit.summary$coefficients[3,1]
		if (NorM != "0") {
			N.est <- itc.fit.summary$coefficients[4,1] 
		}else{
			N.est <- NA
		}
		
		Kd.PB.SD <- sqrt( results_vcov[1,1] )
		DH.PB.SD <- sqrt( results_vcov[2,2] )
		
		DG.PB.est <- comp.DG(Kd.PB.est, temperature)
		DG.PB.SD <- R*(temperature + 273.15)*Kd.PB.SD / Kd.PB.est
		TDS.PB.est <- DG.PB.est/1000.0 - 1000.0 * DH.PB.est
		
		Kd.DH.PB.SD <- 1000000.0 * results_vcov[1,2]
		TDS.PB.SD <- sqrt( DG.PB.SD^2 + (1000000.0 * DH.PB.SD)^2 - 2.0 * R * (temperature + 273.15) * Kd.DH.PB.SD/Kd.PB.est )
		
		#===End: data fit==================================================================================
		
		#===Begin: estimated curve of Q==============
		
		# compute c-value and DH at a molar ratio of zero
		# if (NorM == "N"){
			# #c.value = N.est*Pmax/Kd.est
			# #c.value.sd = 0.0 #sqrt( (N.est.sd*Pmax/Kd.est)^2 + (Kd.sd*Nest*Pmax/(Kd.est)^2)^2 - 2.0*() )
			# #DH.at0 = 1000.0 * ( DH.est*N.est*Pmax/(N.est*Pmax+Kd.est) + Q.dil.est/Lmax)
			# DH.at0 = 1000.0 * ( (DH.PA + DH.PB.est) * ( N.est * Pmax/(N.est * Pmax + Kd.est) + Q.dil.est/Lmax ) )
		# }else if(NorM == "M"){
			# #c.value = Pmax/Kd.est
			# #c.value.sd = Kd.sd*Pmax/(Kd.est)^2
			# DH.at0 = 1000.0 * ( (DH.PA + DH.PB.est) * ( Pmax/( Pmax + Kd.est) + Q.dil.est/(N.est*Lmax) ) )
		# }else{
			# #c.value = Pmax/Kd.est
			# #c.value.sd = Kd.sd*Pmax/(Kd.est)^2
			# #DH.at0 = 1000.0 * ( DH.est*Pmax/(Pmax+Kd.est) + Q.dil.est/Lmax )
			# DH.at0 = 1000.0 * ( (DH.PA + DH.PB.est) * ( Pmax/(Pmax + Kd.est) + Q.dil.est/Lmax ) )
		# }
		
		#plot_all <- rep("n",length(Bad.data[1:m]))
		Q.theo <- matrix(NA, nrow = m, ncol = nb.ds)
		for (i in seq(length = nb.ds) ){ 
			pseudo_Bad.data <- rep( FALSE, length(P0.matrix[,i]) )
			if ( NorM != "0" ){
			   Q.theo[,i] <- itc.curve(P0.matrix[,i], A0.matrix[,i], B0.matrix[,i], dVi.matrix[,i], pseudo_Bad.data, V0, Pmax, Bmax, Kd.PA, Kd.PB.est, 
			   								   DH.PA, DH.PB.est, N.est, Q.dil.est, NorM, m, 1, fetch.data)
			   #print(Q.theo)
			   if ( NorM == "N"){
			   		Q.theo[,i] = 1000.0 * Q.theo[,i]/(dVi.matrix[1:m,i] * Amax)
			   		Q.matrix[1:m,i] = 1000.0 * Q.matrix[1:m,i]/(dVi.matrix[1:m,i] * Amax)	   	
			   }else{
					Q.theo[,i] = 1000.0 * Q.theo[,i]/(dVi.matrix[1:m,i] * N.est * Amax)
				    Q.matrix[1:m,i] = 1000.0 * Q.matrix[1:m,i]/(dVi.matrix[1:m,i] * N.est * Amax)
			   }
			}else{
			   Q.theo[1:m,i] <- itc.curve(P0.matrix[,i], A0.matrix[,i], B0.matrix[,i], dVi.matrix[,i], pseudo_Bad.data, V0, Pmax, Bmax, Kd.PA, Kd.PB.est, 
			   								   DH.PA, DH.PB.est, 1.0, Q.dil.est, NorM, m, 1, fetch.data)
			   Q.theo[,i] = 1000.0 * Q.theo[,i]/(dVi.matrix[1:m,1] * Amax)
			   Q.matrix[1:m,i] = 1000.0 * Q.matrix[1:m,i]/(dVi.matrix[1:m,i] * Amax)
			}	
		}
		#===End: estimated curve of Q=======================================================================
		
		#===Begin: compute lack-of-fit======================================================================
		if ( nb.ds > 1 ){
			Q.mean = rep(0.0,m)
			ni.vector = rep(0.0,m)
		
			# compute mean and number of each replicate
			for (i in seq(length = nb.ds) ){ 
				for (j in seq(length = m) ){ 
					if ( Bad.data[j+(i-1)*m] == FALSE ){
						ni.vector[j] = ni.vector[j] + 1
						Q.mean[j] = Q.mean[j] + Q.matrix[j,i]	
					}
				}	
			}
			# average
			for (j in seq(length = m) ){ 
				if (ni.vector[j] != 0){
					Q.mean[j] = Q.mean[j]/ni.vector[j]
				}
			}	
			N.big = sum(ni.vector,na.rm=TRUE)
		
			n.small = 0
			for (j in seq(length = m) ){ 
				if (ni.vector[j] != 0){
					n.small = n.small + 1	
				}
			}
		
			lack.of.fit.ss = sum( ni.vector*(Q.mean - Q.theo[,1])^2, na.rm=TRUE )
		
			pure.error.sum.ss = 0.0
			for (i in seq(length = nb.ds) ){
				for (j in seq(length = m) ){
					if ( Bad.data[j+(i-1)*m] == FALSE ){
						pure.error.sum.ss = pure.error.sum.ss + (Q.matrix[j,i] - Q.mean[j])^2
					}
				}
			}
			#print(pure.error.sum.ss)
			if (NorM != "0"){ 
				df1 = n.small - 4
			}else{
				df1 = n.small - 3
			}
			df2 = N.big - n.small
			FValue = ( lack.of.fit.ss/df1 )/( pure.error.sum.ss/df2 )
			PValue = pf( FValue, df1, df2, lower.tail=FALSE )
			#print(paste( "F = ", round(FValue,3), "; P = ", round(PValue,3) ))
		}else{
			FValue <- NA
			PValue <- NA
		}
		#===end: compute lack-of-fit============================================================================
		
		#===Begin: extended SD==================================================================================
		
		# maxIter <- 25
		Kd.PB.dist <- 0.0 #rep(NA,maxIter)
		DH.PB.dist <- 0.0 #rep(NA,maxIter)
					
		Kd.PB.SD.dist <- 0.0 #sd(Kd.PB.dist)
		
		DG.PB.SD.dist <- R*(temperature + 273.15)*Kd.PB.SD.dist / Kd.PB.est
		DG.PB.SD.gesamt <- sqrt( DG.PB.SD.dist^2 + DG.PB.SD^2 )
		
		Kd.PB.SD.gesamt <- sqrt( sd( Kd.PB.dist )^2 + Kd.PB.SD^2 )
		DH.PB.SD.gesamt <- sqrt( sd( DH.PB.dist )^2 + DH.PB.SD^2 )
		
		Kd.DH.PB.SD.dist <- 1000000.0 * cov(Kd.PB.dist, DH.PB.dist)
		TDS.PB.SD.dist <- sqrt( DG.PB.SD.dist^2 + (1000000.0 * sd(DH.PB.dist) )^2 - 2.0 * R * (temperature + 273.15) * Kd.DH.PB.SD.dist/Kd.PB.est )
		TDS.PB.SD.gesamt <- sqrt( TDS.PB.SD.dist^2 + TDS.PB.SD^2 )
		
		#===End: extended SD==================================================================================
				
		Kd.CI95 <- c(NA, NA)
		DH.CI95 <- c(NA, NA)
		N.CI95 <- c(NA, NA)
		DG.CI95 <- c(NA, NA)
		Q.dil.CI95 <- c(NA, NA)
		N.sd <- NA
		Q.dil.SD <- NA
		
		# compute standardized residuals
		standardizedResiduals <- computeStandarizedResiduals( residuals(itc.fit), MR.matrix, df.residual(itc.fit), Bad.data )
		
	    Q.bad <- fetch.data( Q.vector, Bad.data, TRUE )
	    MR.bad <- fetch.data( MR.vector, Bad.data, TRUE )
		
		DH.at0 <- NA
		c.value <- NA 
		
		#print(paste("length(Kd.PB.est) :", length(Kd.PB.est)))
		#print(paste("Kd.PB.est :", Kd.PB.est))
		
		#print(paste("displacement -> itc.fit.summary -> ", itc.fit.summary$coefficients))
		output <- list( #summary = itc.fit.summary$coefficients,
					# summary = itc.fit.summary,	
					Kd.PB.est = Kd.PB.est, Kd.PB.SD = Kd.PB.SD.gesamt , Kd.CI95 = Kd.CI95, 
					DH.PB.est = 1000.0 * DH.PB.est, DH.PB.SD = 1000.0 * DH.PB.SD.gesamt, DH.CI95 = 1000.0 * DH.CI95, 
					N.est = N.est, N.SD = N.sd, N.CI95 = N.CI95,
					Q.dil.est = Q.dil.est, Q.dil.SD = Q.dil.SD, Q.dil.CI95,
					DG.PB.est = DG.PB.est/1000.0, DG.PB.SD = DG.PB.SD.gesamt/1000.0, DG.CI95 = DG.CI95/1000.0, 
					TDS.PB.est = TDS.PB.est, TDS.PB.SD = TDS.PB.SD.gesamt/1000.0,
					MR.matrix = MR.matrix, 
					Q.norm.matrix = Q.matrix, 
					DH.at0 = DH.at0, 
					Q.bad = Q.bad, 
					MR.bad = MR.bad, 
					curve = Q.theo, 
					c.value = c.value, 
					lackOfFit = list(FValue = FValue, PValue = PValue), 
					residuals = list( x = standardizedResiduals$molarRatio, y = standardizedResiduals$residuals, DoF = df.residual(itc.fit) )
		)
		
		return(output)
	
	},
	options = list(optimize = 3, supressAll = TRUE)
)

