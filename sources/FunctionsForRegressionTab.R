# functions for regression tab
#========================================================================================

setSubtractionVariable <- function(button){
	if ( length(dataSets[[2]]) > 0 ){
		inputRegressionData$subtractBackground <<- ifelse( button$active, TRUE, FALSE )
	}else{
		inputRegressionData$subtractBackground <<- FALSE
		gtkToggleButtonSetActive( CheckButtonBackgroundSubtraction, is.active = FALSE )
	}
}

#========================================================================================

setSelectionNorM <- function(button){
	if (button["active"]){
		if ( identical(button$getLabel(), "for Analyte (N)") ){
			inputRegressionData$NorM <<- "N"
		}else if ( identical(button$getLabel(), "for Titrant (M)") ){
			inputRegressionData$NorM <<- "M"
		}else{
			inputRegressionData$NorM <<- "0"
		}
	}
}

#========================================================================================

setModelSelection <- function(button){
	temp <- button$getActive()
	if (temp == 0)
	   inputRegressionData$regressionModelID <<- "one-to-one"
	else
	   inputRegressionData$regressionModelID <<- "competition"
}

#========================================================================================

setRegressionParams <- function(button){
	
	if ( length( dataSets[[1]] ) > 0 | length( dataSets[[2]] ) > 0 ){
	
		regressionParams <- NULL
		fileSelectText <- NULL
		
		for (i in seq(along = dataSets[[1]]) ){
				fileSelectText <- c( fileSelectText, dataSets[[1]][[i]]$fileNameShort )
				regressionParams <- rbind( regressionParams, 
											c( dataSets[[1]][[i]]$CellConc, dataSets[[1]][[i]]$SyringeConc, dataSets[[1]][[i]]$CellVolume  ) 
										)
		}
		
		for (i in seq(along = dataSets[[2]]) ){
					fileSelectText <- c( fileSelectText, dataSets[[2]][[i]]$fileNameShort )
					regressionParams <- rbind( regressionParams, 
											c( dataSets[[2]][[i]]$CellConc, dataSets[[2]][[i]]$SyringeConc, dataSets[[2]][[i]]$CellVolume  ) 
				)
		}
						
		#===Begin: dialog window===
		dialog <- gtkDialogNewWithButtons(
		  title = "Conc. & Cell volume",
		  parent = NULL,
		  flags = 0,
		  "gtk-ok", GtkResponseType["ok"],
		  "gtk-cancel", GtkResponseType["cancel"],
		  show = FALSE
		)
		
		writeNewParams <- function(dialog, response){
		  	if (response == GtkResponseType["ok"]){
		  		#print(regressionParams)
		  		
				for (i in seq(along = dataSets[[1]]) ){
						dataSets[[1]][[i]]$CellConc <<- as.numeric( regressionParams[i, 1] )
						dataSets[[1]][[i]]$SyringeConc <<- as.numeric( regressionParams[i, 2] )
						dataSets[[1]][[i]]$CellVolume <<- as.numeric( regressionParams[i, 3] )
				}
				
				for (i in seq(along = dataSets[[2]]) ){
						j = i + length(dataSets[[1]]) #max.data.set.nb.tob[1]
						dataSets[[2]][[i]]$CellConc <<- as.numeric( regressionParams[j, 1] )
						dataSets[[2]][[i]]$SyringeConc <<- as.numeric( regressionParams[j, 2] )
						dataSets[[2]][[i]]$CellVolume <<- as.numeric( regressionParams[j, 3] )
				}
				
		  	}
		  	dialog$Destroy()					
		}
		
		gSignalConnect(dialog, "response", writeNewParams)
		#===End: dialog window===
		
		# Combo Box for data set selection
		hboxFileSelect <- gtkHBox()
		
		hboxFileSelect$packStart( gtkLabel("Files ") )
		
		combo <- gtkComboBoxNewText()
		sapply( fileSelectText, combo$appendText )
		
		combo["active"] <- 0
		
		fileSelection <- function(button){
		  rowNumber <- 1 + button$getActive()
		  gtkEntrySetText( entryCellConc, regressionParams[rowNumber, 1] )
		  gtkEntrySetText( entrySyringeConc, regressionParams[rowNumber, 2] )
		  gtkEntrySetText( entryCellVolume, regressionParams[rowNumber, 3] )
		}
		
		gSignalConnect( combo, "changed", fileSelection )
		
		hboxFileSelect$packStart(combo)

		getNewEntries <- function( newEntry, position){
			#print(newEntry$getText())
			text <- trim( newEntry$getText() )
			if ( nzchar( gsub( "[0-9.]", "", text ) ) == FALSE ){
				if ( is.na( as.numeric(text) ) == FALSE ){
					# text is a number
					newEntry$setIconFromStock( "primary", "gtk-yes" )
					newEntry$setIconTooltipText( "primary", NULL )
					regressionParams[ (1 + combo$getActive()), position ] <<- text
				}else{
					# test is empty
					newEntry$setIconFromStock( "primary", "gtk-no" )
					newEntry$setIconTooltipText( "primary", "Empty entries are not allowed" )
					#print("entry cannot be empty")
				}
			}else{
				# text is not a number
				newEntry$setIconFromStock( "primary", "gtk-no" )
				newEntry$setIconTooltipText( "primary", "Only numbers are allowed" )
				#print("Only numbers are allowed")				
			}
		}
		
		#Entry for Cell Concentration
		entryCellConc <- gtkEntry()
		gtkEntrySetText( entryCellConc, regressionParams[1,1] )
		gSignalConnect( entryCellConc, "changed", getNewEntries, 1  )
		entryCellConc$setIconFromStock( "primary", "gtk-yes" )
		hboxCellConc <- gtkHBox()
		hboxCellConc$packStart( gtkLabel( "Conc. in Cell [muM]:" ), expand = TRUE, padding = 2 )
		hboxCellConc$packStart( entryCellConc, expand = FALSE )

		# Entry for Syringe Concentration				
		entrySyringeConc <- gtkEntry()
		gtkEntrySetText( entrySyringeConc, regressionParams[1,2] )
		gSignalConnect( entrySyringeConc, "changed", getNewEntries, 2  )
		entrySyringeConc$setIconFromStock( "primary", "gtk-yes" )
		hboxSyringeConc <- gtkHBox()
		hboxSyringeConc$packStart( gtkLabel("Conc. in Syringe [muM]:") , expand = TRUE, padding = 2 )
		hboxSyringeConc$packStart( entrySyringeConc, expand = FALSE )
		
		# Entry for Cell Volume				
		entryCellVolume <- gtkEntry()
		gtkEntrySetText( entryCellVolume, regressionParams[1,3] )
		gSignalConnect( entryCellVolume, "changed", getNewEntries, 3  )
		entryCellVolume$setIconFromStock( "primary", "gtk-yes" )
		hboxCellVolume <- gtkHBox()
		hboxCellVolume$packStart( gtkLabel( "Cell Volume [muL]:" ), expand = TRUE, padding = 2 )
		hboxCellVolume$packStart( entryCellVolume, expand = FALSE )
		
		# put everything in a box
		vbox <- dialog$getContentArea()
		vbox["spacing"] <- 20
		vbox$packStart( hboxFileSelect)
		vbox$packStart( hboxCellConc, padding = 2 )
		vbox$packStart( hboxSyringeConc, padding = 2 )
		vbox$packStart( hboxCellVolume, padding = 2 )
										
		dialog$showAll()
		dialog$setModal(TRUE)
	
	}
}

#========================================================================================

#checkBagroundData <- function( titrationDataSet, backgroundDataSet ){
	
	#ok <- TRUE
	
	# 1.) Check Titrant Concentrations
	# 1.a) Check for Titrations: all the same?
	# 1.b) Check for Background: all the same?
	# 1.c) Is there a difference between Titrations and Background?
	
	# 2.) Check dVi
	
	# 3.) Check V0
	
	# 4.) Check Temperature
	
	#return( ok )
#}

#========================================================================================

callOneToOneRegressionDriver <- function( Q.matrix, dVi.matrix, Pmax, Lmax, V0, temperature, buffer ){
	
    input.data <- list(	Q.matrix = Q.matrix, 
    					dVi.matrix = dVi.matrix, 
    					Pmax = Pmax, 
    					Lmax = Lmax, 
    					V0 = V0, 
    					NorM = inputRegressionData$NorM,
                   		temperature = temperature 
                   	)
	
	#===START REGRESSION: ONE-TO-ONE==============================
	outputRegressionData <<- regressionOnetoOneModel( input.data )
	#=============================================================
	
	outputRegressionData <<- c( outputRegressionData, activeTitrationPlot = "showAll" )
	
	outputLyrics <<- createOutputLyrics( outputRegressionData, inputRegressionData$regressionModelID, inputRegressionData$NorM, temperature )
	buffer$setText( outputLyrics )
	
}

#========================================================================================

callCompetitionRegessionDriver <- function( Q.matrix, dVi.matrix, Pmax, Lmax, V0, temperature, buffer ){
	
	dialog <- gtkDialogNewWithButtons(
	  title = "Competition parameter",
	  parent = NULL,
	  flags = 0,
	  "gtk-ok", GtkResponseType["ok"],
	  "gtk-cancel", GtkResponseType["cancel"],
	  show = FALSE
	)
	
	#gSignalConnect( dialog, "response", f <- setCompetitionParams(){} )
	
	setCompetitionParams <- function(dialog, response){
	  	if (response == GtkResponseType["ok"]){
	
			# set text entries of input dialog to current values
	  		gtkEntrySetText( entryKdPA, inputRegressionData$dataForCompetition$Kd.PA )
	  		gtkEntrySetText( entryKdPAsd, inputRegressionData$dataForCompetition$Kd.PA.SD )
	  		gtkEntrySetText( entryDHPA, inputRegressionData$dataForCompetition$DH.PA )
	  		gtkEntrySetText( entryDHPAsd, inputRegressionData$dataForCompetition$DH.PA.SD )
	  		gtkEntrySetText( entryConcB, inputRegressionData$dataForCompetition$Bmax )
	  		
	  		# check if values are right numbers
	  			  			
  			input.data <- list( Q.matrix = Q.matrix, dVi.matrix = dVi.matrix, Pmax = Pmax, Amax = Lmax, 
  								Bmax = as.numeric( inputRegressionData$dataForCompetition$Bmax ), 
  						    	Kd.PA = as.numeric( inputRegressionData$dataForCompetition$Kd.PA ), 
  						    	Kd.PA.SD = as.numeric( inputRegressionData$dataForCompetition$Kd.PA.SD ), 
  						    	DH.PA = as.numeric( inputRegressionData$dataForCompetition$DH.PA ), 
  						    	DH.PA.SD = as.numeric( inputRegressionData$dataForCompetition$DH.PA.SD ), 
  						    	V0 = V0, NorM = inputRegressionData$NorM, temperature = temperature )
	  		
	  		#===START REGRESSION: DISPLACEMENT=========================
	  		outputRegressionData <<- compITCDisplacements( input.data ) #modelCompetitionSigurskjold( input.data )
	  		#==========================================================
	  		
	  		outputRegressionData <<- c( outputRegressionData, activeTitrationPlot = "showAll" )
	  		
			outputLyrics <<- createOutputLyrics( outputRegressionData, inputRegressionData$regressionModelID, inputRegressionData$NorM, temperature )
			buffer$setText( outputLyrics )
									
	  	}
	  	dialog$Destroy()					
	}
	
	gSignalConnect(dialog, "response", setCompetitionParams)
	
	#===Begin: Entries of dialog===
	getNewEntries <- function( newEntry, position){
		#print(newEntry$getText())
		value <- newEntry$getText()
		# check if value is numeric
		if ( identical(position, "Kd.PA") )
			inputRegressionData$dataForCompetition$Kd.PA <<- value
		else if ( identical(position, "Kd.PA.SD") )
			inputRegressionData$dataForCompetition$Kd.PA.SD <<- value
		else if ( identical(position, "DH.PA") )
			inputRegressionData$dataForCompetition$DH.PA <<- value
		else if ( identical(position, "DH.PA.SD") )
			inputRegressionData$dataForCompetition$DH.PA.SD <<- value
		else if ( identical(position, "Bmax") )
			inputRegressionData$dataForCompetition$Bmax <<- value
	}
	
	# Kd of PA entry
	entryKdPA <- gtkEntry()
	gtkEntrySetText( entryKdPA, inputRegressionData$dataForCompetition$Kd.PA  )
	gSignalConnect( entryKdPA, "changed", getNewEntries, "Kd.PA"  )
	hboxKdPA <- gtkHBox()
	hboxKdPA$packStart( gtkLabel("Kd PA [µM]:"), expand = TRUE, padding = 2 )
	hboxKdPA$packStart( entryKdPA, expand = FALSE )
	
	# SD of Kd of PA entry
	entryKdPAsd <- gtkEntry()
	gtkEntrySetText( entryKdPAsd, inputRegressionData$dataForCompetition$Kd.PA.SD  )
	gSignalConnect( entryKdPAsd, "changed", getNewEntries, "Kd.PA.SD"  )
	hboxKdPAsd <- gtkHBox()
	hboxKdPAsd$packStart( gtkLabel("SD of Kd PA [µM]:"), expand = TRUE, padding = 2 )
	hboxKdPAsd$packStart( entryKdPAsd, expand = FALSE )
	
	# DH of PA entry
	entryDHPA <- gtkEntry()
	gtkEntrySetText( entryDHPA, inputRegressionData$dataForCompetition$DH.PA )
	gSignalConnect( entryDHPA, "changed", getNewEntries, "DH.PA"  )
	hboxDHPA <- gtkHBox()
	hboxDHPA$packStart( gtkLabel("DH PA [kJ/mol]:"), expand = TRUE, padding = 2 )
	hboxDHPA$packStart( entryDHPA, expand = FALSE )
	
	# SD of DH of PA
	entryDHPAsd <- gtkEntry()
	gtkEntrySetText( entryDHPAsd, inputRegressionData$dataForCompetition$DH.PA.SD )
	gSignalConnect( entryDHPAsd, "changed", getNewEntries, "DH.PA.SD"  )
	hboxDHPAsd <- gtkHBox()
	hboxDHPAsd$packStart( gtkLabel("SD of DH PA [kJ/mol]:"), expand = TRUE, padding = 2 )
	hboxDHPAsd$packStart( entryDHPAsd, expand = FALSE )
	
	# concentration of B entry
	entryConcB <- gtkEntry()
	gtkEntrySetText( entryConcB, inputRegressionData$dataForCompetition$Bmax )
	gSignalConnect( entryConcB, "changed", getNewEntries, "Bmax"  )
	hboxSyringeConc <- gtkHBox()
	hboxSyringeConc$packStart( gtkLabel("Conc. B0 in Cell [µM]:"), expand = TRUE, padding = 2 )
	hboxSyringeConc$packStart( entryConcB, expand = FALSE )
	#===End: entries of dialog===
	
	# put everything in a box
	vbox <- dialog$getContentArea()
	vbox["spacing"] <- 20
	#vbox$packStart(hboxFileSelect)
	vbox$packStart( hboxKdPA, padding = 2 )
	vbox$packStart( hboxKdPAsd, padding = 2 )
	vbox$packStart( hboxDHPA, padding = 2 )
	vbox$packStart( hboxDHPAsd, padding = 2 )
	vbox$packStart( hboxSyringeConc, padding = 2 )
									
	dialog$showAll()
	dialog$setModal(TRUE)
	#===End: dialog for competition params===
}

#========================================================================================

runRegression <- function(widget, data){
	
	if ( length( dataSets[[1]] ) <= 0 ) return()
	
	view <- data
	buffer <- view$getBuffer()
	
	#===Begin: get background heats===			
	if ( inputRegressionData$subtractBackground == TRUE & length( dataSets[[2]] ) > 0 ){
		
		# check Background data: is it possible to do the substraction
		#ok <- checkBagroundData( dataSets[[1]], dataSets[[2]] )
		#if ( ok == FALSE ) return()
				
		max.length <- NULL
		for ( i in seq(along = dataSets[[2]]) ){
	  		max.length <- c( max.length, length( dataSets[[2]][[i]]$integratedHeats ) )
		}
		
		backgroundHeats.temp <- matrix( NA, nrow = max(max.length), ncol = length( dataSets[[2]] ) )
		
		# integrated background for data set i
		for (i in seq(along = dataSets[[2]]) ){
			
			# Integrate data set
    		dataSets[[2]][[i]]$integratedHeats <<- computeIntegrationOfHeats( dataSets[[2]][[i]] )
    			
    		# average background heats
    		m <- max.length[i]
    			
    		backgroundHeats.temp[1:m, i] <- dataSets[[2]][[i]]$integratedHeats[1:m]
    		    		
		}
		
		# simply average
		backgroundHeats <- NULL
		for (i in seq(length = max(max.length)) ){
			backgroundHeats <- c( backgroundHeats, mean( backgroundHeats.temp[ i, seq(along = dataSets[[2]]) ], na.rm = TRUE ) )
		}
		
	}
	#===End: get background heats===	

	#===Begin: get titration heats===
	#titrationHeats <- computeTitrationHeats( dataSets )
			
	max.length <- NULL
	for ( i in seq(along = dataSets[[1]]) ){
	  max.length <- c( max.length, length( dataSets[[1]][[i]]$integratedHeats ) )	
	}

	Q.matrix <- matrix( NA, nrow = max(max.length), ncol = length(dataSets[[1]]) )
	dVi.matrix <- matrix( NA, nrow = max(max.length), ncol = length(dataSets[[1]]) )
	temperature <- 0.0
	Pmax <- NULL
	Lmax <- NULL
	V0 <- NULL
	
	# collect data, subtract background
	for ( i in seq(along = dataSets[[1]]) ){
						
		# Integrate data set
    	dataSets[[1]][[i]]$integratedHeats <<- computeIntegrationOfHeats( dataSets[[1]][[i]] )
    		
    	m <- max.length[i]
    	if ( inputRegressionData$subtractBackground == TRUE ){	
    		# subtract background				
			Q.matrix[1:m,i] <- dataSets[[1]][[i]]$integratedHeats[1:m] - backgroundHeats[1:m]
		}else{
			Q.matrix[1:m,i] <- dataSets[[1]][[i]]$integratedHeats[1:m]
		}
		
		dVi.matrix[1:m,i] <- dataSets[[1]][[i]]$dViValues[1:m]
		temperature <- temperature + dataSets[[1]][[i]]$meanTemperature
		Pmax <- c( Pmax, dataSets[[1]][[i]]$CellConc )
		Lmax <- c( Lmax, dataSets[[1]][[i]]$SyringeConc )
		V0 <- c( V0, dataSets[[1]][[i]]$CellVolume )
				
	}
	#===End: get titration heats===			
			
	temperature <- temperature/length(dataSets[[1]])
	#meanTemperature <<- temperature

	if ( identical(inputRegressionData$regressionModelID, "one-to-one") ){
		
		callOneToOneRegressionDriver( Q.matrix, dVi.matrix, Pmax, Lmax, V0, temperature, buffer )
		
	}else if( identical(inputRegressionData$regressionModelID, "competition") ){
		
		callCompetitionRegessionDriver( Q.matrix, dVi.matrix, Pmax, Lmax, V0, temperature, buffer )
				
	}else{
		message( "ERROR in function 'runRegression': value of modelID is unknown:", inputRegressionData$regressionModelID )
	}

	#===Begin: compute heats per injectant for background===
	if ( length( dataSets[[2]] ) > 0 ){
		# outputRegressionData <<- computeHeatsPerInjectantForBackground( dataSets )

		molarRatioBackground <- matrix( NA, nrow = length( dataSets[[2]][[1]]$dViValues ), ncol = length( dataSets[[2]] ) )
 		QBackground <- NULL
 		
 		if ( identical(inputRegressionData$NorM, "M") ) {
 			out <- outputRegressionData$summary
 			N.est <- out[4,1]
 		}

 		for ( i in seq( along = dataSets[[2]] ) ){
 			# integrated all heats
 			computeIntegrationOfHeats( dataSets[[2]][[i]] )
 				
 			# compute molar ratio
 			DV <- 0.0
 			for ( j in seq(length = length(dataSets[[2]][[i]]$dViValues) ) ){
				DV <- DV + dataSets[[2]][[i]]$dViValues[j]
				temp <- 0.5 * DV / dataSets[[2]][[i]]$CellVolume
				P0 <- dataSets[[2]][[i]]$CellConc * ( 1.0 - temp )/( 1.0 + temp )
				L0 <- dataSets[[2]][[i]]$SyringeConc * ( DV / dataSets[[1]][[1]]$CellVolume ) * ( 1.0 - temp )
				molarRatioBackground[j,i] <- L0/P0
			}
 				
 			# compute heats per injection
			if ( identical(inputRegressionData$NorM, "M") ){
				QBackground <- cbind( QBackground, 
					1000.0 * dataSets[[2]][[i]]$integratedHeats / ( dataSets[[2]][[i]]$dViValues * N.est * dataSets[[2]][[i]]$SyringeConc ) )
			}else{
   				QBackground <- cbind( QBackground, 
   					1000.0 * dataSets[[2]][[i]]$integratedHeats / ( dataSets[[2]][[i]]$dViValues * dataSets[[2]][[i]]$SyringeConc ) )
			}
				
		}
			
		# average for line
		QBackgroundAverage <- rep( NA, length( QBackground[,1])  )
		
		for (i in seq(along = QBackgroundAverage) ){
			QBackgroundAverage[i] <- mean( QBackground[i, ], na.rm = TRUE )
		}
		
		#print("Hintergrund")
		#print(paste( QBackground  ))
		
		outputRegressionData <<- c( outputRegressionData,list(  molarRatioBackground = molarRatioBackground,
																QBackground = QBackground,
																QBackgroundAverage = QBackgroundAverage,
																activeBackgroundPlot = "showAll"
		 													 ) 
		 						 )
	
	}
	#===End: compute heats per injectant or background===

	#===Begin: compute Heats per Injectant for Titration *without* background subtraction===
	if ( inputRegressionData$subtractBackground == TRUE & length( dataSets[[2]] ) > 0 ){
		#print("PLING!")
		
		QRawTitration <- NULL
		
 		#print(paste("out Tit -> ", out))
 		if ( identical(inputRegressionData$NorM, "M") ) {
 			out <- outputRegressionData$summary
 			N.est <- out[4,1]
 		}
 		
		for ( i in seq(along=dataSets[[1]]) ){
 			# compute heats per injection
			if ( identical(inputRegressionData$NorM, "M") ){
				QRawTitration <- cbind( QRawTitration, 
					1000.0 * dataSets[[1]][[i]]$integratedHeats / ( dataSets[[1]][[i]]$dViValues * N.est * dataSets[[1]][[i]]$SyringeConc ) )
			}else{
   				QRawTitration <- cbind( QRawTitration, 
   					1000.0 * dataSets[[1]][[i]]$integratedHeats / ( dataSets[[1]][[i]]$dViValues * dataSets[[1]][[i]]$SyringeConc ) )
			}
		}
		
		# average for line
		QRawTitrationAverage <- rep( NA, length( QRawTitration[,1])  )
		for (i in seq(along = QRawTitrationAverage) ){
			QRawTitrationAverage[i] <- mean( QRawTitration[i, ], na.rm = TRUE )
		}
		
		outputRegressionData <<- c( outputRegressionData,list(	QRawTitration = QRawTitration,
																subtractBackground = TRUE,
																QRawTitrationAverage = QRawTitrationAverage
																#activeBackgroundPlot = "showAll"
 													 		) 
 						 		)
	}else{
		#print("PLONG!")
		outputRegressionData <<- c( outputRegressionData,list(	#QRawTitration = QRawTitration,
																subtractBackground = FALSE
																#QRawTitrationAverage = QRawTitrationAverage
																#activeBackgroundPlot = "showAll"
 													 		) 
 						 		)
	}
	#===End: compute Heats per Injectant for Titration *without* background subtraction===

	plotRegressionOutput( outputRegressionData )

}

#========================================================================================