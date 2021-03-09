#======================================================================

newReadITCFile <- function( inputFileName ){
	
	output <- list()
	fileIsValid <- FALSE
	
	# check if File exists
	
	# short name of file
	inputFileNameShort <- basename( inputFileName )
	#inputFileNameShort <- strsplit( inputFileName, "/" )
    #inputFileNameShort <- inputFileNameShort[[1]][ length( inputFileNameShort[[1]] ) ]
    
    #===Begin: get number of skipped lines===
	nbOfSkippedLines <- 0
	tempArray <- NULL #c()
	repeat{
	  temp <- scan(file = inputFileName, what = character(), n = 2, skip = nbOfSkippedLines, quiet = TRUE)
	  
	  #if ( strtrim(temp,1) == "#" ){
	  if ( identical( strtrim(temp,1)[1], "#" ) ){
	  	#print(strtrim(temp,1)[1])
	    tempArray <- c( tempArray, temp[2] )
	  }
	  
	  #if ( strtrim(temp,1) == "@" ){
	  if ( identical( strtrim(temp,1)[1], "@" ) ){
	  	#print("PLOPP")
	  	#print(strtrim(temp,1)[1])
	    break
	  }else{
	    nbOfSkippedLines <- nbOfSkippedLines + 1
	  }
	}
    #===End: get number of skipped lines===
	
	#===Begin: get V0, P0, L0===
	#if ( is.numeric( tempArray[2] ) ){
		SyringeConc <- 1000.0 * as.numeric( tempArray[2] )
	#}else{
		#fileIsValid <- FALSE
		#return( list( fileIsValid, output ) )
	#}	
	
	#if ( is.numeric( tempArray[3] ) ){
		CellConc <- 1000.0 * as.numeric( tempArray[3] )
	#}else{
		#fileIsValid <- FALSE
		#return( list( fileIsValid, output ) )
	#}
			
	#if ( is.numeric( tempArray[4] ) ){
		CellVolume <- 1000.0 * as.numeric( tempArray[4] )
	#}else{
		#fileIsValid <- FALSE
		#return( list( fileIsValid, output ) )
	#}
	#===End: get V0, P0, L0===
	
	#===Begin: how many input items===
	temp <- scan( file = inputFileName, what = character(), n = 1, skip = nbOfSkippedLines + 1, quiet = TRUE )
	temp <- strsplit( temp, "," )
	if ( length(temp[[1]]) == 3 ){
	  columnNames <- c("time", "Q", "TempCell")	
	}else{
	  columnNames <- c("time", "Q", "TempCell", "dTemp1?", "TempJacket","dTemp2?","?")
	}
		
	# read input
	input <- read.csv( file = inputFileName,
		               skip = nbOfSkippedLines,
		               fill = TRUE,
		               header = FALSE,
		               stringsAsFactors = FALSE,
		               col.names = columnNames
		             )
	#===End: how many input items===
	
	#===Begin: get position for start of each injection===
	nbOfInjections <- -1
	startPosTimeInjec <- 0
	dVi_values <- c()
	filterPeriod <- c()
	for (i in seq(along = input[,1]) ){
	  #if ( strtrim(input[i,1],1) == "@" ){
	  if ( identical( strtrim(input[i,1],1), "@" ) ){
		if (i == 1){
	       startPosTimeInjec <- i + 1
		}else{
		   startPosTimeInjec <- c( startPosTimeInjec, i + 1 )
		   dVi_values <- c( dVi_values, input[i,2] )
		   filterPeriod <- c( filterPeriod,  as.numeric(input[i+2,1]) - as.numeric(input[i+1,1]) )
		}
		  nbOfInjections <- nbOfInjections + 1
		}
	}
	#===End: get position for start of each injection===
	
	end.pos.time.injec <- length(input[,1])
		
	# get length (for entrys) of thermogramm for delay (injec.length[1]) and injections
	injec.length <- rep( NA, nbOfInjections + 1 ) 
	for (i in seq(length = nbOfInjections) ){
	  injec.length[i] <- startPosTimeInjec[i+1] - startPosTimeInjec[i] - 1  
	}
		
	# for last injection
	injec.length[ nbOfInjections + 1 ] <- end.pos.time.injec - startPosTimeInjec[nbOfInjections + 1] + 1
	
	if ( any(injec.length == 0.0)){
		#cat("ERROR: Data file -> ", input.file.name, " is corrupt.\n")
		#remove.item(widget,treeview)
		#cat("Data removed.\n")
		#remove.data.set(data.set.nb)
	}else{
		
		maxdiff <- max( injec.length )
		
		# seperate thermogramm in delay and injections and get temperature course for each injection 
		injec.time.Matrix <- matrix( NA, ncol = ( nbOfInjections + 1), nrow = maxdiff )
		injec.Q.Matrix <- matrix( NA, ncol = ( nbOfInjections + 1), nrow = maxdiff )
		temperature.course <- matrix( NA, ncol = ( nbOfInjections + 1), nrow = maxdiff )
					
		# create injec.time.Matrix (=x) and injec.Q.Matrix (=y) for data fit
		for (i in seq(length = (nbOfInjections + 1)) ){
		  start <- startPosTimeInjec[i]
		  if (i < nbOfInjections + 1){
		    ende <- startPosTimeInjec[i+1] - 2
		  }else{
		    ende <- end.pos.time.injec
		  }
		  		  
		  injec.time.Matrix[ (1:injec.length[i]), i] <- as.numeric(input[start:ende, 1])
		  injec.Q.Matrix[ (1:injec.length[i]), i] <- input[start:ende, 2]
		  temperature.course[ (1:injec.length[i]), i] <- input[start:ende, 3]
		}
		
		# initial injection numbers
		#injecNb <- 1
		
	}
		
	output <- list( CellConc = CellConc,
					SyringeConc = SyringeConc,
					CellVolume = CellVolume,
					injecTimeMatrix = injec.time.Matrix,
					injecQMatrix = injec.Q.Matrix,
					temperatureCourse = temperature.course,
					#injecNb = injecNb,
					inputFileNameShort = inputFileNameShort,
					dVivalues = dVi_values,
					filterPeriod = filterPeriod,
					nbOfInjections = nbOfInjections,
					injecLength = injec.length
				  )
	fileIsValid <- TRUE
    
    return( list( fileIsValid = fileIsValid, output = output ) )
    
}

#======================================================================

addNewDataSet <- function ( fileContent, titration_or_background ){
		
    newDataSet <- newDataSetEntry #new("newDataEntry")
    
    newDataSet$fileNameShort <- fileContent$inputFileNameShort
    
    newDataSet$SyringeConc <- fileContent$SyringeConc
	newDataSet$CellConc <- fileContent$CellConc
	newDataSet$CellVolume <- fileContent$CellVolume
	
    newDataSet$dViValues <- fileContent$dVivalues
	newDataSet$filterPeriod <- fileContent$filterPeriod
	
	newDataSet$nbOfInjections <- fileContent$nbOfInjections
	newDataSet$maxInjecNb <- fileContent$nbOfInjections + 1
	
	newDataSet$injecLength <- fileContent$injecLength
	newDataSet$injecTimeMatrix <- fileContent$injecTimeMatrix
	newDataSet$injecQMatrix <- fileContent$injecQMatrix
	newDataSet$temperatureCourse <- fileContent$temperatureCourse
	newDataSet$meanTemperature <- mean( as.vector( fileContent$temperatureCourse ), na.rm=TRUE )
	
	newDataSet$injecNb <- 1
	
	newDataSet$bgFitValues <- matrix( 0.0, ncol = newDataSet$maxInjecNb, nrow = 3 )
	newDataSet$integrationRange <- rep( 0.7, newDataSet$maxInjecNb )
	newDataSet$viewportRange <- rep( 0.0, newDataSet$maxInjecNb )
	
	# show file name
	if ( identical(titration_or_background, "titration") ){
		tob <<- 1
		iter <- modelTitration$append()$iter
       	modelTitration$set( iter, 0, newDataSet$fileNameShort )
	}else{
		tob <<- 2
		iter <- modelBackground$append()$iter
       	modelBackground$set( iter, 0, newDataSet$fileNameShort ) 		
	}
		
	# add newDataSet entry to dataSets
	dataSets[[tob]] <<- c( dataSets[[tob]], list(newDataSet) )
	
	# set pointer to active data set 
	activeDataSet[tob] <<- activeDataSet[tob] + 1
	
	# initial background fit
    dataSetNb <- length( dataSets[[tob]] )
	for ( injecNb in seq( length = newDataSet$maxInjecNb ) ){
		fitBackground( tob, dataSetNb, injecNb )
	}
	
	dataSets[[tob]][[ dataSetNb ]]$integratedHeats <<- computeIntegrationOfHeats( dataSets[[tob]][[ dataSetNb ]] )
	#print( dataSets[[tob]][[ dataSetNb ]]$integratedHeats )
	
	#computeIntegrationOfHeats.c( data.sets[[tob]][[data.set.nb]] )
	#integration()
		
	#return( newDataSet )
    
}

#======================================================================

call.open.dialog <- function( titration_or_background ){
	
  # set Tab to "baseline correction" of mainNotebook
  mainNotebook["page"] <- 0
	
  # create open file dialog
  dialog <- gtkFileChooserDialog( title = "Choose a .itc file", 
                                  parent = window, 
                                  action = "open",
                                  "gtk-cancel", GtkResponseType["cancel"], 
                                  "gtk-open", GtkResponseType["accept"]
                                )
  # allow multiple file selections
  dialog["select-multiple"] <- TRUE
  
  # set filter for .itc files
  fileFilter <- gtkFileFilter()
  fileFilter$setName( ".itc files" )
  fileFilter$addPattern( "*.itc" )
  dialog$addFilter( fileFilter )
    
  # open files
  if (dialog$run() == GtkResponseType["accept"]) {
  	
  	file.names <- dialog$getFilenames()
  	for (i in seq(along = file.names) ){
  		
  		fileContent <- newReadITCFile( file.names[[i]] )
  		
  		if ( fileContent$fileIsValid == TRUE ){
  			
  			# create new data set entry
  			addNewDataSet( fileContent$output, titration_or_background )
  			
  		}else{
  			
  		}
  	}

    # show thermogram of latest .itc file
    showThermogram()
  
  }
    
  # close dialog box 
  dialog$destroy()
}

#======================================================================