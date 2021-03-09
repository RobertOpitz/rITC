# functions for toolbar
#==============================================================================

newProject <- function(widget, data){
	
	# clean evertyhing for a new project, if necessary
	
	#if ( length( dataSets[[1]] ) > 0 | length( dataSets[[2]] ) > 0 ){
		
		# remove entries for titration file list
		gtkListStoreClear( modelTitration )

		# remove entries for background file list
		gtkListStoreClear( modelBackground )
		
		# remove output lyrics
		buffer <- viewRegressionResults$getBuffer()
		buffer$setText( "" )
		
		# set correction factor to "N"
		selectNorM.box[[1]]$setActive(TRUE)
		
		# set option "subtract background" to unselected
		gtkToggleButtonSetActive( CheckButtonBackgroundSubtraction, is.active = FALSE )
		
		# remove plots
		for( i in dev.list() ){
			dev.set(i)
			plot.new()
		}
				
		# reset data sets
		dataSets <<- list( titration <- list(), background <- list() )
		
		# pointer to titration (1) or background (2) data sets
		tob <<- 1

		# pointer to loaded titration (first entry) and background (second entry) files
		activeDataSet <<- c(0, 0)

		# reset data for regression
		inputRegressionData <<- list( 	subtractBackground = FALSE, 
										NorM = "N", 
										regressionModelID = "one-to-one", 
										dataForCompetition = list( 	Kd.PA = NA,
				  													Kd.PA.SD = NA,
				  													DH.PA = NA,
				  													DH.PA = NA,
				  													Bmax = NA	
				  												  ) 
									)
		outputRegressionData <<- NULL
		
		# reset comments section
		analyteEntry$setText("")
		titrantEntry$setText("")
		solventEntry$setText("")
		instrumentEntry$setText("")
		operatorEntry$setText("")
		
		buffer <- viewComments$getBuffer()
  		buffer$setText( "" )
		
		# reset file name of project
		projectFileName <<- NULL
		
		# set Tab to "baseline correction" of mainNotebook
		mainNotebook["page"] <- 0
		
		window$setTitle( "rITC" ) 	
	#}	
}

#==============================================================================

setGlobalData <- function( allData ){
	
	dataSets <<- allData$dataSets
	inputRegressionData <<- allData$inputRegressionData
	outputRegressionData <<- allData$outputRegressionData
	
	# set radio button for correction facktor
	if ( identical(inputRegressionData$NorM, "N") ) 
		i  <- 1
	else if ( identical(inputRegressionData$NorM, "M") )
		i <- 2
	else if ( identical(inputRegressionData$NorM, "0") )
		i <- 3
	else{
		message("WARNING: in function 'open_cb': value for NorM is unknown: ", inputRegressionData$NorM)
		inputRegressionData$NorM <<- "N"
		i <- 1
	}
	selectNorM.box[[i]]$setActive(TRUE)
	
	# set toggle button for subtracting background
	gtkToggleButtonSetActive( CheckButtonBackgroundSubtraction, is.active = inputRegressionData$subtractBackground )
		
	# set pointers to data sets
  	activeDataSet <<- c( length(dataSets[[1]]), length(dataSets[[2]]) )
  	
  	# set pointer to titration or background data set
  	if ( activeDataSet[1] <= 0 & activeDataSet[2] > 0 ) 
  		tob <<- 2
  	else
  		tob <<- 1
  	
  	# set regression output lyrics, if available
  	if ( is.null(outputRegressionData) == FALSE ){
	  	meanTemperature <- 0.0
	  	nbOfDataSets <- length( dataSets[[1]] )
	  	for ( i in seq(along = dataSets[[1]]) ){
	       meanTemperature <- meanTemperature + dataSets[[1]][[i]]$meanTemperature / nbOfDataSets		
	  	}
	  	
		outputLyrics <<- createOutputLyrics( outputRegressionData, inputRegressionData$regressionModelID, inputRegressionData$NorM, meanTemperature )#meanTemperature )
		buffer <- viewRegressionResults$getBuffer()
		buffer$setText( outputLyrics )
	}
  	
  	# set file names for titration file list
  	for ( i in seq(along = dataSets[[1]]) ){
		iter <- modelTitration$append()$iter
	    modelTitration$set(iter, 0, dataSets[[1]][[i]]$fileNameShort)		
	}
  	
  	# set file names for background file list
  	for ( i in seq(along = dataSets[[2]]) ){
       iter <- modelBackground$append()$iter
       modelBackground$set(iter, 0, dataSets[[2]][[i]]$fileNameShort) 		
  	}
  	
  	if ( allData$dataFileVersion == 4 ){
  		#print(allData$comments)
  		
 		analyteEntry$setText( allData$comments$analyteText )
 		titrantEntry$setText( allData$comments$titrantText )
		solventEntry$setText( allData$comments$solventText )
		operatorEntry$setText( allData$comments$operatorText )
	  		
  		buffer <- viewComments$getBuffer()
  		buffer$setText( allData$comments$additionalComments )
  	}

  	if ( allData$dataFileVersion == 5 ){
  		#print(allData$comments)
  		
 		analyteEntry$setText( allData$comments$analyteText )
 		titrantEntry$setText( allData$comments$titrantText )
		solventEntry$setText( allData$comments$solventText )
		instrumentEntry$setText( allData$comments$instrumentText )
		operatorEntry$setText( allData$comments$operatorText )
	  		
  		buffer <- viewComments$getBuffer()
  		buffer$setText( allData$comments$additionalComments )
  	}
  		
}

#==============================================================================

openProject <- function(widget) {
    
  # open dialog box   
  dialog <- gtkFileChooserDialog( title = "Choose a .rITC file", 
                                  parent = window, 
                                  action = "open",
                                  "gtk-cancel", GtkResponseType["cancel"], 
                                  "gtk-open", GtkResponseType["accept"]
                                  )
                                  
  fileFilter <- gtkFileFilter()
  fileFilter$setName(".rITC files")
  fileFilter$addPattern("*.rITC")
  dialog$addFilter(fileFilter)
  
  # open files
  if (dialog$run() == GtkResponseType["accept"]) {
  	
  	# cleanup
  	newProject()
  	
  	projectFileName <<- dialog$getFilename()
  	
    shortFileName <- basename( projectFileName )
  	window$setTitle( paste( "rITC -- ", shortFileName ) )
  	
	allData <- dget( dialog$getFilename() )
	
	setGlobalData( allData )
	
	#print( paste( "data file version -> ", allData$dataFileVersion ) )
	  	
    showThermogram()
  	
  }
  
  # close dialog box 
  dialog$destroy()
}

#==============================================================================

saveProjectMainRoutine <- function( FileName ){
	
	   	buffer <- viewComments$getBuffer()
	   	start <- buffer$getStartIter()$iter
	   	end <- buffer$getEndIter()$iter
	   	additionalComments <- buffer$getText(start, end)
	   	
	   	allData <- list( dataFileVersion = 5, 
	   					 dataSets = dataSets, 
	   					 inputRegressionData = inputRegressionData,
	   					 outputRegressionData = outputRegressionData,
	   					 comments = list( 	analyteText = analyteEntry$getText(),
	   					 					titrantText = titrantEntry$getText(),
	   					 					solventText = solventEntry$getText(),
	   					 					instrumentText = instrumentEntry$getText(),
	   					 					operatorText = operatorEntry$getText(),
	   					 					additionalComments = additionalComments
	   					 				)
					 )
	
	   	dput( allData, file = FileName )
	   	
}

#==============================================================================

saveProject <- function(widget) {
	
   if ( is.null( projectFileName ) ){
		saveProjectAs()	
   }else{
   	   	saveProjectMainRoutine( projectFileName )
   }
   
   #io_id <- statusbar$getContextId("I/O")
   #statusbar$push(io_id, "file saved")
   #statusbar$pop(io_id)
   
}

#==============================================================================

saveProjectAs <- function(widget) {
	
   dialog <- gtkFileChooserDialog( title = "Enter a name for the file", 
          						  parent = window, 
          						  action = "save", 
          						  "gtk-cancel", GtkResponseType["cancel"], 
          						  "gtk-save", GtkResponseType["accept"]
          						 )
          						 
   if (dialog$run() == GtkResponseType["accept"]) {
   	FileName <- dialog$getFilename()
   	 	
   	fileDirName <- dirname( FileName )
   	fileBaseName <- basename( FileName )
   	fileBaseName <- strsplit( fileBaseName, ".rITC" )
   	
   	FileName <- file.path( fileDirName, paste0( fileBaseName, ".rITC" ), fsep = .Platform$file.sep )

   	projectFileName <<- FileName
   	
   	saveProjectMainRoutine( FileName )
   	
   	FileName <- basename( FileName )
  	window$setTitle( paste( "rITC -- ", FileName ) )
   }
   
   dialog$destroy()

}

#==============================================================================

exportResults <- function(widget){
	
	if ( is.null( outputRegressionData ) == FALSE ){
	
		dialog <- gtkFileChooserDialog( title = "Enter a name for the file", 
		          						  parent = window, 
		          						  action = "save", 
		          						  "gtk-cancel", GtkResponseType["cancel"], 
		          						  "gtk-save", GtkResponseType["accept"]
		          						 )
		          						 
		if ( dialog$run() == GtkResponseType["accept"] ) {
		 	FileName <- dialog$getFilename()
		 	
		 	fileDirName <- dirname( FileName )
   			fileBaseName <- basename( FileName )
   			fileBaseName <- strsplit( fileBaseName, ".pdf" )
   	
   			FileName <- file.path( fileDirName, paste0( fileBaseName, ".pdf" ), fsep = .Platform$file.sep )
   			
   			print(FileName)
		 	
		 	pdf( file = FileName, paper = "a4" )
		 		#print(dev.cur())
		 		pdfDevice <- dev.cur()
				
				print(outputLyrics)
				textplot( outputLyrics, halign = "left", valign = "top" )
				
				temp.activeTitrationPlot <- outputRegressionData$activeTitrationPlot
				temp.activeBackgroundPlot <- outputRegressionData$activeBackgroundPlot
				outputRegressionData$activeTitrationPlot <<- "showAll"
				outputRegressionData$activeBackgroundPlot <<- "showAll"
				
				plotRegressionOutput( outputRegressionData, pdfDevice )
				
				outputRegressionData$activeTitrationPlot <<- temp.activeTitrationPlot
				outputRegressionData$activeBackgroundPlot <<- temp.activeBackgroundPlot
				
				plotFinalFigure( dataSets )
				
				#plot.new()
				
				#text(10, labels = createOutputLyrics( outputRegressionData, inputRegressionData$regressionModelID, inputRegressionData$NorM, 250 ) )	
		 	
		 		#print(dev.list())
			dev.off( which = pdfDevice )
		 	
		 }
		 
		 dialog$destroy()
		 
	 }
}

#==============================================================================

exportData <- function(widget){
	print("Export Data Function")
	
	dialog <- gtkFileChooserDialog( title = "Enter a name for the file", 
		          				    parent = window, 
		          				    action = "save", 
		          					"gtk-cancel", GtkResponseType["cancel"], 
		          					"gtk-save", GtkResponseType["accept"]
		          				  )
		          				  
		if ( dialog$run() == GtkResponseType["accept"] ) {
		 	FileName <- dialog$getFilename()
		 	
		 	fileDirName <- dirname( FileName )
   			fileBaseName <- basename( FileName )
   			fileBaseName <- strsplit( fileBaseName, ".pdf" )
   	
   			FileName <- file.path( fileDirName, paste0( fileBaseName, ".pdf" ), fsep = .Platform$file.sep )
   			
   			print(FileName)
   		}
	
}

#==============================================================================

quitProgram <- function(widget){
	window$destroy()
	gtkMainQuit()
}

#==============================================================================