#===Begin: Load packages=================================================================
load_library <- function( lib_name ){
	libTest <- require( lib_name, character.only = TRUE )
	if (libTest == FALSE){
		install.packages( as.name(lib_name) )
		libTest <- require( lib_name, character.only = TRUE )
		if (libTest == FALSE) stop( paste( "ERROR: Could not load package ", lib_name ) )
	}
}

load_library("cairoDevice")
load_library("RGtk2")
load_library("SparseM")
load_library("zoo")
load_library("quantreg")
load_library("compiler")
load_library("sandwich")
load_library("lmtest")
load_library("minpack.lm")
load_library("gdata")
load_library("gplots")
load_library("gtools")
#===End: Load packages===================================================================

# suppress warnings
options( warn = -1 )#, verbose = TRUE )

# blue print for titration/background data set
newDataSetEntry <- list( 
					fileNameShort <- NULL,
					nbOfInjections <- NULL,
					maxInjecNb <- NULL,
					injecNb <- NULL,
					integrationRange <- NULL,
					viewportRange <- NULL,
					injecLength <- NULL,
					bgFitValues <- NULL,
					injecTimeMatrix <- NULL,
					injecQMatrix <- NULL,
					integratedHeats <- NULL,
					temperatureCourse <- NULL,
					meanTemperature <- 0.0,
					dViValues <- NULL,
					filterPeriod <- NULL,
					CellVolume <- NA, # in µL
					CellConc <- NA, # in µM
					SyringeConc <- NA # in µM
)

# structure for loaded titration and background data
dataSets <- list( titration <- list(), background <- list() )#, outputRegression <- NULL )

# pointer to titration (1) or background (2) data sets
tob <- 1

# pointer to loaded titration (first entry) and background (second entry) files
activeDataSet <- c(0, 0)

# blue print of input data for regression
inputRegressionData <- list( 	subtractBackground = FALSE, 
								NorM = "N", 
								regressionModelID = "one-to-one", 
								dataForCompetition = list( 	Kd.PA = NA,
		  													Kd.PA.SD = NA,
		  													DH.PA = NA,
		  													DH.PA.SD = NA,
		  													Bmax = NA	
		  												  ) 
							)

# holds regression output data								
outputRegressionData <- NULL

# holds output lyrics
outputLyrics <- NULL

# holds file name of project
projectFileName <- NULL

#===Begin: load functions===
#PATH <- paste( dirname( sys.frame(1)$ofile ), "/sources", sep = "" )
PATH <- paste( getwd(), "/sources", sep = "" )
print(PATH)

source( paste( PATH, "/ReadInputFile.R", sep = "" ) )
source( paste( PATH, "/FitBackground.R", sep = "" ) )
source( paste( PATH, "/PlotInjection.R", sep = "" ) )
source( paste( PATH, "/FunctionsForBaselinecorrectionTab.R", sep = "" ) )
source( paste( PATH, "/FunctionsForRegressionTab.R", sep = "" ) )
source( paste( PATH, "/IntegrateHeats.R", sep = "" ) )
source( paste( PATH, "/RegressionOneToOneModel.R", sep = "" ) )
source( paste( PATH, "/RegressionDisplacementModel.R", sep = "" ) )
source( paste( PATH, "/CreateOutputLyrics.R", sep = "" ) )
source( paste( PATH, "/PlotRegressionResults.R", sep = "" ) )
source( paste( PATH, "/FunctionsForMenuAndToolbar.R", sep = "" ) )
source( paste( PATH, "/FunctionsForFileList.R", sep = "" ) )
source( paste( PATH, "/PlotFinalFigure.R", sep = "" ) )
#===End: load functions===

#===============================================================================================

remove.data.set <- function(tobLocal, dataSetNbLocal){

   # remove dataSets[[tob.local]][[data.set.nb.local]]
   dataSets.temp <- NULL
   for (i in seq(along = dataSets[[tobLocal]]) ){
   	 if (i != dataSetNbLocal){
       dataSets.temp <- c( dataSets.temp, list(dataSets[[tobLocal]][[i]]) )
     }
   }
   if (length(dataSets.temp) > 0)
      dataSets[[tobLocal]] <<- dataSets.temp
   else
   	  dataSets[[tobLocal]] <<- list()
   
   # set pointer to last entry of dataSets[[tobLocal]]
   activeDataSet[tobLocal] <<- length( dataSets[[tobLocal]] )

   # set titration or background (tob) pointer, if dataSets[[tob.local]]
   # is empty, but the other titration or background data set is still avaible   
   if ( length( dataSets[[tobLocal]] ) <= 0 ){
      if (tobLocal == 1 & length(dataSets[[2]]) > 0 ){
         tob <<- 2
      }else{
      	 tob <<- 1
      }
   }
}

#===============================================================================================

#===Begin: file List zusammenstellen===
# vertikale Subbox fuer input file list
vboxMainFileList <- gtkVBox(FALSE, 5)

	#===Begin: file list titration data===
	vbox.file.list.titration <- gtkVBox(FALSE, 5)
	
	#vboxMainFileListvboxMainFileList$packStart(gtkLabelNew("File List"), FALSE, FALSE, 0)
	
	# create scrollable window
	sw.file.list.titration <- gtkScrolledWindowNew(NULL, NULL)
	sw.file.list.titration$setShadowType("etched-in")
	sw.file.list.titration$setPolicy("automatic", "automatic")
	vbox.file.list.titration$packStart(sw.file.list.titration, TRUE, TRUE, 5)
	
		#===Begin: create file list===
		# create model
		modelTitration <- create.model()
		
		# create tree view
		treeview.titration <- gtkTreeViewNewWithModel(modelTitration)
		
		treeview.titration$setRulesHint(TRUE)
		treeview.titration$getSelection()$setMode("single")
		
		# connect selection and reading of file name
		selection.titration <- treeview.titration$getSelection()
		gSignalConnect(selection.titration, "changed", get.item.content.titration, treeview.titration)
		
		add.columns.titration(treeview.titration)
		
		sw.file.list.titration$add(treeview.titration)
		#===End: create file list===
	
		#===Begin: Add add and remove buttons===
		hbox.file.list.titration.buttons <- gtkHBoxNew(TRUE, 0)
		vbox.file.list.titration$packStart(hbox.file.list.titration.buttons, FALSE, FALSE, 10)
		
		button.file.list.add.titration <- gtkButtonNewWithLabel("Add titration file")
		gSignalConnect( button.file.list.add.titration, "clicked", add.item, "titration" )
		hbox.file.list.titration.buttons$packStart(button.file.list.add.titration, TRUE, TRUE, 0)
		
		buttonFileListRemoveTitration <- gtkButtonNewWithLabel("Remove titration file")
		gSignalConnect( buttonFileListRemoveTitration, "clicked", remove.item, treeview.titration )
		hbox.file.list.titration.buttons$packStart( buttonFileListRemoveTitration, TRUE, TRUE, 0 )
		#===End: Add add and remove buttons===
		
	#===End: file list titration data===
	
	#===Begin: file list background data===
	vbox.file.list.background <- gtkVBox(FALSE, 5)
		
	# create scrollable window
	sw.file.list.background <- gtkScrolledWindowNew(NULL, NULL)
	sw.file.list.background$setShadowType("etched-in")
	sw.file.list.background$setPolicy("automatic", "automatic")
	vbox.file.list.background$packStart(sw.file.list.background, TRUE, TRUE, 5)
	
		#===Begin: create file list===
		# create model
		modelBackground <- create.model()
		
		# create tree view
		treeview.background <- gtkTreeViewNewWithModel(modelBackground)
		
		treeview.background$setRulesHint(TRUE)
		treeview.background$getSelection()$setMode("single")
		
		# connect selection and reading of file name
		selection.background <- treeview.background$getSelection()
		gSignalConnect(selection.background, "changed", get.item.content.background, treeview.background)
		
		add.columns.background(treeview.background)
		
		sw.file.list.background$add(treeview.background)
		#===End: create file list===
	
		#===Begin: Add add and remove buttons===
		hbox.file.list.background.buttons <- gtkHBoxNew(TRUE, 0)
		vbox.file.list.background$packStart(hbox.file.list.background.buttons, FALSE, FALSE, 10)
		
		button.file.list.add.background <- gtkButtonNewWithLabel("Add background file")
		gSignalConnect( button.file.list.add.background, "clicked", add.item, "background" )
		hbox.file.list.background.buttons$packStart(button.file.list.add.background, TRUE, TRUE, 0)
		
		buttonFileListRemoveBackground <- gtkButtonNewWithLabel("Remove background file")
		gSignalConnect( buttonFileListRemoveBackground, "clicked", remove.item, treeview.background )
		hbox.file.list.background.buttons$packStart( buttonFileListRemoveBackground, TRUE, TRUE, 0 )
		#===End: Add add and remove buttons===
		
	#===End: file list titration data===
	
# pack vbox.file.list.titration and vbox.file.list.background in vboxMainFileList
vboxMainFileList$packStart(vbox.file.list.titration, TRUE, TRUE, 0)
vboxMainFileList$packStart(vbox.file.list.background, TRUE, TRUE, 0)
#===End: file List zusammenstellen===


#===============================================================================================s

# note books
mainNotebook <- gtkNotebook()

# set Tab to "baseline correction" of mainNotebook
mainNotebook["page"] <- 0

mainNotebookActions <- function( mainNotebook, page, page.num ){
	mainNotebookPointer <<- mainNotebook["page"]
	if ( mainNotebook["page"] == 1 ){
		if ( length(dataSets[[1]]) > 0 )
			showThermogram()
	}else if ( mainNotebook["page"] == 2 & is.null(outputRegressionData) == FALSE ){
		#if ( is.null(outputRegressionData) == FALSE )
			plotRegressionOutput( outputRegressionData )			
	}
}
gSignalConnect(mainNotebook, "switch-page", mainNotebookActions)

#===============================================================================================

#===Begin: Baseline Tab===
vbox.BL.corr <- gtkVBox()
		
# horizontale box for previous and next buttons
hbox.BL.corr.next_previous <- gtkHBox()

#buttonFirstInjection <- gtkButton("|< first Injection")
#hbox.BL.corr.next_previous$packStart( buttonFirstInjection, fill = TRUE, padding = 0 )

buttonPreviousInjection <- gtkButton("< previous Injection")
gSignalConnect( buttonPreviousInjection, "clicked", previuosInjection )
hbox.BL.corr.next_previous$packStart( buttonPreviousInjection, fill = TRUE, padding = 0 )

buttonNextInjection <- gtkButton("next Injection >")
gSignalConnect( buttonNextInjection, "clicked", nextInjection )
hbox.BL.corr.next_previous$packStart( buttonNextInjection, fill = TRUE, padding = 0 )

#buttonLastInjection <- gtkButton("last Injection >|")
#hbox.BL.corr.next_previous$packStart( buttonLastInjection, fill = TRUE, padding = 0 )

# horizontale box for thermogram, integration and temperature course buttons
hbox.thergr.int.temp <- gtkHBox()

buttonShowThermogram <- gtkButton("Thermogram")
gSignalConnect( buttonShowThermogram, "clicked", showThermogram )
hbox.thergr.int.temp$packStart( buttonShowThermogram, fill = TRUE, padding = 0 )

buttonShowIntegratedHeats <- gtkButton("Integrated Heats")
gSignalConnect( buttonShowIntegratedHeats, "clicked", showIntegratedHeats )
hbox.thergr.int.temp$packStart( buttonShowIntegratedHeats, fill = TRUE, padding = 0 )

buttonShowTemperatureCourse <- gtkButton("Temperature Course")
gSignalConnect( buttonShowTemperatureCourse, "clicked", showTemperatureCourse )
hbox.thergr.int.temp$packStart( buttonShowTemperatureCourse, fill = TRUE, padding = 0 )
		
# Graphik in vbox packen
# Graphikbereich erzeugen
graphics <- gtkDrawingAreaNew()

# R soll Graphik in den Fensterbereich zeichen
asCairoDevice(graphics)
#plot1.dev <- as.integer(dev.next())#dev.cur())

#===Begin: HBox for Graphics and ViewPort slider===
graphAndViewport <- gtkHBox()
graphAndViewport$packStart(graphics, expand = TRUE, fill = TRUE, padding = 5)

sliderViewportRange <- gtkVScale(min = -10.0, max = 10.0, step = 0.1)
gSignalConnect( sliderViewportRange, "value-changed", setViewportRange )
graphAndViewport$packStart( sliderViewportRange, expand = FALSE, fill = FALSE, padding = 0 )
#===End: HBox for Graphics and ViewPort slider===

# Graphik packen
vbox.BL.corr$packStart(graphAndViewport, expand = TRUE, fill = TRUE, padding = 5)

# Baseline Slider for integration range
sliderIntegrationRange <- gtkHScale( min = 0.0, max = 0.99, step = 0.01 )
gSignalConnect( sliderIntegrationRange, "value-changed", setIntegrationRange )
vbox.BL.corr$packStart( sliderIntegrationRange, expand = FALSE, fill = FALSE, padding = 0 )

# previous/next-button-box in Baseline-correction-box
vbox.BL.corr$packStart(hbox.BL.corr.next_previous, expand = FALSE, fill = FALSE, padding = 10)

# thermogram/integration/temperature.course-box in Baseline-correction-box
vbox.BL.corr$packStart(hbox.thergr.int.temp, expand = FALSE, fill = FALSE, padding = 10)

# add new page to main notebook
mainNotebook$appendPage(vbox.BL.corr, gtkLabel("Baseline correction") )
#===End: Baseline Tab===

#===Begin: Regression Tab===
vboxRegression <- gtkVBox(FALSE)
gtkWidgetSetSizeRequest(vboxRegression, 350, -1)
	
	#===Begin: scrolled window for titration regression output===
	viewRegressionResults <- gtkTextView()
	viewRegressionResults["editable"] <- FALSE
	viewRegressionResults["cursor-visible"] <- FALSE
	#pangoFontDescriptionSetAbsoluteSize(  )
	scrolled.window <- gtkScrolledWindow()
	scrolled.window$add(viewRegressionResults)
	scrolled.window$setPolicy("automatic", "automatic")
	#===End: scrolled window for titration regression output===
	
	#===Begin: Subtract Background Option===
	CheckButtonBackgroundSubtraction <- gtkCheckButton("Subtract background")	
	gSignalConnect( CheckButtonBackgroundSubtraction, "toggled", setSubtractionVariable )
	#===End: Subtract Background Option===
	
	#===Begin: Set Regression Params Button===
	buttonRegressionParams <- gtkButtonNewWithLabel("Conc. & Cell volume")
	gSignalConnect( buttonRegressionParams, "clicked", setRegressionParams )
	#===End: Set Regression Params Button===
	
	#===Begin: Selection M/N/0===
	frameForCorrectionFactorSelection <- gtkFrame("Correction Factor")
	selectNorM.box <- gtkHBox()
	selectNorM <- list()
	selectNorM$N <- gtkRadioButton(label = "for Analyte (N)")
	selectNorM$M <- gtkRadioButton(selectNorM, label = "for Titrant (M)")
	selectNorM$O <- gtkRadioButton(selectNorM, label = "none")
	sapply( selectNorM, selectNorM.box$packStart )	
	sapply( selectNorM, gSignalConnect, "toggled", setSelectionNorM )
	frameForCorrectionFactorSelection$add( selectNorM.box )
	#===End: Selection M/N/0===
	
	#===Begin: Regression model===
	# Combo Box for regresion model
	hboxSelectModel <- gtkHBox()
			
	hboxSelectModel$packStart( gtkLabel("Regression Model:") )
	comboRegressionModels <- gtkComboBoxNewText()
	sapply( c("one-to-one", "competition"), comboRegressionModels$appendText )
	comboRegressionModels["active"] <- 0		
	gSignalConnect( comboRegressionModels, "changed", setModelSelection )
	hboxSelectModel$packStart( comboRegressionModels )
	#===End: Regression model===
	
	#===Begin: Regression Button===
	buttonRegression <- gtkButtonNewWithLabel("Run Regression")
	gSignalConnect( buttonRegression, "clicked", runRegression, viewRegressionResults )
	#===End: Regression Button===
	
	#===Begin: pack window "titration regression" for subNotebookRegression Tab===
	# main hbox for regression tab
	hbox.main.regression <- gtkHBox()
		
	# Graphikbereich erzeugen
	graphicsB <- gtkDrawingAreaNew()
		
	# R soll Graphik in den Fensterbereich zeichen
	asCairoDevice(graphicsB)
		
	# pack drawing area to hbox.main.regression
	hbox.main.regression$packStart(graphicsB, expand = TRUE, fill = TRUE, padding = 5)
		
	# pack "Conc. & Vol." button and "Subtract Background" checkbox in one hbox
	hBoxSubRegression <- gtkHBox(spacing = 10)
	hBoxSubRegression$packStart( CheckButtonBackgroundSubtraction, expand = FALSE, fill = FALSE )
	hBoxSubRegression$packStart( buttonRegressionParams, expand = TRUE, fill = TRUE )
		
	# pack regression sub window
	vboxRegression$packStart(scrolled.window, TRUE, TRUE, 10)
	vboxRegression$packStart(hBoxSubRegression, expand = FALSE, fill = FALSE, padding = 10)
	vboxRegression$packStart(frameForCorrectionFactorSelection, FALSE, FALSE, 10)
	vboxRegression$packStart(hboxSelectModel, FALSE, FALSE, 10)
	vboxRegression$packStart(buttonRegression, FALSE, FALSE, 10)
		
	# pack regression sub window to hbox.main.regression
	hbox.main.regression$packStart(vboxRegression, expand = FALSE, fill = FALSE, padding = 5)
	#===Begin: pack window for subNotebookRegression Tab===

mainNotebook$appendPage(hbox.main.regression, gtkLabel("Regression") )
#===End: Regression Tab===


#===Begin: Comments Tab===
vbox_main_comments <- gtkVBox()

	hboxAnalyteEntry <- gtkHBox()
	analyteEntry <- gtkEntry()
	hboxAnalyteEntry$packStart( gtkLabel( "Analyte    :" ), expand = FALSE, padding = 2 )
	hboxAnalyteEntry$packStart( analyteEntry, expand = TRUE )
	
	hboxTitrantEntry <- gtkHBox()
	titrantEntry <- gtkEntry()
	hboxTitrantEntry$packStart( gtkLabel( "Titrant    :" ), expand = FALSE, padding = 2 )
	hboxTitrantEntry$packStart( titrantEntry, expand = TRUE )
	
	hboxSolventEntry <- gtkHBox()
	solventEntry <- gtkEntry()
	hboxSolventEntry$packStart( gtkLabel( "Solvent    :" ), expand = FALSE, padding = 2 )
	hboxSolventEntry$packStart( solventEntry, expand = TRUE )
	
	hboxInstrumentEntry <- gtkHBox()
	instrumentEntry <- gtkEntry()
	hboxInstrumentEntry$packStart( gtkLabel( "Instrument :" ), expand = FALSE, padding = 2 )
	hboxInstrumentEntry$packStart( instrumentEntry, expand = TRUE )
	
	hboxOperatorEntry <- gtkHBox()
	operatorEntry <- gtkEntry()
	hboxOperatorEntry$packStart( gtkLabel( "Operator   :" ), expand = FALSE, padding = 2 )
	hboxOperatorEntry$packStart( operatorEntry, expand = TRUE )
	
	#===Begin: Comments===
	commentFrame <- gtkFrame( "Additional Comments" )
	
	viewComments <- gtkTextView()
	viewComments["editable"] <- TRUE
	viewComments["cursor-visible"] <- TRUE
	#pangoFontDescriptionSetAbsoluteSize(  )
	scrolled_window_comments <- gtkScrolledWindow()
	scrolled_window_comments$add( viewComments )
	scrolled_window_comments$setPolicy("automatic", "automatic")
	
	commentFrame$add( scrolled_window_comments )
	#===End: Comments===

vbox_main_comments$packStart( hboxAnalyteEntry, expand = FALSE, fill = FALSE, padding = 10 )	
vbox_main_comments$packStart( hboxTitrantEntry, expand = FALSE, fill = FALSE, padding = 10 )
vbox_main_comments$packStart( hboxSolventEntry, expand = FALSE, fill = FALSE, padding = 10 )
vbox_main_comments$packStart( hboxInstrumentEntry, expand = FALSE, fill = FALSE, padding = 10 )
vbox_main_comments$packStart( hboxOperatorEntry, expand = FALSE, fill = FALSE, padding = 10 )
vbox_main_comments$packStart( commentFrame, expand = TRUE, fill = TRUE, padding = 10 )

mainNotebook$appendPage( vbox_main_comments, gtkLabel("Comments") )
#===End: Comments Tab===

#===============================================================================================

#===Begin: menu bar===
menubar <- gtkMenuBar()

file_menu <- gtkMenu()
file_item <- gtkMenuItemNewWithMnemonic( label = "_File" )
file_item$setSubmenu( file_menu )

menubar$append( file_item )

new_item <- gtkMenuItemNewWithMnemonic("_New Project")
open_item <- gtkMenuItemNewWithMnemonic("_Open Project")
save_item <- gtkMenuItemNewWithMnemonic("_Save Project")
saveAs_item <- gtkMenuItemNewWithMnemonic("Save Project as ...")
results_item <- gtkMenuItemNewWithMnemonic("_Print Results File")
export_data_item <- gtkMenuItemNewWithMnemonic("_Export Data") 
quit_item <- gtkMenuItemNewWithMnemonic("_Quit rITC") 

gSignalConnect( new_item, "activate", newProject, list( titration = treeview.titration , background = treeview.background ) )
gSignalConnect( open_item, "activate", openProject )
gSignalConnect( save_item, "activate", saveProject )
gSignalConnect( saveAs_item, "activate", saveProjectAs )
gSignalConnect( results_item, "activate", exportResults )
gSignalConnect( export_data_item, "activate", exportData )
gSignalConnect( quit_item, "activate", quitProgram )

file_menu$append( new_item )
file_menu$append( gtkSeparatorMenuItem() )
file_menu$append( open_item )
file_menu$append( save_item )
file_menu$append( saveAs_item )
file_menu$append( gtkSeparatorMenuItem() )
file_menu$append( results_item )
file_menu$append( export_data_item )
file_menu$append( gtkSeparatorMenuItem() )
file_menu$append( quit_item )
#===End: menu bar===

#===Begin: tool bar===
toolbar <- gtkToolbar()

openButton <- gtkToolButton( stock.id = "gtk-open" )
#gtkToolButtonSetLabel( openButton, label = "Open Project" )
openButton$setLabel("Open Project")
saveButton <- gtkToolButton( stock.id = "gtk-save" )
saveButton$setLabel("Save Project")
quitButton <- gtkToolButton( stock.id = "gtk-quit" )
quitButton$setLabel("Quit rITC")

gSignalConnect( openButton, "clicked", openProject )
gSignalConnect( saveButton, "clicked", saveProject )
gSignalConnect( quitButton, "clicked", quitProgram )

toolbar$add(openButton)
toolbar$add(saveButton)
toolbar$add(gtkSeparatorToolItem())
toolbar$add(quitButton)
#===End: tool bar===

# status bar
#statusbar <- gtkStatusbar()

#===============================================================================================

#===Begin: hbox for main notebook and file list===
hbox.main <- gtkHBox()
hbox.main$packStart(vboxMainFileList, FALSE, FALSE, 5)
hbox.main$packStart(mainNotebook)#, TRUE, TRUE, 0)
#===End: hbox for main notebook and file list===

#===Begin: vbox for toolbox and main hbox===
vbox.main <- gtkVBox()
vbox.main$packStart(menubar, FALSE, FALSE, 0)
vbox.main$packStart(toolbar, FALSE, FALSE, 0)
vbox.main$packStart(hbox.main)
#vbox.main$packStart(statusbar, FALSE, FALSE, 5)
#===End: vbox for toolbox and main hbox===

# create main window
window <- gtkWindow("toplevel", show=FALSE)

# Name of main window
window$setTitle("rITC")

# Breite der Borte
window$setBorderWidth(5)

# initiale Fenstergroesse
window$setDefaultSize(1500,900)

# add content
window$add(vbox.main)

# show main window
window$showAll()

gtkMain()

#options(warn = 0)