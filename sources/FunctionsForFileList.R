# functions for file list
#===========================================================================

create.model <- function(){
  # create list store
  model <- gtkListStoreNew("character")
  
  return(model)
}

#===========================================================================

add.item <- function(button, data){  
  # open dialog box
  call.open.dialog(data) 
}

#===========================================================================

remove.item <- function(widget, data){
  checkPtrType(data, "GtkTreeView")
  treeview <- data
  model <- treeview$getModel()
  selection <- treeview$getSelection()

  selected <- selection$getSelected()
  if (selected[[1]])
    {
      iter <- selected$iter
	  
      path <- model$getPath(iter)
      i <- path$getIndices()[[1]]
      model$remove(iter)
      
      # remove selected data set from list data.sets      
      remove.data.set(tob, i+1)
 
      showThermogram()
    }
}

#===========================================================================

add.columns.titration <- function(treeview){
  model <- treeview$getModel()
					       				       
  # file name column
  renderer <- gtkCellRendererTextNew()
  renderer$setData("column", 0)
  treeview$insertColumnWithAttributes(-1, "Titration Files", renderer, text = 0)
}

#===========================================================================

add.columns.background <- function(treeview){
  model <- treeview$getModel()
					       				       
  # file name column
  renderer <- gtkCellRendererTextNew()
  renderer$setData("column", 0)
  treeview$insertColumnWithAttributes(-1, "Background Files", renderer, text = 0)
}

#===========================================================================

get.item.content.titration <- function(widget, data){

  tob <<- 1

  checkPtrType(data, "GtkTreeView")
  treeview <- data
  model <- treeview$getModel()
  selection <- treeview$getSelection()

  selected <- selection$getSelected()
  if (selected[[1]] == TRUE){
  	iter <- selected$iter
	  
    path <- model$getPath(iter)
    i <- path$getIndices()[[1]]
            
    activeDataSet[1] <<- i + 1

    if ( mainNotebook["page"] == 0 ){
    	showThermogram()
	}else if ( mainNotebook["page"] == 1 ){
	     if ( is.null( outputRegressionData ) == FALSE ){
	     	outputRegressionData$activeBackgroundPlot <<- "showAll"
	  		outputRegressionData$activeTitrationPlot <<- i + 1
	  	    plotRegressionOutput( outputRegressionData )
	     }
	}
  }
  selection.bg <- treeview.background$getSelection()
  selected.bg 	<- selection.bg$getSelected()
  if (selected.bg[[1]]){
  	#if (mainNotebook["page"] == 0){
      iter <- selected.bg$iter
      model.bg <- treeview.background$getModel()
      path.bg <- model.bg$getPath(iter)
      gtkTreeSelectionUnselectPath( selection.bg, path.bg )
    #}
  }
}

#===========================================================================

get.item.content.background <- function(widget, data){
	
  tob <<- 2
	
  checkPtrType(data, "GtkTreeView")
  treeview <- data
  model <- treeview$getModel()
  selection <- treeview$getSelection()

  selected <- selection$getSelected()
  if ( selected[[1]] == TRUE ){
      iter <- selected$iter
	  
      path <- model$getPath(iter)
      i <- path$getIndices()[[1]]
            
      activeDataSet[2] <<- i + 1
      
      #print(mainNotebook["page"])
      
      if (mainNotebook["page"] == 0){
      	showThermogram()
	  }else if ( mainNotebook["page"] == 1 ){
	     if ( is.null( outputRegressionData ) == FALSE ){
      		#print(i+1)
      		outputRegressionData$activeTitrationPlot <<- "showAll"
      		outputRegressionData$activeBackgroundPlot <<- i + 1
      		plotRegressionOutput( outputRegressionData )
      	 }
      }
      #plot.func(data.sets[[2]][[data.set.nb.tob[2]]])

  }
  selection.tit <- treeview.titration$getSelection()
  selected.tit 	<- selection.tit$getSelected()
  if (selected.tit[[1]]){
    iter <- selected.tit$iter
    model.tit <- treeview.titration$getModel()
    path.tit <- model.tit$getPath(iter)
    gtkTreeSelectionUnselectPath( selection.tit, path.tit )
  }
}

#===========================================================================