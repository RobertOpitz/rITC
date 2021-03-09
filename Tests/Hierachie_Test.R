model <- gtkTreeStore("gchararray")

by(Cars93, Cars93$Manufacturer, 
	function(DF){
 		parent_iter <- model$append()
 		model$setValue( parent_iter$iter, column = 0, value = DF$Manufacturer[1] )
 		sapply(DF$Model, 
 			function(car_model){
 				child_iter <- model$append(parent=parent_iter$iter)
				if (is.null(child_iter$retval)) model$setValue(child_iter$iter, column=0, value=car_model)
			}
		)
	}
)

iter <- model$getIterFromString("0:0")

print( model$getValue( iter$iter, column=0)$value )

view <- gtkTreeView()
view$insertColumnWithAttributes( 0, "Make", gtkCellRendererText(), text=0 )
view$setModel(model)

window <- gtkWindow()
window$add(view)