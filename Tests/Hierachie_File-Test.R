model <- gtkTreeStore("gchararray")

addNewProject <- function(model, projectName){
	parent_iter <- model$append()
	model$setValue( parent_iter$iter, column=0, value = projectName )

	child_tit_iter <- model$append( parent=parent_iter$iter )
	model$setValue( child_tit_iter$iter, column=0, value="Titration" )

	child_bak_iter <- model$append( parent=parent_iter$iter )
	model$setValue( child_bak_iter$iter, column=0, value="Background" )

	grandchild_A_iter <- model$append( parent=child_tit_iter$iter )
	model$setValue( grandchild_A_iter$iter, column=0, value="FileA" )
	grandchild_A_iter <- model$append( parent=child_tit_iter$iter )
	model$setValue( grandchild_A_iter$iter, column=0, value="FileB" )
}

addNewProject(model, "Project 1")
addNewProject(model, "Project 2")

view <- gtkTreeView()
view$insertColumnWithAttributes( 0, "Projects", gtkCellRendererText(), text=0 )
view$setModel(model)

window <- gtkWindow()
window$add(view)