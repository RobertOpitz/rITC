fitBackground <- cmpfun( 
	f = function(tob, data.set.nb, injec.nb){
		
		#injec.nb <- data.set$injec.nb
			
		if (injec.nb == 1){
		   start <- 1
		}else{
		   start <- round( dataSets[[tob]][[data.set.nb]]$injecLength[injec.nb] * dataSets[[tob]][[data.set.nb]]$integrationRange[injec.nb])
		}
	
		end <- dataSets[[tob]][[data.set.nb]]$injecLength[injec.nb]
		
		if (start >= end ) start <- end
		#print(paste(start,end,end-start+1))
		
		if ( (end - start + 1) < 15 ){
			# use constant model
			#print( median( dataSets[[tob]][[data.set.nb]]$injecQMatrix[start:end, injec.nb] ) )
			
			dataSets[[tob]][[data.set.nb]]$bgFitValues[ , injec.nb] <<- c( median( dataSets[[tob]][[data.set.nb]]$injecQMatrix[start:end, injec.nb] ), 0.0, 0.0 )
		}else{
			
			# test constant model with straight line
			x <- dataSets[[tob]][[data.set.nb]]$injecTimeMatrix[start:end, injec.nb]
			y <- dataSets[[tob]][[data.set.nb]]$injecQMatrix[start:end, injec.nb]
	
			bg.fit.const <- rq( y ~ -(x), tau=0.5 )
			bg.fit.lin <- rq( y ~ x, tau=0.5)
					
			if ( anova(bg.fit.const, bg.fit.lin)[1][[1]][4] <= 0.05 ){ #if p-value is smaller or equal to 5 %, reject constant model
				
			  # test straight line with quad. model 
			  xquad <- x^2
			  bg.fit.quad <- rq( y ~ x + xquad, tau=0.5 ) #try( rq( y ~ x + xquad, tau=0.5), silent = TRUE  )
			  
			  if ( anova(bg.fit.lin, bg.fit.quad)[1][[1]][4] <= 0.01 ){ #if p-value is smaller or equal to 1 %, reject straight line model
			  	# use quad. model
			  	dataSets[[tob]][[data.set.nb]]$bgFitValues[ , injec.nb] <<- bg.fit.quad$coefficients
			  }else{
			  	# use straight line model
			  	dataSets[[tob]][[data.set.nb]]$bgFitValues[ , injec.nb] <<- c( bg.fit.lin$coefficients, 0.0 )
			  }
			  		
			}else{
			  # use constant model
			  dataSets[[tob]][[data.set.nb]]$bgFitValues[ , injec.nb] <<- c( bg.fit.const$coefficients, 0.0, 0.0 )
			}
	
		}
	},
	options = list(optimize = 3, supressAll = TRUE)
)