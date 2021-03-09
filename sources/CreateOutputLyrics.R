createOutputLyrics <- cmpfun(

	f = function( output, modelID, NorM, temperature){
		
		#print("start")
		#print(paste( output ))
		#print("end")
		
		#===Begin: functions======================================================
		
		myRound <- function( input, digits ){
			return( tryCatch( round( input, digits ), error = function(e) NA ) )
		}
		
		#--------------------------------------------------------------------------
		
		mySignif <- function( input, digits = 1 ){
			return( tryCatch( signif( input, digits ), error = function(e) NA ) )
		}
		
		#--------------------------------------------------------------------------
		
		signifRound <- function( value, sd ){
			
			if ( is.na(sd) ) return( mySignif(value,3) )
				
			#---Begin: Functions---------------------------------------------------
			compute_power_of_ten <- function( input_data ){
				
				if ( invalid(input_data) ) return(NA)
				
				#print(paste("input_data ->", input_data))
				if ( input_data >= 1.0 ){
					
					temp <- input_data
					i <- 0
					repeat{
						
						temp <- temp %/% 10
										
						if (temp <= 0) break
						
						i <- i + 1
						
					}
					
				}else{
					
					temp <- input_data
					i <- 0
					repeat{
						
						temp <- temp * 10
						
						#print(paste("temp ->", temp))		
						if ( (temp %/% 10) > 0) break
						
						i <- i - 1
										
					}
					
				}
				
				return(i)
				
			}
			#---End: Functions--------------------------------------------
			
			#print(paste("length(sd) ->", length(sd)))
			potenz_von_sd <- compute_power_of_ten(sd)
			
			ergebnis <- NULL
			for (i in seq(along = value)){
			
				if ( invalid( value[i] ) | invalid( potenz_von_sd ) ){
					ergebnis <- c( ergebnis, NA )
				}else{
					sign_of_value <- sign(value[i])
			
					temp <- abs(value[i])
			
					potenz_von_value <- compute_power_of_ten(temp)
				
					potenz_diff <- potenz_von_value - potenz_von_sd + 1
			
					ergebnis <- c( ergebnis, sign_of_value * signif(temp, digits = potenz_diff) )
				}
		
			}
			
			return(ergebnis)
			
		}
		#===End: functions=====================================================
	
		outputLyrics <- paste( " model -> ", modelID, "\n\n" )
		
		outputLyrics <- paste(	outputLyrics, "Temperature = ", myRound(temperature,2), " degree Celsius\n\n" )
		
		if ( identical(modelID, "one-to-one") ){
		
			out <- output$summary
			#print(out)
			
			#outputLyrics <- paste(	outputLyrics, "Temperature = ", myRound(temperature,2), " °C\n\n" )
			
			Kd.SE <- mySignif( out[1,2], digits = 1 )
			Kd.est <- signifRound( out[1,1], Kd.SE )
			Kd.procSE <- myRound(100.0 * out[1,2]/Kd.est, 1)
			Kd.CI95 <- signifRound( output$Kd.CI95, Kd.SE )
			
			DH.SE <- mySignif( out[2,2], digits = 1 )
			DH.est <- signifRound( out[2,1], DH.SE )
			DH.CI95 <- signifRound( output$DH.CI95, DH.SE )
			
			DH.SE <- 1000.0 * DH.SE
			DH.est <- 1000.0 * DH.est
			DH.CI95 <- 1000.0 * DH.CI95
			
			if (NorM != "0"){
				N.SE <- mySignif( out[4,2], digits = 1 )
				N.est <- signifRound( out[4,1], N.SE )
				N.CI95 <- signifRound( output$N.CI95, N.SE )
				Ninv.SE <- mySignif( out[4,2]/out[4,1]^2, digits = 1 )
				Ninv.est <- signifRound( 1.0/out[4,1], Ninv.SE )
			}
			
			DG.SE <- mySignif( output$DG.sd, digits = 1 )
			DG.est <- signifRound( output$DG, DG.SE )
			DG.CI95 <- signifRound( output$DG.CI, DG.SE )
	
			TDS.sd <- mySignif( output$TDS.sd, digits = 1 )		
			TDS.est <- signifRound( output$TDS, TDS.sd )
			
			if (Kd.est < 1.0){ # nano molar dissociation constante
		
				outputLyrics <- paste( outputLyrics,
									"Kd = ", 1000 * Kd.est, " nM; SE = ", 1000.0 * Kd.SE, " nM; pct. SE = ", Kd.procSE, "%\n",
									"95 % CI Kd : ", 1000.0 * Kd.CI95[1], " to ", 1000.0 * Kd.CI95[2], " nM\n\n"
									)
		
			}else if(Kd.est > 1000.0){ # milli molar dissociation constant
				
				outputLyrics <- paste( outputLyrics,
									"Kd = ", Kd.est/1000.0, " mM; SE = ", Kd.SE/1000.0, " mM; pct. SE = ", Kd.procSE, "%\n",
									"95 % CI Kd : ", Kd.CI95[1]/1000.0, " to ", Kd.CI95[2]/1000.0, " mM\n\n"
									)
				
			}else{ # micro molar dissociation constant
				
				outputLyrics <- paste( outputLyrics,
									"Kd = ", Kd.est, "muM; SE = ", Kd.SE, "muM; pct. SE = ", Kd.procSE, "%\n",
									"95 % CI Kd : ", Kd.CI95[1], " to ", Kd.CI95[2], " muM\n\n"
									)
				
			}
									
			outputLyrics <- paste( outputLyrics,
									"DG = ", DG.est, " kJ/mol; SE = ", DG.SE, " kJ/mol\n",
									"95 % CI DG : ", DG.CI95[1], " to ", DG.CI95[2], " kJ/mol\n\n",
									"DH = ", DH.est, " kJ/mol; SE = ", DH.SE, " kJ/mol\n",
									"95 % CI DH : ", DH.CI95[1], " to ", DH.CI95[2], " kJ/mol\n\n",
									"-T*DS = ", TDS.est, " kJ/mol; SE = ", TDS.sd, " kJ/mol\n\n"#,
					)
					
			if (NorM == "N") {
			  outputLyrics <- paste( outputLyrics, paste("N  = ", N.est, "; SE = ", N.SE, "\n",
														"95 % CI N : ", N.CI95[1], " to ", N.CI95[2], "\n"),
														"[ M = ",  Ninv.est, "; SE = ", Ninv.SE, " ] \n\n"
											   )
			}else if (NorM == "M"){
			  outputLyrics <- paste( outputLyrics, paste("M  = ", N.est, "; SE = ", N.SE, "\n",
														"95 % CI M : ", N.CI95[1], " to ", N.CI95[2], "\n"),
														"[ N = ",  Ninv.est, "; SE = ", Ninv.SE, " ] \n\n"
											   )				
			}
														
			outputLyrics <- paste( outputLyrics, paste("c = ", myRound(output$c.value,3) ), "\n\n" )#, "; SE = ", myRound(output$c.value.sd,3), "\n\n") )
			
			if ( is.na(output$lackOfFit$FValue) == FALSE ){
			  outputLyrics <- paste( outputLyrics, paste("lack-of-Fit: F = ", myRound( output$lackOfFit$FValue, 3 ), "; P =", myRound( output$lackOfFit$PValue, 3) ))
			}
		
		}else if( identical(modelID, "competition") ){
				
			Kd.PB.SE <- mySignif( output$Kd.PB.SD, digits = 1 )
			Kd.PB.est <- signifRound( output$Kd.PB.est, Kd.PB.SE ) #out[1,1]#out$coef[1,1]
			Kd.PB.procSE <-  myRound( 100.0 * output$Kd.PB.SD / Kd.PB.est ) #myRound(100.0 * out[1,2]/Kd.est, 1)
			Kd.PB.CI95 <- signifRound( output$Kd.PB.CI95, Kd.PB.SE )
			
			DH.PB.SE <- mySignif( output$DH.PB.SD, digits = 1 ) #signif( out[2,2], digits = 1 )
			DH.PB.est <- signifRound( output$DH.PB.est, DH.PB.SE ) #1000.0 * out[2,1]
			DH.PB.CI95 <- signifRound( output$DH.PB.CI95, DH.PB.SE )
			
			if (NorM != "0"){
				N.est <- output$N.est #out[4,1]
				N.SE <- output$N.SD #out[4,2]	
			}
			
			DG.PB.SE <- mySignif( output$DG.PB.SD, digits = 1 ) #signif( output$DG.sd, digits = 1 )
			DG.PB.est <- signifRound( output$DG.PB.est, DG.PB.SE )
			
			TDS.PB.SE <- mySignif( output$TDS.PB.SD, digits = 1 ) #signif( output$TDS.sd, digits = 1 )
			TDS.PB.est <- signifRound( output$TDS.PB.est, TDS.PB.SE ) #output$TDS
			#print(TDS.PB.est)
			
			if ( invalid(Kd.PB.est) == FALSE ){
				if (Kd.PB.est < 1.0){ # nano molar dissociation constante
			
					outputLyrics <- paste( outputLyrics,
										"Kd.PB = ", 1000.0 * Kd.PB.est, " nM; SE.PB = ", 1000.0 * Kd.PB.SE, " nM; pct. SE.PB = ", Kd.PB.procSE, "%\n",
										"95 % CI Kd.PB : ", 1000.0 * Kd.PB.CI95[1], " to ", 1000.0 * Kd.PB.CI95[2], " nM\n\n"
										)
			
				}else if(Kd.PB.est > 1000.0){ # milli molar dissociation constant
					
					outputLyrics <- paste( outputLyrics,
										"Kd.PB = ", Kd.PB.est/1000.0, " mM; SE.PB = ", Kd.PB.SE/1000.0, " mM; pct. SE.PB = ", Kd.PB.procSE, "%\n",
										"95 % CI Kd.PB : ", Kd.PB.CI95[1]/1000.0, " to ", Kd.PB.CI95[2]/1000.0, " mM\n\n"
										)
					
				}else{ # micro molar dissociation constant
					
					outputLyrics <- paste( outputLyrics,
										"Kd.PB = ", Kd.PB.est, " µM; SE.PB = ", Kd.PB.SE, " µM; pct. SE.PB = ", Kd.PB.procSE, "%\n",
										"95 % CI Kd.PB : ", Kd.PB.CI95[1], " to ", Kd.PB.CI95[2], " µM\n\n"
										)
					
				}
			}else{
				outputLyrics <- paste( outputLyrics, "Kd = NA" )
			}
									
			outputLyrics <- paste( outputLyrics,
									"DG = ", DG.PB.est, " kJ/mol; SE = ", DG.PB.SE, " kJ/mol\n",
									"95 % CI DG : ", output$DG.PB.CI[1], " to ", output$DG.PB.CI[2], " kJ/mol\n\n",
									"DH = ", DH.PB.est, " kJ/mol; SE = ", DH.PB.SE, " kJ/mol\n",
									"95 % CI DH : ", DH.PB.CI95[1], " to ", DH.PB.CI95[2], " kJ/mol\n\n",
									"-T*DS = ", TDS.PB.est, " kJ/mol; SE = ", TDS.PB.SE, " kJ/mol\n\n"
					)
					
			if (NorM == "N") {
			  outputLyrics <- paste( outputLyrics, paste("N  = ", myRound(N.est,3), "; SE = ", myRound(N.SE,4), "\n",
														"95 % CI N : ", myRound(output$N.CI95[1],2), " to ", myRound(output$N.CI95[2],2), "\n"),
														"[ M = ",  myRound(1.0/N.est, 3), "; SE = ", myRound(N.SE/N.est^2, 4), " ] \n\n"
											   )
			}else if (NorM == "M"){
			  outputLyrics <- paste( outputLyrics, paste("M  = ", myRound(N.est,3), "; SE = ", myRound(N.SE,4), "\n",
														"95 % CI M : ", myRound(output$N.CI95[1],2), " to ", myRound(output$N.CI95[2],2), "\n"),
														"[ N = ",  myRound(1.0/N.est,3), "; SE = ", myRound(N.SE/N.est^2,4), " ] \n\n"
											   )				
			}
														
			outputLyrics <- paste( outputLyrics, paste("c = ", myRound(output$c.value,3) ), "\n\n" )#, "; SE = ", myRound(output$c.value.sd,3), "\n\n") )
			
			if ( is.na(output$lackOfFit$FValue) == FALSE ){
			  outputLyrics <- paste( outputLyrics, paste("lack-of-Fit: F = ", myRound( output$lackOfFit$FValue, 3 ), "; P =", myRound( output$lackOfFit$PValue, 3) ))
			}
	
		}else{
			
			outputLyrics <- paste( "Model '", modelID, "' has no defined output." )
			
		}
		  	
		#print(outputLyrics)
		return(outputLyrics)
	},
	options = list(optimze = 3, supressAll = TRUE)
) 
	
