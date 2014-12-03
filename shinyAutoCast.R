shinyAutoCast <- function(out,outfile){
	require(shiny)
	
	if(class(out)=="autocast"){
		out <- list(out)
		isList <- FALSE
		#i <- 1
	} else{
		isList <- TRUE
		forecastNames <- names(out)
	}
		
	shinyApp(
		ui = fluidPage(
  			tags$head(
    		includeCSS('/Users/Kostya/Desktop/Git/shinyAutoCast/css/bootstrap.css'),
    		includeScript('/Users/Kostya/Desktop/Git/shinyAutoCast/js/autocast.js')
  			),
  			title = "AutoCast",
  			tags$h2("AutoCast: time-series cross-sectional demographic forecasting"),
  			fluidRow(
      			column(2,align="center", uiOutput("navPrev")),
      			column(8, align="center", uiOutput("selectForecast")),
      			column(2,align="center", uiOutput("navNext"))
			),	
    		wellPanel(fluidRow(
      			conditionalPanel(condition = "input.toggleMore == null | input.toggleMore % 2 == 0",
      				column(8, align="left", sliderInput("tradeoff", NULL, min=0, max=1, value=0.5, step=0.01))
      			),
      			conditionalPanel(condition = "input.toggleMore != null & input.toggleMore % 2 == 1",
      	  	    	column(2,align="center",div(class="pushDown",numericInput("w_mse", "Weight on fit:", 70))),
    				column(2,align="center",numericInput("w_age", "Weight on age smoothness:", 10)),
    				column(2,align="center",numericInput("w_time", "Weight on time smoothness:", 10)),
    				column(2,align="center",numericInput("w_agetime", "Weight on age/time smoothness:", 10))
    			),
    			column(4, align="left", actionButton("toggleMore", label = "More details", icon=icon("level-down")))
    		),
      		fluidRow(
      			column(4, align="left", uiOutput("dynamicButton")),
      			column(2, align="left", checkboxInput("showHistograms", label = "Show diagnostics", value = TRUE))
      		)),
  			hr(),
			fluidRow(
    			column(6,align="center", plotOutput("agePlot", height="400px")),
    			column(6,align="center", plotOutput("timePlot", height="400px"))
    		),
    		conditionalPanel(condition = "input.showHistograms",
    			fluidRow(column(12, align="center", plotOutput("histograms", height="300px"))
      		)),
    		hr(),
    		fluidRow(column(12,selectizeInput("selectedWeights", "Saved weight combinations:", choices=c(),multiple=TRUE, options=list(create=TRUE)))),
    		fluidRow(column(12,align="center",actionButton("downloadButton", label = "Save as .RData file", icon=icon("download")))),
    		hr()
			
 		),
		server = function(input, output, session) {
			session$selectedWeightList <- list()
			session$selectedWeights <- c()
			rvalues <- reactiveValues(priorWeight=NULL)
			
			############## NAVIGATION ################
			reactivePosition <- reactiveValues(i=1)
			
			### render UI for prev / next buttons
			output$navPrev <- renderUI({
				if(isList){
					return(actionButton("prevButton", label = "Previous", icon=icon("arrow-left")))
				}
				else{
					return(invisible())
				}
			})
			
			output$navNext <- renderUI({
				if(isList){
					return(actionButton("nextButton", label = "Next",icon("arrow-right")))
				}
				else{
					return(invisible())
				}
			})
			
			observe({
				if(is.null(input$prevButton)){
					return()
				}
				if(input$prevButton==0){		
					return()
				}
				isolate({
		    			reactivePosition$i <- reactivePosition$i-1
		    			updateSelectInput(session, "selectForecast", selected = forecastNames[reactivePosition$i])
		    		})
		    		
			})
			
			observe({
				if(is.null(input$nextButton)){
					return()
				}
				if(input$nextButton==0){		
					return()
				}
				isolate({
		    			reactivePosition$i <- reactivePosition$i+1
		    			updateSelectInput(session, "selectForecast", selected = forecastNames[reactivePosition$i])
		    		})
		    		
			})
			
			### render UI for select
			output$selectForecast <- renderUI({
				if(isList){
					return(selectInput("selectForecast", label = NULL,choices= forecastNames, selected= forecastNames[1]))
				}
				else{
					return(invisible())
				}
			})
			
			### when change forecast from dropdown menu, change i
			observe({
				if(is.null(input$selectForecast)){
					return()
				} else{
					current.i<- which(forecastNames %in% input$selectForecast)
				 	isolate({
				 		if(reactivePosition$i == current.i){
				 			return()
				 		} else{
				 			reactivePosition$i <- current.i
				 		}
				 	})	
				}
			})
			
			### if i changes, send message to enable or disable buttons
		  	observe({
		  		# add reactivity to prevButton so that it is disabled at startup; otherwise
		  		# message is sent before button is rendered
		  		input$prevButton
		  		
  				if(reactivePosition$i==1){
  					prevDisable = TRUE
  					nextDisable = FALSE
  				}
  				else if(reactivePosition$i==length(forecastNames)){
   					prevDisable = FALSE
  					nextDisable = TRUE					
  				} else{
  					prevDisable = FALSE
  					nextDisable = FALSE			
  				}
     			session$sendCustomMessage(
       				type = "disableNav", 
       				message = list(prevDisable = prevDisable, nextDisable = nextDisable)
     			)			
     		})	
						
			############## ON FLUSH (STORE WEIGHTS) ################
				
			### onFlush fxn is run right before Shiny flushes (stores previous value of weights)
			### need this in order to properly update weight input widgets when toggle between slider
			### and input boxes (needs to remember last entered weight)
			session$onFlush(once=FALSE, function(){
				isolate({
				rvalues$priorWeight <- getOptim()$weights
				})
			})
			
			
			############## WEIGHT SELECTION ################
			
   			### send message to javascript to toggle between more / fewer details button
 			### responsive to toggleMore button
 			observe({
 				session$sendCustomMessage(
       			type = "updateDetailToggle", 
       			message = list(
         			name = "toggleMore",
         			val = as.numeric(input$toggleMore)) # need as.numeric otherwise formatC throws warning
     			)
     		})
     		
  			observe({
  				if(!is.null(input$tradeoff)){
 				session$sendCustomMessage(
       			type = "updateSliderLabels", 
       			message = list(
         			val = TRUE) # need as.numeric otherwise formatC throws warning
     			)
     			}
     		})
     		
			### on toggleMore, set other input (slider or numeric) to the one that was previously selected
 			observe({
 				if(input$toggleMore!=0 & input$toggleMore %% 2 == 0){
 					isolate({prevWeights <- rvalues$priorWeight})
 					updateSliderInput(session, "tradeoff",value = (1-prevWeights[1]))
 				}
 				else {
 					isolate({prevWeights <- rvalues$priorWeight})
 		 			updateNumericInput(session, "w_mse",value = prevWeights[1])
		    		updateNumericInput(session, "w_age",value = prevWeights[2])
		    		updateNumericInput(session, "w_time",value = prevWeights[3])
		    		updateNumericInput(session, "w_agetime",value = prevWeights[4])					
 				}
 				
			})
 
 			### toggle between select and remove button (based on whether current weight combination is already selected)
 			### reactive to changing weights, toggling input, and selectedWeights
 			output$dynamicButton <- renderUI({
  				if(input$toggleMore %% 2 == 0){
  					weight.values <- c(1-input$tradeoff,input$tradeoff/3, input$tradeoff/3, input$tradeoff/3)
  				}
  				else {
  					weight.values <- c(input$w_mse, input$w_age, input$w_time, input$w_agetime)
  					weight.values <- weight.values/sum(weight.values)
  				}
  				weight.value.text <- paste(round(weight.values,2),collapse="-")
  				if(weight.value.text %in% input$selectedWeights){
 					return(actionButton("removeButton", label = paste("Remove weights (", weight.value.text, ")", sep=""), icon("minus")))
 				} else{
 					return(actionButton("selectButton", label = paste("Save weights (", weight.value.text, ")", sep=""), icon("plus")))
 				}
 			})
 			
 
 			### define select button
 			### once click on select button, take current weights and send to selectize (responsive to button)
 			observe({
 				if(is.null(input$selectButton)){
 					return()
 				} 
 				if(input$selectButton==0){		
 					return()
 				}
 				session$selectedWeightList[[length(session$selectedWeightList)+1]] <- isolate(getOptim()$weights)
 				session$selectedWeights <- sapply(session$selectedWeightList, function(x)  paste(round(x,2), collapse="-"))
 					
 				updateSelectInput(session, "selectedWeights",
       				choices = session$selectedWeights,
       				selected = session$selectedWeights
     			)
 			})
 
 			### define remove button
 			### once click on select button, take current weights and remove from selectize (responsive to button)
 			observe({
 				if(is.null(input$removeButton)){
 					return()
 				} 
 				if(input$removeButton==0){		
 					return()
 				}
 				session$selectedWeightList <- session$selectedWeightList[!sapply(session$selectedWeightList, function(l) identical(l,isolate(getOptim()$weights)))]
 				session$selectedWeights <- sapply(session$selectedWeightList, function(x)  paste(round(x,2), collapse="-"))
 					
 				updateSelectInput(session, "selectedWeights",
       				choices = session$selectedWeights,
       				selected = session$selectedWeights
     			)
 			})			
 			
 			### once click on selectize item, update weight combination (responsive to clickedWeight from js)
 			observe({
 				if(!is.null(input$clickedWeight)){
 					clickedWeights <- as.numeric(strsplit(input$clickedWeight$weight,"-")[[1]])
					if(isolate(input$toggleMore) %% 2 ==0){
 						updateSliderInput(session, "tradeoff",value = (1-clickedWeights[1]))
 					} else{
 						updateNumericInput(session, "w_mse",value = clickedWeights[1])
 		    				updateNumericInput(session, "w_age",value = clickedWeights[2])
 		    				updateNumericInput(session, "w_time",value = clickedWeights[3])
 		    				updateNumericInput(session, "w_agetime",value = clickedWeights[4])
 					}
 				}				
   			})
   			
   			
   			############## DOWNLOAD BUTTON ################
 			
 			### define download button
 			### responsive to download button
 			observe({
 				if(input$downloadButton==0){		
 					return()
 				}
   				selectWeights <- input$selectedWeights #isolate later
     			save(selectWeights, file=outfile)
 			})
 			
 			

			############## OBJECTIVE FUNCTION CALCULATION ################
			
 			getOptim <- reactive({
 				weight.values.slider <- c(1-input$tradeoff,input$tradeoff/3, input$tradeoff/3, input$tradeoff/3)
 				weight.values.detail <- c(input$w_mse, input$w_age, input$w_time, input$w_agetime)
 				weight.values.detail <- weight.values.detail/sum(weight.values.detail)
 				
 				# isolate toggleMore so that graphs aren't replotted when click "more details"
 				if(isolate(input$toggleMore) %% 2 == 0){
 					weight.values <- weight.values.slider
 				}
 				else {
 					weight.values <- weight.values.detail
 				}
 				
 				autoObject <- out[[reactivePosition$i]]
 				
 				obj.fxn <- apply(autoObject$validation$diags, 1, function(x) sum(x*weight.values))
				opt <- which.min(obj.fxn)
				sigma.opt <- autoObject$aux$sigma[opt,]

				yhat.df <- melt(autoObject$validation$yhat[[opt]])
				colnames(yhat.df) <- c("age","time","yhat")
				autoObject$yhat <- yhat.df
	
				### dat for plots
				dat <- list()
				dat$weights <- weight.values
				dat$opt <- opt
				dat$diags <- autoObject$validation$diags
				dat$y <- autoObject$y
				dat$yhat <- autoObject$yhat
				dat$bounds <- autoObject$bounds
				dat$sample.frame <- autoObject$aux$args.yourcast$sample.frame
				dat$validation.years <- as.numeric(autoObject$aux$holdout.years)
				return(dat)
 			})
 			
 			### get current weights
 			#output$currentWeights <- renderText({
 			#	dat <- getOptim()
 			#	return(paste(round(dat$weights,2),collapse="-"))
 			#}) 
 			
 			############## PLOTS ################
 			
 			### age plot
 	 		output$agePlot <- renderPlot({
				dat <- getOptim()
				print(ageplot(dat$y, dat$yhat, dat$bounds, dat$sample.frame, dat$validation.years, uncertainty=F))
			})
			
			### time plot
			output$timePlot <- renderPlot({
				dat <- getOptim()
				print(timeplot(dat$y, dat$yhat, dat$bounds, dat$sample.frame, dat$validation.years, uncertainty=F))
			})
			
			### histograms of diagnostics 
 			output$histograms <- renderPlot({
 				dat <- getOptim()
 				colnames(dat$diags) <- c("MSE", "Age Arc", "Time Arc", "Age/Time Arc")
 				diags.opt <- dat$diags[dat$opt,]
 				names(diags.opt) <- c("MSE", "Age Arc", "Time Arc", "Age/Time Arc")
 				diags.melt <- melt(dat$diags, id=NULL)
 				diags.opt.melt <- melt(diags.opt, id=NULL)
 				suppressMessages(print(ggplot(data=diags.melt, aes(x=value)) + geom_histogram(position="identity") + facet_grid(~variable, scales="free_x") + geom_vline(data= diags.opt.melt, aes(xintercept=value), color="red", size=2, alpha=0.5) + scale_x_continuous("Value of Diagnostic") + scale_y_continuous("Number of Forecasts") + theme_bw()))
 			})
		}
	)
}
