shinyAutoCast <- function(out,diags, weights, outfile){
	require(shiny)

	shinyApp(
		ui = fluidPage(
  			tags$head(
    		includeCSS('/Users/Kostya/Desktop/Git/shinyAutoCast/css/bootstrap.css'),
    		includeScript('/Users/Kostya/Desktop/Git/shinyAutoCast/js/autocast.js')
  			),
  			title = "AutoCast",
  			tags$h2("AutoCast: time-series cross-sectional demographic forecasting"),
			fluidRow(
    			column(4,align="center", actionButton("prevButton", label = "Previous", icon=icon("arrow-left"))),
    			column(4,align="center", actionButton("nextButton", label = "Next",icon("arrow-right"),cursor="move"))
    		),
    		hr(),
  			conditionalPanel(condition = "input.toggleMore == null | input.toggleMore % 2 == 0",
  				fluidRow( 
  				column(3, align="center", sliderInput("tradeoff", "Fit-Smoothness Tradeoff:", min=0, max=1, value=0.5, step=0.01))
      		)),
      		conditionalPanel(condition = "input.toggleMore != null & input.toggleMore % 2 == 1",
      	  		fluidRow(
      	  	    column(3,align="center",numericInput("w_mse", "Weight on fit:", 70)),
    			column(3,align="center",numericInput("w_age", "Weight on age smoothness:", 10)),
    			column(3,align="center",numericInput("w_time", "Weight on time smoothness:", 10)),
    			column(3,align="center",numericInput("w_agetime", "Weight on age/time smoothness:", 10))
    		)),
    		fluidRow(
  				column(3, align="center", actionButton("toggleMore", label = "More details", icon=icon("arrow-down")))
            ), 
      		fluidRow(
      			column(4, align="center",textOutput("currentWeights")),
      			column(4, align="center", uiOutput("dynamicButton")),
      			column(4, align="center", checkboxInput("showHistograms", label = "Show diagnostics", value = TRUE))
      		),
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
			#diags <- calcDiags(out)
			session$selectedWeightList <- list()
			session$selectedWeights <- c()
			rvalues <- reactiveValues(priorWeight=NULL)
			
			### onFlush fxn is run right before Shiny flushes (stores previous value of weights) 
			session$onFlush(once=FALSE, function(){
				isolate({
				rvalues$priorWeight <- getOptim()$weights
				})
			})
			
			
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
     		
			### on toggleMore, set other input (slider or numeric) to the one that was previously selected
 			observe({
 				if(input$toggleMore %% 2 == 0){
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
  				if(paste(round(weight.values,2),collapse="-") %in% input$selectedWeights){
 					return(actionButton("removeButton", label = "Remove",icon("minus")))
 				} else{
 					return(actionButton("selectButton", label = "Select",icon("plus")))
 				}
 			})
 			
 			### define download button
 			### responsive to download button
 			observe({
 				if(input$downloadButton==0){		
 					return()
 				}
   				selectWeights <- input$selectedWeights #isolate later
     			save(selectWeights, file=outfile)
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
 					clickedWeights <- as.numeric(strsplit(input$clickedWeight,"-")[[1]])
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

			### get optimal forecast
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
 				
 				obj.fxn <- apply(diags, 1, function(x) sum(x*weight.values))
				opt <- which.min(obj.fxn)
				sigma.opt <- out$aux$sigma[opt,]

				yhat.df <- melt(out$validation$yhat[[opt]])
				colnames(yhat.df) <- c("age","time","yhat")
				out$yhat <- yhat.df
	
				### dat for plots
				dat <- list()
				dat$weights <- weight.values
				dat$opt <- opt
				dat$diags <- diags
				dat$y <- out$y
				dat$yhat <- out$yhat
				dat$bounds <- out$bounds
				dat$sample.frame <- out$aux$args.yourcast$sample.frame
				dat$validation.years <- as.numeric(out$aux$holdout.years)
				return(dat)
 			})
 			
 			### get current weights
 			output$currentWeights <- renderText({
 				dat <- getOptim()
 				return(paste(round(dat$weights,2),collapse="-"))
 			}) 
 			
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
 				suppressMessages(print(ggplot(data=diags.melt, aes(x=value)) + geom_histogram() + facet_grid(~variable, scales="free_x") + geom_vline(data= diags.opt.melt, aes(xintercept=value), color="red", size=2, alpha=0.5) + theme_bw()))
 			})
		}
	)
}
