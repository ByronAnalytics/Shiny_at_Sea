makePlotUI <- function(id, value){
  ns <- NS(id)
  tabPanel(id, value = value,
           fluidRow(
             column(9,plotOutput(ns("plot"), height = "750px", click = ns("plotClick"), 
                                 brush = brushOpts(id=ns("plotBrush"))),
                    actionButton(ns("excludeSelected"), "Exclude Selected"),
                    actionButton(ns("includeSelected"), "Include Selected"),
                    actionButton(ns("includeAll"), "Clear All Selected"),
                    actionButton(ns("reset"), "Reset")
             ),
             column(3, checkboxInput(ns("checkbox"), label = "Default Range", value = FALSE),
                       HTML("<p>&nbsp;</p><p>&nbsp;</p><p>&nbsp;</p><p>&nbsp;</p><p>&nbsp;</p>"),
                       img(src="keep-discard_2.jpg", height="110px", width="90px"))
           )
           
  )
  
}

makePlot <- function(input, output, session, dataSet, stationMetaData){
  
  vals <- reactiveValues(
    keeprows = NULL,
    dataOut = list("keepData" = NULL,
                   "excludedData" = NULL,
                   "binned" = NULL)
    )
  
  # Initialize keep vector if not yet created
  observe({
    validate(need(dataSet(), "No Data"))
    
    if(is.null(vals$keeprows)) vals$keeprows <- dataSet()[[1]]$toKeep
    if(!is.null(dataSet())){
      vals$dataOut$keepData <- dataSet()[[1]][vals$keeprows, , drop = FALSE]
      vals$dataOut$excludedData <- dataSet()[[1]][!vals$keeprows, , drop = FALSE]
      vals$dataOut$sensor <- names(dataSet())
    }
    
    keep    <- dataSet()[[1]][vals$keeprows, , drop = FALSE]
    vals$dataOut$binned<- CreateBins(keep$x, keep$y, stationMetaData$kMaxDepth)
    })

  output$plot <- renderPlot({
    validate(need(dataSet, "No Data"))
    
    keep    <- dataSet()[[1]][vals$keeprows, , drop = FALSE]
    exclude <- dataSet()[[1]][!vals$keeprows, , drop = FALSE]
    OverviewPlot(keep, exclude, vals$dataOut$binned, stationMetaData, input$checkbox, vals$dataOut$sensor)
  })
  
   # Toggle points that are clicked
  observeEvent(input$plotClick, {
    res <- nearPoints(dataSet()[[1]], input$plotClick, threshold = 4, xvar = "y", yvar = "x", allRows = TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
   })
  
   # Toggle points that are brushed, when button is clicked
   observeEvent(input$includeSelected, {
     res <- brushedPoints(dataSet()[[1]], input$plotBrush, allRows = TRUE)
     vals$keeprows[res$selected_ & !vals$keeprows] <- TRUE
   })
   
   observeEvent(input$excludeSelected, {
     res <- brushedPoints(dataSet()[[1]], input$plotBrush, allRows = TRUE)
     vals$keeprows[res$selected_ & vals$keeprows] <- FALSE
   })

   # Clear all selected points
   observeEvent(input$includeAll, {
     vals$keeprows[vals$keeprows == FALSE] <- TRUE
   })

   # Reset all points
   observeEvent(input$reset, {
     vals$keeprows <- dataSet()[[1]]$toKeep
   })
   
   observeEvent(input$keyPressed,{print(input$keyPressed)})
   
   # For Data Write Out, returns selected points
   return(reactive(vals$dataOut))
}

# -----------------------------------------------------------------------------
makeOverviewPlotTabUI <- function(id, value, sensors){
  ns <- NS(id)
  plotsLeft <- sensors[seq(1,length(sensors), by = 2)]
  plotsRight <- sensors[seq(2,length(sensors), by = 2)]
  
  tabPanel(id, value = value,
           fluidRow(
             column(5,
                    lapply(plotsLeft, function(x) plotDivUI(ns(x)))
                    ),
             column(1,
                    #for (i in 1:ceiling(length(sensors)/2)){ # cant get this loop to work yet
                      HTML("<p>&nbsp;</p><p>&nbsp;</p><p>&nbsp;</p><p>&nbsp;</p><p>&nbsp;</p>"),
                      img(src="keep-discard_2.jpg", height="75px", width="60px")
                    #}
             ),
             column(5,
                    lapply(plotsRight, function(x) plotDivUI(ns(x)))        
                    )
             )
           )
}

makeOverviewPlotTab <- function(input, output, session, dl, stationMetaData){
  vals <- reactiveValues(sensors = NULL)
  
  observe({
    validate(need(dl(),""))
    vals$sensors <- sapply(dl(), function(x) return(x$sensor))
    })
  
  observe({
    validate(need(dl(), ""))
    lapply(seq_along(vals$sensors), function(i, dl){
      callModule(plotDiv, vals$sensors[i], reactive(dl()[[i]]), reactive(stationMetaData()), FALSE)
    }, dl())
  })
}

# -----------------------------------------------------------------------------
plotDivUI <- function(id){
  ns <- NS(id)
  plotOutput(ns("OVplot"))
}

plotDiv <- function(input, output, session, dl, stationMetaData, checkbox){
  output$OVplot <- renderPlot(OverviewPlot(dl()$keepData, dl()$excludedData, 
                                           dl()$binned, stationMetaData(), 
                                           checkbox, dl()$sensor))
}
