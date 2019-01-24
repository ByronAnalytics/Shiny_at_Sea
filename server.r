shinyServer(function(input, output, session) {
  values <- reactiveValues(project.data=NULL)
  
  observeEvent(input$file1,{
   #setup tab counter for keyboard interaction/js code
    values$tabCount <- 0    
    
    dataList <- readNSmooth(input$file1$datapath)
    values$myData <- dataList$data
    values$station <- dataList$station
    values$date<- dataList$date
    values$dataOut <- list()
    # Structure of values$obj:
    #   $x
    #   $y
    #   $modeled.y
    #   $y.sd
    #   $toKeep

    # Generate non-reactive list of constants in global environment for plots
    values$metaData <- list(station = dataList$station, 
                      maxDepth = setDepthReq(dataList$station), 
                      defaultParamRange = read.csv("Data/parameter_axis_scales.csv"))
  })
  
                      ####### END DATA LOAD ########
  
  # ------- Build Tabs -------
  output$mytabs <- renderUI({
    validate(need(values$myData, ""))
    
    mytabs <- lapply(seq_along(c("Overview", names(values$myData))),
                    function(i, x){
                      if(i == 1){
                        makeOverviewPlotTabUI(x[i], i, names(values$myData))
                      } else {
                        makePlotUI(x[i], i)
                        }  
                      }, x= c("Overview", names(values$myData)))
    do.call(tabsetPanel, c(mytabs, list(id = "mainTabsetPanel")))
  })
  
  # ----- Populate Tabs ----- 
  
  # interactive plots
  dataOut <- reactive({lapply(names(values$myData), function(x, plotData, metaData){
                          Data <- callModule(makePlot, x, reactive(plotData[x]), metaData)
                          }, plotData = values$myData, metaData = values$metaData)
              })
  
  observe({
    validate(need(newData(), ""))
    callModule(makeOverviewPlotTab, "Overview", dl = reactive(newData()), reactive(values$metaData))
  })
  
  newData <- reactive({
    lapply(dataOut(), function(x) return(x()))
    })

  # ------- Save Data Interface -------
  saveOutBinned <- reactive({
    as.data.frame(formatForOutput(newData(), "binned"))
  })
  saveOutKept <- reactive({
    as.data.frame(formatForOutput(newData(), "keepData"))
  })
  saveOutExcluded <- reactive({
    as.data.frame(formatForOutput(newData(), "excludedData"))
  })

  output$binnedDataTable <- DT::renderDataTable({
    DT::datatable(data = saveOutBinned(), options = list(scrollX = TRUE))
  })

  output$keptDataTable <- DT::renderDataTable({
    DT::datatable(data = saveOutKept(), options = list(scrollX = TRUE))
  })
  
  output$excludedDataTable <- DT::renderDataTable({
    DT::datatable(data = saveOutExcluded(), options = list(scrollX = TRUE))
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(values$station,'_',values$date, '.zip', sep='')
    },
    content = function(file) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      fs <- c(paste(values$station,'_',values$date, '_binned.csv', sep=''), 
              paste(values$station,'_',values$date, '_kept.csv', sep=''),
              paste(values$station,'_',values$date, '_discarded.csv', sep=''))
      write.csv(saveOutBinned(), fs[1], row.names = FALSE)
      write.csv(saveOutKept(), fs[2], row.names = FALSE)
      write.csv(saveOutExcluded(), fs[3], row.names = FALSE)
      
      zip(zipfile=file, files=fs)
    },
    contentType = "application/zip"
  )

# ------- Cycle through Tab panels with tabKey -------
  observeEvent(input$keyPressed, {
    #print(input$keyPressed)
    
    if(input$keyPressed == 9 & !is.null(input$mainTabsetPanel)) {
      ntabs <- length(values$myData) + 1
      activeTab <- as.numeric(input$mainTabsetPanel)
      if(activeTab == ntabs){
        nextTab <- 1
      } else {
        nextTab <- activeTab + 1
      }
      updateTabsetPanel(session, "mainTabsetPanel", selected = as.character(nextTab))
    }

    if(input$keyPressed == 83){
      #Prompt data export
      toggleModal(session, "exportWindow", toggle = "toggle")
    }
  })
  
# ***end***  
})




