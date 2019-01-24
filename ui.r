shinyUI(fluidPage(
  
  shinyjs::useShinyjs(),
  div(headerPanel("CTD Data Validation"), class = "header"),
    fluidRow(
      column(6,
             fileInput('file1', 'Choose .CNV File of Raw CTD Data',
                       accept=c('text/csv', 
                                'text/comma-separated-values,text/plain', 
                                '.csv',
                                '.cnv'))
             ),
      column(6,
             p(),
             actionButton("exportData", "Export Data")
            )
    ),
    hr(),
    # setting up tabs and their contents, 'Overview' is actually a blank tab
    uiOutput('mytabs'),
    
   bsModal(id = "exportWindow", title = "Ready to Save Data File", trigger = "exportData", size = "large", 
            h1("hello"),
            downloadButton("downloadData", "Save Data"),
            div(
            tabsetPanel(id = "exportTabs",
                        tabPanel("Binned Values",
                                 DT::dataTableOutput("binnedDataTable")
                                 ),
                        tabPanel("Kept Values",
                                 dataTableOutput("keptDataTable")
                        ),
                        tabPanel("Discarded Values",
                                 dataTableOutput("excludedDataTable")
                                 )
                       )
            )
           ),
    
    
# ----- Javascripts ----- 
    tags$script(
      '$(document).on("keypress", 
                      function (e) { 
                        if(e.which == 0){
                          e.preventDefault();
                        }
                      });

      $(document).on("keydown", 
                      function (e) { 
                        if(e.which == 0){
                          e.preventDefault();
                        }
                        Shiny.onInputChange("keyPressed", e.which);
                        console.log("KeyDown")
                      });

      $(document).on("keyup", 
                      function (e) { 
                        if(e.which == 0){
                          e.preventDefault();
                        }
                        Shiny.onInputChange("keyPressed", null);
                        console.log("KeyUp")
                        
                      });'
      ) 

))
