##############################################################
##        FJ Tech Toolbox by Josh Luthy                     ##
##############################################################
library(shiny)
library(shinyFiles)
library(shinydashboard)
library(shinyWidgets)
library(flowWorkspace)
library(CytoML)
library(ggplot2)
library(ggiraph)
library(ggcyto)
library(FlowSOM)
library(dplyr)
library(heatmaply)
# library(pheatmap)
library(plotly)
library(viridisLite)
library(DT)
library(ggplot2)
library(cowplot)

hmPalette <- c("magma", "plasma", "inferno", "viridis", "cividis", "rainbow")
paramNames <- c("None", "FlowSOM")

setClass("FJObj", 
         slots = c(
           paths = "character", # Tie this into my Noir package to use this object with FlowJo server.
           selectedDF = "ANY", # create some slots to hold data for later use
           paramNames = "ANY",
           plottingParams = "ANY"
         ),
         prototype = list(
           paths = "paths go here",
           selectedDF = data.frame(),
           paramNames = data.frame(),
           plottingParams = data.frame()
         )
)
# Define UI for application that draws a histogram
ui <- fluidPage(

    
    dashboardPage(skin = "blue",
                  dashboardHeader(title = "FJ R-Toolbox"), # Application title
                  dashboardSidebar(
                      sidebarMenu(
                          menuItem(text = "Data", tabName = "data", icon = icon("table")),
                          menuItem(text = "Plots", tabName = "heatmaps", icon = icon("chart-line"))
                      )
                      
                  ),
                  # ),
                  dashboardBody(
                      
                      tags$head(tags$script('
      // Define function to set height of "map" and "map_container"
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();

        var boxHeight = window_height - header_height - 30;

        $("#Heatmaps").height(boxHeight);
        $("#heatmaps1").height(boxHeight - 20);
        $("#violins").height(boxHeight);
        $("#violins").height(boxHeight - 20);
        $("#Beeswarms").height(boxHeight);
        $("#Beeswarms").height(boxHeight - 20);
      };

      // Set input$box_height when the connection is established
      $(document).on("shiny:connected", function(event) {
        setHeight();
      });

      // Refresh the box height on every window resize event    
      $(window).on("resize", function(){
        setHeight();
      });
    ')),
                      ##################################################
                      # Define UI Layout for DATA TABLE Tab            #
                      ##################################################
                      tabItems(
                          tabItem(tabName = "data",
                                  fluidRow(
                                      box(title = "Select WSP File",
                                          width = 4,
                                          status = "warning",
                                          collapsible = T,
                                          background = "light-blue",
                                          # selectInput("DTParameters",
                                          #             label = "Select the workspace file",
                                          #             choices = file.choose(),
                                          #             multiple = TRUE,
                                          #             selectize = TRUE),
                                          shinyFilesButton(id = "wspFile", 
                                                           label = "Workspace Path", multiple = FALSE,
                                                           title = "Select a FlowJo Workspace File",
                                                           style = "margin-bottom: 10px"
                                                           ),
                                          verbatimTextOutput("wspFileTextPath", placeholder = TRUE)  # added a placeholder
                                      ),
                                      box(title = "Select FCS Data Folder",
                                          width = 4,
                                          status = "warning",
                                          collapsible = T,
                                          background = "light-blue",
                                          # selectInput("DTParameters",
                                          #             label = "Select the workspace file",
                                          #             choices = file.choose(),
                                          #             multiple = TRUE,
                                          #             selectize = TRUE),
                                          shinyDirButton(id = "fcsFolder", 
                                                         label = "Input directory", multiple = FALSE,
                                                         title = "Select a Folder With The FCS Files For This Workspace",
                                                         style = "margin-bottom: 10px"),
                                          verbatimTextOutput("dirFolder", placeholder = TRUE)  # added a placeholder
                                      ),
                                      box(title ="Workspace Data View",
                                          width = 6,
                                          status = "primary", solidHeader = T,
                                          verbatimTextOutput("nodeName"),
                                          DTOutput("table1"),
                                          actionButton("getWSPdata",
                                                       label = "Get WSP Data",
                                                       style = "gradient",
                                                       color = "success",
                                                       size = "sm",
                                                       icon = icon("chart-line"))
                                      ),
                                      box(title ="Workspace Plots View",
                                          width = 6,
                                          status = "primary", solidHeader = T,
                                          plotOutput("gatingH"),
                                          DTOutput("table2"),
                                          actionBttn(
                                              "refreshGSPlot",
                                              label = "Refresh Plot",
                                              style = "gradient",
                                              color = "success",
                                              size = "sm",
                                              icon = icon("chart-line")
                                          ),
                                          actionBttn(
                                            "getPopData",
                                            label = "getPop Data",
                                            style = "gradient",
                                            color = "success",
                                            size = "sm",
                                            icon = icon("chart-line")
                                          )
                                      ),
                                      
                                      
                                  )
                          ),
                          ##################################################
                          # Define UI Layout for HEATMAP Tab             #
                          ##################################################
                          tabItem(tabName = "heatmaps",
                                  fluidRow(
                                    uiOutput("hmParameters"),
                                   
                                    box(title = "Select a Categorical Parameter",
                                        width = 4,
                                        collapsible = T,
                                        status = "warning",
                                        background = "light-blue",
                                        selectInput("hmCatParam",
                                                    label = "Select a Categorical Parameter",
                                                    choices = c("None", as.character(paramNames)),
                                                    selected = NULL),
                                        actionBttn("HMrefreshPlot",
                                                   label = "Refresh Plot",
                                                   style = "material-flat",
                                                   color = "success",
                                                   icon = icon("sliders"))
                                    ),
                                      box(title ="Heatmaps",
                                          #TODO: move all selectIbput to the dropdownButton
                                          collapsible = TRUE,
                                          collapsed = TRUE,
                                          width = 12,
                                          # height = 200,
                                          status = "primary", solidHeader = T,
                                          dropdownButton(
                                            circle = FALSE,
                                            status = "danger",
                                            icon = icon("bars"),
                                            size = "sm",
                                            margin = "10px",
                                            tooltip = tooltipOptions(title = "Click to see additional options!"),
                                            selectInput("hmPalette",
                                                        label = "Select a color palette for heatmap",
                                                        choices = hmPalette,
                                                        width = '400px',
                                                        multiple = FALSE,
                                                        selectize = TRUE),
                                            materialSwitch("scaleHM",
                                                           label = "Normalize to Mode",
                                                           right = TRUE,
                                                           status = "warning",
                                                           TRUE),
                                            materialSwitch("dendrogram",
                                                           label = "Add Dendrograms",
                                                           right = TRUE,
                                                           status = "warning",
                                                           TRUE)
                                          ),
                                          plotlyOutput("heatmaps1")
                                      ),
                                    
                                    box(title ="Dimensionality Reduction Plots",
                                        collapsible = TRUE,
                                        collapsed = TRUE,
                                        width = 12,
                                        # height = 200,
                                        status = "primary", solidHeader = T,
                                        dropdownButton(
                                          circle = FALSE,
                                          status = "danger",
                                          icon = icon("bars"),
                                          size = "sm",
                                          margin = "25px",
                                          inline = TRUE,
                                          tooltip = tooltipOptions(title = "Click to see additional options!"),
                                          uiOutput("xAxisParam"),
                                          uiOutput("yAxisParam"),
                                          uiOutput("coloringParam"),
                                          selectInput("drPalette",
                                                      label = "Select a color palette",
                                                      choices = hmPalette,
                                                      multiple = FALSE,
                                                      selectize = TRUE),
                                          # selectInput("kustKey",
                                          #             label = "Select a color palette for Dim Redux Plot",
                                          #             choices = as.character(c("None", "FlowSOM")),
                                          #             multiple = FALSE,
                                          #             selectize = TRUE),
                                          sliderInput("dotSize",
                                                      "Dot Size:",
                                                      min = .5,
                                                      max = 10,
                                                      step = .5,
                                                      value = 1.5)
                                        ),
                                        actionBttn(
                                          "freshDimRedux",
                                          label = "Refresh Plot",
                                          style = "gradient",
                                          color = "success",
                                          size = "sm",
                                          icon = icon("chart-line")
                                        ),
                                        girafeOutput("girafePlot")
                                    ),
                                      
                                  )
                          )
                      )
                  ))
    
)

# Define server logic
server <- function(input, output) {
    
    ##################################################
    ##    Define Workspace and FCS Files            ##
    ##################################################
    volumes <- c(Home = fs::path_home())
    shinyFileChoose(input,'wspFile', roots = volumes, filetypes = c("", "wsp")
    )
    
    # wspFilePath <- reactiveValues(wspFile = NULL)
    
    observe({
        cat("ninput$wspFile value:\n\n")
        print(input$wspFile)
    })

    output$wspFileTextPath <- renderText ({
        if(is.integer(input$wspFile)) {
            cat("No Files have been selected")
        } else {
            wspPath <- parseFilePaths(volumes, input$wspFile)
            ws <- wspPath$datapath
            ws <<- ws[[1]]
        return(ws)
        }
    })
    

    shinyDirChoose(input,'fcsFolder',roots = volumes,
        filetypes = c("", "fcs", "mqd", "lmd")
    )
    
    # fcsFilesPath <- reactiveValues(fcsFolder = NULL)
    
    observe({
        cat("ninput$fcsFolder value:\n\n")
        print(input$fcsFolder)
    })
    
    output$dirFolder <- renderText({  # use renderText instead of renderPrint
        if(is.integer(input$fcsFolder)){
            cat("No Folder has been selected")
        } else {
            fcsPath <- parseDirPath(volumes, input$fcsFolder)
            # fcsPath <- fcsPath$datapath
            fcsPath <<- as.character(fcsPath[1])
            return(fcsPath)
        }

    })

    ##################################################
    ##    Define Workspace Pops of Interest         ##
    ##################################################

    getWSPselekt <- eventReactive(input$getWSPdata, {
      fjWorkspace <- open_flowjo_xml(ws) # reads the FJ XML
      # reads the fcs files associated withy wsp and the gates
      fjGatingSet <- flowjo_to_gatingset(fjWorkspace, name = 1, path = fcsPath, includeGates = TRUE)
    })
    
    fjGatingSet2 <- eventReactive(input$getWSPdata, {
      # This will get gatingSet from Flowjo
      gs2 <- getWSPselekt()
    })
    
    allNodes <- reactive({
      go <- input$getWSPdata
      # This will find gatingset then get nodes list
      nodelist <- gs_get_pop_paths(fjGatingSet2(), path = "auto")
      # nodelist <- as.data.frame(nodelist)
      popStats <- gs_pop_get_stats(fjGatingSet2())
      popStats <- as.data.frame(popStats)
      popStats$names <- nodelist
      popStats$sample <- NULL
      rownames(popStats) <- nodelist
      return(popStats)
    })
    ##################################################
    ## Define Pops of Interest from Table Selection ##
    ##################################################
    popSelected <- reactiveValues(table1_rows_selected = NULL)
    
    observeEvent(input$table1_rows_selected, ignoreNULL = TRUE, suspended = FALSE,{
      nodelist <- gs_get_pop_paths(fjGatingSet2(), path = "auto")
      node_cellsToPlot <- nodelist[input$table1_rows_selected]
      popSelected$table1_rows_selected <- input$table1_rows_selected
      return(node_cellsToPlot)
    })

    gatingSetPlot <- eventReactive(input$refreshGSPlot, {
        fjWorkspace <- open_flowjo_xml(ws)
        fjGatingSet <- flowjo_to_gatingset(fjWorkspace, name = 1, path = fcsPath, includeGates = TRUE)
        gs <- plot(fjGatingSet)
        
    })
    
    finalDF <- eventReactive(input$getPopData,
                             ignoreNULL = TRUE,{
      #### This will return final data frame for use in making killer plots! ####
      nodeToPlot <- rownames(allNodes()[popSelected$table1_rows_selected,])
      myData <- gs_pop_get_data(fjGatingSet2(), nodeToPlot)
      myData2 <- exprs(myData[[1]]) 
      myData2 <- as.data.frame(myData2)
      return(myData2)
    })
    
    makeObj <- reactive({
      getObject <- input$getPopData

      flowObject <- new("FJObj")
      # flowObject@paths <- c(output$dirFolder, output$wspFileTextPath)
      flowObject@selectedDF <- as.data.frame(finalDF())
      flowObject@paramNames <- as.data.frame(colnames(finalDF()))
      flowObject@plottingParams <- as.data.frame(colnames(finalDF()))
      return(flowObject)
    })
    
    outParams <- reactive({
      getPars <- input$getPopData
      findParameters = TRUE
      if(findParameters){
        theObj <- makeObj()
        parNames <- theObj@paramNames
        parNames <- as.list(parNames)
        return(parNames)
      }else{print("didn't find params")}

    })
    ###################################################
    ## Make Parameter Selectors Based on Loaded File ##
    ###################################################   
    output$hmParameters <- renderUI({
      box(title = "Choose Parameters to Plot",
          width = 4,
          status = "warning",
          collapsible = T,
          background = "light-blue",
          selectInput("Parameters",
                      label = "Select Parameters to Plot",
                      choices = c("None", outParams()),
                      multiple = TRUE,
                      selectize = TRUE)
          )
      })
    output$xAxisParam <- renderUI({
          selectInput("xAxParam",
                      label = "Select X Axis Parameter",
                      choices = outParams(),
                      width = '400px',
                      multiple = FALSE,
                      selectize = TRUE)
    })
    output$yAxisParam <- renderUI({
          selectInput("yAxParam",
                      label = "Select Y Axis Parameter",
                      choices = outParams(),
                      width = '400px',
                      multiple = FALSE,
                      selectize = TRUE)
    })
    output$coloringParam <- renderUI({
      selectInput("coloringParam",
                  label = "Select Categorical Parameter",
                  choices = outParams(),
                  width = '400px',
                  multiple = FALSE,
                  selectize = TRUE)
    })
    ##################################################
    ## Make some data tables ##
    ##################################################    
    
    output$table2 <- renderDT(
      server = TRUE,
      datatable(
        finalDF(),
        rownames = TRUE,
        escape = FALSE,
        fillContainer = FALSE,
        selection = list(target = "row"),
        # width = '100px',
        # height = '10000px',
        extensions = 'Buttons',
        plugins = 'natural',
        options = list(
          pageLength = 10,
          scrollX = TRUE, # makes the table scrollable to left/right!
          # info = FALSE,
          # lengthMenu = list(c(11,-1), c("11", "All")),
          # columnDefs = list(list(type = 'natural',
          #                        targets = 'all')),
          dom = 'Bfrtip',
          buttons = list(list(
            extend = "copy",
            text = "COPY",
            title = "Table Export"
          ))
        )
      )
    )
    
    output$gatingH <- renderPlot( gatingSetPlot() )
    
    proxy <- dataTableProxy("table1") # start with the proxy and update with plot selections
    
    output$table1 <- renderDT(
      server = FALSE,
      datatable(
        allNodes(),
        rownames = FALSE,
        escape = FALSE,
        fillContainer = FALSE,
        selection = list(mode = "single",
                         target = "row"),
        width = 11,
        height = 9,
        extensions = 'Buttons',
        plugins = 'natural',
        options = list(
          pageLength = 11,
          info = FALSE,
          lengthMenu = list(c(11,-1), c("11", "All")),
          columnDefs = list(list(type = 'natural',
                                 targets = 'all')),
          dom = 'Bfrtip',
          buttons = list(list(
            extend = "copy",
            text = "COPY",
            title = "Table Export"
          ))
        )
      ) #%>% formatStyle(backgroundColor = styleInterval(3, c('gray', 'yellow')))
    )
    #################################
    # Create the Heatmap Data Frame #
    #################################
    # This is wrapped inside of reactive func to make it respond to user selection. 
    # This will return the df for heatmaps
    filterData <- reactive({
      
        if (input$hmCatParam == "None"){
            HMdf <- finalDF() %>% dplyr::select(input$Parameters)
            HMdf <- round(HMdf, 2)
            return(HMdf)
        }else{
            # This takes input from Categorical Param Selector for group_by func
            groupedInput <- finalDF() %>%
                # dplyr::filter(eval(parse(text = input$catParam)) %in% input$clusters) %>%
                dplyr::group_by_at(vars(input$hmCatParam)) %>%
                dplyr::summarise(across(.cols = input$Parameters, .fns = mean))
            
            # This makes sure the names of populations are added to the data frame (if we have them). And that they're ordered the way one would hope (natural order)
            groupedInput <- as.data.frame(groupedInput)
            # namers <- groupedInput[, input$hmCatParam]
            # 
            # namers <- namers[order(nchar(namers), namers)]                                 ## order names that will be used as the row names momentarily
            # groupedInput <- groupedInput[mixedorder(groupedInput[[input$hmCatParam]]), ]      ## order rows based on the categorical parameter
            # groupedInput[input$hmCatParam] <- NULL
            # row.names(groupedInput) <- namers
            
            GI <- dplyr::select(groupedInput, input$Parameters) # This is much easier to implement without having to remove unwanted columns from above code ex
            GI <- as.matrix(round(GI, 2))
            return(GI)
        }
        
    })
    ######################################
    # Create the Refresh Button Heatmap #
    ######################################
    # the eventReactive will respond to the Refresh buttons in UI
    heatmap1 <- eventReactive(input$HMrefreshPlot, {
        #TODO make these interactive with heatmaply
        #TODO if using heatmaply, also change to renderPlotly
        # if(input$scaleHM){
        #     scl = "column"
        # }else{
        #     scl = "none"
        # }
        if(input$dendrogram){
          clusterD = T
        }else{
          clusterD = F
        }
        
        # if(input$hmCatParam == "None"){
            # pheatmap::pheatmap(filterData(),color = eval(parse(text = input$hmPalette))(n=length(mat_breaks)-1),
            #                    border_color = "grey20",
            #                    main = "",
            #                    show_rownames = TRUE,
            #                    show_colnames = TRUE,
            #                    fontsize_col = 8,
            #                    angle_col = "45",
            #                    fontsize_row = 8,
            #                    kmeans_k = 30,
            #                    cluster_rows = clusterD,
            #                    cluster_cols = clusterD)
            heatmaply(filterData(),
                      colors = eval(parse(text = input$hmPalette)),
                      scale = "none",# scl,
                      fontsize_row = 12, #input$hmFonts,
                      fontsize_col = 12, #input$hmFonts,
                      dendrogram = input$dendrogram,
                      label_names = c("Cell", "Gene", "Value"))
        #     )
        #     
        # }else{
        #     # pheatmap::pheatmap(filterData(), color = eval(parse(text = input$hmPalette))(n=length(mat_breaks)-1),
        #     #                    border_color = "grey20",
        #     #                    main = "",
        #     #                    show_rownames = TRUE,
        #     #                    show_colnames = TRUE,
        #     #                    fontsize_col = 8,
        #     #                    angle_col = "45",
        #     #                    fontsize_row = 8,
        #     #                    cluster_rows = clusterD,
        #     #                    cluster_cols = clusterD)
        #     heatmaply(filterData(),
        #               colors = eval(parse(text = input$hmPalette))(n=length(mat_breaks)-1),
        #               scale = scl,
        #               fontsize_row = input$hmFonts,
        #               fontsize_col = input$hmFonts,
        #               dendrogram = input$dendrogram,
        #               label_names = c("Cluster", "Gene", "Value")
        #     )
            
        # }
    })
    output$heatmaps1 <- renderPlotly(heatmap1())

    dimReduxPlotting <- eventReactive(input$freshDimRedux, {
      downPop2 <- finalDF()
      if(nrow(downPop2) > 10000){
        print("downsample pop")
        # Want to randomly downsample the df, specify how many rows below
        downPop2 <- dplyr::slice_sample(downPop2, n=8000)
      } else{
        print("don't downsample, use default population size")
        downPop2 <- downPop2}
      
      dimRedux.plot <- ggplot(data = downPop2) +
        scale_color_viridis_d_interactive(option = input$drPalette) +
        geom_point_interactive(aes(x = eval(parse(text = input$xAxParam)), 
                                   y = eval(parse(text = input$yAxParam)), 
                                   color = factor(downPop2[,input$coloringParam]),
                                   tooltip = downPop2[,input$coloringParam],
                                   data_id = downPop2[,input$coloringParam]),
                               size = input$dotSize) +
        theme_classic() +
        theme(
          legend.position       = 'right',
          legend.title          = element_blank(),
          legend.background     = element_blank(),
          legend.box.background = element_blank(),
          legend.key            = element_rect_interactive(fill = "transparent"),
          panel.background      = element_rect_interactive(fill = "transparent"),
          plot.background       = element_rect_interactive(fill = "transparent"),
          panel.border          = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          axis.text = element_blank()
        )
      # scale_color_viridis_c(option = "inferno")
      # my_gg <- g + geom_point_interactive(aes(tooltip = df[,input$hmCatParam]), size = 0.5)
      techDimReduxPlot <- girafe(
        code = print(dimRedux.plot),
        width_svg = 15,
        height_svg = 9,
        options = list(
          opts_tooltip(use_fill = TRUE),
          opts_toolbar(saveaspng = FALSE),
          opts_zoom(max = 5),
          opts_selection(
            type = "multiple",
            only_shiny = FALSE,
            css = "fill:red;stroke:white;stroke-width:1px;"
          ),
          opts_selection_key(
            css = "stroke:black;r:4pt;",
            #TODO: make legends selectable
            type = "multiple",
            only_shiny = TRUE
          ),
          opts_hover(css = "fill:red;stroke:white;stroke-width:1px;cursor:pointer;"),
          opts_hover_key(css = "stroke:black;r:4pt;cursor:pointer;")
        )
      )
      return(techDimReduxPlot)
    })
    
    output$girafePlot <- renderGirafe({ dimReduxPlotting() })
}

# Run the application 
shinyApp(ui = ui, server = server)
