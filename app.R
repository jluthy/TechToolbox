##############################################################
##        FJ Tech Toolbox by Josh Luthy                     ##
##############################################################
packages<-function(x){
  x<-as.character(match.call()[[2]])
  if(!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(shiny)
packages(shinyFiles)
packages(shinydashboard)
packages(shinyWidgets)
packages(flowWorkspace)
packages(CytoML)
packages(ggplot2)
packages(ggiraph)
packages(ggcyto)
packages(FlowSOM)
packages(tidyr)
packages(dplyr)
packages(heatmaply)
packages(plotly)
packages(viridisLite)
packages(DT)
packages(ggplot2)
packages(cowplot)
packages(htmlwidgets)
packages(shinyalert)

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
                              box(title ="Workspace Hierarchy Table View",
                                  width = 6,
                                  status = "primary", solidHeader = T,
                                  verbatimTextOutput("nodeName"),
                                  DTOutput("table1"),
                                  actionButton("getWSPgating",
                                               label = "Get WSP Gating Heirarchy",
                                               style = "gradient",
                                               color = "success",
                                               size = "sm",
                                               icon = icon("chart-line"),
                                  ),
                                  useShinyalert(), 
                                  actionButton(
                                    "getPopData",
                                    label = "getPop Matrix",
                                    style = "gradient",
                                    color = "success",
                                    size = "sm",
                                    icon = icon("chart-line"))
                              ),
                              box(title ="Workspace Plots View",
                                  width = 6,
                                  status = "primary", solidHeader = T,
                                  plotOutput("gatingH",
                                             click = "plot_click",
                                             dblclick = "plot_dblclick",
                                             hover = "plot_hover",
                                             brush = "plot_brush"),
                                  verbatimTextOutput("info"),
                              )
                            ),
                            fluidRow(
                              box(title = "Selected Workspace Population Table",
                                  width = 11,
                                  status = "primary", solidHeader = T,
                                  DTOutput("table2"),
                              )
                            )
                    ),
                    ##################################################
                    # Define UI Layout for HEATMAP Tab             #
                    ##################################################
                    tabItem(tabName = "heatmaps",
                            # fluidRow(
                            #
                            #
                            #   box(title = "Select a Categorical Parameter",
                            #       width = 4,
                            #       collapsible = T,
                            #       status = "warning",
                            #       background = "light-blue",
                            #
                            #       )
                            #   ),
                            box(title ="Heatmaps",
                                #TODO: move all selectIbput to the dropdownButton
                                collapsible = TRUE,
                                collapsed = TRUE,
                                width = 11,
                                # height = 200,
                                status = "primary", solidHeader = T,
                                dropdownButton(
                                  circle = FALSE,
                                  status = "danger",
                                  icon = icon("bars"),
                                  size = "sm",
                                  inline = TRUE,
                                  margin = "25px",
                                  tooltip = tooltipOptions(title = "Click to see additional options!"),
                                  uiOutput("hmParameters"),
                                  uiOutput("hmCatParam"),
                                  selectInput("hmCatParam",
                                              label = "Select a Categorical Parameter",
                                              choices = c("None", as.character(paramNames)),
                                              selected = NULL),
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
                                actionButton("HMrefreshPlot",
                                           label = "Refresh Heatmap",
                                           style = "unite",
                                           color = "success",
                                           size = "sm",
                                           icon = icon("chart-line")
                                ),
                                plotlyOutput("heatmaps1"),
                                downloadButton("download_plotly_heatmap", "Download Interactive graph", class = "heatButton"),
                                tags$head(tags$style("heatButton{background-color:#E8E8E8;} heatButton{color: Black;}"))
                            ),

                            box(title ="Dimensionality Reduction Plots",
                                collapsible = TRUE,
                                collapsed = TRUE,
                                width = 11,
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
                                actionButton(
                                  "freshDimRedux",
                                  label = "Refresh Plot",
                                  style = "gradient",
                                  color = "success",
                                  size = "sm",
                                  icon = icon("chart-line")
                                  ),
                                # style = "position:absolute;right:2em;")
                                girafeOutput("girafePlot"), #download_dimRedux_Rplots
                                downloadButton("download_dimRedux_Rplots", "Download graph", class = "dimButton"),
                                tags$head(tags$style("dimButton{background-color:#E8E8E8;} dimButton{color: Black;}"))
                            ),
                            box(title ="Correlation Plots",
                                collapsible = TRUE,
                                collapsed = TRUE,
                                width = 11,
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
                                  uiOutput("correlationParams"),
                                  selectInput("corrPalette",
                                              label = "Select a color palette for heatmap",
                                              choices = hmPalette,
                                              width = '400px',
                                              multiple = FALSE,
                                              selectize = TRUE)
                                ),
                                actionButton(
                                  "freshCorrPlot",
                                  label = "Refresh Correlation Plots",
                                  style = "gradient",
                                  color = "success",
                                  size = "sm",
                                  icon = icon("chart-line")
                                ),
                                plotlyOutput("correlation1"),
                                downloadButton("download_corr_plots", "Download correlation graph", class = "corButton"),
                                tags$head(tags$style("corButton{background-color:#E8E8E8;} corButton{color: Black;}"))
                            ),

                    )
                  )
                  )
                  ))

# )

# Define server logic
server <- function(input, output, session) {

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

  getWSPselekt <- eventReactive(input$getWSPgating, {
    fjWorkspace <- open_flowjo_xml(ws) # reads the FJ XML
    # reads the fcs files associated withy wsp and the gates
    fjGatingSet <- flowjo_to_gatingset(fjWorkspace, name = 1, path = fcsPath, includeGates = TRUE)
  })

  fjGatingSet2 <- eventReactive(input$getWSPgating, {
    # This will get gatingSet from Flowjo
    fjWorkspace <- open_flowjo_xml(ws)
    fjGatingSet <- flowjo_to_gatingset(fjWorkspace, name = 1, path = fcsPath, includeGates = TRUE)

    output$gatingH <- renderPlot(plot(fjGatingSet))
    gs2 <- getWSPselekt()
    return(gs2)
  })

  allNodes <- reactive({
    go <- input$getWSPgating
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


  finalDF <- eventReactive(input$getPopData,
                           ignoreNULL = TRUE,{
                             #### This will return final data frame for use in making killer plots! ####
                             nodeToPlot <- rownames(allNodes()[popSelected$table1_rows_selected,])
                             myData <- gs_pop_get_data(fjGatingSet2(), nodeToPlot)
                             myData2 <- exprs(myData[[1]])
                             myData2 <- as.data.frame(myData2)
                             return(myData2)
                           })
  
observeEvent(input$table1_rows_selected, {
  
    output$gatingH <- renderPlot({
      tryCatch({
        nodeToPlot <- rownames(allNodes()[popSelected$table1_rows_selected,])
        fjWorkspace <- open_flowjo_xml(ws)
        fjGatingSet <- flowjo_to_gatingset(fjWorkspace, name = 1, path = fcsPath, includeGates = TRUE)
        plot(fjGatingSet, nodeToPlot)
      }, warning = function (w) {
        writeLine(paste0("an warning occured with plotting",w))
      }, error = function(e) {
        shinyalert("Oops!", "Selected terminal population, no plot to show.", type = "error")
      }, finally = {
        writeLines("Plotted gates from selected heirarchy, otherwise alerted user")
      })
    })
})

  makeObj <- reactive({
    getObject <- input$getPopData

    flowObject <- new("FJObj")
    # flowObject@paths <- c(output$dirFolder, output$wspFileTextPath)
    newPopMatrix <- as.data.frame(finalDF())
    allparNames <- colnames(newPopMatrix)
    parNamesTemp <- gsub("\\-", "_", allparNames)
    parNamesTemp <- gsub(" ", "_", parNamesTemp)
    parNamesTemp <- gsub("\\.", "_", parNamesTemp)
    colnames(newPopMatrix) <- parNamesTemp
    parNames2 <- as.data.frame(parNamesTemp)
    rm(allparNames)
    rm(parNamesTemp)
    flowObject@selectedDF <- newPopMatrix
    flowObject@paramNames <- parNames2
    flowObject@plottingParams <- parNames2
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
    selectInput("Parameters",
                label = "Select Parameters to Plot",
                choices = c("None", outParams()),
                multiple = TRUE,
                selectize = TRUE)

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
  output$correlationParams <- renderUI({
    selectInput("corrParams",
                label = "Select Parameters for Correlation Heatmaps",
                choices = outParams(),
                width = '400px',
                multiple = TRUE,
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

  # output$gatingH <- renderPlot( gatingSetPlot() )

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
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
  #################################
  # Create the Heatmap Data Frame #
  #################################
  # This is wrapped inside of reactive func to make it respond to user selection.
  # This will return the df for heatmaps
  filterData <- reactive({

    if (input$hmCatParam == "None"){
      fjPop <- makeObj()

      HMdf <- fjPop@selectedDF %>% dplyr::select(input$Parameters)
      HMdf <- round(HMdf, 2)
      return(HMdf)
    }else{
      # This takes input from Categorical Param Selector for group_by_at func
      fjPop <- makeObj()
      groupedInput <- fjPop@selectedDF %>%
        # dplyr::filter(eval(parse(text = input$catParam)) %in% input$clusters) %>%
        dplyr::group_by_at(vars(input$hmCatParam)) %>%
        dplyr::summarise(across(.cols = input$Parameters, .fns = mean))

      groupedInput <- as.data.frame(groupedInput)

      GI <- dplyr::select(groupedInput, input$Parameters) # This is much easier to implement without having to remove unwanted columns from above code ex
      GI <- as.matrix(round(GI, 2))
      return(GI)
    }

  })

  ######################################
  # Create the Refresh Button Heatmap #
  ######################################
  # the eventReactive will respond to the Refresh buttons in UI
  Iheatmap1 <- eventReactive(input$HMrefreshPlot, {
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

    if(input$hmCatParam == "None"){

      heatmaply(filterData(),
                colors = eval(parse(text = input$hmPalette)),
                scale = "none",# scl,
                fontsize_row = 12, #input$hmFonts,
                fontsize_col = 12, #input$hmFonts,
                dendrogram = input$dendrogram,
                label_names = c("Cell", "Gene", "Value"))

    }else{
      heatmaply(filterData(),
                colors = eval(parse(text = input$hmPalette)),
                scale = "none",
                fontsize_row = 12,#input$hmFonts,
                fontsize_col = 12,#input$hmFonts,
                dendrogram = input$dendrogram,
                label_names = c("Cluster", "Gene", "Value")
      )

    }
  })
  output$heatmaps1 <- renderPlotly({
    req(input$HMrefreshPlot)
    Iheatmap1()
  })
  # Download the heatmap html file
  output$download_plotly_heatmap <- downloadHandler(
    filename = function() {
      paste("Interactive Heatmaps-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      # export plotly html widget as a temp file to download.
      saveWidget(as_widget(Iheatmap1()), file, selfcontained = TRUE)
    }

  )
  #########################################
  # Create Correlation Heatmap Data Frame #
  #########################################
  correlationData <- reactive({

    coreDF <- makeObj()
    coreDF <- coreDF@selectedDF
    return(coreDF)
  })

  correlationPlots1 <- eventReactive(input$freshCorrPlot, {
    # Select specific parameters of interest if you want
    # df <- correlationData()
    # # df <- as.matrix(df)
    # df2 <<- df
    # which1s <- select(df, input$correlationPrams)
    corrMat <- correlationData() %>% select(as.character(input$corrParams)) # This is much easier to implement without having to remove unwanted columns from above code ex
    # GI <- sapply(GI, as.numeric)
    corrMat <- as.data.frame(corrMat)

    # Finally make correlation heatmaps
    heatmaply_cor(
      cor(corrMat),
      # colors = eval(parse(text = input$corrPalette)),
      xlab = "Features",
      ylab = "Features",
      grid_gap = 1,
      label_names = c("Y Feature", "X Feature", "Value"),
      width = 10,
      height = 20

    )

  })

  output$correlation1 <- renderPlotly({
    req(input$freshCorrPlot)
    correlationPlots1()
  })

  output$download_corr_plots <- downloadHandler(
    filename = function() {
      paste("Correlation Plot-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      # export plotly html widget as a temp file to download.
      saveWidget(as_widget(correlationPlots1()), file, selfcontained = TRUE)
    }
  )
  ##########################################
  # Create Interactive Dim Reduction Plots #
  ##########################################
  dimReduxPlotting <- eventReactive(input$freshDimRedux, {
    downPop2 <- makeObj()
    downPop2 <- downPop2@selectedDF
    if(nrow(downPop2) > 20000){
      print("downsample pop")
      # Want to randomly downsample the df, specify how many rows below
      downPop2 <- dplyr::slice_sample(downPop2, n=10000)
    } else{
      print("don't downsample, use default population size")
      downPop2 <- downPop2}
    #TODO make legend scales interactive
    dimRedux.plot <- ggplot(data = downPop2) +
      scale_color_viridis_d_interactive(option = input$drPalette,
                                        tooltip = as.character(downPop2[,input$coloringParam]),
                                        data_id = as.character(downPop2[,input$coloringParam]),
                                        guide = "legend") +
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

  output$girafePlot <- renderGirafe({
    req(input$freshDimRedux)
    dimReduxPlotting()
  })

  output$download_dimRedux_Rplots <- downloadHandler(
    filename = function() {
      paste("Dimensionlity Reduction Plot-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      # export plotly html widget as a temp file to download.
      saveWidget(as_widget(dimReduxPlotting()), file, selfcontained = TRUE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
