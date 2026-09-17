# **********************************************************************************************************************
# Name of Application: MesWin - XplorerHiddenClusters
# Version: 1.0
# Date created: 2021/01/26
# Developer: Filmon Mesgun 79513, Julia Winter 79498
# **********************************************************************************************************************

# Section 0: Library Imports ---------------------------------------------------

# Shiny related libraries
library(shiny) #
library(shinyjs) # adds javascript functionalities to shiny dashboards 
library(bs4Dash) # enables Bootstrap 4 Shiny dashboards using AdminLTE 3
library(shinyFiles) # provides server-side file system viewer for Shiny
library(shinycssloaders) # provides CSS loader animations for Shiny outputs

# D3js realted libraries
library(r2d3) # enables the implementation of d3js into shiny dashboards

# Clustering related libraries
library(klustR) # provides functions to generate a parallel coordinate plot
library(subspace) # offers function to perform subspace clustering
library(rJava) # R to Java Interface (used for subspace clustering)
library(cluster) # offers function to evaluate clustering algorithms
library(factoextra) # offers plots for cluster analysis 

# Visualisation related libraries
library(Cairo) # creating high quality pictures
library(hrbrthemes) # theming for ggplot2
library(viridis) # offers color maps based on matplotlib
library(ggridges) # provides function to create rideline plots
library(ggpubr) # enables publication-ready plots
library(corrplot) # provides function to create correlation matrix
library(plotly) # offers interactive plots
library(DT) # R interface to the DataTables library.

# Data manipluation related libraries
library(tidyverse) # enables support functions for  data manipulation
library(dplyr) # enables high level functions for data manipulation

# Section 1: Datasets Imports --------------------------------------------------

# Setting dataset path
path <- "evaluation_data_clusters/"


# Load in datasets
dt_d3 <- read.csv(paste(sep="", path,  "dat_clusters_3D.csv"))
dt_d5 <- read.csv(paste(sep="", path,  "dat_clusters_5D.csv"))
dt_d10 <- read.csv(paste(sep="", path,  "dat_clusters_10D.csv"))
dt_d20 <- read.csv(paste(sep="", path,  "dat_clusters_20D.csv"))
dt_d5_hid <- read.csv(paste(sep="", path,  "dat_hidden_clusters_5D.csv"))
dt_d10_hid <- read.csv(paste(sep="", path,  "dat_hidden_clusters_10D.csv"))
dt_d20_hid <- read.csv(paste(sep="", path,  "dat_hidden_clusters_20D.csv"))
heart13D <- as.data.frame(scale(read.csv(
  paste(sep="", path,  "heart_failure_clinical_records_dataset.csv"))))

# Section 2: Declaration of predefined functions -------------------------------

# Enabling users to choose between preloaded datasets
chooseDS  <- function(selectedDS){
  ds_plot <- switch(selectedDS,
                    "Heart13D" = heart13D,
                    "3D" = dt_d3,
                    "5D" = dt_d5,
                    "10D" = dt_d10,
                    "20D"= dt_d20,
                    "Hidden5D" = dt_d5_hid,
                    "Hidden10D" = dt_d10_hid,
                    "Hidden20D" = dt_d20_hid)
  ds_plot[1:length(ds_plot)] <- sapply(ds_plot, as.numeric)
  ds_plot[sapply(ds_plot, function(x) all(is.na(x)))] <- NULL
  return(ds_plot) # return preprocessed dataset
}

#  Processing the results of subspace clustering for 3D Scatterplot
clearDataForScatter <- function(datasetubClustering, originalDS){
  
  
  columnNames <- c("DimensionName", "SubClusterNo", "ObjectNo", "RowValue")
  # Predefined result dataframe
  result <- as.data.frame(matrix(ncol = 4))
  colnames(result) <- columnNames
  for (i in 1:length(datasetubClustering)){
    for (t in 1:length(datasetubClustering[[i]]$subspace)){
      columnName <- list()
      if(datasetubClustering[[i]]$subspace[t] == TRUE){ # if the index of a feature in the current subspace is included (=TRUE), add its name, every index and rowvalue into a dataframe
        columnName <- append(columnName, (colnames(originalDS)[t]))
        
        for (n in datasetubClustering[[i]]$objects[1]:datasetubClustering[[i]]$objects[length(datasetubClustering[[i]]$objects)]){
          newRow <- as.data.frame(columnName)
          newRow <- cbind(newRow, i)
          newRow <- cbind(newRow, n)
          newRow <- cbind(newRow, originalDS[n,t])
          colnames(newRow) <- columnNames
          result <- rbind(result, newRow)
        }
      }
    }
  }
  return(result <- result[-c(1),]) # return dataframe with all subclusters
  
}

#  Further Processing the results of clearDataForScatter for left D3js Barplot
clearDataForBarPlot_ClusterByFeature <- function(dataTbl){
  # generate dataframe to display in how many subclusters each feature is
  barplotData <- unique.data.frame(dataTbl[1:2])
  barplotDataSorted <- barplotData[order(barplotData$DimensionName),]
  
  tmpColumnNames <- c("DimensionName", "SubCluster", "Amount")
  tmpDimName <- ""
  # create predefined result dataframe
  barplotResult <- as.data.frame(matrix(ncol = 3))
  
  # change column Names to predefined column names
  colnames(barplotResult) <- tmpColumnNames
  counter <- 0
  for(i in 1:nrow(barplotDataSorted)){
    if(i == 1){
      counter <- 1
      tmpCluster <- as.character(barplotDataSorted$SubClusterNo[i])
    }else{
      
      if(barplotDataSorted$DimensionName[i] == tmpDimName){
        counter <- counter + 1
        tmpCluster <- paste(tmpCluster, ", ", as.character(barplotDataSorted$SubClusterNo[i]))
      } else
      {
        tmpRow <- as.data.frame(tmpDimName)
        tmpRow <- cbind(tmpRow, tmpCluster)
        tmpRow <- cbind(tmpRow, counter)
        colnames(tmpRow) <- tmpColumnNames
        barplotResult <- rbind(barplotResult,tmpRow)
        counter <- 1
        tmpCluster <- as.character(barplotDataSorted$SubClusterNo[i])
      }
    }
    
    
    
    tmpDimName <- barplotDataSorted$DimensionName[i]  
    if(i == nrow(barplotDataSorted)){
      tmpRow <- as.data.frame(tmpDimName)
      tmpRow <- cbind(tmpRow, tmpCluster)
      tmpRow <- cbind(tmpRow, counter)
      colnames(tmpRow) <- tmpColumnNames
      barplotResult <- rbind(barplotResult,tmpRow)
    }
    
    
  }
  return(barplotResult <- barplotResult[-c(1),]) # return dataframe for the left barplot
}

#  Further Processing the results of clearDataForScatter for right D3js Barplot
clearDataForBarPlot_FeatureByCluster<- function(dataTbl){
  # generate dataframe to display how many features are in each subcluster
  barplotData <- unique.data.frame(dataTbl[1:2])
  barplotDataSorted <- barplotData[order(barplotData$SubClusterNo),]
  
  
  tmpColumnNames <- c("DimensionName", "SubCluster", "Amount")
  tmpDimName <- ""
  # create predefined result dataframe
  barplotResult <- as.data.frame(matrix(ncol = 3))
  
  # change column Names to predefined column names
  colnames(barplotResult) <- tmpColumnNames
  counter <- 0
  for(i in 1:nrow(barplotDataSorted)){
    if(i == 1){
      counter <- 1
      tmpCluster <- as.character(barplotDataSorted$DimensionName[i])
      
    }else{
      
      if(barplotDataSorted$SubClusterNo[i] == tmpDimName){
        counter <- counter + 1
        tmpCluster <- paste(tmpCluster, ", ", as.character(barplotDataSorted$DimensionName[i]))
      } else
      {
        tmpRow <- as.data.frame(tmpDimName)
        tmpRow <- cbind(tmpRow, tmpCluster)
        tmpRow <- cbind(tmpRow, counter)
        colnames(tmpRow) <- tmpColumnNames
        barplotResult <- rbind(barplotResult,tmpRow)
        counter <- 1
        tmpCluster <- as.character(barplotDataSorted$DimensionName[i])
        
      }
    }
    
    tmpDimName <- barplotDataSorted$SubClusterNo[i]
    if(i == nrow(barplotDataSorted)){
      tmpRow <- as.data.frame(tmpDimName)
      tmpRow <- cbind(tmpRow, tmpCluster)
      tmpRow <- cbind(tmpRow, counter)
      colnames(tmpRow) <- tmpColumnNames
      barplotResult <- rbind(barplotResult,tmpRow)
    }
    
    
  }
  return(barplotResult <- barplotResult[-c(1),])# return dataframe for the right barplot
}

#  Function to filter dataset based on the clicked bar of barplots by user
subClusterFeatureFilter <- function(subClusterResult, barScatterClick, oDS){
  # select all features from subclustering dataframe based on click on barplot
  resultFeatureVector <- subClusterResult[subClusterResult$DimensionName 
                                          == barScatterClick,]
  # keep only unique feature names
  resultFeatureVector <- unique(resultFeatureVector$SubClusterNo)
  # save the subcluster index number in a vector 
  toSubClusterFilter <- c("")
  for(i in 1:length(resultFeatureVector)){
    toSubClusterFilter <- c(toSubClusterFilter, resultFeatureVector[i])
  }
  toSubClusterFilter <- toSubClusterFilter[-c(1)]
  
  # select all feature that are assigned to the specific subcluster index number
  featureDF <- unique(subClusterResult[subClusterResult$SubClusterNo 
                                       == toSubClusterFilter,]$DimensionName)
  
  # save these features in a vector 
  toDSFilter <- c("")
  for(i in 1:length(featureDF)){
    toDSFilter <- c(toDSFilter, featureDF[i])
  }
  toDSFilter <- toDSFilter[-c(1)]
  
  # filter the dataset based on the feature vector
  oDS <- oDS[toDSFilter]
  return(oDS) # return subcluster and feature filtered dataset
}

# Section 3: Shiny UI-----------------------------------------------------------

# Initiation shiny app
shinyApp(
  
  # Initiation UI
  ui = bs4DashPage(
    
    title = "MesWin - XplorationHiddenCluster",
    fullscreen = FALSE,

    # Organize navigation bar
    header = bs4DashNavbar(
      
      # Conditonal Panels
      
      conditionalPanel(
        'input.tabs == "subClusterAnalysis"',
        "Interactive Subspace Cluster Analysis"
      ), 
      conditionalPanel(
        'input.tabs == "optimalCluster"',
        "Cluster Analysis"),
      conditionalPanel(
        'input.tabs == "uploadDataset"',
        "Upload own Dataset"),
      
      # Navigation bar title header
      title = bs4DashBrand(
        title = "MesWin - XplorationHiddenCluster",
        color = "primary",
        image = "https://i.ibb.co/KqBnMVH/Logo-117-113.png"
      ),
      skin = "light",
      border = TRUE,
      fixed = TRUE
    ),
  dark = FALSE,
  
  
    # Navigation bar menu points structure

    sidebar = bs4DashSidebar(
      skin = "light",
      status = "primary",
      opacity = 0.8,
      expandOnHover =  FALSE,
      elevation = 3,

      # Menu points
      
      sidebarMenu(
        id = "tabs",
        sidebarHeader("SubCluster Analysis"),
        menuItem(
          "Subspace Cluster Analysis",
          tabName = "subClusterAnalysis",
          icon = icon("chart-bar")
        ),
        
        
        menuItem(
          "Cluster Analysis",
          tabName = "optimalCluster",
          icon = icon("object-ungroup")
        ),
        
        
        menuItem(
          "Upload your own Dataset",
          tabName = "uploadDataset",
          icon = icon("upload")
        )
      ),
      
      # Interactivity widgets
      
        selectInput(inputId = "dataset", label =  "Choose a cluster dataset:",
                         choices = c("Heart13D","3D", "5D", "10D", "20D", "Hidden5D", "Hidden10D", "Hidden20D"
                                     )),
      
          conditionalPanel(
             'input.tabs == "optimalCluster"',
              selectInput(inputId = "clusteralgorithmus", label =  "Choose the preferred algorithm:",
                          choices = c("K-Means", "Fuzzy C-Means")),

              numericInput(inputId = 'clusters', label = 'Cluster count', 2, min = 1, max = 10)
            ),

          conditionalPanel(
            'input.tabs == "subClusterAnalysis"',
            numericInput(inputId = 'DensityThreshold', label = 'Choose your density threshold:', 
                        0.5, min = 0.1, max = 1.0, step = 0.1, )
          ),
        
          list(uiOutput("out_features"))

    ),
    
    # Footer
    footer = bs4DashFooter(
      left = HTML(paste("Developed by: Filmon Mesgun & Julia Winter", 
      "Filmon Mesgun, Julia Winter, Andreas Theissler (2021):", 
      "'Guided Detection of Hidden Subclusters in a High-Dimensional Data Space via Interactive Visual Analytics'.", 
      sep="<br/>")),
      right = img(src = "hochschuleAalenLogo.png", height = 72, width = 500)
    ),
    
    # Body
    
    body = bs4DashBody(
      useShinyjs(),
      bs4TabItems(
        
        # Contents: menu point 'Cluster Analysis'
        
        bs4TabItem(
          tabName = "subClusterAnalysis",
          
          fluidRow(
            column(width=4,
                   bs4Table(
                     cardWrap = TRUE,
                     headTitles = c("Barplot - Cluster Per Feature",
                          tooltip(
                            actionButton(
                              "InfoButton", 
                              "i",
                              size = "xs",
                              status = "primary", style="position:absolute;right:1em;top:36em;"),
                            title = "This Barplot shows in how much subclusters one feature was assigned to. 
                              Hover over the bar of one feature and  and see the corresponding subclusters.
                              For deeper analysis of each feature, click on the feature bar to filter the 3D Scatterplot.",
                            placement = "bottom"
                          )
                          ),
                     shinycssloaders::withSpinner(
                       d3Output(outputId = "out_barplotD3_clusterPerFeature_NEU")),
                   )  
                   
            ),
            
            column(width = 4,
                   bs4Table(
                      cardWrap = TRUE,
                      headTitles = c("Barplot - Feature per Cluster",
                        tooltip(
                          actionButton(
                            "InfoButton", 
                            "i",
                            size = "xs",
                            status = "primary", style="position:absolute;right:1em;top:36em;"),
                          title = "This Barplot shows how much features where assigned to one subcluster. 
                          Hover over the bar of one subcluster and see the corresponding features.
                          For deeper analysis of each subcluster, click on the sublucster bar to filter the general data overview plots.",
                          placement = "bottom"
                        )
                        ),
                     shinycssloaders::withSpinner(
                       d3Output(outputId = "out_barplotD3_FeaturePerCluster_NEU")),
                   )
            ),
            
            column(width = 4,
                   bs4Table(
                     cardWrap = TRUE,
                     headTitles = c("Boxplot",
                          tooltip(
                            actionButton(
                              "InfoBoxPlot", 
                              "i",
                              size = "xs",
                              status = "primary", style="position:absolute;right:1em;top:36em;"),
                            title = "The Boxplot displays summarized metrics of each feature in direct comparison to each other. 
                      Here you can see which feature are different by mean/median or outliers and therwith identify potential corrupt features.",
                            placement = "bottom"
                          )),
                     shinycssloaders::withSpinner(
                       plotlyOutput(outputId = "out_boxPlot"))
                   )
            )
          ),
          
          fluidRow(
            column(width=8,
                   bs4Table(
                     cardWrap = TRUE,
                     headTitles = c("3D Scatterplot",
                          tooltip(
                            actionButton(
                              "InfoScatterPlot", 
                              "i",
                              size = "xs",
                              status = "primary", style="position:absolute;right:1em;top:36em;"),
                            title = "This 3D Scatterplot displays the distribution of the datapoints divided by several subclusters.
                            In this 3 dimensional space, the datapoints are placed by the position of their index number, 
                            subcluster assignment and datapoint value. 
                            
                            ",
                            placement = "bottom"
                          )),                                    
                     shinycssloaders::withSpinner(
                       plotlyOutput(outputId = "out_scatterPlotly"))
                   )
            ),
            
            column(width = 4,                     
                   bs4Table(
                     cardWrap = TRUE,
                     headTitles = c("Ridgeplot",
                        tooltip(
                          actionButton(
                            "InfoRidgeplot", 
                            "i",
                            size = "xs",
                            status = "primary", style="position:absolute;right:1em;top:36em;"),
                          title = "The Ridgeplot shows the distribution of each feature.
                  Completely different distributions can be an indicator for noisy features.",
                          placement = "bottom"
                        )),
                     shinycssloaders::withSpinner(
                       plotOutput(outputId = "out_distPlot"))
                   )
                  )
          ),
          
          fluidRow(
            column(width=4,
                   bs4Table(
                     cardWrap = TRUE,
                     headTitles = c("Barplot - count Elements per subcluster in Feature",
                                    tooltip(
                                      actionButton(
                                        "InfoBarplotCountFeature", 
                                        "i",
                                        size = "xs",
                                        status = "primary", style="position:absolute;right:1em;top:36em;"),
                                      title = "This counter bar plot shows the amount of datapoint per feature which where assigned to each subcluster.",
                                      placement = "bottom"
                                    )),                                    
                     shinycssloaders::withSpinner(
                       plotOutput(outputId = "out_barplotCountFeature"))
                   )
            ),
            
            column(width = 4,                     
                   bs4Table(
                     cardWrap = TRUE,
                     headTitles = c("Barplot - count Elements per subcluster in Feature",
                                    tooltip(
                                      actionButton(
                                        "InfoBarplotCountSubcluster", 
                                        "i",
                                        size = "xs",
                                        status = "primary", style="position:absolute;right:1em;top:36em;"),
                                      title = "This counter bar plot describes how much datapoints each subcluster has from which feature.",
                                      placement = "bottom"
                                    )),
                     shinycssloaders::withSpinner(
                       plotOutput(outputId = "out_barplotCountCluster"))
                   )
                   ),
            
            column(width = 4,                     
                   bs4Table(
                     cardWrap = TRUE,
                     headTitles = c("Correlationplot",
                                    tooltip(
                                      actionButton(
                                        "InfoCorrelationplot", 
                                        "i",
                                        size = "xs",
                                        status = "primary", style="position:absolute;right:1em;top:36em;"),
                                      title = "The Correlationplot shows how much features correlate to each other.
                            +/-1 are perfect linear relationships. This indicates 
                            that two features can be perfectly clustered.",
                                      placement = "bottom"
                                    )),
                     shinycssloaders::withSpinner(
                       plotOutput(outputId = "out_corrPlot")
                     )
                   )
            )
          )
        ),
        
        # Contents: menu point 'Cluster Analysis'
        bs4TabItem(
          tabName = "optimalCluster",
          fluidRow(
            column(width = 8,
                   bs4Table(
                     cardWrap = TRUE,
                     headTitles = c("Clusterplot",
                          tooltip(
                                actionButton(
                                  "InfoClusterplot", 
                                  "i",
                                  size = "xs",
                                  status = "primary", style="position:absolute;right:1em;top:36em;"),
                                title = "The Clusterplot shows how the choosen clustering algorihm 
                                assigned the datapoints to unique clusters.",
                                placement = "bottom"
                              )),           
                     shinycssloaders::withSpinner(
                       plotOutput(outputId = "out_ClusterPlot"))
                   ),
                   bs4Table(
                     cardWrap = TRUE,
                     headTitles = c("Parallel Coordinate Plot",
                                    tooltip(
                                      actionButton(
                                        "InfoParallelCoordinatePlot", 
                                        "i",
                                        size = "xs",
                                        status = "primary", style="position:absolute;right:1em;top:44em;"),
                                      title = "The Parallel Coordinate Plot visualized the relationship
                                      between datapoints in different features, connected by results
                                      of the chosen cluster algorithm. 
                                      ",
                                      placement = "bottom"
                                    )),           
                     shinycssloaders::withSpinner(
                       pacoplotOutput(outputId = "out_parCoordPlot", width = '100%', height = 500))
                   )
                   
            ),
            
            column(width=4,
                   bs4Table(
                     cardWrap = TRUE,
                     headTitles = c("Optimal Amount of Clusters",
                                    tooltip(
                                      actionButton(
                                        "InfoClusterAmountAlgorithms", 
                                        "i",
                                        size = "xs",
                                        status = "primary", style="position:absolute;right:1em;top:44em;"),
                                      title = "To find the optimal amount of clusters different
                            algorithms can be used. The Gap Statistic evaluates a 
                            metric of error (the within cluster sum of squares) and 
                            calculates optimal number of clusters with the least 
                            total error. The elbow method displays the explained 
                            Variation of clusters. At the point where the plot 
                            declines the most, the best number of clusters can be 
                            found.",
                                      placement = "bottom"
                                    )), 
                     tabBox(
                       width = 12,
                       id="clustergraphs",
                       selected="Gap Statistic",
                       status = "primary",
                       solidHeader = FALSE,
                       type="tabs",
                       tabPanel(
                         title = "Gap Statistic",
                         shinycssloaders::withSpinner(
                           plotOutput(outputId = "out_gapPlot"))
                       ),
                       tabPanel(
                         title = "Elbow Method",
                         shinycssloaders::withSpinner(
                           plotOutput(outputId = "out_elbowPlot"))
                       )
                     )
                   ),
                   bs4Table(
                     cardWrap = TRUE,
                     headTitles = c("Silhouette plot",
                            tooltip(
                              actionButton(
                                "InfoSilhouette", 
                                "i",
                                size = "xs",
                                status = "primary", style="position:absolute;right:1em;top:36em;"),
                              title = "The Silhouette Plot visualize a measure of 
                              similarity between the datapoints and their own 
                              clusters in comparsion to other clusters. A score close 
                              to 1 indicates that the datapoints are close to the 
                              center of the cluster while they are between two 
                              clusters at 0.",
                              placement = "bottom"
                            )),      
                     shinycssloaders::withSpinner(
                       plotOutput(outputId = "out_silhouettePlot"))
                   )
            )
          )
        ),

        # Contents: menu point 'Upload your own dataset'
        bs4TabItem(
          tabName = "uploadDataset",
          fluidRow(
          column(width=4,
                 bs4Table(
                   cardWrap = TRUE,
                   headTitles = "",
          
          titlePanel("Uploading Files"),
          
          sidebarLayout(
          sidebarPanel(
            width = 12,
            fileInput("file1", c("Choose CSV File",
                                 tooltip(
                                   actionButton(
                                     "InfoUpload", 
                                     "i",
                                     size = "xs",
                                     status = "primary"),
                                   title = "This field allows users to inspect their own datasets within the tool and ultimately, upload it to the tool for 
                                   further analysis. The tool supports only numeric dataset.",
                                   placement = "bottom"
                                 )),
                    
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"),
                      buttonLabel = "Browse...."),
            
            tags$hr(),
            
            checkboxInput("header", "Header", TRUE),
            
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            tags$hr(),
            
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
            tags$hr(),
            
            radioButtons("uploadScale", "Scaling?",
                         choices = c(Yes = "Yes",
                                     No = "No"),
                         selected = "No"),
          ),
         mainPanel(
           
           disabled(actionButton(inputId = "uploadButton", "Upload Dataset"))
         )
          )
        )
        ),
        
        column(width=8,
               bs4Table(
                 cardWrap = TRUE,
                 headTitles = "",
        dataTableOutput("contents", width = "100%", height = "auto"))
          )
          )
      )
      )
    )
    
    # End: Body
    
  ),
  
  # End: UI
  
# Section 4: Shiny Server-------------------------------------------------------

  server = function(input, output, session) {
    
    # General Dashboard Features------------------------------------------------
  
      # Selecting and switching datasets
      values <- reactiveValues(selectedData = NULL)
      
      observeEvent(input$dataset, {
        values$selectedData <- chooseDS(input$dataset)
      })
    
      # Display feature list of the selected dataset 
      output$out_features <- renderUI({
        checkboxGroupInput(
          inputId = "selected_var",
          label = "Feature list",
          choices = c(names(values$selectedData)),
          selected = c(names(values$selectedData)), 
          width = '50px',
          inline = FALSE
        )
      })
      
      # Filter the dataset dynamically with checkbox
      filtereddata <- eventReactive({
        input$selected_var
        #input$dataset
        #values$selectedData
      },  {
        req(values$selectedData)
        
            values$selectedData %>% select(input$selected_var)
      }
      )
      
      
    # Dashboard Algorithms------------------------------------------------------  
      
      # Depending on the user's choice, perform particular clustering algorithm
      clusteringAlgorithm <- reactive({
        set.seed(123)
        
        if(input$clusteralgorithmus == "Fuzzy C-Means"){
          fanny(scale(filtereddata()), input$clusters)
        }         else{
          kmeans(filtereddata(), input$clusters)}
        
      })
      
      # Perform CLIQUE Algorithm
      subclusteringAlgorithm <- reactive({
        CLIQUE(filtereddata(), xi = 3, tau = input$DensityThreshold)
      })
      
      # Server-based function to process data from CLIQUE algorithm
      cleanedSubClusteringData_scatterplot <- reactive({
        
        clearDataForScatter(subclusteringAlgorithm(), filtereddata())
      })
      
      # Server-based function to transfrom processed data for D3js Barplot (left)
      cleanedSubClusteringData_barplot<- reactive({
        clearDataForBarPlot_ClusterByFeature(cleanedSubClusteringData_scatterplot())
      })
      
      # Server-based function to transfrom processed data for D3js Barplot (right)
      cleanedSubClusteringData_barplot_FC<- reactive({
        clearDataForBarPlot_FeatureByCluster(cleanedSubClusteringData_scatterplot())
      })
      
      # Function to filter data for general data overview via plot 'Feature by Cluster'
      barPlotFilter <- function(barPlotClick){
        subClusterResultScatter <- cleanedSubClusteringData_scatterplot()
        subClusterClickFiltered <- unique(subClusterResultScatter[
          subClusterResultScatter$SubClusterNo == barPlotClick,]$DimensionName)
        toFilter <- c("")
        for(i in 1:length(subClusterClickFiltered)){
          toFilter <- c(toFilter, subClusterClickFiltered[i])
        }
        toFilter <- toFilter[-c(1)]
        return(toFilter) # return vector with dimension names 
      }
      
    # Dashboard Features - Subspace Cluster Analysis----------------------------
      
      # Plots
      
        # D3Js Plots
        
          # Plot D3js script of the barplot "Cluster per Feature"
          output$out_barplotD3_clusterPerFeature_NEU  <- renderD3({

            r2d3(
              data = cleanedSubClusteringData_barplot(),
              script= "Javascript/Histogram_scatterplot.js",
              d3_version = "4",
              container = "div")
            
          })
          
          # Plot D3js script of the barplot "Feature per Cluster"
          output$out_barplotD3_FeaturePerCluster_NEU  <- renderD3({

            r2d3(data = cleanedSubClusteringData_barplot_FC(),
                 script= "Javascript/Histogram_generalinformation.js", 
                 d3_version = "4",
                 container = "div")
            
          })
          
      
        # General Data Overview
      
          # Create boxplot for each available feature
          output$out_boxPlot <- renderPlotly({
            # filter boxplot based on user's click on "Feature per Cluster"
            if(is.numeric(input$bar_general_clicked) == TRUE & is.null(input$bar_general_clicked) == FALSE){
              barData <- filtereddata()[barPlotFilter(input$bar_general_clicked)]
              fig <- plot_ly(stack(barData),  y = stack(barData)$values, color = stack(barData)$ind, type = "box")
              fig
            } 
            # when there was no click, plot each available feature
            else{
              fig <- plot_ly(stack(filtereddata()),  y = stack(filtereddata())$values, 
                             color = stack(filtereddata())$ind, type = "box")
              fig 
            }
          })
          
          
          # Create ridgeplot for each available feature
          output$out_distPlot <- renderPlot({
            
            # filter ridgeplot based on user's click on "Feature per Cluster"
            if(is.numeric(input$bar_general_clicked) == TRUE & is.null(input$bar_general_clicked) == FALSE){
              ggplotData <- filtereddata()[barPlotFilter(input$bar_general_clicked)]
              ggplot(stack(ggplotData), aes(x = values, y = ind, fill = ind)) +
                geom_density_ridges() +
                theme_ridges() +
                theme(legend.position = "none")
            } else {
              
              # when there was no click, plot each available feature
              ggplot(stack(filtereddata()), aes(x = values, y = ind, fill = ind)) +
                geom_density_ridges() +
                theme_ridges() +
                theme(legend.position = "none")
            }
            
          })
        
          # Create correlation plot for each available feature
      
          # function to compute the matrix of p-value
          cor.mtest <- function(mat) {
            mat <- as.matrix(mat)
            n <- ncol(mat)
            p.mat<- matrix(NA, n, n)
            diag(p.mat) <- 0
            for (i in 1:(n - 1)) {
              for (j in (i + 1):n) {
                tmp <- cor.test(mat[, i], mat[, j])
                p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
              }
            }
            colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
            p.mat
          }
          
          # Plot correlation plot for each available feature
          output$out_corrPlot <- renderPlot({
            
            # filter correlation plot based on user's click on "Feature per Cluster"
            if(is.numeric(input$bar_general_clicked) == TRUE & is.null(input$bar_general_clicked) == FALSE){
              corplotData <- filtereddata()[barPlotFilter(input$bar_general_clicked)]
              
              # exception, when only one feature is selected
              if(length(corplotData) == 1){
                corrplot(cor(corplotData), type="upper", method="pie")
              } 
              else{
                p.mat <- cor.mtest(corplotData)
                corrplot(cor(corplotData), type="upper", order="hclust", 
                         p.mat = p.mat, sig.level = input$siglevel, method="pie")}
            } 
            
            # plot correlation plot for each available feature
            else{
              p.mat <- cor.mtest(filtereddata())
              corrplot(cor(filtereddata()), type="upper", order="hclust", 
                       p.mat = p.mat, sig.level = input$siglevel, insig = "blank", method="pie")         }
          })
        
            
        # Subspace clustering analysis
          
          # Plot 3D scatterplot to visualize the created subspaces
          output$out_scatterPlotly <- renderPlotly({
            resultClique <- subclusteringAlgorithm()
            resultScatter <- cleanedSubClusteringData_scatterplot()
            # filter the input data based on user's click on "Cluster by Feature"
            if(is.null(input$bar_scatter_clicked) == FALSE){
              resultScatter <- resultScatter[resultScatter$DimensionName == input$bar_scatter_clicked,]
            } 
            scatter_fz <- plot_ly(resultScatter,
                                  x= ~ resultScatter[[4]], 
                                  y= ~ resultScatter[[3]], 
                                  z =  ~ resultScatter[[2]], 
                                  text = ~paste(
                                    "Feature: ", DimensionName,
                                    "<br>Sub Cluster No: ", SubClusterNo, 
                                    "<br>Object No: ", ObjectNo, 
                                    "<br>RowValue: ", RowValue,
                                    "<br>Count Objeects: ", nrow(subset(subset(resultScatter,
                                                                               resultScatter[[2]] == SubClusterNo),
                                                                        resultScatter[[1]] == DimensionName))
                                  ),
                                  hoverinfo = "text", 
                                  marker = list(color =  resultScatter[[2]], showscale = TRUE,
                                                colorscale='Viridis')
            )
            scatter_fz  <- scatter_fz  %>% add_markers()
            scatter_fz  <- scatter_fz  %>% layout(scene = list(xaxis = list(title = "Value of data points"),
                                                               yaxis = list(title = "Position of data points"),
                                                               zaxis = list(title = "SubCluster No.")),
                                                  annotations = list(
                                                    x = 1.09,
                                                    y = 1.05,
                                                    text = 'Cluster',
                                                    showarrow = FALSE
                                                  ))
            scatter_fz
            
          })

          # Plot barplots to visualize the distribution of datapoints
          
            # Plot distribution, ordered by features (left)
            output$out_barplotCountFeature <- renderPlot({
              ggplot(cleanedSubClusteringData_scatterplot(),
                     aes(x= factor(DimensionName),
                         fill=factor(SubClusterNo)))+
                geom_bar()+
                xlab('Feature')+
                labs(fill='SubClusterNo.') +
                geom_text(aes(label=..count..),stat="count",position=position_stack())
            })
            
            # Plot distribution, ordered by subspaces (right)
            output$out_barplotCountCluster <- renderPlot({
              ggplot(cleanedSubClusteringData_scatterplot(),
                     aes(x=factor(SubClusterNo),
                         fill=factor(DimensionName)))+
                geom_bar()+
                labs(fill='Feature')+
                xlab('SubClusterNo.') +
                geom_text(aes(label=..count..),stat="count",position=position_stack())
            })
      
    # Dashboard Features - Cluster Analysis-------------------------------------
      
      # Plots
      
      # Parallel Coordinate Plot for each clustering algorithm
      
      output$out_parCoordPlot <- renderpacoplot({

        if(input$clusteralgorithmus == "Fuzzy C-Means"){
        pacoplot(scale(filtereddata()), clusteringAlgorithm()$clustering, colorScheme = "schemeCategory10",
                 width = '100%', height = 500, labelSizes = list(yaxis = 12, yticks = 10, tooltip = 15), lineSize = NULL,
                 measures = list(avg = mean,
                                 upper = function(x){return(quantile(x, c(0.75)))},
                                 lower = function(x){return(quantile(x, c(0.25)))}))
        } 
        else{
          pacoplot(scale(filtereddata()), clusteringAlgorithm()$cluster, colorScheme = "schemeCategory10",
                   width = '100%', height = 500, labelSizes = list(yaxis = 12, yticks = 10, tooltip = 15), lineSize = NULL,
                   measures = list(avg = mean,
                                   upper = function(x){return(quantile(x, c(0.75)))},
                                   lower = function(x){return(quantile(x, c(0.25)))}))
          
      }
      })
      
      
      # Cluster plot for each clustering algorithm
      output$out_ClusterPlot <- renderPlot({
        
        if(input$clusteralgorithmus == "Fuzzy C-Means"){
          fviz_cluster(clusteringAlgorithm(), ellipse  = TRUE, repel = TRUE,
                       palette = "jco", ggtheme = theme_minimal(),
                       legend = "right")}
        else{
          fviz_cluster(clusteringAlgorithm(), data = filtereddata(), ellipse  = TRUE, repel = TRUE,
                       palette = "jco", ggtheme = theme_minimal(),
                       legend = "right")}
      })
      
      
      # Silhouette plot for each clustering algorithm
      output$out_silhouettePlot <- renderPlot({
        if(input$clusteralgorithmus == "Fuzzy C-Means"){
          fviz_silhouette(clusteringAlgorithm())}
        else{
          
          if(is.null(input$bar_scatter_clicked) == TRUE){
            fviz_silhouette((silhouette(clusteringAlgorithm()$cluster, dist(scale(filtereddata())))))}
          else{
            fviz_silhouette((silhouette(clusteringAlgorithm()$cluster, dist(scale(filtereddata())))))}
        }
      })
      
      # Plots to determine optimal number of clusters
      
        # Gap Statistics plot 
        output$out_gapPlot <- renderPlot({
          set.seed(123)
          if(input$clusteralgorithmus == "Fuzzy C-Means"){
            fviz_nbclust(scale(filtereddata()), fanny, method = "gap_stat", nboot = 10)+
              labs(subtitle = "Gap statistic method")
          }
          else{
            fviz_nbclust(scale(filtereddata()), kmeans, method = "gap_stat", nboot = 10)+
              labs(subtitle = "Gap statistic method")
          }
        })
        
        # Elbow Method 
        output$out_elbowPlot <- renderPlot({
          set.seed(123)
          if(input$clusteralgorithmus == "Fuzzy C-Means"){
            fviz_nbclust(scale(filtereddata()), FUN = fanny, method = "wss", k.max =10)}
          else{
            fviz_nbclust(scale(filtereddata()), FUN = kmeans, method = "wss", k.max = 10)
          }
        })
          
        
    # Dashboard Features - Upload your own Dataset------------------------------
        
        # Upload function 
        output$contents <- DT::renderDataTable({
        
          # check if a file is selected
          req(input$file1)
          
          # try to upload file with settings set by user
          tryCatch(
            {
              df <- data.frame(read.csv(input$file1$datapath,
                             header = input$header,
                             sep = input$sep,
                             quote = input$quote))
            },
            
          # if upload fails, print error message
            error = function(e) {
              stop(safeError(e))
            }
          )
          
          # return dataframe either scaled or not scaled based on users input
          if(input$disp == "head" && input$uploadScale == "Yes") {
            return(head(scale(df)))
          }
          else if(input$disp == "head" && input$uploadScale == "No"){
            return(head(df))
          }
          else if(input$disp == "all" && input$uploadScale == "Yes"){
            return(scale(df))
          }
          else {
            return(df)
          }
        },
        # add srollbars and print all entries in the datatable  
        options=list(scrollY = '600px', paging = FALSE,   scrollX = TRUE)
        )
        
        # Activate "Upload Dataset" button when user picked a dataset
        observeEvent(input$file1, {
          enable("uploadButton")
        })
        
        # Upload dataset in dashboard when user clicks on "Upload Dataset" button
        observeEvent(input$uploadButton, {
          values$selectedData <- data.frame(read.csv(input$file1$datapath,
                                                     header = input$header,
                                                     sep = input$sep,
                                                     quote = input$quote))
          
          values$selectedData[1:length(values$selectedData)] <- sapply(values$selectedData, as.numeric)
          values$selectedData[sapply(values$selectedData, function(x) all(is.na(x)))] <- NULL
        })
    
   
  
    # output$out_scatterAllData <- renderD3({
    #   subClusterDS <- subclusteringAlgorithm()
    #   r2d3::r2d3(data = subClusterDS, 
    #              script = "ScatterplotD3_MD.js",
    #              options = list(marginX = 3, 
    #                             marginY = 138, # kommt dazu weil sich die Werte von x und y sehr stark unterscheiden
    #                             barPadding = 0.5,
    #                             colour = "rgba(255,0,0,1)",
    #                             hovercolour = "rgba(50,50,50,1)",
    #                             xLabel = "x label",
    #                             yLabel = "Object No",
    #                             xmin = 0,
    #                             xmax = max(subClusterDS$RowValue),
    #                             ymin = 0,
    #                             ymax = max(subClusterDS$ObjectNo),
    #                             chartTitle = "Subclustered DataPoints"))
    #   
    # })
    # 
    # getColorElement <- function(element, resultScatter){
    #   if(element == "DimensionName"){
    #     varReturn <- resultScatter$DimensionName
    #   }
    #   else
    #   {
    #     varReturn <- resultScatter$SubClusterNo
    #   }
    #   return(varReturn)
    # }
    

    

        
    # output$out_scatterPlotly2 <- renderPlotly({
    #   resultScatter <- cleanedSubClusteringData_scatterplot()
    #   if(is.null(input$bar_scatter_clicked) == FALSE){
    #     resultScatter <- resultScatter[resultScatter$DimensionName == input$bar_scatter_clicked,]
    #   } 
    #   
    #   scatterVar <- plot_ly(resultScatter, 
    #                         x= ~ resultScatter[[4]],#as.list(resultScatter[selectedScalingXAxis()][[1]]), 
    #                         y= ~ resultScatter[[3]],#as.list(resultScatter[selectedScalingYAxis()][[1]]), 
    #                         color = ~ resultScatter[[2]], #getColorElement(selectedScalingColour(), resultScatter),
    #                         showlegend = FALSE,
    #                         text = ~paste(
    #                           "Feature: ", DimensionName,
    #                           "<br>Sub Cluster No: ", SubClusterNo, 
    #                           "<br>Object No: ", ObjectNo, 
    #                           "<br>RowValue: ", RowValue))
    #   
    #   scatterVar
    #   
    # })
    
  }
)
