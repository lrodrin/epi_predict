options(shiny.maxRequestSize = 60*1024^2) # set max file size to 60MB


source("global.R") # load global.R file


# UI ----------------------------------------------------------------------


ui <- fluidPage(
  
  useShinyjs(),
  
  # application title
  tags$head(
    tags$title("EPI-PREDICT webapp")
  ),
  titlePanel(
    div(
      h2("EPI-PREDICT webapp", style = "color: #3474A7;"),
      h4("Shiny webapp for the Exploratory Data Analysis of Spatio-Temporal Cholera Data", style = "color: #555; margin-bottom: 30px;")
    )
  ),
  
  # sidebar layout
  sidebarLayout(
    
    # sidebar panel for inputs
    sidebarPanel(
      helpText("File needs to have columns <Codigo Ine><Provincia><Municipio><Fecha><Total_invasiones><Total_defunciones><Total_poblacion>."),
      fileInput(
        inputId = "filedata",
        label = "1. Upload data. Choose csv file",
        accept = c(".csv")
      ),
      column(
        align = "center",
        width = 12, 
        actionButton("loadSampleData1", "Load Sample Data")
      ),
      br(),
      br(),
      br(),
      helpText("Upload all map files at once: shp, dbf, shx and prj."),
      fileInput(
        inputId = "filemap",
        label = "2. Upload map. Choose shapefile",
        multiple = TRUE,
        accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj")
      ),
      column(
        align = "center",
        width = 12, 
        actionButton("loadSampleData2", "Load Sample Map"),
      ),
      br(),
      br(),
      br(),
      selectizeInput(
        inputId = "countyselected",
        label = "Select county",
        choices = COUNTIES,
        selected = COUNTIES[3] # "valencia" province for default
      ),
      selectizeInput(
        inputId = "monthselected",
        label = "Select month",
        choices = MONTHS,
        selected = MONTHS[1] # all months for default
      ),
      radioButtons(
        inputId = "variableselected", 
        label = "Select variable", 
        inline = TRUE, 
        choices = VARIABLES, 
        selected = VARIABLES[1] # "Total_invasiones" for default
      ),
      br(), div(style = "border-bottom: 1px solid #ddd; margin-top: 10px; margin-bottom: 10px;"), br(),
      div(
        fluidRow(
          column(align = "right",
            width = 6,
            img(src = "imageBBVA.png", width = "100%", height = "70px")
          ),
          column(align = "left",
            width = 6,
            img(src = "imageUOC.jpg", width = "100%", height = "70px")
          )
        )
      ), 
      br(), br(), div(style = "text-align: center;", p("© 2023-2024 Laura Rodríguez-Navas. All rights reserved."))
    ),
    
    # main panel for outputs
    mainPanel(
      leafletOutput("map"),
      dygraphOutput("timetrend"),
      DTOutput("table")
    )
  )
)


# server logic ------------------------------------------------------------


server <- function(input, output) {

  # read data
  inputdata <- reactive({
    req(input$filedata)
    read.csv(input$filedata$datapath)
  })
  
  # read sample data
  sampledata <- reactive({
    req(input$loadSampleData1)
    read.csv("data/sample_data.csv")
  })

  # using isolate to avoid reavaluating the reactive expression
  data <- reactive({
    if (!is.null(input$filedata)) {
      isolate(inputdata())
    } else if (!is.null(input$loadSampleData1)) {
      isolate(sampledata())
    }
  })

  # read shapefile
  inputmap <- reactive({
    req(input$filemap)
    shpdf <- input$filemap 
    
    tempdirname <- dirname(shpdf$datapath[1]) # get the directory name of the first file in the "shpdf" data frame
    
    # rename each file in the "datapath" by appending its corresponding "name" in the "shpdf" data frame
    for (i in 1:nrow(shpdf)) { file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i])) }
    
    # use the "st_read" function from the "rgdal" package to read the shapefile
    inputmap <- st_read(paste(tempdirname, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/"), quiet = TRUE)
    inputmap
  })

  # read sample shapefile
  samplemap <- reactive({
    req(input$loadSampleData2)
    shpdf <- c("data/Municipios_IGN.shp", "data/Municipios_IGN.dbf", "data/Municipios_IGN.shx", "data/Municipios_IGN.prj")

    # use the "st_read" function from the "rgdal" package to read the local shapefile
    samplemap <- st_read(shpdf[1], quiet = TRUE)
    samplemap
  })

  # using isolate to avoid reavaluating the reactive expression
  map <- reactive({
    if (!is.null(input$filemap)) {
      isolate(inputmap())
    } else if (!is.null(input$loadSampleData2)) {
      isolate(samplemap())
    }
  })

  # filter data
  filtered_data <- reactive({
    filter_data(data(), input$countyselected, input$monthselected)
  })

  # render table
  output$table <- renderDT({
    req(filtered_data(), input$countyselected, input$monthselected)
    rownames(filtered_data) <- NULL
    datatable(filtered_data())
  })

  # render time trend
  output$timetrend <- renderDygraph({
    req(data(), input$countyselected)

    if (input$countyselected != "all") { datafiltered <- subset(data(), Provincia == input$countyselected) } # filter by county
    else { datafiltered <- data() }

    dataxts <- NULL
    municipalities <- unique(datafiltered$`Codigo.Ine`)

    for (l in 1:length(municipalities)) { # loop over municipalities

      datamunicipality <- datafiltered[datafiltered$`Codigo.Ine` == municipalities[l],] # filter by municipality

      # create xts object
      dd <- xts(
        datamunicipality[, input$variableselected],
        order.by = as.Date(paste0("1885-", datamunicipality$Fecha, "-01"))
      )
      dataxts <- cbind(dataxts, dd)
    }
    colnames(dataxts) <- municipalities
    dygraph(dataxts) %>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.2) -> d1
    d1$x$css <- "
    .dygraph-legend > span {display:none;}
    .dygraph-legend > span.highlight { display: inline; }
    "
    d1
  })

  # render map
  output$map <- renderLeaflet({
    req(filtered_data(), map(), input$countyselected, input$monthselected)
    mapfiltered <- merge(map(), filtered_data(), by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR) # merge map and data
    l <- tmap_leaflet(create_tmap(mapfiltered, mapfiltered, input$variableselected, "jenks")) # create tmap object
  })
}

# Run the application
shinyApp(ui = ui, server = server)
