options(shiny.maxRequestSize = 60*1024^2) # set the maximum upload file size to 60 megabytes


source("global.R")


# UI ----------------------------------------------------------------------


ui <- fluidPage(
  
  useShinyjs(),
  
  # app title
  tags$head(
    tags$title("EPI-PREDICT webapp")
  ),
  titlePanel(
    div(
      h2("EPI-PREDICT webapp", style = "color: #3474A7;"),
      h4("Shiny webapp for the Exploratory Data Analysis of Spatio-Temporal Cholera Data", style = "color: #555; margin-bottom: 30px;")
    )
  ),
  
  # sidebar layout with input and output defunitions
  sidebarLayout(
    
    # sidebar panel for inputs
    sidebarPanel(
      helpText("File needs to have columns <Codigo Ine><Provincia><Municipio><Fecha><Total_invasiones><Total_defunciones><Total_poblacion>."),
      fileInput(
        inputId = "filedata",
        label = "1. Upload data. Choose csv file",
        accept = c(".csv")
      ),
      helpText("Upload all map files at once: shp, dbf, shx and prj."),
      fileInput(
        inputId = "filemap",
        label = "2. Upload map. Choose shapefile",
        multiple = TRUE,
        accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj")
      ),
      selectizeInput(
        inputId = "countyselected",
        label = "Select county",
        choices = COUNTIES,
        selected = COUNTIES[3] # "valencia" county for default
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
        selected = VARIABLES[1] # invasions for default
      ),
      br(), div(style = "border-bottom: 1px solid #ddd; margin-top: 10px; margin-bottom: 10px;"), br(),
      div(
        fluidRow(
          column(
            width = 3,
            img(src = "imageBBVA.jpg", width = "100px", height = "70px")
          ),
          column(
            width = 3,
            img(src = "imageUOC.jpg", width = "190px", height = "70px"),
            style = "margin-right: 90px;"
          ),
          column(
            width = 3,
            img(src = "imageCED.png", width = "170px", height = "70px")
          )
        )
      ), 
      br(), br(), div(style = "text-align: center;", p("© 2023 Laura Rodríguez-Navas. All rights reserved."))
    ),
    
    # main panel for displaying outputs
    mainPanel(
      leafletOutput("map"),
      dygraphOutput("timetrend"),
      DTOutput("table")
    )
  )
)


# server logic ------------------------------------------------------------


server <- function(input, output, session) {
  
  data <- reactive({
    req(input$filedata)
    read.csv(input$filedata$datapath)
    
  })
  
  map <- reactive({
    req(input$filemap)
    shpdf <- input$filemap 
    
    tempdirname <- dirname(shpdf$datapath[1])
    
    # rename each file in the "datapath" by appending its corresponding "name" in the "shpdf" data frame
    for (i in 1:nrow(shpdf)) { file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i])) }
    
    # use the "st_read" function from the "rgdal" package to read the shapefile
    # concatenate the directory name and the file names with the ".shp" extension
    # filter the file names to include only those ending with ".shp"
    map <- st_read(paste(tempdirname, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/"), quiet = TRUE)
    map
    
  })
  
  filtered_data <- reactive({
    filter_data(data(), input$countyselected, input$monthselected)
    
  })
  
  output$table <- renderDT({
    req(filtered_data(), input$countyselected, input$monthselected)
    rownames(filtered_data) <- NULL
    datatable(filtered_data())
    
  })
  
   output$timetrend <- renderDygraph({
     req(data(), input$countyselected)

     if (input$countyselected != "all") { datafiltered <- subset(data(), Provincia == input$countyselected) }
     else { datafiltered <- data() }

     dataxts <- NULL
     municipalities <- unique(datafiltered$`Codigo.Ine`)
     for (l in 1:length(municipalities)) {
       datamunicipality <- datafiltered[datafiltered$`Codigo.Ine` == municipalities[l], ]
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
  
  output$map <- renderLeaflet({
    req(filtered_data(), map(), input$countyselected, input$monthselected)
    
    # add data to map
    mapfiltered <-  merge(map(), filtered_data(), by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
    
    # create tmap
    l <- tmap_leaflet(create_tmap(mapfiltered, mapfiltered, input$variableselected, "jenks"))
    
  })
}

# run the app
shinyApp(ui = ui, server = server)
