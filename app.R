library(shiny)
library(googleVis)
library(DT)
library(RColorBrewer)
options(gvis.plot.tag = 'chart')
options(scipen = "999")
library(shinyBS)
library(shinyLP)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(broom)
library(memisc)
library(shinyjs)
library(ggthemes)
library(wordcloud)
library(tm)
library(leaflet)
library(plotly)

source('functions.R')
source('global.R')

# add tab for raw data
# summary stats second 
# add in more group
# HERE FILTERS NOT WORKING
ui <- dashboardPage(skin = 'red',
                    dashboardHeader(
                      title = "Shipping Source Data Dashboard",
                      titleWidth = 300
                    ),
                    
                    dashboardSidebar(width = 300,
                                     
                                     sidebarMenu(
                                       menuItem('Explore Data',
                                                icon = icon('table'),
                                                tabName = 'explore'),
                                       menuItem('Visualize Data',
                                                icon = icon('bar-chart-o'),
                                                tabName = 'viz_data'),
                                       menuItem('Raw Data',
                                                icon = icon('microchip'),
                                                tabName = 'raw_data'))),
                    dashboardBody(
                      
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      ),
                      tabItems(
                        tabItem(tabName = "explore",
                                h2('Explore your data'),
                                helpText('Choose one start and one end date'),
                                fluidRow(column(6,
                                                radioButtons('load_dates',
                                                             'Start Date (Load)',
                                                             choices = date_cols_load,
                                                             selected = NULL,
                                                             inline = TRUE))),
                                fluidRow(
                                  column(6,
                                         radioButtons('dis_dates',
                                                      'End date (Discharge)',
                                                      choices = date_cols_dis,
                                                      selected = NULL,
                                                      inline = TRUE)),
                                  column(6,
                                         align = 'left',
                                         dateRangeInput('date_picker',
                                                        'Choose Dates',
                                                        start = beginning_date,
                                                        end = ending_date,
                                                        format = 'yyyy/mm/dd',
                                                        separator = '--',
                                                        startview = "Month"))),
                                
                                helpText('Choose variables to group by'),
                                fluidRow(
                                         column(2,
                                                checkboxInput('load_country',
                                                              'Load Country',
                                                              value = TRUE)),
                                         column(2,
                                                checkboxInput('load_port',
                                                              'Load Port')),
                                         column(2,
                                                checkboxInput('discharge_country',
                                                              'Discharge Country')),
                                         column(2,
                                                checkboxInput('discharge_port',
                                                              'Discharge Port')),
                                         column(2,
                                                checkboxInput('vessel_name',
                                                              'Vessel Name')),
                                         column(2,
                                                checkboxInput('trading_company',
                                                              'Trading Company'))),
                                fluidRow(
                                         column(2,
                                                uiOutput('load_country_filter')),
                                         column(2,
                                                uiOutput('load_port_filter')),
                                         column(2,
                                                uiOutput('discharge_country_filter')),
                                         column(2,
                                                uiOutput('discharge_port_filter')),
                                         column(2,
                                                uiOutput('vessel_name_filter')),
                                         column(2,
                                                uiOutput('trading_company_filter'))),
                                tabsetPanel(
                                  tabPanel('Table',
                                           fluidRow(column(12,
                                                           DT::dataTableOutput('group_table')
                                           ))))),
                        
                        tabItem(tabName = "viz_data",
                                h2(''),
                                tabsetPanel(
                                  tabPanel('Weight, DWT, and Trip Duration from
                                           Load Country to Discharge Country',
                                           fluidRow(column(6,
                                                           selectInput('loading_country',
                                                                       'Choose a load country',
                                                                       choices = load_country,
                                                                       selected = 'Estonia',
                                                                       multiple = FALSE)),
                                                    column(6,
                                                           uiOutput('loading_port'))
                                                    
                                                    ), 
                                           fluidRow(column(6,
                                                           uiOutput('numeric_col')),
                                                    column(6,
                                                           dateRangeInput('date_picker_load',
                                                                          'Choose Dates',
                                                                          start = beginning_date,
                                                                          end = ending_date,
                                                                          format = 'yyyy/mm/dd',
                                                                          separator = '--',
                                                                          startview = "Year"))
                                                    ),
                                           fluidRow(column(6,
                                                           h2('Discharge Desitination'),
                                                           splitLayout(
                                                             style = "border-width:3px;  
                                                             border-style:groove;",
                                                             
                                                             plotOutput('loading_bar_weight'))
                                                           ),
                                                    column(6,
                                                           uiOutput('cloud_or_table'),
                                                           splitLayout(
                                                             style = "border-width:1px;
                                                             border-color:grey;
                                                             border-style:dashed;",
                                                             uiOutput('loading_cloud_table'))
                                                           )),
                                           fluidRow(column(6,
                                                           h2('Global Reach'),
                                                           splitLayout(
                                                             style = "border-width:3px;  
                                                             border-style:groove;",
                                                             leafletOutput('the_map'))
                                                          ),
                                                    column(6,
                                                           uiOutput('weight_or_time'),
                                                           splitLayout(
                                                             style = "border-width:3px;  
                                                             border-style:groove;",
                                                             uiOutput('loading_weight_time'))
                                                           ))
                                           )
                              )),
                        tabItem(tabName = "raw_data",
                                h2('Download shipping data'),
                                helpText(''),
                                fluidRow(column(12,
                                                DT::dataTableOutput('placeholder_table')))
                                
                        ),
                        tabItem(
                          tabName = 'about',
                          fluidPage(
                            fluidRow(
                              div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
                              h4('Built in partnership with ',
                                 a(href = 'http://databrew.cc',
                                   target='_blank', 'Databrew'),
                                 align = 'center'),
                              p('Empowering research and analysis through collaborative data science.', align = 'center'),
                              div(a(actionButton(inputId = "email", label = "info@databrew.cc", 
                                                 icon = icon("envelope", lib = "font-awesome")),
                                    href="mailto:info@databrew.cc",
                                    align = 'center')), 
                              style = 'text-align:center;'
                            )
                          )
                        )
                        
                      )))




# Define server 
server <- function(input, output) {
  
  ######### Explore Data Tab
  
  # load_country filter
  output$trading_company_filter <- renderUI({
    if(input$trading_company){
      choices <-  sort(unique(source_data$`Trading Co`))
      #remove ontario
      choices <- choices[!is.na(choices)]
      selectInput('trading_company_filter',
                  'Filter',
                  choices = choices,
                  selected = NULL,
                  multiple = TRUE)
    }
    
  })
  
  
  # load_country filter
  output$discharge_port_filter <- renderUI({
    if(input$discharge_port){
      choices <-  sort(unique(source_data$`Discharge Port`))
      #remove ontario
      choices <- choices[!is.na(choices)]
      selectInput('discharge_port_filter',
                  'Filter',
                  choices = choices,
                  selected = NULL,
                  multiple = TRUE)
    }
    
  })
  # load_country filter
  output$discharge_country_filter <- renderUI({
    if(input$discharge_country){
      choices <-  sort(unique(source_data$`Discharge Country`))
      #remove ontario
      choices <- choices[!is.na(choices)]
      selectInput('discharge_country_filter',
                  'Filter',
                  choices = choices,
                  selected = NULL,
                  multiple = TRUE)
    }
    
  })

  # load_country filter
  output$load_port_filter <- renderUI({
    if(input$load_port){
      choices <-  sort(unique(source_data$`Load Port`))
      #remove ontario
      choices <- choices[!is.na(choices)]
      selectInput('load_port_filter',
                  'Filter',
                  choices = choices,
                  selected = NULL,
                  multiple = TRUE)
    }
    
  })
  
  # load_country filter
  output$load_country_filter <- renderUI({
    if(input$load_country){
      choices <-  sort(unique(source_data$`Load Country`))
      #remove ontario
      choices <- choices[!is.na(choices)]
      selectInput('load_country_filter',
                  'Filter',
                  choices = choices,
                  selected = NULL,
                  multiple = TRUE)
    }
    
  })
  # vessel_name filter
  output$vessel_name_filter <- renderUI({
    if(input$vessel_name){
      choices <-  sort(unique(source_data$`Vessel Name`))
      #remove ontario
      choices <- choices[!is.na(choices)]
      selectInput('vessel_name_filter',
                  'Filter',
                  choices = choices,
                  selected = NULL,
                  multiple = TRUE)
    }
  })
  
  
  
  get_date <- reactive({

    df <- source_data
     load_dis_type <- c(input$load_dates, input$dis_dates)

     # get start date
    if(grepl('Arrival', load_dis_type[1])) {

      names(df)[names(df) == 'Arrival Date (Load)'] <- 'date_start'

    } else {
      names(df)[names(df) == 'Departure Date (Load)'] <- 'date_start'

    }

     # get start end
     if(grepl('Arrival', load_dis_type[2])) {

       names(df)[names(df) == 'Arrival Date (Discharge)'] <- 'date_end'

     } else {

       names(df)[names(df) == 'Departure Date (Discharge)'] <- 'date_end'

     }

     df

  })
  # 
  # create reactive object that gets all the inputs into a dataset from the explore data page
  get_data_main <- reactive({
    
    df <- get_date()
    
    # subset by date_start and date_end #HERE
    df <- df %>% dplyr::filter(date_start > input$date_picker[1] & date_end < input$date_picker[2])
    # df <- df %>% dplyr::filter(date_start > "2015-12-07" & date_end < "2017-04-26")
    
    # 
    trading_company<- discharge_port <- discharge_country <- load_port <- load_country <- vessel_name <- NULL
    
    if(input$trading_company){
      trading_company <- 'Trading Co'
    }
    
    if(input$discharge_port){
      discharge_port <- 'Discharge Port'
    }
    
    if(input$discharge_country){
      discharge_country <- 'Discharge Country'
    }
    
    if(input$vessel_name){
      vessel_name <- 'Vessel Name'
    } 
    
    if(input$load_country){
      load_country <- 'Load Country'
    } 
    
    if(input$load_port){
      load_port <- 'Load Port'
    }
    group_by_cols <- c(trading_company, discharge_port, discharge_country, load_port, load_country, vessel_name)
    
    if(is.null(group_by_cols)) {
      return(NULL)
    } else {
      x  <-  df %>% 
        group_by_at(group_by_cols) %>%  
        summarise(`Mean Weight` = round(mean(Weight, na.rm = T), 2),
                  `Mean DWT` = round(mean(DWT, na.rm = T), 2),
                  `Mean Diff Weight DWT` = round(mean(`DWT And Weight Difference`, na.rm = T), 2),
                  `Mean Trip Duration` = round(mean(`Trip Duration`, na.rm = T), 2),
                  Counts = n())
      
      if(input$trading_company & !is.null(input$trading_company_filter)) {
        x <- x %>% dplyr::filter(`Trading Co` %in% input$trading_company_filter)
      }
      
      if(input$vessel_name & !is.null(input$vessel_name_filter)) {
        x <- x %>% dplyr::filter(`Vessel Name` %in% input$vessel_name_filter)
      }
      
      if(input$discharge_country & !is.null(input$discharge_country_filter)) {
        x <- x %>% dplyr::filter(`Discharge Country` %in% input$discharge_country_filter)
      }
      
      if(input$discharge_port & !is.null(input$discharge_port_filter)) {
        x <- x %>% dplyr::filter(`Discharge Port` %in% input$discharge_port_filter)
      }
      
      if(input$load_country & !is.null(input$load_country_filter)) {
        x <- x %>% dplyr::filter(`Load Country` %in% input$load_country_filter)
      }
      
      if(input$load_port & !is.null(input$load_port_filter)) {
        x <- x %>% dplyr::filter(`Load Port` %in% input$load_port_filter)
      }
      
    }
    
    return(x)
    
  })
  
  # get data table
  output$group_table <- renderDataTable({
    x <- get_data_main()
    if(!is.null(x)){
      # colnames(x) <- c('Gender','Mean age of onset', 'Means age of sample collection', 'Mean current age')
      out <- prettify(the_table = x, download_options = TRUE)
      return(out)
    } else {
      datatable(data_frame(' ' = 'Please choose at least one variable to group by'), rownames = FALSE, options = list(dom = 't'))
    }
  })
  
  
  ########
  # Visualize data tab
  # vessel_name filter
  
  # vessel_name filter
  output$loading_port <- renderUI({
    
    
    if(is.null(input$loading_country)){
      NULL
    } else {
      
      loading_country <- input$loading_country
      
      temp <- source_data %>% dplyr::filter(`Load Country` %in% loading_country)
      loading_port_choices <- sort(unique(temp$`Load Port`))
      
      
      choices <-  sort(unique(source_data$`Vessel Name`))
      #remove ontario
      choices <- choices[!is.na(choices)]
      selectInput('loading_port',
                  'Select port from load country',
                  choices = loading_port_choices,
                  selected = NULL,
                  multiple = TRUE)
      
    }
      
  })
  
  # get numeric data for plotting
  output$numeric_col <- renderUI({
    
    if(is.null(input$loading_country)){
      NULL
    } else {
      
      selectInput('numeric_col',
                  'Choose a numeric variable
                  for the Y axis',
                  choices = c('Weight', 'Trip Duration', 'DWT', 'DWT And Weight Difference'),
                  selected = NULL,
                  multiple = FALSE)
      
    }
    
  })
  
  output$cloud_or_table <- renderUI({
    
    #radio buttons
    if(is.null(input$loading_country)){
      NULL
    } else {
      radioButtons('cloud_or_table',
                  'Table or Word Cloud',
                  choices = c('Table', 'Word Cloud'),
                  selected = 'Table',
                  inline = TRUE)
      
    }
  })
  

  get_loading_data <- reactive({
    # country <- 'Estonia'
    # port <- 'Parnu
    # get reactive objects
    df <- source_data
    country <- input$loading_country
    port <- input$loading_port
    first_date <- input$date_picker_load[1]
    second_date <- input$date_picker_load[2]

    # subset by date_start and date_end #HERE
    df <- df %>% dplyr::filter(`Departure Date (Load)` > first_date & `Arrival Date (Discharge)` < second_date)
    # df <- df %>% dplyr::filter(date_start > "2015-12-07" & date_end < "2017-04-26")

    # get country 
    loading_data <- df %>% dplyr::filter(`Load Country` %in% country)
    
    # if a loading port is specified, filter as well
    if(!is.null(port)) {
      loading_data <- loading_data %>% dplyr::filter(`Load Port` %in% port)
    } 
    return(loading_data)
  })
  
  
  output$loading_cloud_table <- renderUI({
    cloud_or_table <- input$cloud_or_table
    if(is.null(cloud_or_table)) {
      NULL
    } else {
      
      if(cloud_or_table == 'Table') {
        DT::dataTableOutput('comments_table')
      } else {
        plotOutput('comments_cloud')
      }
    }
    
  })

  output$comments_cloud <- renderPlot({
    
    cloud_data <- get_loading_data()
    cloud_or_table <- input$cloud_or_table
    if(all(is.na(cloud_data$Comments))) {
      ggplot() +
        theme_bw() +
        labs(title = 'No comments to generate words for wordcloud')
    } else {
      cloud_data <- cloud_data[!is.na(cloud_data$Comments),]
      cloud_data <- cloud_data[, c('Comments', "Arrival Date (Discharge)")]
      
      # get wordccloud
      temp_comments <- paste0(as.character(cloud_data$Comments), collapse = '/')
      wordcloud(cloud_data$Comments, random.order = FALSE,rot.per=0.35,
                colors = brewer.pal(8, "Dark2"), use.r.layout=FALSE, 
                scale=c(3.75,1.5))
    }
   
    
   
  })
  
  
  output$comments_table <- renderDataTable({
    cloud_data <- get_loading_data()
    cloud_or_table <- input$cloud_or_table
    if(all(is.na(cloud_data$Comments))) {
      DT::datatable(data_frame(' ' = 'No comments for the chosen parameters'), rownames = FALSE, options = list(dom = 't'))
    } else {
      cloud_data <- cloud_data[!is.na(cloud_data$Comments),]
      cloud_data <- cloud_data[, c('Comments', "Arrival Date (Discharge)")]
      
      # get table 
      datatable(cloud_data, colnames = c('', ''), 
                options = list(searching = FALSE, scrollY = '300px', paging = FALSE))
    }
   
    
    
  })

  # loading_bar_weight, loading_bar_trip, loading_bar_diff
  # bar plot (loading_bar)
  output$loading_bar_weight <- renderPlot({
    # plot_data <- loading_data
    
    plot_data <- get_loading_data()
    numeric_col <- input$numeric_col
    
    if(is.null(plot_data) | is.null(numeric_col)) {
      NULL
    } else {
      # get data
      x <- plot_data[ ,c('Discharge Country', 'Discharge Port', numeric_col)]
      
      # get original labels
      orig_labs <- colnames(x)
      
      # new column names for plotting
      colnames(x) <- c('V1', 'V2', 'V3')
      
      cols <- colorRampPalette(brewer.pal(n = 8, 'Spectral'))(length(unique(x$V2)))
      
      # plot data
     p<- ggplot(x, aes(V1, V3, 
                    group = V2, 
                    fill = V2)) +
        geom_bar(stat = 'identity', 
                 position = 'dodge', alpha = 0.6) +
        labs(x = '', y = orig_labs[3], 
             fill = '') +
        theme_dark(base_size = 14, 
                    base_family = 'Ubuntu') + 
        theme(legend.position="top", 
              legend.direction="horizontal",
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values = cols)
      
      p 
      
    }

  })
  
  
  ##########
  #mapping - map_text, map_title, the_map
  output$the_map <- renderLeaflet({
    
    plot_data <- get_loading_data()
    numeric_col <- input$numeric_col
    
    if(is.null(plot_data) | is.null(numeric_col)) {
      NULL
    } else {
      # get data
      x <- plot_data[ ,c('Discharge Country', 'Discharge Port', numeric_col, 
                         'Discharge Port Lon', 'Discharge Port Lat')]
      
      # get original labels
      orig_labs <- colnames(x)
      
      # get generic columns
      colnames(x) <- paste0('V', seq(1, ncol(x), 1))
      
      m = leaflet()
      
      # add openstreetmap tiles (default)
      m = addTiles(m)
      
      if(grepl('Duration', numeric_col)){
        radius_size <- x$V3^(1/2)*2
      } else {
        radius_size = x$V3^(1/4)
      }
      
      # you can now see that our maptiles are rendered
      temp = addCircleMarkers(m, 
                           lng =x$V4, # we feed the longitude coordinates 
                           lat = x$V5,
                           popup = x$V3, 
                           radius = radius_size, 
                           stroke = FALSE, 
                           fillOpacity = 0.4, 
                           color = 'darkblue',
                           group = "1 - graffiti & noise")
      
      temp
    #   # 
    #   p <- ggplot() +
    #     geom_polygon(data = world, 
    #                  aes(x = long, y = lat,
    #                      group = group)) +
    #     geom_point(data = x,
    #                aes(x = `Discharge Port Lon`,
    #                    y = `Discharge Port Lat`,
    #                    size = numeric_col),
    #                col = 'red',
    #                alpha = 0.8) +
    #     theme_classic() +
    #     theme(legend.position="none") +
    #     labs(x = '', y = '') +
    #     ggthemes::theme_map() +
    #     theme(legend.position = 'none')
    #   
    #   p <- ggplotly(p)
    #   return(p)
    }
    # 
    
  })
  
  output$weight_or_time <- renderUI({
    #radio buttons
    if(is.null(input$loading_country)){
      NULL
    } else {
      radioButtons('weight_or_time',
                   'Cumulative Weight Over Time or
                   Weight vs Duration',
                   choices = c('Weight', 'Time'),
                   selected = 'Time', 
                   inline = TRUE)
    }
  })
  
  output$loading_weight_time <- renderUI({
    weight_or_time<- input$weight_or_time
    if(is.null(weight_or_time)) {
      NULL
    } else {
      
      if(weight_or_time == 'Weight') {
        plotlyOutput('weight_plot')
      } else {
        plotlyOutput('time_plot')
      }
    }
    
  })
  
  
  output$weight_plot <- renderPlotly({
    
    plot_data <- get_loading_data()
    
    if(is.null(plot_data)) {
      NULL
    } else {
      # get data
      x <- plot_data[ ,c('Vessel Name', "DWT And Weight Difference", 'Trip Duration', 'Weight')]
      
      # get labs 
      orig_labs <- colnames(x)
      
      # get generic columns
      colnames(x) <- paste0('V', seq(1, ncol(x), 1))
      
      # plot
      p <- plot_ly(x, x = ~V2, y = ~V3, 
                   color = 'darkblue', colors = 'grey28',
                   type = 'scatter', mode = 'markers', 
                   marker = list(symbol = 'circle', size = ~x$V4^(1/3), opacity = 0.8, 
                                 sizemode = 'diameter', line = list(width = 2, color = 'darkgrey')),
                   text = ~paste0('Vessel name: ', V1),
                   legendgroup = 'Weight', 
                   showlegend = F)  %>%
        layout(title = '',
               xaxis = list(title = orig_labs[2],
                            gridcolor = 'rgb(255, 255, 255)',
                            range = c(0, ~max(V2)),
                            zerolinewidth = 1,
                            ticklen = 5,
                            gridwidth = 2),
               yaxis = list(title = orig_labs[3],
                            gridcolor = 'rgb(255, 255, 255)',
                            range = c(0, ~max(V3)),
                            zerolinewidth = 1,
                            ticklen = 5,
                            gridwith = 2),
               paper_bgcolor = 'rgb(243, 243, 243)',
               plot_bgcolor = 'rgb(243, 243, 243)') 
      
      
      return(p)

      
    }
  })

  
  
  output$time_plot <- renderPlotly({
    
    plot_data <- get_loading_data()
    
    if(is.null(plot_data)) {
      NULL
    } else {
      # get data
      x <- plot_data[ ,c('Vessel Name', "Departure Date (Load)", "Weight")]
      
      # get cumulative sum
      x$weight_cum_sum <- cumsum(x$Weight)
      
      # get labs 
      orig_labs <- colnames(x)
      
      # get generic columns
      colnames(x) <- paste0('V', seq(1, ncol(x), 1))
      
      # plot
      p <- plot_ly(x, x = ~V2, y = ~V4, 
                   color = 'darkblue', colors = 'grey28',
                   type = 'scatter', mode = 'markers', 
                   marker = list(symbol = 'circle', size = 15, opacity = 0.7, 
                                 sizemode = 'diameter', line = list(width = 1, color = 'darkgrey')),
                   text = ~paste0('Vessel name: ', V1),
                   legendgroup = 'Weight', 
                   showlegend = F)  %>%
        layout(title = '',
               xaxis = list(title = orig_labs[2],
                            gridcolor = 'rgb(255, 255, 255)',
                            range = c(~min(V2), ~max(V2)),
                            zerolinewidth = 1,
                            ticklen = 5,
                            gridwidth = 2),
               yaxis = list(title = orig_labs[3],
                            gridcolor = 'rgb(255, 255, 255)',
                            range = c(0, ~min(V4), ~max(V4)),
                            zerolinewidth = 1,
                            ticklen = 5,
                            gridwith = 2),
               paper_bgcolor = 'rgb(243, 243, 243)',
               plot_bgcolor = 'rgb(243, 243, 243)') 
      
      
      return(p)
      
    }
  })
  
}




# Run the application 
shinyApp(ui = ui,
         # htmlTemplate("www/index.html"), 
         server)

