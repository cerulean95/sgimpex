library(timeDate)
library(rsconnect)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
library(reshape2)
library(shinyWidgets)
library(sf)
library(tmap)
library(leaflet)
library(ggmosaic)
library(htmltools)
library(raster)
library(rgdal)
library(rgeos)
require(maps)
require(mapdata)
library('treemap')
library(RColorBrewer)
library(htmlwidgets)
library(dplyr)
library(ggrepel)
library(scales)
library(sp)
library(data.table)
#----------------------------------------Package installation--------------------------------------------
packages = c('rsconnect', 'tinytex','plotly', 'RColorBrewer','classInt','ggthemes',
             'tidyverse', 'pivottabler', 'dplyr','shiny','shinythemes', 'lubridate',
             'sf', 'tmap', 'shinyWidgets', 'leaflet', 'ggmosaic', 'htmltools', 'raster', 'rgdal', 'rgeos', 'remotes',
             'ggrepel', 'scales', 'd3Tree', 'data.table')
for(p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p, character.only = T)
}
#--------------------------------------------------------------------------------------------------------


#---------------------------------------- Total Import and Export of Singapore----------
export_data <- read_csv("data/Exports Country.csv")
# 1. Separate each row within date column (e.g. "1976-Jan" into "1976" and "Jan")
tidy_data <- separate(export_data, Date, c("month", "year"))
# 2. Converts each row within year column into Date format
tidy_data$year <- as.Date(tidy_data$year, format = "%y")
# 3. Remove first column of dataset (months), as I don't need months for time series
tidy_data <- tidy_data[-c(1)]
# 4. Use gather() to assign key-value pairs, making the dataset tall/long instead of wide
exportdatalong <- gather(tidy_data, country, export_value, -year)
# 5. Rearrange dataset and store it in a new variable
exportdata2 <- exportdatalong %>%
    group_by(country, year) %>%
    summarise(export_value = sum(export_value))
# 6. Export tidied dataset to a CSV file
#write_csv(exportdata2, "data/exports_country_tidy.csv")

exportdata2$year <- as.integer(format(exportdata2$year, "%Y"))
exportdata2 <- na.omit(exportdata2)

export_commodity_data <- read_csv("data/Exports Commodity.csv")
# 1. Separate each row within date column (e.g. "1976-Jan" into "1976" and "Jan")
tidy_data <- separate(export_commodity_data, Date, c("month", "year"))
# 2. Converts each row within year column into Date format
tidy_data$year <- as.Date(tidy_data$year, format = "%y")
# 3. Remove first column of dataset (months), as I don't need months for time series
tidy_data <- tidy_data[-c(1)]
# 4. Use gather() to assign key-value pairs, making the dataset tall/long instead of wide
exportdatalong <- gather(tidy_data, country, export_value, -year)
# 5. Rearrange dataset and store it in a new variable
exportdata2 <- exportdatalong %>%
    group_by(country, year) %>%
    summarise(export_value = sum(export_value))
# 6. Export tidied dataset to a CSV file
#write_csv(exportdata2, "data/exports_country_tidy.csv")

exportdata2$year <- as.integer(format(exportdata2$year, "%Y"))
exportdata2 <- na.omit(exportdata2)
#-----------------------------------------------------------------------------------------------------------


#-------------------------------------------------Slope graph----------------------------------------------
#upload data
country_lists_export <- read.csv("data/SlopeGraphExportPartners.csv")
#rename column year from X2000 to 2000 and so on
colnames(country_lists_export) <- c("Destination", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", 
                                    "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

country_lists_import <- read.csv("data/SlopeGraphImportPartners.csv")
colnames(country_lists_import) <- c("Destination", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", 
                                    "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

yearList <- c("2000" = "2000", "2001" = "2001", "2002" = "2002", "2003" = "2003", "2004" = "2004", "2005" = "2005",
              "2006" = "2006", "2007" = "2007", "2008" = "2008", "2009" = "2009", "2010" = "2010","2011" = "2011",
              "2012" = "2012", "2013" = "2013", "2014" = "2014", "2015" = "2015", "2016" = "2016", "2017" = "2017",
              "2018" = "2018")
#-----------------------------------------------------------------------------------------------------------
ui <- dashboardPage(
    skin = "green",
    
    dashboardHeader(
        title = "IMPEX",
        tags$li(class = "dropdown",
                tags$style(".main-header {max-height: 45px}"),
                tags$style(".main-header .logo {height: 45px;}"),
                tags$style(".sidebar-toggle {height: 45px; padding-top: 1px !important;}"),
                tags$style(".navbar {min-height:45px !important}")
        ) 
    ),
    dashboardSidebar(
        width=300,
        sidebarMenu(
            menuItem("ABOUT US", tabName = "MAINPAGE", icon = icon("dashboard")),
            
            menuItem("HOME", tabName = "HOME", icon = icon("dashboard"),
                     menuItem("TRADE BALANCE", tabName = "TRADEBALANCE", icon = icon("dashboard")),
                     menuItem("MAGIC QUADRANT", tabName = "MAGICQUADRANT", icon = icon("dashboard"))),
            
            menuItem("IMPORT", tabName = "TABIMPORT", icon = icon("dashboard"),
                     menuItem("TOP IMPORTERS", tabName = "TOPIMPORTERS", icon = icon("dashboard")),
                     menuItem("TREND BY COUNTRY", tabName = "IMPORTERSTRENDBYCOUNTRY", icon = icon("dashboard")),
                     menuItem("TREND BY COMMODITY", tabName = "IMPORTERSTRENDBYCOMMODITY", icon = icon("dashboard")),
                     menuItem("IMPORTS BY COMMODITY", tabName = "IMPORTERSBYCOMMODITY", icon = icon("dashboard"))),
            
            menuItem("EXPORT", tabName = "TABEXPORT", icon = icon("dashboard"),
                     menuItem("TOP EXPORTERS", tabName = "TOPEXPORTERS", icon = icon("dashboard")),
                     menuItem("TREND BY COUNTRY", tabName = "EXPORTERSTRENDBYCOUNTRY", icon = icon("dashboard")),
                     menuItem("TREND BY COMMODITY", tabName = "EXPORTERSTRENDBYCOMMODITY", icon = icon("dashboard")),
                     menuItem("EXPORTS BY COMMODITY", tabName = "EXPORTSBYCOMMODITY", icon = icon("dashboard"))),
            
            menuItem("DOMESTIC EXPORT", tabName = "TABIMPORT", icon = icon("dashboard"),
                     menuItem("TOP DOMESTIC EXPORTERS", tabName = "TOPDOMESTICEXPORTERS", icon = icon("dashboard")),
                     menuItem("TREND BY COUNTRY", tabName = "DOMESTICEXPORTERSTRENDBYCOUNTRY", icon = icon("dashboard")),
                     menuItem("TREND BY COMMODITY", tabName = "DOMESTICEXPORTERSTRENDBYCOMMODITY", icon = icon("dashboard")),
                     menuItem("DOMESTIC EXPORTS BY COMMODITY", tabName = "DOMESTICEXPORTSBYCOMMODITY", icon = icon("dashboard"))),
            
            menuItem("RE-EXPORT", tabName = "TABIMPORT", icon = icon("dashboard"),
                     menuItem("TOP RE-EXPORTERS", tabName = "TOPRE-EXPORTERS", icon = icon("dashboard")),
                     menuItem("TREND BY COUNTRY", tabName = "RE-EXPORTERSTRENDBYCOUNTRY", icon = icon("dashboard")),
                     menuItem("TREND BY COMMODITY", tabName = "RE-EXPORTERSTRENDBYCOMMODITY", icon = icon("dashboard")),
                     menuItem("RE-EXPORTS BY COMMODITY", tabName = "RE-EXPORTSBYCOMMODITY", icon = icon("dashboard")))
        )
    ),
    dashboardBody(
        tabItems(
            #------------------------------------------------MAINPAGE DASHBOARD-------------------------------------------------
            tabItem(tabName = "MAINPAGE",
                    fluidRow(
                        column(12, h1("Problem and Motivation")),
                        column(12, h4("Singapore has long been the key transport hub in the ASEAN region. However, global developments such as the trade war between the US and China, and events closer to home such as Thailand building the Kra canal may shift the balance out of Singapore’s favour.")),
                        column(12, h4("It is key to correctly strategise in such uncertain times. We aim to be able to create new insight and actionable strategy as to how Singapore can maintain its position as a key transport hub.")),
                        column(12, h1("Our Objective")),
                        column(12, tags$div(
                            tags$ul(
                                tags$li(h4("Gain the overall insight on the yearly pattern of Singapore's export and import, and 
                                           the top trading partners.")),
                                tags$li(h4("Identify the demand for the product and gain insight into the customers' preference 
                                           based on the goods being exported and imported to Indonesia.")),
                                tags$li(h4("Gain overall insights into Indonesia's economic performance based on the Trade 
                                           Balance trends."))
                            )
                        ))
                    )
            ),
            #-------------------------------------------------------------------------------------------------------------------

            
            #-------------------------------------------------------TRADE BALANCE DASHBOARD------------------------------------------
            tabItem(tabName = "TRADEBALANCE",
                    fluidRow(
                   )
            ),
            #-------------------------------------------------------------------------------------------------------------------
            
            
            #-------------------------------------------------------MAGIC QUADRANT DASHBOARD------------------------------------------
            tabItem(tabName = "MAGICQUADRANT",
                    fluidRow(
                    )
            ),
            #-------------------------------------------------------------------------------------------------------------------
            
            #------------------------------------------------TOPIMPORTERS DASHBOARD---------------------------------------------------
            tabItem(tabName = "TOPIMPORTERS",
                    fluidRow(
                    )
            ),
            #-------------------------------------------------------------------------------------------------------------------
            
            #------------------------------------------------IMPORTERSTRENDBYCOUNTRY DASHBOARD---------------------------------------------------
            tabItem(tabName = "IMPORTERSTRENDBYCOUNTRY",
                    fluidRow(
                    )
            ),
            #-------------------------------------------------------------------------------------------------------------------
            
            
            #------------------------------------------------IMPORTERSTRENDBYCOMMODITY DASHBOARD---------------------------------------------------
            tabItem(tabName = "IMPORTERSTRENDBYCOMMODITY",
                    fluidRow(
                        
                    )
            ),
            #-------------------------------------------------------------------------------------------------------------------
            
            #---------------------------------------------IMPORTERSBYCOMMODITY DASHBOARD----------------------------------------------
            tabItem(tabName = "IMPORTERSBYCOMMODITY",
                    fluidRow(
                    )
            ),
            #-------------------------------------------------------------------------------------------------------------------
            
            #---------------------------------------------TOPEXPORTERS DASHBOARD----------------------------------------------
            tabItem(tabName = "TOPEXPORTERS",
                    fluidRow(
                    )
            ),
            #-------------------------------------------------------------------------------------------------------------------
            
            
            #---------------------------------------------EXPORTERSTRENDBYCOUNTRY DASHBOARD----------------------------------------------
            tabItem(tabName = "EXPORTERSTRENDBYCOUNTRY",
                    fluidRow(
                    )
            ),
            #-------------------------------------------------------------------------------------------------------------------
            
            #------------------------------------------------EXPORTERSTRENDBYCOMMODITY DASHBOARD---------------------------------------------------
            tabItem(tabName = "EXPORTERSTRENDBYCOMMODITY",
                    fluidRow(
                    )
            ),
            #-------------------------------------------------------------------------------------------------------------------
            
            #---------------------------------------------EXPORTSBYCOMMODITY DASHBOARD----------------------------------------------
            tabItem(tabName = "EXPORTSBYCOMMODITY",
                    fluidRow(
                        column(12, h1("Export by Commodity")),
                        column(10, plotOutput("ExportGoodsCategory", height="500px")),
                        column(2, sliderInput(
                            inputId = "FilterYearExportProduct",
                            label = "Year",
                            min = min(exportdata2$year),
                            max = max(exportdata2$year),
                            value = max(exportdata2$year),
                            sep = "",
                            animate = animationOptions(loop = TRUE)))
                    )
            ))
            #-------------------------------------------------------------------------------------------------------------------
    )
)

server <- function(input, output) {
        #----------------------------------------------------EXPORT PRODUCT DASHBOARD-----------------------------------------------
    output$ExportGoodsCategory <- renderPlot({

        newTitle <- paste0("Category of Product exported in ", input$FilterYearExportProduct)
        treemap <- treemap(exportdata2,
                           index = "country",
                           vSize="export_value",
                           type="index",
                           palette=brewer.pal(n=8, "Spectral"),
                           title=newTitle,
                           title.legend = "Amount (Million US$)",
                           align.labels = list(c("left", "top"), c("right", "bottom")), fontsize.labels=20
        )
        treemap
    })
    #---------------------------------------------------------------------------------------------------------------------------
    
    #---------------------------------------------SLOPE GRAPH------------------------------------------------------------
    output$slopegraph <- renderPlotly({
        country_lists <- country_lists_import
        year1input <- input$Year1
        year2input <- input$Year2
        
        if(input$ImportExportSlope == "Import"){
            country_lists = country_lists_import
        } else{
            country_lists = country_lists_export
        }
        
        year1 <- match(input$Year1,names(country_lists))
        year2 <- match(input$Year2, names(country_lists))
        
        Year_1_filter <- country_lists[,c("Destination", input$Year1)]
        Year_1_filter$Year <- input$Year1
        names(Year_1_filter)[names(Year_1_filter) == input$Year1] <- "Value"
        Year_1_filter$log <- log10(Year_1_filter$Value)
        
        Year_2_filter <- country_lists[,c("Destination", input$Year2)]
        Year_2_filter$Year <- input$Year2
        names(Year_2_filter)[names(Year_2_filter) == input$Year2] <- "Value"
        Year_2_filter$log <- log10(Year_2_filter$Value)
        
        mergeCol <- merge(Year_1_filter, Year_2_filter, by="Destination")
        mergeCol$status <- ifelse(mergeCol$Value.y - mergeCol$Value.x >0, "Increase", "Decrease")
        
        total <- rbind(Year_1_filter, Year_2_filter)
        total$status <- plyr::mapvalues(total$Destination, from = mergeCol$Destination, to = mergeCol$status)
        
        slope_graph <- ggplot(total, aes(x=Year, y=log, y1=Value, group=Destination, color=status)) +
            geom_line()+
            scale_color_manual(values=c(Increase = "#00ba38", Decrease = "#f8766d"))+
            scale_x_discrete()+
            scale_y_discrete(name = input$ImportExportSlope)+
            theme(panel.background = element_blank(),
                  axis.text.y=element_blank())
        
        ggplotly(slope_graph)
    })
    #---------------------------------------------------------------------------------------------------------------------------
}

shinyApp(ui = ui, server = server)
