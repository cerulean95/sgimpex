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

# ----- Reading Data Files -----
export_data <- read_csv("data/Domestic Exports Country.csv")
import_data <- read_csv("data/Import Country.csv")
export_commodity_data <- read_csv("data/Domestic Exports Commodity.csv")
import_commodity_data <- read_csv("data/Import Commodity.csv")

# ----- Exports by Country -----
tidy_data <- separate(export_data, Date, c("month", "year"))
tidy_data$year <- as.Date(tidy_data$year, format = "%y")
tidy_data$year <- year(tidy_data$year)
tidy_data$year <- as.integer(tidy_data$year)
tidy_data <- tidy_data[-c(1)]
tidy_data <- gather(tidy_data, country, export_value, -year)
tidy_data <- filter(tidy_data, year != 2020)
exportdatafinal <- tidy_data %>%
    group_by(country, year) %>%
    summarise(export_value = sum(export_value))

# ----- Imports by Country -----
tidy_data <- separate(import_data, date, c("month", "year"))
tidy_data$year <- as.Date(tidy_data$year, format = "%y")
tidy_data$year <- year(tidy_data$year)
tidy_data$year <- as.integer(tidy_data$year)
tidy_data <- tidy_data[-c(1)]
tidy_data <- gather(tidy_data, country, import_value, -year)
importdatafinal <- tidy_data %>%
    group_by(country, year) %>%
    summarise(import_value = sum(import_value))

# ----- Exports by Commodity -----
tidy_data <- separate(export_commodity_data, Date, c("month", "year"))
tidy_data$year <- as.Date(tidy_data$year, format = "%y")
tidy_data$year <- year(tidy_data$year)
tidy_data$year <- as.integer(tidy_data$year)
tidy_data <- tidy_data[-c(1)]
tidy_data <- gather(tidy_data, commodity, export_value, -year)
tidy_data <- filter(tidy_data, year != 2020)
exportcommdatafinal <- tidy_data %>%
    group_by(commodity, year) %>%
    summarise(export_value = sum(export_value))

# ----- Imports by Commodity -----
tidy_data <- separate(import_commodity_data, Date, c("month", "year"))
tidy_data$year <- as.Date(tidy_data$year, format = "%y")
tidy_data$year <- year(tidy_data$year)
tidy_data$year <- as.integer(tidy_data$year)
tidy_data <- tidy_data[-c(1)]
tidy_data <- gather(tidy_data, commodity, import_value, -year)
tidy_data <- filter(tidy_data, year != 2020)
importcommdatafinal <- tidy_data %>%
    group_by(commodity, year) %>%
    summarise(import_value = sum(import_value))

# ----- Merge of Imports + Exports by Country -----
importexportdata <- merge(exportdatafinal, importdatafinal, by=c("country","year"))
importexportdata <- filter(importexportdata, year != 2020)
importexportdata <- filter(importexportdata, !grepl("Oil", country))
magicquadrantdata <- mutate(importexportdata, export_percentile = ntile(importexportdata$export_value,100))
magicquadrantdata <- mutate(magicquadrantdata, import_percentile = ntile(importexportdata$import_value,100))
magicquadrantdata$trade_balance <- magicquadrantdata$export_value - magicquadrantdata$import_value

#---------------------------------------- Total Import and Export of Singapore----------
export_country_data <- read_csv("data/Exports Country.csv")
colnames(export_country_data)[1] <- "Date"
# 1. Separate each row within date column (e.g. "1976-Jan" into "1976" and "Jan")
export_country_tidy_data <- separate(export_country_data, Date, c("month", "year"))
# 2. Converts each row within year column into Date format
export_country_tidy_data$year <- as.Date(export_country_tidy_data$year, format = "%y")
# 3. Remove first column of dataset (months), as I don't need months for time series
export_country_tidy_data <- export_country_tidy_data[-c(1)]
# 4. Use gather() to assign key-value pairs, making the dataset tall/long instead of wide
export_country_datalong <- gather(export_country_tidy_data, country, export_value, -year)
# 5. Rearrange dataset and store it in a new variable
export_country_data2 <- export_country_datalong %>%
    group_by(country, year) %>%
    summarise(export_value = sum(export_value))
# 6. Export tidied dataset to a CSV file
#write_csv(exportdata2, "data/exports_country_tidy.csv")

export_country_data2$year <- as.integer(format(export_country_data2$year, "%Y"))
export_country_data2 <- na.omit(export_country_data2)

#Commodity Data
#-----------------------------------------------------------------------------------------------------------

export_commodity_data <- read_csv("data/Exports Commodity.csv")
colnames(export_commodity_data)[1] <- "Date"
# 1. Separate each row within date column (e.g. "1976-Jan" into "1976" and "Jan")
export_commodity_tidy_data <- separate(export_commodity_data, Date, c("month", "year"))
# 2. Converts each row within year column into Date format
export_commodity_tidy_data$year <- as.Date(export_commodity_tidy_data$year, format = "%y")
# 3. Remove first column of dataset (months), as I don't need months for time series
export_commodity_tidy_data <- export_commodity_tidy_data[-c(1)]
# 4. Use gather() to assign key-value pairs, making the dataset tall/long instead of wide
export_commodity_datalong <- gather(export_commodity_tidy_data, commodity, export_value, -year)
# 5. Rearrange dataset and store it in a new variable
export_commodity_data2 <- export_commodity_datalong %>%
    group_by(commodity, year) %>%
    summarise(export_value = sum(export_value))
# 6. Export tidied dataset to a CSV file
#write_csv(exportdata2, "data/exports_country_tidy.csv")

export_commodity_data2$year <- as.integer(format(export_commodity_data2$year, "%Y"))
export_commodity_data2 <- na.omit(export_commodity_data2)

import_commodity_data <- read_csv("data/Import Commodity.csv")
colnames(import_commodity_data)[1] <- "Date"
# 1. Separate each row within date column (e.g. "1976-Jan" into "1976" and "Jan")
import_commodity_tidy_data <- separate(import_commodity_data, Date, c("month", "year"))
# 2. Converts each row within year column into Date format
import_commodity_tidy_data$year <- as.Date(import_commodity_tidy_data$year, format = "%y")
# 3. Remove first column of dataset (months), as I don't need months for time series
import_commodity_tidy_data <- import_commodity_tidy_data[-c(1)]
# 4. Use gather() to assign key-value pairs, making the dataset tall/long instead of wide
import_commodity_datalong <- gather(import_commodity_tidy_data, commodity, import_value, -year)
# 5. Rearrange dataset and store it in a new variable
import_commodity_data2 <- import_commodity_datalong %>%
    group_by(commodity, year) %>%
    summarise(import_value = sum(import_value))
# 6. import tidied dataset to a CSV file
#write_csv(importdata2, "data/imports_country_tidy.csv")

import_commodity_data2$year <- as.integer(format(import_commodity_data2$year, "%Y"))
import_commodity_data2 <- na.omit(import_commodity_data2)

reexport_commodity_data <- read_csv("data/ReExports Commodity.csv")
colnames(reexport_commodity_data)[1] <- "Date"
# 1. Separate each row within date column (e.g. "1976-Jan" into "1976" and "Jan")
reexport_commodity_tidy_data <- separate(reexport_commodity_data, Date, c("month", "year"))
# 2. Converts each row within year column into Date format
reexport_commodity_tidy_data$year <- as.Date(reexport_commodity_tidy_data$year, format = "%y")
# 3. Remove first column of dataset (months), as I don't need months for time series
reexport_commodity_tidy_data <- reexport_commodity_tidy_data[-c(1)]
# 4. Use gather() to assign key-value pairs, making the dataset tall/long instead of wide
reexport_commodity_datalong <- gather(reexport_commodity_tidy_data, commodity, reexport_value, -year)
# 5. Rearrange dataset and store it in a new variable
reexport_commodity_data2 <- reexport_commodity_datalong %>%
    group_by(commodity, year) %>%
    summarise(reexport_value = sum(reexport_value))
# 6. import tidied dataset to a CSV file
#write_csv(importdata2, "data/imports_country_tidy.csv")

reexport_commodity_data2$year <- as.integer(format(reexport_commodity_data2$year, "%Y"))
reexport_commodity_data2 <- na.omit(reexport_commodity_data2)

domesticexport_commodity_data <- read_csv("data/Domestic Exports Commodity.csv")
colnames(domesticexport_commodity_data)[1] <- "Date"
# 1. Separate each row within date column (e.g. "1976-Jan" into "1976" and "Jan")
domesticexport_commodity_tidy_data <- separate(domesticexport_commodity_data, Date, c("month", "year"))
# 2. Converts each row within year column into Date format
domesticexport_commodity_tidy_data$year <- as.Date(domesticexport_commodity_tidy_data$year, format = "%y")
# 3. Remove first column of dataset (months), as I don't need months for time series
domesticexport_commodity_tidy_data <- domesticexport_commodity_tidy_data[-c(1)]
# 4. Use gather() to assign key-value pairs, making the dataset tall/long instead of wide
domesticexport_commodity_datalong <- gather(domesticexport_commodity_tidy_data, commodity, domesticexport_value, -year)
# 5. Rearrange dataset and store it in a new variable
domesticexport_commodity_data2 <- domesticexport_commodity_datalong %>%
    group_by(commodity, year) %>%
    summarise(domesticexport_value = sum(domesticexport_value))
# 6. import tidied dataset to a CSV file
#write_csv(importdata2, "data/imports_country_tidy.csv")

domesticexport_commodity_data2$year <- as.integer(format(domesticexport_commodity_data2$year, "%Y"))
domesticexport_commodity_data2 <- na.omit(domesticexport_commodity_data2)


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
                     menuItem("IMPORTS BY COMMODITY", tabName = "IMPORTSBYCOMMODITY", icon = icon("dashboard"))),
            
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
                     menuItem("TOP RE-EXPORTERS", tabName = "TOPREEXPORTERS", icon = icon("dashboard")),
                     menuItem("TREND BY COUNTRY", tabName = "REEXPORTERSTRENDBYCOUNTRY", icon = icon("dashboard")),
                     menuItem("TREND BY COMMODITY", tabName = "REEXPORTERSTRENDBYCOMMODITY", icon = icon("dashboard")),
                     menuItem("RE-EXPORTS BY COMMODITY", tabName = "REEXPORTSBYCOMMODITY", icon = icon("dashboard")))
        )
    ),
    dashboardBody(
        tabItems(
            #------------------------------------------------MAINPAGE DASHBOARD-------------------------------------------------
            tabItem(tabName = "MAINPAGE",
                    fluidRow(
                        column(12, h1("Problem and Motivation")),
                        column(12, h4("Singapore has long been the key transport hub in the ASEAN region. However, global developments such as the trade war between the US and China, and events closer to home such as Thailand building the Kra canal may shift the balance out of Singapore's favour.")),
                        column(12, h4("It is key to correctly strategise in such uncertain times. We aim to be able to create new insight and actionable strategy as to how Singapore can maintain its position as a key transport hub.")),
                        column(12, h1("Our Objective")),
                        column(12, tags$div(
                            tags$ul(
                                tags$li(h4("Gain the overall insight on the yearly pattern of Singapore's export and import, and 
                                           the top trading partners.")),
                                tags$li(h4("Identify the demand for the product and gain insight into the customers' preference 
                                           based on the goods being exported and imported to Singapore.")),
                                tags$li(h4("Gain overall insights into Singapore's economic performance based on the Trade 
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
                        column(12, h1("Magic Quadrant for Singapore's Trade Partners")),
                        
                        column(10, plotlyOutput("magicQuadrant", height = "500px"))
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
            
            #---------------------------------------------IMPORTSBYCOMMODITY DASHBOARD----------------------------------------------
            tabItem(tabName = "IMPORTSBYCOMMODITY",
                    fluidRow(
                        column(12, h1("Import by Commodity")),
                        column(10, plotOutput("ImportCommodity", height="500px")),
                        column(2, sliderInput(
                            inputId = "FilterYearImportCommodity",
                            label = "Year",
                            min = min(import_commodity_data2$year),
                            max = max(import_commodity_data2$year),
                            value = max(import_commodity_data2$year),
                            sep = "",
                            animate = animationOptions(loop = TRUE)))
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
                        column(12, h1("Exporters Trend by Country")),
                        
                        column(10, plotlyOutput(outputId="timeExportCountry", height = "500px"))
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
                        column(10, plotOutput("ExportCommodity", height="500px")),
                        column(2, sliderInput(
                            inputId = "FilterYearExportCommodity",
                            label = "Year",
                            min = min(export_commodity_data2$year),
                            max = max(export_commodity_data2$year),
                            value = max(export_commodity_data2$year),
                            sep = "",
                            animate = animationOptions(loop = TRUE)))
                    )
            ),
            #-------------------------------------------------------------------------------------------------------------------
        
        #---------------------------------------------TOPDOMESTICEXPORTERS DASHBOARD----------------------------------------------
        tabItem(tabName = "TOPEXPORTERS",
                fluidRow(
                )
        ),
        #-------------------------------------------------------------------------------------------------------------------
        
        
        #---------------------------------------------DOMESTICEXPORTERSTRENDBYCOUNTRY DASHBOARD----------------------------------------------
        tabItem(tabName = "EXPORTERSTRENDBYCOUNTRY",
                fluidRow(
                )
        ),
        #-------------------------------------------------------------------------------------------------------------------
        
        #------------------------------------------------DOMESTICEXPORTERSTRENDBYCOMMODITY DASHBOARD---------------------------------------------------
        tabItem(tabName = "DOMESTICEXPORTSBYCOMMODITY",
                fluidRow(
                    column(12, h1("Domestic Export by Commodity")),
                    column(10, plotOutput("DomesticExportCommodity", height="500px")),
                    column(2, sliderInput(
                        inputId = "FilterYearDomesticExportCommodity",
                        label = "Year",
                        min = min(domesticexport_commodity_data2$year),
                        max = max(domesticexport_commodity_data2$year),
                        value = max(domesticexport_commodity_data2$year),
                        sep = "",
                        animate = animationOptions(loop = TRUE)))
                )
        ),
        #-------------------------------------------------------------------------------------------------------------------
        
        #---------------------------------------------DOMESTICEXPORTSBYCOMMODITY DASHBOARD----------------------------------------------

    #-------------------------------------------------------------------------------------------------------------------
    
    #---------------------------------------------TOPRE-EXPORTERS DASHBOARD----------------------------------------------
    tabItem(tabName = "TOPRE-EXPORTERS",
            fluidRow(
            )
    ),
    #-------------------------------------------------------------------------------------------------------------------
    
    
    #---------------------------------------------REEXPORTERSTRENDBYCOUNTRY DASHBOARD----------------------------------------------
    tabItem(tabName = "-REEXPORTERSTRENDBYCOUNTRY",
            fluidRow(
            )
    ),
    #-------------------------------------------------------------------------------------------------------------------
    
    #------------------------------------------------REEXPORTERSTRENDBYCOMMODITY DASHBOARD---------------------------------------------------
    tabItem(tabName = "REEXPORTERSTRENDBYCOMMODITY",
            fluidRow(
            )
    ),
    #-------------------------------------------------------------------------------------------------------------------
    
    #---------------------------------------------REEXPORTSBYCOMMODITY DASHBOARD----------------------------------------------
    tabItem(tabName = "REEXPORTSBYCOMMODITY",
            fluidRow(
                column(12, h1("Re-Export by Commodity")),
                column(10, plotOutput("ReExportCommodity", height="500px")),
                column(2, sliderInput(
                    inputId = "FilterYearReExportCommodity",
                    label = "Year",
                    min = min(export_commodity_data2$year),
                    max = max(export_commodity_data2$year),
                    value = max(export_commodity_data2$year),
                    sep = "",
                    animate = animationOptions(loop = TRUE)))
            )
    ))
#-------------------------------------------------------------------------------------------------------------------

    )
)

server <- function(input, output) {
    
    # ----- MAGIC QUADRANT DASHBOARD -----
    output$magicQuadrant <- renderPlotly({
        magicquadrantdata_2019 <- filter(magicquadrantdata, year == 2019)
        plot <- ggplot(magicquadrantdata_2019, aes(x=export_percentile, y=import_percentile, color=trade_balance, text= paste0("Country: ", country))) +
            labs(title = "Magic Quadrant of Singapore's Trade Balance", 
                 x = "Export Percentile",
                 y = "Import Percentile") +
            geom_point(alpha = 0.7) +
            scale_color_gradient(name = "Trade Balance", low = "red", high = "green") +
            geom_hline(yintercept=50, linetype="dashed", color="grey") +
            geom_vline(xintercept=50, linetype="dashed", color="grey") +
            theme(panel.background = element_blank())
        
        plot <- plot + annotate("text", x=25,y=100,label="Low Export, High Import", alpha = 0.6) +
            annotate("text", x=80,y=100,label="Top Trade Partners", alpha = 0.6) +
            annotate("text", x=25,y=45,label="Low Export, Low Import", alpha = 0.6) +
            annotate("text", x=80,y=45,label="High Export, Low Import", alpha = 0.6)
        
        ggplotly(plot)
    })
    
    # ----- Exports Country trends -----
    output$timeExportCountry <- renderPlotly({
        plot2 <- ggplot(importexportdata, aes(x = year, y = export_value, color = country)) +
            geom_line() +
            labs(title = "Time Series of Exports by Country",
                 x = "Year",
                 y = "Exports")
        
        ggplotly(plot2)
    })

        #----------------------------------------------------EXPORT COMMODITY DASHBOARD-----------------------------------------------
    output$ExportCommodity <- renderPlot({
        newTitle <- paste0("Commodity exported in ", input$FilterYearExportProduct)
        treemap <- treemap(export_commodity_data2,
                           index = "commodity",
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
    
    #----------------------------------------------------IMPORT COMMODITY DASHBOARD-----------------------------------------------
    output$ImportCommodity <- renderPlot({
        
        newTitle <- paste0("Commodity imported in ", input$FilterYearImportCommodity)
        treemap <- treemap(import_commodity_data2,
                           index = "commodity",
                           vSize="import_value",
                           type="index",
                           palette=brewer.pal(n=8, "Spectral"),
                           title=newTitle,
                           title.legend = "Amount (Million US$)",
                           align.labels = list(c("left", "top"), c("right", "bottom")), fontsize.labels=20
        )
        treemap
    })
    #---------------------------------------------------------------------------------------------------------------------------
    #----------------------------------------------------RE-EXPORT COMMODITY DASHBOARD-----------------------------------------------
    output$ReExportCommodity <- renderPlot({
        
        newTitle <- paste0("Commodity re-exported in ", input$FilterYearReExportCommodity)
        treemap <- treemap(reexport_commodity_data2,
                           index = "commodity",
                           vSize="reexport_value",
                           type="index",
                           palette=brewer.pal(n=8, "Spectral"),
                           title=newTitle,
                           title.legend = "Amount (Million US$)",
                           align.labels = list(c("left", "top"), c("right", "bottom")), fontsize.labels=20
        )
        treemap
    })
    #---------------------------------------------------------------------------------------------------------------------------
    #----------------------------------------------------RE-EXPORT COMMODITY DASHBOARD-----------------------------------------------
    output$DomesticExportCommodity <- renderPlot({
        
        newTitle <- paste0("Commodity domestically exported in ", input$FilterYearDomesticExportCommodity)
        treemap <- treemap(domesticexport_commodity_data2,
                           index = "commodity",
                           vSize="domesticexport_value",
                           type="index",
                           palette=brewer.pal(n=8, "Spectral"),
                           title=newTitle,
                           title.legend = "Amount (Million US$)",
                           align.labels = list(c("left", "top"), c("right", "bottom")), fontsize.labels=20
        )
        treemap
    })
    #---------------------------------------------------------------------------------------------------------------------------
    
    
    
     }

shinyApp(ui = ui, server = server)
