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
    summarise(export_value = sum(export_value))%>%
    ungroup()

# 6. Export tidied dataset to a CSV file
#write_csv(exportdata2, "data/exports_country_tidy.csv")

export_country_data2$year <- as.integer(format(export_country_data2$year, "%Y"))
export_country_data2=export_country_data2[export_country_data2$year<2020 & export_country_data2$year>2000,]
export_country_data2=export_country_data2[export_country_data2$country=="Indonesia" | export_country_data2$country=="Brunei" | export_country_data2$country=="Cambodia" | export_country_data2$country=="Lao Peo Dem Rep" | export_country_data2$country=="Malaysia" |export_country_data2$country=="Myanmar" | export_country_data2$country=="Philippines" | export_country_data2$country=="Thailand" | export_country_data2$country=="Vietnam" ,]
export_country_data2 <- na.omit(export_country_data2)

reexport_country_data <- read_csv("data/ReExports Country.csv")
colnames(reexport_country_data)[1] <- "Date"
# 1. Separate each row within date column (e.g. "1976-Jan" into "1976" and "Jan")
reexport_country_tidy_data <- separate(reexport_country_data, Date, c("month", "year"))
# 2. Converts each row within year column into Date format
reexport_country_tidy_data$year <- as.Date(reexport_country_tidy_data$year, format = "%y")
# 3. Remove first column of dataset (months), as I don't need months for time series
reexport_country_tidy_data <- reexport_country_tidy_data[-c(1)]
# 4. Use gather() to assign key-value pairs, making the dataset tall/long instead of wide
reexport_country_datalong <- gather(reexport_country_tidy_data, country, reexport_value, -year)
# 5. Rearrange dataset and store it in a new variable
reexport_country_data2 <- reexport_country_datalong %>%
    group_by(country, year) %>%
    summarise(reexport_value = sum(reexport_value))%>%
    ungroup()

# 6. reexport tidied dataset to a CSV file
#write_csv(reexportdata2, "data/reexports_country_tidy.csv")

reexport_country_data2$year <- as.integer(format(reexport_country_data2$year, "%Y"))
reexport_country_data2=reexport_country_data2[reexport_country_data2$year<2020 & reexport_country_data2$year>2000,]
reexport_country_data2=reexport_country_data2[substring(reexport_country_data2$country,1,3)=="Oil",]
reexport_country_data2$country <- substring(reexport_country_data2$country,6,)
reexport_country_data2=reexport_country_data2[reexport_country_data2$country=="Indonesia" | reexport_country_data2$country=="Brunei Darussalam"| reexport_country_data2$country=="Malaysia"| reexport_country_data2$country=="Thailand"| reexport_country_data2$country=="Vietnam"| reexport_country_data2$country=="Cambodia"| reexport_country_data2$country=="Myanmar" | reexport_country_data2$country=="Philippines" ,]


reexport_country_data2 <- na.omit(reexport_country_data2)

domesticexport_country_data <- read_csv("data/Domestic Exports Country.csv")
colnames(domesticexport_country_data)[1] <- "Date"
# 1. Separate each row within date column (e.g. "1976-Jan" into "1976" and "Jan")
domesticexport_country_tidy_data <- separate(domesticexport_country_data, Date, c("month", "year"))
# 2. Converts each row within year column into Date format
domesticexport_country_tidy_data$year <- as.Date(domesticexport_country_tidy_data$year, format = "%y")
# 3. Remove first column of dataset (months), as I don't need months for time series
domesticexport_country_tidy_data <- domesticexport_country_tidy_data[-c(1)]
# 4. Use gather() to assign key-value pairs, making the dataset tall/long instead of wide
domesticexport_country_datalong <- gather(domesticexport_country_tidy_data, country, domesticexport_value, -year)
# 5. Rearrange dataset and store it in a new variable
domesticexport_country_data2 <- domesticexport_country_datalong %>%
    group_by(country, year) %>%
    summarise(domesticexport_value = sum(domesticexport_value))%>%
    ungroup()

# 6. domesticexport tidied dataset to a CSV file
#write_csv(domesticexportdata2, "data/Domestic Exports_country_tidy.csv")

domesticexport_country_data2$year <- as.integer(format(domesticexport_country_data2$year, "%Y"))
domesticexport_country_data2=domesticexport_country_data2[domesticexport_country_data2$year<2020 & domesticexport_country_data2$year>2000,]
domesticexport_country_data2=domesticexport_country_data2[domesticexport_country_data2$country=="Indonesia" | domesticexport_country_data2$country=="Brunei" | domesticexport_country_data2$country=="Cambodia" | domesticexport_country_data2$country=="Lao Peo Dem Rep" | domesticexport_country_data2$country=="Malaysia" |domesticexport_country_data2$country=="Myanmar" | domesticexport_country_data2$country=="Philippines" | domesticexport_country_data2$country=="Thailand" | domesticexport_country_data2$country=="Vietnam" ,]
domesticexport_country_data2 <- na.omit(domesticexport_country_data2)

import_country_data <- read_csv("data/Import Country.csv")
colnames(import_country_data)[1] <- "Date"
# 1. Separate each row within date column (e.g. "1976-Jan" into "1976" and "Jan")
import_country_tidy_data <- separate(import_country_data, Date, c("month", "year"))
# 2. Converts each row within year column into Date format
import_country_tidy_data$year <- as.Date(import_country_tidy_data$year, format = "%y")
# 3. Remove first column of dataset (months), as I don't need months for time series
import_country_tidy_data <- import_country_tidy_data[-c(1)]
# 4. Use gather() to assign key-value pairs, making the dataset tall/long instead of wide
import_country_datalong <- gather(import_country_tidy_data, country, import_value, -year)
# 5. Rearrange dataset and store it in a new variable
import_country_data2 <- import_country_datalong %>%
    group_by(country, year) %>%
    summarise(import_value = sum(import_value))%>%
    ungroup()

# 6. import tidied dataset to a CSV file
#write_csv(importdata2, "data/imports_country_tidy.csv")

import_country_data2$year <- as.integer(format(import_country_data2$year, "%Y"))
import_country_data2=import_country_data2[import_country_data2$year<2020 & import_country_data2$year>2000,]
import_country_data2=import_country_data2[import_country_data2$country=="Indonesia" | import_country_data2$country=="Brunei" | import_country_data2$country=="Cambodia" | import_country_data2$country=="Lao Peo Dem Rep" | import_country_data2$country=="Malaysia" |import_country_data2$country=="Myanmar" | import_country_data2$country=="Philippines" | import_country_data2$country=="Thailand" | import_country_data2$country=="Vietnam" ,]
import_country_data2 <- na.omit(import_country_data2)

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
export_commodity_data2=export_commodity_data2[export_commodity_data2$year<2020 & export_commodity_data2$year>2000,]
export_commodity_data2=export_commodity_data2[!grepl(":",export_commodity_data2$commodity,fixed=TRUE),]
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
import_commodity_data2=import_commodity_data2[import_commodity_data2$year<2020 & import_commodity_data2$year>2000,]
import_commodity_data2=import_commodity_data2[!grepl(":",import_commodity_data2$commodity,fixed=TRUE),]
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
reexport_commodity_data2=reexport_commodity_data2[reexport_commodity_data2$year<2020 & reexport_commodity_data2$year>2000,]
reexport_commodity_data2=reexport_commodity_data2[!grepl(":",reexport_commodity_data2$commodity,fixed=TRUE),]
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
domesticexport_commodity_data2=domesticexport_commodity_data2[domesticexport_commodity_data2$year<2020 & domesticexport_commodity_data2$year>2000,]
domesticexport_commodity_data2=domesticexport_commodity_data2[!grepl(":",domesticexport_commodity_data2$commodity,fixed=TRUE),]
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
                     menuItem("ASEAN IMPORTERS", tabName = "TOPIMPORTERS", icon = icon("dashboard")),
                     menuItem("TREND BY COUNTRY", tabName = "IMPORTERSTRENDBYCOUNTRY", icon = icon("dashboard")),
                     menuItem("TREND BY COMMODITY", tabName = "IMPORTERSTRENDBYCOMMODITY", icon = icon("dashboard")),
                     menuItem("IMPORTS BY COMMODITY", tabName = "IMPORTSBYCOMMODITY", icon = icon("dashboard"))),
            
            menuItem("EXPORT", tabName = "TABEXPORT", icon = icon("dashboard"),
                     menuItem("ASEAN EXPORTERS", tabName = "TOPEXPORTERS", icon = icon("dashboard")),
                     menuItem("TREND BY COUNTRY", tabName = "EXPORTERSTRENDBYCOUNTRY", icon = icon("dashboard")),
                     menuItem("TREND BY COMMODITY", tabName = "EXPORTERSTRENDBYCOMMODITY", icon = icon("dashboard")),
                     menuItem("EXPORTS BY COMMODITY", tabName = "EXPORTSBYCOMMODITY", icon = icon("dashboard"))),
            
            menuItem("DOMESTIC EXPORT", tabName = "TABDOMESTICEXPORT", icon = icon("dashboard"),
                     menuItem("ASEAN DOMESTIC EXPORTERS", tabName = "TOPDOMESTICEXPORTERS", icon = icon("dashboard")),
                     menuItem("TREND BY COUNTRY", tabName = "DOMESTICEXPORTERSTRENDBYCOUNTRY", icon = icon("dashboard")),
                     menuItem("TREND BY COMMODITY", tabName = "DOMESTICEXPORTERSTRENDBYCOMMODITY", icon = icon("dashboard")),
                     menuItem("DOMESTIC EXPORTS BY COMMODITY", tabName = "DOMESTICEXPORTSBYCOMMODITY", icon = icon("dashboard"))),
            
            menuItem("RE-EXPORT", tabName = "TABREEXPORT", icon = icon("dashboard"),
                     menuItem("ASEAN RE-EXPORTERS BY OIL", tabName = "TOPREEXPORTERS", icon = icon("dashboard")),
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
                    )
            ),
            #-------------------------------------------------------------------------------------------------------------------
            
            #------------------------------------------------TOPIMPORTERS DASHBOARD---------------------------------------------------
            tabItem(tabName = "TOPIMPORTERS",
                    fluidRow(
                        column(12, h1("ASEAN Importers")),
                        column(12, plotlyOutput("AseanImporters", height="500px")))
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
                            min = min(as.numeric(import_commodity_data2$year)),
                            max = max(as.numeric(import_commodity_data2$year)),
                            value = max(as.numeric(import_commodity_data2$year)),
                            sep = "",
                            animate = animationOptions(loop = TRUE)))
                    )
            ),
            #-------------------------------------------------------------------------------------------------------------------
            
            #---------------------------------------------ASEANEXPORTERS DASHBOARD----------------------------------------------
            tabItem(tabName = "TOPEXPORTERS",
                    fluidRow(
                        column(12, h1("ASEAN Exporters")),
                        column(12, plotlyOutput("AseanExporters", height="500px")))
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
                        column(10, plotOutput("ExportCommodity", height="500px")),
                        column(2, sliderInput(
                            inputId = "FilterYearExportCommodity",
                            label = "Year",
                            min = min(as.numeric(export_commodity_data2$year)),
                            max = max(as.numeric(export_commodity_data2$year)),
                            value = max(export_commodity_data2$year),
                            sep = "",
                            animate = animationOptions(loop = TRUE)))
                    )
            ),
            #-------------------------------------------------------------------------------------------------------------------
        
        #---------------------------------------------TOPDOMESTICEXPORTERS DASHBOARD----------------------------------------------
        tabItem(tabName = "TOPDOMESTICEXPORTERS",
                fluidRow(
                    column(12, h1("ASEAN Domestic Exporters")),
                    column(12, plotlyOutput("AseanDomesticExporters", height="500px")))
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
                        min = min(as.numeric(domesticexport_commodity_data2$year)),
                        max = max(as.numeric(domesticexport_commodity_data2$year)),
                        value = max(as.numeric(domesticexport_commodity_data2$year)),
                        sep = "",
                        animate = animationOptions(loop = TRUE)))
                )
        ),
        #-------------------------------------------------------------------------------------------------------------------
        
        #---------------------------------------------DOMESTICEXPORTSBYCOMMODITY DASHBOARD----------------------------------------------

    #-------------------------------------------------------------------------------------------------------------------
    
    #---------------------------------------------TOPRE-EXPORTERS DASHBOARD----------------------------------------------
    tabItem(tabName = "TOPREEXPORTERS",
            fluidRow(
                column(12, h1("ASEAN Re-Exporters by Oil")),
                column(12, plotlyOutput("AseanReExporters", height="500px")))
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
                    min = min(as.numeric(reexport_commodity_data2$year)),
                    max = max(as.numeric(reexport_commodity_data2$year)),
                    value = max(as.numeric(reexport_commodity_data2$year)),
                    sep = "",
                    animate = animationOptions(loop = TRUE)))
            )
    ))
#-------------------------------------------------------------------------------------------------------------------

    )
)

server <- function(input, output) {
        #----------------------------------------------------EXPORT COMMODITY DASHBOARD-----------------------------------------------
    output$ExportCommodity <- renderPlot({
        export_commodity_data2 <- filter(export_commodity_data2,year == input$FilterYearExportCommodity)
        newTitle <- paste0("Commodity Exported in Millions of SGD in ", input$FilterYearExportCommodity)
        treemap <- treemap(export_commodity_data2,
                           index = c("commodity", "export_value"),
                           vSize="export_value",
                           type="index",
                           palette=brewer.pal(n=8, "Spectral"),
                           title=newTitle,
                           title.legend = "Amount (Million SGD$)",
                           align.labels = list(c("left", "top"), c("right", "bottom")), fontsize.labels=14
        )
        treemap
    })
    #---------------------------------------------------------------------------------------------------------------------------
    
    #----------------------------------------------------IMPORT COMMODITY DASHBOARD-----------------------------------------------
    output$ImportCommodity <- renderPlot({
        import_commodity_data2 <- filter(import_commodity_data2,year == input$FilterYearImportCommodity)
        newTitle <- paste0("Commodity Imported in Millions of SGD in ", input$FilterYearImportCommodity)
        treemap <- treemap(import_commodity_data2,
                           index = c("commodity", "import_value"),
                           vSize="import_value",
                           type="index",
                           palette=brewer.pal(n=8, "Spectral"),
                           title=newTitle,
                           title.legend = "Amount (Million sGD$)",
                           align.labels = list(c("left", "top"), c("right", "bottom")), fontsize.labels=14
        )
        treemap
    })
    #---------------------------------------------------------------------------------------------------------------------------

        #----------------------------------------------------RE-EXPORT COMMODITY DASHBOARD-----------------------------------------------
    output$ReExportCommodity <- renderPlot({
        reexport_commodity_data2 <- filter(reexport_commodity_data2,year == input$FilterYearReExportCommodity)
        newTitle <- paste0("Commodity Re-Exported in  in Millions of SGD in ", input$FilterYearReExportCommodity)
        treemap <- treemap(reexport_commodity_data2,
                           index = c("commodity", "reexport_value"),
                           vSize="reexport_value",
                           type="index",
                           palette=brewer.pal(n=8, "Spectral"),
                           title=newTitle,
                           title.legend = "Amount (Million SGD$)",
                           align.labels = list(c("left", "top"), c("right", "bottom")), fontsize.labels=14
        )
        treemap
    })
    #---------------------------------------------------------------------------------------------------------------------------

        #----------------------------------------------------DOMESTIC EXPORT COMMODITY DASHBOARD-----------------------------------------------
    output$DomesticExportCommodity <- renderPlot({
        domesticexport_commodity_data2 <- filter(domesticexport_commodity_data2,year == input$FilterYearDomesticExportCommodity)
        newTitle <- paste0("Commodity Domestically Exported in Millions of SGD in ", input$FilterYearDomesticExportCommodity)
        treemap <- treemap(domesticexport_commodity_data2,
                           index = c("commodity", "domesticexport_value"),
                           vSize="domesticexport_value",
                           type="index",
                           palette=brewer.pal(n=8, "Spectral"),
                           title=newTitle,
                           title.legend = "Amount (Million SGD$)",
                           align.labels = list(c("left", "top"), c("right", "bottom")), fontsize.labels=14
        )
        treemap
    })
    #---------------------------------------------------------------------------------------------------------------------------
    
    
    #---------------------------------------------ASEAN EXPORTERS GRAPH------------------------------------------------------------
    output$AseanExporters <- renderPlotly({
        p<-ggplot(data = export_country_data2, aes(x =year , y =export_value, group = country)) +
                geom_line(aes(color = country), size = 1) +
                geom_point(aes(color = country), size = 2) +
                # move the x axis labels up top
                scale_x_discrete(position = "top") +
                theme_bw() +
                # Format tweaks
                # Remove the legend
                # Remove the panel border
                theme(panel.border     = element_blank()) +
                # Remove just about everything from the y axis
                theme(panel.grid.major.y = element_blank()) +
                theme(panel.grid.minor.y = element_blank()) +
                # Remove a few things from the x axis and increase font size
                theme(axis.text.x.top      = element_text(size=12)) +
                # Remove x & y tick marks
                theme(axis.ticks       = element_blank()) +
                labs(
                       title = "ASEAN Export Value from 2000 to 2019")
        p<-ggplotly(p)
        p
    })
    #---------------------------------------------------------------------------------------------------------------------------

    #---------------------------------------------ASEAN IMPORTERS GRAPH------------------------------------------------------------
    output$AseanImporters <- renderPlotly({
        p<-ggplot(data = import_country_data2, aes(x =year , y =import_value, group = country)) +
            geom_line(aes(color = country), size = 1) +
            geom_point(aes(color = country), size = 2) +
            # move the x axis labels up top
            scale_x_discrete(position = "top") +
            theme_bw() +
            # Format tweaks
            # Remove the legend
            # Remove the panel border
            theme(panel.border     = element_blank()) +
            # Remove just about everything from the y axis
            theme(panel.grid.major.y = element_blank()) +
            theme(panel.grid.minor.y = element_blank()) +
            # Remove a few things from the x axis and increase font size
            theme(axis.text.x.top      = element_text(size=12)) +
            # Remove x & y tick marks
            theme(axis.ticks       = element_blank()) +
            labs(
                title = "ASEAN Import Value from 2000 to 2019")
        p<-ggplotly(p)
        p
    })
    #---------------------------------------------------------------------------------------------------------------------------
    
    #---------------------------------------------ASEAN DOMESTIC EXPORTERS GRAPH------------------------------------------------------------
    output$AseanDomesticExporters <- renderPlotly({
        p<-ggplot(data = domesticexport_country_data2, aes(x =year , y =domesticexport_value, group = country)) +
            geom_line(aes(color = country), size = 1) +
            geom_point(aes(color = country), size = 2) +
            # move the x axis labels up top
            scale_x_discrete(position = "top") +
            theme_bw() +
            # Format tweaks
            # Remove the legend
            # Remove the panel border
            theme(panel.border     = element_blank()) +
            # Remove just about everything from the y axis
            theme(panel.grid.major.y = element_blank()) +
            theme(panel.grid.minor.y = element_blank()) +
            # Remove a few things from the x axis and increase font size
            theme(axis.text.x.top      = element_text(size=12)) +
            # Remove x & y tick marks
            theme(axis.ticks       = element_blank()) +
            labs(
                title = "ASEAN Domestic Export Value from 2000 to 2019")
        p<-ggplotly(p)
        p
    })
    #---------------------------------------------------------------------------------------------------------------------------

    #---------------------------------------------ASEAN RE-EXPORTERS GRAPH------------------------------------------------------------
    output$AseanReExporters <- renderPlotly({
        p<-ggplot(data = reexport_country_data2, aes(x =year , y =reexport_value, group = country)) +
            geom_line(aes(color = country), size = 1) +
            geom_point(aes(color = country), size = 2) +
            # move the x axis labels up top
            scale_x_discrete(position = "top") +
            theme_bw() +
            # Format tweaks
            # Remove the legend
            # Remove the panel border
            theme(panel.border     = element_blank()) +
            # Remove just about everything from the y axis
            theme(panel.grid.major.y = element_blank()) +
            theme(panel.grid.minor.y = element_blank()) +
            # Remove a few things from the x axis and increase font size
            theme(axis.text.x.top      = element_text(size=12)) +
            # Remove x & y tick marks
            theme(axis.ticks       = element_blank()) +
            labs(
                title = "ASEAN Re-Export Value by Oil from 2000 to 2019")
        p<-ggplotly(p)
        p
    })
    #---------------------------------------------------------------------------------------------------------------------------
     }

shinyApp(ui = ui, server = server)
