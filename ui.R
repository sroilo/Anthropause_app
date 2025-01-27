###################################################
#
# Title: Anthropause_app/UI.R 
# Author: Stephanie Roilo, Dresden University of Technology (TUD) & University of Bonn
# Date: last edited on January 25th, 2025
#
###################################################

library(shiny)
library(dplyr)
library(ggplot2)
library(viridis)
library(ggExtra)
library(ISOcodes)
library(corrplot)
library(data.table)
library(DT)
library(plotly)
library(stringr)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(r2symbols)
Sys.setlocale("LC_ALL","C")

# read data with the number of GBIF records per day, the stringency index and the human mobility data for each country
allcn = fread("Data_250_countries_Jan2025snapshot_20250109.csv") 
names(allcn) <- c("countrycode", "Date", "adm0_a3", "year", "month", "day",
                  "Nr_records", "n_CLO", "Country", "Stringency_index", "Population", 
                  "Change_park_visitors","Change_time_at_home" ,"weekday", "weeknr")
allcn$Date = as.Date(allcn$Date)
Encoding(allcn$Country) <- "UTF-8"

# User Interface
ui <- navbarPage("Anthropause app",  # name of the Shiny app
                 tabPanel("Global",   # name of the first tab
                             titlePanel("How did the COVID-19 lockdown impact biodiversity data collection globally"),
                             sidebarLayout(
                               sidebarPanel(
                                 h4("Welcome to the Anthropause", symbol("sup1"), "  app!"),
                                 p("This app was developed to investigate how the COVID-19 pandemic impacted biodiversity data collection across the globe. 
                                    The app makes use of data from the Global Biodiversity Information Facility (GBIF) to quantify changes in the number of biodiversity records collected through human observations, e.g. by citizen science projects and fieldwork campaigns, during the COVID-19 pandemic.
                                    Through the app, you can explore how these changes are related to the stringency of different countries' lockdown regimes and their consequent impact on people's movements."),
                                 h4("What is this data and where does it come from?"),
                                 p(strong("Nr. of records"),": number of georeferenced human observations uploaded to GBIF (as of January 2025) for the selected country.", br(),
                                   "Data source: ",
                                 a("GBIF.org", href="https://www.gbif.org/", target="_blank"), " (01 January 2025) GBIF Occurrence Download ",
                                   a("https://doi.org/10.15468/dl.uzt932", href="https://doi.org/10.15468/dl.uzt932", target="_blank")),
                                 p(strong("Stringency index"), ": the Government Response Stringency Index is a composite measure based on nine response indicators including stay-at-home requirements and restrictions on internal movements and international travels, rescaled to a value from 0 (no restrictions) to 100 (highest stringency), developed as part of the Oxford COVID-19 Government Response Tracker",
                                   symbol("sup2"),".", br(),
                                   "Data source: ",
                                   a("Our World In Data/covid-19-data", href="https://docs.owid.io/projects/covid/en/latest/dataset.html", target="_blank"), ". Accessed on January 20th 2025."),
                                 p(strong("% change in visitors to parks"), ": percent change in the number of visitors to parks and outdoor spaces relative to the median value from the pre-pandemic baseline period January 3rd to February 6th 2020.", br(),
                                   "Data source: ",
                                   a("Google COVID-19 Community Mobility Reports", href="https://www.google.com/covid19/mobility/", target="_blank"), ". Accessed on January 20th 2025."),
                                 p(strong("% change in time spent at home"), ": percent change in the duration (hours) of stays in residential areas relative to the median value from the pre-pandemic baseline period January 3rd to February 6th 2020.", br(),
                                   "Data source: ",
                                   a("Google COVID-19 Community Mobility Reports", href="https://www.google.com/covid19/mobility/", target="_blank"), ". Accessed on January 20th 2025."),
                                 # interactive sidebar for the selection of the time period of interest
                                 dateInput(inputId = "start_date1", label = "Choose a start date:",
                                           value = "2020-03-15", min= "2020-01-01", max = "2022-10-15") ,
                                 dateInput(inputId = "end_date1", label = "Choose an end date (this must come after the start date):",
                                           value = "2020-05-01", min= "2020-01-02", max = "2022-10-15") ,
                               ),
                               # Main panel with plots and table
                               mainPanel( 
                                 tabsetPanel(
                                   tabPanel("Plot",
                                     h4("Number of GBIF records collected in the selected time period, per country"),
                                     p("In this plot, each bubble represent a country, and the size of the bubble reflects the number of georeferenced human records collected during the selected time period and stored in GBIF."),
                                     plotlyOutput("Plot_bubble"),
                                     h4("Change in the number of GBIF records compared to the previous year"),
                                     p("To assess how biodiversity data collection changed during the lockdown (or during any other time period defined in the left-hand sidebar), one can calculate the change in the number of records collected during the lockdown relative to the same period in the previous year. This is calculated as: "),
                                     withMathJax('$$\\text{Change records (%) = } \\frac{ \\text{Nr. records}_{t1} - \\text{Nr. records}_{t0} }{ \\text{Nr. records}_{t0} } \\cdot 100$$'),
                                     p("where t1 corresponds to the time period selected in the left-hand sidebar, and t0 corresponds to the same time period in the previous year.\
                                       The following map shows the percent change in records for each country:"),
                                     plotlyOutput("Plot_map_change"),
                                     h4("Change in GBIF records relative to the stringency index and human mobility"),
                                     p("In the next plot, the change in records is related to the mean stringency index and the mean change in park visitors, to compare the effect of the different lockdown regimes and their impacts on human mobility, on changes in the amount of biodiversity data collected across countries.
                                       Each bubble represents a country and its size reflects the number of GBIF records collected in the selected time period."),
                                     plotlyOutput("Plot_change")
                                   ),
                                   tabPanel("Table", DT::dataTableOutput("Table_global"))
                                 ),
                                 
                                 h4("References:"),
                                 p(symbol("sup1"), "For the definition of 'Anthropause', check out the paper by ", 
                                   a ("Rutz, et al. (2020) 'COVID-19 lockdown allows researchers to quantify the effects of human activity on wildlife'", href="https://www.nature.com/articles/s41559-020-1237-z", target="_blank")),
                                 p(symbol("sup2"), a ("Hale, et al. (2021) 'A global panel database of pandemic policies (Oxford COVID-19 Government Response Tracker)'", href="https://doi.org/10.1038/s41562-021-01079-8", target="_blank")),
                                 h4("Acknowledgments:"),
                                 p("Many thanks to Meike Will, Jan Engler, and the ",
                                   a("Chair of Computational Landscape Ecology", href="https://cle.geo.tu-dresden.de/", target="_blank"), "of the TUD Dresden University of Technology for constructive feedback during the development of this app."),
                                 h4("Note:"),
                                 p("This app is developed and mantained by Stephanie Roilo. Should you encounter bugs or other issues, please report them to stephanie.roilo@uni-bonn.de. Thanks!")
                               )
                             )
                  ),
                 
                 # second tab
                 tabPanel("Single country",
                          titlePanel("How did the COVID-19 lockdown impact biodiversity data collection globally?"),
                          sidebarLayout(
                            sidebarPanel(
                              h4("Welcome to the Anthropause", symbol("sup1"), "  app!"),
                              p("On this tab of the app, you can have a closer look at the data for specific countries. Choose a country or a territory from the drop-down menu and select a time period of interest to 
                                 see how the number of collected GBIF records changed over time, and how it correlates with changes in the stringency index and in human mobility."),
                             h4("What is this data and where does it come from?"),
                             p(strong("Nr. of records"),": number of georeferenced human observations uploaded to GBIF (as of January 2025) for the selected country.", br(),
                               "Data source: ",
                               a("GBIF.org", href="https://www.gbif.org/", target="_blank"), " (01 January 2025) GBIF Occurrence Download ",
                               a("https://doi.org/10.15468/dl.uzt932", href="https://doi.org/10.15468/dl.uzt932", target="_blank")),
                             p(strong("Previous year's records"),": number of georeferenced human observations uploaded to GBIF (as of January 2025) for the selected country.", br(),
                               "Data source: ",
                               a("GBIF.org", href="https://www.gbif.org/", target="_blank"), " (01 January 2025) GBIF Occurrence Download ",
                               a("https://doi.org/10.15468/dl.uzt932", href="https://doi.org/10.15468/dl.uzt932", target="_blank")),
                             p(strong("Stringency index"), ": the Government Response Stringency Index is a composite measure based on nine response indicators including stay-at-home requirements and restrictions on internal movements and international travels, rescaled to a value from 0 (no restrictions) to 100 (highest stringency), developed as part of the Oxford COVID-19 Government Response Tracker",
                               symbol("sup2"),".", br(),
                               "Data source: ",
                               a("Our World In Data/covid-19-data", href="https://docs.owid.io/projects/covid/en/latest/dataset.html", target="_blank"), ". Accessed on January 20th 2025."),
                             p(strong("% change in visitors to parks"), ": percent change in the number of visitors to parks and outdoor spaces relative to the median value from the pre-pandemic baseline period January 3rd to February 6th 2020.", br(),
                               "Data source: ",
                               a("Google COVID-19 Community Mobility Reports", href="https://www.google.com/covid19/mobility/", target="_blank"), ". Accessed on January 20th 2025."),
                             p(strong("% change in time spent at home"), ": percent change in the duration (hours) of stays in residential areas relative to the median value from the pre-pandemic baseline period January 3rd to February 6th 2020.", br(),
                               "Data source: ",
                               a("Google COVID-19 Community Mobility Reports", href="https://www.google.com/covid19/mobility/", target="_blank"), ". Accessed on January 20th 2025."),
                             # sidebar for interactive selection for country and period of interest
                             selectInput(inputId = "country_name", label = p("Choose a country:"), choices = as.list(sort(unique(allcn$Country))), selected = "Italy"),
                               dateInput(inputId = "start_date2", label = "Choose a start date:",
                                         value = "2020-02-15", min= "2019-01-01", max = "2022-10-15") ,
                               dateInput(inputId = "end_date2", label = "Choose an end date (this must come after the start date):",
                                         value = "2020-12-31", min= "2019-01-02", max = "2022-10-15") 
                            ),
                            # Main panel with plots and table
                            mainPanel( 
                              tabsetPanel(
                                tabPanel("Plot", 
                                         h4("Development of data in time"),
                                         plotlyOutput("Plot"), 
                                         h4("Pearson's correlation coefficient across variables"),
                                         splitLayout(cellWidths = c("70%", "30%"), 
                                                     plotOutput("Plot_corr"), 
                                                     p("Asterisks indicate whether the correlation is significant at the 0.05 (*), 0.01 (**) or 0.001 (***) level.", style = "white-space: normal;"), 
                                                     cellArgs = list(style = "vertical-align: middle") )),
                                tabPanel("Table", DT::dataTableOutput("Table"))),
                              
                              h4("References:"),
                              p(symbol("sup1"), "For the definition of 'Anthropause', check out the paper by ", 
                                a ("Rutz, et al. (2020) 'COVID-19 lockdown allows researchers to quantify the effects of human activity on wildlife'", href="https://www.nature.com/articles/s41559-020-1237-z", target="_blank")),
                              p(symbol("sup2"), a ("Hale, et al. (2021) 'A global panel database of pandemic policies (Oxford COVID-19 Government Response Tracker)'", href="https://doi.org/10.1038/s41562-021-01079-8", target="_blank")),
                              h4("Acknowledgments:"),
                              p("Many thanks to Meike Will, Jan Engler, and the ",
                                a("Chair of Computational Landscape Ecology", href="https://cle.geo.tu-dresden.de/", target="_blank"), "of the TUD Dresden University of Technology for constructive feedback during the development of this app."),
                              h4("Note:"),
                              p("This app is developed and mantained by Stephanie Roilo. Should you encounter bugs or other issues, please report them to stephanie.roilo@uni-bonn.de. Thanks!")
                            )
                          )
                 )
)
