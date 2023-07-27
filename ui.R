###################################################
#
# Title: Anthropause_app/UI.R 
# Author: Stephanie Roilo, Dresden University of Technology (TUD)
# Date: last edited on July 26th, 2023
#
###################################################

# set the language to EN
Sys.setenv(LANGUAGE="en")
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
allcn = fread("Data_249_countries_20230321.csv") 
allcn$Date = as.Date(allcn$Date)
names(allcn)[1:10] <- c("Date","Nr_records", "n_CLO", "Stringency_index","Change_park_visits","Change_time_at_home" ,"weekday",
                        "weeknr","Year","Records_100")
Encoding(allcn$Country) <- "latin1"

# User Interface
ui <- navbarPage("Anthropause app",  # name of the Shiny app
                 tabPanel("Global",   # name of the first tab
                             titlePanel("How did COVID-19 lockdowns impact biodiversity data collection?"),
                             sidebarLayout(
                               sidebarPanel(
                                 h4("Welcome to the Anthropause", symbol("sup1"), "  app!"),
                                 p("This app was developed to investigate how the COVID-19 pandemic impacted primary biodiversity data collection across the globe.\n
                                    The app makes use of data from the Global Biodiversity Information Facility (GBIF) to quantify changes in the number of biodiversity records collected through human observations, e.g. by citizen science projects and fieldwork campaigns, during the COVID-19 pandemic.
                                    Through the app, you can explore how these changes are related to the stringency of different countries' lockdown regimes and their consequent impacts on people's movements."),
                                 h4("What is this data and where does it come from?"),
                                 p(strong("Nr. of records"),": daily number of georeferenced human observations uploaded to GBIF for the selected country, downloaded on March 20th 2023 through the ", 
                                   a("GBIF API", href="https://www.gbif.org/developer/summary", target="_blank"), ". Data source: ",
                                   a("GBIF", href="https://www.gbif.org/", target="_blank")),
                                 p(strong("Stringency index"), ": the Government Response Stringency Index is a composite measure based on nine response indicators including stay-at-home requirements and restrictions on internal movements and international travels, rescaled to a value from 0 (no restrictions) to 100 (highest stringency), developed as part of the Oxford COVID-19 Government Response Tracker",
                                   symbol("sup2"),". Downloaded from: ",
                                   a("Our World In Data/covid-19-data", href="https://github.com/owid/covid-19-data/tree/master/public/data", target="_blank"), " on March 20th 2023."),
                                 p(strong("% change in visitors to parks"), ": percent change in the number of visitors to parks and outdoor spaces, relative to the median value from the pre-pandemic baseline period January 3rd to February 6th 2020. Data source: ",
                                   a("Google COVID-19 Community Mobility Reports", href="https://www.google.com/covid19/mobility/", target="_blank"), ". Accessed on March 20th 2023."),
                                 p(strong("% change in time spent at home"), ": percent change in the duration (hours) of stays in residential areas, relative to the median value from the pre-pandemic baseline period January 3rd to February 6th 2020. Data source: ",
                                   a("Google COVID-19 Community Mobility Reports", href="https://www.google.com/covid19/mobility/", target="_blank"), ". Accessed on March 20th 2023."),
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
                                     h4("Number of GBIF records per day per country"),
                                     p("In this plot, each bubble represent a country, and the size of the bubble reflects the mean daily number of georeferenced human records collected during the selected time period and stored in GBIF."),
                                     plotlyOutput("Plot_bubble"),
                                     h4("Change in the number of GBIF records compared to the previous year"),
                                     p("The number of human-collected GBIF records per day varies greatly across countries and is influenced by several factors, among which population size, participation rate in citizen science projects, research-related fieldwork campaigns, etc. 
                                        To correct for these country-specific confounding factors, the percent change in mean daily number of records is calculated using the equation: "),
                                     withMathJax('$$\\text{Change records (%) = } \\frac{ \\text{Mean daily nr. records}_{t1} - \\text{Mean daily nr. records}_{t0} }{ \\text{Mean daily nr. records}_{t0} } \\cdot 100$$'),
                                     p("where t1 corresponds to the time period selected in the left-hand sidebar, and t0 corresponds to the same time period in the previous year. The following map shows the percent change in records for each country:"),
                                     plotlyOutput("Plot_map_change"),
                                     h4("Change in GBIF records relative to the stringency index and human mobility"),
                                     p("In the next plot, the change in records is related to the mean stringency index and the mean change in park visitors, to compare the effect of the different lockdown regimes and their impacts on human mobility, on changes in the amount of biodiversity data collected across countries.
                                       Each bubble represents a country and its size reflects the mean daily number of GBIF records collected in the selected time period."),
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
                                   a("Chair of Computational Landscape Ecology", href="https://cle.geo.tu-dresden.de/", target="_blank"), "of the Dresden University of Technology for constructive feedback during the development of this app."),
                                 h4("Note:"),
                                 p("Should you encounter bugs or other issues with the app, please report them to stephanie.roilo@tu-dresden.de. Thanks!")
                               )
                             )
                  ),
                 
                 # second tab
                 tabPanel("Single country",
                          titlePanel("How did COVID-19 lockdowns impact biodiversity data collection?"),
                          sidebarLayout(
                            sidebarPanel(
                              h4("Welcome to the Anthropause", symbol("sup1"), "  app!"),
                              p("On this tab of the app, you can have a closer look at the data for specific countries. Choose a country from the drop-down menu and select a time period of interest to 
                                 see how the number of collected GBIF records changed over time, and how it correlates with changes in the stringency index and in human mobility."),
                             h4("What is this data and where does it come from?"),
                             p(strong("Nr. of records"),": daily number of georeferenced human observations uploaded to GBIF for the selected country, downloaded on March 20th 2023 through the ", 
                               a("GBIF API", href="https://www.gbif.org/developer/summary", target="_blank"), ". Data source: ",
                               a("GBIF", href="https://www.gbif.org/", target="_blank")),
                             p(strong("GBIF records in the previous year"),": daily total number of georeferenced human observations uploaded to GBIF for the selected country in the previous year, downloaded on March 20th 2023 through the ", 
                               a("GBIF API", href="https://www.gbif.org/developer/summary", target="_blank"), ". Data source: ",
                               a("GBIF", href="https://www.gbif.org/", target="_blank")),
                             p(strong("Stringency index"), ": the Government Response Stringency Index is a composite measure based on nine response indicators including stay-at-home requirements and restrictions on internal movements and international travels, rescaled to a value from 0 (no restrictions) to 100 (highest stringency), developed as part of the Oxford COVID-19 Government Response Tracker",
                               symbol("sup2"),". Downloaded from: ",
                               a("Our World In Data/covid-19-data", href="https://github.com/owid/covid-19-data/tree/master/public/data", target="_blank"), " on March 20th 2023."),
                             p(strong("% change in visitors to parks"), ": percent change in the number of visitors to parks and outdoor spaces, relative to the median value from the pre-pandemic baseline period January 3rd to February 6th 2020. Data source: ",
                               a("Google COVID-19 Community Mobility Reports", href="https://www.google.com/covid19/mobility/", target="_blank"), ". Accessed on March 20th 2023."),
                             p(strong("% change in time spent at home"), ": percent change in the duration (hours) of stays in residential areas, relative to the median value from the pre-pandemic baseline period January 3rd to February 6th 2020. Data source: ",
                               a("Google COVID-19 Community Mobility Reports", href="https://www.google.com/covid19/mobility/", target="_blank"), ". Accessed on March 20th 2023."),
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
                                a("Chair of Computational Landscape Ecology", href="https://cle.geo.tu-dresden.de/", target="_blank"), "of the Dresden University of Technology for constructive feedback during the development of this app."),
                              h4("Note:"),
                              p("Should you encounter bugs or other issues with the app, please report them to stephanie.roilo@tu-dresden.de. Thanks!")
                            )
                          )
                 )
)
