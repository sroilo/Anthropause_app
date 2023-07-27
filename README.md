# Anthropause app
How did the COVID-19 lockdown impact biodiversity data collection across the globe? Find out with the Anthropause app: https://sroilo.shinyapps.io/Anthropause_app/

# Developed by
Stephanie Roilo (stephanie.roilo@tu-dresden.de), Dresden University of Technology (TUD), Germany.

# Abstract and rationale
The COVID-19 pandemic dramatically disrupted human activities across the globe, forcing over half of the global population into lockdown during spring 2020. Biodiversity data collection was impacted by the pandemic, too, as travel restrictions interrupted fieldwork campaigns and confined people at home. On the other hand, this global confinement also impacted the human experience of nature, and people spent more time observing other species. The Anthropause app constitutes the first global assessment of how the COVID-19 lockdown in spring 2020 impacted primary biodiversity data collection, using data from the Global Biodiversity Information Facility to quantify changes in the number of collected records relative to pre-pandemic levels. The app is an open-source, interactive webtool which allows users to explore country-level data on the daily number of GBIF records collected during the pandemic, and to related them to human mobility data (i.e. percent change in visitors to parks and outdoor areas, percent change in time spent at home, from the Google Community Mobility Reports) and to the stringency of the countries’ pandemic policies (from the Oxford COVID-19 Government Response Tracker by Hale et al. 2021). 

# Operating instructions
The Anthropause app (https://sroilo.shinyapps.io/Anthropause_app/) is an open-source, interactive Shiny app, composed of two tabs. The first tab (‘Global’) allows for cross-country comparisons of changes in collected GBIF records during the lockdown. On the left-hand side of the webpage, users can select the time period of interest (between January 1st 2020 and October 15th 2022), for which three interactive plots are produced:
1.	A scatterplot in which each bubble represents a different country, the size of the bubble reflects the mean daily number of GBIF records, and its colour reflects the mean stringency index. The countries are plotted along gradients in human mobility (the mean change in time spent at home on the x-axis and the mean change in visitors to park on the y-axis);
2.	A world map showing the percent change in the number of GBIF records (Change records) compared to the same period in the previous year. Here, countries with negative change in records are shown in red, while those with increases in records compared to the previous year are shown in blue;
3.	A scatterplot in which each bubble represents a different country, the size of the bubble reflects the mean daily number of GBIF records, and its colour reflects the mean percent change in park visitors. The countries are plotted against the change in GBIF records (x-axis) and the mean stringency index (y-axis).


An additional ‘Table’ panel allows users to inspect the raw data underlying all plots.


The second tab (‘Single country’) of the Anthropause app allows users to explore temporal trends and correlation structures in the data for individual countries. On the left-hand side of the webpage, users can choose the country and the time period of interest, for which two plots are produced:
1.	An interactive line chart displaying the daily number of GBIF records (for the selected time period as well as for the same period of the previous year), the stringency index and the human mobility metrics, plotted along time (x-axis);
2.	A correlation plot visualising the correlations between the number of GBIF records, stringency index and human mobility metrics. The Pearson’s correlation coefficient is calculated and displayed via a colour scale for each pair of variables.
As citizen science data contributions are often higher on weekends than on weekdays, information on weekends is also displayed in the line chart as well as in the correlation plot. In this tab, too, the ‘Table’ panel allows to explore the raw data behind the plots.
Because retrieving the number of GBIF records for all countries takes some time, the app runs on a pre-compiled spreadsheet (‘Data_249_countries_20230321.csv’). The R script used to compile the dataset (‘Data_download.R’), as well as the two R scripts (‘ui.R’ and ‘server.R’) necessary to run the app locally, are open-source and accessible at https://github.com/sroilo/Anthropause_app.

# Link to visuals

# Link to source location
Link to the Anthropause app: https://sroilo.shinyapps.io/Anthropause_app/
Link to GitHub repository: https://github.com/sroilo/Anthropause_app/ 

# Acknowledgements
Many thanks to Meike Will, Jan Engler, and the Chair of Computational Landscape Ecology (https://cle.geo.tu-dresden.de/) of the Dresden University of Technology for constructive feedback during the development of this app. 

# References
Hale, Thomas, et al. "A global panel database of pandemic policies (Oxford COVID-19 Government Response Tracker)." Nature human behaviour 5.4 (2021): 529-538.
