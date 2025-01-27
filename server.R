###################################################
#
# Title: Anthropause_app/server.R
# Author: Stephanie Roilo, Dresden University of Technology (TUD)
# Date: last edited on July 26th, 2023
#
###################################################

# set the language to English
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
# setwd("C:/Users/steph/Documents/Research/Papers/GBIF_COVID19/GBIF/Anthropause_app/")

# read data with the number of GBIF records per day, the stringency index and the human mobility data for each country
allcn = fread("Data_250_countries_Jan2025snapshot_20250109.csv") 
names(allcn) <- c("countrycode", "Date", "adm0_a3", "year", "month", "day",
                  "Nr_records", "n_CLO", "Country", "Stringency_index", "Population", 
                  "Change_park_visitors","Change_time_at_home" ,"weekday", "weeknr")
allcn$Date = as.Date(allcn$Date)
Encoding(allcn$Country) <- "UTF-8"

# Server
server <- function(input, output) {

  # elements for tab 1 ('Global')
  start_date1 = reactive({input$start_date1})  
  end_date1 = reactive({input$end_date1})
  
  # elements for tab 2 ('Single country')
  country_name = reactive({input$country_name})
  start_date2 = reactive({input$start_date2})  
  end_date2 = reactive({input$end_date2})
  
  ### Cross-country bubble plot:
  output$Plot_bubble <- renderPlotly({
    
    # compute means across the selected period for all countries, and the sum of records
    cntall = allcn %>% filter(Date<=end_date1() & Date>=start_date1())  %>%
      group_by(Country) %>% summarise(Stringency_index = mean(Stringency_index, na.rm=T), 
                                      Change_park_visitors = mean(Change_park_visitors, na.rm=T), 
                                      Change_time_at_home = mean(Change_time_at_home, na.rm=T),
                                      Nr_records = sum(Nr_records))
    # omit countries for which one of the variables is NA
    cntall = na.omit(cntall) 
    # round mean values to 1 digit
    cntall[,c("Stringency_index", "Change_park_visitors", "Change_time_at_home")] <- round(cntall[,c("Stringency_index", "Change_park_visitors", "Change_time_at_home")], digits=1)
    
    # set up bubble plot to compare countries
    g_bubbles = ggplot() +
      geom_point(cntall, mapping = aes(x=Change_time_at_home, y=Change_park_visitors, size=Nr_records, colour=Stringency_index, label=Country), alpha=0.8) +  
      scale_size(range=c(1,20)) +
      scale_color_viridis(option="F", direction=-1) +
      theme_minimal() + theme(legend.text=element_text(size=12), legend.title=element_text(size=12), axis.title=element_text(size=12)) +   
      labs(x="Change in time spent at home (%)", y="Change in visitors to parks (%)", colour="Stringency index", size=" Nr. of records")
    # make the plot interactive
    p_bubbles = ggplotly(g_bubbles, dynamicTicks=TRUE, tooltip = c("Country", "x","y", "Nr_records", "Stringency_index")) 
    # clean the labels of the legend from brackets
    for (i in 1:length(p_bubbles$x$data)){
      if (!is.null(p_bubbles$x$data[[i]]$name)){
        p_bubbles$x$data[[i]]$name =  sub(".", "",str_split(p_bubbles$x$data[[i]]$name,",")[[1]][1])
      }
    }
    #plot
    p_bubbles
  })

  
  ### World map, with GBIF records on top:
  output$Plot_map_change <- renderPlotly({
    
    # compute means across the selected period for all countries 
    cntall = allcn %>% filter(Date<=end_date1() & Date>=start_date1())  %>%
      group_by(Country) %>% summarise(Nr_records = sum(Nr_records),
                                      adm0_a3 = first(adm0_a3))
    # compute also the mean daily records in the same period of the previous year, for comparison
    cntall_PY = allcn %>% filter(Date<= (end_date1() - years(1)) & Date>= (start_date1() - years(1)) )  %>%
      group_by(Country) %>% summarise_at(vars(Nr_records), sum)
    # merge to the other dataframe
    cntall$Records_prev_year = cntall_PY$Nr_records[match(cntall$Country, cntall_PY$Country)]
    # compute the percent change in records collected between the two consecutive years
    cntall$Change_records = ifelse(cntall$Records_prev_year>0, (cntall$Nr_records - cntall$Records_prev_year)/cntall$Records_prev_year*100, NA)
    # round to one digit
    cntall$Change_records = round(cntall$Change_records, digits=1)
    # load world country data, and join the COVID and GBIF data to it
    wmap = ne_countries(scale = "medium", type = "countries", returnclass = c( "sf")) %>% select(c("name_long","formal_en", "adm0_a3"))
    wmap = merge(wmap, cntall, by = "adm0_a3", all=T)
    # fill in information on missing countries' names for display in the interactive map
    wmap$Country = ifelse(is.na(wmap$Country), wmap$name_long, wmap$Country)
    # delete empty geometries
    wmap = dplyr::filter(wmap, !sf::st_is_empty(geometry))
    Encoding(wmap$Country) <- "UTF-8"
    
    # create map
    changemap <- ggplot() +
      geom_sf(data = wmap, mapping = aes(fill = Change_records, label= Country), color = "black", size = 0.1) +
      scale_fill_gradient2(limits=c(-100, 100), oob = scales::squish, breaks = c(-100, -50, 0 , 50, 100), labels = c("-100", "-50", "0", "50", "100+")) +
      theme_minimal() + guides(shape = "none") + ylim(c(-52, 100)) +
      labs(x="", y="", fill = "Change records (%)", color="", size="")
    
    # make map interactive and plot it
    p_wmapc = ggplotly(changemap, dynamicTicks=F, tooltip = c("Country", "Change_records")) 
    p_wmapc
    
  })
  
  
  ### Cross-country scatter plot of change in daily records compared to previous year:
  output$Plot_change <- renderPlotly({
    
    # compute means across the selected period for all countries, and the sum of records
    cntall = allcn %>% filter(Date<=end_date1() & Date>=start_date1())  %>%
      group_by(Country) %>% summarise(Stringency_index = mean(Stringency_index, na.rm=T), 
                                      Change_park_visitors = mean(Change_park_visitors, na.rm=T), 
                                      Change_time_at_home = mean(Change_time_at_home, na.rm=T),
                                      Nr_records = sum(Nr_records))
    # omit countries for which one of the variables is NA
    cntall = na.omit(cntall) 
    # compute also the sum of records in the same period of the previous year, for comparison
    cntall_PY = allcn %>% filter(Date<= (end_date1() - years(1)) & Date>= (start_date1() - years(1)) )  %>%
      group_by(Country) %>% summarise_at(vars(Nr_records), sum)
    # merge to the other dataframe
    cntall$Records_prev_year = cntall_PY$Nr_records[match(cntall$Country, cntall_PY$Country)]
    # compute the percent change in records collected between the two consecutive years
    cntall$Change_records = ifelse(cntall$Records_prev_year>0, (cntall$Nr_records - cntall$Records_prev_year)/cntall$Records_prev_year*100, NA)
    
    # round mean values to 1 digit
    cntall[,c("Stringency_index", "Change_park_visitors", "Change_time_at_home", "Change_records")] <- round(cntall[,c("Stringency_index", "Change_park_visitors", "Change_time_at_home", "Change_records")], digits=1)
    
    # set up bubble plot to compare countries
    g_change = ggplot() +
      geom_point(cntall, mapping = aes(x=Change_records, y=Stringency_index, size=Nr_records, colour=Change_park_visitors, label=Country), alpha=0.8) +  
      scale_size(range=c(1,20)) +
      scale_color_viridis(option="D", direction=1) +
      theme_minimal() + theme(legend.text=element_text(size=12), legend.title=element_text(size=12), axis.title=element_text(size=12)) +   
      labs(x="Change records (%)", y="Stringency index", colour="Change in park visitors (%)", size="Records per day")
    # make interactive
    p_change = ggplotly(g_change, dynamicTicks=TRUE, tooltip = c("Country", "x","y", "Nr_records", "Change_records")) 
    # clean the labels of the legend from brackets
    for (i in 1:length(p_change$x$data)){
      if (!is.null(p_change$x$data[[i]]$name)){
        p_change$x$data[[i]]$name =  sub(".", "",str_split(p_change$x$data[[i]]$name,",")[[1]][1])
      }
    }
    #plot
    p_change
  })
  
  ### Interactive table, to view the data underlying the global, cross-country plots:
  output$Table_global <- DT::renderDataTable({
    # compute means across the selected period for all countries, and sum of records
    cntall = allcn %>% filter(Date<=end_date1() & Date>=start_date1())  %>%
      group_by(Country) %>% summarise(Stringency_index = mean(Stringency_index, na.rm=T), 
                                      Change_park_visitors = mean(Change_park_visitors, na.rm=T), 
                                      Change_time_at_home = mean(Change_time_at_home, na.rm=T),
                                      Nr_records = sum(Nr_records))
    # compute also the sum of records in the same period of the previous year, for comparison
    cntall_PY = allcn %>% filter(Date<= (end_date1() - years(1)) & Date>= (start_date1() - years(1)) )  %>%
      group_by(Country) %>% summarise_at(vars(Nr_records), sum)
    # bind to the other dataframe
    cntall$Records_prev_year = cntall_PY$Nr_records[match(cntall$Country, cntall_PY$Country)]
    # compute the percent change in records collected between the two consecutive years
    cntall$Change_records = ifelse(cntall$Records_prev_year>0, (cntall$Nr_records - cntall$Records_prev_year)/cntall$Records_prev_year*100, NA)
    
    # round mean values to 1 digit
    cntall[,c("Stringency_index", "Change_park_visitors", "Change_time_at_home", "Change_records")] <- round(cntall[,c("Stringency_index", "Change_park_visitors", "Change_time_at_home", "Change_records")], digits=1)
    
    # reorder columns and change names
    cntall = cntall[,c("Country", "Nr_records", "Records_prev_year","Change_records", "Stringency_index", "Change_park_visitors", "Change_time_at_home")]
    names(cntall) <- c("Country", "Nr. of records", "Previous year's records", "Change records (%)", "Stringency index", "% change park visitors", "% change time at home")  
    # create interactive data.table
    datatable(cntall, class=c("compact", "nowrap", "hover", "stripe"), rownames=F)
    
  }) 
  
  
  ### Single country time-explicit plot:
  output$Plot <- renderPlotly({

    # filter to the selected country and time period
    onecn = allcn %>% filter(Country==country_name()) 
    dates <- onecn %>% filter(Date<=end_date2() & Date>=start_date2())
    
    # add number of GBIF records collected in the previous year, for comparison
    dates$Prev_year = dates$Date - years(1)
    dates$Records_prev_year <- onecn$Nr_records[match(dates$Prev_year, allcn$Date)]

    # create a dataframe with all weekends days
    wed = dates %>% filter(weekday %in% c("Saturday", "Sunday")) %>% 
      group_by(year, weeknr) %>% summarise(xmin = min(Date), xmax = max(Date))
    
    # check how to best scale the plot
    if (median(dates$Nr_records) > 100000) {
      dates$Records_10000 = dates$Nr_records/10000
      dates$Records_prev_10000 = dates$Records_prev_year/10000
      # create main plot
      g = ggplot() + 
        # highlight weekends
        geom_rect(data=wed, aes(xmin = xmin, xmax = xmax, 
                                # cannot use -Inf/+ Inf with ggplotly, have to set fixed ymin and ymax..
                                ymin = min(dates[,c("Stringency_index", "Records_10000", "Records_prev_10000", "Change_park_visitors", "Change_time_at_home")], na.rm=T),
                                ymax = max(dates[,c("Stringency_index", "Records_10000", "Records_prev_10000", "Change_park_visitors", "Change_time_at_home")], na.rm=T),
                                fill="Weekend"), 
                  alpha = 0.2) +
        # plot the GBIF and stringency index and movement data
        geom_line(dates, mapping=aes(x=Date, y=Stringency_index, colour="Stringency index")) +
        geom_line(dates, mapping=aes(x=Date, y=Records_10000, colour="Records (tens of thousands)")) +
        geom_line(dates, mapping=aes(x=Date, y=Records_prev_10000, colour="Previous year's records (tens of thousands)")) +
        geom_line(dates, mapping=aes(x=Date, y=Change_park_visitors, colour="% change in visitors to parks")) +
        geom_line(dates, mapping=aes(x=Date, y=Change_time_at_home, colour="% change in time spent at home"))+ 
        theme_minimal() +
        scale_fill_manual(name= '', values = c("Weekend" = 'orange'))  +
        scale_color_manual(name = "", values = c("Records (tens of thousands)" = "blue",
                                                 "Previous year's records (tens of thousands)" = "lightblue",
                                                 "Stringency index" = "red", 
                                                 "% change in visitors to parks" = "green",
                                                 "% change in time spent at home" = "brown")) +
        theme(legend.text=element_text(size=12),legend.title=element_text(size=12),axis.title=element_text(size=12)) +      
        labs(y="", x="Date") 
      # transform the plot in an interactive one with ggplotly
      p = ggplotly(g, tooltip=c("Date", "y")) %>%  layout(hovermode = "x unified") 
      # clean the labels of the legend from brackets
      for (i in 1:length(p$x$data)){
        if (!is.null(p$x$data[[i]]$name)){
          p$x$data[[i]]$name =  sub(".", "",str_split(p$x$data[[i]]$name,",")[[1]][1])
        }
      }
      #plot
      p %>% style(p, visible = "legendonly", traces=c(4))
      
    } else if (median(dates$Nr_records) > 10000 & median(dates$Nr_records) < 1000000) {
      dates$Records_1000 = dates$Nr_records/1000
      dates$Records_prev_1000 = dates$Records_prev_year/1000
      # create main plot
      g = ggplot() + 
        # highlight weekends
        geom_rect(data=wed, aes(xmin = xmin, xmax = xmax, 
                                # cannot use -Inf/+ Inf with ggplotly, have to set fixed ymin and ymax..
                                ymin = min(dates[,c("Stringency_index", "Records_1000", "Records_prev_1000", "Change_park_visitors", "Change_time_at_home")], na.rm=T),
                                ymax = max(dates[,c("Stringency_index", "Records_1000", "Records_prev_1000", "Change_park_visitors", "Change_time_at_home")], na.rm=T),
                                fill="Weekend"), 
                  alpha = 0.2) +
        # plot the GBIF and stringency index and movement data
        geom_line(dates, mapping=aes(x=Date, y=Stringency_index, colour="Stringency index")) +
        geom_line(dates, mapping=aes(x=Date, y=Records_1000, colour="Records (thousands)")) +
        geom_line(dates, mapping=aes(x=Date, y=Records_prev_1000, colour="Previous year's records (thousands)")) +
        geom_line(dates, mapping=aes(x=Date, y=Change_park_visitors, colour="% change in visitors to parks")) +
        geom_line(dates, mapping=aes(x=Date, y=Change_time_at_home, colour="% change in time spent at home"))+ 
        theme_minimal() +
        scale_fill_manual(name= '', values = c("Weekend" = 'orange'))  +
        scale_color_manual(name = "", values = c("Records (thousands)" = "blue",
                                                 "Previous year's records (thousands)" = "lightblue",
                                                 "Stringency index" = "red", 
                                                 "% change in visitors to parks" = "green",
                                                 "% change in time spent at home" = "brown")) +
        theme(legend.text=element_text(size=12),legend.title=element_text(size=12),axis.title=element_text(size=12)) +      
        labs(y="", x="Date") 
      # make interactive!
      p = ggplotly(g, tooltip=c("Date", "y")) %>%  layout(hovermode = "x unified")
      # clean the labels of the legend from brackets
      for (i in 1:length(p$x$data)){
        if (!is.null(p$x$data[[i]]$name)){
          p$x$data[[i]]$name =  sub(".", "",str_split(p$x$data[[i]]$name,",")[[1]][1])
        }
      }
      #plot
      p %>% style(p, visible = "legendonly", traces=c(4))
      
    } else if (median(dates$Nr_records) < 100) {
      # create main plot
      g= ggplot() + 
        # highlight weekends
        geom_rect(data=wed, aes(xmin = xmin, xmax = xmax, 
                                # cannot use -Inf/+ Inf with ggplotly, have to set fixed ymin and ymax..
                                ymin = min(dates[,c("Stringency_index", "Nr_records", "Records_prev_year", "Change_park_visitors", "Change_time_at_home")], na.rm=T),
                                ymax = max(dates[,c("Stringency_index", "Nr_records", "Records_prev_year", "Change_park_visitors", "Change_time_at_home")], na.rm=T),
                                fill="Weekend"), 
                  alpha = 0.2) +
        # plot the GBIF and stringency index and movement data
        geom_line(dates, mapping=aes(x=Date, y=Stringency_index, colour="Stringency index")) +
        geom_line(dates, mapping=aes(x=Date, y=Nr_records, colour="Records")) +
        geom_line(dates, mapping=aes(x=Date, y=Records_prev_year, colour="Previous year's records")) +
        geom_line(dates, mapping=aes(x=Date, y=Change_park_visitors, colour="% change in visitors to parks")) +
        geom_line(dates, mapping=aes(x=Date, y=Change_time_at_home, colour="% change in time spent at home"))+ 
        theme_minimal() +
        scale_fill_manual(name= '', values = c("Weekend" = 'orange'))  +
        scale_color_manual(name = "", values = c("Records" = "blue",
                                                 "Previous year's records" = "lightblue",
                                                 "Stringency index" = "red", 
                                                 "% change in visitors to parks" = "green",
                                                 "% change in time spent at home" = "brown")) +
        theme(legend.text=element_text(size=12),legend.title=element_text(size=12),axis.title=element_text(size=12)) +      
        labs(y="", x="Date") 
      # make interactive!
      p = ggplotly(g, tooltip=c("Date", "y")) %>%  layout(hovermode = "x unified")
      # clean the labels of the legend from brackets
      for (i in 1:length(p$x$data)){
        if (!is.null(p$x$data[[i]]$name)){
          p$x$data[[i]]$name =  sub(".", "",str_split(p$x$data[[i]]$name,",")[[1]][1])
        }
      }
      #plot
      p %>% style(p, visible = "legendonly", traces=c(4))
      
    }  else {
      dates$Records_100 = dates$Nr_records/100
      dates$Records_prev_100 = dates$Records_prev_year/100
      # create main plot
      g = ggplot() + 
        # highlight weekends
        geom_rect(data=wed, aes(xmin = xmin, xmax = xmax, 
                                # cannot use -Inf/+ Inf with ggplotly, have to set fixed ymin and ymax..
                                ymin = min(dates[,c("Stringency_index", "Records_100", "Records_prev_100", "Change_park_visitors", "Change_time_at_home")], na.rm=T),
                                ymax = max(dates[,c("Stringency_index", "Records_100", "Records_prev_100", "Change_park_visitors", "Change_time_at_home")], na.rm=T),
                                fill="Weekend"), 
                  alpha = 0.2) +
        # plot the GBIF and stringency index and movement data
        geom_line(dates, mapping=aes(x=Date, y=Stringency_index, colour="Stringency index")) +
        geom_line(dates, mapping=aes(x=Date, y=Records_100, colour="Records (hundreds)")) +
        geom_line(dates, mapping=aes(x=Date, y=Records_prev_100, colour="Previous year's records (hundreds)")) +
        geom_line(dates, mapping=aes(x=Date, y=Change_park_visitors, colour="% change in visitors to parks")) +
        geom_line(dates, mapping=aes(x=Date, y=Change_time_at_home, colour="% change in time spent at home"))+ 
        theme_minimal() +
        scale_fill_manual(name= '', values = c("Weekend" = 'orange'))  +
        scale_color_manual(name = "", values = c("Records (hundreds)" = "blue",
                                                 "Previous year's records (hundreds)" = "lightblue",
                                                 "Stringency index" = "red", 
                                                 "% change in visitors to parks" = "green",
                                                 "% change in time spent at home" = "brown")) +
        theme(legend.text=element_text(size=12),legend.title=element_text(size=12),axis.title=element_text(size=12)) +      
        labs(y="", x="Date") 
      # make interactive!
      p = ggplotly(g, dynamicTicks=TRUE, tooltip=c("Date", "y")) %>%  layout(hovermode = "x unified")
      # clean the labels of the legend from brackets
      for (i in 1:length(p$x$data)){
        if (!is.null(p$x$data[[i]]$name)){
          p$x$data[[i]]$name =  sub(".", "",str_split(p$x$data[[i]]$name,",")[[1]][1])
        }
      }
      #plot, but keep records of previous year off in the default viewer
      p %>% style(p, visible = "legendonly", traces=c(4))
    }
    
  })
  
  ### Pearson's correlation plot, for single country metrics:
  output$Plot_corr <- renderPlot({
    # filter to selected time period
    dates <- allcn %>% filter(Date<=end_date2() & Date>=start_date2())  %>% 
      filter(Country==country_name()) 
    
    # validate whether Google and COVID data are available for that country
    validate(
      need(length(na.omit(dates$Stringency_index))>0, "Government Response stringency index not available for this country."),
      need(length(na.omit(dates$Change_park_visitors))>0, "Google COVID-19 Community Mobility Reports not available for this country."))
    
    # add info on day of week, so that we can highlight the weekends
    dates$weekend = ifelse(dates$weekday %in% c("Saturday", "Sunday"), 1, 0)    
    # create a correlation plot
    nona = na.omit(dates[,c("Nr_records","Stringency_index", "Change_park_visitors", "Change_time_at_home","weekend")])
    M = cor(nona, method="pearson")
    colnames(M) <- c("Nr. records", "Stringency index", "% change park visitors", "% change time at home", "Weekend (0/1)")
    rownames(M) <- c("Nr. records", "Stringency index", "% change park visitors", "% change time at home", "Weekend (0/1)")
    testRes = cor.mtest(nona, conf.level = 0.95)
    corrplot(M, p.mat = testRes$p, method = 'circle', diag = FALSE, type = 'upper',tl.srt = 45,
             sig.level = c(0.001, 0.01, 0.05), pch.cex = 1.4,
             insig = 'label_sig', pch.col = 'grey20', order = 'original')
    
  })
  
  ### Interactive table, to view the data underlying the single country plot:
  output$Table <- DT::renderDataTable({

    # filter to the selected country and time period
    onecn = allcn %>% filter(Country==country_name()) 
    dates <- onecn %>% filter(Date<=end_date2() & Date>=start_date2())
    # add number of GBIF records collected in the previous year, for comparison
    dates$Prev_year = dates$Date - years(1)
    dates$Records_prev_year <- onecn$Nr_records[match(dates$Prev_year, onecn$Date)]
    
    dates = dates[,c("Date", "Nr_records", "Records_prev_year", "Stringency_index", "Change_park_visitors", "Change_time_at_home", "Country")]
    names(dates) <- c("Date", "Nr. of records", "Previous year's records", "Stringency index", "% change park visitors", "% change time at home", "Country")  
    # create interactive data.table
    datatable(dates, class=c("compact", "nowrap", "hover", "stripe"), rownames=F)
    
  }) 
  
}
