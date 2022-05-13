# Load the require packages
install.packages("readr")
install.packages("dplyr") 
install.packages("lubridate")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("forecast")
install.packages("sweep")
install.packages("ggrepel")
install.packages("ggtext")

# Load Libraries
library(readr)  # For Importing CSV File
library(dplyr)  # For Data Manipulation
library(lubridate)  # For Dates Conversion
library(tidyr)  # For Data Cleaning
library(ggplot2)  # For Plotting
library(forecast)  # For Prediction
library(sweep)  # For Time Series Analysis
library(ggrepel)  # For Text Labels on Plots
library(ggtext)  # For Text Labels and Boxes on Plots


# ------------------------------- Loading Data ---------------------------------

# Importing the csv file of the dataset dowloaded from https://ourworldindata.org/covid-cases
our_world_data <- read_csv("our_world_covid_data_18th_dec_2021.csv")


# ------------ Part 1 (Analysis of Total Covid Cases by BarChart) --------------- 

# Filtering and Grouping data for bar_plot
data_for_bar_plot <- our_world_data %>%
  # Selecting only relevant columns
  select(continent, location, date, total_cases) %>% 
  # Ordering by latest daily cases
  arrange(desc(date)) %>%   
  # Grouping by location to get top 1 overall cases for each location
  group_by(location) %>%  
  # Selecting the top 1 values for each loaction
  slice(1:1) %>% 
  # Dropping the rows having any value of NA
  drop_na() %>%     
  # Arranging to get top 20 countries with most cases
  arrange(desc(total_cases)) %>%                          
  ungroup() %>%
  top_n(20)

# Plotting BarChart using ggplot
bar_plot_1 <- ggplot(data_for_bar_plot, 
                     aes(x = reorder(location, total_cases), 
                         y = round(total_cases / 1000000,2), 
                         fill = continent)) +
              
              geom_bar(stat = "identity", position = "identity") + 
              # Adding Text on top of each bar
              geom_text(aes(label = round(total_cases / 1000000,2)), 
                        position = position_dodge(width=0.5), 
                        hjust = -0.05, 
                        size = 4) +
              # Changing Axis Labels
              labs(x = "Country", 
                   title = "Top 20 Countries with Most Cases of Covid-19 (in millions)",
                   caption = "Data Updated: 17th December 2021") +
              # Setting a Theme
              theme_minimal() +
              # Formatting of Axis, Plot, Panel, and Legend
              theme(axis.title.x = element_blank(),
                    axis.text.x  = element_blank(),
                    axis.text.y  = element_text(face = "bold", 
                                                colour = "black",
                                                margin = margin(t = 0, r = -40, 
                                                                b = 0, l = 0, 
                                                                unit = "pt")),
                    axis.ticks.x = element_blank(),
                    plot.title   = element_text(face = "bold", hjust = 0.5),
                    plot.background  = element_blank(),
                    plot.caption = element_text(face = "italic", hjust = 0.9),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    legend.position = c(0.7, 0.15)) +
              #Changing Legend Title
              guides(fill = guide_legend(title = "Continents")) +
              #Flipping the Coordinates
              coord_flip() 

# Saving the BarChart
ggsave(filename = "coursework_barplot", plot = bar_plot_1, device = "png")


# ----------- Part 2 (Forecasting of Daily Covid 19 Cases in UK and Russia) ------------

data_uk_rus <- our_world_data %>%
                 # Filtering Data for UK and Russia
                 filter(location %in% c("United Kingdom", "Russia"))

#             ------------ Data Cleaning ------------

# Checking for NAs in new_cases column
which(is.na(data_uk_rus$new_cases)) 

# Updating the NAs to 0
data_uk_rus$new_cases[is.na(data_uk_rus$new_cases)] <- 0 

# Updating the negative values with their positive equivalent
data_uk_rus$new_cases[data_uk_rus$new_cases < 0] <- abs(data_uk_rus$new_cases[data_uk_rus$new_cases < 0])

#          ------------ End of Data Cleaning ------------

#     ------------ Plotting Data for UK and Russia ------------

# Data preparation for Line Plot
lineplot_data <- data_uk_rus %>%
                   # Adding new columns
                   mutate(month_number = month(date),
                          month_short = month.abb[month(date)],
                          year = year(date)) %>%
                   # Selecting the required columns
                   select(iso_code, continent, location, date, month_number, 
                          month_short, year, new_cases) %>%
                   # Grouping the Data and the summarising
                   group_by(year, month_number, location) %>%
                   summarise(total_monthly_cases = sum(new_cases)) %>%
                   ungroup() %>%
                   # Adding a new column
                   mutate(month_year = my(paste(month.abb[month_number], 
                                          as.character(year), sep = " "))) %>%
                   # Ordering the DataFrame
                   arrange(year, month_number) 

# Visualising Data using a Line Plot
uk_russia_plot <- ggplot(lineplot_data, 
                         aes(x = month_year, y = total_monthly_cases / 1000000, 
                             group = 1, color = location)) + 
                  geom_line() +
                  # Modifying X-axis
                  scale_x_date(date_breaks = "1 months", date_labels = "%b %y",
                               limits=c(as.Date('2020-01-01'), as.Date('2021-12-31')),
                               expand=c(0, .9)) +
                  # Splitting the Plot into two for comparison
                  facet_wrap( ~ location, ncol = 1, scales = 'free_x') +
                  # Setting a Theme
                  theme_minimal() +
                  # Formatting Axis, Legend, Plot, and Panel
                  theme(axis.text.x = element_text(angle = 90),
                        axis.title.x = element_text(face = "bold"),
                        axis.title.y = element_text(face = "bold"),
                        axis.line = element_line(color='black'),
                        legend.title = element_blank(), 
                        legend.position = "none", 
                        plot.title = element_text(face = "bold", hjust = 0.5),
                        plot.background = element_blank(),
                        panel.grid.major.x = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_blank()) +
                  # Updating Scales as per required
                  scale_y_continuous(breaks = seq(0.2, 1.2, by = 0.2)) +
                  # Updating Axis Labels and Plot Title
                  labs(x = "Month & Year", y = "Total Cases (in millions)", 
                       title = "Covid-19: Total Monthly Cases in Russia and United Kingdom") 

# Saving the LinePlot
ggsave(filename = "coursework_lineplot_uk_russia", plot = uk_russia_plot, device = "png")

#      ------------ End of Plotting Data for UK and Russia ------------


# ----- Time Series Analysis for Forecasting Daily Cases in UK and Russia -----

# For United Kingdom: Data Preparation
uk_data_for_forecasting <- data_uk_rus %>%
                             # Selecting relevant Columns
                             select(location, date, new_cases) %>%
                             # Adding a new column
                             mutate(year = year(date)) %>%
                             # Filtering Data to get rows of UK
                             filter(year == 2021, location == "United Kingdom") 

# Converting Data into a Time Series
uk_data_ts <- ts(uk_data_for_forecasting$new_cases, 
                 frequency = 365, 
                 start=c(2021,1,1))

# Building the ARIMA model for Time Series Data
uk_arima_model <- auto.arima(uk_data_ts,seasonal = TRUE, trace = TRUE)

# Forecasting Output of the ARIMA Model for next 20 Days
forecast_uk_output <- forecast(uk_arima_model, h = 20)

# Plotting the Forecasted Output
forecast_uk_plot <- autoplot(forecast_uk_output) +
                    # Updating Axis Labels and Plot Title
                    labs(x = "Time", y = "Total Daily Cases", 
                         title = "United Kingdom's Daily Covid-19 Cases Forecast") +
                    # Setting a Theme
                    theme_bw() +
                    # Formatting Axis, Plot, and Panel
                    theme(axis.title.x = element_text(face = "bold"),
                          axis.title.y = element_text(face = "bold"),
                          axis.line = element_line(color = 'black'),
                          plot.title = element_text(face = "bold", hjust = 0.5),
                          plot.background = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_rect(color = "black", size = 2)) +
                    # Updating Scales as required
                    scale_x_continuous(expand = c(0, 0),
                                       breaks = seq(2021, 2022, by = 0.5)) +
                    scale_y_continuous(breaks = seq(25000, 200000, by = 25000),
                                       labels = scales::comma)

# Forecasting Cases of Next 20 Days in the UK
forecast_uk_output

# Formatting Output as model output is difficult to interpret
uk_forecast_output_df <- as.data.frame(forecast_uk_output)

# Updating Column Names of the Forecasted Output DataFrame
uk_forecast_output_df<- setNames(cbind(rownames(uk_forecast_output_df), 
                                       uk_forecast_output_df, 
                                       row.names = NULL), 
                                 c("Time", "Forecast", "Lo 80", "Hi 80", "Lo 95", "Hi 95"))

# Modifying the output and storing in a DataFrame
uk_daily_cases_forecast <- uk_forecast_output_df %>%
                             # Adding a new columns to interpret date returned from Model Output
                             mutate(new_time = lubridate::date_decimal(as.double(Time))) %>%
                             mutate(Expected_Time = format(as.POSIXct(new_time),'%d %b %Y %H:%M')) %>%
                             # Renaming Output Columns as per convenience
                             rename("Time (Model Output)" = "Time", 
                                    "Expected Time (Calculated)" = "Expected_Time", 
                                    "Forecasted Daily Cases" = "Forecast", 
                                    "Lo_80" = "Lo 80", "Hi_80" = "Hi 80", 
                                    "Lo_95" = "Lo 95", "Hi_95" = "Hi 95") %>%
                             # Selecting necessary columns
                             select("Time (Model Output)", "Expected Time (Calculated)", 
                                    "Forecasted Daily Cases", "Lo_80", "Hi_80", "Lo_95", "Hi_95")

# Checking the Summary of the Model
summary(uk_arima_model)

# Saving the Forecast Plot for UK
ggsave(filename = "coursework_uk_arima_forecast_Plot", 
       plot = forecast_uk_plot, device = "png")


# End of Forecasting for UK


# Forecasting For Russia: Data Preparation
rus_data_for_forecasting <- data_uk_rus %>%
                              # Selecting relevant Columns
                              select(location, date, new_cases) %>%
                              # Adding a new column
                              mutate(year = year(date)) %>%
                              # Filtering Data to get rows of Russia
                              filter(year == 2021, location == "Russia") 

# Converting Data into a Time Series
rus_data_ts <- ts(rus_data_for_forecasting$new_cases, 
                  frequency = 365, 
                  start=c(2021,1,1))

# Building the ARIMA model for Time Series Data
rus_arima_model <- auto.arima(rus_data_ts,seasonal = TRUE)

# Forecasting Output of the ARIMA Model for next 20 Days
forecast_rus_output <- forecast(rus_arima_model, h = 20)

# Plotting the Forecasted Output
forecast_rus_plot <- autoplot(forecast_rus_output) +
                     # Updating Axis Labels and Plot Title
                     labs(x = "Time", y = "Total Daily Cases", 
                          title = "Russia's Daily Covid-19 Cases Forecast") +
                     # Setting a Theme
                     theme_bw() +
                     # Formatting Axis, Plot, and Panel
                     theme(axis.title.x = element_text(face = "bold"),
                           axis.title.y = element_text(face = "bold"),
                           axis.line = element_line(color='black'),
                           plot.title = element_text(face = "bold", hjust = 0.5),
                           plot.background = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.border = element_rect(color = "black", size = 2)) +
                     # Updating Scales as required
                     scale_x_continuous(expand = c(0, 0),
                                        breaks = seq(2021, 2022, by = 0.5)) +
                     scale_y_continuous(expand = c(0, 0),
                                        breaks = seq(5000, 50000, by = 5000),
                                        labels = scales::comma) +
                     # To avoid showing 0 on Y-Axis
                     expand_limits(y = c(1, 50000)) 

# Forecasting Cases for Next 20 Days in Russia
forecast_rus_output

# Formatting Output as model output is difficult to interpret
rus_forecast_output_df <- as.data.frame(forecast_rus_output)

# Updating Column Names of the Forecasted Output DataFrame
rus_forecast_output_df<- setNames(cbind(rownames(rus_forecast_output_df), 
                                        rus_forecast_output_df, 
                                        row.names = NULL), 
                                  c("Time", "Forecast", "Lo 80", "Hi 80", "Lo 95", "Hi 95"))

# Modifying the output and storing in a DataFrame
rus_daily_cases_forecast <- rus_forecast_output_df %>%
                              # Adding a new columns to interpret date returned from Model Output
                              mutate(new_time = lubridate::date_decimal(as.double(Time))) %>%
                              mutate(Expected_Time = format(as.POSIXct(new_time),'%d %b %Y %H:%M')) %>%
                              # Renaming Output Columns as per convenience
                              rename("Time (Model Output)" = "Time", 
                                     "Expected Time (Calculated)" = "Expected_Time", 
                                     "Forecasted Daily Cases" = "Forecast", 
                                     "Lo_80" = "Lo 80", "Hi_80" = "Hi 80", 
                                     "Lo_95" = "Lo 95", "Hi_95" = "Hi 95") %>%
                              # Selecting necessary columns
                              select("Time (Model Output)", "Expected Time (Calculated)", 
                                     "Forecasted Daily Cases", "Lo_80", "Hi_80", "Lo_95", "Hi_95")

# Checking the Summary of the Model
summary(rus_arima_model)

# Saving the Forecast Plot for Russia
ggsave(filename = "coursework_russia_arima_forecast_Plot", 
       plot = forecast_rus_plot, device = "png")

# End of Forecasting for Russia


# ------------ Part 3 (Analysis of Fully Vaccinated Population) --------------- 


# Preparing Vaccination Data
vaccination_data <- our_world_data %>%
                      # Selecting only relevant columns
                      select(continent, location, date, people_fully_vaccinated, 
                             population) %>%     
                      # Ordering by latest vaccination data
                      arrange(desc(date)) %>%   
                      # Grouping by location to get top 1 overall vaccination for each location
                      group_by(location) %>% 
                      # Selecting the top 1 values for each loaction
                      slice(1:1) %>%    
                      # Dropping the rows having any value of NA
                      drop_na() %>%  
                      # Ordering by most vaccination
                      arrange(desc(people_fully_vaccinated)) %>%                          
                      ungroup()

# Preparing Data for Scatter Plot 
population_filter <- vaccination_data %>%
                       # Adding a new column: percentage of people vaccinated
                       mutate(people_vaccinated_percent = people_fully_vaccinated / population * 100) %>%
                       # Filtering Countries with Population more than 45 Million
                       filter(population >= 50000000) %>%
                       # Ordering by countries with highest percentage of fully vaccinated people
                       arrange(desc(people_vaccinated_percent))



#  ------------  Part 4 (Analysis of Covid-19 Deaths in Countries with 
#                  Highest Percentage of Fully Vaccinated Population)   ---------------  


# Preparing Covid-19 Deaths Data
deaths_data <- our_world_data %>%
                 # Selecting only relevant columns
                 select(continent, location, date, total_deaths) %>%    
                 # Removing All the Rows with NAs
                 drop_na()    

# Joining Data with Scatter Plot Data to get locations
death_data_for_plot_v2 <- population_filter %>%
                            # Inner Join to get countries with Highest Fully vaccinated Population
                            inner_join(deaths_data, by = "location",
                                     suffix = c("_vaccination_data", "_deaths_data")) %>%
                            # Selecting relevant columns for Daily Deaths 
                            select(continent_vaccination_data, location, 
                                   date_deaths_data, total_deaths) %>%
                            # Renaming Columns
                            rename("continent" = "continent_vaccination_data", 
                                   "date" = "date_deaths_data") 


# Calculating Date When First Fully Vaccination Recorded
fully_vaccinated_data <- our_world_data %>%
                           # Selecting only relevant columns
                           select(continent, location, date, people_fully_vaccinated, 
                                  population) %>%     
                           # Ordering by earliest fully vaccination data
                           arrange(date) %>%   
                           # Dropping the rows having any value of NA
                           drop_na() %>% 
                           # Grouping by location to get top 1 overall vaccination for each location
                           group_by(location) %>% 
                           # Selecting the top 1 values for each loaction
                           slice(1:1) %>%  
                           # Ordering by earliest fully vaccination data
                           arrange(date) %>%
                           ungroup()

# Joining Fully Vaccinated Date with Deaths on that Date to Plot
first_date_of_fully_vaccinated <- fully_vaccinated_data %>%
                                    inner_join(population_filter, by = "location",
                                    suffix = c("_vaccination_data", "_deaths_data")) %>%
                                    select(location, date_vaccination_data) %>%
                                    rename("date" = "date_vaccination_data") %>%
                                    inner_join(death_data_for_plot_v2, by = c("location", "date"))


# Plotting Deaths Since Pandemic Started
deaths_since_pandemic_plot <- ggplot(death_data_for_plot_v2 , 
                                     aes(x=date, y= total_deaths, 
                                         color = location, group = location)) +
                              geom_line() +
                              # To get point on the latest record
                              geom_point(data = death_data_for_plot_v2 %>% 
                                                  arrange(desc(date)) %>% 
                                                  group_by(location) %>%
                                                  slice(1:1), 
                                         mapping = aes(x = date, 
                                                       y = total_deaths)) +
                              # Selecting a Theme
                              theme_bw()  +
                              # Updating Axis, Plot, Panel, and Legend
                              theme(axis.ticks.x = element_line(color = "black"),
                                    axis.ticks.y = element_blank(),
                                    plot.title = element_text(face = "bold", hjust = 0.5),
                                    plot.caption = element_text(face = "italic", hjust = 0.9, size = 12),
                                    #plot.tag = element_text(hjust = -1.1),
                                    plot.tag.position = "bottomleft",
                                    panel.grid.major.x  = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.border = element_rect(color = "black", size = 2),
                                    panel.spacing = unit(0, "pt"),
                                    legend.position = "none") +
                              # Updating Scale as needed
                              scale_y_continuous(expand = c(0, 0),
                                                 breaks = seq(0, 1000000, by = 200000),
                                                 labels = scales::comma) +
                              # To avoid showing 0 on Y-Axis
                              expand_limits(y = c(1, 1000000)) +
                              # Setting up scales on x axis to avoid over crowding of Date
                              scale_x_date(date_breaks = "3 months",
                                           labels = scales::date_format("%d-%b-%Y"),
                                           limits = as.Date(c("2020-03-01","2022-02-10")), 
                                           expand = c(0, 0)) +
                              # Adding Text inside a Box for each line
                              geom_label_repel(data = death_data_for_plot_v2 %>% 
                                                        arrange(desc(date)) %>% 
                                                        group_by(location) %>%
                                                        slice(1:1), 
                                               aes(x = as.Date("2021-12-29"), 
                                                   label = location, 
                                                   color = location),
                                               direction="y",
                                               hjust = 0,
                                               na.rm = TRUE,
                                               segment.color = "white",
                                               size = 3,
                                               xlim = as.Date(c("2021-12-17", "2022-03-31"))) +
                              # Updating Axis Labels and Plot Title
                              labs(x = "Date", y = "Total Deaths", 
                                   title = ~ atop(paste(bold('Pattern in Covid - 19 Deaths Since March 2020 ')),
                                                  paste(scriptstyle(italic("Countries with highest percentage of fully vaccinated people")))),
                                   caption = "● Date when first instance of fully vaccinated was recorded") +
                              # Adding a Point to know first instance of fully vaccinated 
                              geom_point(data = first_date_of_fully_vaccinated, 
                                         inherit.aes = FALSE,
                                         mapping = aes(x = date,
                                                       y = total_deaths)) 


# Saving the Line Plot for Covid-19 Deaths since Pandemic Started
ggsave(filename = "coursework_deaths_Plot", 
       plot = deaths_since_pandemic_plot, device = "png")

# --------------------------------------- End ----------------------------------------



