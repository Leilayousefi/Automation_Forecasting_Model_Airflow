#This script aims to create the forecasting for the Family Law Act case type.

#1) Load packages
library(botor)
library(openxlsx)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(forecast)
library(timetk)
library(Rdbtools)
library(seasonal)


#1.1) Set inputs - these will be used when extracting the raw data from the S3. 
case_type <- "Family Law Act"
period_start <- lubridate::dmy("01-01-2015")

#1.2) Set the folder in which the final output will be uploade to in S3
output_path <- paste0("s3://alpha-civil-and-family-forecast/forecast_testing_2022/", case_type,"/")

#2) Load the data from S3 Athena
#Load the latest snapshot available in the data (latest date in mojap_snapshot_date variable)

familyman_sql <- paste0("
  SELECT *
  FROM familyman_derived_dev_v3.cases_derived
  WHERE mojap_snapshot_date = (
  SELECT MAX(mojap_snapshot_date)
  FROM familyman_derived_dev_v3.cases_derived)
  AND case_type_name_group IN ('",case_type, "')
  AND date(issue_month) >= date('",period_start,"') 
  ")

familyman_extract <- Rdbtools::read_sql(familyman_sql)


#3)Calculate number of cases over time
case_numbers <- familyman_extract %>% 
  dplyr::group_by(issue_month, case_type_name_group) %>% 
  dplyr::summarise(cases = n()) %>% 
  dplyr::mutate(model = "Actuals") %>% 
  dplyr::rename("Date" = "issue_month")


#3.1) MANUAL STEP - check whether the last row of the data is incomplete or not.
#If the raw gets updated at a point where the latest data is incomplete, then it should be removed to not skew the forecast.
#The code below will be commented out, but should be run in case the row needs to be removed.

# case_numbers <- case_numbers %>% 
#   head(-1)


#4) Turn the number of cases into a timeseries
time_series <- stats::ts(case_numbers$cases, start = c(2015,1), frequency = 12)


#5) Plot and save the timeseries
png(file = "time_series_plot.jpeg")

ggplot2::autoplot(time_series) + 
  ggtitle(paste0(case_type, " time series")) +
  ylab("Cases") + 
  xlab("Years")

dev.off()

############################## DECOMPOSITION #####################################
#There are three types of time series patterns: trends, seasonality and cycles. 
#A decomposition allows you to extract these three components and visualise them separately. 
#It can give an insight into the data and help with forecasting accuracy. 

#Will be using an X11 decomposition here. 
#It's based on classical decomposition, but it's been improved to overcome the drawbacks of classical decomposition. 
#It only works with quarterly and monthly data. It can handle both additive and multiplicative decompositions. 
#It does this automatically and technique is quite robust to outliers and level shifts in the time series. 

png(file = "time_series_decomposition.jpeg")

time_series %>% seasonal::seas(x11 = "") %>% 
  ggplot2::autoplot() +
  ggplot2::ggtitle(paste0("X11 decomposition of ", case_type, " data")) +
  ggplot2::xlab("Years")

dev.off()

############################## OUTLIERS ###########################################
#Another step that is mostly manual. 
#It comes down to judgement to determine whether you want to clean the data of outliers or not. 

#1) Flag outliers
forecast::tsoutliers(time_series)

#2) Clean outliers and replace missing values. 
#Read into the tsclean() function by running ?tsclean to understand how the replacement is done. 
#The calculation is based on STL decomposition and linear interpolation. 
#Code has been commented, but uncomment and run if required. 

#time_series <- forecast::tsclean(time_series, replace.missing = TRUE)


############################## FORECASTING ########################################

#The data that we deal with has a seasonality aspect to it. 
#When using forecasting techniques, it's important we keep this in mind and apply the appropriate forecasting method. 

#1) Seasonal naive

#For naive forecasts in general, we set all future forecast values to be the same as the last observed value. 
#Seasonal naive forecasting is a variation of the standard naive forecast. 
#You set future forecasted values to be the same as the last observed value from the SAME SEASON. 
#For example, with monthly data, the forecast for all future February values is equal to the last observed February value. 
#With quarterly data, the forecast of all future Q2 values is equal to the last observed Q2 value
seasonal_naive_forecast <- forecast::snaive(time_series, h = 60)

#1.1) Plot the forecast
png(file = "seasonal_naive_plot.jpeg")
ggplot2::autoplot(seasonal_naive_forecast)
dev.off()

#1.2) Create the "summary" of the forecast - this shows the forecast values we are interested in as well as the error measures. 
summary(seasonal_naive_forecast)

#1.3) Convert the time series object into a tibble
seasonal_naive_table <- timetk::tk_tbl(seasonal_naive_forecast, rename_index = "Date", timetk_idx = "TRUE") %>% 
  dplyr::mutate(Date = as.Date(paste0("01", Date), "%d%b%Y"),
         model = "Seasonal naive")


#2) Holt-Winters
#This technique is an extension of exponential smoothing that accounts for trend and seasonality.
holt_winters_forecast <- forecast::hw(time_series, seasonal = "multiplicative", h = 60, damped = TRUE, phi = 0.9)

#2.1) Autoplot the forecast
png(file = "holt_winters_plot.jpeg")
ggplot2::autoplot(holt_winters_forecast)
dev.off()

#2.2) Create the summary
summary(holt_winters_forecast)

#2.3) Convert time series object into tibble
holt_winters_table <- timetk::tk_tbl(holt_winters_forecast, rename_index = "Date", timetk_idx = "TRUE") %>% 
  dplyr::mutate(Date = as.Date(paste0("01", Date), "%d%b%Y"),
         model = "Holt Winters")


#3) Combination of the two forecasts
#Taking an average of the point forecast of both seasonal naive and HW
combined_forecast <- seasonal_naive_table %>% 
  dplyr::rename("seasonal_naive" = "Point Forecast") %>% 
  dplyr::select(Date, seasonal_naive) %>% 
  dplyr::left_join(., holt_winters_table %>% rename("holt_winters" = "Point Forecast") %>% select(Date, holt_winters), by = "Date") %>% 
  dplyr::mutate(`Point Forecast` = (seasonal_naive + holt_winters)/2) %>% 
  dplyr::select(-c(holt_winters, seasonal_naive)) %>% 
  dplyr::mutate(model = "Combined forecast")


#4) Put all the figures together
actuals_and_forecasts <- case_numbers %>% 
  dplyr::select(-case_type_name_group) %>% 
  dplyr::rename("Point Forecast" = "cases") %>% 
  dplyr::bind_rows(seasonal_naive_table %>% select(Date, `Point Forecast`, model)) %>% 
  dplyr::bind_rows(holt_winters_table %>% select(Date, `Point Forecast`, model)) %>% 
  dplyr::bind_rows(combined_forecast %>% select(Date, `Point Forecast`, model)) %>% 
  tidyr::pivot_wider(id_cols = model,
              names_from = Date,
              values_from = `Point Forecast`)


#4) Check accuracy of the Seasonal Naive and Holt Winters



###################### WRITING TO EXCEL ##########################

#1) Set file name
#Extract date of current extract
snapshot_date <- familyman_extract %>% 
  dplyr::distinct(mojap_snapshot_date) %>% 
  dplyr::pull() %>% 
  format("%Y%m")

filename <- paste0(snapshot_date, "_", case_type, "_forecast.xlsx")

#2) Create workbook
wb <- openxlsx::createWorkbook()

options("openxlsx.dateFormat" = "dd/mm/yyyy")


#Add worksheets and write data in

openxlsx::addWorksheet(wb, sheetName = "Summary")
openxlsx::writeData(wb, sheet = "Summary", actuals_and_forecasts)

openxlsx::addWorksheet(wb, sheetName = "Actuals")
openxlsx::writeData(wb, sheet = "Actuals", case_numbers)
openxlsx::insertImage(wb, sheet = "Actuals", file = "time_series_plot.jpeg", startRow = 1, startCol = 5, width = 5, height = 3.5, units = "in")
openxlsx::insertImage(wb, sheet = "Actuals", file = "time_series_decomposition.jpeg", startRow = 20, startCol = 5, width = 7, height = 5, units = "in")


openxlsx::addWorksheet(wb, sheetName = "Seasonal Naive")
openxlsx::writeData(wb, sheet = "Seasonal Naive", seasonal_naive_table)
openxlsx::insertImage(wb, sheet = "Seasonal Naive", file = "seasonal_naive_plot.jpeg", startRow = 1, startCol = 8, width = 5, height = 3.5, units = "in")


openxlsx::addWorksheet(wb, sheetName = "Holt Winters")
openxlsx::writeData(wb, sheet = "Holt Winters", holt_winters_table)
openxlsx::insertImage(wb, sheet = "Holt Winters", file = "holt_winters_plot.jpeg", startRow = 1, startCol = 8, width = 5, height = 3.5, units = "in")

openxlsx::addWorksheet(wb, sheetName = "Combined Forecast")
openxlsx::writeData(wb, sheet = "Combined Forecast", combined_forecast)

#Save workbook
openxlsx::saveWorkbook(wb, filename, overwrite = T)


#Write file to S3
botor::s3_upload_file(filename, paste0(output_path, filename))

#Remove all files from working environment
file.remove(filename, "holt_winters_plot.jpeg", "seasonal_naive_plot.jpeg", "time_series_decomposition.jpeg", "time_series_plot.jpeg")

