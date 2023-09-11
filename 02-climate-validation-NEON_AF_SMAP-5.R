## Climate variable validation script to compare soil moisture data from SMAP with NEON tower data
## Copyright NEON 2023

## ----setup-env--------------------------------------------------------------------------------------------
# Install needed package (only uncomment & run if not already installed)
#install.packages("neonUtilities")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("lubridate")
#install.packages("Metrics")
#install.packages("stringr")

library(ggplot2)  # for plotting
library(dplyr)  # for data munging
library(tidyr)  # for data munging
library(tidyverse)
library(ggpubr)
library(lubridate)
library(Metrics)
library(stringr)

# set working directory
wd <- "~/R_Scripts/NEON-Climate-variable-validation/data" 
setwd(wd)

siteID = 'HARV'
NEON_startdate = "2018-01-01"
NEON_enddate = "2021-12-31"

sites <- c("BART", "HARV", "SCBI", "SERC", "BLAN", "OSBS", "DSNY", "JERC", "STEI", "TREE",
           "UNDE", "KONZ", "KONA", "UKFS", "ORNL", "GRSM", "MLBS", "TALL", "LENO", "DELA",
           "WOOD", "DCFS", "NOGP","CPER", "RMNP", "STER", "CLBJ", "OAES", "YELL", "NIWO",
           "MOAB", "SRER", "JORN", "ONAQ", "ABBY", "WREF", "SJER", "SOAP", "TEAK")

# sites <- c("BART", "HARV")

# Create an empty data frame
stats_neon_af_clim_smap_meanSWC <- data.frame(site=character(),
                                              r_squared=double(),
                                              slope=double(),
                                              intercept=double(),
                                              std_error=double(),
                                              mean_bias=double(),
                                              stringsAsFactors=FALSE)

for(siteID in sites) {
  # Read file from disk if it already exists
  neon_af_clim <- read.csv(gsub(" ","", sprintf("~/R_Scripts/NEON-Climate-variable-validation/data/AMF_US-x%s_BASE_HH.csv", siteID), 
                                fixed=TRUE), header = TRUE, stringsAsFactors=FALSE, skip=2)
  
  # Convert timestamp to date-time string
  neon_af_clim$TIMESTAMP_START <- as.POSIXct(as.character(neon_af_clim$TIMESTAMP_START) , format = "%Y%m%d%H%M")
  neon_af_clim$TIMESTAMP_END <- as.POSIXct(as.character(neon_af_clim$TIMESTAMP_END) , format = "%Y%m%d%H%M")
  
  # # Correct for GMT
  # neon_af_clim$TIMESTAMP_START <- neon_af_clim$TIMESTAMP_START + lubridate::days(1)
  # neon_af_clim$TIMESTAMP_END <- neon_af_clim$TIMESTAMP_END + lubridate::days(1)
  
  # Pull the variables
  tempAir <- neon_af_clim$TA_1_1_1
  rh <- neon_af_clim$RH
  presAir <- neon_af_clim$PA
  vpd <- neon_af_clim$VPD_PI
  ppt <- neon_af_clim$P
  swc <- neon_af_clim$SWC_1_1_1
  
  # Re-insert NAs
  tempAir[tempAir == -9999] <- NA
  rh[rh == -9999] <- NA
  presAir[presAir == -9999] <- NA
  vpd[vpd == -9999] <- NA
  ppt[ppt == -9999] <- NA
  swc[swc == -9999] <- NA
  
  # Add climate variables to data frame
  neon_af_clim$tempAir <- tempAir
  neon_af_clim$relHum <- rh
  neon_af_clim$vapPresDef <- vpd
  neon_af_clim$presAir <- presAir
  neon_af_clim$ppt <- ppt
  neon_af_clim$soilwc <- swc
  
  # Drop NA values
  neon_af_clim_noNA <- neon_af_clim %>%
    #drop_na(tempAir, relHum, vapPresDef, presAir, ppt, soilwc)
    drop_na(soilwc)
 
  #Convert time stamp to date
  neon_af_clim_noNA$Date <- as.Date(neon_af_clim_noNA$TIMESTAMP_START)
  
  # Define Start and end times for the subset as R objects that are the time class
  startTime <- as.Date("2021-08-01")
  endTime <- as.Date("2021-08-31")
  
  # create a start and end time R object
  start.end <- c(startTime,endTime)
  str(start.end)
  
  # Reduce and group by maximum air temperature per day
  neon_af_maxTemp_day <- neon_af_clim_noNA %>%
    group_by(Date) %>%
    slice(which.max(tempAir))
  
  # Subset and sum precipitation per day
  neon_af_PPT = subset(neon_af_clim_noNA, select = c(Date, P))
  neon_af_PPT <- neon_af_PPT %>%
    group_by(Date) %>%
    #distinct(Date, .keep_all=T) %>%
    mutate(sumPPT=sum(P))
  
  neon_af_PPT = subset(neon_af_PPT, select = c(Date, sumPPT))
  
  neon_af_sumPPT_day <- neon_af_PPT %>%
    group_by(Date) %>%
    distinct(Date, .keep_all=T)
  
  neon_af_sumPPT_day <- neon_af_sumPPT_day %>%
    drop_na(Date)
  
  # Reduce and group by maximum VPD per day
  neon_af_vapPresDef = subset(neon_af_clim_noNA, select = c(Date, vapPresDef)) 
  
  neon_af_maxVPD_day <- neon_af_vapPresDef %>%
    group_by(Date) %>%
    slice(which.max(vapPresDef))
  
  #neon_af_maxVPD_day$vapPresDef <- neon_af_maxVPD_day$vapPresDef #/10
  
  # Subset and calculate mean Soil Water Content per day
  neon_af_soilwc = subset(neon_af_clim_noNA, select = c(Date, soilwc))
  neon_af_soilwc <- neon_af_soilwc %>%
    group_by(Date) %>%
    #distinct(Date, .keep_all=T) %>%
    mutate(meanSoilWC=mean(soilwc))
  
  neon_af_meanSWC_day <- neon_af_soilwc %>%
    group_by(Date) %>%
    distinct(Date, .keep_all=T)
  
  neon_af_meanSWC_day <- neon_af_meanSWC_day %>%
    drop_na(Date)
  
  neon_af_meanSWC_day = subset(neon_af_meanSWC_day, select = c(Date, meanSoilWC))

  # Merge data frames
  neon_af_clim_day <- merge(x = neon_af_maxTemp_day, y = neon_af_maxVPD_day, by = "Date")
  
  # Rename columns
  neon_af_clim_day <- neon_af_clim_day %>% 
    rename("maxTempAir" = "tempAir",
           "maxVPD" = "vapPresDef.y")
  
  # Subset columns
  neon_af_clim_day = subset(neon_af_clim_day, select = c(Date, maxTempAir, relHum, presAir, maxVPD))
  
  # Merge data frames
  neon_af_clim_day <- merge(x = neon_af_clim_day, y = neon_af_sumPPT_day, by = "Date")
  
  # Merge data frames
  neon_af_clim_day <- merge(x = neon_af_clim_day, y = neon_af_meanSWC_day, by = "Date")
  
  # Reduce to months of April-September 
  neon_af_clim_day_2018 <- neon_af_clim_day %>% filter(between(Date, as.Date('2018-04-01'), as.Date('2018-09-30')))
  neon_af_clim_day_2019 <- neon_af_clim_day %>% filter(between(Date, as.Date('2019-04-01'), as.Date('2019-09-30')))
  neon_af_clim_day_2020 <- neon_af_clim_day %>% filter(between(Date, as.Date('2020-04-01'), as.Date('2020-09-30')))
  neon_af_clim_day_2021 <- neon_af_clim_day %>% filter(between(Date, as.Date('2021-04-01'), as.Date('2021-09-30')))
  
  # Put all data frames into list
  df_list <- list(neon_af_clim_day_2018, neon_af_clim_day_2019, neon_af_clim_day_2020, neon_af_clim_day_2021)
  
  # Merge all data frames in list
  neon_af_clim_day_summer <- df_list %>% reduce(full_join, by='Date')
  
  neon_af_clim_day_summer <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
  
  #---------------- Read and process SMAP data file--------------------------------
  
  # Read PRISM csv file
  df <- read.csv(file=sprintf("SMAP_2018_2021_%s.csv", siteID), stringsAsFactors = FALSE)
  
  df$new_date <- str_sub(df$system.index, 1, 8)
  df$new_time <- str_sub(df$system.index, 10, 13)
  
  # Separate YEAR, MONTH and DAY
  df <- extract(df, new_date, into = c("YEAR", "MONTH", "DAY"), "(.{4})(.{2})(.{2})")
  
  # Convert to numeric
  df$P_YEAR <- as.numeric(df$YEAR)
  df$P_MONTH <- as.numeric(df$MONTH)
  df$P_DAY <- as.numeric(df$DAY)
  
  df$Date<-as.Date(with(df,paste(YEAR,MONTH,DAY,sep="-")),"%Y-%m-%d")
  
  smap_soilwc = subset(df, select = c(Date, new_time, sm_surface))
  
  # Subset and calculate mean Soil Water Content per day  
  smap_soilwc_day <- smap_soilwc %>%
    group_by(Date) %>%
    #distinct(Date, .keep_all=T) %>%
    mutate(SMAP_daily_meanSoilWC=mean(sm_surface))
  
  smap_soilwc_day <- smap_soilwc_day %>%
    group_by(Date) %>%
    distinct(Date, .keep_all=T)
  
  smap_soilwc_day <- smap_soilwc_day %>%
    drop_na(Date)
  
  smap_soilwc_day = subset(smap_soilwc_day, select = c(Date, SMAP_daily_meanSoilWC))
  
  smap_soilwc_day$SMAP_daily_meanSoilWC <- smap_soilwc_day$SMAP_daily_meanSoilWC*100  
    
  neon_af_clim_smap_day <- merge(x = neon_af_clim_day_summer, y = smap_soilwc_day, by = "Date")
  
  ## ----Save csv file ------------------------------------------------------------------------------
  ## 
  write.csv(neon_af_clim_smap_day, file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/data_out/NEON_AF_clim_smap_daily_%s.csv", siteID)), row.names=F)
  
  #---------------- Create linear regression models for Temp and VPD and output stats to data frames ---------------------------------------
  
  model_soilWCmean <- lm(SMAP_daily_meanSoilWC ~ meanSoilWC, data = neon_af_clim_smap_day)
  stat_lm_swcmean <- data.frame(siteID, summary(model_soilWCmean)$r.squared, summary(model_soilWCmean)$coefficients[2], summary(model_soilWCmean)$coefficients[1], summary(model_soilWCmean)$sigma, mae(neon_af_clim_smap_day$SMAP_daily_meanSoilWC, predict(model_soilWCmean)))
  names(stat_lm_swcmean) <- c("site", "r_squared", "slope", "intercept", "std_error", "mean_bias")
  stats_neon_af_clim_smap_meanSWC <- bind_rows(stats_neon_af_clim_smap_meanSWC, stat_lm_swcmean)
  
  write.csv(stats_neon_af_clim_smap_meanSWC, file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/data_out/temp/NEON_AF_clim_smap_stats_meanSoilWC_%s.csv", siteID)))
  
  
  #---------------- Create scatter plots ----------------------------------------------------------------------------
  
  meanswcScatter_smap <- ggplot(neon_af_clim_smap_day,aes(x = SMAP_daily_meanSoilWC, y = meanSoilWC)) + 
    geom_point(color="black", size=0.5) + 
    geom_smooth(method = "lm", se=FALSE) +
    stat_regline_equation(label.y = 80, aes(label = ..eq.label..)) +
    stat_regline_equation(label.y = 70, aes(label = ..rr.label..)) +
    ggtitle("Mean Soil Water Content\nApril-September 2018-2021", siteID) +
    xlab("SMAP SWC (%)") + ylab("Tower SWC (%)")
  
  meanswcScatter_smap
  
  ## ----Save scatter plots ------------------------------------------------------------------------------
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_SMAP_meanSoilWC_ScatterPlot.png", siteID)), plot = meanswcScatter_smap, width = 7, height = 7)
  
  ## ----Create and save Soil WC line plots------------------------------------------------------------------------------
  
  neon_af_clim_smap_day_subset <- neon_af_clim_smap_day[c("Date", "meanSoilWC", "SMAP_daily_meanSoilWC")]
  neon_af_clim_smap_day_subset$Date <- ymd(neon_af_clim_smap_day_subset$Date)
  neon_af_clim_smap_day_subset <- neon_af_clim_smap_day_subset %>% rename("Tower_mean_Soil_Water_Content" = "meanSoilWC", "SMAP_mean_Soil_Water_Content" = "SMAP_daily_meanSoilWC")
  
  # Subset and plot 2018
  neon_af_clim_smap_day_2018 <- neon_af_clim_smap_day_subset %>% filter(between(Date, as.Date('2018-04-01'), as.Date('2018-09-30')))
  
  neon_af_clim_smap_day_2018 <- neon_af_clim_smap_day_2018 %>% pivot_longer(cols = -Date, names_to = "Data_Source", values_to = "value") %>%
    ggplot(aes(x=Date, y=value)) +
    geom_line(aes(linetype=Data_Source)) +
    theme_bw() + 
    ggtitle("Mean Soil Water Content\nApril-September 2018", siteID) +
    xlab("Date") + ylab("Mean SWC (%)")
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_LinePlot_SMAP_meanSoilWC_2018.png", siteID)), plot = neon_af_clim_smap_day_2018, width = 7, height = 7)
  
  # Subset and plot 2019
  neon_af_clim_smap_day_2019 <- neon_af_clim_smap_day_subset %>% filter(between(Date, as.Date('2019-04-01'), as.Date('2019-09-30')))
  
  neon_af_clim_smap_day_2019 <- neon_af_clim_smap_day_2019 %>% pivot_longer(cols = -Date, names_to = "Data_Source", values_to = "value") %>%
    ggplot(aes(x=Date, y=value)) +
    geom_line(aes(linetype=Data_Source)) +
    theme_bw() +
    ggtitle("Mean Soil Water Content\nApril-September 2019", siteID) +
    xlab("Date") + ylab("Mean SWC (%)")
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_LinePlot_SMAP_meanSoilWC_2019.png", siteID)), plot = neon_af_clim_smap_day_2019, width = 7, height = 7)
  
  # Subset and plot 2020
  neon_af_clim_smap_day_2020 <- neon_af_clim_smap_day_subset %>% filter(between(Date, as.Date('2020-04-01'), as.Date('2020-09-30')))
  
  neon_af_clim_smap_day_2020 <- neon_af_clim_smap_day_2020 %>% pivot_longer(cols = -Date, names_to = "Data_Source", values_to = "value") %>%
    ggplot(aes(x=Date, y=value)) +
    geom_line(aes(linetype=Data_Source)) +
    theme_bw() +
    ggtitle("Mean Soil Water Content\nApril-September 2020", siteID) +
    xlab("Date") + ylab("Mean SWC (%)")
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_LinePlot_SMAP_meanSoilWC_2020.png", siteID)), plot = neon_af_clim_smap_day_2020, width = 7, height = 7)
  
  # Subset and plot 2021
  
  neon_af_clim_smap_day_2021 <- neon_af_clim_smap_day_subset %>% filter(between(Date, as.Date('2021-04-01'), as.Date('2021-09-30')))
  
  neon_af_clim_smap_day_2021 <- neon_af_clim_smap_day_2021 %>% pivot_longer(cols = -Date, names_to = "Data_Source", values_to = "value") %>%
    ggplot(aes(x=Date, y=value)) +
    geom_line(aes(linetype=Data_Source)) +
    theme_bw() +
    ggtitle("Mean Soil Water Content\nApril-September 2020", siteID) +
    xlab("Date") + ylab("Mean SWC (%)")
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_LinePlot_SMAP_meanSoilWC_2021.png", siteID)), plot = neon_af_clim_smap_day_2021, width = 7, height = 7)
  
  
}  

# Save cummulative stats data frames to files
write.csv(stats_neon_af_clim_smap_meanSWC, file=("~/R_Scripts/NEON-Climate-variable-validation/data_out/NEON_AF_clim_smap_stats_meanSoilWC_no_shift.csv"))

