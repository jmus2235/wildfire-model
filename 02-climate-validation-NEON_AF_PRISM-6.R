## Climate variable validation script to compare temperature, vapor pressure deficit and precipitation data 
## from PRISM with NEON tower data
## Copyright NEON 2023

## ----setup-env--------------------------------------------------------------------------------------------
# Install needed package (only uncomment & run if not already installed)
#install.packages("neonUtilities")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("lubridate")
#install.packages("Metrics")

library(ggplot2)  # for plotting
library(dplyr)  # for data munging
library(tidyr)  # for data munging
library(tidyverse)
library(ggpubr)
library(lubridate)
library(Metrics)

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
stats_neon_af_clim_prism_maxVPD <- data.frame(site=character(),
                                              r_squared=double(),
                                              slope=double(),
                                              intercept=double(),
                                              std_error=double(),
                                              mean_bias=double(),
                                              stringsAsFactors=FALSE)

stats_neon_af_clim_prism_maxTemp <- data.frame(site=character(),
                                               r_squared=double(),
                                               slope=double(),
                                               intercept=double(),
                                               std_error=double(),
                                               mean_bias=double(),
                                               stringsAsFactors=FALSE)

stats_neon_af_clim_prism_sumPPT <- data.frame(site=character(),
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
  neon_af_clim$TIMESTAMP_START <- neon_af_clim$TIMESTAMP_START + lubridate::hours(12)
  neon_af_clim$TIMESTAMP_END <- neon_af_clim$TIMESTAMP_END + lubridate::hours(12)
  
  #Convert time stamp to date
  neon_af_clim$Date <- as.Date(neon_af_clim$TIMESTAMP_START)
  
  # Subset to summer months
  neon_af_clim_2018 <- neon_af_clim %>% filter(between(Date, as.Date('2018-04-01'), as.Date('2018-09-30')))
  neon_af_clim_2019 <- neon_af_clim %>% filter(between(Date, as.Date('2019-04-01'), as.Date('2019-09-30')))
  neon_af_clim_2020 <- neon_af_clim %>% filter(between(Date, as.Date('2020-04-01'), as.Date('2020-09-30')))
  neon_af_clim_2021 <- neon_af_clim %>% filter(between(Date, as.Date('2021-04-01'), as.Date('2021-09-30')))
  
  # Put all data frames into list
  df_list <- list(neon_af_clim_2018, neon_af_clim_2019, neon_af_clim_2020, neon_af_clim_2021)
  
  # Merge all data frames in list
  neon_af_clim_summer <- df_list %>% reduce(full_join, by='Date')
  
  neon_af_clim_summer <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
  
  neon_af_clim <- neon_af_clim_summer
  
  # Pull the variables
  tempAir <- neon_af_clim$TA_1_1_1
  rh <- neon_af_clim$RH
  presAir <- neon_af_clim$PA
  vpd <- neon_af_clim$VPD_PI
  ppt <- neon_af_clim$P
  
  # Re-insert NAs
  tempAir[tempAir == -9999] <- NA
  rh[rh == -9999] <- NA
  presAir[presAir == -9999] <- NA
  vpd[vpd == -9999] <- NA
  ppt[ppt == -9999] <- NA
  
  # Add climate variables to data frame
  neon_af_clim$tempAir <- tempAir
  neon_af_clim$relHum <- rh
  neon_af_clim$vapPresDef <- vpd
  neon_af_clim$presAir <- presAir
  neon_af_clim$ppt <- ppt
  
  # Drop NA values from variables and convert time stamp to date
  # Air Temp
  neon_af_clim_noNA_tempAir <- neon_af_clim %>%
    drop_na(tempAir)
  
  # PPT
  neon_af_clim_noNA_ppt <- neon_af_clim %>%
    drop_na(ppt)
  
  # VPD
  neon_af_clim_noNA_vapPresDef <- neon_af_clim %>%
    drop_na(vapPresDef)
  
  # Reduce and group by maximum air temperature per day
  neon_af_clim_maxTemp_day <- neon_af_clim_noNA_tempAir %>%
    group_by(Date) %>%
    slice(which.max(tempAir))
  
  neon_af_clim_maxTemp_day$maxTemp <- neon_af_clim_maxTemp_day$tempAir
  
  neon_af_clim_maxTemp_day <- subset(neon_af_clim_maxTemp_day, select = c(Date, maxTemp))
  
  # Subset and sum precipitation per day
  neon_af_PPT = subset(neon_af_clim_noNA_ppt, select = c(Date, P))
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
  
  # Subset and calculate mean VPD per day
  neon_af_VPD = subset(neon_af_clim_noNA_vapPresDef, select = c(Date, vapPresDef))
  neon_af_VPD <- neon_af_VPD %>%
    group_by(Date) %>%
    #distinct(Date, .keep_all=T) %>%
    mutate(meanVPD=mean(vapPresDef))
  
  neon_af_meanVPD_day <- neon_af_VPD %>%
    group_by(Date) %>%
    distinct(Date, .keep_all=T)
  
  neon_af_meanVPD_day <- neon_af_meanVPD_day %>%
    drop_na(Date)
  
  neon_af_meanVPD_day = subset(neon_af_meanVPD_day, select = c(Date, meanVPD))
  
  neon_af_meanVPD_day$meanVPD <- neon_af_meanVPD_day$meanVPD/10
  
  # Reduce and group by maximum VPD per day
  neon_af_vapPresDef = subset(neon_af_clim_noNA_vapPresDef, select = c(Date, vapPresDef)) 
  
  neon_af_maxVPD_day <- neon_af_vapPresDef %>%
    group_by(Date) %>%
    slice(which.max(vapPresDef))
  
  
  neon_af_maxVPD_day$maxVPD <- neon_af_maxVPD_day$vapPresDef# /10

  #---------------- Read and process PRISM data file--------------------------------
  
  # Read PRISM csv file
  df <- read.csv(file=sprintf("PRISM_2018_2021_%s.csv", siteID), stringsAsFactors = FALSE)
  
  # Separate YEAR, MONTH and DAY
  df <- extract(df, system.index, into = c("YEAR", "MONTH", "DAY"), "(.{4})(.{2})(.{2})")
  
  # Convert to numeric
  df$P_YEAR <- as.numeric(df$YEAR)
  df$P_MONTH <- as.numeric(df$MONTH)
  df$P_DAY <- as.numeric(df$DAY)
  
  df$Date<-as.Date(with(df,paste(YEAR,MONTH,DAY,sep="-")),"%Y-%m-%d")
  
  df$PRISM_daily_tmax <- df$tmax 
  
  df$PRISM_daily_vpdmax <- df$vpdmax
  
  df$PRISM_daily_sumPPT <- df$ppt
  
  df = subset(df, select = -c(date,.geo, YEAR, MONTH, DAY, tmax, vpdmax, ppt))
  
  neon_af_clim_prism_day_maxTemp <- merge(x = neon_af_clim_maxTemp_day, y = df, by = "Date")
  
  neon_af_clim_prism_day_sumPPT <- merge(x = neon_af_sumPPT_day, y = df, by = "Date")
  
  neon_af_clim_prism_day_maxVPD <- merge(x = neon_af_maxVPD_day, y = df, by = "Date")
  
  ## ----Save csv file ------------------------------------------------------------------------------
  ## 
  write.csv(neon_af_clim_prism_day_maxTemp, file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/data_out/NEON_AF_clim_prism_maxTemp_%s.csv", siteID)), row.names=F)
  
  write.csv(neon_af_clim_prism_day_sumPPT, file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/data_out/NEON_AF_clim_prism_sumPPT_%s.csv", siteID)), row.names=F)
  
  write.csv(neon_af_clim_prism_day_maxVPD, file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/data_out/NEON_AF_clim_prism_maxVPD_%s.csv", siteID)), row.names=F)
  
  #---------------- Create linear regression models for Temp and VPD and output stats to data frames ---------------------------------------
  
  model_tmax <- lm(PRISM_daily_tmax ~ maxTemp, data = neon_af_clim_prism_day_maxTemp)
  stat_lm_tmax <- data.frame(siteID, summary(model_tmax)$r.squared, summary(model_tmax)$coefficients[2], summary(model_tmax)$coefficients[1], summary(model_tmax)$sigma, mae(neon_af_clim_prism_day_maxTemp$PRISM_daily_tmax, predict(model_tmax)))
  names(stat_lm_tmax) <- c("site", "r_squared", "slope", "intercept", "std_error", "mean_bias")
  stats_neon_af_clim_prism_maxTemp <- bind_rows(stats_neon_af_clim_prism_maxTemp, stat_lm_tmax)
  
  model_sumppt <- lm(PRISM_daily_sumPPT ~ sumPPT, data = neon_af_clim_prism_day_sumPPT)
  stat_lm_sumppt <- data.frame(siteID, summary(model_sumppt)$r.squared, summary(model_sumppt)$coefficients[2], summary(model_sumppt)$coefficients[1], summary(model_sumppt)$sigma, mae(neon_af_clim_prism_day_sumPPT$PRISM_daily_sumPPT, predict(model_sumppt)))
  names(stat_lm_sumppt) <- c("site", "r_squared", "slope", "intercept", "std_error", "mean_bias")
  stats_neon_af_clim_prism_sumPPT <- bind_rows(stats_neon_af_clim_prism_sumPPT, stat_lm_sumppt)
  
  model_vpdmax <- lm(PRISM_daily_vpdmax ~ maxVPD, data = neon_af_clim_prism_day_maxVPD)
  stat_lm_vpdmax <- data.frame(siteID, summary(model_vpdmax)$r.squared, summary(model_vpdmax)$coefficients[2], summary(model_vpdmax)$coefficients[1], summary(model_vpdmax)$sigma, mae(neon_af_clim_prism_day_maxVPD$PRISM_daily_vpdmax, predict(model_vpdmax)))
  names(stat_lm_vpdmax) <- c("site", "r_squared", "slope", "intercept", "std_error", "mean_bias")
  stats_neon_af_clim_prism_maxVPD <- bind_rows(stats_neon_af_clim_prism_maxVPD, stat_lm_vpdmax)
  
  write.csv(stats_neon_af_clim_prism_maxVPD, file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/data_out/temp/NEON_AF_clim_prism_stats_maxVPD_%s.csv", siteID)))
  
  write.csv(stats_neon_af_clim_prism_maxTemp, file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/data_out/temp/NEON_AF_clim_prism_stats_maxTemp_%s.csv", siteID)))
  
  write.csv(stats_neon_af_clim_prism_sumPPT, file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/data_out/temp/NEON_AF_clim_prism_stats_sumPPT_%s.csv", siteID)))  
  
  #---------------- Create scatter plots ----------------------------------------------------------------------------
  
  maxvpdScatter_prism <- ggplot(neon_af_clim_prism_day_maxVPD,aes(x = PRISM_daily_vpdmax, y = maxVPD)) + 
    geom_point(color="black", size=0.5) + 
    geom_smooth(method = "lm", se=FALSE) +
    stat_regline_equation(label.y = 25, aes(label = ..eq.label..)) +
    stat_regline_equation(label.y = 22, aes(label = ..rr.label..)) +
    ggtitle("Maximum Daily Vapor Pressure Deficit\nApril-September 2018-2021", siteID) +
    xlab("PRISM VPD (hPa)") + ylab("Tower VPD (hPa)")
  
  maxvpdScatter_prism
  
  tempScatter_prism <- ggplot(neon_af_clim_prism_day_maxTemp,aes(x = PRISM_daily_tmax, y = maxTemp)) + 
    geom_point(color="black", size=0.5) + 
    geom_smooth(method = "lm", se=FALSE) +
    stat_regline_equation(label.y = 30, aes(label = ..eq.label..)) +
    stat_regline_equation(label.y = 27, aes(label = ..rr.label..)) +
    ggtitle("Maximum Daily Temperature\nApril-September 2018-2021", siteID) +
    xlab("PRISM Temp (C°)") + ylab("Tower Temp (C°)")
  
  tempScatter_prism
  
  sumpptScatter_prism <- ggplot(neon_af_clim_prism_day_sumPPT,aes(x = PRISM_daily_sumPPT, y = sumPPT)) + 
    geom_point(color="black", size=0.5) + 
    geom_smooth(method = "lm", se=FALSE) +
    stat_regline_equation(label.y = 60, aes(label = ..eq.label..)) +
    stat_regline_equation(label.y = 50, aes(label = ..rr.label..)) +
    ggtitle("Cumulative Daily Precipitation\nApril-September 2018-2021", siteID) +
    xlab("PRISM PPT (mm)") + ylab("Tower PPT (mm)")
  
  sumpptScatter_prism
  
  ## ----Save scatter plots ------------------------------------------------------------------------------
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_PRISM_maxVPD_ScatterPlot.png", siteID)), plot = maxvpdScatter_prism, width = 7, height = 7)
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_PRISM_maxTemp_ScatterPlot.png", siteID)), plot = tempScatter_prism, width = 7, height = 7)
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_PRISM_sumPPT_ScatterPlot.png", siteID)), plot = sumpptScatter_prism, width = 7, height = 7)
  
  ## ----Create and save VPD line plots------------------------------------------------------------------------------
  
  neon_af_clim_prism_day_subset <- neon_af_clim_prism_day_maxVPD[c("Date", "maxVPD", "PRISM_daily_vpdmax")]
  neon_af_clim_prism_day_subset$Date <- ymd(neon_af_clim_prism_day_subset$Date)
  neon_af_clim_prism_day_subset <- neon_af_clim_prism_day_subset %>% rename("Tower_maxVPD" = "maxVPD", "PRISM_maxVPD" = "PRISM_daily_vpdmax")
  
  # Subset and plot 2018
  neon_af_clim_prism_day_2018 <- neon_af_clim_prism_day_subset %>% filter(between(Date, as.Date('2018-04-01'), as.Date('2018-09-30')))
  
  vpdLine_neon_af_clim_prism_day_2018 <- neon_af_clim_prism_day_2018 %>% pivot_longer(cols = -Date, names_to = "Data_Source", values_to = "value") %>%
    ggplot(aes(x=Date, y=value)) +
    geom_line(aes(linetype=Data_Source)) +
    theme_bw() + 
    ggtitle("Max Daily Vapor Pressure Deficit\nApril-September 2018", siteID) +
    xlab("Date") + ylab("Max VPD")
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_LinePlot_PRISM_maxVPD_2018.png", siteID)), plot = vpdLine_neon_af_clim_prism_day_2018, width = 7, height = 7)
  
  # Subset and plot 2019
  neon_af_clim_prism_day_2019 <- neon_af_clim_prism_day_subset %>% filter(between(Date, as.Date('2019-04-01'), as.Date('2019-09-30')))
  
  vpdLine_neon_af_clim_prism_day_2019 <- neon_af_clim_prism_day_2019 %>% pivot_longer(cols = -Date, names_to = "Data_Source", values_to = "value") %>%
    ggplot(aes(x=Date, y=value)) +
    geom_line(aes(linetype=Data_Source)) +
    theme_bw() +
    ggtitle("Max Daily Vapor Pressure Deficit\nApril-September 2019", siteID) +
    xlab("Date") + ylab("Max VPD")
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_LinePlot_PRISM_maxVPD_2019.png", siteID)), plot = vpdLine_neon_af_clim_prism_day_2019, width = 7, height = 7)
  
  # Subset and plot 2020
  neon_af_clim_prism_day_2020 <- neon_af_clim_prism_day_subset %>% filter(between(Date, as.Date('2020-04-01'), as.Date('2020-09-30')))
  
  vpdLine_neon_af_clim_prism_day_2020 <- neon_af_clim_prism_day_2020 %>% pivot_longer(cols = -Date, names_to = "Data_Source", values_to = "value") %>%
    ggplot(aes(x=Date, y=value)) +
    geom_line(aes(linetype=Data_Source)) +
    theme_bw() +
    ggtitle("Max Daily Vapor Pressure Deficit\nApril-September 2020", siteID) +
    xlab("Date") + ylab("Max VPD")
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_LinePlot_PRISM_maxVPD_2020.png", siteID)), plot = vpdLine_neon_af_clim_prism_day_2020, width = 7, height = 7)
  
  # Subset and plot 2021
  
  neon_af_clim_prism_day_2021 <- neon_af_clim_prism_day_subset %>% filter(between(Date, as.Date('2021-04-01'), as.Date('2021-09-30')))
  
  vpdLine_neon_af_clim_prism_day_2021 <- neon_af_clim_prism_day_2021 %>% pivot_longer(cols = -Date, names_to = "Data_Source", values_to = "value") %>%
    ggplot(aes(x=Date, y=value)) +
    geom_line(aes(linetype=Data_Source)) +
    theme_bw() +
    ggtitle("Max Daily Vapor Pressure Deficit\nApril-September 2020", siteID) +
    xlab("Date") + ylab("Max VPD")
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_LinePlot_PRISM_maxVPD_2021.png", siteID)), plot = vpdLine_neon_af_clim_prism_day_2021, width = 7, height = 7)
  
  
  # ----Create and save Temp line plots------------------------------------------------------------------------------
  
  neon_af_clim_prism_day_subset <- neon_af_clim_prism_day_maxTemp[c("Date", "maxTemp", "PRISM_daily_tmax")]
  neon_af_clim_prism_day_subset$Date <- ymd(neon_af_clim_prism_day_subset$Date)
  neon_af_clim_prism_day_subset <- neon_af_clim_prism_day_subset %>% rename("Tower_maxTemp" = "maxTemp", "PRISM_maxTemp" = "PRISM_daily_tmax")
  
  # Subset and plot 2018
  neon_af_clim_prism_day_subset_2018 <- neon_af_clim_prism_day_subset %>% filter(between(Date, as.Date('2018-04-01'), as.Date('2018-09-30')))
  
  tempLine_neon_af_clim_prism_day_subset_2018 <- neon_af_clim_prism_day_subset_2018 %>% pivot_longer(cols = -Date, names_to = "Data_Source", values_to = "value") %>%
    ggplot(aes(x=Date, y=value)) +
    geom_line(aes(linetype=Data_Source)) +
    theme_bw() +
    ggtitle("Maximum Daily Temperature\nApril-September 2018", siteID) +
    xlab("Date") + ylab("Temp (C°)")
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_LinePlot_PRISM_maxTemp_2018.png", siteID)), plot = tempLine_neon_af_clim_prism_day_subset_2018, width = 7, height = 7)
  
  # Subset and plot 2019
  neon_af_clim_prism_day_subset_2019 <- neon_af_clim_prism_day_subset %>% filter(between(Date, as.Date('2019-04-01'), as.Date('2019-09-30')))
  
  tempLine_neon_af_clim_prism_day_subset_2019 <- neon_af_clim_prism_day_subset_2019 %>% pivot_longer(cols = -Date, names_to = "Data_Source", values_to = "value") %>%
    ggplot(aes(x=Date, y=value)) +
    geom_line(aes(linetype=Data_Source)) +
    theme_bw() +
    ggtitle("Maximum Daily Temperature\nApril-September 2019", siteID) +
    xlab("Date") + ylab("Temp (C°)")
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_LinePlot_PRISM_maxTemp_2019.png", siteID)), plot = tempLine_neon_af_clim_prism_day_subset_2019, width = 7, height = 7)
  
  # Subset and plot 2020
  neon_af_clim_prism_day_subset_2020 <- neon_af_clim_prism_day_subset %>% filter(between(Date, as.Date('2020-04-01'), as.Date('2020-09-30')))
  
  tempLine_neon_af_clim_prism_day_subset_2020 <- neon_af_clim_prism_day_subset_2020 %>% pivot_longer(cols = -Date, names_to = "Data_Source", values_to = "value") %>%
    ggplot(aes(x=Date, y=value)) +
    geom_line(aes(linetype=Data_Source)) +
    theme_bw() +
    ggtitle("Maximum Daily Temperature\nApril-September 2020", siteID) +
    xlab("Date") + ylab("Temp (C°)")
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_LinePlot_PRISM_maxTemp_2020.png", siteID)), plot = tempLine_neon_af_clim_prism_day_subset_2020, width = 7, height = 7)
  
  # Subset and plot 2021
  
  neon_af_clim_prism_day_subset_2021 <- neon_af_clim_prism_day_subset %>% filter(between(Date, as.Date('2021-04-01'), as.Date('2021-09-30')))
  
  tempLine_neon_af_clim_prism_day_subset_2021 <- neon_af_clim_prism_day_subset_2021 %>% pivot_longer(cols = -Date, names_to = "Data_Source", values_to = "value") %>%
    ggplot(aes(x=Date, y=value)) +
    geom_line(aes(linetype=Data_Source)) +
    theme_bw() +
    ggtitle("Maximum Daily Temperature\nApril-September 2021", siteID) +
    xlab("Date") + ylab("Temp (C°)")
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_LinePlot_PRISM_maxTemp_2021.png", siteID)), plot = tempLine_neon_af_clim_prism_day_subset_2021, width = 7, height = 7)
  
  
  # ----Create and save cummulative PPT line plots------------------------------------------------------------------------------
  
  neon_af_clim_prism_day_subset <- neon_af_clim_prism_day_sumPPT[c("Date", "sumPPT", "PRISM_daily_sumPPT")]
  neon_af_clim_prism_day_subset$Date <- ymd(neon_af_clim_prism_day_subset$Date)
  neon_af_clim_prism_day_subset <- neon_af_clim_prism_day_subset %>% rename("Tower_Daily_sumPPT" = "sumPPT", "PRISM_Daily_sumPPT" = "PRISM_daily_sumPPT")
  
  # Subset and plot 2018
  neon_af_clim_prism_day_subset_2018 <- neon_af_clim_prism_day_subset %>% filter(between(Date, as.Date('2018-04-01'), as.Date('2018-09-30')))
  
  sumpptLine_neon_af_clim_prism_day_subset_2018 <- neon_af_clim_prism_day_subset_2018 %>% pivot_longer(cols = -Date, names_to = "Data_Source", values_to = "value") %>%
    ggplot(aes(x=Date, y=value)) +
    geom_line(aes(linetype=Data_Source)) +
    theme_bw() +
    ggtitle("Cummulative Daily Precipitation\nApril-September 2018", siteID) +
    xlab("Date") + ylab("PPT (mm)")
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_LinePlot_PRISM_sumPPT_2018.png", siteID)), plot = sumpptLine_neon_af_clim_prism_day_subset_2018, width = 7, height = 7)
  
  # Subset and plot 2019
  neon_af_clim_prism_day_subset_2019 <- neon_af_clim_prism_day_subset %>% filter(between(Date, as.Date('2019-04-01'), as.Date('2019-09-30')))
  
  sumpptLine_neon_af_clim_prism_day_subset_2019 <- neon_af_clim_prism_day_subset_2019 %>% pivot_longer(cols = -Date, names_to = "Data_Source", values_to = "value") %>%
    ggplot(aes(x=Date, y=value)) +
    geom_line(aes(linetype=Data_Source)) +
    theme_bw() +
    ggtitle("Cummulative Daily Precipitation\nApril-September 2019", siteID) +
    xlab("Date") + ylab("PPT (mm)")
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_LinePlot_PRISM_sumPPT_2019.png", siteID)), plot = sumpptLine_neon_af_clim_prism_day_subset_2019, width = 7, height = 7)
  
  # Subset and plot 2020
  neon_af_clim_prism_day_subset_2020 <- neon_af_clim_prism_day_subset %>% filter(between(Date, as.Date('2020-04-01'), as.Date('2020-09-30')))
  
  sumpptLine_neon_af_clim_prism_day_subset_2020 <- neon_af_clim_prism_day_subset_2020 %>% pivot_longer(cols = -Date, names_to = "Data_Source", values_to = "value") %>%
    ggplot(aes(x=Date, y=value)) +
    geom_line(aes(linetype=Data_Source)) +
    theme_bw() +
    ggtitle("Cummulative Daily Precipitation\nApril-September 2020", siteID) +
    xlab("Date") + ylab("PPT (mm)")
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_LinePlot_PRISM_sumPPT_2020.png", siteID)), plot = sumpptLine_neon_af_clim_prism_day_subset_2020, width = 7, height = 7)
  
  # Subset and plot 2021
  neon_af_clim_prism_day_subset_2021 <- neon_af_clim_prism_day_subset %>% filter(between(Date, as.Date('2021-04-01'), as.Date('2021-09-30')))
  
  sumpptLine_neon_af_clim_prism_day_subset_2021 <- neon_af_clim_prism_day_subset_2021 %>% pivot_longer(cols = -Date, names_to = "Data_Source", values_to = "value") %>%
    ggplot(aes(x=Date, y=value)) +
    geom_line(aes(linetype=Data_Source)) +
    theme_bw() +
    ggtitle("Cummulative Daily Precipitation\nApril-September 2021", siteID) +
    xlab("Date") + ylab("PPT (mm)")
  
  ggsave(file=(sprintf("~/R_Scripts/NEON-Climate-variable-validation/png_out/%s_LinePlot_PRISM_sumPPT_2021.png", siteID)), plot = sumpptLine_neon_af_clim_prism_day_subset_2021, width = 7, height = 7)
  
}  

# Save cummulative stats data frames to files
write.csv(stats_neon_af_clim_prism_maxVPD, file=("~/R_Scripts/NEON-Climate-variable-validation/data_out/NEON_AF_clim_prism_stats_maxVPD_half-day_shift.csv"))

write.csv(stats_neon_af_clim_prism_maxTemp, file=("~/R_Scripts/NEON-Climate-variable-validation/data_out/NEON_AF_clim_prism_stats_maxTemp_half-day_shift.csv"))

write.csv(stats_neon_af_clim_prism_sumPPT, file=("~/R_Scripts/NEON-Climate-variable-validation/data_out/NEON_AF_clim_prism_stats_sumPPT_half-day_shift.csv"))
