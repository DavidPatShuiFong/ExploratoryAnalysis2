### figure 1 plot for 2nd course project, Exploratory Data Analysis JHU
### 

### This assignment uses data sourced from the US Environmental Protection Agency
### Fine particular matter (PM2.5), National Emissions Inventory

library(tidyverse)
library(ggplot2)

### check if data file already present, if not, then download the file (which is a .zip archive)
### if .zip archive available, extract the data
if (!file.exists('summarySCC_PM25.rds')) {
  if (!file.exists('exdataNEI.zip')) {
    if (download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",'exdataNEI.zip')) {
      stop('Unable to download data file')
    }
  }
  unzip('exdataNEI.zip')
}

summary.emissions <- as.tibble(readRDS('summarySCC_PM25.rds'))
code.table <- as.tibble(readRDS('Source_Classification_Code.rds'))