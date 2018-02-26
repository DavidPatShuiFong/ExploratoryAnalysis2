### figure 1 plot for 2nd course project, Exploratory Data Analysis JHU
### 

### This assignment uses data sourced from the US Environmental Protection Agency
### Fine particular matter (PM2.5), National Emissions Inventory

library(tidyverse)
library(dplyr)
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

emissions.data <- as.tibble(readRDS('summarySCC_PM25.rds'))
code.table <- as.tibble(readRDS('Source_Classification_Code.rds'))

### group by year, and find total emissions (sum of all types) per year

yearly.total <- emissions.data %>% 
  group_by(year) %>% 
  summarise(Emissions = sum(Emissions)) %>%
  arrange(year)

png(filename = 'plot1.png', width = 480, height = 480)

plot(unlist(yearly.total$year), unlist(yearly.total$Emissions),type='l',
     main = 'Total PM2.5 emission USA, 1999 to 2008',
     xlab = 'Year',
     ylab = 'Tonnes of emission')

dev.off()
