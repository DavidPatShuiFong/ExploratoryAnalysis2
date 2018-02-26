### figure 3 plot for 2nd course project, Exploratory Data Analysis JHU
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
### secondary grouping by year ('factor'ed version)
### restrict data to Baltimore City, Maryland (fips == '24510)

yearly.total <- emissions.data %>%
  filter(fips == '24510') %>%
  group_by(year, type = factor(type)) %>% 
  summarise(Emissions = sum(Emissions))

yearly.total
png(filename = 'plot3.png', width = 480, height = 480)

ggplot(yearly.total,
       aes (x = year, y= Emissions, colour = type)) +
  geom_line() +
  labs(x = 'Year') + labs(y = 'PM 2.5 Emissions (tonnes)') +
  labs(title = 'PM2.5 emissions Baltimore City, Maryland, by type') +
  labs(subtitle = '1999 to 2008')

dev.off()
