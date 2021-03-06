### figure 4 plot for 2nd course project, Exploratory Data Analysis JHU
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

### add emission names (some columns only) to emissions.data table

emissions.data <- left_join(emissions.data,
                            code.table[,c('SCC','Short.Name','EI.Sector','SCC.Level.Three','SCC.Level.Four')],
                            by='SCC')

### filter to data which contains 'coal' in any of several description names
### group by year, and find total emissions (sum of all types) per year

yearly.total <- emissions.data %>%
  filter(grepl('coal',
               paste(EI.Sector, SCC.Level.Three, SCC.Level.Four),
               ignore.case = TRUE)) %>%
  group_by(year) %>% 
  summarise(Emissions = sum(Emissions)) %>%
  arrange(year)

png(filename = 'plot4.png', width = 480, height = 480)

myplot <- ggplot(yearly.total,
                 aes (x = year, y= Emissions)) +
  geom_line() +
  labs(x = 'Year') + labs(y = 'PM 2.5 Emissions (tonnes)') +
  labs(title = 'Coal-related PM2.5 emissions USA, total') +
  labs(subtitle = '1999 to 2008')

print(myplot)

dev.off()
