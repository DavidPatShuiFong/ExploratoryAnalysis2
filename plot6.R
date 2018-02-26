### figure 6 plot for 2nd course project, Exploratory Data Analysis JHU
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
                            code.table[,c('SCC','Data.Category','Short.Name','EI.Sector','SCC.Level.Three','SCC.Level.Four')],
                            by='SCC')

### filter to data for motor vehicle sources
### this appears to be best determined by using the 'OnRoad' Data.Category
### (see Source Classification Code description file
### at https://ofmpub.epa.gov/sccsearch/docs/SCC-IntroToSCCs.pdf)

### filter to Baltimore City (fips == '24510') or LA County ('06037')

### group by year, and find total emissions (sum of all types) per year

yearly.total <- emissions.data %>%
  filter(Data.Category == 'Onroad') %>% ### related to motor vehicles
  filter(fips == '24510' | fips == '06037') %>%
  mutate(county = ifelse(fips == '24510', 'Baltimore City', 'Los Angeles County')) %>%
  group_by(year, county) %>% 
  summarise(Emissions = sum(Emissions)) %>%
  

png(filename = 'plot6.png', width = 480, height = 480)

myplot <- ggplot(yearly.total,
                 aes (x = year, y = Emissions, colour = county)) +
  geom_line() +
  labs(x = 'Year') + labs(y = 'PM 2.5 Emissions (tonnes)') +
  labs(title = 'Motor-vehicle PM2.5 emissions, Baltimore City and LA') +
  labs(subtitle = '1999 to 2008')

print(myplot)

dev.off()
