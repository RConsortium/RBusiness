#Pull in tidyverse
library(tidyverse)
library(readr)
#Check Updates
tidyverse_update()

#Load Fin Data
fin_data <- read_csv("~/Documents/GitHub/RBusiness/Developement/Tidy Accounting/Data/financial_data.csv")
View(fin_data)

#Create Plot
ggplot(data = subset(fin_data, country_code == 'US')) + 
  geom_point(mapping = aes(x = EBITDA, y = Revenue))

# Aesthetic Mapping
ggplot(data = subset(fin_data, country_code == 'US')) + 
  geom_point(mapping = aes(x = EBITDA, y = Revenue, color = Ticker),
  show.legend = FALSE)

#Facets

# Facet Your Plot
ggplot(data = subset(fin_data, country_code == 'US')) + 
  geom_point(mapping = aes(x = EBITDA, y = Revenue)) + 
  facet_wrap(~ `Fiscal Year`, nrow = 3)

# Facet Grid
ggplot(data = subset(fin_data, country_code == 'US')) + 
  geom_point(mapping = aes(x = EBITDA, y = Revenue)) + 
    facet_grid(`Fiscal Year` ~ industries)

#Geometric objects

ggplot(data = subset(fin_data, country_code == 'US')) + 
  geom_smooth(mapping = aes(x = EBITDA, y = Revenue))

# Multiple Geoms on one plot
ggplot(data = subset(fin_data, country_code == 'US')) + 
  geom_smooth(mapping = aes(x = EBITDA, y = Revenue, color = industries),
    show.legend = FALSE)

# Place Geom and Point on the same plot
ggplot(data = subset(fin_data, country_code == 'US'),mapping = aes(x = EBITDA, y = Revenue)) + 
  geom_point(mapping = aes(color = industries), show.legend = FALSE) + 
  geom_smooth()

#Load Proc Data
proc_data <- read_csv("~/Documents/GitHub/RBusiness/Developement/Tidy Accounting/Data/procurement_data.csv")
View(proc_data)

# Statistical Transformation

# Bar chart
ggplot(data = proc_data) + 
  geom_bar(mapping = aes(x = `Notice Type`))

# Display Proportions
ggplot(data = proc_data) + 
  geom_bar(mapping = aes(x = `Notice Type`, y = stat(prop), group = 1))

# Reorder Bars
ggplot(data = proc_data) + 
  geom_bar(mapping = aes(x = reorder(`Notice Type`, -stat(prop)), y = stat(prop), group = 1))

# More Stats
ggplot(data = proc_data) + 
  stat_summary(mapping = aes(x = `Notice Type`, y = stat(prop)),
              fun.min = min,
              fun.max = max,
              fun = median)

# Display sections in Bar Chart
ggplot(data = proc_data) + 
  geom_bar(mapping = aes(x = `Notice Type`, fill = `Country Code`), show.legend = FALSE)

# Make bars same height
ggplot(data = proc_data) + 
  geom_bar(mapping = aes(x = `Notice Type`, fill = `Country Code`), position = 'fill', show.legend = FALSE)

# Spread bars out
ggplot(data = proc_data) + 
  geom_bar(mapping = aes(x = `Notice Type`, fill = `Country Code`), position = 'dodge', show.legend = FALSE)


#Sales Data

#Load Data
sales_data <- read_csv("Library/Mobile Documents/com~apple~CloudDocs/Education/R/Tidy Accounting/Data/sales_data.csv")

ggplot(data=sales_data, aes(x=YEAR_ID, y=SALES)) +
  geom_bar(stat="identity")


#Ecom Data

#Load Data
ecom_data <- read_csv("Library/Mobile Documents/com~apple~CloudDocs/Education/R/Tidy Accounting/Data/ecom_data.csv")


#Procurement Data

#Load Data
proc_data <- read_csv("Library/Mobile Documents/com~apple~CloudDocs/Education/R/Tidy Accounting/Data/procurement_data.csv")

#Cash Flow Data

#Load Data
cash_data <- read_csv("Library/Mobile Documents/com~apple~CloudDocs/Education/R/Tidy Accounting/Data/cashflow_data.csv")



