#Pull in tidyverse
library(tidyverse)
library(readr)
#Check Updates
tidyverse_update()

#Fin-Data

#Load Data
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
ggplot(data = subset(fin_data, country_code == 'US' && Ticker == 'AAMC')) + 
  geom_smooth(
    mapping = aes(x = EBITDA, y = Revenue, color = Ticker),
    show.legend = FALSE)


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



