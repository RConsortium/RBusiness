#Pull in tidyverse
library(tidyverse)
library(readr)
#Check Updates
tidyverse_update()

#Load Fin Data
fin_data <- read_csv("~/Documents/GitHub/RBusiness/Developement/Tidy Accounting/Data/financial_data.csv")
View(fin_data)

# Basic Plotting

#Plot 1: Basic Scatterplot
ggplot(data = subset(fin_data, country_code == 'US')) + 
  geom_point(mapping = aes(x = EBITDA, y = Revenue))

#Plot 2: Aesthetic Mapping
ggplot(data = subset(fin_data, country_code == 'US')) + 
  geom_point(mapping = aes(x = EBITDA, y = Revenue, color = Ticker),
  show.legend = FALSE)

#Plot 3: Facets
ggplot(data = subset(fin_data, country_code == 'US')) + 
  geom_point(mapping = aes(x = EBITDA, y = Revenue)) + 
  facet_wrap(~ `Fiscal Year`, nrow = 3)

#Plot 4: Facet Grid
ggplot(data = subset(fin_data, country_code == 'US')) + 
  geom_point(mapping = aes(x = EBITDA, y = Revenue)) + 
    facet_grid(`Fiscal Year` ~ industries)

#Plot 5: Geometric objects
ggplot(data = subset(fin_data, country_code == 'US')) + 
  geom_smooth(mapping = aes(x = EBITDA, y = Revenue))

#Plot 6: Multiple Geoms on one plot - Not working yet!
ggplot(data = subset(fin_data, country_code == 'US')) + 
  geom_smooth(mapping = aes(x = EBITDA, y = Revenue, color = industries),
    show.legend = FALSE)

#Plot 6: Place Geom and Point on the same plot
ggplot(data = subset(fin_data, country_code == 'US'),mapping = aes(x = EBITDA, y = Revenue)) + 
  geom_point(mapping = aes(color = industries), show.legend = FALSE) + 
  geom_smooth()

#Load Proc Data
proc_data <- read_csv("~/Documents/GitHub/RBusiness/Developement/Tidy Accounting/Data/procurement_data.csv")
View(proc_data)

# Statistical Transformation

#Plot 6: Bar chart
ggplot(data = proc_data) + 
  geom_bar(mapping = aes(x = `Notice Type`))

#Plot 7: Display Proportions
ggplot(data = proc_data) + 
  geom_bar(mapping = aes(x = `Notice Type`, y = stat(prop), group = 1))

#Plot 8: Reorder Bars - Not working yet!
ggplot(data = proc_data) + 
  geom_bar(mapping = aes(x = reorder(`Notice Type`, -stat(prop)), y = stat(prop), group = 1))

#Plot 9: More Stats - not working yet
ggplot(data = proc_data) + 
  stat_summary(mapping = aes(x = `Notice Type`, y = stat(prop)),
              fun.min = min,
              fun.max = max,
              fun = median)

#Plot 10: Display sections in Bar Chart
ggplot(data = proc_data) + 
  geom_bar(mapping = aes(x = `Notice Type`, fill = `Country Code`), show.legend = FALSE)

#Plot 11: Make bars same height
ggplot(data = proc_data) + 
  geom_bar(mapping = aes(x = `Notice Type`, fill = `Country Code`), position = 'fill', show.legend = FALSE)

#Plot 12: Spread bars out
ggplot(data = proc_data) + 
  geom_bar(mapping = aes(x = `Notice Type`, fill = `Country Code`), position = 'dodge', show.legend = FALSE)


# Other Data

#Cash Flow Data

#Load Data
cash_data <- read_csv("Library/Mobile Documents/com~apple~CloudDocs/Education/R/Tidy Accounting/Data/cashflow_data.csv")

View(cash_data)

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
