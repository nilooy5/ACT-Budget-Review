library(readr)
library(plotrix)
library(ggplot2)

Budget_review <- read_csv("Budget_review_2021-2022_Infrastructure_Investment_Program.csv")


#Checking missing value

sum(is.na(Budget_review))



#Removing one -119 value from dataset

data <- Budget_review                   # Duplicate data frame
data[data < 0] <- NA       # Replace negative values by NA
data

data <- na.omit(data)      # Remove rows with NA values
data
##########################

#pie chart

x1 <- data$`Total Budgeted Financing ($'000)`
lbl <- data$`Wellbeing Domain/ Government Priority`
library(plotly)

USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[,c('Categorie', 'X1960')]

fig <- plot_ly(data, labels = ~lbl, values = ~x1, type = 'pie')
fig <- fig %>% layout(title = 'Wellbeing Domain/ Government Priority by Total Budgeted Financing',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig
