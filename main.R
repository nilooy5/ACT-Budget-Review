library(readr)
library(plotrix)
library(ggplot2)
library(dplyr)

Budget_review <- read_csv("Budget_review_2021-2022_Infrastructure_Investment_Program.csv")

#Checking missing value
sum(is.na(Budget_review))
# check type of data
str(Budget_review)
data <- Budget_review                   # Duplicate data frame
names(Budget_review)                    # Check column names

x1 <- data$`Total Budgeted Financing ($'000)`
lbl <- data$`Wellbeing Domain/ Government Priority`
pclas <- data$`Project Classification`
ptyp <- data$`Project Type`
library(plotly)

# USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
# USPersonalExpenditure
# data <- USPersonalExpenditure[,c('Categorie', 'X1960')]

fig <- plot_ly(data, labels = ~lbl, values = ~x1, type = 'pie')
fig <- fig %>% layout(title = 'Wellbeing Domain/ Government Priority by Total Budgeted Financing',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig
plot_ly(data, labels = ~pclas, values = ~x1, type = 'pie')
plot_ly(data, labels = ~ptyp, values = ~x1, type = 'pie')
# order data by total budgeted financing in descending order
totalBudgetOrdered <- data[order(-data$`Total Budgeted Financing ($'000)`),]
top10 <- head(totalBudgetOrdered,10)
totalBudgetOrdered$`Total Budgeted Financing ($'000)`

# summerize data by project classification
sumByClass <- aggregate(totalBudgetOrdered$`Total Budgeted Financing ($'000)`, by=list(totalBudgetOrdered$`Project Classification`), FUN=sum)
sumByType <- aggregate(totalBudgetOrdered$`Total Budgeted Financing ($'000)`, by=list(totalBudgetOrdered$`Project Type`), FUN=sum)
sumByType
# aggregate multiple columns
budgetProgressPerType <- totalBudgetOrdered %>%
  group_by(`Project Type`) %>%
  summarise(
    "budget 2021-2022" = sum(`2021-2022 Budgeted Financing ($'000)`),
    "budget 2022-2023" = sum(`2022-2023 Budgeted Financing ($'000)`),
    "budget 2023-2024" = sum(`2023-2024 Budgeted Financing ($'000)`),
    "budget 2024-2025" = sum(`2024-2025 Budgeted Financing ($'000)`),
    "budget 2025-2026" = sum(`2025-2026 Budgeted Financing ($'000)`))

budgetProgressPerTypeWTotal <- totalBudgetOrdered %>%
  group_by(`Project Type`) %>%
  summarise(
    "budget 2021-2022" = sum(`2021-2022 Budgeted Financing ($'000)`),
    "budget 2022-2023" = sum(`2022-2023 Budgeted Financing ($'000)`),
    "budget 2023-2024" = sum(`2023-2024 Budgeted Financing ($'000)`),
    "budget 2024-2025" = sum(`2024-2025 Budgeted Financing ($'000)`),
    "budget 2025-2026" = sum(`2025-2026 Budgeted Financing ($'000)`),
    "total" = sum(`Total Budgeted Financing ($'000)`))

budgetProgressPerType
budgetProgressPerTypeWTotal
# budgetProgressPerTypeWTotal order by total
budgetProgressPerTypeWTotalOrdered <- budgetProgressPerTypeWTotal[order(-budgetProgressPerTypeWTotal$total),]
classesWithMostTotalBudget <- head(budgetProgressPerTypeWTotalOrdered, 5)
classesWithMostTotalBudget
budgetProgressPerType
# remove rows from budgetProgressPerType that are not in classesWithMostTotalBudget$`Project Type`
budgetProgressPerTypeTop5 <- budgetProgressPerType[(budgetProgressPerType$`Project Type` %in% classesWithMostTotalBudget$`Project Type`),]
budgetProgressPerTypeTop5

budgetTypeByYear <- budgetProgressPerType %>%
  gather(key = "bud_year", value = "budget", -`Project Type`)
# make line chart of budget progress per type
budgetTypeByYear %>%
  ggplot(aes(x = bud_year, y = budget, color = `Project Type`, group = `Project Type`)) +
  geom_line() +
  geom_point(size=2) +
  labs(title = "Budget Progress per Type", x = "Project Type", y = "Budget ($'000)")

budgetProgressPerType
# plot budget progress per type
budgetProgressPerType %>%
  ggplot(aes(x = `Project Type`, y = `budget 2021-2022`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Budget Progress per Type", x = "Project Type", y = "Budget ($'000)")
# plot budget progress per type
budgetProgressPerType %>%
  ggplot(aes(x = `Project Type`, y = `budget 2022-2023`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Budget Progress per Type", x = "Project Type", y = "Budget ($'000)")

# make barchart of top 10 projects
top10 %>%
  ggplot(aes(x = `Wellbeing Domain/ Government Priority`, y = `Total Budgeted Financing ($'000)`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Projects", x = "Project", y = "Budget ($'000)")


