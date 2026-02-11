library(readxl)
library(dplyr)
library(tidyr)

lawsuits<-read_excel("Lawsuit.xlsx",sheet = "Event Study")

#Renamed the columns for simplification.
colnames(lawsuits)<-c("Company", "Terminated", "EventWindow", "StockPriceDate", "StockPrice", "ChangePercent")

#We ensure consistency in the "EventWindow" column, which labels each row as before or after the event (lawsuit termination).
lawsuits$EventWindow<-tolower(lawsuits$EventWindow)  # Convert to lowercase to match "before"/"after"
lawsuits$EventWindow<-factor(lawsuits$EventWindow,levels=c("before", "after"))

#Right now data is in long format — 1 row per stock price measurement.We need it in wide format — 1 row per lawsuit termination, with two columns: before and after prices.
#Then, we calculate the raw change in price (after - before) in dollars and percentages.
lawsuits_wide <- lawsuits %>%
  select(Company, Terminated, EventWindow, StockPrice) %>%
  pivot_wider(names_from = EventWindow,values_from = StockPrice)
#in dollars
lawsuits_wide<-lawsuits_wide %>%
  mutate(Change = after - before)
#in percentages
lawsuits_wide<-lawsuits_wide %>%
  mutate(PercentChange=((after - before)/before)*100)


#This tests if there is a statistically significant difference between before and after stock prices.In dollars:
t.test(lawsuits_wide$after,lawsuits_wide$before,paired = TRUE)
#p-value=0.0373<0.05, there is a significant change in stock price after lawsuits
#In percentages:
t.test(lawsuits_wide$PercentChange)
#p-value=0.02795<0.05, there is a significant change in stock price after lawsuits

#Boxplot of before and after not in percentages
library(ggplot2)
lawsuits_long<-lawsuits_wide %>%
  select(Company,Terminated,before,after) %>%
  pivot_longer(cols = c(before, after),names_to = "EventWindow", values_to = "StockPrice")
ggplot(lawsuits_long, aes(x = EventWindow, y = StockPrice, fill = EventWindow)) +
  geom_boxplot() +
  labs(title = "Stock Prices Before vs After Lawsuit Termination",x = "", y = "Stock Price") +
  theme_minimal()
#Bar chart of change by company not in percentages
ggplot(lawsuits_wide, aes(x = reorder(Company, Change), y = Change, fill = Change > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Change in Stock Price After Lawsuit Termination",x = "Company", y = "Change in Stock Price") +
  theme_minimal()


#That bar chart shows the percent change in stock prices for each company after lawsuit resolution. 
ggplot(lawsuits_wide,aes(x=reorder(Company,PercentChange),y=PercentChange,fill=PercentChange>0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Percent Change in Stock Price After Lawsuit Termination",x = "Company", y = "Percent Change (%)") +
  theme_minimal()

#Histogram that visualizes the distribution of percent changes in stock price after lawsuit terminations. 
ggplot(lawsuits_wide,aes(x = PercentChange)) +
  geom_histogram(binwidth = 2,fill = "darkgreen",alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Histogram of Percent Changes in Stock Price",
       x = "Percent Change", y = "Count") +
  theme_minimal()
#X-axis: Percent change in stock price (after vs before), Y-axis: Number of observations (lawsuit events) in each percent-change range
#The dashed vertical line at 0 represents no change.Bars to the right of this line = positive stock impact. Bars to the left = negative stock impact

