library(tidyverse)
library(stringr)
library(dplyr) #handy data manipulation
library(ggplot2) #our today's super star
library(stringr) #to manipulate string date
library(ggthemes) #many nice themes
library(gghighlight) #will abuse it a bit to show nice label


customer = read.csv("C:\\Users\\LNH\\Downloads\\Datathon_VIN\\datathon-2026-round-1\\customers.csv")
orders = read.csv("C:\\Users\\LNH\\Downloads\\Datathon_VIN\\datathon-2026-round-1\\orders.csv")

colSums(is.na(orders))
colSums(is.na(customer))

orders$order_date = parse_date_time(trimws(orders$order_date),'ymd')

customer_order = right_join(customer,orders, by = 'customer_id')
colSums(is.na(customer_order))

customer_order = customer_order %>%
  arrange(order_date) %>%
  mutate(year = year(order_date),
         month = month(order_date),
         quarter = quarter(order_date),
         weekday = weekdays(order_date),
         OrderMonth = parse_date_time(str_sub(order_date,end = 7),'ym')
  )

count_bar = function(df,col,title) {
  print(
    ggplot(df,aes(x=.data[[col]])) +
      geom_bar() +
      geom_text(stat='count',aes(label = ..count..),vjust = -0.33) +
      labs(title = paste('So luong khach hang theo',title))
  )
}
count_bar(customer_order,col='gender',title='gioi tinh')
count_bar(customer_order,col='city',title='thanh pho')
count_bar(customer_order,col='quarter',title='quy')
count_bar(customer_order,col='month',title='thanng')
count_bar(customer_order,col='weekday',title='thu trong tuan')
count_bar(customer_order,col='order_source',title='kenh phan phoi quang cao')
count_bar(customer_order,col='order_status',title='tinh trang don hang')
count_bar(customer_order,col='age_group',title='do tuoi')
count_bar(customer_order,col='device_type',title='thiet bi dat hang')

#Ty le khach hang mua tren 1 don
customer_order %>%
  group_by(customer_id) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  filter(count>1) %>%
  summarise(n = n()/121930)

#Cohort Analyst
customer_order = customer_order %>%
  filter(year(order_date) %in% c(2022,2021,2023))
#Thang nam dau tien mua hang cua khach hang
cohort_group_df = customer_order %>% 
  group_by(customer_id) %>% 
  summarize(CohortGroup = min(OrderMonth))
#So khach hang co tai khoan ma khong mua hang
- length(cohort_group_df$customer_id) + length(customer$customer_id)
#Join voi data goc
df = inner_join(cohort_group_df,customer_order,by='customer_id')
#Danh sach KH tung mua hang
base_cohort_df = df %>% 
  group_by(CohortGroup) %>%
  summarise(TotalUsers = n_distinct(customer_id))
ggplot(base_cohort_df,aes(x = CohortGroup,y=TotalUsers)) +
  geom_line()
#=>Kha nang tiep can khach hang moi ngay cang thap
#Tao activity df
activity_cohort_df = df %>% 
  group_by(CohortGroup, OrderMonth) %>%
  summarise(BuyingUsers = n_distinct(customer_id))
user_cohort_df = inner_join(activity_cohort_df, base_cohort_df, by = 'CohortGroup')
#Thang de quay lai
user_cohort_df = user_cohort_df %>% 
  group_by(CohortGroup) %>% 
  mutate(MonthNumber = 1:n())
# subsetting the data
plot_user_cohort_df = inner_join(base_cohort_df[,c('CohortGroup')], 
                                 user_cohort_df, by = 'CohortGroup') %>%
  mutate(dummy_col = 1)
# create base dataframe for heat map visualization
cohort_heatmap_df = user_cohort_df %>% 
  select(CohortGroup,MonthNumber, BuyingUsers) %>%
  spread(MonthNumber, BuyingUsers)
# the percentage version of the dataframe
cohort_heatmap_df_pct = data.frame(
  cohort_heatmap_df$CohortGroup,
  cohort_heatmap_df[,2:ncol(cohort_heatmap_df)] / cohort_heatmap_df[["1"]]
)
# assign the same column names
colnames(cohort_heatmap_df_pct) = colnames(cohort_heatmap_df)
# melt the dataframes for plotting
plot_data_abs = gather(cohort_heatmap_df, "MonthNumber", "BuyingUsers", 2:ncol(cohort_heatmap_df))
plot_data_pct = gather(cohort_heatmap_df_pct, "MonthNumber", "Retention", 2:ncol(cohort_heatmap_df_pct))
# prepare label names containing absolute number of buyers for the first month and retention percentages for the rest months
label_names = c(plot_data_abs$BuyingUsers[1:(ncol(cohort_heatmap_df)-1)],plot_data_pct$Retention[(ncol(cohort_heatmap_df_pct)):(nrow(plot_data_pct))])
# beautify percentage labels
beauty_print <- function(n) {
  case_when( n <= 1  ~ sprintf("%1.0f %%", n*100),
             n >  1  ~ as.character(n),
             TRUE    ~ " ") # for NA values, skip the label
}
# create dataframe ready for plotting
plot_data = data.frame(
  CohortGroup = plot_data_pct$CohortGroup,
  MonthNumber = plot_data_pct$MonthNumber,
  Retention = plot_data_pct$Retention,
  Label = beauty_print(label_names)
)
plot_data$MonthNumber = as.numeric(plot_data$MonthNumber)

# plotting heatmap
ggplot(plot_data) +
  geom_raster(aes(x = MonthNumber,
                  y = reorder(CohortGroup, desc(CohortGroup)),
                  fill = Retention)) +
  scale_fill_continuous(guide = FALSE, type = "gradient",
                        low = "deepskyblue", high = "darkblue") +
  scale_x_continuous(breaks = seq(from = 1, to = 25, by = 1),
                     expand = c(0,0)) +
  geom_text(aes(x = MonthNumber,
                y = reorder(CohortGroup, desc(CohortGroup)),
                label = Label), col = "white", size = 2) +
  theme_minimal() +
  labs(
    title = "Monthly User Purchasing Cohort",
    x = "K-th Month",
    y = "Cohort Group"
  )

