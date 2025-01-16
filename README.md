# INF4000-Project
This repository contains all the resources for the INF4000 course project. The focus is on advanced data exploration and visualisation. Data Exploration: Cleaning and examining datasets to uncover patterns and insights. Visualization &amp; Reporting: Using R (e.g., ggplot2, plotly) to create interactive and static plots and document findings.

## Table of Contents
1. [Overview](#overview)
2. [System Requirements](#system-requirements)
3. [Installation and Setup](#installation-and-setup)
4. [Running the Scripts](#running-the-scripts)
5. [Project Structure](#project-structure)
6. [License](#license)

## Overview
- **Course**: INF4000
- **Goal**: Analysing retail sales performance in Great Britain.
- **Aims and Objectives**: Understanding the pandemic's impact on retail is essential as it provides deeper insights into the shifts in consumer spending patterns and pinpoints the need for repositioning strategies. The composite visualisation discussed here employs the retail sales pounds dataset of Great Britain (1986 to 2024) from the Office for National Statistics (ONS). It presents four different charts that aim to uncover relevant insights, with each chart addressing a specific sub-question revolving around the retail landscape.
- **Methods**:
  - **Data Visualisation**
  - **ASSERT Framework**
  - **Grammar of graphics (GoG)**
 
  ---

## System Requirements
- **Operating System**: Windows, macOS, or Linux
- **R Version**: 4.1.2 or later
- **R Packages**:
  - `dplyr`
  - `ggplot2`
  - `readr`
  - `tidyr`
  - Others (as needed, specified in your scripts)

---

## Installation and Setup
1. **Clone the Repository**:
   - Click the green "**Code**" button in this GitHub repository.
   - Copy the HTTPS or SSH link provided (e.g., `https://github.com/rabraham2/INF4000-Project.git`).
   - Open your terminal or command prompt and run:
     ```bash
     git clone https://github.com/rabraham2/INF4000-Project.git
     ```

2. **Install Required R Packages**:
   Open **R** or **RStudio** and install the necessary packages:
 ```r
   install.packages("readxl")
   install.packages("dplyr")
   install.packages("tidyr")
   install.packages("tidyverse")
   install.packages("scales")
   install.packages("forcats")
   install.packages("ggplot2")
   install.packages("gridExtra")

```

## Running the Scripts

```r

# Start of Data Visualization Coursework

install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("scales")
install.packages("forcats")
install.packages("ggplot2")
install.packages("gridExtra")

library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(scales)
library(forcats)
library(ggplot2)
library(gridExtra)


# Reading the Retail Pounds Dataset used for Visualization

retail_pounds_data <- read_excel("Retail_Sales_Index_Pounds_Data.xlsx")
view(retail_pounds_data)
str(retail_pounds_data)

# Plot1 - Visualization of Total Annual Retail Sales Data using Line Graph for the year 2016 to 2023

# Filter Retail Data from the Year 2016 to 2023 in the Dataset

retail_pounds_data_filtered <- retail_pounds_data %>% filter(valnsat_time_period_year >= 2016 & valnsat_time_period_year <= 2023) %>% drop_na(valnsat_total_annual_sales_for_all_retailing_including_automotive_fuel) %>% select(valnsat_time_period_year,valnsat_total_annual_sales_for_all_retailing_including_automotive_fuel)

# Define Total Annual Retail Sales value Ranges

breaks <- c(375000000, 400000000, 425000000, 450000000, 475000000, 500000000, 5250000000)

# Mark the Corresponding Labels to Create Breaks in Line Graph

labels <- c("375M-400M", "400M-425M", "425M-450M", "450M-475M", "475M-500M", "500M-525M", "525M-550M")

# Plot the Retail Sales Data on the Line Graph

ggplot(retail_pounds_data_filtered, aes(x = factor(valnsat_time_period_year), 
                        y = valnsat_total_annual_sales_for_all_retailing_including_automotive_fuel,
                        colour = valnsat_total_annual_sales_for_all_retailing_including_automotive_fuel,
                        group = 1)) + 
  geom_line(size = 4) + 
  geom_point(color = "grey", fill = "#f2f2f2", size = 5, shape = 16) +
  labs(x = "Time Period (in Years)", 
       y = "Total Retail Sales (in £thousands)",
       title = "Annual Retail Sales (Including Automotive Fuel) from 2016 to 2023 in Great Britain",
       colour = "Retail Sales (in £thousands)") +
  scale_colour_gradientn(colors = c("lightblue", "skyblue", "dodgerblue", "blue", "royalblue", "navy", "darkblue"), 
                         breaks = breaks,
                         labels = labels) + 
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  theme_minimal() +
  theme(legend.position = "right")

# Plot 2 -  Visualization of Sales by Business Type across Categories using Faceted Pie Chart 2020 vs 2019

# Data Filtering and Calculation of Percentage Contribution of Large and Small Businesses across each Sub-Categories

retail_pound_data_for_large_and_small_business <- retail_pounds_data %>% filter(valnsat_time_period_year >= 2019 & valnsat_time_period_year <= 2020) %>%
  select(
    valnsat_time_period_year,
    valnsat_predominantly_food_stores_category_1,
    valnsatd_predominantly_food_stores_total_large_businesses_category_1_a,
    valnsatd_predominantly_food_stores_total_small_businesses_category_1_b,
    valnsat_total_predominantly_non_food_stores_category_2,
    valnsatd_predominantly_non_food_stores_total_large_businesses_category_2_1,
    valnsatd_predominantly_non_food_stores_total_small_businesses_category_2_2,
    valnsat_non_store_retailing_category_3,
    valnsatd_non_store_retailing_large_businesses_category_3_1,
    valnsatd_non_store_retailing_small_businesses_category_3_1
  )

# Finding the total annual data for each year for large and small businesses in Food stores

total_annual_sales_food_stores <- retail_pound_data_for_large_and_small_business %>%
  group_by(valnsat_time_period_year) %>%
  summarize(
    total_annual_sales_large_businesses_food_stores = sum(valnsatd_predominantly_food_stores_total_large_businesses_category_1_a, na.rm = TRUE),
    total_annual_sales_small_businesses_food_stores = sum(valnsatd_predominantly_food_stores_total_small_businesses_category_1_b, na.rm = TRUE),
    total_annual_sales_for_all_food_stores = sum(valnsat_predominantly_food_stores_category_1, na.rm = TRUE)
  ) %>%
  mutate(
    grand_total_check = total_annual_sales_large_businesses_food_stores + total_annual_sales_small_businesses_food_stores,
    total_sum_matches = grand_total_check == total_annual_sales_for_all_food_stores,
    grand_total_check_mismatch_value = ifelse(grand_total_check != total_annual_sales_for_all_food_stores, grand_total_check, NA)
  )

# Calculating percentages of large and small businesses for food stores 

total_annual_sales_food_stores <- total_annual_sales_food_stores %>%
  group_by(valnsat_time_period_year) %>%
  mutate(
    percent_large_businesses_food_stores = (total_annual_sales_large_businesses_food_stores / grand_total_check) * 100,
    percent_small_businesses_food_stores = (total_annual_sales_small_businesses_food_stores / grand_total_check) * 100
  )

# Finding the total annual data for each year for large and small businesses for non-food stores

total_annual_sales_non_food_stores <- retail_pound_data_for_large_and_small_business %>%
  group_by(valnsat_time_period_year) %>%
  summarize(
    total_annual_sales_large_businesses_non_food_stores = sum(valnsatd_predominantly_non_food_stores_total_large_businesses_category_2_1, na.rm = TRUE),
    total_annual_sales_small_businesses_non_food_stores = sum(valnsatd_predominantly_non_food_stores_total_small_businesses_category_2_2, na.rm = TRUE),
    total_annual_sales_for_all_non_food_stores = sum(valnsat_total_predominantly_non_food_stores_category_2, na.rm = TRUE)
  ) %>%
  mutate(
    grand_total_check = total_annual_sales_large_businesses_non_food_stores + total_annual_sales_small_businesses_non_food_stores,
    total_sum_matches = grand_total_check == total_annual_sales_for_all_non_food_stores,
    grand_total_check_mismatch_value = ifelse(grand_total_check != total_annual_sales_for_all_non_food_stores, grand_total_check, NA)
  )

# Calculating percentages of large and small businesses for non-food stores 

total_annual_sales_non_food_stores <- total_annual_sales_non_food_stores %>%
  group_by(valnsat_time_period_year) %>%
  mutate(
    percent_large_businesses_non_food_stores = (total_annual_sales_large_businesses_non_food_stores / grand_total_check) * 100,
    percent_small_businesses_non_food_stores = (total_annual_sales_small_businesses_non_food_stores / grand_total_check) * 100
  )

# Finding the total annual data for each year for large and small businesses for non-store retailing

total_annual_sales_non_store_retailing <- retail_pound_data_for_large_and_small_business %>%
  group_by(valnsat_time_period_year) %>%
  summarize(
    total_annual_sales_large_businesses_non_store_retailing = sum(valnsatd_non_store_retailing_large_businesses_category_3_1, na.rm = TRUE),
    total_annual_sales_small_businesses_non_store_retailing = sum(valnsatd_non_store_retailing_small_businesses_category_3_1, na.rm = TRUE),
    total_annual_sales_for_all_non_store_retailing = sum(valnsat_non_store_retailing_category_3, na.rm = TRUE)
  ) %>%
  mutate(
    grand_total_check = total_annual_sales_large_businesses_non_store_retailing + total_annual_sales_small_businesses_non_store_retailing,
    total_sum_matches = grand_total_check == total_annual_sales_for_all_non_store_retailing,
    grand_total_check_mismatch_value = ifelse(grand_total_check != total_annual_sales_for_all_non_store_retailing, grand_total_check, NA)
  )

# Calculating percentages of large and small businesses for non-store retailing

total_annual_sales_non_store_retailing <- total_annual_sales_non_store_retailing %>%
  group_by(valnsat_time_period_year) %>%
  mutate(
    percent_large_businesses_non_store_retailing = (total_annual_sales_large_businesses_non_store_retailing / grand_total_check) * 100,
    percent_small_businesses_non_store_retailing = (total_annual_sales_small_businesses_non_store_retailing / grand_total_check) * 100
  )

# Creating pie chart data for Food Stores (2020 vs 2019)

food_stores_retailing <- data.frame(
  year = c(2020, 2019),
  'Large business' = c(total_annual_sales_food_stores$total_annual_sales_large_businesses_food_stores[total_annual_sales_food_stores$valnsat_time_period_year == 2020], 
                       total_annual_sales_food_stores$total_annual_sales_large_businesses_food_stores[total_annual_sales_food_stores$valnsat_time_period_year == 2019]),
  'Small business' = c(total_annual_sales_food_stores$total_annual_sales_small_businesses_food_stores[total_annual_sales_food_stores$valnsat_time_period_year == 2020], 
                       total_annual_sales_food_stores$total_annual_sales_small_businesses_food_stores[total_annual_sales_food_stores$valnsat_time_period_year == 2019])
)

# Converting the data to standard format for ggplot

food_stores_retailing <- food_stores_retailing %>%
  gather(key = "business_type", value = "sales", -year)

# Normalizing sales to percentages for each year under consideration

food_stores_retailing <- food_stores_retailing %>%
  group_by(year) %>%
  mutate(sales_percentage = sales / sum(sales) * 100)

# Reordering to make 2020 appear first and 2019 second

food_stores_retailing$year <- factor(food_stores_retailing$year, levels = c(2020, 2019))

# Pie Chart of Food Stores

food_stores_retailing_pie_chart <- ggplot(food_stores_retailing, aes(x = "", y = sales_percentage, fill = business_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~year, labeller = label_value) +
  theme_void(base_size = 12) +
  labs(title = "Food Stores Retail Sales (2020 vs 2019)", fill = "Business Type") +
  scale_fill_manual(
    values = c("Large.business" = "darkblue", "Small.business" = "#66B3FF"),
    labels = c("Large.business", "Small.business")
  ) +
  geom_text(aes(label = paste0(round(sales_percentage, 1), "%")), position = position_stack(vjust = 0.5), color = "white", size = 2.5) +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, size = 12), legend.text = element_text(size = 9),legend.title = element_text(size = 11))

# Creating pie chart data for Non-Food Stores (2020 vs 2019)

non_food_stores_retailing <- data.frame(
  year = c(2020, 2019),
  'Large business' = c(total_annual_sales_non_food_stores$total_annual_sales_large_businesses_non_food_stores[total_annual_sales_non_food_stores$valnsat_time_period_year == 2020], 
                       total_annual_sales_non_food_stores$total_annual_sales_large_businesses_non_food_stores[total_annual_sales_non_food_stores$valnsat_time_period_year == 2019]),
  'Small business' = c(total_annual_sales_non_food_stores$total_annual_sales_small_businesses_non_food_stores[total_annual_sales_non_food_stores$valnsat_time_period_year == 2020], 
                       total_annual_sales_non_food_stores$total_annual_sales_small_businesses_non_food_stores[total_annual_sales_non_food_stores$valnsat_time_period_year == 2019])
)

# Convert the data to a standard format for ggplot

non_food_stores_retailing <- non_food_stores_retailing %>%
  gather(key = "business_type", value = "sales", -year)

# Normalizing sales to percentages for each year under consideration

non_food_stores_retailing <- non_food_stores_retailing %>%
  group_by(year) %>%
  mutate(sales_percentage = sales / sum(sales) * 100)

# Reordering to make 2020 appear first and 2019 second

non_food_stores_retailing$year <- factor(non_food_stores_retailing$year, levels = c(2020, 2019))

# Pie Chart of Non-Food Stores

non_food_stores_retailing_pie_chart <- ggplot(non_food_stores_retailing, aes(x = "", y = sales_percentage, fill = business_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~year, labeller = label_value) +
  theme_void(base_size = 12) +
  labs(title = "Non-Food Stores Retail Sales (2020 vs 2019)", fill = "Business Type") +
  scale_fill_manual(
    values = c("Large.business" = "darkblue", "Small.business" = "#66B3FF"),
    labels = c("Large.business", "Small.business")
  ) +
  geom_text(aes(label = paste0(round(sales_percentage, 1), "%")), position = position_stack(vjust = 0.5), color = "white", size = 2.5) +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, size = 12), legend.text = element_text(size = 9),legend.title = element_text(size = 11))

# Creating pie chart data for Non-Store Retailing (2020 vs 2019)

non_store_retailing <- data.frame(
  year = c(2020, 2019),
  'Large business' = c(total_annual_sales_non_store_retailing$total_annual_sales_large_businesses_non_store_retailing[total_annual_sales_non_store_retailing$valnsat_time_period_year == 2020], 
                       total_annual_sales_non_store_retailing$total_annual_sales_large_businesses_non_store_retailing[total_annual_sales_non_store_retailing$valnsat_time_period_year == 2019]),
  'Small business' = c(total_annual_sales_non_store_retailing$total_annual_sales_small_businesses_non_store_retailing[total_annual_sales_non_store_retailing$valnsat_time_period_year == 2020], 
                       total_annual_sales_non_store_retailing$total_annual_sales_small_businesses_non_store_retailing[total_annual_sales_non_store_retailing$valnsat_time_period_year == 2019])
)

# Converting the data into standard format for ggplot

non_store_retailing <- non_store_retailing %>%
  gather(key = "business_type", value = "sales", -year)

# Normalizing sales to percentages for each year under consideration

non_store_retailing <- non_store_retailing %>%
  group_by(year) %>%
  mutate(sales_percentage = sales / sum(sales) * 100)

# Reordering to make 2020 appear first and 2019 second

non_store_retailing$year <- factor(non_store_retailing$year, levels = c(2020, 2019))

# Pie Chart of Non-Store Retailing

non_store_retailing_pie_chart <- ggplot(non_store_retailing, aes(x = "", y = sales_percentage, fill = business_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~year, labeller = label_value) +
  theme_void(base_size = 12) +
  labs(title = "Non-Store Retailing Sales (2020 vs 2019)", fill = "Business Type") +
  scale_fill_manual(
    values = c("Large.business" = "darkblue", "Small.business" = "#66B3FF"),
    labels = c("Large.business", "Small.business")
  ) +
  geom_text(aes(label = paste0(round(sales_percentage, 1), "%")), position = position_stack(vjust = 0.5), color = "white", size = 2.5) +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, size = 12), legend.text = element_text(size = 9),legend.title = element_text(size = 11))

# Combining all pie charts into one single visualization in ggplot

gridExtra::grid.arrange(
  food_stores_retailing_pie_chart,
  non_food_stores_retailing_pie_chart,
  non_store_retailing_pie_chart,
  ncol = 1
)

# Plot 3 - Visualization of Sales Change in Percentage across Retail Business Categories using Horizontal Bar Chart 2020 vs 2019

retail_pounds_data_sales_change_filtered <- retail_pounds_data %>% filter(valnsat_time_period_year >= 2019 & valnsat_time_period_year <= 2020) %>% select(
  valnsat_time_period_year,
  valnsat_predominantly_food_stores_category_1, 
  valnsat_total_predominantly_non_food_stores_category_2,
  valnsat_non_specialised_stores_sub_category_2_a,
  valnsat_textile_clothing_and_footwear_stores_sub_category_2_b,
  valnsat_household_goods_stores_sub_category_2_c,
  valnsatd_furniture_lighting_etc_nes_sub_category_2_c_part1,
  valnsatd_electrical_household_appliances_sub_category_2_c_part2,
  valnsatd_hardware_paint_and_glass_sub_category_2_c_part3,
  valnsatd_audio_and_video_and_music_sub_category_2_c_part4,
  valnsat_other_stores_sub_category_2_d,
  valnsat_non_store_retailing_category_3,
  valnsat_automotive_fuel
)

retail_pounds_data_change_percentage <- retail_pounds_data_sales_change_filtered %>%
  gather(key = "category_change", value = "category_based_sales_value", 
         valnsat_predominantly_food_stores_category_1,
         valnsat_total_predominantly_non_food_stores_category_2,
         valnsat_non_specialised_stores_sub_category_2_a,
         valnsat_textile_clothing_and_footwear_stores_sub_category_2_b,
         valnsat_household_goods_stores_sub_category_2_c,
         valnsatd_furniture_lighting_etc_nes_sub_category_2_c_part1,
         valnsatd_electrical_household_appliances_sub_category_2_c_part2,
         valnsatd_hardware_paint_and_glass_sub_category_2_c_part3,
         valnsatd_audio_and_video_and_music_sub_category_2_c_part4,
         valnsat_other_stores_sub_category_2_d,
         valnsat_non_store_retailing_category_3,
         valnsat_automotive_fuel) %>%
  group_by(valnsat_time_period_year, category_change) %>%
  summarise(total_category_sales = sum(category_based_sales_value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = valnsat_time_period_year, values_from = total_category_sales) %>%
  mutate(
    percentage_change = ((`2020` - `2019`) / `2019`) * 100
  )

# Reordering the category_change variable to display positive values first

retail_pounds_data_change_percentage <- retail_pounds_data_change_percentage %>%
  mutate(
    category_change = fct_reorder(category_change, percentage_change)
  )

# Plotting the Horizontal Bar Chart

ggplot(retail_pounds_data_change_percentage, aes(x = percentage_change, y = category_change, fill = percentage_change > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Sales Change across Retail Categories (2020 Vs 2019)",
    x = "Sales Change (in %)",
    y = "Retail Business Categories"
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  scale_y_discrete(labels = c(
    "valnsat_predominantly_food_stores_category_1" = "Food Stores",
    "valnsat_total_predominantly_non_food_stores_category_2" = "Non-Food Stores",
    "valnsat_non_specialised_stores_sub_category_2_a" = "Non-Specialised Stores",
    "valnsat_textile_clothing_and_footwear_stores_sub_category_2_b" = "Clothing and Footwear Stores",
    "valnsat_household_goods_stores_sub_category_2_c" = "Household Goods Stores",
    "valnsatd_furniture_lighting_etc_nes_sub_category_2_c_part1" = "Furniture, Lighting and NES Stores",
    "valnsatd_electrical_household_appliances_sub_category_2_c_part2" = "Household Appliances Stores",
    "valnsatd_hardware_paint_and_glass_sub_category_2_c_part3" = "Hardware, Paint and Glass Stores",
    "valnsatd_audio_and_video_and_music_sub_category_2_c_part4" = "Audio, Video and Music Stores",
    "valnsat_other_stores_sub_category_2_d" = "Other Stores",
    "valnsat_non_store_retailing_category_3" = "Non-Store Retailing",
    "valnsat_automotive_fuel" = "Automotive Fuel"
  )) +
  scale_fill_manual(
    values = c(`TRUE` = "darkblue", `FALSE` = "lightblue")
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 14)
  )

# Plot 4 -  Visualization of Monthly Sales using Clustered Bar Chart 2020 vs 2019

# Data Filtering based on Month

retail_pound_data_month_on_month <- retail_pounds_data %>% filter(valnsat_time_period_year >= 2019 & valnsat_time_period_year <= 2020) %>%
  select(
    valnsat_time_period_year,
    valnsat_time_period_month,
    valnsat_no_of_weeks_per_month,
    valnsat_all_retailing_including_automotive_fuel,
    valnsat_value_of_avg_monthly_weekly_retail_sales_including_automobile_fuel,
    valnsat_month_as_a_percentage_of_total
  )

# Converting the 'valnsat_time_period_month' to a numerals months in order

valnsat_month_in_numerals <- c(
  "JAN" = "01",
  "FEB" = "02",
  "MAR" = "03",
  "APR" = "04",
  "MAY" = "05",
  "JUN" = "06",
  "JUL" = "07",
  "AUG" = "08",
  "SEP" = "09",
  "OCT" = "10",
  "NOV" = "11",
  "DEC" = "12"
)

# Converting the month abbreviations in 'valnsat_time_period_month' to corresponding month numbers

retail_pound_data_month_on_month$valnsat_time_period_month_num <- valnsat_month_in_numerals[retail_pound_data_month_on_month$valnsat_time_period_month]

# Plotting the clustered bar chart with monthly sales values of 2020 and 2019

ggplot(retail_pound_data_month_on_month, aes(x = valnsat_time_period_month_num, 
                                             y = valnsat_all_retailing_including_automotive_fuel, 
                                             fill = factor(valnsat_time_period_year, levels = c(2020, 2019)))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("2020" = "darkblue", "2019" = "lightblue")) +
  geom_line(aes(
    group = factor(valnsat_time_period_year, levels = c(2020, 2019)), 
    color = factor(valnsat_time_period_year, levels = c(2020, 2019))
    ), 
            size = 1, 
            linetype = "solid") +
  labs(
    title = "Month-on-Month Sales for 2020 vs 2019",
    x = "Month on Month (in Months)",
    y = "Monthly Sales including Automotive Fuel (in £thousands)",
    fill = "Time Periods",
    color = "Monthly Sales Trend"
  ) +
  scale_x_discrete(
    breaks = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
    labels = month.abb
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5))

```

## Project Structure

INF4000-Project/
├─ Code/
│  ├─ data_r_code.R
├─ Data/
│  ├─ Retail_Sales_Index_Pounds_Data.xlsx
├─ Results/
│  ├─ Annual_Retail_Sales_from_2016_to_2023.png
│  ├─ Month_on_Month_Sales_for_2020_vs_2019.png
│  ├─ Sales_Change_across_Retail_Categories_2020_Vs_2019.png
│  ├─ Visualization_of_Sales_by_Business_Type_across_Categories_2020_vs_2019.png
├─ LICENSE
├─ README.md

## License

MIT License

Copyright (c) 2025 Roshin Abraham

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
