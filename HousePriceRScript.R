## ----setup, include=FALSE--------------------

# This sets defaults for all code chunks in the Rmd
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

#Load required R packages

# The tidyverse is a collection of R packages for data science 
# (includes dplyr, ggplot2, readr, tidyr, tibble, stringr, forcats).
# We’ll use this for data manipulation and visualization.
library(tidyverse)

# Reads Excel files (.xls, .xlsx) into R.
# Required for loading your CPI data (cpi.xlsx).
library(readxl)

# Creates plots and visualizations.
library(ggplot2)

# Handles knitting (rendering) the Rmd into HTML, PDF, or Word.
# Also provides functions like kable() good looking tables.
library(knitr)

# Contains statistical functions and datasets.
library(MASS)

# Used for fitting Generalized Additive Models (GAMs).
library(mgcv)

# A high-performance data manipulation package.
# Good for large datasets or fast filtering/aggregation.
library(data.table)

# Simplifies working with dates and times.
library(lubridate)

# Custom color palette for plots
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



## --------------------------------------------
# Install (only if needed)
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readr")) install.packages("readr")
if (!require("readxl")) install.packages("readxl")
if (!require("lubridate")) install.packages("lubridate")
if (!require("ggrepel")) install.packages("ggrepel")

library(tidyverse); library(readr); library(readxl); library(lubridate); library(ggrepel)

# 1) Freddie Mac HPI (FMHPI)
avg_house_price <- read_csv("fmhpi_master_file.csv")

# Extract state code from GEO_Name and build a proper Date column,
# and drop rows where State could not be identified.
avg_house_price <- avg_house_price %>%
  mutate(
    # Pull the LAST two uppercase letters from GEO_Name
    State = str_extract(GEO_Name, "[A-Z]{2}$"),
    
    # Construct a Date from Year and Month (day set to 1).
    Date  = make_date(Year, Month, 1),
    
    # Make sure the index columns are numeric
    Index_NSA = as.numeric(Index_NSA),
    Index_SA  = as.numeric(Index_SA)
  ) %>%
  
  # Remove any rows where State was not matched
  filter(!is.na(State))

# Aggregate to State–Date 
avg_house_price <- avg_house_price %>%
  group_by(State, Date, Year, Month) %>%
  summarise(
    Index_NSA = mean(Index_NSA, na.rm = TRUE), # Average the non-seasonally-adjusted HPI
    AvgPrice  = mean(Index_SA,  na.rm = TRUE), # Average the seasonally-adjusted HPI

    .groups = "drop" # Ungroup the result
  )

## --------------------------------------------

# 2) CPI: tidy to Year/Month and keep only Date + cpi
cpi_raw <- read_excel("cpi.xlsx", skip = 4)

# Rename first 14 cols if present
colnames(cpi_raw)[1:14] <- c("Drop","Year","Jan","Feb","Mar","Apr","May","Jun",
                             "Jul","Aug","Sep","Oct","Nov","Dec")

# Remove the "Drop" column, keep only rows with a valid Year
cpi_clean <- cpi_raw[, -1] %>%
  filter(!is.na(Year)) %>%
  mutate(Year = as.integer(Year)) %>%
  mutate(across(Jan:Dec, ~ as.numeric(.)))

# Convert from wide (one row per Year with 12 month columns) to long (one row per Year-Month):
#  - pivot the month columns into two columns: MonthName and Value (the CPI)
#  - translate MonthName to numeric Month using base R's month abbreviations
#  - build a proper Date (first of each month) for clean time‑series joins later
#  - keep only the study window (1975–2025)
#  - and keep just the two columns we’ll need downstream: Date and cpi
cpi_long <- cpi_clean %>%
  pivot_longer(cols = Jan:Dec, names_to = "MonthName", values_to = "Value") %>%
  mutate(
    Month = match(MonthName, month.abb),
    Date  = make_date(Year, Month, 1)
  ) %>%
  filter(Year >= 1975, Year <= 2025) %>%
  dplyr::select(Date, cpi = Value)

#Check the results
head(cpi_long)

## ----fig.height=10, fig.width=14-------------
# 3) Join CPI to prices and compute CPI-adjusted price

# Join the monthly house price index table (avg_house_price) to the monthly CPI table (cpi_long)on the common Date column. 
# inner_join() keeps only the rows where Date appears in BOTH datasets Ensures we only keep months that have both house price data and CPI data.
avg_house_price_cpi <- inner_join(avg_house_price, cpi_long, by = "Date") %>%
  
  # Compute an inflation-adjusted price index
  mutate(AvgPrice_Adj = (AvgPrice / cpi) * 100)

# 4) Region map (CSV with columns: State (full), Code (abbr), Region)

# we already have state_abbrevs.txt
st_abbrs <- read.table("state_abbrevs.txt", header = TRUE)

# Attach StateName and Region using the state code
avg_house_price_cpi <- avg_house_price_cpi %>%
  left_join(st_abbrs, by = c("State" = "Code")) %>%
  
  # Left join keeps ALL rows from avg_house_price_cpi
  # and adds matching columns from st_abbrs where State == Code
  rename(StateName = State.y) 


## ----fig.height=10, fig.width=14-------------
# Collapse monthly data to yearly by taking the mean across months AND states.
# That effectively gives each state equal weight
avg_price_us_adj <- avg_house_price_cpi %>%
  mutate(Year = year(Date)) %>% # extract calendar year
  group_by(Year) %>%
  summarise(AvgPrice_Adj = mean(AvgPrice_Adj, na.rm = TRUE),  .groups = "drop")  # average over all states & months

ggplot(avg_price_us_adj, aes(x = Year, y = AvgPrice_Adj)) +
  geom_line() + geom_point() +
  ggtitle("CPI-Adjusted Average Housing Price in the U.S. Per Year") +
  ylab("Adjusted Average Housing Price ($, CPI Indexed to 100)") +
  theme_grey(base_size = 22)


## ----fig.height=10, fig.width=10-------------
# 2. By state (paged per region to avoid tiny facets)
# Collapse to yearly per state so each facet shows one state’s yearly path
avg_state_adj <- avg_house_price_cpi %>%
  mutate(Year = year(Date)) %>%
  group_by(Year, State, Region) %>%
  summarise(AvgPrice_Adj = mean(AvgPrice_Adj, na.rm = TRUE), .groups = "drop") # annual average per state

# Collect unique region names
regions <- sort(unique(avg_state_adj$Region))

# Iterate over regions so each print() produces a separate plot page
for (r in regions) {
  df <- dplyr::filter(avg_state_adj, Region == r)
  
  # Facet by state so each small multiple is readable
  print(
    ggplot(df, aes(Year, AvgPrice_Adj, color = State, group = State)) +
      geom_line() + geom_point(size = 0.6) +
      facet_wrap(~ State, ncol = 5) +
      ggtitle(paste("CPI-Adjusted Average Housing Price —", r)) +
      xlab("Year") + ylab("Adjusted Average Housing Price (CPI base = 100)") +
      theme_grey(base_size = 12) +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 6))
  )
}


## ----fig.height=10, fig.width=14-------------
# Top 12 most variable states (CPI-adjusted)
topN <- avg_state_adj %>%
  group_by(State) %>%
  summarise(vol = sd(AvgPrice_Adj, na.rm = TRUE), .groups = "drop") %>%
  slice_max(vol, n = 12)

df_top <- dplyr::semi_join(avg_state_adj, topN, by = "State")

ggplot(df_top, aes(Year, AvgPrice_Adj, color = Region)) +
  geom_line() + geom_point(size = 0.8) +
  facet_wrap(~ State, ncol = 4) +
  ggtitle("CPI-Adjusted Prices — Top 12 Most Variable States") +
  xlab("Year") + ylab("Adjusted Average Housing Price (CPI base = 100)") +
  scale_color_manual(values = cb_palette) +
  theme_grey(base_size = 12) +
  theme(axis.text.x = element_text(size = 6))


## ----fig.height=10, fig.width=14-------------
avg_region_adj <- avg_house_price_cpi %>%
  # Extract calendar year from monthly date
  mutate(Year = year(Date)) %>%
  group_by(Year, Region) %>% # Group by year and region
  summarise(AvgPrice_Adj = mean(AvgPrice_Adj, na.rm = TRUE), .groups = "drop")  # Mean CPI-adjusted price across states in that region

ggplot(avg_region_adj, aes(x = Year, y = AvgPrice_Adj, color = Region)) +
  geom_line() + geom_point() +
  facet_wrap(~ Region, ncol = 2, labeller = "label_both") +
  xlab("Year") + ylab("Adjusted Average Housing Price (CPI base = 100)") +
  ggtitle("CPI-Adjusted Average Housing Price by Region Per Year") +
  scale_color_manual(values = cb_palette) +
  theme_grey(base_size = 22)


## --------------------------------------------
# State areas 
state.area <- data.frame(State = state.name, Area = as.numeric(state.x77[, "Area"])) %>%
  bind_rows(tibble(State = "District of Columbia", Area = 61.05))



## ----fig.height=10, fig.width=14-------------
# Historical population by (State, Year, Population): provide/ensure 3 columns in CSV
population <- readr::read_csv(
  "historical_state_population_by_year.csv",
  col_names = c("State", "Year", "Population"),
  col_types  = "cin"   # c=character, i=integer, n=numeric
)

# Aggregate in case of duplicates
population <- population %>%
  group_by(State, Year) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

# Map to full state names using codes, then attach Area
# st_abbrs has Code (abbr), State.y (full), Region
population <- population %>%
  left_join(st_abbrs, by = c("State" = "Code")) %>%
  rename(StateName = State.y)

census <- population %>%
  left_join(state.area, by = c("StateName" = "State")) %>%
  mutate(
    Population = as.numeric(Population),
    Area       = as.numeric(Area)
  )


## ----fig.height=10, fig.width=14-------------
# U.S. density per year (exclude 2025)
census_us_year <- census %>%
  filter(Year != 2025) %>%
  group_by(Year) %>%
  summarise(
    Population = sum(Population, na.rm = TRUE),
    Area       = sum(Area, na.rm = TRUE),
    density    = Population / Area,
    .groups = "drop"
  )

# U.S. avg price per year (CPI-adjusted if available)
price_us_year <- avg_house_price_cpi %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(AvgPrice = mean(AvgPrice_Adj, na.rm = TRUE), .groups = "drop") %>%
  filter(Year != 2025)

# % change vs 1975
base_1975 <- price_us_year %>% filter(Year == 1975) %>% pull(AvgPrice)
us_change <- price_us_year %>%
  mutate(change = ((AvgPrice - base_1975) / base_1975) * 100) %>%
  inner_join(census_us_year, by = "Year")

ggplot(us_change, aes(x = log(density), y = change)) +
  geom_smooth(method = "rlm", se = FALSE, aes(colour = "rlm")) +
  geom_smooth(method = "lm",  se = FALSE, aes(colour = "lm")) +
  ggrepel::geom_text_repel(aes(label = Year), size = 5, max.overlaps = Inf) +
  xlab("Density (Population per unit Area, log scale)") +
  ylab("Percent change in the house price (%)") +
  ggtitle("U.S.: Density vs. Percent Change in Average House Price (1975 baseline)") +
  scale_colour_manual(name = "legend", values = c("blue", "orange")) +
  theme_grey(base_size = 18)


## ----fig.height=10, fig.width=14-------------
latest_year <- max(avg_house_price$Year[avg_house_price$Year != 2025])

# Density in latest year per state
census_state_latest <- census %>%
  filter(Year == latest_year) %>%
  mutate(density = Population / Area) %>%
  dplyr::select(State = State, StateName, density)

# Average price in 1975 per state
price_1975 <- avg_house_price_cpi %>%
  mutate(Year = year(Date)) %>%
  filter(Year == 1975) %>%
  group_by(State) %>%
  summarise(AvgPrice1975 = mean(AvgPrice_Adj, na.rm = TRUE), .groups = "drop")

# Average price in latest year per state
price_latest <- avg_house_price_cpi %>%
  mutate(Year = year(Date)) %>%
  filter(Year == latest_year) %>%
  group_by(State) %>%
  summarise(AvgPriceLatest = mean(AvgPrice_Adj, na.rm = TRUE), .groups = "drop")

# Combine and compute % change
price_change_state <- price_1975 %>%
  inner_join(price_latest, by = "State") %>%
  mutate(change = ((AvgPriceLatest - AvgPrice1975) / AvgPrice1975) * 100) %>%
  inner_join(census_state_latest, by = "State")


## ----echo=FALSE------------------------------
correlation <- cor(
  as.numeric(price_change_state$change),
  as.numeric(price_change_state$density),
  use = "complete.obs",
  method = "pearson"
)

cat("**Result**:The correlation between density and cumulative price change is", round(correlation, 2), ".")


## ----fig.height=10, fig.width=14, dev='png'----
ggplot(price_change_state, aes(x = log(density), y = change)) +
  geom_smooth(method = "rlm", se = FALSE, aes(colour = "rlm")) +
  geom_smooth(method = "lm",  se = FALSE, aes(colour = "lm")) +
  ggrepel::geom_text_repel(aes(label = State), size = 5, max.overlaps = Inf) +
  xlab("Density (Population per unit Area, log scale)") +
  ylab("Percent change in the house price (%)") +
  ggtitle(paste0("By State: Density vs Percent Change in Average House Price (1975 to ", latest_year, ")")) +
  scale_colour_manual(name = "legend", values = c("blue", "orange")) +
  theme_grey(base_size = 18)


## ----fig.height=10, fig.width=14, dev='png'----
ggplot(filter(price_change_state, State != "DC"),
       aes(x = log(density), y = change)) +
  geom_smooth(method = "rlm", se = FALSE, aes(colour="rlm")) +
  geom_smooth(method = "lm",  se = FALSE, aes(colour="lm")) +
  ggrepel::geom_text_repel(aes(label = State), size = 5, max.overlaps = Inf) +
  xlab("Density (Population per unit Area)") +
  ylab("Percent change in the house price (%)") +
  ggtitle("Percent Density vs Percent Change in the House Price") +
  scale_colour_manual(name="legend", values=c("blue", "orange")) +
  theme_grey(base_size = 22)


## --------------------------------------------
# Allowed study years, excluding 2025
yrs_ok <- c(1990, 1999, 2000, 2009, 2010, 2018)

# 1) Standardize census to STATE CODES before we go wide ------------------
# st_abbrs must have columns: State (full name), Code (abbr), Region
# We convert StateName (full) -> Code, and keep Code as the join key.

census_state_year <- census %>%
  mutate(density = Population / Area) %>%
  # map full name -> code
  left_join(st_abbrs, by = c("State" = "Code", "StateName" = "State")) %>%
  dplyr::select(State, Year, Population, density)

# Make a WIDE table by state with Population_YYYY and density_YYYY
census.data.wide <- census_state_year %>%
  filter(Year %in% yrs_ok) %>%
  tidyr::pivot_wider(
    names_from  = Year,
    values_from = c(Population, density),
    names_sep   = "_"
  )

# Population deltas
census.data.change <- tibble(
  State       = census.data.wide$State,
  `1990_2000` = census.data.wide$Population_2000 - census.data.wide$Population_1990,
  `2000_2010` = census.data.wide$Population_2010 - census.data.wide$Population_2000,
  `2010_2018` = census.data.wide$Population_2018 - census.data.wide$Population_2010
) %>%
  pivot_longer(!State, names_to = "Year", values_to = "Population_Change") %>%
  mutate(Year = as.factor(Year))

# 2) Price side (CPI-adjusted), build snapshots via pivot_wider
price_yearly <- avg_house_price_cpi %>%
  mutate(Year = lubridate::year(Date)) %>%
  filter(Year %in% yrs_ok) %>%
  group_by(State, Year) %>%
  summarise(AvgPrice = mean(AvgPrice_Adj, na.rm = TRUE), .groups = "drop")

# One wide row per state with columns P1990, P1999, ...
price_wide <- price_yearly %>%
  mutate(Year = paste0("P", Year)) %>%
  tidyr::pivot_wider(names_from = Year, values_from = AvgPrice)

# Warn which snapshots are missing (causing NAs)
needed_cols <- paste0("P", yrs_ok)
missing_counts <- colSums(is.na(price_wide[, intersect(names(price_wide), needed_cols), drop = FALSE]))
if (any(missing_counts > 0)) {
  message("States with missing price snapshots (counts by column):")
  print(missing_counts[missing_counts > 0])
}

# Compute price deltas (allow NAs if a snapshot is missing)
avg_house_prices.change <- price_wide %>%
  transmute(
    State,
    `1990_2000` = P1999 - P1990,
    `2000_2010` = P2009 - P2000,
    `2010_2018` = P2018 - P2010
  ) %>%
  pivot_longer(!State, names_to = "Year", values_to = "Average_Price_Change") %>%
  mutate(Year = as.factor(Year))

# 3) Merge pop change and price change on STATE CODES
data.change <- left_join(census.data.change, avg_house_prices.change, by = c("State", "Year"))

# attach Region for plotting
data.change <- left_join(data.change, st_abbrs, by = "State")


## ----fig.height=10, fig.width=14, dev='png'----
ggplot(data.change,
       aes(x = log(abs(Population_Change)), y = Average_Price_Change,
           group = Year, color = factor(Year))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Population Change Vs. Housing Price Change") +
  xlab("Population Change (log scale)") +
  ylab("Housing Price Change") +
  scale_color_manual(values = cb_palette) +
  facet_wrap(Year ~ ., labeller = "label_both") +
  labs(color = "Year") +
  theme_grey(base_size = 22)


## ----echo=FALSE, message=FALSE---------------
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# Map Region to State from your main table
region_map <- avg_house_price_cpi %>%
  dplyr::select(State, Region) %>%
  dplyr::distinct()

# Add Region to your price_change_state table
price_change_state_region <- price_change_state %>%
  dplyr::left_join(region_map, by = "State")

# Correlation per region
cor_by_region <- price_change_state_region %>%
  dplyr::filter(!is.na(Region)) %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise(
    correlation = cor(
      as.numeric(change),
      as.numeric(density),
      use = "complete.obs",
      method = "pearson"
    ),
    .groups = "drop"
  )

print(cor_by_region)
cat("**Result.** The correlation differs by region")


## ----fig.height=10, fig.width=14-------------
# Install needed packages
if (!require("caret")) install.packages("caret")
if (!require("class")) install.packages("class")
if (!require("e1071")) install.packages("e1071")

library(caret)
library(class)
library(e1071)

# Prepare data for modeling
# Use the latest year excluding 2025
latest_year <- max(avg_house_price_cpi$Year[avg_house_price_cpi$Year != 2025])
census <- census %>%
  mutate(
    Population = as.numeric(Population),
    Area = as.numeric(Area),
    density = Population / Area
  )
model_df <- avg_house_price_cpi %>%
  filter(Year == latest_year) %>%
  left_join(census %>% filter(Year == latest_year) %>% dplyr::select(StateName, density),
            by = c("StateName")) %>%
  dplyr::select(State, AvgPrice_Adj, density, Region) %>%
  drop_na()

# Convert Region to factor
model_df$Region <- as.factor(model_df$Region)

# Train/test split
set.seed(123)
train_idx <- createDataPartition(model_df$Region, p = 0.8, list = FALSE)
train_data <- model_df[train_idx, ]
test_data  <- model_df[-train_idx, ]

# --- KNN Model ---
# Normalize numeric predictors
norm_vals <- preProcess(train_data[, c("AvgPrice_Adj", "density")], method = c("center", "scale"))
train_norm <- predict(norm_vals, train_data)
test_norm  <- predict(norm_vals, test_data)

knn_pred <- knn(train = train_norm[, c("AvgPrice_Adj", "density")],
                test  = test_norm[, c("AvgPrice_Adj", "density")],
                cl    = train_norm$Region,
                k     = 5)

cat("KNN Accuracy:", mean(knn_pred == test_norm$Region), "\n")
confusionMatrix(knn_pred, test_norm$Region)

# --- Naïve Bayes Model ---
nb_model <- naiveBayes(Region ~ AvgPrice_Adj + density, data = train_data)
nb_pred <- predict(nb_model, newdata = test_data)

cat("Naïve Bayes Accuracy:", mean(nb_pred == test_data$Region), "\n")
confusionMatrix(nb_pred, test_data$Region)

## ----fig.width=10, fig.height=8, fig.cap="Figure 9. Confusion Matrix Heatmap for KNN classification and Figure 10. Confusion Matrix Heatmap for Naïve Bayes classification"----
library(ggplot2)
library(caret)
library(reshape2)

# --- KNN Confusion Matrix ---
knn_cm <- confusionMatrix(knn_pred, test_data$Region)
knn_df <- as.data.frame(knn_cm$table)
colnames(knn_df) <- c("Prediction", "Reference", "Freq")

p1 <- ggplot(knn_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Figure 9. KNN Confusion Matrix") +
  theme_minimal(base_size = 14)

# --- Naive Bayes Confusion Matrix ---
nb_cm <- confusionMatrix(nb_pred, test_data$Region)
nb_df <- as.data.frame(nb_cm$table)
colnames(nb_df) <- c("Prediction", "Reference", "Freq")

p2 <- ggplot(nb_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "white", high = "tomato") +
  labs(title = "Figure 10. Naïve Bayes Confusion Matrix") +
  theme_minimal(base_size = 14)

# Show plots side by side
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)


## ----echo=FALSE------------------------------
model_comp <- data.frame(
  Model = c("KNN", "Naïve Bayes"),
  Accuracy = c(0.8583, 0.4833)
)

print(model_comp)

cat("**Result** KNN gives higher accuracy than Naïve Bayes.")


## --------------------------------------------
knitr::purl("HousePrice.Rmd", output = "HousePriceRScript.R")

