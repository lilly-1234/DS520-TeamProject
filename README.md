# DS520 - Team Project
# Title - Home Price Prediction with Population Trends 

## Project Overview
This project studies housing prices in the United States from 1975 to 2025.  
The focus is on average prices adjusted with the Consumer Price Index.  
Population growth and density are also included in the analysis.  
The data is grouped by states and regions.  
The work is done to explore how population trends connect with housing markets.

## Data Sources
- **House Price Data**: Freddie Mac House Price Index (FMHPI).  
- **Population Data**: U.S. Census Bureau population and housing unit estimates.  
- **Consumer Price Index (CPI)**: Bureau of Labor Statistics.  
- **State Codes and Regions**: State abbreviation files used for mapping.

## Methodology
The data is collected and cleaned.  
House prices are adjusted using CPI values.  
Population density is calculated for each state.  
Trends are plotted to compare changes across years.  
Classification models are used to test regional prediction.

## Tools and Libraries
- **R**: Main environment for analysis.  
- **tidyverse**: For data cleaning and visualization.  
- **ggplot2**: For plots and charts.  
- **caret**: For training models.  
- **readxl**: For loading Excel data.

## Results
The analysis shows housing prices have grown steadily.  
Price growth differs by region.  
Population density has a visible link with price change.  
Classification with KNN shows high accuracy.  
Naïve Bayes shows weaker results.

