# Uber Data Analytics Dashboard

An interactive web application built with R and Shiny to analyze and visualize Uber rideshare data from New York City (April - September 2014). This project provides insights into ride patterns across different times of the day, days of the week, and months.

## Features

- **Interactive Dashboard**: A user-friendly interface built using `shiny` and `shinydashboard`.
- **Data Visualization**:
  - Trips by Hour and Month
  - Trips by Day of the Month
  - Heatmaps of Trips by Hour and Day
  - Heatmaps of Trips by Month and Day
- **Data Tables**: View aggregated hourly trip data.
- **Login System**: Basic authentication to access the dashboard.

## Dataset

The dataset used contains Uber raw data from April to September 2014. The data includes:
- Date and Time of the trip
- Lat/Lon coordinates
- Base company code

You can find the data on Kaggle: [Uber Rides Dataset](https://www.kaggle.com/datasets/fivethirtyeight/uber-pickups-in-new-york-city).

*Note: You need to download all the CSV files (April to September 2014) and place them in the correct directory (or adjust the file paths in the script) to run the full analysis.* 

## Prerequisites

To run this project, you need R installed along with the following packages:

- `shiny`
- `shinydashboard`
- `ggplot2`
- `ggthemes`
- `lubridate`
- `dplyr`
- `tidyr`
- `DT`
- `scales`

You can install these packages in R using:
```R
install.packages(c("shiny", "shinydashboard", "ggplot2", "ggthemes", "lubridate", "dplyr", "tidyr", "DT", "scales"))
```

## How to Run

1. Clone this repository.
2. Download the datasets and ensure the file paths in the script point to your data directory (currently pointing to `~/uber dataset/...`).
3. Open `uber data analytics.R` in RStudio or any R environment.
4. Run the script or click the "Run App" button in RStudio.
5. Log in using the default credentials configured in the script:
   - **Username**: `kiran`
   - **Password**: `***`
   
   *⚠️ Security Warning: Be sure to change the default credentials in the source code before deploying this app in a production environment.*
