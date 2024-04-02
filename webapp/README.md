# EPI-PREDICT Web Application

This repository contains the source code for the EPI-PREDICT web application, a Shiny web application for visualizing spatio-temporal cholera data in Spain. The application allows users to upload data files and shapefiles, select provinces and months, and visualize the data using interactive maps, time trends, and tables.

## Directory Structure

- **www**: Directory containing static files (e.g., images, CSS, JavaScript) used in the web application.
- **app.R**: R script containing the UI and server logic for the Shiny web application.
- **global.R**: R script containing global functions and libraries used in the web application.

## Usage

To use the web application, follow these steps:

1. Ensure you have R and the required packages installed (see Dependencies section below).
2. Run the web application, and execute the `app.R` script in R or RStudio. The application will launch in your default web browser.
3. Upload the necessary data files and shapefiles as instructed in the application.
4. Interact with the application to explore spatio-temporal cholera data.

### Dependencies

The web application requires the following R packages:

- shiny
- shinyjs
- dplyr
- sf
- DT
- dygraphs
- xts
- leaflet
- tmap

Make sure these packages are installed before running the application.
