# Ecuador App

Interactive dashboard for exploring environmental sensor data from Ecuador field sites.

## Project Structure

- `code/` — R/Shiny web application and data generation scripts
- `data/` — TOMST TMS-3 sensor data (temperature, soil moisture)
- `contrib_files/` — External data and scripts from collaborators, staged before integration
  - `data/` — Raw data files as received
  - `scripts/` — Preprocessing scripts
- `docs/` — Metadata and documentation

## Data Sources

- **TOMST TMS-3** sensors: underground temperature and soil moisture
- **HOBO Microstation** weather stations: precipitation and air temperature/humidity

## Running the App

Open `code/app.R` in RStudio and click **Run App**, or from the R console:

```r
shiny::runApp("code/app.R")
```
