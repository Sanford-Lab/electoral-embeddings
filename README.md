---
editor_options: 
  markdown: 
    wrap: 72
---

# Method

## Data acquisition

**2020 precinct-level results** are obtained from Harvard Dataverse
([link](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/K7760H)).
For some reason I was unable to multi-select and download, so I clicked
through every state.

-   *Note:* For states KY and NJ, I used `ky_2020.zip` and `nj_2020.zip`
    (which include a mix of county and precinct-level data) instead of
    the `vtd_estimates` for precinct results.

For this step, there should now be 51 zip files — I manually stored them
in a folder called 'og_zipped'. Path should be: `data/mixed/og_zipped`.

Then run the script `2020_precincts_prep.R` to unzip, append with uuid
primary keys, then re-zipped for GEE use.

``` r
source("2020_precincts_prep.R")
```

**NYT data for 2024** is obtained from the detailed election map
released by NYT ([download
link](https://int.nyt.com/newsgraphics/elections/map-data/2024/national/precincts-with-results.topojson.gz)).
Rename it `precincts-with-results.topojson`. Then in terminal, we
convert it to shapefile using `ogr2ogr`.

IN TERMINAL: first run `brew install gdal` to install `gdal` if not
already installed. Then run (first replace the paths with the correct
paths):

`ogr2ogr -f "ESRI Shapefile" .../electoral-embeddings/data/precincts/nyt_2024_shapefiles ...inputpath/precincts-with-results.topojson -s_srs EPSG:4326 -t_srs EPSG:4326 -lco ENCODING=UTF-8`

Finally, once the folder is created, multi-select the 5 newly created
files, right click, and compress. This creates a file called
`Archive.zip`. Run the chunk in `primary_key_generation.qmd` to add
primary keys, then re-compress for GEE use.

## Data cleaning

add primary key

google earth engine step

# Satellite Embeddings Vote Share Prediction

This project analyzes how well Google's satellite embeddings predict
Trump's 2020 presidential vote share at the county and precinct levels
using machine learning models.

## Overview

We use satellite imagery embeddings from Google Earth Engine
(`GOOGLE/SATELLITE_EMBEDDING/V1/ANNUAL`) to predict electoral outcomes.
The embeddings capture visual patterns in satellite imagery that may
correlate with socioeconomic, demographic, and political characteristics
of different areas.

## Data Sources

### Electoral Returns

-   **Precinct-level**: VEST (Voting and Election Science Team) data
    from [Harvard
    Dataverse](https://dataverse.harvard.edu/dataverse/electionscience)
-   **County-level**: County presidential returns from [Harvard
    Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ)

### Satellite Embeddings

-   **Source**: Google Earth Engine
    `ee.ImageCollection("GOOGLE/SATELLITE_EMBEDDING/V1/ANNUAL")`
-   **Description**: Multi-dimensional embeddings derived from satellite
    imagery

## Files

### R Scripts

-   `download_data.R` - Downloads and processes electoral data from VEST
    and Harvard Dataverse
-   `predict_vote_share.R` - Machine learning analysis using XGBoost,
    Random Forest, and Elastic Net

### JavaScript

-   `extract_embeddings.js` - Google Earth Engine script to extract
    satellite embeddings

## Setup Instructions

### 1. Prerequisites

#### R Dependencies

``` r
install.packages(c(
  "tidyverse",
  "sf", 
  "httr",
  "jsonlite",
  "curl",
  "xgboost",
  "ranger",
  "glmnet",
  "caret",
  "vip",
  "patchwork",
  "here"
))
```

#### Google Earth Engine

1.  Sign up for a [Google Earth Engine
    account](https://earthengine.google.com/)
2.  Access the [Code Editor](https://code.earthengine.google.com/)
3.  Enable the Google Earth Engine API

### 2. Data Download

Run the data download script:

``` r
source("download_data.R")
```

This will: - Create `data/` directory structure - Download VEST precinct
data for selected states - Download county-level returns from Harvard
Dataverse - Process and clean the electoral data

### 3. Extract Satellite Embeddings

#### Upload Shapefiles to Earth Engine

1.  In the Earth Engine Code Editor, go to the Assets tab
2.  Click "New" → "Shape files"
3.  Upload your precinct and county shapefiles
4.  Note the asset paths (e.g., `users/your_username/precincts_2020`)

#### Run Embedding Extraction

1.  Open `extract_embeddings.js` in the Earth Engine Code Editor

2.  Update the asset paths to match your uploaded files:

    ``` javascript
    var precincts = ee.FeatureCollection('users/your_username/precincts_2020');
    ```

3.  Run the script

4.  Monitor the Tasks tab for export progress

5.  Download the CSV files from your Google Drive

### 4. Machine Learning Analysis

Run the analysis script:

``` r
source("predict_vote_share.R")

# County-level analysis
county_results <- run_analysis(
  embeddings_file = "path/to/county_embeddings.csv",
  returns_file = "data/counties/county_returns_2020_processed.csv",
  level = "county"
)

# Precinct-level analysis  
precinct_results <- run_analysis(
  embeddings_file = "path/to/precinct_embeddings.csv",
  returns_file = "data/precincts/precinct_returns_2020_processed.csv", 
  level = "precinct"
)
```

## Model Performance Metrics

The analysis evaluates models using: - **RMSE**: Root Mean Square
Error - **MAE**: Mean Absolute Error\
- **R²**: Coefficient of Determination

## Outputs

### Results Files

-   `results/model_performance_county.csv` - County-level model metrics
-   `results/model_performance_precinct.csv` - Precinct-level model
    metrics

### Visualizations

-   `results/plots/` - Prediction plots, residual plots, and feature
    importance plots
-   Actual vs. predicted scatter plots
-   Residual analysis plots
-   Top 20 most important embedding features

## Methodology

### Data Processing

1.  **Electoral Data**: Download and process 2020 presidential returns
    at county and precinct levels
2.  **Embeddings**: Extract mean satellite embeddings for each
    geographic unit using Google Earth Engine
3.  **Merging**: Combine embeddings with electoral returns using FIPS
    codes (counties) or precinct IDs

### Machine Learning Models

1.  **XGBoost**: Gradient boosting with early stopping and
    cross-validation
2.  **Random Forest**: Ensemble method with permutation importance
3.  **Elastic Net**: Regularized linear model for baseline comparison

### Model Evaluation

-   80/20 train/test split
-   Cross-validation for hyperparameter tuning
-   Feature importance analysis
-   Residual analysis for model diagnostics

## Troubleshooting

### Common Issues

#### Earth Engine Quotas

-   If you hit processing limits, reduce the number of features or use
    sampling
-   Try running county-level analysis first (fewer features than
    precincts)

#### Memory Issues

-   Reduce the `MAX_PIXELS` parameter in the Earth Engine script
-   Use `tileScale` parameter to manage memory usage
-   Process data in smaller batches

#### Data Merging Issues

-   Ensure FIPS codes are properly formatted (5-digit zero-padded)
-   Check that precinct IDs match between embeddings and returns data
-   Verify coordinate reference systems are consistent

### Performance Optimization

-   Start with a sample of counties/precincts for testing
-   Use appropriate scale parameters in Earth Engine
-   Consider dimensionality reduction for very high-dimensional
    embeddings

## Expected Results

Based on similar studies, we expect: - **County-level**: R² of 0.3-0.6
(moderate predictive power) - **Precinct-level**: R² of 0.4-0.7 (higher
granularity may improve predictions) - **Feature Importance**:
Urban/rural indicators, infrastructure, and land use patterns

## Citation

If you use this code or methodology, please cite: - VEST data: [Voting
and Election Science
Team](https://dataverse.harvard.edu/dataverse/electionscience) - County
returns: [Harvard
Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ) -
Satellite embeddings: [Google Earth
Engine](https://earthengine.google.com/)

## License

This project is for research purposes. Please ensure you comply with the
terms of service for all data sources used.
