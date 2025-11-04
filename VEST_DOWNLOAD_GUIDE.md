# VEST Data Download Guide

## Overview
This guide explains how to download VEST (Voting and Election Science Team) precinct-level data for the 2020 election.

## Step-by-Step Instructions

### 1. Access VEST Data
- Go to: https://dataverse.harvard.edu/dataverse/electionscience
- Browse the available datasets

### 2. Find State-Specific Data
Look for datasets named like:
- "California 2020"
- "Texas 2020" 
- "Florida 2020"
- etc.

### 3. Download Shapefiles
For each state you want to analyze:
1. Click on the state dataset
2. Look for files with extensions:
   - `.shp` (shapefile)
   - `.dbf` (database file)
   - `.shx` (shape index)
   - `.prj` (projection file)
3. Download all four files for each state

### 4. Organize Files
Place the downloaded files in the appropriate directory structure:
```
data/precincts/
├── CA_2020/
│   ├── california_2020.shp
│   ├── california_2020.dbf
│   ├── california_2020.shx
│   └── california_2020.prj
├── TX_2020/
│   ├── texas_2020.shp
│   ├── texas_2020.dbf
│   ├── texas_2020.shx
│   └── texas_2020.prj
└── ...
```

### 5. Run Processing
After downloading and organizing the files, run:
```bash
Rscript download_data.R
```

The script will:
- Detect the VEST shapefiles
- Process the electoral data
- Calculate Trump vote shares
- Create a consolidated precinct dataset

## Alternative: Test with Sample Data

If you want to test the functionality first, you can create sample data:

```bash
Rscript download_sample_vest.R
```

This creates fake precinct data that mimics the VEST structure for testing.

## Expected VEST Data Structure

VEST shapefiles typically contain columns like:
- `GEOID` or similar: Geographic identifier
- `VOTES_GOP` or `REP_VOTES`: Republican/Trump votes
- `VOTES_DEM` or `DEM_VOTES`: Democratic/Biden votes
- `TOTAL_VOTES`: Total votes cast
- Geographic columns for boundaries

## Troubleshooting

### No Shapefiles Found
- Ensure all four files (.shp, .dbf, .shx, .prj) are present
- Check that files are in the correct directory structure
- Verify file permissions

### Processing Errors
- Check that the shapefile can be read by R's `sf` package
- Ensure the vote columns exist and contain numeric data
- Verify that total votes > 0 for all precincts

### Missing Vote Columns
The script tries to automatically detect vote columns by looking for:
- Republican votes: `(rep|gop|trump)`
- Democratic votes: `(dem|biden)`

If your data uses different column names, you may need to modify the processing logic.

## States with Available VEST Data

VEST typically provides data for all 50 states. Popular states for analysis include:
- California (CA)
- Texas (TX)
- Florida (FL)
- New York (NY)
- Pennsylvania (PA)
- Ohio (OH)
- Georgia (GA)
- North Carolina (NC)

## File Sizes

VEST shapefiles can be large:
- Small states: 10-50 MB
- Large states: 100-500 MB
- Very large states (CA, TX): 500+ MB

Make sure you have sufficient disk space and bandwidth.

## Citation

If you use VEST data in your research, please cite:
> Voting and Election Science Team. 2020. "2020 Precinct-Level Election Results." Harvard Dataverse. https://dataverse.harvard.edu/dataverse/electionscience
