# Karaçukur Landslide Workflow

This repository contains the R scripts, input datasets, and workflow used to support
the analyses and conceptual model presented in the Karaçukur landslide study.

## Repository structure

- `data/`  
  Input datasets used in the analyses, including:
  - ERA5 daily rainfall
  - ERA5 air skin temperature
  - MODIS snow cover rate

- `scripts/`  
  R scripts used for:
  - Time-series analysis of rainfall, temperature, and snow cover
  - Antecedent precipitation and snowmelt calculations
  - Generation of figures and the conceptual workflow model

## Requirements
The analyses were conducted using R (version ≥ 4.2).  
Required R packages are documented within the scripts.

## Usage
Run the scripts in the `scripts/` directory in the order described in their headers.
All input files are read from the `data/` directory.

## Data and code availability
All data and code supporting the findings of this study are openly available in this
GitHub repository:
https://github.com/fatih6155/karacukur-landslide-workflow

## License
This project is licensed under the MIT License.
