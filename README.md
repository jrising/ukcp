# Reproduction code for "Towards comprehensive assessments of national climate damages: an application to the United Kingdom"

## Overview

This project aims to comprehensively assess climate damages in the United Kingdom by integrating various climate models, socioeconomic factors, and regional data. The study synthesizes past, present, and projected climate impacts on multiple sectors, including agriculture, health, energy, and biodiversity.

## Project Structure

The project is organized into a series of directories and scripts,
each focusing on different aspects of climate damage assessment. Below
is a breakdown of the primary directories within the project:

- **src1/**: Contains the initial set of scripts and analyses, focusing on top-down approaches to climate damage estimation.
  - **topdown/**:  
	Contains scripts that utilize top-down approaches to visualize and combine climate impact data. The focus here is on national and global assessments of climate damages, using historical data and projections. These scripts set the foundation for understanding overarching climate impacts.

  - **regions/**:  
	A collection of scripts that deal with regional mapping and analysis. This includes converting and aligning different geographical datasets (e.g., GADM to NUTS regions) to ensure consistency in regional analyses. Scripts here often involve spatial data handling and mapping.

  - **hazards/**:  
	Dedicated to calculating climate-related hazards and differences across scenarios, this directory focuses on specific risks such as natural disasters and extreme weather events. The scripts examine the economic impacts of these hazards, prepare visualizations, and generate comprehensive summaries.

  - **synthesis/**:  
	Houses scripts that synthesize data from various channels and models, producing comparisons and assessments of climate damages across sectors. Key tasks include aggregating losses to provide an integrated view of climate impacts, comparing damage functions, and visualizing data through barcharts and other comparative charts.

  - **lib/**:  
	Contains utility scripts and functions designed for data manipulation and statistical modeling. It offers support for geographic calculations, hierarchical modeling, and data disaggregation, facilitating complex computations needed across the project.

  - **climate/**:  
	A comprehensive directory where climate data processing occurs, divided into subdirectories based on dataset origins (e.g., UKCP, CMIP6). Scripts here handle climate model exploration, projection adjustments, and detailed temperature analyses for both historical and future scenarios.

  - **socioeconomics/**:  
	Focuses on analyzing socioeconomic data to assess economic indicators like household income, GDP, and the implications of climate change on these factors. This section ensures that economic assessments are contextualized within prevailing socioeconomic conditions.

  - **channels/**:  
	Contains subdirectories targeting specific sectors like agriculture, energy, health, fisheries, and flooding. Each channel includes scripts that compute the sector's climatic and economic damage impacts, often using custom models and datasets to derive detailed assessments tailored to UK conditions.

- **src2/**: Contains a second round of material where COACCH and CCRA3 are incorporated to expand the assessment of climate impacts in the UK. This phase builds upon the initial analyses in `src1`, adding deeper integration of sector-specific models, additional climate scenarios, and data visualization techniques.

  - **times/**:  
    Focuses on handling the temporal aspects of climate data, with scripts dedicated to calculating and converting time variables essential for analysis under various climate scenarios.

  - **annual/**:  
    Contains scripts that project economic impacts of climate scenarios in the UK, exploring the effects under Sustainable Development Pathways (SSPs) to deliver GDP and damage estimates across different future scenarios.

  - **synthesis/**:  
    This directory includes scripts for advanced data synthesis and analysis, employing clustering, factor analysis, and comprehensive aggregation of climate damages by sector and region. It provides visual representations and conducted simulations for welfare impacts.

  - **CCRA3/**:  
    Houses materials focused on climate risk estimation pertinent to the UK's Climate Change Risk Assessment (CCRA3), emphasizing risk modeling and economic impact projections linked to temperature rises.

  - **compare/**:  
    Contains scripts that evaluate and integrate climate impact data by comparing different methodologies and data sources such as COACCH and CCRA3. It leverages regression analysis and risk visualization to compare national damage assessments.

  - **climate/cmip5/**:  
    Encompasses several directories dedicated to processing CMIP5 climate model data and aggregating scenario results, segmented by regions and categories like energy and agriculture.

  - **channels/**:  
    Includes detailed sector-specific analyses, with a focus on estimating damages for sectors like agriculture, energy, transport, and fisheries. These scripts support the extraction and combination of socioeconomic impacts, modeling future risks under diverse scenarios.

## Prerequisites

- **Software**: R, Python, and required packages.
- **Data**: Access to relevant climate models and datasets used within the scripts.
- **System Requirements**: Details about any specific computational requirements or system settings needed to run the scripts effectively.

## Installation

1. Clone the repository:

   ```bash
   git clone https://github.com/jrising/ukcp.git
   cd ukcp

Set up the environment by installing necessary packages for R and Python:

For R, install packages using:

install.packages(c('dplyr', 'ggplot2', 'rstan', 'reshape2', 'PBSmapping'))

## Usage

The following steps produce the main results for the project.

### Step 1: Prepare Inputs

1. **Climate Data Preparation (`/climate/`)**:  
   Begin by processing climate data. This includes creating climate datasets needed for damage assessment processes.
   
   - **`worldclim/adm0patterns.py`**: Uses weight maps and climate data to aggregate regional impacts from both historical and future scenarios.

   - **`worldclim/global-lmst.R`**: Computes global land mean surface temperatures for historical and future periods, which underpins climate modeling.

2. **Regional Mapping (`/regions/`)**:  
   Transform and convert regional data to align different geographical datasets.

   - **`gadm2nuts.R`**: Converts geographical data, crucial for aligning climate impacts with administrative units.

### Step 2: Underlying Climate and Socioeconomic Visualizations

3. **Climate Hazard Visualization (`src1/hazards/`)**:  
   Summarize potential climate hazards using geographical mapping and scenarios.

   - **`cdiff.R`**: Analyzes climate differences across scenarios, integrating these into regional analyses for visual impact assessment.

4. **Socioeconomic Data Analysis (`src1/socioeconomics/`)**:  
   Integrate economic data for context on how climate impacts intersect with economic variables.

   - **`byadm3.R`**: Analyzes regional Gross Disposable Household Income (GDHI), correlating socioeconomic data with geographic information.

### Step 3: Sector-Specific Analysis

5. **Channel-Specific Projections (`src1/channels/`)**:  
   These scripts each target a specific sector under climate change scenarios.

   - **Agriculture**: 
     - **`agriculture/arable-project.R`**: Projects impacts on arable land and outputs economic damage estimates.
     - **`agriculture/milk.R`**: Assesses the effect on milk production under projected climate conditions.
     - **`livecombine.R`**: Integrates agricultural impacts, focusing on livestock and fisheries.
   
   - **Energy**: 
     - **`energy/rodeetal.R`**: Models energy expenditure changes due to climate variations under different scenarios.

   - **Health**: 
     - **`health/emulate.R`**: Evaluates the mortality rate shifts due to climate impacts using Bayesian models.

   - **Sea Level Rise (SLR) and Flooding**: 
     - **`slr/graphs.R`**: Projects economic impacts of SLR, leveraging simulations based on climate projections.
     - **`flooding/flooding-damage.R`**: Assesses regional flooding damage and projects future costs through modeling functions.

### Step 4: High-Level Impact Assessments

6. **Top-Down Damage Projections (`src1/topdown/`)**:  
   Assess broad climate damages at a national scale.

   - **`graphs.R`**: Visualizes economic impacts across scenarios, integrating data from key studies to display projected damages over time.
   
   - **`combine.R`**: Analyzes national impact, combining results from different sources to estimate GDP losses.

### Step 5: Synthesis and Combined Results

7. **Comprehensive Synthesis (`src1/synthesis/`)**:  
   Merge outputs from various analyses into a singular integrated assessment.

   - **`adm3combo.R`**: Aggregates channel-specific damages into a single comprehensive dataset detailing potential economic impacts across climate scenarios.

   - **`barchart.R`**: Integrates multiple sectoral impacts, providing error margins and confidence intervals for a consolidated economic damage view.

### Step 6: Prepare Additional Climate Data

8. **Time Calculations (`src2/times.R`)**:  
   Begin by processing climate model data to calculate time intervals for the analysis period. This step utilizes various methods to handle temporal data effectively.

### Step 7: EconomicImpact Analysis

9. **Annual Economic Projections (`src2/annual/annual.R`)**:  
   This script projects economic damages and GDP impacts due to climate scenarios, incorporating insights from Sustainable Development Pathways (SSPs) up to the year 2100.

### Step 8: Sector-Specific Analysis

10. **Channel-Specific Analyses (`src2/channels/`)**:  
   Each script under this directory can be run independently, focusing on specific sectors. Ensure that these are executed following the execution of relevant scripts in `src1/channels/`, as they build upon the earlier analyses. The channels include, but are not limited to:

   - **Agriculture**
     - **`agriculture/agriculture-damage.R`**: Assesses climate impacts on agriculture and generates visualizations of economic damages.
     - **`agriculture/agcombine-adm3.R`**: Combines agricultural damage data across administrative regions.
     - **`agriculture/Agri-graphs.R`**: Provides mappings and analysis of agricultural damage assessments.

   - **Energy**
     - **`Energy-Supply/combine.R`**: Aggregates energy supply damage data across different climate scenarios.
     - **`Energy-Demand/combine.R`**: Combines energy demand impact data with economic risk assessments.
   
   - **Transport**
     - **`Transport/transport-damage.R`**: Evaluates the financial impacts of climate change on the transport sector.

   - **Fishery**
     - **`Fishery/fishery-damage.R`**: Analyzes climate-induced damages to the fishing industry.

   - **Sea Level Rise (SLR)**
     - **`SLR/slr-damage.R`**: Calculates and visualizes the impacts of sea level rise across UK regions.

11. **Welfare Simulations (`src2/synthesis/welfaresim.R`)**:  
   This script evaluates different economic welfare scenarios influenced by climate damage.

### Step 9: Synthesis of Results

12. **Comprehensive Synthesis (`src2/synthesis/adm3combo.R`)**:  
   Synthesize results from various channel analyses to produce a comprehensive assessment of climate damages categorized by sector and region.

13. **Combined Results Visualization (`src2/synthesis/barchart.R`)**:  
   Finally, produce visual representations of the aggregated economic damages associated with climate impacts across the UK.
