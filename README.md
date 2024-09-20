# MSF Carbon Travel App

Welcome to the Carbon Travel App ! 

This is a multi-function app that help users look into the *Carbon emissions* due to travel across the MSF movement.

It is organised in three parts: 

1. The **Travel Data Analysis**, provides an analysis of most of the travel data from the 6 OCs and Epicentre as well as some partners sections. 

2. The **Meeting Place Planner** helps MSF/EPICENTRE decision makers to identify suitable meeting/events locations in order to minimise the *plane CO2 emissions*.

3. The **Single Travel Estimation** allows users to estimate the distance and *plane CO2 emissions* of their specific travel route. 

## Project genesis

This project stems from the collaboration between the [**CLIMATE SMART MSF**](https://msfintl.sharepoint.com/:u:/r/sites/ClimateSmartMSF/SitePages/Main-Page.aspx?csf=1&web=1&e=8t2nc5), a TIC (Transformational Investment capacity) project hosted by MSF Canada and OCG, and the [Data Science Team](https://epicentre-msf.github.io/gallery/) at [Epicentre](https://epicentre.msf.org/). 

The initial goal of the project was to help minimise MSF flight travel emissions by providing a tool that could find the best location to host the many meetings and trainings that the movement organises. After discussion with carbon analysts, it was quickly realised that a dashboard providing key analysis and visualisation of MSF travel data would be greatly beneficial for carbon analyst work but also to raise awareness across MSF.

For any general enquiries regarding the project or the **Climate Smart MSF** please contact **Maëlle CHARRIER** (Maelle.CHARRIER@geneva.msf.org). For technical enquiries regarding the app, please contact **Hugo SOUBRIER** (hugo.soubrier@epicentre.msf.org) or **Paul CAMPBELL** (paul.campbell@epicentre.msf.org). The code for the app is available on the [Epicentre GitHub](https://github.com/epicentre-msf) where you can also [report an issue](https://github.com/epicentre-msf/carbon-travel-app/issues)

## Data source 

The travel data are gathered from different sources depending on the travel agencies used by MSF entities. These data are cleaned and compiled before visualisations. These data can be exported from the App using the *download data* button.

1. AMEX data are provided from 2019 to 2024 for OCG, OCA, OCB and other partner sections. 
2. WAGRAM data are provided from 2019 to 2023 for Epicentre and OCP.
3. CWT data are provided for 2021 and 2022 for OCBA. 

Please be aware that these data are not exhaustive and so **do not represent** the entire scope of MSF travels emissions. Also some information (reason for travel, project code, missions, etc...) are not available for all entities and dates. 

## Meeting place planner methodology

In order to accurately calculate the best meeting locations based on participants origins, we have used the + 200 000 flights in the MSF travel data to create a network of cities linked together by the available flights. This network is then used to calculate a distance matrix, where the distance between two cities corresponds to the shortest *great-circle distance* (using the Haversine formula, taking into account the Earth curve) across the network (taking into account stop overs if no direct flights is available). 

This distance matrix is then queried to output the total travel distance from the defined origins and one possible destination. This is repeated for all possible destinations selected by the user. Distances are converted into emissions using the emission factors and destinations are ranked from the lowest emissions to the highest before the top 50 destinations are displayed. 

## Emissions factors

The emission factors used in the analysis are following the common MSF methodology designed by **CLIMATE SMART MSF**. Comparing with the ones from DEFRA, MSF choose the ADEME one, according to our flight typology : indeed, while DEFRA insist on the difference of class (economy/business..), ADEME focus on the haul duration (Short/middle/Long). More informations on the methodology behind carbon emissions can be found on the [**CLIMATE SMART MSF** website](https://msfintl.sharepoint.com/:u:/r/sites/ClimateSmartMSF/SitePages/Main-Page.aspx?csf=1&web=1&e=8t2nc5)

# Set-up 

In order to run the App locally, you need to have access to `Carbon-travel-App` in Maëlle Charrier's onedrive. Once this is done, you can run the scripts in `data-prep` to prepare the data and the distance matrix: 

You only need to run `0_master_data_preparation.R` - this will source in the right order the following script:

1. `1_prepare_cities.R` - prepares the cities, airports and MSF data
2. `2_prepare_amex.R` - binds, clean and add cities to the AMEX data (OCB, OCA, OCG)
3. `3_prepare_wagram.R` - binds, clean and add cities to the WAGRAM data (Epicentre, OCP)
4. `4_prepare_cwt.R` - binds, clean and add cities to the CWT data (OCBA)
5. `5_bind_amex_wagram_cwt.R` - binds all cleaned flights data together
5. `6_prepare_distance_matrix.R` - uses the cleaned flights data to generate a network and compute distance matrix from it

# Roadmap

- [ ] Extent to include train data from the various data source
- [ ] Add a way for individual to see their own data
