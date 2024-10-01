# MSF Carbon Travel App

Welcome to the Meeting Place Planner ! 

This is a multi-function app designed to raise awareness and help minimise the *Carbon emissions* caused by travel across the MSF movement.

It is organised in two parts: 

1. The **Meeting Place Planner** helps MSF/EPICENTRE decision makers identify suitable meeting/event locations in order to minimise the *plane CO2 emissions*.

2. The **Single Travel Estimation** allows users to estimate the distance and *plane CO2 emissions* of a specific travel route. 

### Project genesis

This project stems from the collaboration between the [Climate Smart MSF](https://msfintl.sharepoint.com/:u:/r/sites/ClimateSmartMSF/SitePages/Main-Page.aspx?csf=1&web=1&e=8t2nc5), a TIC (Transformational Investment capacity) project co-sponsored by MSF Canada and OCG, and the [Data Science Team](https://epicentre-msf.github.io/gallery/) at [Epicentre](https://epicentre.msf.org/). 

The goal of the project is to help minimise MSF flight travel emissions by providing a tool that could find the best location to host the many meetings and trainings that the movement organises.

For any general enquiries regarding the project or **Climate Smart MSF** please contact [Maëlle CHARRIER](mailto:Maelle.CHARRIER@geneva.msf.org). For technical enquiries regarding the app, please contact [Hugo SOUBRIER](mailto:hugo.soubrier@epicentre.msf.org) or [Paul CAMPBELL](mailto:paul.campbell@epicentre.msf.org). The code for the app is available on the [Epicentre GitHub](https://github.com/epicentre-msf/carbon-travel-app) where you can also [report an issue](https://github.com/epicentre-msf/carbon-travel-app/issues).

## Data source 

The travel data are gathered from different sources depending on the travel agencies used by MSF entities. These data are cleaned and compiled before visualisations. These data can be exported from the App using the *download data* button.

1. AMEX data are provided from 2019 to 2024 for OCG, OCA, OCB and other partner sections. 
2. WAGRAM data are provided from 2019 to 2023 for Epicentre and OCP.
3. CWT data are provided for 2021 and 2022 for OCBA. 

Please be aware that these data are not exhaustive and so **do not represent** the entire scope of MSF travels emissions. Also some information (reason for travel, project code, missions, etc...) are not available for all entities and dates. 

### Meeting place planner methodology

In order to accurately calculate the best meeting locations based on participants origins, we have used the 200 000+ flights in the MSF travel data to create a network of cities linked together by the available flights. 

This network is then used to calculate a distance matrix, where the distance between two cities corresponds to the shortest *great-circle distance* (using the Haversine formula, taking into account the curve of the Earth) across the network (thus taking into account stop overs if no direct flights are available). This distance matrix is then queried to output the total travel distance from the defined origins and one possible destination. This is repeated for all possible destinations selected by the user. 

For emissions, we have used the city network to calculate an emission matrix. Using the shortest *great-circle distance* as per the distance matrix calculation, we retrieve each flight segment that links two cities. Each segment of travel is then converted to the corresponding emissions using the emission factors for short, medium and long hauls. The emissions are then summed over all segments to yield the total travel emissions between two cities. Similarly to the distance matrix, the emission matrix is then queried depending on user inputs to output the total emissions for a planned meeting. 

Distances and emissions for each possible destination are then ranked and displayed in the app. 

### Single Travel Estimation methodology

The single Travel Estimator is meant to be used by user who already know the trip they will do, or have a particular trip in mind. We have created a network linking all the cities in the MSF flights database and the app uses the user inputs to construct a path through the network. Each segment of the path is then analysed for distance and emissions before being displayed.

### Emissions factors

The emission factors used in the analysis are following the common MSF methodology designed by **Climate Smart MSF**. Compared with the emission factors DEFRA, MSF have chosen to use those of ADEME, according to our flight typology. While DEFRA insist on the difference of class (economy/business..), ADEME focus on the haul duration (short/middle/long). More informations on the methodology behind carbon emissions can be found on the [Climate Smart MSF website](https://msfintl.sharepoint.com/:u:/r/sites/ClimateSmartMSF/SitePages/Main-Page.aspx?csf=1&web=1&e=8t2nc5).
