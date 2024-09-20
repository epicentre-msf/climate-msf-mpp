# MSF Carbon Travel App

Welcome to the Carbon Travel App ! 

This is a multi-function app designed to raise awareness and help minimise the *Carbon emissions* caused by travel across the MSF movement.

It is organised in two parts: 

1. The **Meeting Place Planner** helps MSF/EPICENTRE decision makers identify suitable meeting/event locations in order to minimise the *plane CO2 emissions*.

2. The **Single Travel Estimation** allows users to estimate the distance and *plane CO2 emissions* of a specific travel route. 

### Project genesis

This project stems from the collaboration between the [Climate Smart MSF](https://msfintl.sharepoint.com/:u:/r/sites/ClimateSmartMSF/SitePages/Main-Page.aspx?csf=1&web=1&e=8t2nc5), a TIC (Transformational Investment capacity) project co-sponsored by MSF Canada and OCG, and the [Data Science Team](https://epicentre-msf.github.io/gallery/) at [Epicentre](https://epicentre.msf.org/). 

The goal of the project is to help minimise MSF flight travel emissions by providing a tool that could find the best location to host the many meetings and trainings that the movement organises.

For any general enquiries regarding the project or **Climate Smart MSF** please contact [Maëlle CHARRIER](mailto:Maelle.CHARRIER@geneva.msf.org). For technical enquiries regarding the app, please contact [Hugo SOUBRIER](mailto:hugo.soubrier@epicentre.msf.org) or [Paul CAMPBELL](mailto:paul.campbell@epicentre.msf.org). The code for the app is available on the [Epicentre GitHub](https://github.com/epicentre-msf/carbon-travel-app) where you can also [report an issue](https://github.com/epicentre-msf/carbon-travel-app/issues).

### Meeting place planner methodology

In order to accurately calculate the best meeting locations based on participants origins, we have used the 200 000+ flights in the MSF travel data to create a network of cities linked together by the available flights. This network is then used to calculate a distance matrix, where the distance between two cities corresponds to the shortest *great-circle distance* (using the Haversine formula, taking into account the curve of the Earth) across the network (taking into account stop overs if no direct flights are available). 

This distance matrix is then queried to output the total travel distance from the defined origins and one possible destination. This is repeated for all possible destinations selected by the user. Distances are converted into emissions using the emission factors and destinations are ranked from the lowest emissions to the highest before the top 50 destinations are displayed. 

### Emissions factors

The emission factors used in the analysis are following the common MSF methodology designed by **Climate Smart MSF**. Compared with the emission factors DEFRA, MSF have chosen to use those of ADEME, according to our flight typology. While DEFRA insist on the difference of class (economy/business..), ADEME focus on the haul duration (short/middle/long). More informations on the methodology behind carbon emissions can be found on the [Climate Smart MSF website](https://msfintl.sharepoint.com/:u:/r/sites/ClimateSmartMSF/SitePages/Main-Page.aspx?csf=1&web=1&e=8t2nc5).
