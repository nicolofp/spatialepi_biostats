Data Management
================
Thursday, 23<sup>rd</sup> May 2024

## Introduction

Air and water quality are fundamental determinants of public health.
Understanding how these environmental factors influence population
health requires robust data collection and analysis. In Lombardy, Italy,
**ARPA Lombardia**, the regional environmental protection agency, plays
a vital role by providing open-source data on air and water pollution.
This data offers a treasure trove of insights for researchers and public
health professionals. By analyzing the spatial and temporal trends in
these datasets, we can identify areas with higher pollution levels,
potential correlations between pollutants and health outcomes, and
inform targeted interventions to safeguard public health in Lombardy’s
communities.

## Data

APIs act as digital messengers, enabling us to programmatically retrieve
and integrate data from ARPA Lombardia’s platform. They provide a
structured and standardized way to access specific datasets, eliminating
the need for manual data collection or parsing complex web pages. By
leveraging APIs, we can automate the data extraction process, ensuring a
streamlined and efficient workflow

- **Water quality**: historical data from *ISPRA* →
  [here](https://sinacloud.isprambiente.it/portal/apps/sites/?fromEdit=true#/portalepesticidi/pages/area-download)
- **Water human usage**: water for human usage →
  [here](https://www.dati.lombardia.it/resource/beda-kb7b.csv)
- **Air quality**: the air quality data are available as *APIs* from
  *Regione Lombardia* website. The dataset has different tables which
  need to be combined based on the data needs. Note that the APIs have
  limit to 1000 point (see documentation
  [here](https://dev.socrata.com/docs/endpoints)).
  - **Sensor details**: data details on each sensor per station →
    [here](https://www.dati.lombardia.it/resource/ib47-atvt.csv)
  - **Stations details**: data details on each station →
    [here](https://www.dati.lombardia.it/resource/9xaz-9vbz.csv)
  - **Air quality 2018-now**: hourly data →
    [here](https://www.dati.lombardia.it/resource/g2hp-ar79.csv)
  - **Air quality 2010-2017**: hourly data →
    [here](https://www.dati.lombardia.it/resource/nr8w-tj77.csv)
  - **Air quality 2000-2009**: hourly data →
    [here](https://www.dati.lombardia.it/resource/cthp-zqrr.csv)
- **Hospitalization**: number of hospitalization by location and cause →
  [here](https://www.dati.lombardia.it/resource/fwpe-xzv8.csv)
- **Visit**: number of visits by location and cause →
  [here](https://www.dati.lombardia.it/resource/qm4z-s92m.csv)
- **Geolocation**: geolocation of the hospitals →
  [here](https://www.dati.lombardia.it/resource/6n7g-5p5e.csv)

To ensure smooth analysis and accessibility, we take a three-step
approach: parsing, organizing, and storing the data in AWS S3. Parsing
involves transforming the raw data into a consistent, user-friendly
format. We then organize this data based on relevant factors like time,
location, or pollutant type. Finally, we leverage Amazon Web Services’
S3 storage. S3 offers several advantages: it’s highly scalable to
accommodate future data growth, accessible from anywhere for
collaborative research, cost-effective with a pay-as-you-go model, and
secure with robust encryption. By following these steps, we create a
readily available and well-structured resource for researchers,
empowering them to analyze environmental data and contribute to a
healthier Lombardy.
