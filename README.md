# Accuracy of Copernicus Altimeter Water Level Data in Italian Rivers

This repository contains the code supporting the analyses presented in:

**Deidda, C., De Michele, C., Arslan, A. N., Pecora, S., & Taburet, N. (2021).**
*Accuracy of Copernicus Altimeter Water Level Data in Italian Rivers Accounting for Narrow River Sections.*
**Remote Sensing, 13(21), 4456.**
https://doi.org/10.3390/rs13214456

## Overview

Accurate information on river water levels is essential for hydrological monitoring and flood and drought risk assessment. However, in-situ monitoring stations are sparse or unavailable in many parts of river networks.

Satellite radar altimetry provides an alternative source of water-level information with broad spatial coverage.

In this study, we evaluate the accuracy and potential of satellite altimetry for monitoring water levels in Italian rivers, with particular attention to relatively narrow river sections.

Satellite observations from several altimetric missions are compared with corresponding in-situ measurements to assess their performance and potential for operational river monitoring.

## Satellite missions

The analysis considers water-level observations retrieved from Copernicus and satellite altimetry missions, including:

* **Sentinel-3A**
* **Sentinel-3B**
* **Jason-2**
* **Jason-3**

Satellite observations are compared with in-situ water-level measurements from **19 gauging stations across Italy**, with river widths ranging approximately from 50 to 555 m.

## Methodology

The accuracy of satellite-derived water levels is evaluated by comparing satellite and in-situ observations at corresponding locations and times.

The analysis first considers differences between successive water-level measurements in order to evaluate whether satellite observations correctly reproduce temporal variations in river levels.

Satellite water-level time series are then reconstructed using two approaches:

1. using the first joint satellite–in-situ observation as the initial water level;
2. calibrating the initial water level by minimizing the mean absolute error between satellite and in-situ observations.

Performance is evaluated using statistical metrics including correlation and error m
