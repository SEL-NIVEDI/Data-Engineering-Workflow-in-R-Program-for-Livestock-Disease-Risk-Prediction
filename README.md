This work consists of a suite of R programs designed to implement a complete data engineering pipeline for forecasting
livestock disease outbreaks in India. The R scripts systematically automate data extraction, cleaning, transformation, and
integration processes needed for predictive modeling. The codebase includes modules that extract epidemiological,
climatic, livestock, and geospatial data from structured databases and external sources using SQL queries and API calls.
The R code handles missing data imputation, removes duplicates, and performs spatial and temporal alignment by linking
disease data with geocoded district shapefiles and month-wise climatic variables. A major component of the code
performs PCA on 23 climatic and delta parameters to reduce dimensionality, and integrates these with historical disease
data. It also generates lag variables and computes disease risk scores to capture seasonal patterns and outbreak intensity.
Feature engineering functions in the code assign risk weightages and compute monthly disease scores for each district-
disease combination. Finally, all components are merged into a structured dataset and exported as a CSV file, ready for
machine learning forecasting. These R programs form a core part of the operational Livestock Disease Risk Forewarning
System and are critical for producing accurate and timely risk predictions at the district level.
