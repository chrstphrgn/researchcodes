This is a repository of the datasets and codes used in the 2024 paper titled "Nighttime Lights and Vegetation Indices—A Spatial Analysis of Urban Vitality and Model Prediction Building for Learning Spaces in Lipa City" by Christopher Al Eugenio, Yasmine Joy Familaran, and Lieh Honoridez, undergraduate BS Statistics students of the School of Statistics, University of the Philippines Diliman, Quezon City. 

Several software programs were used in the different sections of the analyses. Included here are the R codes and Google Earth Engine codes for reproducibility of the results. 

"R Codes Part One" covers the following sections of the analysis:
  1. Exploration of nighttime lights
  2. Kernel density estimation of nighttime lights
  3. Quadrat analysis for the correlation between nighttime lights and vegetation
  4. Variogram modeling
  5. Kriging
  6. Computation of DistNS (distance to nearest school) covariate
  7. Testing for departure from complete spatial randomness of learning spaces

"R Codes Part Two" covers the following:
  1. Exploration of learning spaces
  3. Kernel density estimation of learning spaces
  4. Covariate spatial image preparation
  5. Fitting Log-Gaussian Cox Process and Inhomogeneous Poisson Process models to model the intensity of learning spaces

As mentioned in the paper, Google Earth Engine is used to overlay the nighttime lights and vegetation cover satellite images over Lipa City. Pixel information were extracted there as well. Running the code in GEE requires uploading the necessary shape file for the study region. 
