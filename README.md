This is a repository of the datasets and codes used in our paper titled "Nighttime Lights and Vegetation Indices—A Spatial Analysis of Urban Vitality and Model Prediction Building for Learning Spaces in Lipa City". 

Several software programs were used in the different sections of the analyses. Included here are the R codes and Google Earth Engine codes for reproducibility of the results. 

"R Codes Part One" covers the following sections of the analysis:
  1. Exploration of nighttime lights
  2. Kernel density estimation of nighttime lights
  3. Quadrat analysis for the correlation between Nnighttime lights and vegetation
  4. Variogram modeling
  5. Kriging
  6. Computation of DistNS (distance to nearest school) covariate
  7. Testing for departure from complete spatial randomness of learning spaces
  8. Covariate spatial image preparation for modeling intensity of learning spaces

"R Codes Part Two" covers the following:
  1. Exploration of learning spaces
  2. Kernel density estimation of learning spaces
  3. Fitting Log-Gaussian Cox Process and Inhomogeneous Poisson Process models to model the intensity of learning spaces

As mentioned in the paper, Google Earth Engine is used to overlay the nighttime lights and vegetation cover satellite images over Lipa City. Pixel information were extracted there as well. Running the code in GEE requires uploading the necessary shape files for the study region. 
