# Hot-springs-geochemistry

This script was written to analyse the environmental variables which influence metal availability in hot spring waters. It includes a principal component analysis (PCA) and plots the outputs of this in R (v.2024.09.1+394). This is followed by multiple general linear models which were compared to determine the best statistical representation of the geochemical data and interactions between metals and environmental variables.
The dataset is available on the BGS repository (see link below).

While dissolved sulfide (S) was included in the original dataset and in general linear models, it was removed from the PCA due to a lack of literature data with S measurements. 

Galloway, T., Baidya, A. S., Cousins, C., Stueeken, E. (2024). Geochemical data from hot springs and associated bed rocks worldwide along with Mars samples and meteorites. NERC EDS National Geoscience Data Centre. (Dataset). https://doi.org/10.5285/49b34dca-2726-4eba-a166-b6adddb61cec
