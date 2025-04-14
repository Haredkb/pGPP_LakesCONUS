# pGPP_LakesCONUS
Estimating pelagic lake primary productivity 

<!-- GETTING STARTED --
## Getting Started
This base project contains all files required to run for HUC04 0202, subregion: Upper Hudson. 
Additional HUC4 should be added to the NHD folder or redirected to local NHDPlus folder.  

To conduct this analysis as is, run 
(1) 
   ```sh
library(targets)
    ```
(2) Check file paths 
(3) In consule run: 
   ```sh
tar_make()
    ```
(4) To check output within R, use 
   ```sh
output_file -> read_tar(ODE_)
    ```
