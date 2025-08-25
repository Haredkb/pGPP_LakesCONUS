<!-- Lake Pelagic Primary Production Estimate -->

Created by Danielle K. Hare 2024 
Github haredkb
Last Update June 18 2025 

<!-- ABOUT THE PROJECT -->
## About The Project

Estimating pelagic lake primary productivity within continental USA
Based on analysis and methodology from Kelly et al. 2018

<!-- GETTING STARTED -->

This base project contains all files required to run for HUC04 0202, subregion: Upper Hudson. 
Additional HUC4 should be added to the NHD folder or redirected to local NHDPlus folder.  

To conduct this analysis as is, run 
(1) library(targets)
(2) Check file paths in _targets.R and functions_tar_pGPP.R
(3) In consule run: tar_make()
(4) To check output within R, use 
output_file -> read_tar(ODE_output)

Also will be saved within folder 5_ModelOutputs

#---------------------------------------------#

If new to targets, can check each function output using tar_read(tar_object) in the console,for example tar_read(input_df)

#---------------------------------------------#
# Methods

Check functions_tar_pGPP.R for all analysis methods; these link to each of the functions within targets workflow. 

<!-- CONTACT -->
Danielle K Hare, PhD, University of Massachusetts Amherst - dhare@umass.edu

Project Link: [https://github.com/github_username/repo_name](https://github.com/github_username/repo_name)

<p align="right">(<a href="#readme-top">back to top</a>)</p>