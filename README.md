Utility Calculator
==================

This is a Shiny app for calculating Utility (Precision and Return on Investment) for employee selection methods.

If you want to see a live version of the app, please visit https://psychometrics.shinyapps.io/utility/

The code is available here for transparency, collaboration in developing the functionality further and for downloading and using the app locally. 


How to use this app on your computer
------------------------------------

1. Download and install R and (optionally) RStudio.

2. Download this repository to a folder on your local computer and remember the path 
  (e.g. `"C:\Users\morgstro\Documents"` on a Windows computer or `"~/Documents"` on a Mac/Linux)

3. Start RStudio and install the required packages by typing the following in the console window:

  `install.packages(c("shiny", "mvtnorm", "MASS", "dplyr", "ggvis"))`

4. Run the app by typing the following (Change `PATH` to the path where you saved the repository):

  `shiny::runApp("PATH")`

Enjoy!
