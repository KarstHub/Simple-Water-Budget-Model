# Simple-Water-Budget-Model
Simple water budget model for the Lower and Upper Macleay areas

Instructions regarding the R code for recharge time series simulation of the Lower and Upper Macleay areas:

1) Copy/save the R code and the Input_Data folder into a folder of your preferred location on your computer. The location of this folder will be set as your working directory in your R code. The Input_Data folder contains two txt files (Lower and Upper Macleay) with time series of precipitation [mm/d], actual evapotranspiration [mm/d] and observed recharge events for the time period 2012 to 2019.

2) Before running the "Model_Macleay.R" file, R program must be installed on your computer. 

	Follow the instructions below to install R and to run the R code:
	• Install R program
	    Download and install R (version: R-3.5.0 or latest version) for your operating system from https://cran.r-project.org
	• Install R studio
	    Download and install R studio from https://www.rstudio.com, Rstudio is also available as part of Anaconda distribution (https://www.anaconda.com). I


3) Open the R file. Insert the working directory in line 30 to the direction of your folder. Run the R code.

4) Results of Macleay function in the R code:
	• After running the Macleay function, numbers of observed and simulated events and numbers of the matches and missmatches are printed in the console.

5) Output of the R code:
	• txt files containing time series of recharge [mm/d], matches and missmatches for the  Lower and Upper Macleay areas. Stored in the "Output_Data" folder generated by the R code at your working directory. As the model is considering a warm up period the time series of the output files are from 01.07.2014 to 01.07.2019. 


For questions - about the model contact Romane Berthelin; Email: romane.berthelin@hydrology.uni-freiburg.de
				about the code contact Mirjam Scheller; Email: mirjam.karstlab@gmail.com