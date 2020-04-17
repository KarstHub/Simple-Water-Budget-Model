#------------------------------------------------------------------------------------------------------------------------------------------------------
# Simple water budget model for the upper and lower Macleay areas 
# Author : Mirjam Scheller
# Institute: University of Freiburg, Germany
# Email: mirjam.karstlab@gmail.com
#------------------------------------------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------------------------------------------
# Functions
#------------------------------------------------------------------------------------------------------------------------------------------------------
# Function to check if packages are already installed on your computer
  fileIO.packageRequire <- function(package, repos=NULL){
    
    if(is.null(repos)) repos='https://cran.r-project.org/'
    if(package %in% rownames(installed.packages()) == FALSE) install.packages(package, repos)
    else cat(sprintf('-> %s already installed\n', package))
  }

#------------------------------------------------------------------------------------------------------------------------------------------------------
# Packages
#------------------------------------------------------------------------------------------------------------------------------------------------------
  fileIO.packageRequire("lubridate")
  fileIO.packageRequire("tidyr")
  library("lubridate")
  library("tidyr")

#------------------------------------------------------------------------------------------------------------------------------------------------------
# Directories
#------------------------------------------------------------------------------------------------------------------------------------------------------
  setwd("insert working dir") # set your working directory
  path_data <- (paste0(getwd(), "/Input_Data/"))
  dir.create("Output_Data") # create empty folder in working directory for results
  path_results <- (paste0(getwd(), "/Output_Data/"))

#------------------------------------------------------------------------------------------------------------------------------------------------------
# Input Data and Model Parameters
#------------------------------------------------------------------------------------------------------------------------------------------------------
# Input data time series
  Lower_Macleay <- read.csv(paste0(path_data,"Lower_Macleay.txt"))
  Upper_Macleay <- read.csv(paste0(path_data,"Upper_Macleay.txt"))  
  Lower_Macleay <- Lower_Macleay[,2:ncol(Lower_Macleay)]
  Upper_Macleay <- Upper_Macleay[,2:ncol(Upper_Macleay)]
  Lower_Macleay$Date <- dmy(Lower_Macleay$Date)
  Upper_Macleay$Date <- dmy(Upper_Macleay$Date)

# set model parameters 
  # Lower Macleay
  Overflow_Lower_Macleay <- 65 
  dailyDrain_Lower_Macleay <- 4.8

  # Upper Macleay
  Overflow_Upper_Macleay <- 80
  dailyDrain_Upper_Macleay <- 0.4

#------------------------------------------------------------------------------------------------------------------------------------------------------
# Model
#------------------------------------------------------------------------------------------------------------------------------------------------------
  Macleay <- function(Data,Overflow,dailyDrain) {
    
        # Soil and Epikarst Storage
        Data$SoilEpikarstStorage_mm<- NA
        Data[1,5] <- 0
        for (i in 2:nrow(Data)) {
          Data[i,5] <- max(min((Data[(i-1),5]+Data[i,2]-Data[i,3]-dailyDrain), Overflow, na.rm=T), 0)
        }
        # Total Recharge
        Data$RechargeTotal_mmd<- NA
        Data[1,6] <- 0
        for (i in 2:nrow(Data)) {
          Data[i,6] <- max((Data[(i-1),5]+Data[i,2]-Data[i,3]-Overflow-dailyDrain), 0, na.rm=T)
        }
        # Simulated recharge with Buffer (7d)
        Data$SimulatedRechargeWithBuffer<- NA
        Data[1:7,7] <- 0
        Data[(nrow(Data)-3):nrow(Data),7] <- 0
        for (i in 8:(nrow(Data)-3)) {
          Data[i,7] <- ifelse(max(Data[(i-3):(i+3), 6]) > 1, 1, 0)
        }
        # Number of observed events
        NuObsEvents<-sum(Data$ObservedRecharge)
        # Simulated recharge events during observation period
        Data$SimulatedRechargeEventsDuringObservationPeriod <- NA
        for (i in which(Data$Date == dmy("01.07.2014")):which(Data$Date == dmy("01.07.2019"))) {
          Data[i,8] <- ifelse(Data[i,4] == Data[i,7], Data[i,7], 
                              ifelse(Data[i,7] == 1 && Data[(i+1),7] == 0 
                                     && max(Data[(i-6):i,4]) == 0, -1, 0))
        }
        # Number of simulated events
        NuSimEvents <- sum(Data$SimulatedRechargeEventsDuringObservationPeriod, na.rm = T)
        # Missmatches  (simulated but not observed events)
        Data$Missmatches<- NA
        for (i in which(Data$Date==dmy("01.07.2014")):which(Data$Date==dmy("01.07.2019"))) {
          Data[i,9] <- ifelse(Data[i,8] < 0, Data[i,8], 0)
        }
        # Number of missmatches
        NuMissmatches <- sum(Data$Missmatches, na.rm = T)
        # Matches (simulated and observed events)
        Data$Matches <- NA
        for (i in which(Data$Date==dmy("01.07.2014")):which(Data$Date==dmy("01.07.2019"))) {
          Data[i,10] <- ifelse(Data[i,8] > 0, Data[i,8], 0)
        }
        # Number of matches
        NuMatches <- sum(Data$Matches, na.rm = T)

    # Create a list as output
    result <- list(Data$RechargeTotal_mmd, Data$Matches, Data$Missmatches, 
                   NuObsEvents, NuSimEvents, NuMatches, NuMissmatches)
    return(result)
    } 

#------------------------------------------------------------------------------------------------------------------------------------------------------
# Run Model
#------------------------------------------------------------------------------------------------------------------------------------------------------
  # Lower Macleay
  Lower_Macleay_Model <- Macleay(Lower_Macleay, Overflow_Lower_Macleay, dailyDrain_Lower_Macleay)
  cat(paste0("Results Lower Macleay", "\n", "Observed Events: ", Lower_Macleay_Model[[4]], "\n", "Simulated Events: ", Lower_Macleay_Model[[5]], "\n",
             "Number of Matches: ", Lower_Macleay_Model[[6]], "\n", "Number of Missmatches: ", Lower_Macleay_Model[[7]], "\n"))

  # Upper Macleay
  Upper_Macleay_Model <- Macleay(Upper_Macleay, Overflow_Upper_Macleay, dailyDrain_Upper_Macleay)
  cat(paste0("Results Upper Macleay", "\n", "Observed Events: ", Upper_Macleay_Model[[4]], "\n", "Simulated Events: ", Upper_Macleay_Model[[5]], "\n",
             "Number of Matches: ", Upper_Macleay_Model[[6]], "\n", "Number of Missmatches: ", Upper_Macleay_Model[[7]], "\n"))
  
#------------------------------------------------------------------------------------------------------------------------------------------------------
# Output
#------------------------------------------------------------------------------------------------------------------------------------------------------
  # Lower Macleay
  Lower_Macleay_Output <- as.data.frame(matrix(NA, ncol = 4, nrow = nrow(Lower_Macleay)))  # Create data frame to store results
  colnames(Lower_Macleay_Output) <- c("Date", "Recharge_mmd", "Matches", "Missmatches")
  Lower_Macleay_Output$Date <- Lower_Macleay$Date
  Lower_Macleay_Output$Recharge_mmd <- Lower_Macleay_Model[[1]]
  Lower_Macleay_Output$Matches <- Lower_Macleay_Model[[2]]
  Lower_Macleay_Output$Missmatches <- Lower_Macleay_Model[[3]]
  Lower_Macleay_Output <- Lower_Macleay_Output %>% dplyr::filter(Date >= dmy("01.07.2014") & Date <= dmy("01.07.2019"))
  
  # Upper Macleay
  Upper_Macleay_Output <- as.data.frame(matrix(NA, ncol = 4, nrow = nrow(Upper_Macleay)))  # Create data frame to store results
  colnames(Upper_Macleay_Output) <- c("Date", "Recharge_mmd", "Matches", "Missmatches")
  Upper_Macleay_Output$Date <- Upper_Macleay$Date
  Upper_Macleay_Output$Recharge_mmd <- Upper_Macleay_Model[[1]]
  Upper_Macleay_Output$Matches <- Upper_Macleay_Model[[2]]
  Upper_Macleay_Output$Missmatches <- Upper_Macleay_Model[[3]]
  Upper_Macleay_Output <- Upper_Macleay_Output %>% dplyr::filter(Date >= dmy("01.07.2014") & Date <= dmy("01.07.2019"))
  
#------------------------------------------------------------------------------------------------------------------------------------------------------
# Save Output
#------------------------------------------------------------------------------------------------------------------------------------------------------
  write.csv(Lower_Macleay_Output, paste0(path_results, "Lower_Macleay_Output.txt"))
  write.csv(Upper_Macleay_Output, paste0(path_results, "Upper_Macleay_Output.txt"))
  