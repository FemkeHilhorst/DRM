
#################
# Getting started 
#################

# Remove everything from R's memory.
rm(list=ls())

# Load the WALRUS package.
library(WALRUS)

# **USER DEMAND** Change working directory to the folder where data-, figures- and output-subfolders 
# are located.
setwd("D:/Thesis/WALRUS")


######
# Data
######

# **USER DEMAND** Read daily or hourly precipitation, potential evapotranspiration and discharge data.
data = read.table("DRM/PEQ_Reusel_hour.dat", header=TRUE)

# **USER DEMAND** Specify which period of the total data set you want to use as forcing data.
# Use the same date format as in data file (for example yyyymmddhh).
forc = WALRUS_selectdates("data", 2011010000, 2011020000)

# Preprocessing forcing and specifying for which moments output should be stored. 
# The argument dt is time step size used in the output data file (in hours).
WALRUS_preprocessing(f=forc, dt=1)


###############################
# Parameters and initial values
###############################

# **USER DEMAND** Define the parameters (cW, cV, cG, cQ, cS), initial conditions (dG0) and 
# catchment characteristics (cD, aS, soil type).
pars = data.frame(cW=249, cV=70, cG=5.6e6, cQ=41.8, cS=5, 
                  dG0=1056.96, cD=1500, aS=0.01, st="sand")


#####
# Run
#####

# Run the WALRUS model. 
mod = WALRUS_loop(pars=pars)

# Postprocessing: create datafiles and show figures.
WALRUS_postprocessing(o=mod, pars=pars, n="basic_example")

################################
# Specify River Characteristics
################################

# **USER DEMAND** Specify the input variables of the dynamic river component. 
# This is the waterlevel a0, the width at water level b.a0 and the bottom 
# depth of the river measured at three locations along the river. With:
# 1: at the outlet of the river
# 2: at the middle of the river
# 3: at the origin of the river

# Initial waterlevel along the river:
a0 = data.frame(a1=1.4, a2=0.9, a3=1.1) # meter

# Width at initial water level:
b.a0 = data.frame(b1=4, b2=1.9, b3=0.2) # meter

# Bottom depth of the river: 
bot.depth = data.frame(zb1=5.4, zb2=14.8, zb3=31.1) # meter

# Measurment locations along the river:
bottom.x = data.frame(x1=0, x2=15000, x1=33455) # meter

# **USER DEMAND** Set the Manning coefficient to a reasonable value:
n = 0.038 #s/m^1/3


##############################
# Preprocess DRM
##############################

# Load the FV Package
library(FVFE1D)

# Preprocessing with river characteristics as input 
source(file="DRM/source_files/DRM_preprocessing.R")


##############################
# Run the Dynamic River Model
##############################

# Run the Dynamic River Model
source(file="DRM/source_files/DRM_run.R")


##################################
# Output Files and Figures of DRM
##################################

# Postprocessing: create datafiles and show figures of DRM
source(file="DRM/source_files/DRM_postprocessing.R")
