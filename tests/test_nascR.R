# A utility for extracting CPS backscatter (cps.nasc) from Echoview CSV files
# Source the script (Ctrl+Shift+S) and respond to the prompts

# To run in Rstudio, press the Source button or Ctrl + Shift + S

# If a path to Exported Images is provided (path.img), corresponding
# echograms will be displayed to assist echo scrutiny

# Clear work space to avoid any issues with processing
rm(list = ls())

# To specify file paths by copy/paste from Windows Explorer, put path in r"(PATH)"
# Install and load pacman (library management package)
if (!require("pacman")) install.packages("pacman")
if (!require("pak")) install.packages("pak")

# Install and load required packages from CRAN ---------------------------------
pacman::p_load(tidyverse, here, fs, atm)

# Close any open graphics windows
graphics.off()

# Extract only CPS backscatter -------------------------------------------------
atm::extract_cps_nasc(
  # Most used options ----------------------------------------------------------
  path.in     = "C:\\SURVEY\\2407RL\\PROCESSED\\EV\\CSV", # CSV file source                                                                                                 e
  pattern.in  = "-Final 38 kHz CPS.csv", # CSV file regex
  path.out    = "C:\\SURVEY\\2407RL\\PROCESSED\\EV\\CSV", # Processed file destination
  suffix.out  = "_nasc_cps.csv",             # Suffix applied to processed CSV files
  path.img    = "C:\\SURVEY\\2407RL\\PROCESSED\\EV\\Exported_Images", # Location of exported image files, or NULL
  pattern.img = "-38 Remove Passive Pings.png", # Exported image regex
  # Lesser used options --------------------------------------------------------
  expansion   = 2,     # Constant for expanding axes
  max.range   = 350,   # Depth range for bubble plots
  dist.bin    = 2000,  # Distance bins for results plot (2000 default, smaller for short transects)
  root        = 2,     # Constant for controlling bubble size (2)
  scaling     = 0.1,   # Constant for controlling bubble size (0.1)
  jpeg        = TRUE,  # Save intermediate plots from line picks
  x11.w       = 1600,  # Width of graphics window (px)
  x11.h       = 600)   # Height of graphics window (px)

# # Configure input and output paths ---------------------------------------------
# ## CSV input path (source)
# path.in  <- here("Data/Backscatter/RL") # KLS laptop
#  path.in <- "C:/SURVEY/2207RL/PROCESSED/EV/CSV/LASKER" # Echoview PC
