#########################################################
####Created by Elise Pendall##################
####Modified by Anne Greibel and Jinyan Yang##
#########################################################

# Table showing abbreviations and units################################
# Abbr	Variable details	                          Units
# Fe/LE	Latent heat flux	                          W m-2 per unit time
# Fg	Soil heat flux 	                                  W m-2 per unit time
# Fh	Sensible heat flux from virtual heat flux	  W m-2 per unit time
# Fsd	Solar downwelling radiation	                  W m-2 per unit time
# Fsu	Solar upwelling radiation	                  W m-2 per unit time
# Fld	Longwave downwelling radiation	                  W m-2 per unit time
# Flu	Longwave upwelling radiation	                  W m-2 per unit time
# NEP	Net ecosystem productivity	                  umol m-2 s-1
# GPP	Gross primary productivity	                  umol m-2 s-1
# ER	Ecosystem respiration	                          umol m-2 s-1
# RH	Relative humidity	                          %
# Ta	Air temperature	                                  C

#########################################################
#### LOADING A FILE AND GET BASIC SUMMARY STATISTICS ####
#########################################################

#read data files and create data frames:
flux.df <- read.csv("04Jan19.csv",                # read your csv data file 
                 header=TRUE,               # your data file contains variable names in addition to the data
                 skip=2)                    # skip the first two rows to use the abbreviated variable names in column 3 as your variable descriptor
flux.df$timestamp <- as.POSIXct(flux.df$xlDateTime, # create a new column called 'timestamp', which is now a date-time object (as.POSIXct)
                             tz="Australia/Sydney",     # declare the time zone in which the data was recorded (Australia/Sydney or Australian Eastern Standard Time)
                             format = "%d/%m/%Y %H:%M")  # describe the format of the date-time column in the original file

# check structure of data frame:
str(flux.df)

# getting summary statistics 
summary(flux.df)                           # summary statistics for the whole data frame
summary(flux.df$Fe)                        # summary stats for the specified column

#########################################################
#### Step 3##############################################
#########################################################
flux.df$albedo <- flux.df$Fsu / flux.df$Fsd
summary(day_flux.df$albedo)

# How to subset data frames
day_flux.df <- subset(flux.df, flux.df$Fsd>10)     # flux.df$Fsd>10 is the condition we specify, and our new data frame is called "day_flux.df"
# this data frame will now only contain data where the incoming shortwave radiation is positive (the sun is up = daytime hours)
summary(day_flux.df$albedo)

# few different options of how to calculate variables
sum(day_flux.df$Fsd)                            # gives you just the sum of shortwave-downwelling radiation for the data frame flux.df
mean(day_flux.df$Fsd)


#########################################################
#### Step 4##############################################
#########################################################
# Calculate the net radiation from the component radiation as Fnet = Fsd+Fld-Fsu-Flu. 
flux.df$Fnet <- flux.df$Fsd + flux.df$Fld - flux.df$Flu - flux.df$Fsu
# a)	Plot Fnet and the shortwave and longwave components vs. time ####
plot(flux.df$Fnet~flux.df$timestamp,               # plot(...) creates a new plot
     type='l', 
     col="blue", 
     ylim=c(-100,1200),
     ylab="Energy flux (W/m2/30min)", 
     xlab="Time of Day", 
     main="Jan 2019")
# downwelling
points(flux.df$Fsd~flux.df$timestamp,             # points(...) adds a new line to the existing plot
       type='l',
       col="red")
points(flux.df$Fld~flux.df$timestamp, 
       type='l', 
       col="orange")
# upwelling
points(flux.df$Flu~flux.df$timestamp, 
       type='l', 
       col="orange",lty='dashed')
points(flux.df$Fsu~flux.df$timestamp, 
       type='l', 
       col="red",lty='dashed')

# adding a legend to the plot 
legend("topright",                       # where to place it
       legend = c("Fnet","Fsd", "Fld",'Fsu','Flu'),      # specify how many variables you have plotted and keep the order of the colour the same as the order of variables
       col = c("blue","red","orange","red","orange"),   # specify the colour of the variables in your plot (keep the order of the variables!)
       ncol = 1,                         # how many columns your legend should have (1 =  below each other, 3 = next to each other)
       lty = c(1,1,1,2,2),                   # lty describes the line type, try playing with different numbers and see what happens)
       bty="n")                          # in case you don't want a box around your legend, simply remove if you do want to keep it

# adding a straight line to the plot (e.g. line across 0)
abline(h=0,
       col="gray",
       lwd=2) 

# b)	Plot Fnet, Fh, Fe, and Fg vs. time on one graph for each day####
plot(flux.df$Fnet~flux.df$timestamp,               
     type='l', 
     col="blue", 
     ylim=c(-100,1000),
     ylab=expression("Energy flux"~(W~m^-2~'30min'^-1)), 
     xlab="Time of Day", 
     main="Jan 2019")
# downwelling
points(flux.df$Fh~flux.df$timestamp,             
       type='l',
       col="red")
points(flux.df$Fe~flux.df$timestamp, 
       type='l', 
       col="lightskyblue")
points(flux.df$Fg~flux.df$timestamp, 
       type='l', 
       col="black")

# adding a legend to the plot 
legend("topright",                       
       legend = c("Fnet","Fh", "Fe",'Fg'),     
       col = c("blue","red","lightskyblue","black"),   
       ncol = 1,                         
       lty = c(1,1,1,1),                  
       bty="n")   


#########################################################
#### Step 5##############################################
#########################################################
# a)	Show how LE can be converted to ET 
# let first define the converting factors based on unit
J2WS  <-  1
S230min <- 1800
Mj2J <-   1e-6  
KG2MJ <- 1/2.45
m32KG <- 1e-3
mm2m <- 1e3
# Et then can be calculated from Fe/LE 
flux.df$ET <- flux.df$Fe * J2WS * S230min * 
        Mj2J * KG2MJ * m32KG * mm2m

# b)	What is the total water loss for each day 
sum(flux.df$ET)
sum(flux.df$ET[flux.df$Fsd>10])

