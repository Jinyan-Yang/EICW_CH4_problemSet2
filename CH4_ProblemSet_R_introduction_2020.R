#########################################################
####Created by Elise Pendall##################
####Modified by Anne Greibel and Jinyan Yang##
#########################################################

# Table showing abbreviations and units################################
# Abbr	Variable details	                          Units
# Fe	Latent heat flux	                          W m-2 per unit time
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
flux.df <- read.csv("28Jul19.csv",                # read your csv data file 
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
#### BASIC DATA MANIPULATION ############################
#########################################################
# How to subset data frames
day_flux.df <- subset(flux.df, flux.df$Fsd>10)     # flux.df$Fsd>10 is the condition we specify, and our new data frame is called "day_flux.df"
# this data frame will now only contain data where the incoming shortwave radiation is positive (the sun is up = daytime hours)

# calculuate a new vector and attach it to data frame "day_flux.df" based on dividing two existing vectors within that data frame
day_flux.df$albedo <- day_flux.df$Fsu / day_flux.df$Fsd
flux.df$albedo <-flux.df$Fsu / flux.df$Fsd
# few different options of how to calculate variables
# e.g. average soil heat flux
summary(flux.df)                           # gives summary statistics for the whole data frame night&day
summary(day_flux.df)                       # gives summary statistics for the data frame that only contains daytime data
summary(day_flux.df$Fg)                    # gives you a bunch of summary statistics (e.g. minimum, maximum, mean...) of Fg for the data frame "day_flux.df"
mean(flux.df$Fg)                           # gives you just the mean ground heat flux for the data frame flux.df
sum(flux.df$Fg)                            # gives you just the sum of ground heat flux for the data frame flux.df

#########################################################
#### INTRODUCTION TO BASIC DATA PLOTTING ################
#########################################################

# creating a line plot
plot(flux.df$Fg~flux.df$timestamp,               # plot(...) creates a new plot
     type='l',                           # type indicates if it is a line ('l'), or points ('p'), or both ('b')
     col="blue",                         # set the colour
     ylim=c(-20,30),                     # set the limits of the y-axis (note that 'c(,)' indicates a vector with two entries)
     ylab="Ground heat flux (W m^2)",    # describe the y-axis label
     xlab="Time of Day",                 # describe the x-axis label
     main="flux.dfy 2019")                   # give your plot a header so you know which data set you are looking at

# adding another line to the same plot
plot(flux.df$Fg~flux.df$timestamp,               # plot(...) creates a new plot
     type='l', 
     col="blue", 
     ylim=c(-20,30), 
     ylab="Ground heat flux (W m^2)", 
     xlab="Time of Day", 
     main="flux.dfy 2019")
points(flux.df$Ta~flux.df$timestamp,             # points(...) adds a new line to the existing plot
       type='l',
       col="red")
points(flux.df$Ts~flux.df$timestamp, 
       type='l', 
       col="orange")

# adding a legend to the plot 
legend("topright",                       # where to place it
       legend = c("Fg","Ta", "Ts"),      # specify how many variables you have plotted and keep the order of the colour the same as the order of variables
       col = c("blue","red","orange"),   # specify the colour of the variables in your plot (keep the order of the variables!)
       ncol = 1,                         # how many columns your legend should have (1 =  below each other, 3 = next to each other)
       lty = c(1,1,1),                   # lty describes the line type, try playing with different numbers and see what happens)
       cex=0.7,                          # scales your legend size (<1 means smaller, >1 means larger)
       bty="n")                          # in case you don't want a box around your legend, simply remove if you do want to keep it
 
# adding a straight line to the plot (e.g. line across 0)
abline(h=0,
       col="gray",
       lty=2) 

# creating a boxplot
boxplot(flux.df$Fg, 
        ylab="Ground heat flux (W/m2) in flux.dfy")

#changing the plot arrangements
#plot 1 figure in one plot window (default setting)
par(mfrow=(c(1,1)))                      # the c(1,1) refers to 1 row and 1 column

#plot 2 figures in the same plot window
par(mfrow=(c(1,2)))                      # the c(1,2) refers to 1 row and 2 columns, so it plots them next to each other
plot(flux.df$Fg~flux.df$timestamp, 
     type='l', 
     col="blue", 
     ylim=c(-20,30), 
     ylab="Ground heat flux (W m^2)", 
     xlab="Time of Day", 
     main="flux.dfy 2019")
plot(flux.df$Ta~flux.df$timestamp, 
     type='l', 
     col="red", 
     ylim=c(-20,30), 
     ylab="Air temperature (deg C)", 
     xlab="Time of Day", 
     main="flux.dfy 2019")

par(mfrow=(c(2,1)))                       # the c(2,1) refers to 2 rows and 1 column, so it plots them below each other
plot(flux.df$Fg~flux.df$timestamp, 
     type='l', 
     col="blue", 
     ylim=c(-20,30), 
     ylab="Ground heat flux (W m^2)", 
     xlab="Time of Day", 
     main="flux.dfy 2013")
plot(flux.df$Ta~flux.df$timestamp, 
     type='l', 
     col="red", 
     ylim=c(-20,30), 
     ylab="Air temperature (deg C)", 
     xlab="Time of Day", 
     main="flux.dfy 2019")

# 4 plots in a 2 by 2 arrangement:
par(mfrow=(c(2,2))) 
boxplot(flux.df$Fsd,
        ylab="Shortwave radiation down (W/m2)", 
        ylim=c(-20,600))
boxplot(flux.df$Fsu,
        ylab="Shortwave radiation up (W/m2)", 
        ylim=c(-20,600))
boxplot(flux.df$Fld, 
        ylab="Longwave radiation down (W/m2)", 
        ylim=c(-20,600))
boxplot(flux.df$Flu, 
        ylab="Longwave radiation up(W/m2)", 
        ylim=c(-20,600))

# always remember to set it back to c(1,1) if you just want 1 plot again!
par(mfrow=(c(1,1)))    # reset your plotting paramters

#barplots
Fsd<-sum(flux.df$Fsd)
Fsu<-sum(flux.df$Fsu)
Fld<-sum(flux.df$Fld)
Flu<-sum(flux.df$Flu)

radiation_components<- c(Fsd,Fsu,Fld,Flu)
barplot (radiation_components,
         names.arg=c("Fsd","Fsu","Fld","Flu"))       # assign names to bars in barplot
#########################################################
#### INTRODUCTION TO BASIC LINEAR REGRESSION ############
#########################################################


#linear regression
fit_BR_flux.df<-lm(flux.df$Fh~flux.df$Fe)                 # declare a name for your regression
summary(fit_flux.df)                           # summarize your fit

# extract the coefficients of the fit
R <- summary(lm(fit_BR_flux.df))$adj.r.squared         # extract the adjusted R^2 (R)
p <- summary(lm(fit_BR_flux.df))$coefficients[2,4]     # extract the p-value (p)
m <- summary(lm(fit_BR_flux.df))$coefficients [2,1]    # extract the slope (m)
c <- summary(lm(fit_BR_flux.df))$coefficients [1,1]    # extract the intercept/offset (c)


##########################################################################
#creating a bar plot from summed fluxes
sum_Fh<-sum(flux.df$Fh)
sum_Fe<-sum(flux.df$Fe)
sum_Fg<-sum(flux.df$Fg)
sum_energy<-c(sum_Fh,sum_Fe,sum_Fg)
barplot(sum_energy,
        ylim=c(-20,6000),
        names.arg=c("Fh","Fe","Fg"))


