
# Set working directory
# setwd()


new_data <- read.table('nlsy.dat', sep=' ')
names(new_data) <- c('H0012400',
  'H0012500',
  'H0022300',
  'H0022500',
  'R0000100',
  'R0009100',
  'R0173600',
  'R0214700',
  'R0214800',
  'R0216400',
  'R0217900',
  'R0402800',
  'R7090700',
  'T4120500')


# Handle missing values

  new_data[new_data == -1] = NA  # Refused 
  new_data[new_data == -2] = NA  # Dont know 
  new_data[new_data == -3] = NA  # Invalid missing 
  new_data[new_data == -4] = NA  # Valid missing 
  new_data[new_data == -5] = NA  # Non-interview 


# If there are values not categorized they will be represented as NA

vallabels = function(data) {
  data$H0012400 <- factor(data$H0012400, 
    levels=c(0.0,1.0), 
    labels=c("NO",
      "YES"))
  data$H0012500 <- factor(data$H0012500, 
    levels=c(1.0,2.0,3.0,4.0,5.0), 
    labels=c("Excellent",
      "Very Good",
      "Good",
      "Fair",
      "Poor"))
  data$H0022300[8.0 <= data$H0022300 & data$H0022300 <= 14.0] <- 8.0
  data$H0022300[15.0 <= data$H0022300 & data$H0022300 <= 100.0] <- 15.0
  data$H0022300 <- factor(data$H0022300, 
    levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,15.0), 
    labels=c("0",
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8 TO 14: 8 to 14",
      "15 TO 100: 15+"))
  data$H0022500[8.0 <= data$H0022500 & data$H0022500 <= 14.0] <- 8.0
  data$H0022500[15.0 <= data$H0022500 & data$H0022500 <= 100.0] <- 15.0
  data$H0022500 <- factor(data$H0022500, 
    levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,15.0), 
    labels=c("0",
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8 TO 14: 8 to 14",
      "15 TO 100: 15+"))
  data$R0009100[16.0 <= data$R0009100 & data$R0009100 <= 99999.0] <- 16.0
  data$R0009100 <- factor(data$R0009100, 
    levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0), 
    labels=c("0",
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "10",
      "11",
      "12",
      "13",
      "14",
      "15",
      "16 TO 99999: 16+"))
  data$R0173600 <- factor(data$R0173600, 
    levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0), 
    labels=c("CROSS MALE WHITE",
      "CROSS MALE WH. POOR",
      "CROSS MALE BLACK",
      "CROSS MALE HISPANIC",
      "CROSS FEMALE WHITE",
      "CROSS FEMALE WH POOR",
      "CROSS FEMALE BLACK",
      "CROSS FEMALE HISPANIC",
      "SUP MALE WH POOR",
      "SUP MALE BLACK",
      "SUP MALE HISPANIC",
      "SUP FEM WH POOR",
      "SUP FEMALE BLACK",
      "SUP FEMALE HISPANIC",
      "MIL MALE WHITE",
      "MIL MALE BLACK",
      "MIL MALE HISPANIC",
      "MIL FEMALE WHITE",
      "MIL FEMALE BLACK",
      "MIL FEMALE HISPANIC"))
  data$R0214700 <- factor(data$R0214700, 
    levels=c(1.0,2.0,3.0), 
    labels=c("HISPANIC",
      "BLACK",
      "NON-BLACK, NON-HISPANIC"))
  data$R0214800 <- factor(data$R0214800, 
    levels=c(1.0,2.0), 
    labels=c("MALE",
      "FEMALE"))
  data$R0216400 <- factor(data$R0216400, 
    levels=c(1.0,2.0,3.0,4.0), 
    labels=c("NORTHEAST",
      "NORTH CENTRAL",
      "SOUTH",
      "WEST"))
  data$R0217900[1.0 <= data$R0217900 & data$R0217900 <= 999.0] <- 1.0
  data$R0217900[1000.0 <= data$R0217900 & data$R0217900 <= 1999.0] <- 1000.0
  data$R0217900[2000.0 <= data$R0217900 & data$R0217900 <= 2999.0] <- 2000.0
  data$R0217900[3000.0 <= data$R0217900 & data$R0217900 <= 3999.0] <- 3000.0
  data$R0217900[4000.0 <= data$R0217900 & data$R0217900 <= 4999.0] <- 4000.0
  data$R0217900[5000.0 <= data$R0217900 & data$R0217900 <= 5999.0] <- 5000.0
  data$R0217900[6000.0 <= data$R0217900 & data$R0217900 <= 6999.0] <- 6000.0
  data$R0217900[7000.0 <= data$R0217900 & data$R0217900 <= 7999.0] <- 7000.0
  data$R0217900[8000.0 <= data$R0217900 & data$R0217900 <= 8999.0] <- 8000.0
  data$R0217900[9000.0 <= data$R0217900 & data$R0217900 <= 9999.0] <- 9000.0
  data$R0217900[10000.0 <= data$R0217900 & data$R0217900 <= 14999.0] <- 10000.0
  data$R0217900[15000.0 <= data$R0217900 & data$R0217900 <= 19999.0] <- 15000.0
  data$R0217900[20000.0 <= data$R0217900 & data$R0217900 <= 24999.0] <- 20000.0
  data$R0217900[25000.0 <= data$R0217900 & data$R0217900 <= 49999.0] <- 25000.0
  data$R0217900[50000.0 <= data$R0217900 & data$R0217900 <= 9999999.0] <- 50000.0
  data$R0217900 <- factor(data$R0217900, 
    levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0,10000.0,15000.0,20000.0,25000.0,50000.0), 
    labels=c("0",
      "1 TO 999",
      "1000 TO 1999",
      "2000 TO 2999",
      "3000 TO 3999",
      "4000 TO 4999",
      "5000 TO 5999",
      "6000 TO 6999",
      "7000 TO 7999",
      "8000 TO 8999",
      "9000 TO 9999",
      "10000 TO 14999",
      "15000 TO 19999",
      "20000 TO 24999",
      "25000 TO 49999",
      "50000 TO 9999999: 50000+"))
  data$R0402800 <- factor(data$R0402800, 
    levels=c(1.0,2.0,3.0,4.0,5.0,6.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0), 
    labels=c("ABOARD SHIP, BARRACKS",
      "BACHELOR, OFFICER QUARTERS",
      "DORM, FRATERNITY, SORORITY",
      "HOSPITAL",
      "JAIL",
      "OTHER TEMPORARY QUARTERS",
      "OWN DWELLING UNIT",
      "ON-BASE MIL FAM HOUSING",
      "OFF-BASE MIL FAM HOUSING",
      "ORPHANAGE",
      "RELIGIOUS INSTITUTION",
      "OTHER INDIVIDUAL QUARTERS",
      "PARENTAL",
      "HHI CONDUCTED WITH PARENT",
      "R IN PARENTAL HOUSEHOLD"))
  data$R7090700 <- factor(data$R7090700, 
    levels=c(1.0,2.0,3.0,4.0,5.0,6.0,11.0,12.0,13.0,15.0,16.0,19.0), 
    labels=c("OPEN BAY OR TROOP BARRACKS, ABOARD SHIP",
      "BACHELOR ENLISTED OR OFFICER QUARTERS",
      "DORMITORY, FRATERNITY OR SORORITY",
      "HOSPITAL",
      "JAIL",
      "OTHER TEMPORARY INDIVIDUAL QUARTERS (SPECIFY)",
      "OWN DWELLING UNIT",
      "ON-BASE MILITARY FAMILY HOUSING",
      "OFF-BASE MILITARY FAMILY HOUSING",
      "CONVENT, MONASTERY, OTHER RELIGIOUS INSTITUTE",
      "OTHER INDIVIDUAL QUARTERS (SPECIFY)",
      "RESPONDENT IN PARENT HOUSEHOLD"))
  data$T4120500[1.0 <= data$T4120500 & data$T4120500 <= 9.0] <- 1.0
  data$T4120500[10.0 <= data$T4120500 & data$T4120500 <= 14.0] <- 10.0
  data$T4120500[15.0 <= data$T4120500 & data$T4120500 <= 17.0] <- 15.0
  data$T4120500[18.0 <= data$T4120500 & data$T4120500 <= 19.0] <- 18.0
  data$T4120500[20.0 <= data$T4120500 & data$T4120500 <= 24.0] <- 20.0
  data$T4120500[25.0 <= data$T4120500 & data$T4120500 <= 29.0] <- 25.0
  data$T4120500[30.0 <= data$T4120500 & data$T4120500 <= 34.0] <- 30.0
  data$T4120500[35.0 <= data$T4120500 & data$T4120500 <= 39.0] <- 35.0
  data$T4120500[40.0 <= data$T4120500 & data$T4120500 <= 44.0] <- 40.0
  data$T4120500[45.0 <= data$T4120500 & data$T4120500 <= 60.0] <- 45.0
  data$T4120500 <- factor(data$T4120500, 
    levels=c(-998.0,-997.0,-996.0,1.0,10.0,15.0,18.0,20.0,25.0,30.0,35.0,40.0,45.0), 
    labels=c("-998: NO 1ST BIRTH HAS OCCURRED",
      "-997: NO 2ND BIRTH HAS OCCURRED",
      "-996: NO 3RD BIRTH HAS OCCURRED",
      "1 TO 9: Under age 10",
      "10 TO 14",
      "15 TO 17",
      "18 TO 19",
      "20 TO 24",
      "25 TO 29",
      "30 TO 34",
      "35 TO 39",
      "40 TO 44",
      "45 TO 60: 45 or over"))
  return(data)
}

varlabels <- c("CCR-EYE GLASSES OR CTCT LENSES? XRND",
  "CCR-QUAL EYESIGHT (GLASSES/CNTCTS) XRND",
  "HOW MUCH SLEEP R GETS AT NIGHT ON WEEKDAYS-HRS XRND",
  "HOW MUCH SLEEP R GETS AT NIGHT ON WEEKENDS-HRS XRND",
  "ID# (1-12686) 79",
  "# OF SIBS 79",
  "SAMPLE ID  79 INT",
  "RACL/ETHNIC COHORT /SCRNR 79",
  "SEX OF R 79",
  "REGION OF CURRENT RESIDENCE 79",
  "TOT NET FAMILY INC P-C YR 79",
  "TYPE OF RESIDENCE R IS LIVING 80",
  "TYPE OF RESIDENCE R IS LIVING IN 2002",
  "AGE OF R AT 1ST BIRTH AS OF 12 INT 2012"
)


# Use qnames rather than rnums

qnames = function(data) {
  names(data) <- c("H40-CHRC-15_XRND",
    "H40-CHRC-16_XRND",
    "H50SLP-1_XRND",
    "H50SLP-2_XRND",
    "CASEID_1979",
    "FAM-28A_1979",
    "SAMPLE_ID_1979",
    "SAMPLE_RACE_78SCRN",
    "SAMPLE_SEX_1979",
    "REGION_1979",
    "TNFI_TRUNC_1979",
    "HH1-1_1980",
    "HH1-1_2002",
    "AGE1B12_2012")
  return(data)
}


#********************************************************************************************************

# Remove the '#' before the following line to create a data file called "categories" with value labels. 
#categories <- vallabels(new_data)

# Remove the '#' before the following lines to rename variables using Qnames instead of Reference Numbers
#new_data <- qnames(new_data)
#categories <- qnames(categories)

# Produce summaries for the raw (uncategorized) data file
summary(new_data)

# Remove the '#' before the following lines to produce summaries for the "categories" data file.
#categories <- vallabels(new_data)
#summary(categories)

#************************************************************************************************************

