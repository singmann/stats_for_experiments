
# this file assumes that the data and demographics are stored in a separate 'data' directory stored in the same directory that contains the Rmd file


# read in the csv data file
mydata <- read.csv("../data/infostudy_data.csv", fileEncoding="UTF-8-BOM", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE)

# recode -99 to be missing
mydata[mydata==-99] <- NA


##### Compute relevant values to be reported in the Participants subsection #####

# calculate frequencies for gender
desc_gender <- table(mydata$gender_coded, useNA = "always")
desc_gender <- data.frame(desc_gender)
desc_gender$percent <- round(100*desc_gender$Freq/sum(desc_gender$Freq),digits=0)
desc_gender$label <- c("female","male","Declined")

# calculate frequencies for anticipated year of graduation
desc_year <- table(mydata$year, useNA = "always")
desc_year <- data.frame(desc_year)
desc_year$percent <- round(100*desc_year$Freq/sum(desc_year$Freq),digits=0)
desc_year$label <- c("Senior","Junior","Sophomore","Freshman",NA)

# calculate frequencies for race/ethnicity and make it a data frame
desc_race <- table(mydata$race_ethnicity_coded, useNA = "always")
desc_race <- data.frame(desc_race)
desc_race$percent <- round((100*desc_race$Freq)/sum(desc_race$Freq),digits=0)

# create a column containing a label for each race/ethnicity number
desc_race$label[1] <- "African American or Black"
desc_race$label[2] <- "Asian"
desc_race$label[3] <- "Multiracial"
desc_race$label[4] <- "Caucasian or White"
desc_race$label[5] <- "Hispanic or Latino"
desc_race$label[6] <- "Declined"

