# Dimensions of Adverse Childhood Experiences in the Millennium Cohort Study

``` r
library(foreign) # for loading data
library(Hmisc) # for data manipulation
library(psych) # for data manipulation
library(plyr) # for data manipulation
library(dplyr) # for data manipulation
library(tidyr) # for data manipulation
library(car) # for data manipulation
library(stats) # for data manipulation
library(tibble) # for data manipulation

library(reshape2) # for reshaping data
library(ggplot2) # for plotting graphs

library(kableExtra) # for creating tables
library(stargazer) # for creating tables
```

# 0. Merge MCS dataset

To begin, we will merge the measures of adverse childhood experiences
(ACEs) from their respective MCS datasets (Sweeps 1, 2, 3, 4, 5, 6: ages
9 months, 3, 5, 7, 11, and 14 years) into a single dataset.

``` r
# Set shortcuts for working directory (fill in paths below according to your working directory)
MCS1 <- "~/Desktop/MCS Datafiles/MCS1/spss/spss25"
MCS2 <- "~/Desktop/MCS Datafiles/MCS2/spss/spss25"
MCS3 <- "~/Desktop/MCS Datafiles/MCS3/spss/spss25"
MCS4 <- "~/Desktop/MCS Datafiles/MCS4/spss/spss25"
MCS5 <- "~/Desktop/MCS Datafiles/MCS5/spss/spss25"
MCS6 <- "~/Desktop/MCS Datafiles/MCS6/spss/spss25"
MCS7 <- "~/Desktop/MCS Datafiles/MCS7/spss/spss25"

################### MEASURES FROM SWEEP 1 ###################

## ================== MCS1 CM DEMOGRAPHICS ===================================
# Get cohort member sex variable from hhgrid dataset
mcs1_hhgrid <- read.spss(paste0(MCS1, "/", "mcs1_hhgrid.sav"), to.data.frame = TRUE)

# Check whether IDs are repeated (yes)
n_occur <- data.frame(table(mcs1_hhgrid$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs1_hhgrid$MCSID) # family identifier 
table(mcs1_hhgrid$ACNUM00) # cohort member number within family

# Recode cohort member number variable to numeric
levels(mcs1_hhgrid$ACNUM00) # factor with 3 levels
mcs1_hhgrid$ACNUM00 <- recode(mcs1_hhgrid$ACNUM00, "'1st Cohort Member of the family'=1; '2nd Cohort Member of the family'=2; '3rd Cohort Member of the family'=3")
levels(mcs1_hhgrid$ACNUM00) # numeric

# Remove white spaces from ID variable
mcs1_hhgrid$MCSID = as.factor(trimws(mcs1_hhgrid$MCSID))
str(mcs1_hhgrid$MCSID)

# Create new dataset with relevant variables
mcs1_hhgrid_subset <- subset(mcs1_hhgrid, ACNUM00==1, 
                                          select=c(MCSID, # IDs
                                                   AHCSEX00)) # cohort member sex
colnames(mcs1_hhgrid_subset)

table(mcs1_hhgrid_subset$AHCSEX00) # check gender distribution
                                
dim(mcs1_hhgrid_subset) # Check number of rows to check that each family is only represented once
dim(mcs1_hhgrid) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs1_hhgrid)

# Get cohort member ethnicity, birthweight, and gestation variables from cm derived dataset
mcs1_cm_derived <- read.spss(paste0(MCS1, "/", "mcs1_cm_derived.sav"), to.data.frame = TRUE)

# Check whether IDs are repeated (yes)
n_occur <- data.frame(table(mcs1_cm_derived$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs1_cm_derived$MCSID) # family identifier 
table(mcs1_cm_derived$ACNUM00) # cohort member number within family

# Recode cohort member number variable to numeric
levels(mcs1_cm_derived$ACNUM00) # factor with 3 levels
mcs1_cm_derived$ACNUM00 <- recode(mcs1_cm_derived$ACNUM00, "'1st Cohort Member of the family'=1; '2nd Cohort Member of the family'=2; '3rd Cohort Member of the family'=3")
levels(mcs1_cm_derived$ACNUM00) # numeric

# Remove white spaces from ID variable
mcs1_cm_derived$MCSID = as.factor(trimws(mcs1_cm_derived$MCSID))
str(mcs1_cm_derived$MCSID)

# Create new dataset with relevant variables
mcs1_cm_derived_subset <- subset(mcs1_cm_derived, ACNUM00==1, 
                                          select=c(MCSID, # IDs
                                                   ADBWGT00, # birthweight in kilos
                                                   ADGEST00, # gestation time in days
                                                   ADC06E00)) # cohort member ethnicity
colnames(mcs1_cm_derived_subset)

table(mcs1_cm_derived_subset$ADC06E00) # check ethnicity distribution
                                
dim(mcs1_cm_derived_subset) # Check number of rows to check that each family is only represented once
dim(mcs1_cm_derived) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs1_cm_derived)

## ================== MCS1 PARENT INTERVIEW ===================================
mcs1_parent_interview <- read.spss(paste0(MCS1, "/", "mcs1_parent_interview.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs1_parent_interview$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs1_parent_interview$MCSID) # family identifier 
table(mcs1_parent_interview$APNUM00) # person number within an MCS family

# Remove white spaces from ID variable
mcs1_parent_interview$MCSID = as.factor(trimws(mcs1_parent_interview$MCSID))
str(mcs1_parent_interview$MCSID)

# Create new dataset with relevant variables
mcs1_parent_interview_subset <- subset(mcs1_parent_interview, APNUM00==1,
                                       select=c(MCSID, # IDs 
                                                APACQU00, # highest maternal academic qualification
                                                ADROOW00, # tenure of current home (owns/rents)
                                                APTIRE00, APDEPR00, APWORR00, APRAGE00, # parental psychosocial distress (Rutter)
                                                APSCAR00, APUPSE00, APKEYD00, APNERV00, APHERA00, # parental psychosocial distress (Rutter)
                                                APALDR00, # frequency of alcohol consumption
                                                APFCIN00, # marital status 
                                                APHOSA00, # satisfaction with home
                                                APAREA00, # satisfaction with area
                                                APARNN00, # noisy neighbours
                                                APARRU00, # how common are rubbish/litter in area
                                                APARVD00, # how common are vandalism in area
                                                APARRC00, # how common are racist insults/attacks in area
                                                APTRAN00, # poor public transport
                                                APSHOP00, # food shops/supermarkets in easy access
                                                APARPG00, # pollution/grime/environmental problems
                                                APPLSA00, # any places where children can play safely
                                                APRESE00, APREIS00, APRELO00, APREJO00, # parental relationship (Golombok)
                                                APREWA00, APRESN00, APMAUP00, APHARE00, # parental relationship (Golombok)
                                                APFORC00)) # whether partner ever used force in relationship (Golombok)
colnames(mcs1_parent_interview_subset)

dim(mcs1_parent_interview_subset) # Check number of rows to check that each family is only represented once
dim(mcs1_parent_interview) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs1_parent_interview)

## ================== MCS1 PARENT DERIVED INTERVIEW ===================================
mcs1_parent_derived_interview <- read.spss(paste0(MCS1, "/", "mcs1_parent_derived.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs1_parent_derived_interview$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs1_parent_derived_interview$MCSID) # family identifier 
table(mcs1_parent_derived_interview$APNUM00) # person number within an MCS family

# Remove white spaces from ID variable
mcs1_parent_derived_interview$MCSID = as.factor(trimws(mcs1_parent_derived_interview$MCSID))
str(mcs1_parent_derived_interview$MCSID)

# Create new dataset with relevant variables
mcs1_parent_derived_interview_subset <- subset(mcs1_parent_derived_interview, APNUM00==1,
                                       select=c(MCSID, # IDs 
                                                ADD05C00, # socioeconomic NS-SEC 5 classes 
                                                ADWGTK00, # maternal weight in kilos
                                                ADDAGB00)) # mother's age at birth of CM   
colnames(mcs1_parent_derived_interview_subset)

dim(mcs1_parent_derived_interview_subset) # Check number of rows to check that each family is only represented once
dim(mcs1_parent_derived_interview) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs1_parent_derived_interview)

## ================== MCS1 FAMILY DERIVED VARIABLES ===================================
mcs1_family_derived <- read.spss(paste0(MCS1, "/", "mcs1_family_derived.sav"), to.data.frame = TRUE)

# Check whether data is in long format (no)
n_occur <- data.frame(table(mcs1_family_derived$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs1_family_derived$MCSID) # family identifier

# Remove white spaces from ID variable
mcs1_family_derived$MCSID = as.factor(trimws(mcs1_family_derived$MCSID))
str(mcs1_family_derived$MCSID)

# Create new dataset with relevant variables
mcs1_family_derived_subset <- dplyr::select(mcs1_family_derived, 
                                      MCSID, # IDs
                                      ADMBMI00, # maternal BMI at interview (CM age 9 months)
                                      AOECDUK0) # household income (OECD weighted quintiles UK analysis)
colnames(mcs1_family_derived_subset)   

dim(mcs1_family_derived_subset)  

# Remove original dataset
rm(mcs1_family_derived)

################### MEASURES FROM SWEEP 2 ###################

## ================== MCS2 CM DERIVED INTERVIEW ===================================
mcs2_cm_derived <- read.spss(paste0(MCS2, "/", "mcs2_cm_derived.sav"), to.data.frame = TRUE)

# Check whether IDs are repeated (yes)
n_occur <- data.frame(table(mcs2_cm_derived$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs2_cm_derived$MCSID) # family identifier 
table(mcs2_cm_derived$BCNUM00) # cohort member number within family

# Recode cohort member number variable to numeric
levels(mcs2_cm_derived$BCNUM00) # factor with 3 levels
mcs2_cm_derived$BCNUM00 <- recode(mcs2_cm_derived$BCNUM00, "'1st Cohort Member of the family'=1; '2nd Cohort Member of the family'=2; '3rd Cohort Member of the family'=3")
levels(mcs2_cm_derived$BCNUM00) # numeric

# Remove white spaces from ID variable
mcs2_cm_derived$MCSID = as.factor(trimws(mcs2_cm_derived$MCSID))
str(mcs2_cm_derived$MCSID)

# Create new dataset with relevant variables
mcs2_cm_derived_subset <- subset(mcs2_cm_derived, BCNUM00==1, 
                                          select=c(MCSID, # IDs
                                                   BEBDTOT)) # SDQ Total Difficulties MCS2 
colnames(mcs2_cm_derived_subset)
                                
dim(mcs2_cm_derived_subset) # Check number of rows to check that each family is only represented once
dim(mcs2_cm_derived) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs2_cm_derived)

## ================== MCS2 PARENT INTERVIEW ===================================
mcs2_parent_interview <- read.spss(paste0(MCS2, "/", "mcs2_parent_interview.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs2_parent_interview$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs2_parent_interview$MCSID) #family identifier
table(mcs2_parent_interview$BPNUM00) # person number within an MCS family

# Remove white spaces from ID variable
mcs2_parent_interview$MCSID = as.factor(trimws(mcs2_parent_interview$MCSID))
str(mcs2_parent_interview$MCSID)

# Create new dataset with relevant variables
mcs2_parent_interview_subset <- subset(mcs2_parent_interview, BPNUM00==1,
                                       select=c(MCSID, # IDs
                                                BPPHDE00, BPPHHO00, BPPHRF00, BPPHEE00, BPPHWO00, BPPHNE00, # parental mental health (Kessler)
                                                BPALDR00, # frequency of alcohol consumption
                                                BPDRUG00, # used recreational drugs
                                                BPFCIN00, # marital status
                                                BPRESE00, BPRELS00, BPRELO00, BPRESN00, # parental relationship (Golombok)
                                                BPREIS00, BPCOLT00, BPHARE00, # parental relationship (Golombok)
                                                BPFORC00, # whether partner ever used force in relationship (Golombok)
                                                BPDIIG00, BPDISM00, BPDISH00, BPDIBN00, # parental discipline (Straus)
                                                BPDITR00, BPDITE00, BPDIBR00, # parental discipline (Straus)
                                                BPARGD00, # is this a good area to bring up a child
                                                BPARAR00)) # how safe do you feel about the area you live in
colnames(mcs2_parent_interview_subset)

dim(mcs2_parent_interview_subset) # Check number of rows to check that each family is only represented once
dim(mcs2_parent_interview) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs2_parent_interview)

## ================== MCS2 PARENT CM INTERVIEW ===================================
mcs2_parent_cm_interview <- read.spss(paste0(MCS2, "/", "mcs2_parent_cm_interview.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs2_parent_cm_interview$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs2_parent_cm_interview$MCSID) # family identifier
table(mcs2_parent_cm_interview$BCNUM00) # cohort member number within family 
table(mcs2_parent_cm_interview$BPNUM00) # person number within an MCS family

# Recode cohort member number variable to numeric
levels(mcs2_parent_cm_interview$BCNUM00) # factor with 3 levels
mcs2_parent_cm_interview$BCNUM00 <- recode(mcs2_parent_cm_interview$BCNUM00, "'1st Cohort Member of the family'=1; '2nd Cohort Member of the family'=2; '3rd Cohort Member of the family'=3")
levels(mcs2_parent_cm_interview$BCNUM00) # numeric

# Remove white spaces from ID variable
mcs2_parent_cm_interview$MCSID = as.factor(trimws(mcs2_parent_cm_interview$MCSID))
str(mcs2_parent_cm_interview$MCSID)

# Create new dataset with relevant variables
mcs2_parent_cm_interview_subset <- subset(mcs2_parent_cm_interview, BCNUM00==1 & BPNUM00==1,
                                          select=c(MCSID, # IDs
                                                   BPOFRE00, BPREEL00, BPREOF00, # how often child is read to
                                                   BPTOLI00, BPOFLI00, # how often child is taken to library 
                                                   BPSDPA00, # anyone teaches child sport
                                                   BPALPH00, BPOFAB00, # how often child is helped with alphabet
                                                   BPNUMB00, BPOFCO00, # how often child is taught counting
                                                   BPSONG00, BPOFSO00, # how often child is taught songs
                                                   BPDRAW00, BPPAMA00, # how often child paints/draws at home
                                                   BPEATW00, # child eaten with family past week   
                                                   BPBIRT00, # something special for child's third birthday
                                                   BPYOCH00, # been visited by friends with young children 
                                                   BPSDPB00, # CM picked on or bullied by other children
                                                   BPCLSI00)) # child has any longstanding health conditions
colnames(mcs2_parent_cm_interview_subset)

dim(mcs2_parent_cm_interview_subset) # Check number of rows to check that each family is only represented once
dim(mcs2_parent_cm_interview) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs2_parent_cm_interview)

## ================== MCS2 FAMILY DERIVED VARIABLES ===================================
mcs2_family_derived <- read.spss(paste0(MCS2, "/", "mcs2_family_derived.sav"), to.data.frame = TRUE)

# Check whether data is in long format (no)
n_occur <- data.frame(table(mcs2_family_derived$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs2_family_derived$MCSID) # family identifier

# Remove white spaces from ID variable
mcs2_family_derived$MCSID = as.factor(trimws(mcs2_family_derived$MCSID))
str(mcs2_family_derived$MCSID)

# Create new dataset with relevant variables
mcs2_family_derived_subset <- dplyr::select(mcs2_family_derived, 
                                      MCSID, # IDs
                                      BOECDUK0) # household income (OECD weighted quintiles UK analysis)
dim(mcs2_family_derived_subset)  
colnames(mcs2_family_derived_subset)   

# Remove original dataset
rm(mcs2_family_derived)

## ================== MCS2 CM COGNITIVE ASSESSMENT ===================================
mcs2_cm_cognitive_assessment <- read.spss(paste0(MCS2, "/", "mcs2_cm_cognitive_assessment.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs2_cm_cognitive_assessment$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs2_cm_cognitive_assessment$MCSID) # family identifier
table(mcs2_cm_cognitive_assessment$BCNUM00) # cohort member number within family 

# Recode cohort member number variable to numeric
levels(mcs2_cm_cognitive_assessment$BCNUM00) # factor with 3 levels
mcs2_cm_cognitive_assessment$BCNUM00 <- recode(mcs2_cm_cognitive_assessment$BCNUM00, "'1st Cohort Member of the family'=1; '2nd Cohort Member of the family'=2; '3rd Cohort Member of the family'=3")
levels(mcs2_cm_cognitive_assessment$BCNUM00) # numeric

# Remove white spaces from ID variable
mcs2_cm_cognitive_assessment$MCSID = as.factor(trimws(mcs2_cm_cognitive_assessment$MCSID))
str(mcs2_cm_cognitive_assessment$MCSID)

# Create new dataset with relevant variables
mcs2_cm_cognitive_assessment_subset <- subset(mcs2_cm_cognitive_assessment, BCNUM00==1,
                                              select=c(MCSID, 
                                                       BCENVI00, BCTOYS00, BCSEEC00, # home environment (HOME-SF)
                                                       BCCOMF00, BCDARK00, BCRCLE00, BCUNCL00, # home environment (HOME-SF)
                                                       BCSPEA00, BCMCON00, BCANSW00, # home environment (HOME-SF)
                                                       BCPRAI00, BCKISS00, BCINTI00, # home environment (HOME-SF)
                                                       BCSCOL00, BCPHYS00, BCSLAP00)) # home environment (HOME-SF)
colnames(mcs2_cm_cognitive_assessment_subset)

dim(mcs2_cm_cognitive_assessment_subset) # Check number of rows to check that each family is only represented once
dim(mcs2_cm_cognitive_assessment) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs2_cm_cognitive_assessment)

################### MEASURES FROM SWEEP 3 ###################

## ================== MCS3 CM DERIVED INTERVIEW ===================================
mcs3_cm_derived <- read.spss(paste0(MCS3, "/", "mcs3_cm_derived.sav"), to.data.frame = TRUE)

# Check whether IDs are repeated (yes)
n_occur <- data.frame(table(mcs3_cm_derived$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs3_cm_derived$MCSID) # family identifier 
table(mcs3_cm_derived$CCNUM00) # cohort member number within family

# Recode cohort member number variable to numeric
levels(mcs3_cm_derived$CCNUM00) # factor with 3 levels
mcs3_cm_derived$CCNUM00 <- recode(mcs3_cm_derived$CCNUM00, "'1st Cohort Member of the family'=1; '2nd Cohort Member of the family'=2; '3rd Cohort Member of the family'=3")
levels(mcs3_cm_derived$CCNUM00) # numeric

# Remove white spaces from ID variable
mcs3_cm_derived$MCSID = as.factor(trimws(mcs3_cm_derived$MCSID))
str(mcs3_cm_derived$MCSID)

# Create new dataset with relevant variables
mcs3_cm_derived_subset <- subset(mcs3_cm_derived, CCNUM00==1, 
                                          select=c(MCSID, # IDs
                                                   CEBDTOT)) # SDQ Total Difficulties MCS3
colnames(mcs3_cm_derived_subset)
                                
dim(mcs3_cm_derived_subset) # Check number of rows to check that each family is only represented once
dim(mcs3_cm_derived) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs3_cm_derived)

## =================== MCS3 PARENT INTERVIEW ===================================
mcs3_parent_interview <- read.spss(paste0(MCS3, "/", "mcs3_parent_interview.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs3_parent_interview$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs3_parent_interview$MCSID) # family identifier 
table(mcs3_parent_interview$CPNUM00) # person number within an MCS family

# Remove white spaces from ID variable
mcs3_parent_interview$MCSID = as.factor(trimws(mcs3_parent_interview$MCSID))
str(mcs3_parent_interview$MCSID)

# Create new dataset with relevant variables
mcs3_parent_interview_subset <- subset(mcs3_parent_interview, CPNUM00==1,
                                       select=c(MCSID, # IDs 
                                                CPSMKR00, # whether anyone smokes in the same room as CM  
                                                CPPHDE00, CPPHHO00, CPPHRF00, CPPHEE00, CPPHWO00, CPPHNE00, # parental mental health (Kessler)
                                                CPALDR00, # frequency of alcohol consumption
                                                CPDRUG00, # used recreational drugs
                                                CPFCIN00, # marital status 
                                                CPRESE00, CPRELS00, CPRELO00, CPRESN00, # parental relationship (Golombok)
                                                CPREIS00, CPCOLT00, CPHARE00, # parental relationship (Golombok)
                                                CPFORC00, # whether partner ever used force in relationship (Golombok)
                                                CPARGD00, # is this a good area to bring up a child
                                                CPARAR00)) # how safe do you feel about the area you live in
colnames(mcs3_parent_interview_subset)

dim(mcs3_parent_interview_subset) # Check number of rows to check that each family is only represented once
dim(mcs3_parent_interview) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs3_parent_interview)

## ================== MCS3 PARENT CM INTERVIEW ===================================
mcs3_parent_cm_interview <- read.spss(paste0(MCS3, "/", "mcs3_parent_cm_interview.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs3_parent_cm_interview$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs3_parent_cm_interview$MCSID) # family identifier
table(mcs3_parent_cm_interview$CCNUM00) # cohort member number within family 
table(mcs3_parent_cm_interview$CPNUM00) # person number within an MCS family

# Recode cohort member number variable to numeric
levels(mcs3_parent_cm_interview$CCNUM00) # factor with 3 levels
mcs3_parent_cm_interview$CCNUM00 <- recode(mcs3_parent_cm_interview$CCNUM00, "'1st Cohort Member of the family'=1; '2nd Cohort Member of the family'=2; '3rd Cohort Member of the family'=3")
levels(mcs3_parent_cm_interview$CCNUM00) # numeric

# Remove white spaces from ID variable
mcs3_parent_cm_interview$MCSID = as.factor(trimws(mcs3_parent_cm_interview$MCSID))
str(mcs3_parent_cm_interview$MCSID)

# Create new dataset with relevant variables
mcs3_parent_cm_interview_subset <- subset(mcs3_parent_cm_interview, CCNUM00==1 & CPNUM00==1,
                                          select=c(MCSID, # IDs
                                                   CPREOF00, CPSITS00, # how often child is read to
                                                   CPPLMU00, CPPAMA00, # how often musical activities & painting
                                                   CPACTI00, CPGAME00, CPWALK00, # how often plays games with child & playground
                                                   CPBEDR00,    # how often puts CM to bed
                                                   CPLOOK00,    # how often looks after CM on own 
                                                   CPDIIG00, CPDISM00, CPDISH00, CPDIBN00, # parental discipline (Straus)
                                                   CPDITR00, CPDITE00, CPDIBR00, # parental discipline (Straus)
                                                   CPSDPB00, # CM picked on or bullied by other children
                                                   CPCLSI00)) # child has any longstanding health conditions
colnames(mcs3_parent_cm_interview_subset)
        
dim(mcs3_parent_cm_interview_subset) # Check number of rows to check that each family is only represented once
dim(mcs3_parent_cm_interview) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs3_parent_cm_interview)

## ================== MCS3 FAMILY DERIVED VARIABLES ===================================
mcs3_family_derived <- read.spss(paste0(MCS3, "/", "mcs3_family_derived.sav"), to.data.frame = TRUE)

# Check whether data is in long format (no)
n_occur <- data.frame(table(mcs3_family_derived$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs3_family_derived$MCSID) # family identifier

# Remove white spaces from ID variable
mcs3_family_derived$MCSID = as.factor(trimws(mcs3_family_derived$MCSID))
str(mcs3_family_derived$MCSID)

# Create new dataset with relevant variables
mcs3_family_derived_subset <- dplyr::select(mcs3_family_derived, 
                                      MCSID, # IDs
                                      COECDUK0) # household income (OECD weighted quintiles UK analysis)
dim(mcs3_family_derived_subset)  
colnames(mcs3_family_derived_subset)   

# Remove original dataset
rm(mcs3_family_derived)

################### MEASURES FROM SWEEP 4 ###################

## ================== MCS4 CM DERIVED INTERVIEW ===================================
mcs4_cm_derived <- read.spss(paste0(MCS4, "/", "mcs4_cm_derived.sav"), to.data.frame = TRUE)

# Check whether IDs are repeated (yes)
n_occur <- data.frame(table(mcs4_cm_derived$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs4_cm_derived$MCSID) # family identifier 
table(mcs4_cm_derived$DCNUM00) # cohort member number within family

# Recode cohort member number variable to numeric
levels(mcs4_cm_derived$DCNUM00) # factor with 3 levels
mcs4_cm_derived$DCNUM00 <- recode(mcs4_cm_derived$DCNUM00, "'1st Cohort Member of the family'=1; '2nd Cohort Member of the family'=2; '3rd Cohort Member of the family'=3")
levels(mcs4_cm_derived$DCNUM00) # numeric

# Remove white spaces from ID variable
mcs4_cm_derived$MCSID = as.factor(trimws(mcs4_cm_derived$MCSID))
str(mcs4_cm_derived$MCSID)

# Create new dataset with relevant variables
mcs4_cm_derived_subset <- subset(mcs4_cm_derived, DCNUM00==1, 
                                          select=c(MCSID, # IDs
                                                   DDDEBDTOT)) # SDQ Total Difficulties MCS4
colnames(mcs4_cm_derived_subset)
                                
dim(mcs4_cm_derived_subset) # Check number of rows to check that each family is only represented once
dim(mcs4_cm_derived) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs4_cm_derived)

## =================== MCS4 PARENT INTERVIEW ===================================
mcs4_parent_interview <- read.spss(paste0(MCS4, "/", "mcs4_parent_interview.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs4_parent_interview$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs4_parent_interview$MCSID) # family identifier 
table(mcs4_parent_interview$DPNUM00) # person number within an MCS family

# Remove white spaces from ID variable
mcs4_parent_interview$MCSID = as.factor(trimws(mcs4_parent_interview$MCSID))
str(mcs4_parent_interview$MCSID)

# Create new dataset with relevant variables
mcs4_parent_interview_subset <- subset(mcs4_parent_interview, DPNUM00==1,
                                       select=c(MCSID, # IDs 
                                                DPPHDE00, DPPHHO00, DPPHRF00, DPPHEE00, DPPHWO00, DPPHNE00, # parental mental health (Kessler)
                                                DPALDR00, # frequency of alcohol consumption
                                                DPFCIN00, # marital status 
                                                DPREGN00, DPCOLT00, DPHARE00, # parental relationship (Golombok)
                                                DPFORC00)) # whether partner ever used force in relationship (Golombok)
colnames(mcs4_parent_interview_subset)

dim(mcs4_parent_interview_subset) # Check number of rows to check that each family is only represented once
dim(mcs4_parent_interview) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs4_parent_interview)

## ================== MCS4 PARENT CM INTERVIEW ===================================
mcs4_parent_cm_interview <- read.spss(paste0(MCS4, "/", "mcs4_parent_cm_interview.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs4_parent_cm_interview$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs4_parent_cm_interview$MCSID) # family identifier
table(mcs4_parent_cm_interview$DCNUM00) # cohort member number within family 
table(mcs4_parent_cm_interview$DPNUM00) # person number within an MCS family

# Recode cohort member number variable to numeric
levels(mcs4_parent_cm_interview$DCNUM00) # factor with 3 levels
mcs4_parent_cm_interview$DCNUM00 <- recode(mcs4_parent_cm_interview$DCNUM00, "'1st Cohort Member of the family'=1; '2nd Cohort Member of the family'=2; '3rd Cohort Member of the family'=3")
levels(mcs4_parent_cm_interview$DCNUM00) # numeric

# Remove white spaces from ID variable
mcs4_parent_cm_interview$MCSID = as.factor(trimws(mcs4_parent_cm_interview$MCSID))
str(mcs4_parent_cm_interview$MCSID)

# Create new dataset with relevant variables
mcs4_parent_cm_interview_subset <- subset(mcs4_parent_cm_interview, DCNUM00==1 & DPNUM00==1,
                                          select=c(MCSID, # IDs
                                                   DPREOF00, DPSITS00, # how often child is read to
                                                   DPPLMU00, DPPAMA00, # how often musical activities & painting
                                                   DPACTI00, DPGAME00, DPWALK00, # how often plays games with child & playground
                                                   DPBEDR00,    # how often puts CM to bed
                                                   DPLOOK00,    # how often looks after CM on own 
                                                   DPDIIG00, DPDISM00, DPDISH00, DPDIBN00, # parental discipline (Straus)
                                                   DPDITR00, DPDITE00, DPDIBR00, # parental discipline (Straus)
                                                   DPSDPB00, # CM picked on or bullied by other children
                                                   DPCLSI00)) # child has any longstanding health conditions
colnames(mcs4_parent_cm_interview_subset)
        
dim(mcs4_parent_cm_interview_subset) # Check number of rows to check that each family is only represented once
dim(mcs4_parent_cm_interview) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs4_parent_cm_interview)

## ================== MCS4 FAMILY DERIVED VARIABLES ===================================
mcs4_family_derived <- read.spss(paste0(MCS4, "/", "mcs4_family_derived.sav"), to.data.frame = TRUE)

# Check whether data is in long format (no)
n_occur <- data.frame(table(mcs4_family_derived$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs4_family_derived$MCSID) # family identifier

# Remove white spaces from ID variable
mcs4_family_derived$MCSID = as.factor(trimws(mcs4_family_derived$MCSID))
str(mcs4_family_derived$MCSID)

# Create new dataset with relevant variables
mcs4_family_derived_subset <- dplyr::select(mcs4_family_derived, 
                                      MCSID, # IDs
                                      DOECDUK0) # household income (OECD weighted quintiles UK analysis)
dim(mcs4_family_derived_subset)  
colnames(mcs4_family_derived_subset)   

# Remove original dataset
rm(mcs4_family_derived)

## ================== MCS4 CM INTERVIEW ===================================
mcs4_cm_interview <- read.spss(paste0(MCS4, "/", "mcs4_cm_interview.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs4_cm_interview$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs4_cm_interview$MCSID) # family identifier
table(mcs4_cm_interview$DCNUM00) # cohort member number within family 

# Recode cohort member number variable to numeric
levels(mcs4_cm_interview$DCNUM00) # factor with 3 levels
mcs4_cm_interview$DCNUM00 <- recode(mcs4_cm_interview$DCNUM00, "'1st Cohort Member of the family'=1; '2nd Cohort Member of the family'=2; '3rd Cohort Member of the family'=3")
levels(mcs4_cm_interview$DCNUM00) # numeric

# Remove white spaces from ID variable
mcs4_cm_interview$MCSID = as.factor(trimws(mcs4_cm_interview$MCSID))
str(mcs4_cm_interview$MCSID)

# Create new dataset with relevant variables
mcs4_cm_interview_subset <- subset(mcs4_cm_interview, DCNUM00==1,
                                          select=c(MCSID, # IDs
                                                   DCSC0036)) # how often other children bully CM
colnames(mcs4_cm_interview_subset)
        
dim(mcs4_cm_interview_subset) # Check number of rows to check that each family is only represented once
dim(mcs4_cm_interview) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs4_cm_interview)

## =================== MCS4 TEACHER CM SURVEY ===================================
mcs4_teacher_cm_survey <- read.spss(paste0(MCS4, "/", "mcs4_cm_teacher_survey.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs4_teacher_cm_survey$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs4_teacher_cm_survey$MCSID) # family identifier
table(mcs4_teacher_cm_survey$DCNUM00) # cohort member number within family 

# Recode cohort member number variable to numeric
levels(mcs4_teacher_cm_survey$DCNUM00) # factor with 3 levels
mcs4_teacher_cm_survey$DCNUM00 <- recode(mcs4_teacher_cm_survey$DCNUM00, "'1st Cohort Member of the family'=1; '2nd Cohort Member of the family'=2; '3rd Cohort Member of the family'=3")
levels(mcs4_teacher_cm_survey$DCNUM00) # numeric

# Remove white spaces from ID variable
mcs4_teacher_cm_survey$MCSID = as.factor(trimws(mcs4_teacher_cm_survey$MCSID))
str(mcs4_teacher_cm_survey$MCSID)

# Create new dataset with relevant variables
mcs4_teacher_cm_survey_subset <- subset(mcs4_teacher_cm_survey, DCNUM00==1,
                                          select=c(MCSID, # IDs
                                                   DQ2189)) # CM is picked on or bullied by other children 
colnames(mcs4_teacher_cm_survey_subset)
        
dim(mcs4_teacher_cm_survey_subset) # Check number of rows to check that each family is only represented once
dim(mcs4_teacher_cm_survey) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs4_teacher_cm_survey)

################### MEASURES FROM SWEEP 5 ###################

## =================== MCS5 PARENT INTERVIEW ===================================
mcs5_parent_interview <- read.spss(paste0(MCS5, "/", "mcs5_parent_interview.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs5_parent_interview$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs5_parent_interview$MCSID) # family identifier 
table(mcs5_parent_interview$EPNUM00) # person number within an MCS family

# Remove white spaces from ID variable
mcs5_parent_interview$MCSID = as.factor(trimws(mcs5_parent_interview$MCSID))
str(mcs5_parent_interview$MCSID)

# Create new dataset with relevant variables
mcs5_parent_interview_subset <- subset(mcs5_parent_interview, EPNUM00==1,
                                       select=c(MCSID, # IDs 
                                                EPPHDE00, EPPHHO00, EPPHRF00, EPPHEE00, EPPHWO00, EPPHNE00, # parental mental health (Kessler)
                                                EPALDR00, # frequency of alcohol consumption
                                                EPFCIN00, # marital status 
                                                EPHARE00, # parental relationship (Golombok)
                                                EPFORC00, # whether partner ever used force in relationship (Golombok)
                                                EPARGD00)) # is this a good area to bring up a child
colnames(mcs5_parent_interview_subset)

dim(mcs5_parent_interview_subset) # Check number of rows to check that each family is only represented once
dim(mcs5_parent_interview) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs5_parent_interview)

## ================== MCS5 PARENT CM INTERVIEW ===================================
mcs5_parent_cm_interview <- read.spss(paste0(MCS5, "/", "mcs5_parent_cm_interview.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs5_parent_cm_interview$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs5_parent_cm_interview$MCSID) # family identifier
table(mcs5_parent_cm_interview$ECNUM00) # cohort member number within family 
table(mcs5_parent_cm_interview$EPNUM00) # person number within an MCS family

# Recode cohort member number variable to numeric
levels(mcs5_parent_cm_interview$ECNUM00) # factor with 3 levels
mcs5_parent_cm_interview$ECNUM00 <- recode(mcs5_parent_cm_interview$ECNUM00, "'1st Cohort Member of the family'=1; '2nd Cohort Member of the family'=2; '3rd Cohort Member of the family'=3")
levels(mcs5_parent_cm_interview$ECNUM00) # numeric

# Remove white spaces from ID variable
mcs5_parent_cm_interview$MCSID = as.factor(trimws(mcs5_parent_cm_interview$MCSID))
str(mcs5_parent_cm_interview$MCSID)

# Create new dataset with relevant variables
mcs5_parent_cm_interview_subset <- subset(mcs5_parent_cm_interview, ECNUM00==1 & EPNUM00==1,
                                          select=c(MCSID, # IDs
                                                   EPACTI00, # how often do you play physically active games with CM?  
                                                   EPGAME00, # frequency play INDOOR games with child  
                                                   EPTAIM00, # frequency talks to CM about things important to them
                                                   EPLOOK00, # frequency looks after CM on own without main respondent being there 
                                                   EPCLSI00)) # child has any longstanding health conditions
colnames(mcs5_parent_cm_interview_subset)
        
dim(mcs5_parent_cm_interview_subset) # Check number of rows to check that each family is only represented once
dim(mcs5_parent_cm_interview) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs5_parent_cm_interview)

## ================== MCS5 FAMILY DERIVED VARIABLES ===================================
mcs5_family_derived <- read.spss(paste0(MCS5, "/", "mcs5_family_derived.sav"), to.data.frame = TRUE)

# Check whether data is in long format (no)
n_occur <- data.frame(table(mcs5_family_derived$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs5_family_derived$MCSID) # family identifier

# Remove white spaces from ID variable
mcs5_family_derived$MCSID = as.factor(trimws(mcs5_family_derived$MCSID))
str(mcs5_family_derived$MCSID)

# Create new dataset with relevant variables
mcs5_family_derived_subset <- dplyr::select(mcs5_family_derived, 
                                      MCSID, # IDs
                                      EOECDUK0) # household income (OECD weighted quintiles UK analysis)
dim(mcs5_family_derived_subset)  
colnames(mcs5_family_derived_subset)   

# Remove original dataset
rm(mcs5_family_derived)

## ================== MCS5 CM INTERVIEW ===================================
mcs5_cm_interview <- read.spss(paste0(MCS5, "/", "mcs5_cm_interview.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs5_cm_interview$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs5_cm_interview$MCSID) # family identifier
table(mcs5_cm_interview$ECNUM00) # cohort member number within family 

# Recode cohort member number variable to numeric
levels(mcs5_cm_interview$ECNUM00) # factor with 3 levels
mcs5_cm_interview$ECNUM00 <- recode(mcs5_cm_interview$ECNUM00, "'1st Cohort Member of the family'=1; '2nd Cohort Member of the family'=2; '3rd Cohort Member of the family'=3")
levels(mcs5_cm_interview$ECNUM00) # numeric

# Remove white spaces from ID variable
mcs5_cm_interview$MCSID = as.factor(trimws(mcs5_cm_interview$MCSID))
str(mcs5_cm_interview$MCSID)

# Create new dataset with relevant variables
mcs5_cm_interview_subset <- subset(mcs5_cm_interview, ECNUM00==1,
                                          select=c(MCSID, # IDs
                                                   ECQ23X00, # how safe is it to walk/play in this area 
                                                   EPSDPB00, # CM picked on or bullied by other children
                                                   ECQ56X00, # how often other children bully CM
                                                   ECQ54X00)) # how often brothers/sisters pick on CM
colnames(mcs5_cm_interview_subset)
        
dim(mcs5_cm_interview_subset) # Check number of rows to check that each family is only represented once
dim(mcs5_cm_interview) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs5_cm_interview)

## =================== MCS5 TEACHER CM SURVEY ===================================
mcs5_teacher_cm_survey <- read.spss(paste0(MCS5, "/", "mcs5_cm_teacher_survey.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs5_teacher_cm_survey$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs5_teacher_cm_survey$MCSID) # family identifier
table(mcs5_teacher_cm_survey$ECNUM00) # cohort member number within family 

# Recode cohort member number variable to numeric
levels(mcs5_teacher_cm_survey$ECNUM00) # factor with 3 levels
mcs5_teacher_cm_survey$ECNUM00 <- recode(mcs5_teacher_cm_survey$ECNUM00, "'1st Cohort Member of the family'=1; '2nd Cohort Member of the family'=2; '3rd Cohort Member of the family'=3")
levels(mcs5_teacher_cm_survey$ECNUM00) # numeric

# Remove white spaces from ID variable
mcs5_teacher_cm_survey$MCSID = as.factor(trimws(mcs5_teacher_cm_survey$MCSID))
str(mcs5_teacher_cm_survey$MCSID)

# Create new dataset with relevant variables
mcs5_teacher_cm_survey_subset <- subset(mcs5_teacher_cm_survey, ECNUM00==1,
                                          select=c(MCSID, # IDs
                                                   EEBDTO_T, # SDQ Total Difficulties MCS5 (teacher reported)
                                                   EQ5S)) # CM is is picked on or bullied by other children 
colnames(mcs5_teacher_cm_survey_subset)
        
dim(mcs5_teacher_cm_survey_subset) # Check number of rows to check that each family is only represented once
dim(mcs5_teacher_cm_survey) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs5_teacher_cm_survey)

################### MEASURES FROM SWEEP 6 ###################

## ================== MCS6 CM DERIVED INTERVIEW ===================================
mcs6_cm_derived <- read.spss(paste0(MCS6, "/", "mcs6_cm_derived.sav"), to.data.frame = TRUE)

# Check whether IDs are repeated (yes)
n_occur <- data.frame(table(mcs6_cm_derived$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs6_cm_derived$MCSID) # family identifier 
table(mcs6_cm_derived$FCNUM00) # cohort member number within family

# Recode cohort member number variable to numeric
levels(mcs6_cm_derived$FCNUM00) # factor with 3 levels
mcs6_cm_derived$FCNUM00 <- recode(mcs6_cm_derived$FCNUM00, "'1st Cohort Member of the family'=1; '2nd Cohort Member of the family'=2; '3rd Cohort Member of the family'=3")
levels(mcs6_cm_derived$FCNUM00) # numeric

# Remove white spaces from ID variable
mcs6_cm_derived$MCSID = as.factor(trimws(mcs6_cm_derived$MCSID))
str(mcs6_cm_derived$MCSID)

# Create new dataset with relevant variables
mcs6_cm_derived_subset <- subset(mcs6_cm_derived, FCNUM00==1, 
                                          select=c(MCSID, # IDs
                                                   FEBDTOT)) # SDQ Total Difficulties MCS6
colnames(mcs6_cm_derived_subset)
                                
dim(mcs6_cm_derived_subset) # Check number of rows to check that each family is only represented once
dim(mcs6_cm_derived) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs6_cm_derived)

## =================== MCS6 PARENT INTERVIEW ===================================
mcs6_parent_interview <- read.spss(paste0(MCS6, "/", "mcs6_parent_interview.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs6_parent_interview$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs6_parent_interview$MCSID) # family identifier 
table(mcs6_parent_interview$FPNUM00) # person number within an MCS family

# Remove white spaces from ID variable
mcs6_parent_interview$MCSID = as.factor(trimws(mcs6_parent_interview$MCSID))
str(mcs6_parent_interview$MCSID)

# Create new dataset with relevant variables
mcs6_parent_interview_subset <- subset(mcs6_parent_interview, FPNUM00==1,
                                       select=c(MCSID, # IDs 
                                                FPPHDE00, FPPHHO00, FPPHRF00, FPPHEE00, FPPHWO00, FPPHNE00, # parental mental health (Kessler)
                                                FPALDR00, # frequency of alcohol consumption
                                                FPDRUG00, # used recreational drugs
                                                FPFCIN00, # marital status 
                                                FPHARE00, # parental relationship (Golombok)
                                                FPFORC00)) # whether partner ever used force in relationship (Golombok)
colnames(mcs6_parent_interview_subset)

dim(mcs6_parent_interview_subset) # Check number of rows to check that each family is only represented once
dim(mcs6_parent_interview) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs6_parent_interview)

## ================== MCS6 PARENT CM INTERVIEW ===================================
mcs6_parent_cm_interview <- read.spss(paste0(MCS6, "/", "mcs6_parent_cm_interview.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs6_parent_cm_interview$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs6_parent_cm_interview$MCSID) # family identifier
table(mcs6_parent_cm_interview$FCNUM00) # cohort member number within family 
table(mcs6_parent_cm_interview$FPNUM00) # person number within an MCS family

# Recode cohort member number variable to numeric
levels(mcs6_parent_cm_interview$FCNUM00) # factor with 3 levels
mcs6_parent_cm_interview$FCNUM00 <- recode(mcs6_parent_cm_interview$FCNUM00, "'1st Cohort Member of the family'=1; '2nd Cohort Member of the family'=2; '3rd Cohort Member of the family'=3")
levels(mcs6_parent_cm_interview$FCNUM00) # numeric

# Remove white spaces from ID variable
mcs6_parent_cm_interview$MCSID = as.factor(trimws(mcs6_parent_cm_interview$MCSID))
str(mcs6_parent_cm_interview$MCSID)

# Create new dataset with relevant variables
mcs6_parent_cm_interview_subset <- subset(mcs6_parent_cm_interview, FCNUM00==1 & FPNUM00==1,
                                          select=c(MCSID, # IDs
                                                   FPSDPB00, # CM picked on or bullied by other children
                                                   FPCLSI00)) # child has any longstanding health conditions
colnames(mcs6_parent_cm_interview_subset)
        
dim(mcs6_parent_cm_interview_subset) # Check number of rows to check that each family is only represented once
dim(mcs6_parent_cm_interview) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs6_parent_cm_interview)

## ================== MCS6 FAMILY DERIVED VARIABLES ===================================
mcs6_family_derived <- read.spss(paste0(MCS6, "/", "mcs6_family_derived.sav"), to.data.frame = TRUE)

# Check whether data is in long format (no)
n_occur <- data.frame(table(mcs6_family_derived$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs6_family_derived$MCSID) # family identifier

# Remove white spaces from ID variable
mcs6_family_derived$MCSID = as.factor(trimws(mcs6_family_derived$MCSID))
str(mcs6_family_derived$MCSID)

# Create new dataset with relevant variables
mcs6_family_derived_subset <- dplyr::select(mcs6_family_derived, 
                                      MCSID, # IDs
                                      FOECDUK0) # household income (OECD weighted quintiles UK analysis)
dim(mcs6_family_derived_subset)  
colnames(mcs6_family_derived_subset)   

# Remove original dataset
rm(mcs6_family_derived)

## ================== MCS6 CM INTERVIEW ===================================
mcs6_cm_interview <- read.spss(paste0(MCS6, "/", "mcs6_cm_interview.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs6_cm_interview$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs6_cm_interview$MCSID) # family identifier
table(mcs6_cm_interview$FCNUM00) # cohort member number within family 

# Recode cohort member number variable to numeric
levels(mcs6_cm_interview$FCNUM00) # factor with 3 levels
mcs6_cm_interview$FCNUM00 <- recode(mcs6_cm_interview$FCNUM00, "'1st Cohort Member of the family'=1; '2nd Cohort Member of the family'=2; '3rd Cohort Member of the family'=3")
levels(mcs6_cm_interview$FCNUM00) # numeric

# Remove white spaces from ID variable
mcs6_cm_interview$MCSID = as.factor(trimws(mcs6_cm_interview$MCSID))
str(mcs6_cm_interview$MCSID)

# Create new dataset with relevant variables
mcs6_cm_interview_subset <- subset(mcs6_cm_interview, FCNUM00==1,
                                          select=c(MCSID, # IDs
                                                   FCVICG00, # CM insulted/threatened/shouted at
                                                   FCVICA00, # been physically violent towards CM
                                                   FCVICC00, # hit or used weapon against CM
                                                   FCVICE00, # stolen something from CM
                                                   FCVICF0A, # sexually assaulted CM
                                                   FCHURT00, # how often other children bully CM
                                                   FCBULB00)) # how often brothers/sisters pick on CM
colnames(mcs6_cm_interview_subset)
        
dim(mcs6_cm_interview_subset) # Check number of rows to check that each family is only represented once
dim(mcs6_cm_interview) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs6_cm_interview)

## ================== MCS7 CM INTERVIEW ===================================
mcs7_cm_interview <- read.spss(paste0(MCS7, "/", "mcs7_cm_interview.sav"), to.data.frame = TRUE)

# Check whether data is in long format (yes)
n_occur <- data.frame(table(mcs7_cm_interview$MCSID))
n_occur[n_occur$Freq > 1,] 

# Check variables needed are in this dataset
str(mcs7_cm_interview$MCSID) # family identifier
table(mcs7_cm_interview$GCNUM00) # cohort member number within family 

# Recode cohort member number variable to numeric
levels(mcs7_cm_interview$GCNUM00) # factor with 3 levels
mcs7_cm_interview$GCNUM00 <- recode(mcs7_cm_interview$GCNUM00, "'1st Cohort Member of the family'=1; '2nd Cohort Member of the family'=2; '3rd Cohort Member of the family'=3")
levels(mcs7_cm_interview$GCNUM00) # numeric

# Remove white spaces from ID variable
mcs7_cm_interview$MCSID = as.factor(trimws(mcs7_cm_interview$MCSID))
str(mcs7_cm_interview$MCSID)

# Create new dataset with relevant variables
mcs7_cm_interview_subset <- subset(mcs7_cm_interview, GCNUM00==1,
                                          select=c(MCSID, # IDs
                                                   ## Psychological distress ##
                                                   GCPHDE00, # Kessler: so depressed nothing could cheer you up
                                                   GCPHHO00, # Kessler: hopeless
                                                   GCPHRF00, # Kessler: restless or fidgety
                                                   GCPHEE00, # Kessler: everything was an effort
                                                   GCPHWO00, # Kessler: worthless
                                                   GCPHNE00, # Kessler: nervous
                                                   ## Internalising symptoms ##
                                                   GCSDQC00, # SDQ: headaches, stomachaches
                                                   GCSDQH00, # SDQ: worry a lot
                                                   GCSDQM00, # SDQ: often unhappy 
                                                   GCSDQP00, # SDQ: nervous in new situations
                                                   GCSDQX00, # SDQ: many fears, easily scared
                                                   GCSDQF00, # SDQ: usually on my own
                                                   GCSDQK00, # SDQ: one good friend or more
                                                   GCSDQN00, # SDQ: other people my age like me
                                                   GCSDQS00, # SDQ: others pick on me or bully me
                                                   GCSDQW00, # SDQ: get on better with adults
                                                   ## Externalising symptoms ##
                                                   GCSDQE00, # SDQ: often lose my temper
                                                   GCSDQG00, # SDQ: usually do as told
                                                   GCSDQL00, # SDQ: fight a lot
                                                   GCSDQR00, # SDQ: often accused of lying or cheating
                                                   GCSDQV00, # SDQ: take things that are not mine
                                                   GCSDQB00, # SDQ: restless, cannot stay still 
                                                   GCSDQJ00, # SDQ: constantly fidgeting 
                                                   GCSDQO00, # SDQ: easily distracted
                                                   GCSDQU00, # SDQ: think before do things
                                                   GCSDQY00)) # SDQ: finish the work, attention is good
colnames(mcs7_cm_interview_subset)
        
dim(mcs7_cm_interview_subset) # Check number of rows to check that each family is only represented once
dim(mcs7_cm_interview) # Check number of rows in old dataset (more because families represented multiple times when twins/triplets)

# Remove original dataset
rm(mcs7_cm_interview)

## =================== MERGE ALL FILES INTO A SINGLE DATASET ===================================
MCS_merged <- Reduce(function(x,y) merge(x,y,by="MCSID", all.x=TRUE), 
                    list(mcs1_hhgrid_subset, 
                         mcs1_cm_derived_subset,
                         mcs1_parent_interview_subset,
                         mcs1_parent_derived_interview_subset,
                         mcs1_family_derived_subset,
                         mcs2_cm_derived_subset,
                         mcs2_parent_interview_subset,
                         mcs2_parent_cm_interview_subset,
                         mcs2_family_derived_subset,
                         mcs2_cm_cognitive_assessment_subset, 
                         mcs3_cm_derived_subset,
                         mcs3_parent_interview_subset,
                         mcs3_parent_cm_interview_subset,
                         mcs3_family_derived_subset,
                         mcs4_cm_derived_subset,
                         mcs4_parent_interview_subset,
                         mcs4_parent_cm_interview_subset,
                         mcs4_family_derived_subset,
                         mcs4_cm_interview_subset,
                         mcs4_teacher_cm_survey_subset,
                         mcs5_parent_interview_subset,
                         mcs5_parent_cm_interview_subset, 
                         mcs5_family_derived_subset,
                         mcs5_cm_interview_subset,
                         mcs5_teacher_cm_survey_subset,
                         mcs6_cm_derived_subset,
                         mcs6_parent_interview_subset,
                         mcs6_parent_cm_interview_subset,
                         mcs6_family_derived_subset,
                         mcs6_cm_interview_subset,
                         mcs7_cm_interview_subset))
dim(MCS_merged)
colnames(MCS_merged)
head(MCS_merged)

## Check whether IDs are duplicated (e.g. multiple children per family) - no
n_occur <- data.frame(table(MCS_merged$MCSID))
n_occur[n_occur$Freq > 1,]

# Save MCS merged dataset
write.table(MCS_merged, file = "MCS_merged.txt", sep = "\t", dec = ".")

# Remove datasets and working directory shortcuts
rm(mcs1_hhgrid_subset, 
   mcs1_cm_derived_subset,
   mcs1_parent_interview_subset,
   mcs1_parent_derived_interview_subset,
   mcs1_family_derived_subset,
   mcs2_cm_derived_subset,
   mcs2_parent_interview_subset,
   mcs2_parent_cm_interview_subset,
   mcs2_family_derived_subset,
   mcs2_cm_cognitive_assessment_subset, 
   mcs3_cm_derived_subset,
   mcs3_parent_interview_subset,
   mcs3_parent_cm_interview_subset,
   mcs3_family_derived_subset,
   mcs4_cm_derived_subset,
   mcs4_parent_interview_subset,
   mcs4_parent_cm_interview_subset,
   mcs4_family_derived_subset,
   mcs4_cm_interview_subset,
   mcs4_teacher_cm_survey_subset,
   mcs5_parent_interview_subset,
   mcs5_parent_cm_interview_subset, 
   mcs5_family_derived_subset,
   mcs5_cm_interview_subset,
   mcs5_teacher_cm_survey_subset,
   mcs6_cm_derived_subset,
   mcs6_parent_interview_subset,
   mcs6_parent_cm_interview_subset,
   mcs6_family_derived_subset,
   mcs6_cm_interview_subset,
   mcs7_cm_interview_subset,
   n_occur,
   MCS1, MCS2, MCS3, MCS4, MCS5, MCS6, MCS7)
```

# 1. Data cleaning

Next, we will sum together measures, derive variables, and ensure all
variables are recoded to the correct format before conducting factor
analysis.

``` r
# Load MCS merged dataset
setwd("/Users/athenachowruwern/Desktop")
MCS_merged <-  read.delim("MCS_merged.txt", header = TRUE, sep = "\t", dec = ".")

# Look at structure of MCS_merged to see how each variable is originally coded
str(MCS_merged)

## ================== Parental mental health: Rutter Malaise Inventory Scale (Sweep 1, 9 months) ==================
# Factor variable with 2 levels; originally coded as "Yes" or "No"
# First we will recode levels to be numeric, so they can be summed together into one parental mental health variable where higher score = worse parental mental health
# Then we will recode the summed parental mental health variable to be binary according to a score cutoff

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$APTIRE00, useNA="always")
table(MCS_merged$APDEPR00, useNA="always")
table(MCS_merged$APWORR00, useNA="always")
table(MCS_merged$APRAGE00, useNA="always")
table(MCS_merged$APSCAR00, useNA="always")
table(MCS_merged$APUPSE00, useNA="always")
table(MCS_merged$APKEYD00, useNA="always")
table(MCS_merged$APNERV00, useNA="always")
table(MCS_merged$APHERA00, useNA="always")

# Recode variables to 1/0
MCS_merged <- MCS_merged %>% 
  mutate_at(c("APTIRE00","APDEPR00","APWORR00","APRAGE00","APSCAR00","APUPSE00","APKEYD00","APNERV00","APHERA00"), 
            function(x) recode(x, "'Yes'=1; 'No'=0"))

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("APTIRE00","APDEPR00","APWORR00","APRAGE00","APSCAR00","APUPSE00","APKEYD00","APNERV00","APHERA00"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly 
table(MCS_merged$APTIRE00, useNA="always")
table(MCS_merged$APDEPR00, useNA="always")
table(MCS_merged$APWORR00, useNA="always")
table(MCS_merged$APRAGE00, useNA="always")
table(MCS_merged$APSCAR00, useNA="always")
table(MCS_merged$APUPSE00, useNA="always")
table(MCS_merged$APKEYD00, useNA="always")
table(MCS_merged$APNERV00, useNA="always")
table(MCS_merged$APHERA00, useNA="always")

# The below code first indicates whether there are missing observations in the columns of the variables (2=columns), then it sums the number of missing items across variables per participant/row (1=row), and then if more than 50% of cells are missing per participant, it codes NA for that participant

# Sum items together into Rutter_MCS1
MCS_merged$Rutter_MCS1 = NA

MCS_merged$Rutter_MCS1 = apply(MCS_merged[,c("APTIRE00","APDEPR00","APWORR00","APRAGE00","APSCAR00","APUPSE00","APKEYD00","APNERV00","APHERA00")],1, mean, na.rm=T)*9 # multiply average of 9 items by 9

MCS_merged$Rutter_MCS1[apply(apply(MCS_merged[,c("APTIRE00","APDEPR00","APWORR00","APRAGE00","APSCAR00","APUPSE00","APKEYD00","APNERV00","APHERA00")],2,is.na),1,sum) >5] = NA # make NA when missing more than 5 items

describe(MCS_merged$Rutter_MCS1)
table(MCS_merged$Rutter_MCS1, useNA="always")

# If scores >=4, code to 1 for poor parental mental health, else code to 0
MCS_merged$Rutter_MCS1 <- ifelse(MCS_merged$Rutter_MCS1>=4, 1, 0)
table(MCS_merged$Rutter_MCS1, useNA="always")

## ================== Parental mental health: Kessler Scale (Sweeps 2-6, ages 3, 5, 7, 11, 14) ==================
# Factor variable with 6 levels; originally coded "All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time", "Can't say"
# First we will recode levels to be numeric, so they can be summed together into one parental mental health variable where higher score = worse parental mental health
# Then we will recode the summed parental mental health variable to be binary according to a score cutoff

### (Sweep 2, age 3)

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$BPPHDE00, useNA="always")
table(MCS_merged$BPPHHO00, useNA="always")
table(MCS_merged$BPPHRF00, useNA="always")
table(MCS_merged$BPPHEE00, useNA="always")
table(MCS_merged$BPPHWO00, useNA="always")
table(MCS_merged$BPPHNE00, useNA="always")

# Note: some levels were coded inconsistently (e.g., Can't or Can t), so we have to work around the single apostrophe ' or extra spacing, otherwise they will cause string parsing errors

# Recode variables to 5-point scale
MCS_merged <- MCS_merged %>% 
  mutate_at(c("BPPHDE00","BPPHHO00","BPPHRF00","BPPHEE00","BPPHWO00","BPPHNE00"), 
            function(x) recode(x, "\"All of the time\"=4; \"Most of the time\"=3; \"Some of the time\"=2; \"A little of the time\"=1; \"None of the time\"=0; \"Can't say\"=NA"))

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("BPPHDE00","BPPHHO00","BPPHRF00","BPPHEE00","BPPHWO00","BPPHNE00"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly 
table(MCS_merged$BPPHDE00, useNA="always")
table(MCS_merged$BPPHHO00, useNA="always")
table(MCS_merged$BPPHRF00, useNA="always")
table(MCS_merged$BPPHEE00, useNA="always")
table(MCS_merged$BPPHWO00, useNA="always")
table(MCS_merged$BPPHNE00, useNA="always")

# Sum items together into Kessler_MCS2
MCS_merged$Kessler_MCS2 = NA

MCS_merged$Kessler_MCS2 = apply(MCS_merged[,c("BPPHDE00","BPPHHO00","BPPHRF00","BPPHEE00","BPPHWO00","BPPHNE00")],1, mean, na.rm=T)*6 # multiply average of 6 items by 6

MCS_merged$Kessler_MCS2[apply(apply(MCS_merged[,c("BPPHDE00","BPPHHO00","BPPHRF00","BPPHEE00","BPPHWO00","BPPHNE00")],2,is.na),1,sum) >3] = NA # make NA when missing more than 3 items

describe(MCS_merged$Kessler_MCS2)
table(MCS_merged$Kessler_MCS2, useNA="always")

# If scores >=13, code to 1 for poor parental mental health, else code to 0
MCS_merged$Kessler_MCS2 <- ifelse(MCS_merged$Kessler_MCS2>=13, 1, 0)
table(MCS_merged$Kessler_MCS2, useNA="always")

### (Sweep 3, age 5)

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$CPPHDE00, useNA="always")
table(MCS_merged$CPPHHO00, useNA="always")
table(MCS_merged$CPPHRF00, useNA="always")
table(MCS_merged$CPPHEE00, useNA="always")
table(MCS_merged$CPPHWO00, useNA="always")
table(MCS_merged$CPPHNE00, useNA="always")

# Recode variables to 5-point scale
MCS_merged <- MCS_merged %>% 
  mutate_at(c("CPPHDE00","CPPHHO00","CPPHRF00","CPPHEE00","CPPHWO00","CPPHNE00"), 
            function(x) recode(x, "'All of the time'=4; 'Most of the time'=3; 'Some of the time'=2; 'A little of the time'=1; 'None of the time'=0; 'Can t say'=NA"))

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("CPPHDE00","CPPHHO00","CPPHRF00","CPPHEE00","CPPHWO00","CPPHNE00"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly 
table(MCS_merged$CPPHDE00, useNA="always")
table(MCS_merged$CPPHHO00, useNA="always")
table(MCS_merged$CPPHRF00, useNA="always")
table(MCS_merged$CPPHEE00, useNA="always")
table(MCS_merged$CPPHWO00, useNA="always")
table(MCS_merged$CPPHNE00, useNA="always")

# Sum items together into Kessler_MCS3
MCS_merged$Kessler_MCS3 = NA

MCS_merged$Kessler_MCS3 = apply(MCS_merged[,c("CPPHDE00","CPPHHO00","CPPHRF00","CPPHEE00","CPPHWO00","CPPHNE00")],1, mean, na.rm=T)*6 # multiply average of 6 items by 6

MCS_merged$Kessler_MCS3[apply(apply(MCS_merged[,c("CPPHDE00","CPPHHO00","CPPHRF00","CPPHEE00","CPPHWO00","CPPHNE00")],2,is.na),1,sum) >3] = NA # make NA when missing more than 3 items

describe(MCS_merged$Kessler_MCS3)
table(MCS_merged$Kessler_MCS3, useNA="always")

# If scores >=13, code to 1 for poor parental mental health, else code to 0
MCS_merged$Kessler_MCS3 <- ifelse(MCS_merged$Kessler_MCS3>=13, 1, 0)
table(MCS_merged$Kessler_MCS3, useNA="always")

### (Sweep 4, age 7)

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$DPPHDE00, useNA="always")
table(MCS_merged$DPPHHO00, useNA="always")
table(MCS_merged$DPPHRF00, useNA="always")
table(MCS_merged$DPPHEE00, useNA="always")
table(MCS_merged$DPPHWO00, useNA="always")
table(MCS_merged$DPPHNE00, useNA="always")

# Recode variables to 5-point scale
MCS_merged <- MCS_merged %>% 
  mutate_at(c("DPPHDE00","DPPHHO00","DPPHRF00","DPPHEE00","DPPHWO00","DPPHNE00"), 
            function(x) recode(x, "'All of the time'=4; 'Most of the time       '=3; 'Some of the time       '=2; 'A little of the time   '=1; 'None of the time       '=0; 'Can t say      '=NA"))

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("DPPHDE00","DPPHHO00","DPPHRF00","DPPHEE00","DPPHWO00","DPPHNE00"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly 
table(MCS_merged$DPPHDE00, useNA="always")
table(MCS_merged$DPPHHO00, useNA="always")
table(MCS_merged$DPPHRF00, useNA="always")
table(MCS_merged$DPPHEE00, useNA="always")
table(MCS_merged$DPPHWO00, useNA="always")
table(MCS_merged$DPPHNE00, useNA="always")

# Sum items together into Kessler_MCS4
MCS_merged$Kessler_MCS4 = NA

MCS_merged$Kessler_MCS4 = apply(MCS_merged[,c("DPPHDE00","DPPHHO00","DPPHRF00","DPPHEE00","DPPHWO00","DPPHNE00")],1, mean, na.rm=T)*6 # multiply average of 6 items by 6

MCS_merged$Kessler_MCS4[apply(apply(MCS_merged[,c("DPPHDE00","DPPHHO00","DPPHRF00","DPPHEE00","DPPHWO00","DPPHNE00")],2,is.na),1,sum) >3] = NA # make NA when missing more than 3 items

describe(MCS_merged$Kessler_MCS4)
table(MCS_merged$Kessler_MCS4, useNA="always")

# If scores >=13, code to 1 for poor parental mental health, else code to 0
MCS_merged$Kessler_MCS4 <- ifelse(MCS_merged$Kessler_MCS4>=13, 1, 0)
table(MCS_merged$Kessler_MCS4, useNA="always")

### (Sweep 5, age 11)

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$EPPHDE00, useNA="always")
table(MCS_merged$EPPHHO00, useNA="always")
table(MCS_merged$EPPHRF00, useNA="always")
table(MCS_merged$EPPHEE00, useNA="always")
table(MCS_merged$EPPHWO00, useNA="always")
table(MCS_merged$EPPHNE00, useNA="always")

# Recode variables to 5-point scale
MCS_merged <- MCS_merged %>% 
  mutate_at(c("EPPHDE00","EPPHHO00","EPPHRF00","EPPHEE00","EPPHWO00","EPPHNE00"), 
            function(x) recode(x, "\"All of the time\"=4; \"Most of the time       \"=3; \"Some of the time       \"=2; \"A little of the time   \"=1; \"None of the time       \"=0; \"Don’t know/don’t wish to answer\"=NA"))

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("EPPHDE00","EPPHHO00","EPPHRF00","EPPHEE00","EPPHWO00","EPPHNE00"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly 
table(MCS_merged$EPPHDE00, useNA="always")
table(MCS_merged$EPPHHO00, useNA="always")
table(MCS_merged$EPPHRF00, useNA="always")
table(MCS_merged$EPPHEE00, useNA="always")
table(MCS_merged$EPPHWO00, useNA="always")
table(MCS_merged$EPPHNE00, useNA="always")

# Sum items together into Kessler_MCS5
MCS_merged$Kessler_MCS5 = NA

MCS_merged$Kessler_MCS5 = apply(MCS_merged[,c("EPPHDE00","EPPHHO00","EPPHRF00","EPPHEE00","EPPHWO00","EPPHNE00")],1, mean, na.rm=T)*6 # multiply average of 6 items by 6

MCS_merged$Kessler_MCS5[apply(apply(MCS_merged[,c("EPPHDE00","EPPHHO00","EPPHRF00","EPPHEE00","EPPHWO00","EPPHNE00")],2,is.na),1,sum) >3] = NA # make NA when missing more than 3 items

describe(MCS_merged$Kessler_MCS5)
table(MCS_merged$Kessler_MCS5, useNA="always")

# If scores >=13, code to 1 for poor parental mental health, else code to 0
MCS_merged$Kessler_MCS5 <- ifelse(MCS_merged$Kessler_MCS5>=13, 1, 0)
table(MCS_merged$Kessler_MCS5, useNA="always")

### (Sweep 6, age 14)

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$FPPHDE00, useNA="always")
table(MCS_merged$FPPHHO00, useNA="always")
table(MCS_merged$FPPHRF00, useNA="always")
table(MCS_merged$FPPHEE00, useNA="always")
table(MCS_merged$FPPHWO00, useNA="always")
table(MCS_merged$FPPHNE00, useNA="always")

# Recode variables to 5-point scale
MCS_merged <- MCS_merged %>% 
  mutate_at(c("FPPHDE00","FPPHHO00","FPPHRF00","FPPHEE00","FPPHWO00","FPPHNE00"), 
            function(x) recode(x, "'All of the time'=4; 'Most of the time       '=3; 'Some of the time       '=2; 'A little of the time   '=1; 'None of the time       '=0"))

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("FPPHDE00","FPPHHO00","FPPHRF00","FPPHEE00","FPPHWO00","FPPHNE00"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly 
table(MCS_merged$FPPHDE00, useNA="always")
table(MCS_merged$FPPHHO00, useNA="always")
table(MCS_merged$FPPHRF00, useNA="always")
table(MCS_merged$FPPHEE00, useNA="always")
table(MCS_merged$FPPHWO00, useNA="always")
table(MCS_merged$FPPHNE00, useNA="always")

# Sum items together into Kessler_MCS6
MCS_merged$Kessler_MCS6 = NA

MCS_merged$Kessler_MCS6 = apply(MCS_merged[,c("FPPHDE00","FPPHHO00","FPPHRF00","FPPHEE00","FPPHWO00","FPPHNE00")],1, mean, na.rm=T)*6 # multiply average of 6 items by 6

MCS_merged$Kessler_MCS6[apply(apply(MCS_merged[,c("FPPHDE00","FPPHHO00","FPPHRF00","FPPHEE00","FPPHWO00","FPPHNE00")],2,is.na),1,sum) >3] = NA # make NA when missing more than 3 items

describe(MCS_merged$Kessler_MCS6)
table(MCS_merged$Kessler_MCS6, useNA="always")

# If scores >=13, code to 1 for poor parental mental health, else code to 0
MCS_merged$Kessler_MCS6 <- ifelse(MCS_merged$Kessler_MCS6>=13, 1, 0)
table(MCS_merged$Kessler_MCS6, useNA="always")

### SUM RUTTER AND KESSLER SCORES ACROSS SWEEPS 1,2,3,4,5,6

# If parents scored poor mental health for at least 1 sweep, code as 1 for poor parental mental health
MCS_merged$Parental_mental_health <- 0

MCS_merged$Parental_mental_health[MCS_merged$Rutter_MCS1==1 | MCS_merged$Kessler_MCS2==1 | 
                                     MCS_merged$Kessler_MCS3==1 | MCS_merged$Kessler_MCS4==1 | 
                                     MCS_merged$Kessler_MCS5==1 | MCS_merged$Kessler_MCS6==1] <- 1

# If missing data for all sweeps 1-6, code to NA
MCS_merged$Parental_mental_health[is.na(MCS_merged$Rutter_MCS1) & is.na(MCS_merged$Kessler_MCS2) & 
                                     is.na(MCS_merged$Kessler_MCS3) & is.na(MCS_merged$Kessler_MCS4) & 
                                     is.na(MCS_merged$Kessler_MCS5) & is.na(MCS_merged$Kessler_MCS6)] <- NA 

table(MCS_merged$Parental_mental_health, useNA="always")

## ================== Adolescent mental health: Kessler Scale (Sweep 7, age 17) ==================
# Factor variable with 6 levels; originally coded "All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time", "Can't say"
# First we will recode levels to be numeric, so they can be summed together into one adolescent mental health variable where higher score = worse adolescent mental health
# This adolescent mental health variable will be used as the outcome variable in the regression

# Look at prevalence of depression and anxiety
table(MCS_merged$GCPHDE00, useNA="always") # Kessler: so depressed nothing could cheer you up
table(MCS_merged$GCPHHO00, useNA="always") # Kessler: hopeless
table(MCS_merged$GCPHRF00, useNA="always") # Kessler: restless or fidgety
table(MCS_merged$GCPHEE00, useNA="always") # Kessler: everything was an effort
table(MCS_merged$GCPHWO00, useNA="always") # Kessler: worthless
table(MCS_merged$GCPHNE00, useNA="always") # Kessler: nervous

# Recode variables to 5-point scale
MCS_merged <- MCS_merged %>% 
  mutate_at(c("GCPHDE00","GCPHHO00","GCPHRF00","GCPHEE00","GCPHWO00","GCPHNE00"), 
            function(x) recode(x, "'All of the time'=4; 'Most of the time'=3; 'Some of the time'=2; 'A little of the time'=1; 'None of the time'=0"))

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("GCPHDE00","GCPHHO00","GCPHRF00","GCPHEE00","GCPHWO00","GCPHNE00"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly 
table(MCS_merged$GCPHDE00, useNA="always")
table(MCS_merged$GCPHHO00, useNA="always")
table(MCS_merged$GCPHRF00, useNA="always")
table(MCS_merged$GCPHEE00, useNA="always")
table(MCS_merged$GCPHWO00, useNA="always")
table(MCS_merged$GCPHNE00, useNA="always")

# Sum items together into Adolescent_mental_health
MCS_merged$Adolescent_mental_health = NA

MCS_merged$Adolescent_mental_health <- apply(MCS_merged[,c("GCPHDE00","GCPHHO00","GCPHRF00","GCPHEE00","GCPHWO00","GCPHNE00")], 1, sum, na.rm=T) # sum items

MCS_merged$Adolescent_mental_health[apply(apply(MCS_merged[,c("GCPHDE00","GCPHHO00","GCPHRF00","GCPHEE00","GCPHWO00","GCPHNE00")],2,is.na),1,sum) >3] = NA # make NA when missing more than 3 items

describe(MCS_merged$Adolescent_mental_health)
table(MCS_merged$Adolescent_mental_health, useNA="always")

## ================== Frequent parental alcohol use (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years) ==================
# Factor variable with 5-7 levels; originally coded "Every day", "5-6 times per week", "3-4 times per week", "1-2 times per week", "1-2 times per month", "Less than once a month", "Never"
# "Every day" and "5-6 times per week" will be recoded to 1, while all other levels recoded to 0

### (Sweep 1, 9 months)
table(MCS_merged$APALDR00, useNA="always")

MCS_merged$APALDR00 <- recode(MCS_merged$APALDR00, "'Every day'=1; '5-6 times per week'=1; '3-4 times per week'=0; '1-2 times per week'=0; '1-2 times per month'=0; 'Less than once a month'=0; 'Never'=0")

MCS_merged$APALDR00 <- as.numeric(as.character(MCS_merged$APALDR00))

table(MCS_merged$APALDR00, useNA="always")

### (Sweep 2, age 3)
table(MCS_merged$BPALDR00, useNA="always")

MCS_merged$BPALDR00 <- recode(MCS_merged$BPALDR00, "'Every day'=1; '5-6 times per week'=1; '3-4 times per week'=0; '1-2 times per week'=0; '1-2 times per month'=0; 'Less than once a month'=0; 'Never'=0; 'Refused'=NA; 'Refused_duplicated_8'=NA")

MCS_merged$BPALDR00 <- as.numeric(as.character(MCS_merged$BPALDR00))

table(MCS_merged$BPALDR00, useNA="always")

### (Sweep 3, age 5)
table(MCS_merged$CPALDR00, useNA="always")

MCS_merged$CPALDR00 <- recode(MCS_merged$CPALDR00, "'Every day'=1; '5-6 times per week'=1; '3-4 times per week'=0; '1-2 times per week'=0; '1-2 times per month'=0; 'Less than once a month'=0; 'Never'=0")

MCS_merged$CPALDR00 <- as.numeric(as.character(MCS_merged$CPALDR00))

table(MCS_merged$CPALDR00, useNA="always")

### (Sweep 4, age 7)
table(MCS_merged$DPALDR00, useNA="always")

MCS_merged$DPALDR00 <- recode(MCS_merged$DPALDR00, "'Every day      '=1; '5-6 times per week     '=1; '3-4 times per week     '=0; '1-2 times per week     '=0; '1-2 times per month    '=0; 'Less than once a month '=0; 'Never  '=0")

MCS_merged$DPALDR00 <- as.numeric(as.character(MCS_merged$DPALDR00))

table(MCS_merged$DPALDR00, useNA="always")

### (Sweep 5, age 11)
table(MCS_merged$EPALDR00, useNA="always")

MCS_merged$EPALDR00 <- recode(MCS_merged$EPALDR00, "'4 or more times a week '=1; '2-3 times a week       '=0; '2-4 times per month    '=0; 'Monthly or less'=0; 'Never  '=0; 'Don’t know/don’t wish to answer'=NA")

MCS_merged$EPALDR00 <- as.numeric(as.character(MCS_merged$EPALDR00))

table(MCS_merged$EPALDR00, useNA="always")

### (Sweep 6, age 14)
table(MCS_merged$FPALDR00, useNA="always")

MCS_merged$FPALDR00 <- recode(MCS_merged$FPALDR00, "'4 or more times a week '=1; '2-3 times a week       '=0; '2-4 times per month    '=0; 'Monthly or less'=0; 'Never  '=0")

MCS_merged$FPALDR00 <- as.numeric(as.character(MCS_merged$FPALDR00))

table(MCS_merged$FPALDR00, useNA="always")

### SUM ALCOHOL SCORES ACROSS SWEEPS 1,2,3,4,5,6

# If parents scored frequent alcohol use for at least 1 sweep, code as 1 for frequent parental alcohol use
MCS_merged$Parental_alcohol <- 0

MCS_merged$Parental_alcohol[MCS_merged$APALDR00==1 | MCS_merged$BPALDR00==1 | 
                                     MCS_merged$CPALDR00==1 | MCS_merged$DPALDR00==1 |
                                     MCS_merged$EPALDR00==1 | MCS_merged$FPALDR00==1] <- 1

# If missing data for all sweeps 1-6, code to NA
MCS_merged$Parental_alcohol[is.na(MCS_merged$APALDR00) & is.na(MCS_merged$BPALDR00) & 
                                     is.na(MCS_merged$CPALDR00) & is.na(MCS_merged$DPALDR00) &
                                     is.na(MCS_merged$EPALDR00) & is.na(MCS_merged$FPALDR00)] <- NA 

table(MCS_merged$Parental_alcohol, useNA="always")

## ================== Frequent parental drug use (Sweeps 2, 3, 6; ages 3, 5, 14) ==================
# Factor variable with 3-4 levels; originally coded "Occasionally", "Regularly", "Never", "Can't say"
# "Occasionally" and "Regularly" will be recoded to 1, "Never" will be recoded to 0

### (Sweep 2, age 3)
table(MCS_merged$BPDRUG00, useNA="always")

MCS_merged$BPDRUG00 <- recode(MCS_merged$BPDRUG00, "\"Occasionally\"=1; \"Regularly\"=1; \"Never\"=0; \"Can't say\"=NA")

MCS_merged$BPDRUG00 <- as.numeric(as.character(MCS_merged$BPDRUG00))

table(MCS_merged$BPDRUG00, useNA="always")

### (Sweep 3, age 5)
table(MCS_merged$CPDRUG00, useNA="always")

MCS_merged$CPDRUG00 <- recode(MCS_merged$CPDRUG00, "'Occasionally'=1; 'Regularly'=1; 'Never'=0; 'Can t say'=NA")

MCS_merged$CPDRUG00 <- as.numeric(as.character(MCS_merged$CPDRUG00))

table(MCS_merged$CPDRUG00, useNA="always")

### (Sweep 6, age 14)
table(MCS_merged$FPDRUG00, useNA="always")

MCS_merged$FPDRUG00 <- recode(MCS_merged$FPDRUG00, "'Occasionally   '=1; 'Regularly      '=1; 'Never  '=0")

MCS_merged$FPDRUG00 <- as.numeric(as.character(MCS_merged$FPDRUG00))

table(MCS_merged$FPDRUG00, useNA="always")

### SUM DRUG SCORES ACROSS SWEEPS 2,3,6

# If parents scored frequent drug use for at least 1 sweep, code as 1 for frequent parental drug use
MCS_merged$Parental_drug <- 0

MCS_merged$Parental_drug[MCS_merged$BPDRUG00==1 | MCS_merged$CPDRUG00==1 | 
                                     MCS_merged$FPDRUG00==1] <- 1

# If missing data for all sweeps 2,3,6, code to NA
MCS_merged$Parental_drug[is.na(MCS_merged$BPDRUG00) & is.na(MCS_merged$CPDRUG00) & 
                                     is.na(MCS_merged$FPDRUG00)] <- NA 

table(MCS_merged$Parental_drug, useNA="always")

## ================== Single parent status (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years) ==================
# Factor variable with 6-9 levels; originally coded "Legally separated", "Married", "Remarried", "Single", "Divorced", "Widowed", "Civil Partner" etc.
# "Married" and "Remarried" will be recoded to 0, all other categories will be recoded to 1
# We want to differentiate between parents who have a partner vs those without 

### (Sweep 1, 9 months)
table(MCS_merged$APFCIN00, useNA="always")

MCS_merged$APFCIN00 <- recode(MCS_merged$APFCIN00, "'Legally separated'=1; 'Married, 1st and only marriage'=0; 'Remarried, 2nd or later marriage'=0; 'Single never married'=1; 'Divorced'=1; 'Widowed'=1")

MCS_merged$APFCIN00 <- as.numeric(as.character(MCS_merged$APFCIN00))

table(MCS_merged$APFCIN00, useNA="always")

### (Sweep 2, 3 years)
table(MCS_merged$BPFCIN00, useNA="always")

MCS_merged$BPFCIN00 <- recode(MCS_merged$BPFCIN00, "'Legally separated'=1; 'Married, 1st and only marriage'=0; 'Remarried, 2nd or later marriage'=0; 'Single never married'=1; 'Divorced'=1; 'Widowed'=1")

MCS_merged$BPFCIN00 <- as.numeric(as.character(MCS_merged$BPFCIN00))

table(MCS_merged$BPFCIN00, useNA="always")

### (Sweep 3, 5 years)
table(MCS_merged$CPFCIN00, useNA="always")

MCS_merged$CPFCIN00 <- recode(MCS_merged$CPFCIN00, "'Legally separated'=1; 'Married, 1st and only marriage'=0; 'Remarried, 2nd or later marriage'=0; 'Single never married'=1; 'Divorced'=1; 'Widowed'=1")

MCS_merged$CPFCIN00 <- as.numeric(as.character(MCS_merged$CPFCIN00))

table(MCS_merged$CPFCIN00, useNA="always")

### (Sweep 4, 7 years)
table(MCS_merged$DPFCIN00, useNA="always")

MCS_merged$DPFCIN00 <- recode(MCS_merged$DPFCIN00, "'Legally separated      '=1; 'Married, 1st and only marriage '=0; 'Remarried, 2nd or later marriage       '=0; 'Single never married   '=1; 'Divorced       '=1; 'Widowed'=1")

MCS_merged$DPFCIN00 <- as.numeric(as.character(MCS_merged$DPFCIN00))

table(MCS_merged$DPFCIN00, useNA="always")

### (Sweep 5, 11 years)
table(MCS_merged$EPFCIN00, useNA="always")

MCS_merged$EPFCIN00 <- recode(MCS_merged$EPFCIN00, "'Legally separated      '=1; 'Married, 1st and only marriage '=0; 'Remarried, 2nd or later marriage       '=0; 'Single, never married  '=1; 'Divorced       '=1; 'Widowed'=1; 'A Civil Partner (legally recognised)   '=0; 'A former Civil Partner '=1; 'A surviving Civil Partner      '=1")

MCS_merged$EPFCIN00 <- as.numeric(as.character(MCS_merged$EPFCIN00))

table(MCS_merged$EPFCIN00, useNA="always")

### (Sweep 6, 14 years)
table(MCS_merged$FPFCIN00, useNA="always")

MCS_merged$FPFCIN00 <- recode(MCS_merged$FPFCIN00, "'Legally separated      '=1; 'Married, 1st and only marriage '=0; 'Remarried, 2nd or later marriage       '=0; 'Single, never married and never in a Civil Partnership'=1; 'Divorced       '=1; 'Widowed'=1; 'A Civil Partner in a legally recognised Civil Partnership'=0; 'A former Civil Partner (where Civil Partnership legally dissolved)'=1; 'A surviving Civil Partner (where Civil Partner has died)'=1")

MCS_merged$FPFCIN00 <- as.numeric(as.character(MCS_merged$FPFCIN00))

table(MCS_merged$FPFCIN00, useNA="always")

### SUM SINGLE PARENT SCORES ACROSS SWEEPS 1,2,3,4,5,6

# If parents scored no partner for at least 1 sweep, code as 1 
MCS_merged$Single_parent <- 0

MCS_merged$Single_parent[MCS_merged$APFCIN00==1 | MCS_merged$BPFCIN00==1 | 
                                     MCS_merged$CPFCIN00==1 | MCS_merged$DPFCIN00==1 |
                                     MCS_merged$EPFCIN00==1 | MCS_merged$FPFCIN00==1] <- 1

# If missing data for all sweeps 1-6, code to NA
MCS_merged$Single_parent[is.na(MCS_merged$APFCIN00) & is.na(MCS_merged$BPFCIN00) & 
                                     is.na(MCS_merged$CPFCIN00) & is.na(MCS_merged$DPFCIN00) &
                                     is.na(MCS_merged$EPFCIN00) & is.na(MCS_merged$FPFCIN00)] <- NA 

table(MCS_merged$Single_parent, useNA="always")

## ================== Parental relationship happiness (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years) ==================
# Factor variable with 7 levels; originally coded on a scale of "Very unhappy" to "Very happy" 
# "Very unhappy" to 3 will be recoded to 1; 4 to "Very happy" will be recoded to 0 

### (Sweep 1, 9 months)
table(MCS_merged$APHARE00, useNA="always")

MCS_merged$APHARE00 <- recode(MCS_merged$APHARE00, "'Very unhappy'=1; '2'=1; '3'=1; '4'=0; '5'=0; '6'=0; 'Very happy'=0")

MCS_merged$APHARE00 <- as.numeric(as.character(MCS_merged$APHARE00))

table(MCS_merged$APHARE00, useNA="always")

### (Sweep 2, 3 years)
table(MCS_merged$BPHARE00, useNA="always")

MCS_merged$BPHARE00 <- recode(MCS_merged$BPHARE00, "\"Very unhappy\"=1; \"2\"=1; \"3\"=1; \"4\"=0; \"5\"=0; \"6\"=0; \"Very happy\"=0; \"Can't say\"=NA")

MCS_merged$BPHARE00 <- as.numeric(as.character(MCS_merged$BPHARE00))

table(MCS_merged$BPHARE00, useNA="always")

### (Sweep 3, 5 years)
table(MCS_merged$CPHARE00, useNA="always")

MCS_merged$CPHARE00 <- recode(MCS_merged$CPHARE00, "\"Very unhappy\"=1; \"2\"=1; \"3\"=1; \"4\"=0; \"5\"=0; \"6\"=0; \"Very happy\"=0; \"Can't say\"=NA")

MCS_merged$CPHARE00 <- as.numeric(as.character(MCS_merged$CPHARE00))

table(MCS_merged$CPHARE00, useNA="always")

### (Sweep 4, 7 years)
table(MCS_merged$DPHARE00, useNA="always")

MCS_merged$DPHARE00 <- recode(MCS_merged$DPHARE00, "\"Very unhappy\"=1; \"2\"=1; \"3\"=1; \"4\"=0; \"5\"=0; \"6\"=0; \"Very happy\"=0; \"Can't say\"=NA")

MCS_merged$DPHARE00 <- as.numeric(as.character(MCS_merged$DPHARE00))

table(MCS_merged$DPHARE00, useNA="always")

### (Sweep 5, 11 years)
table(MCS_merged$EPHARE00, useNA="always")

MCS_merged$EPHARE00 <- recode(MCS_merged$EPHARE00, "'Very Unhappy   '=1; '2'=1; '3'=1; '4'=0; '5'=0; '6'=0; 'Very Happy     '=0")

MCS_merged$EPHARE00 <- as.numeric(as.character(MCS_merged$EPHARE00))

table(MCS_merged$EPHARE00, useNA="always")

### (Sweep 6, 14 years)
table(MCS_merged$FPHARE00, useNA="always")

MCS_merged$FPHARE00 <- recode(MCS_merged$FPHARE00, "'Very unhappy   '=1; '2'=1; '3'=1; '4'=0; '5'=0; '6'=0; 'Very happy     '=0")

MCS_merged$FPHARE00 <- as.numeric(as.character(MCS_merged$FPHARE00))

table(MCS_merged$FPHARE00, useNA="always")

### SUM PARENTAL RELATIONSHIP SCORES ACROSS SWEEPS 1,2,3,4,5,6

# If parents scored unhappy relationship for at least 1 sweep, code as 1 
MCS_merged$Parental_relationship <- 0

MCS_merged$Parental_relationship[MCS_merged$APHARE00==1 | MCS_merged$BPHARE00==1 | 
                                     MCS_merged$CPHARE00==1 | MCS_merged$DPHARE00==1 |
                                     MCS_merged$EPHARE00==1 | MCS_merged$FPHARE00==1] <- 1

# If missing data for all sweeps 1-6, code to NA
MCS_merged$Parental_relationship[is.na(MCS_merged$APHARE00) & is.na(MCS_merged$BPHARE00) & 
                                     is.na(MCS_merged$CPHARE00) & is.na(MCS_merged$DPHARE00) &
                                     is.na(MCS_merged$EPHARE00) & is.na(MCS_merged$FPHARE00)] <- NA 

table(MCS_merged$Parental_relationship, useNA="always")

## ================== Domestic violence (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years) ==================
# Factor variable with 3 levels; originally coded as "Yes" or "No" or "Don't want to answer"
# "Yes" will be recoded to 1; "No" to 0

### (Sweep 1, 9 months)
table(MCS_merged$APFORC00, useNA="always")

MCS_merged$APFORC00 <- recode(MCS_merged$APFORC00, "'Yes'=1; 'No'=0; 'Don t want to answer'=NA")

MCS_merged$APFORC00 <- as.numeric(as.character(MCS_merged$APFORC00))

table(MCS_merged$APFORC00, useNA="always")

### (Sweep 2, 3 years)
table(MCS_merged$BPFORC00, useNA="always")

MCS_merged$BPFORC00 <- recode(MCS_merged$BPFORC00, "\"Refusal\"=NA; \"Yes\"=1; \"No\"=0; \"Don't want to answer\"=NA")

MCS_merged$BPFORC00 <- as.numeric(as.character(MCS_merged$BPFORC00))

table(MCS_merged$BPFORC00, useNA="always")

### (Sweep 3, 5 years)
table(MCS_merged$CPFORC00, useNA="always")

MCS_merged$CPFORC00 <- recode(MCS_merged$CPFORC00, "'Yes'=1; 'No'=0; 'Don t want to answer'=NA")

MCS_merged$CPFORC00 <- as.numeric(as.character(MCS_merged$CPFORC00))

table(MCS_merged$CPFORC00, useNA="always")

### (Sweep 4, 7 years)
table(MCS_merged$DPFORC00, useNA="always")

MCS_merged$DPFORC00 <- recode(MCS_merged$DPFORC00, "'Yes    '=1; 'No     '=0; 'Don t want to answer   '=NA")

MCS_merged$DPFORC00 <- as.numeric(as.character(MCS_merged$DPFORC00))

table(MCS_merged$DPFORC00, useNA="always")

### (Sweep 5, 11 years)
table(MCS_merged$EPFORC00, useNA="always")

MCS_merged$EPFORC00 <- recode(MCS_merged$EPFORC00, "\"Yes    \"=1; \"No     \"=0; \"Don’t know/don’t wish to answer\"=NA")

MCS_merged$EPFORC00 <- as.numeric(as.character(MCS_merged$EPFORC00))

table(MCS_merged$EPFORC00, useNA="always")

### (Sweep 6, 14 years)
table(MCS_merged$FPFORC00, useNA="always")

MCS_merged$FPFORC00 <- recode(MCS_merged$FPFORC00, "'Yes    '=1; 'No     '=0")

MCS_merged$FPFORC00 <- as.numeric(as.character(MCS_merged$FPFORC00))

table(MCS_merged$FPFORC00, useNA="always")

### SUM DOMESTIC VIOLENCE SCORES ACROSS SWEEPS 1,2,3,4,5,6

# If parents scored domestic violence for at least 1 sweep, code as 1
MCS_merged$Domestic_violence <- 0

MCS_merged$Domestic_violence[MCS_merged$APFORC00==1 | MCS_merged$BPFORC00==1 | 
                                     MCS_merged$CPFORC00==1 | MCS_merged$DPFORC00==1 |
                                     MCS_merged$EPFORC00==1 | MCS_merged$FPFORC00==1] <- 1

# If missing data for all sweeps 1-6, code to NA
MCS_merged$Domestic_violence[is.na(MCS_merged$APFORC00) & is.na(MCS_merged$BPFORC00) & 
                                     is.na(MCS_merged$CPFORC00) & is.na(MCS_merged$DPFORC00) &
                                     is.na(MCS_merged$EPFORC00) & is.na(MCS_merged$FPFORC00)] <- NA 

table(MCS_merged$Domestic_violence, useNA="always")

## ================== Parental discipline: Straus Conflict Tactics Scale (Sweeps 2-4, ages 3, 5, 7 years) ==================
# Factor variable with 6 levels; originally coded "Never", "Rarely", "Once a month", "Once a week or more", "Daily", "Can't say"
# "Once a month", "Once a week or more", and "Daily" will be recoded to 1; "Never" and "Rarely" to 0
# Then sum them together into one parental discipline variable where higher score = harsher parental discipline
# Then we will recode the summed parental discipline variable to be binary according to a score cutoff

### (Sweep 2, age 3)

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$BPDIIG00, useNA="always")
table(MCS_merged$BPDISH00, useNA="always")
table(MCS_merged$BPDIBN00, useNA="always")
table(MCS_merged$BPDITR00, useNA="always")
table(MCS_merged$BPDITE00, useNA="always")
table(MCS_merged$BPDIBR00, useNA="always")

# Recode variables to binary
MCS_merged <- MCS_merged %>% 
  mutate_at(c("BPDIIG00","BPDISH00","BPDIBN00","BPDITR00","BPDITE00","BPDIBR00"), 
            function(x) recode(x, "\"Never\"=0; \"Rarely\"=0; \"Once a month\"=1; \"Once a week or more\"=1; \"Daily\"=1; \"Can't say\"=NA"))

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("BPDIIG00","BPDISH00","BPDIBN00","BPDITR00","BPDITE00","BPDIBR00"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly 
table(MCS_merged$BPDIIG00, useNA="always")
table(MCS_merged$BPDISH00, useNA="always")
table(MCS_merged$BPDIBN00, useNA="always")
table(MCS_merged$BPDITR00, useNA="always")
table(MCS_merged$BPDITE00, useNA="always")
table(MCS_merged$BPDIBR00, useNA="always")

# Sum items together into Straus_MCS2
MCS_merged$Straus_MCS2 = NA

MCS_merged$Straus_MCS2 = apply(MCS_merged[,c("BPDIIG00","BPDISH00","BPDIBN00","BPDITR00","BPDITE00","BPDIBR00")],1, mean, na.rm=T)*6 # multiply average of 6 items by 6

MCS_merged$Straus_MCS2[apply(apply(MCS_merged[,c("BPDIIG00","BPDISH00","BPDIBN00","BPDITR00","BPDITE00","BPDIBR00")],2,is.na),1,sum) >3] = NA # make NA when missing more than 3 items

describe(MCS_merged$Straus_MCS2)
table(MCS_merged$Straus_MCS2, useNA="always")

# If scores >=5, code to 1 for harsh parental discipline, else code to 0
MCS_merged$Straus_MCS2 <- ifelse(MCS_merged$Straus_MCS2>=5, 1, 0)
table(MCS_merged$Straus_MCS2, useNA="always")

### (Sweep 3, age 5)

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$CPDIIG00, useNA="always")
table(MCS_merged$CPDISH00, useNA="always")
table(MCS_merged$CPDIBN00, useNA="always")
table(MCS_merged$CPDITR00, useNA="always")
table(MCS_merged$CPDITE00, useNA="always")
table(MCS_merged$CPDIBR00, useNA="always")

# Recode variables to binary
MCS_merged <- MCS_merged %>% 
  mutate_at(c("CPDIIG00","CPDISH00","CPDIBN00","CPDITR00","CPDITE00","CPDIBR00"), 
            function(x) recode(x, "'Never'=0; 'Rarely'=0; 'Sometimes (about once a month)'=1; 'Often (about once a week or more)'=1; 'Daily'=1; 'Cant say'=NA"))

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("CPDIIG00","CPDISH00","CPDIBN00","CPDITR00","CPDITE00","CPDIBR00"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly 
table(MCS_merged$CPDIIG00, useNA="always")
table(MCS_merged$CPDISH00, useNA="always")
table(MCS_merged$CPDIBN00, useNA="always")
table(MCS_merged$CPDITR00, useNA="always")
table(MCS_merged$CPDITE00, useNA="always")
table(MCS_merged$CPDIBR00, useNA="always")

# Sum items together into Straus_MCS3
MCS_merged$Straus_MCS3 = NA

MCS_merged$Straus_MCS3 = apply(MCS_merged[,c("CPDIIG00","CPDISH00","CPDIBN00","CPDITR00","CPDITE00","CPDIBR00")],1, mean, na.rm=T)*6 # multiply average of 6 items by 6

MCS_merged$Straus_MCS3[apply(apply(MCS_merged[,c("CPDIIG00","CPDISH00","CPDIBN00","CPDITR00","CPDITE00","CPDIBR00")],2,is.na),1,sum) >3] = NA # make NA when missing more than 3 items

describe(MCS_merged$Straus_MCS3)
table(MCS_merged$Straus_MCS3, useNA="always")

# If scores >=5, code to 1 for harsh parental discipline, else code to 0
MCS_merged$Straus_MCS3 <- ifelse(MCS_merged$Straus_MCS3>=5, 1, 0)
table(MCS_merged$Straus_MCS3, useNA="always")

### (Sweep 4, age 7)

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$DPDIIG00, useNA="always")
table(MCS_merged$DPDISH00, useNA="always")
table(MCS_merged$DPDIBN00, useNA="always")
table(MCS_merged$DPDITR00, useNA="always")
table(MCS_merged$DPDITE00, useNA="always")
table(MCS_merged$DPDIBR00, useNA="always")

# Recode variables to binary
MCS_merged <- MCS_merged %>% 
  mutate_at(c("DPDIIG00","DPDISH00","DPDIBN00","DPDITR00","DPDITE00","DPDIBR00"), 
            function(x) recode(x, "'Never  '=0; 'Rarely '=0; 'Sometimes (about once a month) '=1; 'Often (about once a week or more)      '=1; 'Daily  '=1; 'Cant say       '=NA"))

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("DPDIIG00","DPDISH00","DPDIBN00","DPDITR00","DPDITE00","DPDIBR00"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly 
table(MCS_merged$DPDIIG00, useNA="always")
table(MCS_merged$DPDISH00, useNA="always")
table(MCS_merged$DPDIBN00, useNA="always")
table(MCS_merged$DPDITR00, useNA="always")
table(MCS_merged$DPDITE00, useNA="always")
table(MCS_merged$DPDIBR00, useNA="always")

# Sum items together into Straus_MCS4
MCS_merged$Straus_MCS4 = NA

MCS_merged$Straus_MCS4 = apply(MCS_merged[,c("DPDIIG00","DPDISH00","DPDIBN00","DPDITR00","DPDITE00","DPDIBR00")],1, mean, na.rm=T)*6 # multiply average of 6 items by 6

MCS_merged$Straus_MCS4[apply(apply(MCS_merged[,c("DPDIIG00","DPDISH00","DPDIBN00","DPDITR00","DPDITE00","DPDIBR00")],2,is.na),1,sum) >3] = NA # make NA when missing more than 3 items

describe(MCS_merged$Straus_MCS4)
table(MCS_merged$Straus_MCS4, useNA="always")

# If scores >=5, code to 1 for harsh parental discipline, else code to 0
MCS_merged$Straus_MCS4 <- ifelse(MCS_merged$Straus_MCS4>=5, 1, 0)
table(MCS_merged$Straus_MCS4, useNA="always")

### SUM PARENTAL DISCIPLINE SCORES ACROSS SWEEPS 2,3,4

# If parents scored using harsh discipline for at least 1 sweep, code as 1 
MCS_merged$Parental_discipline <- 0

MCS_merged$Parental_discipline[MCS_merged$Straus_MCS2==1 | MCS_merged$Straus_MCS3==1 | 
                                     MCS_merged$Straus_MCS4==1] <- 1

# If missing data for all sweeps 2,3,4, code to NA
MCS_merged$Parental_discipline[is.na(MCS_merged$Straus_MCS2) & is.na(MCS_merged$Straus_MCS3) & 
                                     is.na(MCS_merged$Straus_MCS4)] <- NA 

table(MCS_merged$Parental_discipline, useNA="always")

## ================== Parental smacking: Straus Conflict Tactics Scale (Sweeps 2-4, ages 3, 5, 7 years) ==================
# Factor variable with 6 levels; originally coded "Never", "Rarely", "Once a month", "Once a week or more", "Daily", "Can't say"
# "Once a month", "Once a week or more", and "Daily" will be recoded to 1; "Never" and "Rarely" to 0

### (Sweep 2, age 3)
table(MCS_merged$BPDISM00, useNA="always")

MCS_merged$BPDISM00 <- recode(MCS_merged$BPDISM00, "\"Never\"=0; \"Rarely\"=0; \"Once a month\"=1; \"Once a week or more\"=1; \"Daily\"=1; \"Can't say\"=NA")

MCS_merged$BPDISM00 <- as.numeric(as.character(MCS_merged$BPDISM00))

table(MCS_merged$BPDISM00, useNA="always")

### (Sweep 3, age 5)
table(MCS_merged$CPDISM00, useNA="always")

MCS_merged$CPDISM00 <- recode(MCS_merged$CPDISM00, "'Never'=0; 'Rarely'=0; 'Sometimes (about once a month)'=1; 'Often (about once a week or more)'=1; 'Daily'=1; 'Cant say'=NA")

MCS_merged$CPDISM00 <- as.numeric(as.character(MCS_merged$CPDISM00))

table(MCS_merged$CPDISM00, useNA="always")

### (Sweep 4, age 7)
table(MCS_merged$DPDISM00, useNA="always")

MCS_merged$DPDISM00 <- recode(MCS_merged$DPDISM00, "'Never  '=0; 'Rarely '=0; 'Sometimes (about once a month) '=1; 'Often (about once a week or more)      '=1; 'Daily  '=1; 'Cant say       '=NA")

MCS_merged$DPDISM00 <- as.numeric(as.character(MCS_merged$DPDISM00))

table(MCS_merged$DPDISM00, useNA="always")

### SUM PARENTAL SMACKING SCORES ACROSS SWEEPS 2,3,4

# If parents scored using frequent smacking for at least 1 sweep, code as 1 
MCS_merged$Parental_smacking <- 0

MCS_merged$Parental_smacking[MCS_merged$BPDISM00==1 | MCS_merged$CPDISM00==1 | 
                                     MCS_merged$DPDISM00==1] <- 1

# If missing data for all sweeps 2,3,4, code to NA
MCS_merged$Parental_smacking[is.na(MCS_merged$BPDISM00) & is.na(MCS_merged$CPDISM00) & 
                                     is.na(MCS_merged$DPDISM00)] <- NA 

table(MCS_merged$Parental_smacking, useNA="always")

## ================== Home environment: HOME-SF (Sweep 2, age 3) ==================
# Factor variable with 2-4 levels; originally coded for different observations such as "Did not scold", "Scolded more than once", "Did not use restraint", "Restraint", "Did not slap or spank", "Slapped or spank" etc.
# For example, "Scolded more than once", "Restraint", and "Slapped or spank" will be recoded to 1; "Did not scold", "Did not use restraint", and "Did not slap or spank" to 0
# Then sum them together into one home environment variable where higher score = negative home environment
# Then we will recode the summed home environment variable to be binary according to a score cutoff

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$BCENVI00, useNA="always") # child's in-home play environment safe
table(MCS_merged$BCTOYS00, useNA="always") # parents provided toys during visit
table(MCS_merged$BCSEEC00, useNA="always") # parent kept child in vision
table(MCS_merged$BCCOMF00, useNA="always") # how at ease did parent appear
table(MCS_merged$BCDARK00, useNA="always") # interior of home dark
table(MCS_merged$BCRCLE00, useNA="always") # house/flat reasonably clean
table(MCS_merged$BCUNCL00, useNA="always") # house/flat reasonably uncluttered
table(MCS_merged$BCSPEA00, useNA="always") # mother's voice positive when speaking to child
table(MCS_merged$BCMCON00, useNA="always") # mother converses at least twice with child
table(MCS_merged$BCANSW00, useNA="always") # mother answers child's questions verbally
table(MCS_merged$BCPRAI00, useNA="always") # mother praises child spontaneously 
table(MCS_merged$BCKISS00, useNA="always") # mother caresses or kisses child
table(MCS_merged$BCINTI00, useNA="always") # mother introduces interviewer to child 
table(MCS_merged$BCSCOL00, useNA="always") # mother scolded child more than once
table(MCS_merged$BCPHYS00, useNA="always") # mother used physical restraint on child
table(MCS_merged$BCSLAP00, useNA="always") # mother slapped or spanked child

# Recode variables to 1/0

# Some levels need to be recoded using plyr:revalue instead of car:recode because the = sign in "= Can't tell - including does not speak English" causes a string parsing error

# Child's in-home play environment safe
MCS_merged$BCENVI00 <- recode(MCS_merged$BCENVI00, "'Safe   '=0; 'Not safe       '=1; 'Not observed   '=NA")
MCS_merged$BCENVI00 <- as.numeric(as.character(MCS_merged$BCENVI00))

# Parents provided toys during visit
MCS_merged$BCTOYS00 <- recode(MCS_merged$BCTOYS00, "'Provided toys  '=0; 'Did not provide toys   '=1; 'Not observed   '=NA")
MCS_merged$BCTOYS00 <- as.numeric(as.character(MCS_merged$BCTOYS00))

# Parent kept child in vision
MCS_merged$BCSEEC00 <- recode(MCS_merged$BCSEEC00, "'In range       '=0; 'Child only present during assessment   '=0; 'Not in range   '=1")
MCS_merged$BCSEEC00 <- as.numeric(as.character(MCS_merged$BCSEEC00))

# How at ease did parent appear
MCS_merged$BCCOMF00 <- recode(MCS_merged$BCCOMF00, "'Completely comfortable and at ease     '=0; 'Moderately comfortable '=0; 'Slightly ill at ease   '=0; 'Very uncomfortable     '=1")
MCS_merged$BCCOMF00 <- as.numeric(as.character(MCS_merged$BCCOMF00))

# Interior of home dark
MCS_merged$BCDARK00 <- recode(MCS_merged$BCDARK00, "'No     '=0; 'Yes    '=1; 'Not observed   '=NA")
MCS_merged$BCDARK00 <- as.numeric(as.character(MCS_merged$BCDARK00))

# House/flat reasonably clean
MCS_merged$BCRCLE00 <- recode(MCS_merged$BCRCLE00, "'Yes    '=0; 'No     '=1; 'Not observed   '=NA")
MCS_merged$BCRCLE00 <- as.numeric(as.character(MCS_merged$BCRCLE00))

# House/flat reasonably uncluttered
MCS_merged$BCUNCL00 <- recode(MCS_merged$BCUNCL00, "'Yes    '=0; 'No     '=1")
MCS_merged$BCUNCL00 <- as.numeric(as.character(MCS_merged$BCUNCL00))

# Mother's voice positive when speaking to child
MCS_merged$BCSPEA00 <- revalue(MCS_merged$BCSPEA00, c("Positive       "=0, "Not positive   "=1, "= Can't tell - including does not speak English"=NA))
MCS_merged$BCSPEA00 <- as.numeric(as.character(MCS_merged$BCSPEA00))

# Mother converses at least twice with child
MCS_merged$BCMCON00 <- revalue(MCS_merged$BCMCON00, c("Converses      "=0, "Did not converse       "=1, "= Can't tell - including does not speak English"=NA))
MCS_merged$BCMCON00 <- as.numeric(as.character(MCS_merged$BCMCON00))

# Mother answers child's questions verbally
MCS_merged$BCANSW00 <- revalue(MCS_merged$BCANSW00, c("Answers"=0, "Did not answer "=1, "= Can't tell - including does not speak English"=NA))
MCS_merged$BCANSW00 <- as.numeric(as.character(MCS_merged$BCANSW00))

# Mother praises child spontaneously
MCS_merged$BCPRAI00 <- revalue(MCS_merged$BCPRAI00, c("Spontaneous:praise more than once      "=0, "Not Spontaneous:praise "=1, "= Can't tell - including does not speak English"=NA))
MCS_merged$BCPRAI00 <- as.numeric(as.character(MCS_merged$BCPRAI00))

# Mother caresses or kisses child
MCS_merged$BCKISS00 <- recode(MCS_merged$BCKISS00, "'Affectionate   '=0; 'Not affectionate       '=1")
MCS_merged$BCKISS00 <- as.numeric(as.character(MCS_merged$BCKISS00))

# Mother introduces interviewer to child
MCS_merged$BCINTI00 <- revalue(MCS_merged$BCINTI00, c("Introduce      "=0, "Did not introduce      "=1, "= Can't tell - including does not speak English"=NA))
MCS_merged$BCINTI00 <- as.numeric(as.character(MCS_merged$BCINTI00))

# “Mother scolded child more than once”
MCS_merged$BCSCOL00 <- revalue(MCS_merged$BCSCOL00, c("Did not scold  "=0, "Scolded more than once "=1, "= Can't tell - including does not speak English"=NA))
MCS_merged$BCSCOL00 <- as.numeric(as.character(MCS_merged$BCSCOL00))

# “Mother used physical restraint on child”
MCS_merged$BCPHYS00 <- recode(MCS_merged$BCPHYS00, "'Did not use restraint  '=0; 'Restraint      '=1")
MCS_merged$BCPHYS00 <- as.numeric(as.character(MCS_merged$BCPHYS00))

# “Mother slapped or spanked child”
MCS_merged$BCSLAP00 <- recode(MCS_merged$BCSLAP00, "'Did not slap or spank  '=0; 'Slapped or spanked     '=1")
MCS_merged$BCSLAP00 <- as.numeric(as.character(MCS_merged$BCSLAP00))

# After recoding, run tables to check we recoded it correctly 
table(MCS_merged$BCENVI00, useNA="always") # child's in-home play environment safe
table(MCS_merged$BCTOYS00, useNA="always") # parents provided toys during visit
table(MCS_merged$BCSEEC00, useNA="always") # parent kept child in vision
table(MCS_merged$BCCOMF00, useNA="always") # how at ease did parent appear
table(MCS_merged$BCDARK00, useNA="always") # interior of home dark
table(MCS_merged$BCRCLE00, useNA="always") # house/flat reasonably clean
table(MCS_merged$BCUNCL00, useNA="always") # house/flat reasonably uncluttered
table(MCS_merged$BCSPEA00, useNA="always") # mother's voice positive when speaking to child
table(MCS_merged$BCMCON00, useNA="always") # mother converses at least twice with child
table(MCS_merged$BCANSW00, useNA="always") # mother answers child's questions verbally
table(MCS_merged$BCPRAI00, useNA="always") # mother praises child spontaneously 
table(MCS_merged$BCKISS00, useNA="always") # mother caresses or kisses child
table(MCS_merged$BCINTI00, useNA="always") # mother introduces interviewer to child 
table(MCS_merged$BCSCOL00, useNA="always") # mother scolded child more than once
table(MCS_merged$BCPHYS00, useNA="always") # mother used physical restraint on child
table(MCS_merged$BCSLAP00, useNA="always") # mother slapped or spanked child

# Sum items together into Home_environment
MCS_merged$Home_environment = NA

MCS_merged$Home_environment = apply(MCS_merged[,c("BCENVI00","BCTOYS00","BCSEEC00","BCCOMF00","BCDARK00","BCRCLE00","BCUNCL00","BCSPEA00","BCMCON00","BCANSW00","BCPRAI00","BCKISS00","BCINTI00","BCSCOL00","BCPHYS00","BCSLAP00")],1, mean, na.rm=T)*16 # multiply average of 16 items by 16

MCS_merged$Home_environment[apply(apply(MCS_merged[,c("BCENVI00","BCTOYS00","BCSEEC00","BCCOMF00","BCDARK00","BCRCLE00","BCUNCL00","BCSPEA00","BCMCON00","BCANSW00","BCPRAI00","BCKISS00","BCINTI00","BCSCOL00","BCPHYS00","BCSLAP00")],2,is.na),1,sum) >7] = NA # make NA when missing more than 7 items

describe(MCS_merged$Home_environment)
table(MCS_merged$Home_environment, useNA="always")

describe(MCS_merged$Home_environment) # Mean = 1.22, SD = 1.67
# A home environment score of two standard deviations above the mean would be considered unsafe

# If scores >=4.56, code to 1 for negative home environment, else code to 0
MCS_merged$Home_environment <- ifelse(MCS_merged$Home_environment>=4.56, 1, 0)
table(MCS_merged$Home_environment, useNA="always")

## ================== Peer victimisation (Sweeps 2-6, ages 3, 5, 7, 11, 14 years) ==================
# Factor variable with 3-6 levels; coded as "Not true", "Somewhat true", "Certainly true", "Can't say" (Parent and Teacher), "All of the time", "Some of the time", "Never" / "Most days", "About once a week", "About once a month", "Every few months", "Less often", "Never" (Child)
# "Certainly true" will be recoded to 1; "Somewhat true" and "Not true" to 0; "Can't say" to NA
# "All of the time" will be recoded to 1; "Some of the time" and "Never" to 0
# "Most days" and "About once a week" will be recoded to 1; "About once a month", "Every few months", "Less often" and "Never" to 0

# PARENT REPORTS OF BULLYING 

### (Sweep 2, age 3)
table(MCS_merged$BPSDPB00, useNA="always") # child picked on or bullied by other children 

MCS_merged$BPSDPB00 <- recode(MCS_merged$BPSDPB00, "\"Not true\"=0; \"Somewhat true\"=0; \"Certainly true\"=1; \"Can't say\"=NA")

MCS_merged$BPSDPB00 <- as.numeric(as.character(MCS_merged$BPSDPB00))

table(MCS_merged$BPSDPB00, useNA="always") # child picked on or bullied by other children 

### (Sweep 3, age 5)
table(MCS_merged$CPSDPB00, useNA="always") # child picked on or bullied by other children

MCS_merged$CPSDPB00 <- recode(MCS_merged$CPSDPB00, "'Not true'=0; 'Somewhat true'=0; 'Certainly true'=1; 'Can t say'=NA")

MCS_merged$CPSDPB00 <- as.numeric(as.character(MCS_merged$CPSDPB00))

table(MCS_merged$CPSDPB00, useNA="always") # child picked on or bullied by other children

### (Sweep 4, age 7)
table(MCS_merged$DPSDPB00, useNA="always") # child picked on or bullied by other children

MCS_merged$DPSDPB00 <- recode(MCS_merged$DPSDPB00, "'Not true       '=0; 'Somewhat true  '=0; 'Certainly true '=1; 'Can t say      '=NA")

MCS_merged$DPSDPB00 <- as.numeric(as.character(MCS_merged$DPSDPB00))

table(MCS_merged$DPSDPB00, useNA="always") # child picked on or bullied by other children

### (Sweep 6, age 14)
table(MCS_merged$FPSDPB00, useNA="always") # child picked on or bullied by other children

MCS_merged$FPSDPB00 <- recode(MCS_merged$FPSDPB00, "'Not True       '=0; 'Somewhat true  '=0; 'Certainly true '=1")

MCS_merged$FPSDPB00 <- as.numeric(as.character(MCS_merged$FPSDPB00))

table(MCS_merged$FPSDPB00, useNA="always") # child picked on or bullied by other children

# CHILD REPORTS OF BULLYING 

### (Sweep 4, age 7)
table(MCS_merged$DCSC0036, useNA="always") # how often do other children bully you?

MCS_merged$DCSC0036 <- recode(MCS_merged$DCSC0036, "'All of the time'=1; 'Some of the time       '=0; 'Never  '=0")

MCS_merged$DCSC0036 <- as.numeric(as.character(MCS_merged$DCSC0036))

table(MCS_merged$DCSC0036, useNA="always") # child picked on or bullied by other children

### (Sweep 5, age 11)
table(MCS_merged$EPSDPB00, useNA="always") # child picked on or bullied by other children

MCS_merged$EPSDPB00 <- recode(MCS_merged$EPSDPB00, "\"Not true       \"=0; \"Somewhat true  \"=0; \"Certainly true \"=1; \"Don’t know/Don’t wish to answer\"=NA")

MCS_merged$EPSDPB00 <- as.numeric(as.character(MCS_merged$EPSDPB00))

table(MCS_merged$EPSDPB00, useNA="always") # child picked on or bullied by other children

### (Sweep 5, age 11)
table(MCS_merged$ECQ56X00, useNA="always") # how often do other children hurt you or pick on you on purpose? 

MCS_merged$ECQ56X00 <- recode(MCS_merged$ECQ56X00, "'Most days      '=1; 'About once a week      '=1; 'About once a month     '=0; 'Every few months       '=0; 'Less often     '=0; 'Never  '=0")

MCS_merged$ECQ56X00 <- as.numeric(as.character(MCS_merged$ECQ56X00))

table(MCS_merged$ECQ56X00, useNA="always") # child picked on or bullied by other children

### (Sweep 6, age 14)
table(MCS_merged$FCHURT00, useNA="always") # how often other children hurt or pick on child

MCS_merged$FCHURT00 <- recode(MCS_merged$FCHURT00, "'Most days      '=1; 'About once a week      '=1; 'About once a month     '=0; 'Every few months       '=0; 'Less often     '=0; 'Never  '=0")

MCS_merged$FCHURT00 <- as.numeric(as.character(MCS_merged$FCHURT00))

table(MCS_merged$FCHURT00, useNA="always") # child picked on or bullied by other children

# TEACHER REPORTS OF BULLYING

### (Sweep 4, age 7)
table(MCS_merged$DQ2189, useNA="always") # is picked on or bullied by other children 

MCS_merged$DQ2189 <- recode(MCS_merged$DQ2189, "'Not true       '=0; 'Somewhat true  '=0; 'Certainly true '=1")

MCS_merged$DQ2189 <- as.numeric(as.character(MCS_merged$DQ2189))

table(MCS_merged$DQ2189, useNA="always") # child picked on or bullied by other children

### (Sweep 5, age 11)
table(MCS_merged$EQ5S, useNA="always") # is picked on or bullied by other children 

MCS_merged$EQ5S <- recode(MCS_merged$EQ5S, "'Not true       '=0; 'Is somewhat true       '=0; 'Very true      '=1")

MCS_merged$EQ5S <- as.numeric(as.character(MCS_merged$EQ5S))

table(MCS_merged$EQ5S, useNA="always") # child picked on or bullied by other children

### SUM PEER VICTIMISATION SCORES ACROSS SWEEPS 2-6

# If child experienced peer victimisation for at least 1 sweep, code as 1 
MCS_merged$Peer_victimisation <- 0

MCS_merged$Peer_victimisation[MCS_merged$BPSDPB00==1 | MCS_merged$CPSDPB00==1 |
                           MCS_merged$DPSDPB00==1 | MCS_merged$FPSDPB00==1 | 
                           MCS_merged$DCSC0036==1 | MCS_merged$EPSDPB00==1 | 
                           MCS_merged$ECQ56X00==1 | MCS_merged$FCHURT00==1 |
                           MCS_merged$DQ2189==1 | MCS_merged$EQ5S==1] <- 1

# If missing data for all sweeps 2-6, code to NA
MCS_merged$Peer_victimisation[is.na(MCS_merged$BPSDPB00) & is.na(MCS_merged$CPSDPB00) &
                           is.na(MCS_merged$DPSDPB00) & is.na(MCS_merged$FPSDPB00) & 
                           is.na(MCS_merged$DCSC0036) & is.na(MCS_merged$EPSDPB00) & 
                           is.na(MCS_merged$ECQ56X00) & is.na(MCS_merged$FCHURT00) & 
                           is.na(MCS_merged$DQ2189) & is.na(MCS_merged$EQ5S)] <- NA 

table(MCS_merged$Peer_victimisation, useNA="always")

## ================== Sibling victimisation (Sweeps 5-6, ages 11, 14) ==================
# Factor variable with 7 levels; coded as "Most days", "About once a week", "About once a month", "Every few months", "Less often", "Never", "Don't have brothers or sisters"
# "Most days" will be recoded to 1; "About once a week", "About once a month", "Every few months", "Less often", "Not at all", and "Don't have brothers or sisters" to 0

# Before recoding, run tables to see the distribution of the variable as a factor
table(MCS_merged$ECQ54X00, useNA="always")
table(MCS_merged$FCBULB00, useNA="always")

# Recode variables to binary
MCS_merged <- MCS_merged %>% 
  mutate_at(c("ECQ54X00","FCBULB00"), 
            function(x) recode(x, "\"Most days      \"=1; \"About once a week      \"=0; \"About once a month     \"=0; \"Every few months       \"=0; \"Less often     \"=0; \"Never  \"=0; \"Don't have brothers or sisters \"=0"))

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("ECQ54X00","FCBULB00"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly 
table(MCS_merged$ECQ54X00, useNA="always")
table(MCS_merged$FCBULB00, useNA="always")

### SUM SIBLING VICTIMISATION SCORES ACROSS SWEEPS 5-6

# If child experienced sibling victimisation for at least 1 sweep, code as 1 
MCS_merged$Sibling_victimisation <- 0

MCS_merged$Sibling_victimisation[MCS_merged$ECQ54X00==1 | MCS_merged$FCBULB00==1] <- 1

# If missing data for all sweeps 5-6, code to NA
MCS_merged$Sibling_victimisation[is.na(MCS_merged$ECQ54X00) & is.na(MCS_merged$FCBULB00)] <- NA 

table(MCS_merged$Sibling_victimisation, useNA="always")

## ================== Child victimisation (Sweep 6, age 14) ==================
# Factor variable with 2 levels; originally coded "Yes" or "No"
# "Yes" will be recoded to 1; "No" to 0

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$FCVICG00, useNA="always") # CM insulted/threatened/shouted at
table(MCS_merged$FCVICA00, useNA="always") # been physically violent towards CM
table(MCS_merged$FCVICC00, useNA="always") # hit or used weapon against CM
table(MCS_merged$FCVICE00, useNA="always") # stolen something from CM
table(MCS_merged$FCVICF0A, useNA="always") # sexually assaulted CM

# Recode variables to binary
MCS_merged <- MCS_merged %>% 
  mutate_at(c("FCVICG00","FCVICA00","FCVICC00","FCVICE00","FCVICF0A"), 
            function(x) recode(x, "'Yes    '=1; 'No     '=0"))

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("FCVICG00","FCVICA00","FCVICC00","FCVICE00","FCVICF0A"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly 
table(MCS_merged$FCVICG00, useNA="always") # CM insulted/threatened/shouted at
table(MCS_merged$FCVICA00, useNA="always") # been physically violent towards CM
table(MCS_merged$FCVICC00, useNA="always") # hit or used weapon against CM
table(MCS_merged$FCVICE00, useNA="always") # stolen something from CM
table(MCS_merged$FCVICF0A, useNA="always") # sexually assaulted CM

# Treat verbal victimisation separately
MCS_merged$Verbal_victimisation <- MCS_merged$FCVICG00

# Sum physically violent and weapon items together as physical victimisation

# If scored at least 1/2 for physically violent and/or weapon, code as 1
MCS_merged$Physical_victimisation <- 0

MCS_merged$Physical_victimisation[MCS_merged$FCVICA00==1 | MCS_merged$FCVICC00==1] <- 1

# If missing data for both items, code to NA
MCS_merged$Physical_victimisation[is.na(MCS_merged$FCVICA00) & is.na(MCS_merged$FCVICC00)] <- NA

table(MCS_merged$Physical_victimisation, useNA="always")

# Treat theft victimisation separately
MCS_merged$Theft_victimisation <- MCS_merged$FCVICE00

# Treat sexual victimisation separately
MCS_merged$Sexual_victimisation <- MCS_merged$FCVICF0A

## ================== Household income OECD Income Weighted Quintiles (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years) ==================
# Factor variable with 5 levels; originally coded "Lowest quintile", "Second quintile", "Third quintile", "Fourth quintile", "Highest quintile"
# "Lowest quintile" will be recoded to 1; all other quintiles to 0

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$AOECDUK0, useNA="always")
table(MCS_merged$BOECDUK0, useNA="always")
table(MCS_merged$COECDUK0, useNA="always")
table(MCS_merged$DOECDUK0, useNA="always")
table(MCS_merged$EOECDUK0, useNA="always")
table(MCS_merged$FOECDUK0, useNA="always")

# Recode variables to binary 
MCS_merged <- MCS_merged %>% 
  mutate_at(c("AOECDUK0","BOECDUK0","COECDUK0","DOECDUK0"), 
            function(x) recode(x, "'Lowest quintile'=1; 'Second quintile'=0; 'Third quintile '=0; 'Fourth quintile'=0; 'Highest quintile       '=0"))

MCS_merged$EOECDUK0 <- recode(MCS_merged$EOECDUK0, "'Bottom '=1; 'Second '=0; 'Third  '=0; 'Fourth '=0; 'Top    '=0")

MCS_merged$FOECDUK0 <- recode(MCS_merged$FOECDUK0, "'Lower quantile '=1; 'Second quantile'=0; 'Third quantile '=0; 'Fourth quantile'=0; 'Highest quantile       '=0")

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("AOECDUK0","BOECDUK0","COECDUK0","DOECDUK0","EOECDUK0","FOECDUK0"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly 
table(MCS_merged$AOECDUK0, useNA="always")
table(MCS_merged$BOECDUK0, useNA="always")
table(MCS_merged$COECDUK0, useNA="always")
table(MCS_merged$DOECDUK0, useNA="always")
table(MCS_merged$EOECDUK0, useNA="always")
table(MCS_merged$FOECDUK0, useNA="always")

### SUM HOUSEHOLD INCOME SCORES ACROSS SWEEPS 1,2,3,4,5,6

# If income scored as lowest quintile for at least 1 sweep, code as 1 
MCS_merged$Household_income <- 0

MCS_merged$Household_income[MCS_merged$AOECDUK0==1 | MCS_merged$BOECDUK0==1 | 
                                     MCS_merged$COECDUK0==1 | MCS_merged$DOECDUK0==1 |
                                     MCS_merged$EOECDUK0==1 | MCS_merged$FOECDUK0==1] <- 1

# If missing data for all sweeps 1-6, code to NA
MCS_merged$Household_income[is.na(MCS_merged$AOECDUK0) & is.na(MCS_merged$BOECDUK0) & 
                                     is.na(MCS_merged$COECDUK0) & is.na(MCS_merged$DOECDUK0) &
                                     is.na(MCS_merged$EOECDUK0) & is.na(MCS_merged$FOECDUK0)] <- NA 

table(MCS_merged$Household_income, useNA="always")

## ================== Neighbourhood deprivation (Sweep 1, 9 months) ==================
# Factor variable with 2-5 levels; originally coded "Very satisfied", "Fairly satisfied" or "Very common", "Fairly common" etc.
# "Fairly dissatisfied" and "Very dissatisfied will be recoded to 1; "Very satisfied", "Fairly satisfied", and "Neither satisfied nor dissatisfied" to 0
# "Very common" and "Fairly common" will be recoded to 1; "Not very common" and "Not at all common" to 0
# "No" will be recoded to 1; "Yes" to 0

# Before recoding, run tables to see the distribution of the variable as a factor
table(MCS_merged$APHOSA00, useNA="always") # satisfaction with home
table(MCS_merged$APAREA00, useNA="always") # satisfaction with area
table(MCS_merged$APARNN00, useNA="always") # noisy neighbours
table(MCS_merged$APARRU00, useNA="always") # how common are rubbish/litter in area
table(MCS_merged$APARVD00, useNA="always") # how common are vandalism in area
table(MCS_merged$APARRC00, useNA="always") # how common are racist insults/attacks in area
table(MCS_merged$APTRAN00, useNA="always") # poor public transport
table(MCS_merged$APSHOP00, useNA="always") # food shops/supermarkets in easy access
table(MCS_merged$APARPG00, useNA="always") # pollution/grime/environmental problems
table(MCS_merged$APPLSA00, useNA="always") # any places where children can play safely

# Recode variables to binary
MCS_merged <- MCS_merged %>% 
  mutate_at(c("APHOSA00","APAREA00"), 
            function(x) recode(x, "'Very satisfied'=0; 'Fairly satisfied'=0; 'Neither satisfied nor dissatisfied'=0; 'Fairly dissatisfied'=1; 'Very dissatisfied'=1"))

MCS_merged <- MCS_merged %>% 
  mutate_at(c("APARNN00","APARRU00","APARVD00","APARRC00","APTRAN00","APARPG00"), 
            function(x) recode(x, "'Very common'=1; 'Fairly common'=1; 'Not very common'=0; 'Not at all common'=0"))

# Food shops/supermarkets in easy access needs to be reverse coded because it is phrased positively 
MCS_merged$APSHOP00 <- recode(MCS_merged$APSHOP00, "'Very common'=0; 'Fairly common'=0; 'Not very common'=1; 'Not at all common'=1")

MCS_merged$APPLSA00 <- recode(MCS_merged$APPLSA00, "'Yes'=0; 'No'=1")

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("APHOSA00", "APAREA00", "APARNN00", "APARRU00", "APARVD00", "APARRC00", "APTRAN00", "APSHOP00", "APARPG00", "APPLSA00"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly 
table(MCS_merged$APHOSA00, useNA="always") # satisfaction with home
table(MCS_merged$APAREA00, useNA="always") # satisfaction with area
table(MCS_merged$APARNN00, useNA="always") # noisy neighbours
table(MCS_merged$APARRU00, useNA="always") # how common are rubbish/litter in area
table(MCS_merged$APARVD00, useNA="always") # how common are vandalism in area
table(MCS_merged$APARRC00, useNA="always") # how common are racist insults/attacks in area
table(MCS_merged$APTRAN00, useNA="always") # poor public transport
table(MCS_merged$APSHOP00, useNA="always") # food shops/supermarkets in easy access
table(MCS_merged$APARPG00, useNA="always") # pollution/grime/environmental problems
table(MCS_merged$APPLSA00, useNA="always") # any places where children can play safely

# Sum items together into Neighbourhood_deprivation
MCS_merged$Neighbourhood_deprivation = NA

MCS_merged$Neighbourhood_deprivation = apply(MCS_merged[,c("APHOSA00", "APAREA00", "APARNN00", "APARRU00", "APARVD00", "APARRC00", "APTRAN00", "APSHOP00", "APARPG00", "APPLSA00")],1, mean, na.rm=T)*10 # multiply average of 10 items by 10

MCS_merged$Neighbourhood_deprivation[apply(apply(MCS_merged[,c("APHOSA00", "APAREA00", "APARNN00", "APARRU00", "APARVD00", "APARRC00", "APTRAN00", "APSHOP00", "APARPG00", "APPLSA00")],2,is.na),1,sum) >5] = NA # make NA when missing more than 5 items

describe(MCS_merged$Neighbourhood_deprivation)
table(MCS_merged$Neighbourhood_deprivation, useNA="always")

# If scores >=5, code to 1 for neighbourhood deprivation, else code to 0
MCS_merged$Neighbourhood_deprivation <- ifelse(MCS_merged$Neighbourhood_deprivation>=5, 1, 0)
table(MCS_merged$Neighbourhood_deprivation, useNA="always")

## ================== Safety of home area (Sweeps 2,3,5, ages 3,5,11) ==================
# Factor variable with 5 levels; originally coded "Excellent", "Good", "Average", "Poor", "Very poor"
# "Poor" and "Very poor" will be recoded to 1; "Excellent", "Good", and "Average" to 0

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$BPARGD00, useNA="always") # is this a good area to bring up a child
table(MCS_merged$CPARGD00, useNA="always") # is this a good area to bring up a child

### (Sweep 2, age 3)
table(MCS_merged$BPARGD00, useNA="always")

MCS_merged$BPARGD00 <- revalue(MCS_merged$BPARGD00, c("Dont know"=NA, "Excellent"=0, "Good"=0, "Average"=0, "Poor"=1, "Very poor"=1, "Spontaneous:ONLY: Don't know"=NA))

MCS_merged$BPARGD00 <- as.numeric(as.character(MCS_merged$BPARGD00))

table(MCS_merged$BPARGD00, useNA="always")

### (Sweep 3, age 5)
table(MCS_merged$CPARGD00, useNA="always")

MCS_merged$CPARGD00 <- recode(MCS_merged$CPARGD00, "'Excellent'=0; 'Good'=0; 'Average'=0; 'Poor'=1; 'Very poor'=1")

MCS_merged$CPARGD00 <- as.numeric(as.character(MCS_merged$CPARGD00))

table(MCS_merged$CPARGD00, useNA="always")

# Factor variable with 5 levels; originally coded "Very safe", "Fairly safe", "Neither safe nor unsafe", "Fairly unsafe", "Very unsafe"
# "Fairly unsafe" and "Very unsafe" will be recoded to 1; "Very safe", "Fairly safe", and "Neither safe nor unsafe", to 0

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$BPARAR00, useNA="always") # how safe do you feel about the area you live in
table(MCS_merged$CPARAR00, useNA="always") # how safe do you feel about the area you live in
table(MCS_merged$ECQ23X00, useNA="always") # how safe is it to walk/play in this area 

### (Sweep 2, age 3)
table(MCS_merged$BPARAR00, useNA="always")

MCS_merged$BPARAR00 <- recode(MCS_merged$BPARAR00, "'Very safe'=0; 'Fairly safe'=0; 'Neither safe nor unsafe'=0; 'Fairly unsafe'=1; 'Very unsafe'=1")

MCS_merged$BPARAR00 <- as.numeric(as.character(MCS_merged$BPARAR00))

table(MCS_merged$BPARAR00, useNA="always")

### (Sweep 3, age 5)
table(MCS_merged$CPARAR00, useNA="always")

MCS_merged$CPARAR00 <- recode(MCS_merged$CPARAR00, "'Very safe'=0; 'Fairly safe'=0; 'Neither safe nor unsafe'=0; 'Fairly unsafe'=1; 'Very unsafe'=1")

MCS_merged$CPARAR00 <- as.numeric(as.character(MCS_merged$CPARAR00))

table(MCS_merged$CPARAR00, useNA="always")

### (Sweep 5, age 11)
table(MCS_merged$ECQ23X00, useNA="always")

MCS_merged$ECQ23X00 <- recode(MCS_merged$ECQ23X00, "'Very safe      '=0; 'Safe   '=0; 'Not very safe  '=1; 'Not at all safe'=1")

MCS_merged$ECQ23X00 <- as.numeric(as.character(MCS_merged$ECQ23X00))

table(MCS_merged$ECQ23X00, useNA="always")

### SUM HOME AREA SAFETY SCORES ACROSS SWEEPS 2,3,5

# If scored as unsafe home area for at least 1 sweep, code as 1 
MCS_merged$Area_safety <- 0

MCS_merged$Area_safety[MCS_merged$BPARGD00==1 | MCS_merged$CPARGD00==1 | 
                                     MCS_merged$BPARAR00==1 | MCS_merged$CPARAR00==1 |
                                     MCS_merged$ECQ23X00==1] <- 1

# If missing data for all sweeps 2,3,5, code to NA
MCS_merged$Area_safety[is.na(MCS_merged$BPARGD00) & is.na(MCS_merged$CPARGD00) & 
                                     is.na(MCS_merged$BPARAR00) & is.na(MCS_merged$CPARAR00) &
                                     is.na(MCS_merged$ECQ23X00)] <- NA 

table(MCS_merged$Area_safety, useNA="always")

## ================== Child illness (Sweeps 2-6, ages 3, 5, 7, 11, 14 years) ==================
# Factor variable with 2 levels; originally coded "Yes" or "No"
# "Yes" will be recoded to 1; "No" to 0

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$BPCLSI00, useNA="always")
table(MCS_merged$CPCLSI00, useNA="always")
table(MCS_merged$DPCLSI00, useNA="always")
table(MCS_merged$EPCLSI00, useNA="always")
table(MCS_merged$FPCLSI00, useNA="always")

### (Sweep 2, age 3)
table(MCS_merged$BPCLSI00, useNA="always")

MCS_merged$BPCLSI00 <- recode(MCS_merged$BPCLSI00, "'Yes'=1; 'No'=0")

MCS_merged$BPCLSI00 <- as.numeric(as.character(MCS_merged$BPCLSI00))

table(MCS_merged$BPCLSI00, useNA="always")

### (Sweep 3, age 5)
table(MCS_merged$CPCLSI00, useNA="always")

MCS_merged$CPCLSI00 <- recode(MCS_merged$CPCLSI00, "'Yes'=1; 'No'=0")

MCS_merged$CPCLSI00 <- as.numeric(as.character(MCS_merged$CPCLSI00))

table(MCS_merged$CPCLSI00, useNA="always")

### (Sweep 4, age 7)
table(MCS_merged$DPCLSI00, useNA="always")

MCS_merged$DPCLSI00 <- recode(MCS_merged$DPCLSI00, "'Yes    '=1; 'No     '=0")

MCS_merged$DPCLSI00 <- as.numeric(as.character(MCS_merged$DPCLSI00))

table(MCS_merged$DPCLSI00, useNA="always")

### (Sweep 5, age 11)
table(MCS_merged$EPCLSI00, useNA="always")

MCS_merged$EPCLSI00 <- recode(MCS_merged$EPCLSI00, "\"Yes    \"=1; \"No     \"=0; \"Refused\"=NA; \"Don't know     \"=NA")

MCS_merged$EPCLSI00 <- as.numeric(as.character(MCS_merged$EPCLSI00))

table(MCS_merged$EPCLSI00, useNA="always")

### (Sweep 6, age 14)
table(MCS_merged$FPCLSI00, useNA="always")

MCS_merged$FPCLSI00 <- recode(MCS_merged$FPCLSI00, "'Yes    '=1; 'No     '=0")

MCS_merged$FPCLSI00 <- as.numeric(as.character(MCS_merged$FPCLSI00))

table(MCS_merged$FPCLSI00, useNA="always")

### SUM CHILD ILLNESS SCORES ACROSS SWEEPS 2,3,4,5,6

# If child had longstanding limiting illness for at least 1 sweep, code as 1 
MCS_merged$Child_illness <- 0

MCS_merged$Child_illness[MCS_merged$BPCLSI00==1 | MCS_merged$CPCLSI00==1 | 
                                     MCS_merged$DPCLSI00==1 | MCS_merged$EPCLSI00==1 |
                                     MCS_merged$FPCLSI00==1] <- 1

# If missing data for all sweeps 2-6, code to NA
MCS_merged$Child_illness[is.na(MCS_merged$BPCLSI00) & is.na(MCS_merged$CPCLSI00) & 
                                     is.na(MCS_merged$DPCLSI00) & is.na(MCS_merged$EPCLSI00) &
                                     is.na(MCS_merged$FPCLSI00)] <- NA 

table(MCS_merged$Child_illness, useNA="always")

## ================== Cognitive stimulation (Sweeps 2-5, ages 3, 5, 7, 11 years) ==================
# Factor variable with 2-7 levels; originally coded "Every day", "Several times a week", "Once or twice a week", "Once or twice a month", "Less often", "Not at all" etc.
# "Not at all" will be recoded to 1; "Every day", "Several times a week", "Once or twice a week", "Once or twice a month", and "Less often" will be recoded to 0
# "No" will be recoded to 1; "Yes" to 0
# "On special occasions" will be recoded to 1; "Once a month", "Once a fortnight", and "Once a week" to 0
# "Occasionally or less than once a week" will be recoded to 1; "1 - 2 days per week", "3 times a week", "4 times a week", "5 times a week", "6 times a week", and "7 times a week constantly" to 0

### (Sweep 2, age 3)

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$BPOFRE00, useNA="always") # how often do you read to the child
table(MCS_merged$BPREEL00, useNA="always") # anyone else read to the child
table(MCS_merged$BPREOF00, useNA="always") # how often does anyone else read to the child
table(MCS_merged$BPTOLI00, useNA="always") # anyone at home take child to the library
table(MCS_merged$BPSDPA00, useNA="always") # anyone at home help child to learn sport
table(MCS_merged$BPALPH00, useNA="always") # does anyone help child with alphabet/reading
table(MCS_merged$BPOFAB00, useNA="always") # how often help child learn alphabet
table(MCS_merged$BPNUMB00, useNA="always") # anyone at home teach child counting
table(MCS_merged$BPOFCO00, useNA="always") # how often at home try to teach child counting
table(MCS_merged$BPSONG00, useNA="always") # anyone at home try teach child songs
table(MCS_merged$BPOFSO00, useNA="always") # how often teach child songs/poems/rhymes
table(MCS_merged$BPDRAW00, useNA="always") # does child paint/draw at home
table(MCS_merged$BPPAMA00, useNA="always") # how often does child paint/draw at home
table(MCS_merged$BPEATW00, useNA="always") # child eaten with family past week 
table(MCS_merged$BPBIRT00, useNA="always") # something special for child's third birthday
table(MCS_merged$BPYOCH00, useNA="always") # been visited by friends with young children 

# Recode variables to binary
MCS_merged <- MCS_merged %>% 
  mutate_at(c("BPOFRE00","BPREOF00"), 
            function(x) recode(x, "'Every day'=0; 'Several times a week'=0; 'Once or twice a week'=0; 'Once or twice a month'=0; 'Less often'=0; 'Not at all'=1"))

MCS_merged <- MCS_merged %>% 
  mutate_at(c("BPREEL00","BPTOLI00","BPSDPA00","BPALPH00","BPNUMB00","BPSONG00","BPDRAW00","BPEATW00","BPYOCH00"), 
            function(x) recode(x, "'Yes'=0; 'No'=1"))

MCS_merged <- MCS_merged %>% 
  mutate_at(c("BPOFCO00","BPOFSO00","BPPAMA00"), 
            function(x) recode(x, "'Occasionally or less than once a week'=1; '1 - 2 days per week'=0; '3 times a week'=0; '4 times a week'=0; '5 times a week'=0; '6 times a week'=0; '7 times a week constantly'=0"))

MCS_merged$BPOFAB00 <- recode(MCS_merged$BPOFAB00, "'Occasionally or less than once a week'=1; '1-2 days per week'=0; '3 times a week'=0; '4 times a week'=0; '5 times a week'=0; '6 times a week'=0; '7 times a week/constantly'=0")

MCS_merged$BPBIRT00 <- recode(MCS_merged$BPBIRT00, "'No'=1; 'Yes'=0; 'Not had yet'=0")

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("BPOFRE00", "BPREEL00", "BPREOF00", "BPTOLI00", "BPSDPA00", "BPALPH00", "BPOFAB00", "BPNUMB00", "BPOFCO00", "BPSONG00", "BPOFSO00", "BPDRAW00", "BPPAMA00", "BPEATW00", "BPBIRT00", "BPYOCH00"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly 
table(MCS_merged$BPOFRE00, useNA="always") # how often do you read to the child
table(MCS_merged$BPREEL00, useNA="always") # anyone else read to the child
table(MCS_merged$BPREOF00, useNA="always") # how often does anyone else read to the child
table(MCS_merged$BPTOLI00, useNA="always") # anyone at home take child to the library
table(MCS_merged$BPSDPA00, useNA="always") # anyone at home help child to learn sport
table(MCS_merged$BPALPH00, useNA="always") # does anyone help child with alphabet/reading
table(MCS_merged$BPOFAB00, useNA="always") # how often help child learn alphabet
table(MCS_merged$BPNUMB00, useNA="always") # anyone at home teach child counting
table(MCS_merged$BPOFCO00, useNA="always") # how often at home try to teach child counting
table(MCS_merged$BPSONG00, useNA="always") # anyone at home try teach child songs
table(MCS_merged$BPOFSO00, useNA="always") # how often teach child songs/poems/rhymes
table(MCS_merged$BPDRAW00, useNA="always") # does child paint/draw at home
table(MCS_merged$BPPAMA00, useNA="always") # how often does child paint/draw at home
table(MCS_merged$BPEATW00, useNA="always") # child eaten with family past week 
table(MCS_merged$BPBIRT00, useNA="always") # something special for child's third birthday
table(MCS_merged$BPYOCH00, useNA="always") # been visited by friends with young children 

# Sum items together into Cog_MCS2
MCS_merged$Cog_MCS2 = NA

MCS_merged$Cog_MCS2 = apply(MCS_merged[,c("BPOFRE00", "BPREEL00", "BPREOF00", "BPTOLI00", "BPSDPA00", "BPALPH00", "BPOFAB00", "BPNUMB00", "BPOFCO00", "BPSONG00", "BPOFSO00", "BPDRAW00", "BPPAMA00", "BPEATW00", "BPBIRT00", "BPYOCH00")],1, mean, na.rm=T)*16 # multiply average of 16 items by 16

MCS_merged$Cog_MCS2[apply(apply(MCS_merged[,c("BPOFRE00", "BPREEL00", "BPREOF00", "BPTOLI00", "BPSDPA00", "BPALPH00", "BPOFAB00", "BPNUMB00", "BPOFCO00", "BPSONG00", "BPOFSO00", "BPDRAW00", "BPPAMA00", "BPEATW00", "BPBIRT00", "BPYOCH00")],2,is.na),1,sum) >8] = NA # make NA when missing more than 8 items

describe(MCS_merged$Cog_MCS2)
table(MCS_merged$Cog_MCS2, useNA="always")

# If scores >=8, code to 1 for poor cognitive stimulation, else code to 0
MCS_merged$Cog_MCS2 <- ifelse(MCS_merged$Cog_MCS2>=8, 1, 0)
table(MCS_merged$Cog_MCS2, useNA="always")

### (Sweep 3, age 5)

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$CPREOF00, useNA="always") # how often do you read to the child
table(MCS_merged$CPSITS00, useNA="always") # how often tells stories to child
table(MCS_merged$CPPLMU00, useNA="always") # how often does musical activities with child
table(MCS_merged$CPPAMA00, useNA="always") # how often does child paint/draw at home
table(MCS_merged$CPACTI00, useNA="always") # how often plays physically active games with child
table(MCS_merged$CPGAME00, useNA="always") # frequency plays indoor games with child
table(MCS_merged$CPWALK00, useNA="always") # frequency takes child to park/playground

# Recode variables to binary
MCS_merged <- MCS_merged %>% 
  mutate_at(c("CPREOF00","CPSITS00","CPPLMU00","CPPAMA00","CPACTI00","CPGAME00","CPWALK00"), 
            function(x) recode(x, "'Every day'=0; 'Several times a week'=0; 'Once or twice a week'=0; 'Once or twice a month'=0; 'Less often'=0; 'Not at all'=1"))

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("CPREOF00","CPSITS00","CPPLMU00","CPPAMA00","CPACTI00","CPGAME00","CPWALK00"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly 
table(MCS_merged$CPREOF00, useNA="always") # how often do you read to the child
table(MCS_merged$CPSITS00, useNA="always") # how often tells stories to child
table(MCS_merged$CPPLMU00, useNA="always") # how often does musical activities with child
table(MCS_merged$CPPAMA00, useNA="always") # how often does child paint/draw at home
table(MCS_merged$CPACTI00, useNA="always") # how often plays physically active games with child
table(MCS_merged$CPGAME00, useNA="always") # frequency plays indoor games with child
table(MCS_merged$CPWALK00, useNA="always") # frequency takes child to park/playground

# Sum items together into Cog_MCS3
MCS_merged$Cog_MCS3 = NA

MCS_merged$Cog_MCS3 = apply(MCS_merged[,c("CPREOF00","CPSITS00","CPPLMU00","CPPAMA00","CPACTI00","CPGAME00","CPWALK00")],1, mean, na.rm=T)*7 # multiply average of 7 items by 7

MCS_merged$Cog_MCS3[apply(apply(MCS_merged[,c("CPREOF00","CPSITS00","CPPLMU00","CPPAMA00","CPACTI00","CPGAME00","CPWALK00")],2,is.na),1,sum) >3.5] = NA # make NA when missing more than 3.5 items

describe(MCS_merged$Cog_MCS3)
table(MCS_merged$Cog_MCS3, useNA="always")

# If scores >=3.5, code to 1 for poor cognitive stimulation, else code to 0
MCS_merged$Cog_MCS3 <- ifelse(MCS_merged$Cog_MCS3>=3.5, 1, 0)
table(MCS_merged$Cog_MCS3, useNA="always")

### (Sweep 4, age 7)

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$DPREOF00, useNA="always") # how often do you read to the child
table(MCS_merged$DPSITS00, useNA="always") # how often tells stories to child
table(MCS_merged$DPPLMU00, useNA="always") # how often does musical activities with child
table(MCS_merged$DPPAMA00, useNA="always") # how often does child paint/draw at home
table(MCS_merged$DPACTI00, useNA="always") # how often plays physically active games with child
table(MCS_merged$DPGAME00, useNA="always") # frequency plays indoor games with child
table(MCS_merged$DPWALK00, useNA="always") # frequency takes child to park/playground

# Recode variables to binary
MCS_merged <- MCS_merged %>% 
  mutate_at(c("DPREOF00","DPSITS00","DPPLMU00","DPPAMA00","DPACTI00","DPGAME00","DPWALK00"), 
            function(x) recode(x, "'Every day or almost every day  '=0; 'Several times a week   '=0; 'Once or twice a week   '=0; 'Once or twice a month  '=0; 'Less often than once a month   '=0; 'Not at all     '=1"))

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("DPREOF00","DPSITS00","DPPLMU00","DPPAMA00","DPACTI00","DPGAME00","DPWALK00"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly
table(MCS_merged$DPREOF00, useNA="always") # how often do you read to the child
table(MCS_merged$DPSITS00, useNA="always") # how often tells stories to child
table(MCS_merged$DPPLMU00, useNA="always") # how often does musical activities with child
table(MCS_merged$DPPAMA00, useNA="always") # how often does child paint/draw at home
table(MCS_merged$DPACTI00, useNA="always") # how often plays physically active games with child
table(MCS_merged$DPGAME00, useNA="always") # frequency plays indoor games with child
table(MCS_merged$DPWALK00, useNA="always") # frequency takes child to park/playground

# Sum items together into Cog_MCS4
MCS_merged$Cog_MCS4 = NA

MCS_merged$Cog_MCS4 = apply(MCS_merged[,c("DPREOF00","DPSITS00","DPPLMU00","DPPAMA00","DPACTI00","DPGAME00","DPWALK00")],1, mean, na.rm=T)*7 # multiply average of 7 items by 7

MCS_merged$Cog_MCS4[apply(apply(MCS_merged[,c("DPREOF00","DPSITS00","DPPLMU00","DPPAMA00","DPACTI00","DPGAME00","DPWALK00")],2,is.na),1,sum) >3.5] = NA # make NA when missing more than 3.5 items

describe(MCS_merged$Cog_MCS4)
table(MCS_merged$Cog_MCS4, useNA="always")

# If scores >=3.5, code to 1 for poor cognitive stimulation, else code to 0
MCS_merged$Cog_MCS4 <- ifelse(MCS_merged$Cog_MCS4>=3.5, 1, 0)
table(MCS_merged$Cog_MCS4, useNA="always")

### (Sweep 5, age 11)

# Before recoding, run tables to see the distribution of the variable as a factor 
table(MCS_merged$EPACTI00, useNA="always") # how often do you play physically active games with CM?  
table(MCS_merged$EPGAME00, useNA="always") # frequency play INDOOR games with child  
table(MCS_merged$EPTAIM00, useNA="always") # frequency talks to CM about things important to them

# Recode variables to binary
MCS_merged <- MCS_merged %>% 
  mutate_at(c("EPACTI00","EPGAME00","EPTAIM00"), 
            function(x) recode(x, "\"Every day or almost every day  \"=0; \"Several times a week   \"=0; \"Once or twice a week   \"=0; \"Once or twice a month  \"=0; \"Less often than once a month   \"=0; \"Not at all     \"=1; \"Refused\"=NA; \"Don't know     \"=NA"))

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("EPACTI00","EPGAME00","EPTAIM00"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly
table(MCS_merged$EPACTI00, useNA="always") # how often do you play physically active games with CM?  
table(MCS_merged$EPGAME00, useNA="always") # frequency play INDOOR games with child  
table(MCS_merged$EPTAIM00, useNA="always") # frequency talks to CM about things important to them

# Sum items together into Cog_MCS5
MCS_merged$Cog_MCS5 = NA

MCS_merged$Cog_MCS5 = apply(MCS_merged[,c("EPACTI00","EPGAME00","EPTAIM00")],1, mean, na.rm=T)*3 # multiply average of 3 items by 3

MCS_merged$Cog_MCS5[apply(apply(MCS_merged[,c("EPACTI00","EPGAME00","EPTAIM00")],2,is.na),1,sum) >1.5] = NA # make NA when missing more than 1.5 items

describe(MCS_merged$Cog_MCS5)
table(MCS_merged$Cog_MCS5, useNA="always")

# If scores >=1.5, code to 1 for poor cognitive stimulation, else code to 0
MCS_merged$Cog_MCS5 <- ifelse(MCS_merged$Cog_MCS5>=1.5, 1, 0)
table(MCS_merged$Cog_MCS5, useNA="always")

### SUM COGNITIVE STIMULATION SCORES ACROSS SWEEPS 2,3,4,5

# If child had poor cognitive stimulation for at least 1 sweep, code as 1 
MCS_merged$Cognitive_stimulation <- 0

MCS_merged$Cognitive_stimulation[MCS_merged$Cog_MCS2==1 | MCS_merged$Cog_MCS3==1 | 
                                     MCS_merged$Cog_MCS4==1 | MCS_merged$Cog_MCS5==1] <- 1

# If missing data for all sweeps 2,3,4,5, code to NA
MCS_merged$Cognitive_stimulation[is.na(MCS_merged$Cog_MCS2) & is.na(MCS_merged$Cog_MCS3) & 
                                     is.na(MCS_merged$Cog_MCS4) & is.na(MCS_merged$Cog_MCS5)] <- NA 

table(MCS_merged$Cognitive_stimulation, useNA="always")

## ================== Internalising and Externalising Symptoms (Sweep 7, age 17) ==================
# Factor variable with 3 levels; originally coded "Certainly true", "Not true", "Somewhat true"
# "Certainly true" will be recoded to 2; "Somewhat true" to 1; "Not true" to 0; "Can't say" to NA
# Following guidance from: https://terapia.co.uk/wp-content/uploads/2020/05/SDQ-scoring_Instructions_4-18-years.pdf
# We will recode levels to be numeric, so they can be summed together into sum scores where higher score = worse internalising/externalising symptoms
# These internalising and externalising variables will be used as the outcome variables in the regression

### SDQ Internalising ###
## Emotional problems
# GCSDQC00 I get a lot of headaches, stomachaches or sickness
# GCSDQH00 I worry a lot
# GCSDQM00 I am often unhappy, down-hearted or tearful
# GCSDQP00 I am nervous in new situations. I easily lose confidence
# GCSDQX00 I have many fears, I am easily scared

## Peer problems
# GCSDQF00 I am usually on my own. I generally play alone or keep to myself
# GCSDQK00 I have one good friend or more
# GCSDQN00 Other people my age generally like me
# GCSDQS00 Other children or young people pick on me or bully me
# GCSDQW00 I get on better with adults than with people my own age

### SDQ Externalising ###

## Conduct problems
# GCSDQE00 I get very angry and often lose my temper
# GCSDQG00 I usually do as I am told
# GCSDQL00 I fight a lot. I can make other people do what I want
# GCSDQR00 I am often accused of lying or cheating
# GCSDQV00 I take things that are not mine from home, school or elsewhere

## Hyperactivity/inattention
# GCSDQB00 I am restless, I cannot stay still for long
# GCSDQJ00 I am constantly fidgeting or squirming
# GCSDQO00 I am easily distracted, I find it difficult to concentrate
# GCSDQU00 I think before I do things
# GCSDQY00 I finish the work I'm doing. My attention is good

# Look at prevalence of internalising symptoms
table(MCS_merged$GCSDQC00, useNA="always") # SDQ: headaches, stomachaches
table(MCS_merged$GCSDQH00, useNA="always") # SDQ: worry a lot
table(MCS_merged$GCSDQM00, useNA="always") # SDQ: often unhappy 
table(MCS_merged$GCSDQP00, useNA="always") # SDQ: nervous in new situations
table(MCS_merged$GCSDQX00, useNA="always") # SDQ: many fears, easily scared

table(MCS_merged$GCSDQF00, useNA="always") # SDQ: usually on my own
table(MCS_merged$GCSDQK00, useNA="always") # SDQ: one good friend or more
table(MCS_merged$GCSDQN00, useNA="always") # SDQ: other people my age like me
table(MCS_merged$GCSDQS00, useNA="always") # SDQ: others pick on me or bully me
table(MCS_merged$GCSDQW00, useNA="always") # SDQ: get on better with adults

# Look at prevalence of externalising symptoms
table(MCS_merged$GCSDQE00, useNA="always") # SDQ: often lose my temper
table(MCS_merged$GCSDQG00, useNA="always") # SDQ: usually do as told
table(MCS_merged$GCSDQL00, useNA="always") # SDQ: fight a lot
table(MCS_merged$GCSDQR00, useNA="always") # SDQ: often accused of lying or cheating
table(MCS_merged$GCSDQV00, useNA="always") # SDQ: take things that are not mine

table(MCS_merged$GCSDQB00, useNA="always") # SDQ: restless, cannot stay still 
table(MCS_merged$GCSDQJ00, useNA="always") # SDQ: constantly fidgeting 
table(MCS_merged$GCSDQO00, useNA="always") # SDQ: easily distracted
table(MCS_merged$GCSDQU00, useNA="always") # SDQ: think before do things
table(MCS_merged$GCSDQY00, useNA="always") # SDQ: finish the work, attention is good

# Recode variables to 3-point scale
# Emotional problems
MCS_merged <- MCS_merged %>% 
  mutate_at(c("GCSDQC00","GCSDQH00","GCSDQM00","GCSDQP00","GCSDQX00"), 
            function(x) recode(x, "\"Certainly true\"=2; \"Somewhat true\"=1; \"Not true\"=0; \"Can't say\"=NA"))

# Peer problems
MCS_merged <- MCS_merged %>% 
  mutate_at(c("GCSDQF00","GCSDQS00","GCSDQW00"), 
            function(x) recode(x, "\"Certainly true\"=2; \"Somewhat true\"=1; \"Not true\"=0; \"Can't say\"=NA"))
# Peer problems (reverse coded because they were phrased positively)
MCS_merged <- MCS_merged %>% 
  mutate_at(c("GCSDQK00","GCSDQN00"), 
            function(x) recode(x, "\"Certainly true\"=0; \"Somewhat true\"=1; \"Not true\"=2; \"Can't say\"=NA"))

# Conduct problems
MCS_merged <- MCS_merged %>% 
  mutate_at(c("GCSDQE00","GCSDQL00","GCSDQR00","GCSDQV00"), 
            function(x) recode(x, "\"Certainly true\"=2; \"Somewhat true\"=1; \"Not true\"=0; \"Can't say\"=NA"))
# Conduct problems (reverse coded because it was phrased positively)
MCS_merged <- MCS_merged %>% 
  mutate_at(c("GCSDQG00"), 
            function(x) recode(x, "\"Certainly true\"=0; \"Somewhat true\"=1; \"Not true\"=2; \"Can't say\"=NA"))

# Hyperactivity/inattention
MCS_merged <- MCS_merged %>% 
  mutate_at(c("GCSDQB00","GCSDQJ00","GCSDQO00"), 
            function(x) recode(x, "\"Certainly true\"=2; \"Somewhat true\"=1; \"Not true\"=0; \"Can't say\"=NA"))
# Hyperactivity/inattention (reverse coded because they were phrased positively)
MCS_merged <- MCS_merged %>% 
  mutate_at(c("GCSDQU00","GCSDQY00"), 
            function(x) recode(x, "\"Certainly true\"=0; \"Somewhat true\"=1; \"Not true\"=2; \"Can't say\"=NA"))

# Convert variables from factor to numeric
MCS_merged <- MCS_merged %>% 
  mutate_at(c("GCSDQC00","GCSDQH00","GCSDQM00","GCSDQP00","GCSDQX00",
              "GCSDQF00","GCSDQK00","GCSDQN00","GCSDQS00","GCSDQW00",
              "GCSDQE00","GCSDQG00","GCSDQL00","GCSDQR00","GCSDQV00",
              "GCSDQB00","GCSDQJ00","GCSDQO00","GCSDQU00","GCSDQY00"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly 
table(MCS_merged$GCSDQC00, useNA="always") # SDQ: headaches, stomachaches
table(MCS_merged$GCSDQH00, useNA="always") # SDQ: worry a lot
table(MCS_merged$GCSDQM00, useNA="always") # SDQ: often unhappy 
table(MCS_merged$GCSDQP00, useNA="always") # SDQ: nervous in new situations
table(MCS_merged$GCSDQX00, useNA="always") # SDQ: many fears, easily scared

table(MCS_merged$GCSDQF00, useNA="always") # SDQ: usually on my own
table(MCS_merged$GCSDQK00, useNA="always") # SDQ: one good friend or more
table(MCS_merged$GCSDQN00, useNA="always") # SDQ: other people my age like me
table(MCS_merged$GCSDQS00, useNA="always") # SDQ: others pick on me or bully me
table(MCS_merged$GCSDQW00, useNA="always") # SDQ: get on better with adults

table(MCS_merged$GCSDQE00, useNA="always") # SDQ: often lose my temper
table(MCS_merged$GCSDQG00, useNA="always") # SDQ: usually do as told
table(MCS_merged$GCSDQL00, useNA="always") # SDQ: fight a lot
table(MCS_merged$GCSDQR00, useNA="always") # SDQ: often accused of lying or cheating
table(MCS_merged$GCSDQV00, useNA="always") # SDQ: take things that are not mine

table(MCS_merged$GCSDQB00, useNA="always") # SDQ: restless, cannot stay still 
table(MCS_merged$GCSDQJ00, useNA="always") # SDQ: constantly fidgeting 
table(MCS_merged$GCSDQO00, useNA="always") # SDQ: easily distracted
table(MCS_merged$GCSDQU00, useNA="always") # SDQ: think before do things
table(MCS_merged$GCSDQY00, useNA="always") # SDQ: finish the work, attention is good

# Sum items together into Internalising_symptoms
MCS_merged$Internalising_symptoms = NA

MCS_merged$Internalising_symptoms <- apply(MCS_merged[,c("GCSDQC00","GCSDQH00","GCSDQM00","GCSDQP00","GCSDQX00","GCSDQF00","GCSDQK00","GCSDQN00","GCSDQS00","GCSDQW00")], 1, sum, na.rm=T) # sum items

MCS_merged$Internalising_symptoms[apply(apply(MCS_merged[,c("GCSDQC00","GCSDQH00","GCSDQM00","GCSDQP00","GCSDQX00","GCSDQF00","GCSDQK00","GCSDQN00","GCSDQS00","GCSDQW00")],2,is.na),1,sum) >5] = NA # make NA when missing more than 5 items

describe(MCS_merged$Internalising_symptoms)
table(MCS_merged$Internalising_symptoms, useNA="always")

# Sum items together into Externalising_symptoms
MCS_merged$Externalising_symptoms = NA

MCS_merged$Externalising_symptoms <- apply(MCS_merged[,c("GCSDQE00","GCSDQG00","GCSDQL00","GCSDQR00","GCSDQV00","GCSDQB00","GCSDQJ00","GCSDQO00","GCSDQU00","GCSDQY00")], 1, sum, na.rm=T) # sum items

MCS_merged$Externalising_symptoms[apply(apply(MCS_merged[,c("GCSDQE00","GCSDQG00","GCSDQL00","GCSDQR00","GCSDQV00","GCSDQB00","GCSDQJ00","GCSDQO00","GCSDQU00","GCSDQY00")],2,is.na),1,sum) >5] = NA # make NA when missing more than 5 items

describe(MCS_merged$Externalising_symptoms)
table(MCS_merged$Externalising_symptoms, useNA="always")

## ================== DESCRIPTIVE STATISTICS OF DERIVED VARIABLES ==================
# Derived variables were all coded to binary such that 1 = risk; 0 = no risk
# Check n and percentages for each ACE

# Poor parental mental health (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years)
table(MCS_merged$Parental_mental_health, useNA="always")
prop.table(table(MCS_merged$Parental_mental_health, useNA="always"))*100

# Frequent parental alcohol use (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years)
table(MCS_merged$Parental_alcohol, useNA="always") 
prop.table(table(MCS_merged$Parental_alcohol, useNA="always"))*100

# Parental drug use (Sweeps 2,3,6; ages 3, 5, 14)
table(MCS_merged$Parental_drug, useNA="always") 
prop.table(table(MCS_merged$Parental_drug, useNA="always"))*100

# Single parent (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years)
table(MCS_merged$Single_parent, useNA="always") 
prop.table(table(MCS_merged$Single_parent, useNA="always"))*100

# Unhappy parental relationship (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years)
table(MCS_merged$Parental_relationship, useNA="always")
prop.table(table(MCS_merged$Parental_relationship, useNA="always"))*100

# Domestic violence (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years)
table(MCS_merged$Domestic_violence, useNA="always")
prop.table(table(MCS_merged$Domestic_violence, useNA="always"))*100

# Harsh parental discipline (Sweeps 2-4, ages 3, 5, 7 years)
table(MCS_merged$Parental_discipline, useNA="always") 
prop.table(table(MCS_merged$Parental_discipline, useNA="always"))*100

# Parental smacking (Sweeps 2-4, ages 3, 5, 7 years)
table(MCS_merged$Parental_smacking, useNA="always") 
prop.table(table(MCS_merged$Parental_smacking, useNA="always"))*100

# Negative home environment (Sweep 2, age 3)
table(MCS_merged$Home_environment, useNA="always")
prop.table(table(MCS_merged$Home_environment, useNA="always"))*100

# Peer victimisation (Sweeps 2-6, ages 3, 5, 7, 11, 14 years)
table(MCS_merged$Peer_victimisation, useNA="always")
prop.table(table(MCS_merged$Peer_victimisation, useNA="always"))*100

# Sibling victimisation (Sweeps 5,6, ages 11, 14)
table(MCS_merged$Sibling_victimisation, useNA="always")
prop.table(table(MCS_merged$Sibling_victimisation, useNA="always"))*100

# Verbal victimisation (Sweep 6, age 14)
table(MCS_merged$Verbal_victimisation, useNA="always")
prop.table(table(MCS_merged$Verbal_victimisation, useNA="always"))*100

# Physical victimisation (Sweep 6, age 14)
table(MCS_merged$Physical_victimisation, useNA="always") 
prop.table(table(MCS_merged$Physical_victimisation, useNA="always"))*100

# Theft victimisation (Sweep 6, age 14)
table(MCS_merged$Theft_victimisation, useNA="always") 
prop.table(table(MCS_merged$Theft_victimisation, useNA="always"))*100

# Sexual victimisation (Sweep 6, age 14)
table(MCS_merged$Sexual_victimisation, useNA="always") 
prop.table(table(MCS_merged$Sexual_victimisation, useNA="always"))*100

# Low household income (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years)
table(MCS_merged$Household_income, useNA="always")
prop.table(table(MCS_merged$Household_income, useNA="always"))*100

# Neighbourhood deprivation (Sweep 1, 9 months)
table(MCS_merged$Neighbourhood_deprivation, useNA="always")
prop.table(table(MCS_merged$Neighbourhood_deprivation, useNA="always"))*100

# Unsafe home area (Sweeps 2,3,5, ages 3,5,11)
table(MCS_merged$Area_safety, useNA="always")
prop.table(table(MCS_merged$Area_safety, useNA="always"))*100

# Child illness (Sweeps 2-6, ages 3, 5, 7, 11, 14 years)
table(MCS_merged$Child_illness, useNA="always")
prop.table(table(MCS_merged$Child_illness, useNA="always"))*100

# Low cognitive stimulation (Sweeps 2-4, ages 3, 5, 7 years)
table(MCS_merged$Cognitive_stimulation, useNA="always")
prop.table(table(MCS_merged$Cognitive_stimulation, useNA="always"))*100

# Adolescent mental health (Sweep 7, age 14 years)
describe(MCS_merged$Adolescent_mental_health)
table(MCS_merged$Adolescent_mental_health, useNA="always")

# Internalising symptoms (Sweep 7, age 14 years)
describe(MCS_merged$Internalising_symptoms)
table(MCS_merged$Internalising_symptoms, useNA="always")

# Externalising symptoms (Sweep 7, age 14 years)
describe(MCS_merged$Externalising_symptoms)
table(MCS_merged$Externalising_symptoms, useNA="always")

## Save dataset
setwd("/Users/athenachowruwern/Desktop")
save(MCS_merged, file = "MCSpreImputation.RData")
```

# 2. Exploratory factor analysis

We will visualise the correlation matrix, conduct parallel analysis, and
examine fit coefficients to inform the number of factors to retain in
the exploratory factor analysis.

``` r
# Load MCS pre-imputation dataset
setwd("/Users/athenachowruwern/Desktop")
load("MCSpreImputation.RData")

# Create new dataframe with all derived binary ACE variables
MCS_ACEs <- select(MCS_merged,
                   Parental_mental_health,
                   Parental_alcohol,
                   Parental_drug,
                   Single_parent,
                   Parental_relationship,
                   Domestic_violence,
                   Parental_discipline,
                   Parental_smacking,
                   Home_environment,
                   Peer_victimisation,
                   #Sibling_victimisation, commented out due to low loading < 0.3 
                   Verbal_victimisation,
                   Physical_victimisation,
                   Theft_victimisation,
                   Sexual_victimisation,
                   Household_income,
                   Neighbourhood_deprivation,
                   Area_safety,
                   #Child_illness, commented out due to low loading < 0.3 
                   Cognitive_stimulation)
dim(MCS_ACEs)
str(MCS_ACEs)
colnames(MCS_ACEs)

# Check whether IDs are duplicated (e.g. multiple children per family) - no
n_occur <- data.frame(table(MCS_ACEs$MCSID))
n_occur[n_occur$Freq > 1,]

#------------------------------------ Exploratory Factor Analysis (EFA) ------------------------------------#
library(corrplot)
library(EFA.dimensions)

# Compute tetrachoric correlations
tet_corr <- tetrachoric(MCS_ACEs)
tet_corr

# Tetrachoric correlation matrix
tet_corr_matrix<- tet_corr$rho
tet_corr_matrix

# Visualise tetrachoric correlation matrix
corrplot(tet_corr_matrix, order="original", method="circle")

# Make correlations larger/bolder
tet_corr_matrix=tanh(5*atanh(tet_corr_matrix))
corrplot(tet_corr_matrix, order="original", method="circle")

# Cluster analysis technique to put items which are more similar closer to one another
corrplot(tet_corr_matrix, order="hclust", method="circle")

# Show lower diagonal only and change font colour to black
corrplot(tet_corr_matrix, type="lower", order="hclust", method="circle", tl.col="black")

# Check factorability of correlation matrix (Bartlett's test for sphericity, Kaiser-Meyer-Olkin test for sampling adequacy etc.)
factorability <- FACTORABILITY(MCS_ACEs, corkind="polychoric", Ncases=nrow(MCS_ACEs))
factorability

###### Parallel analysis, 1000 Monte-Carlo simulations, 99% confidence intervals

# Weighted least squares parallel analysis
parallel_analysis_wls <- fa.parallel(MCS_ACEs, fm="wls", fa="both", n.iter=1000, cor="tet", error.bars=TRUE, sim=FALSE, quant=.99, main="Parallel Analysis Scree Plot" )

parallel_analysis_wls$fa.values # eigenvalues
parallel_analysis_wls
# WLS parallel analysis recommends that the number of factors = 5 and the number of components = 5

###### Exploratory factor analysis (using item response theory (IRT) code)

## IRT one-factor model
irt_1factor <- irt.fa(tet_corr, nfactors=1, plot=FALSE, n.obs=nrow(MCS_ACEs), rotate="promax", fm="wls")
irt_1factor$fa # loadings
fa.diagram(irt_1factor, cut=0.2, rsize=.30) # factor diagram

## IRT two-factor model
irt_2factor <- irt.fa(tet_corr, nfactors=2, plot=FALSE, n.obs=nrow(MCS_ACEs), rotate="promax", fm="wls")
irt_2factor$fa # loadings
fa.diagram(irt_2factor, cut=0.2, rsize=.30) # factor diagram

## IRT three-factor model
irt_3factor <- irt.fa(tet_corr, nfactors=3, plot=FALSE, n.obs=nrow(MCS_ACEs), rotate="promax", fm="wls")
irt_3factor$fa # loadings
fa.diagram(irt_3factor, cut=0.2, rsize=.30) # factor diagram

# IRT four-factor model
irt_4factor <- irt.fa(tet_corr, nfactors=4, plot=FALSE, n.obs=nrow(MCS_ACEs), rotate="oblimin", fm="wls") # promax doesn't work
irt_4factor$fa # loadings
colnames(irt_4factor$fa$loadings) <- c("Deprivation", "Victimisation", "Parental Threat", "Parental Discipline")
fa.diagram(irt_4factor, cut=0.2, rsize=.30) # factor diagram

# IRT five-factor model
irt_5factor <- irt.fa(tet_corr, nfactors=5, plot=FALSE, n.obs=nrow(MCS_ACEs), rotate="oblimin", fm="wls") # promax doesn't work
irt_5factor$fa # loadings
fa.diagram(irt_5factor, cut=0.2, rsize=.30) # factor diagram

#------------------------------------ Plot factor analysis results ------------------------------------#
# Following guidance from Dr Dan Mirman: https://rpubs.com/danmirman/plotting_factor_analysis

# Extract loadings from 4-factor model
class(irt_4factor$fa$loadings) # psych loadings class 
factor_loadings <- unclass(irt_4factor$fa$loadings) # convert to matrix
factor_loadings <- as.data.frame(factor_loadings) # convert to dataframe
factor_loadings <- rownames_to_column(factor_loadings, "ACEs") # name first column

# Rename ACEs
factor_loadings$ACEs <- recode(factor_loadings$ACEs, "'Household_income'='Low household income'; 'Home_environment'='Negative home environment'; 'Area_safety'='Unsafe home area'; 'Neighbourhood_deprivation'='Neighbourhood deprivation'; 'Single_parent'='Single parent'; 'Parental_mental_health'='Poor parental mental health'; 'Cognitive_stimulation'='Low cognitive stimulation'; 'Physical_victimisation'='Physical victimisation'; 'Verbal_victimisation'='Verbal victimisation'; 'Theft_victimisation'='Theft victimisation'; 'Sexual_victimisation'='Sexual victimisation'; 'Peer_victimisation'='Peer victimisation'; 'Parental_drug'='Parental drug use'; 'Domestic_violence'='Domestic violence'; 'Parental_alcohol'='Frequent parental alcohol use'; 'Parental_relationship'='Unhappy parental relationship'; 'Parental_smacking'='Parental smacking'; 'Parental_discipline'='Harsh parental discipline'")

# Manually order ACEs by the 4 factor groups
factor_loadings$ACEs <- factor(factor_loadings$ACEs, levels = c("Harsh parental discipline", "Parental smacking", "Peer victimisation", "Sexual victimisation", "Theft victimisation", "Verbal victimisation", "Physical victimisation", "Low cognitive stimulation", "Poor parental mental health", "Single parent", "Neighbourhood deprivation", "Unsafe home area", "Negative home environment", "Low household income", "Unhappy parental relationship", "Frequent parental alcohol use", "Domestic violence", "Parental drug use"))
factor_loadings <- factor_loadings[order(factor_loadings$ACEs), ]
factor_loadings

# Melt data into long form for plotting
library(reshape2)
loadings.m <- melt(factor_loadings, id="ACEs", 
                   measure=c("Parental Threat", "Deprivation", "Victimisation", "Parental Discipline"), 
                   variable.name="Factor", value.name="Loading")
loadings.m

# For each ACE, plot the loading as length and fill color of a bar
# note that the length will be the absolute value of the loading but the 
# fill color will be the signed value, more on this below
loadings_plot <- ggplot(loadings.m, aes(ACEs, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Factor, nrow=1) + # place the factors in separate facets
  geom_bar(stat="identity") + # make the bars
  coord_flip() + # flip the axes so the test names can be horizontal  
  # define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "skyblue", mid = "white", low = "red", 
                       midpoint=0, guide="none") + 
  ylab("Loading Strength") + # improve y-axis label
  theme_bw(base_size=10) # use a black and white theme with set font size

loadings_plot

# Extract correlations from correlation matrix
class(tet_corr_matrix) # matrix array class
tet_corr_df <- as.data.frame(tet_corr_matrix) # convert to dataframe
tet_corr_df <- rownames_to_column(tet_corr_df, "ACEs") # name first column
head(tet_corr_df)

# Melt data into long form for plotting
corrs.m <- melt(tet_corr_df, id="ACEs", variable.name="ACEs2", value.name="Correlation")
head(corrs.m)

# Rename ACEs
corrs.m$ACEs <- recode(corrs.m$ACEs, "'Household_income'='Low household income'; 'Home_environment'='Negative home environment'; 'Area_safety'='Unsafe home area'; 'Neighbourhood_deprivation'='Neighbourhood deprivation'; 'Single_parent'='Single parent'; 'Parental_mental_health'='Poor parental mental health'; 'Cognitive_stimulation'='Low cognitive stimulation'; 'Physical_victimisation'='Physical victimisation'; 'Verbal_victimisation'='Verbal victimisation'; 'Theft_victimisation'='Theft victimisation'; 'Sexual_victimisation'='Sexual victimisation'; 'Peer_victimisation'='Peer victimisation'; 'Parental_drug'='Parental drug use'; 'Domestic_violence'='Domestic violence'; 'Parental_alcohol'='Frequent parental alcohol use'; 'Parental_relationship'='Unhappy parental relationship'; 'Parental_smacking'='Parental smacking'; 'Parental_discipline'='Harsh parental discipline'")

corrs.m$ACEs2 <- recode(corrs.m$ACEs2, "'Household_income'='Low household income'; 'Home_environment'='Negative home environment'; 'Area_safety'='Unsafe home area'; 'Neighbourhood_deprivation'='Neighbourhood deprivation'; 'Single_parent'='Single parent'; 'Parental_mental_health'='Poor parental mental health'; 'Cognitive_stimulation'='Low cognitive stimulation'; 'Physical_victimisation'='Physical victimisation'; 'Verbal_victimisation'='Verbal victimisation'; 'Theft_victimisation'='Theft victimisation'; 'Sexual_victimisation'='Sexual victimisation'; 'Peer_victimisation'='Peer victimisation'; 'Parental_drug'='Parental drug use'; 'Domestic_violence'='Domestic violence'; 'Parental_alcohol'='Frequent parental alcohol use'; 'Parental_relationship'='Unhappy parental relationship'; 'Parental_smacking'='Parental smacking'; 'Parental_discipline'='Harsh parental discipline'")

# Reorder ACEs in correlation matrix
corrs.m$ACEs <- factor(corrs.m$ACEs, levels = c("Harsh parental discipline", "Parental smacking", "Peer victimisation", "Sexual victimisation", "Theft victimisation", "Verbal victimisation", "Physical victimisation", "Low cognitive stimulation", "Poor parental mental health", "Single parent", "Neighbourhood deprivation", "Unsafe home area", "Negative home environment", "Low household income", "Unhappy parental relationship", "Frequent parental alcohol use", "Domestic violence", "Parental drug use"))
corrs.m <- corrs.m[order(corrs.m$ACEs), ]

corrs.m$ACEs2 <- factor(corrs.m$ACEs2, levels = c("Parental drug use", "Domestic violence", "Frequent parental alcohol use", "Unhappy parental relationship", "Low household income", "Negative home environment", "Unsafe home area", "Neighbourhood deprivation", "Single parent", "Poor parental mental health", "Low cognitive stimulation", "Physical victimisation", "Verbal victimisation", "Theft victimisation", "Sexual victimisation", "Peer victimisation", "Parental smacking", "Harsh parental discipline"))
corrs.m <- corrs.m[order(corrs.m$ACEs2), ]

corrs.m

# Plot correlation matrix
library(grid) 
# for adjusting plot margins
# place the tests on the x- and y-axes, 
# fill the elements with the strength of the correlation
corr_matrix <- ggplot(corrs.m, aes(ACEs2, ACEs, fill=abs(Correlation))) + 
  geom_tile() + # rectangles for each correlation
  # add actual correlation value in the rectangle
  geom_text(aes(label = round(Correlation, 2)), size=2.5) + 
  theme_bw(base_size=10) + # black and white theme with set font size
  # rotate x-axis labels so they don't overlap, 
  # get rid of unnecessary axis titles
  # adjust plot margins
  theme(axis.text.x = element_text(angle = 90), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        plot.margin = unit(c(3, 1, 0, 0), "mm")) +
  # set correlation fill gradient
  scale_fill_gradient(low="white", high="skyblue") + 
  guides(fill=F) # omit unnecessary gradient legend

corr_matrix

# Store the correlation matrix plot object for later
p1 <- last_plot() 

# Load Wes Anderson color palette
library(wesanderson)

# Load Harry Potter colour palette
library(harrypotter)

# Plot stacked bar graph
p2 <- ggplot(loadings.m, aes(ACEs, abs(Loading), fill=Factor)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ylab("Loading Strength") + theme_bw(base_size=10) + 
  # remove labels and tweak margins for combining with the correlation matrix plot
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = unit(c(3,1,30.75,-3), "mm")) +
  scale_fill_manual(values = wes_palette("Moonrise3", n = 4))
p2

#---------------- This is the same code but different colour themes ----------------#
p2 <- ggplot(loadings.m, aes(ACEs, abs(Loading), fill=Factor)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ylab("Loading Strength") + theme_bw(base_size=10) + 
  # remove labels and tweak margins for combining with the correlation matrix plot
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = unit(c(3,1,30.75,-3), "mm")) +
  scale_fill_manual(values = wes_palette("GrandBudapest2", n = 4))

p2 <- ggplot(loadings.m, aes(ACEs, abs(Loading), fill=Factor)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ylab("Loading Strength") + theme_bw(base_size=10) + 
  # remove labels and tweak margins for combining with the correlation matrix plot
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = unit(c(3,1,30.75,-3), "mm")) +
  scale_fill_hp(discrete=TRUE, option = "Ravenclaw")

p2 <- ggplot(loadings.m, aes(ACEs, abs(Loading), fill=Factor)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ylab("Loading Strength") + theme_bw(base_size=10) + 
  # remove labels and tweak margins for combining with the correlation matrix plot
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = unit(c(3,1,30.75,-3), "mm")) +
  scale_fill_hp(discrete=TRUE, option = "Always")
#-----------------------------------------------------------------------------------#

# Add stacked bar graph of factor loadings to correlation matrix
# so that we can see how the factor analysis transformed these pairwise correlations into factors
library(gridExtra)
grid.arrange(p1, p2, ncol=2, widths=c(2, 1))
```

# 3. Regression analysis

We will extract the factor scores as predictors for the regression
analysis, with internalising and externalising symptoms as the outcome
variables, as well as gender and ethnicity as covariates.

``` r
#------------------------- Extract factor scores ------------------------------------#
# Get factor scores 
factor_scores <- factor.scores(MCS_ACEs, irt_4factor$fa)
factor_scores

# Get columns of factor scores 
fs_cols <- factor_scores$scores
fs_cols <- as.data.frame(fs_cols)

# Add factor score columns to MCS_merged dataset
MCS_merged <- MCS_merged %>% mutate(
  "Deprivation" = fs_cols$Deprivation,
  "Victimisation" = fs_cols$Victimisation,
  "Parental_Threat" = fs_cols$`Parental Threat`,
  "Parental_Discipline" = fs_cols$`Parental Discipline`,
)

head(MCS_merged)

# Rename covariates
MCS_merged$Sex <- as.factor(MCS_merged$AHCSEX00)
MCS_merged$Ethnicity <- as.factor(MCS_merged$ADC06E00)

# Check prevalence of sex and ethnicity variables
table(MCS_merged$Sex, useNA="always")
table(MCS_merged$Ethnicity, useNA="always")

# Recode sex reference level to male
MCS_merged$Sex <- relevel(MCS_merged$Sex, ref = "Male")

# Recode race reference level to White
MCS_merged$Ethnicity <- relevel(MCS_merged$Ethnicity, ref = "White  ")

## Check prevalence of missing data

# ACE dimensions
summary(MCS_merged$Deprivation, useNA="always") 
summary(MCS_merged$Victimisation, useNA="always") 
summary(MCS_merged$Parental_Threat, useNA="always") 
summary(MCS_merged$Parental_Discipline, useNA="always") 

# Adolescent mental health outcomes
summary(MCS_merged$Internalising_symptoms, useNA="always")
summary(MCS_merged$Externalising_symptoms, useNA="always")

#------------------------- Check linear regression assumptions ------------------------------------# 

# Check correlations between factor scores to ensure no collinearity
cor(MCS_merged[, c("Deprivation", "Victimisation", "Parental_Threat", "Parental_Discipline")], use = "complete.obs")

# Check whether the outcome variables follow a normal distribution
hist(MCS_merged$Internalising_symptoms)
hist(MCS_merged$Externalising_symptoms)
# Distribution is skewed to the left, but this makes sense because a higher score represents worse mental health

# Standardise predictor and outcome variables
MCS_merged$Deprivation <- scale(MCS_merged$Deprivation)
MCS_merged$Victimisation <- scale(MCS_merged$Victimisation)
MCS_merged$Parental_Threat <- scale(MCS_merged$Parental_Threat)
MCS_merged$Parental_Discipline <- scale(MCS_merged$Parental_Discipline)

MCS_merged$Internalising_symptoms <- scale(MCS_merged$Internalising_symptoms)
MCS_merged$Externalising_symptoms <- scale(MCS_merged$Externalising_symptoms)

# Now the mean is 0 and standard deviation is 1
describe(MCS_merged$Deprivation) 
describe(MCS_merged$Victimisation) 
describe(MCS_merged$Parental_Threat) 
describe(MCS_merged$Parental_Discipline)

describe(MCS_merged$Internalising_symptoms) 
describe(MCS_merged$Externalising_symptoms) 

#------------------------- Unadjusted Regression Models Part 1: one factor and the outcomes -------------------------#

# Internalising symptoms ~ Deprivation
dep_int <- lm(Internalising_symptoms ~ Deprivation, data = MCS_merged)
summary(dep_int)

# Externalising symptoms ~ Deprivation
dep_ext <- lm(Externalising_symptoms ~ Deprivation, data = MCS_merged)
summary(dep_ext)

# Internalising symptoms ~ Victimisation
vic_int <- lm(Internalising_symptoms ~ Victimisation, data = MCS_merged)
summary(vic_int)

# Externalising symptoms ~ Victimisation
vic_ext <- lm(Externalising_symptoms ~ Victimisation, data = MCS_merged)
summary(vic_ext)

# Internalising symptoms ~ Parental Threat
threat_int <- lm(Internalising_symptoms ~ Parental_Threat, data = MCS_merged)
summary(threat_int)

# Externalising symptoms ~ Parental Threat
threat_ext <- lm(Externalising_symptoms ~ Parental_Threat, data = MCS_merged)
summary(threat_ext)

# Internalising symptoms ~ Parental Discipline
disc_int <- lm(Internalising_symptoms ~ Parental_Discipline, data = MCS_merged)
summary(disc_int)

# Externalising symptoms ~ Parental Discipline
disc_ext <- lm(Externalising_symptoms ~ Parental_Discipline, data = MCS_merged)
summary(disc_ext)

#------------------------- Adjusted Regression Models Part 2: one factor + covariates and the outcomes -------------------------#

# Internalising symptoms ~ Deprivation + Sex + Ethnicity
int1 <- lm(Internalising_symptoms ~ Deprivation + Sex + Ethnicity, data = MCS_merged)
summary(int1)

# Externalising symptoms ~ Deprivation + Sex + Ethnicity
ext1 <- lm(Externalising_symptoms ~ Deprivation + Sex + Ethnicity, data = MCS_merged)
summary(ext1)

# Internalising symptoms ~ Victimisation + Sex + Ethnicity
int2 <- lm(Internalising_symptoms ~ Victimisation + Sex + Ethnicity, data = MCS_merged)
summary(int2)

# Externalising symptoms ~ Victimisation + Sex + Ethnicity
ext2 <- lm(Externalising_symptoms ~ Victimisation + Sex + Ethnicity, data = MCS_merged)
summary(ext2)

# Internalising symptoms ~ Parental Threat + Sex + Ethnicity
int3 <- lm(Internalising_symptoms ~ Parental_Threat + Sex + Ethnicity, data = MCS_merged)
summary(int3)
 
# Externalising symptoms ~ Parental Threat + Sex + Ethnicity
ext3 <- lm(Externalising_symptoms ~ Parental_Threat + Sex + Ethnicity, data = MCS_merged)
summary(ext3)

# Internalising symptoms ~ Parental Discipline + Sex + Ethnicity
int4 <- lm(Internalising_symptoms ~ Parental_Discipline + Sex + Ethnicity, data = MCS_merged)
summary(int4)
 
# Externalising symptoms ~ Parental Discipline + Sex + Ethnicity
ext4 <- lm(Externalising_symptoms ~ Parental_Discipline + Sex + Ethnicity, data = MCS_merged)
summary(ext4)

#----------------- Regression Model Part 3: all factors + covariates and the outcomes (multiple regression) -----------------#

multireg_lm <- lm(cbind(Internalising_symptoms, Externalising_symptoms) ~ Deprivation + Victimisation + Parental_Threat + Parental_Discipline + Sex + Ethnicity, data = MCS_merged)
summary(multireg_lm)

mean(multireg_lm$residuals) # check the mean of residuals is approximately zero
qqnorm(multireg_lm$residuals) # check the residuals are normally distributed

# Calculate variance inflation factor (VIF)
# VIF is a measure to analyze the magnitude of multicollinearity of model terms
# VIF less than 5 indicates a low correlation of that predictor with other predictors
library(performance)

# Internalising symptoms
multi_int <- lm(Internalising_symptoms ~ Deprivation + Victimisation + Parental_Threat + Parental_Discipline + Sex + Ethnicity, data = MCS_merged)
summary(multi_int)

check_collinearity(multi_int)
vif(multi_int)

# Externalising symptoms
multi_ext <- lm(Externalising_symptoms ~ Deprivation + Victimisation + Parental_Threat + Parental_Discipline + Sex + Ethnicity, data = MCS_merged)
summary(multi_ext)

check_collinearity(multi_ext)
vif(multi_ext)

#------------------------- Check descriptive statistics -------------------------#

# Find number of observations used in regression (i.e., complete case sample)
nobs(multireg_lm)

# Find n and proportion of sociodemographic variables
table(MCS_merged$Sex, useNA="always")
prop.table(table(MCS_merged$Sex, useNA="always"))*100

table(MCS_merged$Ethnicity, useNA="always")
prop.table(table(MCS_merged$Ethnicity, useNA="always"))*100

# Internalising symptoms
describe(MCS_merged$Internalising_symptoms)
summary(MCS_merged$Internalising_symptoms, useNA="always")

# Externalising symptoms
describe(MCS_merged$Externalising_symptoms)
summary(MCS_merged$Externalising_symptoms, useNA="always")

#------------------------- Format regression tables into word document -------------------------#

# Format unadjusted regression models 
# Internalising symptoms
int_unadjusted <- stargazer(dep_int, vic_int, threat_int, disc_int,
                     type="html",
                     title="Unadjusted associations between ACE dimensions and internalising symptoms for the MCS complete sample",
                     covariate.labels=c("Deprivation", "Victimisation", "Parental Threat", "Parental Discipline"),
                     column.labels=c("Deprivation ~ Internalising", "Victimisation ~ Internalising", "Parental Threat ~ Internalising", "Parental Discipline ~ Internalising"),
                     column.sep.width="15pt",
                     dep.var.caption="Internalising symptoms",
                     dep.var.labels="",
                     ci=TRUE,
                     ci.level=0.95,
                     align=TRUE,
                     out="mcs_unadjusted_internalising.doc",
                     digits=2, digits.extra=3,
                     omit="Constant",
                     single.row=TRUE)

# Externalising symptoms
ext_unadjusted <- stargazer(dep_ext, vic_ext, threat_ext, disc_ext,
                     type="html",
                     title="Unadjusted associations between ACE dimensions and externalising symptoms for the MCS complete sample",
                     covariate.labels=c("Deprivation", "Victimisation", "Parental Threat", "Parental Discipline"),
                     column.labels=c("Deprivation ~ Externalising", "Victimisation ~ Externalising", "Parental Threat ~ Externalising", "Parental Discipline ~ Externalising"),
                     column.sep.width="15pt",
                     dep.var.caption="Externalising symptoms",
                     dep.var.labels="",
                     ci=TRUE,
                     ci.level=0.95,
                     align=TRUE,
                     out="mcs_unadjusted_externalising.doc",
                     digits=2, digits.extra=3,
                     omit="Constant",
                     single.row=TRUE)

## Format adjusted regression models 
# Internalising symptoms
int_adjusted <- stargazer(int1, int2, int3, int4, multi_int,
                     type="html",
                     title="Adjusted associations between ACE dimensions and internalising symptoms for the MCS complete sample",
                     covariate.labels=c("Deprivation", "Victimisation", "Parental Threat", "Parental Discipline",
                                        "Female sex", "Black or Black British", "Indian",
                                        "Mixed", "Other (inc. Chinese)",
                                        "Pakistani and Bangladeshi"),
                     column.labels=c("Deprivation ~ Internalising", "Victimisation ~ Internalising", "Parental Threat ~ Internalising", "Parental Discipline ~ Internalising", 
                                     "Deprivation + Victimisation + Parental Threat + Parental Discipline ~ Internalising"),
                     column.sep.width="15pt",
                     dep.var.caption="Internalising symptoms",
                     dep.var.labels="",
                     ci=TRUE,
                     ci.level=0.95,
                     align=TRUE,
                     out="mcs_adjusted_internalising.doc",
                     digits=2, digits.extra=3,
                     omit="Constant",
                     single.row=TRUE)

# Externalising symptoms
ext_adjusted <- stargazer(ext1, ext2, ext3, ext4, multi_ext,
                     type="html",
                     title="Adjusted associations between ACE dimensions and externalising symptoms for the MCS complete sample",
                     covariate.labels=c("Deprivation", "Victimisation", "Parental Threat", "Parental Discipline",
                                        "Female sex", "Black or Black British", "Indian",
                                        "Mixed", "Other (inc. Chinese)",
                                        "Pakistani and Bangladeshi"),
                     column.labels=c("Deprivation ~ Externalising", "Victimisation ~ Externalising", "Parental Threat ~ Externalising", "Parental Discipline ~ Externalising",
                                     "Deprivation + Victimisation + Parental Threat + Parental Discipline ~ Externalising"),
                     column.sep.width="15pt",
                     dep.var.caption="Externalising symptoms",
                     dep.var.labels="",
                     ci=TRUE,
                     ci.level=0.95,
                     align=TRUE,
                     out="mcs_adjusted_externalising.doc",
                     digits=2, digits.extra=3,
                     omit="Constant",
                     single.row=TRUE)

#------------------------- Exploratory analysis: test for interactions and stratify analyses by sex  -------------------------#

# Test if there is an interaction between sex and main effects

# Internalising symptoms
multireg_int_interaction <- lm(Internalising_symptoms ~ Deprivation + Victimisation + Parental_Threat + Parental_Discipline + Sex + Ethnicity + Sex*Deprivation + Sex*Victimisation + Sex*Parental_Threat + Sex*Parental_Discipline, data=MCS_merged)
summary(multireg_int_interaction)
# There is a significant interaction between Sex*Victimisation for internalising symptoms

# Externalising symptoms
multireg_ext_interaction <- lm(Externalising_symptoms ~ Deprivation + Victimisation + Parental_Threat + Parental_Discipline + Sex + Ethnicity + Sex*Deprivation + Sex*Victimisation + Sex*Parental_Threat + Sex*Parental_Discipline, data=MCS_merged)
summary(multireg_ext_interaction)
# There are no significant interactions for externalising symptoms

## Stratify dataset by sex and rerun analyses
MCS_male <- subset(MCS_merged, MCS_merged$Sex=="Male") 
MCS_female <- subset(MCS_merged, MCS_merged$Sex=="Female") 

# Males: Internalising symptoms ~ Deprivation + Victimisation + Parental Threat + Parental Discipline + Ethnicity
multi_int_m <- lm(Internalising_symptoms ~ Deprivation + Victimisation + Parental_Threat + Parental_Discipline + Ethnicity, data = MCS_male)
summary(multi_int_m)

# Females: Internalising symptoms ~ Deprivation + Victimisation + Parental Threat + Parental Discipline + Ethnicity
multi_int_f <- lm(Internalising_symptoms ~ Deprivation + Victimisation + Parental_Threat + Parental_Discipline + Ethnicity, data = MCS_female)
summary(multi_int_f)
```

# 4. Multiple imputation

We will conduct multiple imputation using the missForest package, a
random forest imputation algorithm for missing data.

``` r
library(missForest)
library(doParallel)
library(naniar)

# Load MCS merged dataset
setwd("/Users/athenachowruwern/Desktop")
MCS_merged <-  read.delim("MCS_merged.txt", header = TRUE, sep = "\t", dec = ".")

###### Check average percentage of missing data 

# Percentage of missingness of the whole dataset
pct_miss(MCS_merged)

# Check amount of complete data
describe(MCS_merged)$n

# Check amount of missing data per variable
colSums(is.na(MCS_merged)) 

# Find percentage of missing data per variable 
missing_pct <- as.data.frame((colMeans(is.na(MCS_merged)))*100)

missing_pct <- missing_pct %>% 
  rename(percentage_missing = "(colMeans(is.na(MCS_merged))) * 100") # rename column name

missing_pct %>% arrange(desc(percentage_missing)) # descending order

# Remove variables with > 70% missingness
drops <- c("EPLOOK00", # 97%, frequency looks after CM on own without main respondent being there
"DPLOOK00", # 97%, how often looks after CM on own 
"DPBEDR00", # 97%, how often puts CM to bed
"CPBEDR00", # 97%, how often puts CM to bed
"CPLOOK00", # 97%, how often looks after CM on own 
"BPOFLI00") # 69%, how often child is taken to library

MCS_merged <- MCS_merged[ , !(names(MCS_merged) %in% drops)]

# Also remove the following variables from Golombok marital questionnaire 
# as they were not used to derive the parental relationship variable
drops <- c("APRESE00", "APREIS00", "APRELO00", "APREJO00", "APREWA00", "APRESN00", "APMAUP00", 
           "BPRESE00", "BPRELS00", "BPRELO00", "BPRESN00", "BPREIS00", "BPCOLT00", 
           "CPRESE00", "CPRELS00", "CPRELO00", "CPRESN00", "CPREIS00", "CPCOLT00", 
           "DPREGN00", "DPCOLT00")

MCS_merged <- MCS_merged[ , !(names(MCS_merged) %in% drops)]

str(MCS_merged, list.len=ncol(MCS_merged))

#------------------------- Impute, then transform  -------------------------#
# Raw variables used to create the derived variables will be imputed individually, 
# and after imputation the variables will be re-derived from the imputed dataset

# Specify categories for transformation in imputation model
colnames(MCS_merged)

# Recode auxiliary variables that should be numeric
numeric_vars <- c("ADDAGB00", # mother's age at birth of CM  
                  "ADBWGT00", # birthweight in kilos
                  "ADGEST00", # gestation time in days
                  "ADWGTK00", # maternal weight in kilos
                  "ADMBMI00", # maternal BMI at interview (CM age 9 months)
                  "BEBDTOT", # SDQ Total Difficulties MCS2 
                  "CEBDTOT", # SDQ Total Difficulties MCS3
                  "DDDEBDTOT", # SDQ Total Difficulties MCS4
                  "EEBDTO_T", # SDQ Total Difficulties MCS5 (teacher reported)
                  "FEBDTOT") # SDQ Total Difficulties MCS6

MCS_merged[numeric_vars] <- lapply(MCS_merged[numeric_vars], as.numeric)

###### Recode categorical auxiliary variables to have fewer categories

# Tenure of current home (owns/rents) will be recoded to home ownership (yes/no)
table(MCS_merged$ADROOW00, useNA="always")
MCS_merged$ADROOW00 <- recode(MCS_merged$ADROOW00, "'Own outright'='Yes'; 'Own - mortgage/loan'='Yes'; 'Part rent/part mortgage (shared equity)'='Yes'; 'Rent from local authority'='No'; 'Rent from Housing Association'='No'; 'Rent privately'='No'; 'Living with parents'='No'; 'Live rent free'='No'; 'Squatting'='No'; 'Other'='No'")
table(MCS_merged$ADROOW00, useNA="always")

# Highest maternal academic qualification will be recoded to degree (yes/no)
table(MCS_merged$APACQU00, useNA="always")
MCS_merged$APACQU00 <- recode(MCS_merged$APACQU00, "'Higher degree'='Yes'; 'First degree'='Yes'; 'Diplomas in higher education'='Yes'; 'A / AS / S levels'='No'; 'O level / GCSE grades A-C'='No'; 'GCSE grades D-G'='No'; 'Other academic qualifications'='No'; 'None of these qualifications'='No'")
table(MCS_merged$APACQU00, useNA="always")

# Check that auxiliary variables have been recoded correctly
str(MCS_merged, list.len=ncol(MCS_merged))

###### Recode marital status variables to be binary (as they will be recoded to single parent variable anyway)
table(MCS_merged$APFCIN00, useNA="always")
MCS_merged$APFCIN00 <- recode(MCS_merged$APFCIN00, "'Legally separated'=1; 'Married, 1st and only marriage'=0; 'Remarried, 2nd or later marriage'=0; 'Single never married'=1; 'Divorced'=1; 'Widowed'=1")
table(MCS_merged$APFCIN00, useNA="always")

table(MCS_merged$BPFCIN00, useNA="always")
MCS_merged$BPFCIN00 <- recode(MCS_merged$BPFCIN00, "'Legally separated'=1; 'Married, 1st and only marriage'=0; 'Remarried, 2nd or later marriage'=0; 'Single never married'=1; 'Divorced'=1; 'Widowed'=1")
table(MCS_merged$BPFCIN00, useNA="always")

table(MCS_merged$CPFCIN00, useNA="always")
MCS_merged$CPFCIN00 <- recode(MCS_merged$CPFCIN00, "'Legally separated'=1; 'Married, 1st and only marriage'=0; 'Remarried, 2nd or later marriage'=0; 'Single never married'=1; 'Divorced'=1; 'Widowed'=1")
table(MCS_merged$CPFCIN00, useNA="always")

table(MCS_merged$DPFCIN00, useNA="always")
MCS_merged$DPFCIN00 <- recode(MCS_merged$DPFCIN00, "'Legally separated      '=1; 'Married, 1st and only marriage '=0; 'Remarried, 2nd or later marriage       '=0; 'Single never married   '=1; 'Divorced       '=1; 'Widowed'=1")
table(MCS_merged$DPFCIN00, useNA="always")

table(MCS_merged$EPFCIN00, useNA="always")
MCS_merged$EPFCIN00 <- recode(MCS_merged$EPFCIN00, "'Legally separated      '=1; 'Married, 1st and only marriage '=0; 'Remarried, 2nd or later marriage       '=0; 'Single, never married  '=1; 'Divorced       '=1; 'Widowed'=1; 'A Civil Partner (legally recognised)   '=0; 'A former Civil Partner '=1; 'A surviving Civil Partner      '=1")
table(MCS_merged$EPFCIN00, useNA="always")

table(MCS_merged$FPFCIN00, useNA="always")
MCS_merged$FPFCIN00 <- recode(MCS_merged$FPFCIN00, "'Legally separated      '=1; 'Married, 1st and only marriage '=0; 'Remarried, 2nd or later marriage       '=0; 'Single, never married and never in a Civil Partnership'=1; 'Divorced       '=1; 'Widowed'=1; 'A Civil Partner in a legally recognised Civil Partnership'=0; 'A former Civil Partner (where Civil Partnership legally dissolved)'=1; 'A surviving Civil Partner (where Civil Partner has died)'=1")
table(MCS_merged$FPFCIN00, useNA="always")

###### Recode responses of "can't say/don't know/refused" to NA

# Parental mental health (Sweep 2, age 3)
MCS_merged <- MCS_merged %>% 
  mutate_at(c("BPPHDE00","BPPHHO00","BPPHRF00","BPPHEE00","BPPHWO00","BPPHNE00"), 
            function(x) recode(x, "\"Can't say\"=NA"))

# Parental mental health (Sweep 3, age 5)
MCS_merged <- MCS_merged %>% 
  mutate_at(c("CPPHDE00","CPPHHO00","CPPHRF00","CPPHEE00","CPPHWO00","CPPHNE00"), 
            function(x) recode(x, "'Can t say'=NA"))

# Parental mental health (Sweep 4, age 7)
MCS_merged <- MCS_merged %>% 
  mutate_at(c("DPPHDE00","DPPHHO00","DPPHRF00","DPPHEE00","DPPHWO00","DPPHNE00"), 
            function(x) recode(x, "'Can t say      '=NA"))

# Parental mental health (Sweep 5, age 11)
MCS_merged <- MCS_merged %>% 
  mutate_at(c("EPPHDE00","EPPHHO00","EPPHRF00","EPPHEE00","EPPHWO00","EPPHNE00"), 
            function(x) recode(x, "\"Don’t know/don’t wish to answer\"=NA"))

# Frequent parental alcohol use (Sweep 2, age 3)
MCS_merged$BPALDR00 <- recode(MCS_merged$BPALDR00, "'Refused'=NA; 'Refused_duplicated_8'=NA")

# Frequent parental alcohol use (Sweep 5, age 11)
MCS_merged$EPALDR00 <- recode(MCS_merged$EPALDR00, "'Don’t know/don’t wish to answer'=NA")

# Parental drug use (Sweep 2, age 3)
MCS_merged$BPDRUG00 <- recode(MCS_merged$BPDRUG00, "\"Can't say\"=NA")

# Parental drug use (Sweep 3, age 5)
MCS_merged$CPDRUG00 <- recode(MCS_merged$CPDRUG00, "'Can t say'=NA")

# Parental relationship happiness (Sweep 2, age 3)
MCS_merged$BPHARE00 <- recode(MCS_merged$BPHARE00, "\"Can't say\"=NA")

# Parental relationship happiness (Sweep 3, age 5)
MCS_merged$CPHARE00 <- recode(MCS_merged$CPHARE00, "\"Can't say\"=NA")

# Parental relationship happiness (Sweep 4, age 7)
MCS_merged$DPHARE00 <- recode(MCS_merged$DPHARE00, "\"Can't say\"=NA")

# Domestic violence (Sweep 1, 9 months)
MCS_merged$APFORC00 <- recode(MCS_merged$APFORC00, "'Don t want to answer'=NA")

# Domestic violence (Sweep 2, age 3)
MCS_merged$BPFORC00 <- recode(MCS_merged$BPFORC00, "\"Refusal\"=NA; \"Don't want to answer\"=NA")

# Domestic violence (Sweep 3, age 5)
MCS_merged$CPFORC00 <- recode(MCS_merged$CPFORC00, "'Don t want to answer'=NA")

# Domestic violence (Sweep 4, age 7)
MCS_merged$DPFORC00 <- recode(MCS_merged$DPFORC00, "'Don t want to answer   '=NA")

# Domestic violence (Sweep 5, age 11)
MCS_merged$EPFORC00 <- recode(MCS_merged$EPFORC00, "\"Don’t know/don’t wish to answer\"=NA")

# Parental discipline (Sweep 2, age 3)
MCS_merged <- MCS_merged %>% 
  mutate_at(c("BPDIIG00","BPDISH00","BPDIBN00","BPDITR00","BPDITE00","BPDIBR00"), 
            function(x) recode(x, "\"Can't say\"=NA"))

# Parental discipline (Sweep 3, age 5)
MCS_merged <- MCS_merged %>% 
  mutate_at(c("CPDIIG00","CPDISH00","CPDIBN00","CPDITR00","CPDITE00","CPDIBR00"), 
            function(x) recode(x, "'Cant say'=NA"))

# Parental discipline (Sweep 4, age 7)
MCS_merged <- MCS_merged %>% 
  mutate_at(c("DPDIIG00","DPDISH00","DPDIBN00","DPDITR00","DPDITE00","DPDIBR00"), 
            function(x) recode(x, "'Cant say       '=NA"))

# Parental smacking (Sweep 2, age 3)
MCS_merged$BPDISM00 <- recode(MCS_merged$BPDISM00, "\"Can't say\"=NA")

# Parental smacking (Sweep 3, age 5)
MCS_merged$CPDISM00 <- recode(MCS_merged$CPDISM00, "'Cant say'=NA")

# Parental smacking (Sweep 4, age 7)
MCS_merged$DPDISM00 <- recode(MCS_merged$DPDISM00, "'Cant say       '=NA")

# Home environment (Sweep 2, age 3)
MCS_merged <- MCS_merged %>% 
  mutate_at(c("BCENVI00","BCTOYS00","BCDARK00","BCRCLE00"), 
            function(x) recode(x, "'Not observed   '=NA"))

MCS_merged <- MCS_merged %>% 
  mutate_at(c("BCSPEA00","BCMCON00","BCANSW00","BCPRAI00","BCINTI00","BCSCOL00"), 
            function(x) revalue(x, c("= Can't tell - including does not speak English"=NA)))

# Peer victimisation (Sweep 2, age 3)
MCS_merged$BPSDPB00 <- recode(MCS_merged$BPSDPB00, "\"Can't say\"=NA")

# Peer victimisation (Sweep 3, age 5)
MCS_merged$CPSDPB00 <- recode(MCS_merged$CPSDPB00, "'Can t say'=NA")

# Peer victimisation (Sweep 4, age 7)
MCS_merged$DPSDPB00 <- recode(MCS_merged$DPSDPB00, "'Can t say      '=NA")

# Peer victimisation (Sweep 5, age 11)
MCS_merged$EPSDPB00 <- recode(MCS_merged$EPSDPB00, "\"Don’t know/Don’t wish to answer\"=NA")

# Home area safety (Sweep 2, age 3)
MCS_merged$BPARGD00 <- revalue(MCS_merged$BPARGD00, c("Dont know"=NA, "Spontaneous:ONLY: Don't know"=NA))

# Child illness (Sweep 5, age 11)
MCS_merged$EPCLSI00 <- recode(MCS_merged$EPCLSI00, "\"Refused\"=NA; \"Don't know     \"=NA")

# Cognitive stimulation (Sweep 5, age 11)
MCS_merged <- MCS_merged %>% 
  mutate_at(c("EPACTI00","EPGAME00","EPTAIM00"), 
            function(x) recode(x, "\"Refused\"=NA; \"Don't know     \"=NA"))

# Check that variables have been recoded correctly
str(MCS_merged, list.len=ncol(MCS_merged))

# Next we will run the imputation using missForest, a random forest imputation algorithm for missing data
# MissForest initially imputes all missing data using the mean/mode, then for each variable with missing values, 
# it fits a random forest on the observed part and then predicts the missing part
# This process of training and predicting repeats in an iterative process until a stopping criterion is met

# Following guidance from Liam Morgan: https://rpubs.com/lmorgan95/MissForest

# MacBook Pro 2021 Total Number of Cores:   8 (6 performance and 2 efficiency)
doParallel::registerDoParallel(cores = 8) # set based on number of CPU cores
doRNG::registerDoRNG(seed = 123) # set seed for reproducible results

# Temporarily remove ID column, we will append it back later
# ID column has 18,539 cases - 100% complete, no missingness
MCS_merged <- MCS_merged[, -which(names(MCS_merged) == "MCSID")]

# Convert nominal and ordinal items to factor so that missForest imputes them as integers
# Otherwise the binary items (0, 1) will be imputed as fractions  
str(MCS_merged, list.len=ncol(MCS_merged))

# The following items are all supposed to be integers (whole numbers) or binary (0,1) 
# or they are ranked in ordered categories e.g., ranging from 0 - 5 
MCS_merged <- MCS_merged %>% 
  mutate_at(c(
## Nominal variables
  # Early sociodemographic indicators
  "AHCSEX00", # cohort member sex
  "ADC06E00", # cohort member ethnicity
  "ADROOW00", # tenure of current home (owns/rents)
  "APACQU00", # whether mother has degree 
  "CPSMKR00", # whether anyone smokes in the same room as CM 
  # ACEs at Sweep 1 (9 months)
  "APTIRE00", "APDEPR00", "APWORR00", "APRAGE00", # parental psychosocial distress (Rutter)
  "APSCAR00", "APUPSE00", "APKEYD00", "APNERV00", "APHERA00", # parental psychosocial distress (Rutter)
  "APFCIN00", # marital status
  "APPLSA00", # any places where children can play safely
  "APFORC00", # whether partner ever used force in relationship (Golombok)
  # ACEs at Sweep 2 (age 3)
  "BPFCIN00", # marital status
  "BPFORC00", # whether partner ever used force in relationship (Golombok)
  "BPREEL00", # anyone else read to the child
  "BPTOLI00", # anyone at home takes child to library
  "BPSDPA00", # anyone teaches child sport
  "BPALPH00", # anyone helps child with alphabet
  "BPNUMB00", # anyone helps child with counting
  "BPSONG00", # anyone teaches child songs
  "BPDRAW00", # does child paint/draw at home
  "BPEATW00", # child eaten with family past week   
  "BPBIRT00", # something special for child's third birthday
  "BPYOCH00", # been visited by friends with young children 
  "BPCLSI00", # child has any longstanding health conditions
  "BCENVI00", "BCTOYS00", "BCSEEC00", "BCDARK00", # negative child home environment (HOME-SF)
  "BCRCLE00", "BCUNCL00", "BCSPEA00", "BCMCON00", "BCANSW00", # negative child home environment (HOME-SF)
  "BCPRAI00", "BCKISS00", "BCINTI00", "BCSCOL00", "BCPHYS00", "BCSLAP00", # negative child home environment (HOME-SF)
  # ACEs at Sweep 3 (age 5)
  "CPFCIN00", # marital status 
  "CPFORC00", # whether partner ever used force in relationship (Golombok)
  "CPCLSI00", # child has any longstanding health conditions
  # ACEs at Sweep 4 (age 7)
  "DPFCIN00", # marital status 
  "DPFORC00", # whether partner ever used force in relationship (Golombok)
  "DPCLSI00", # child has any longstanding health conditions
  # ACEs at Sweep 5 (age 11)
  "EPFCIN00", # marital status
  "EPFORC00", # whether partner ever used force in relationship (Golombok)
  "EPCLSI00", # child has any longstanding health conditions
  # ACEs at Sweep 6 (age 14)
  "FPFCIN00", # marital status 
  "FPFORC00", # whether partner ever used force in relationship (Golombok)
  "FPCLSI00", # child has any longstanding health conditions
  "FCVICG00", # CM insulted/threatened/shouted at
  "FCVICA00", # been physically violent towards CM
  "FCVICC00", # hit or used weapon against CM
  "FCVICE00", # stolen something from CM
  "FCVICF0A", # sexually assaulted CM
## Ordinal variables
  # Early sociodemographic indicators
  "ADD05C00", # socioeconomic NS-SEC 5 classes 
  # ACEs at Sweep 1 (9 months)
  "APALDR00", # frequency of alcohol consumption
  "APHOSA00", # satisfaction with home
  "APAREA00", # satisfaction with area
  "APARNN00", # noisy neighbours
  "APARRU00", # how common are rubbish/litter in area
  "APARVD00", # how common are vandalism in area
  "APARRC00", # how common are racist insults/attacks in area
  "APTRAN00", # poor public transport
  "APSHOP00", # food shops/supermarkets in easy access
  "APARPG00", # pollution/grime/environmental problems
  "APRESE00", "APREIS00", "APRELO00", "APREJO00", # parental relationship (Golombok)
  "APREWA00", "APRESN00", "APMAUP00", "APHARE00", # parental relationship (Golombok)
  "AOECDUK0", # household income (OECD weighted quintiles UK analysis)
  # ACEs at Sweep 2 (age 3)
  "BPPHDE00", "BPPHHO00", "BPPHRF00", "BPPHEE00", "BPPHWO00", "BPPHNE00", # parental mental health (Kessler)
  "BPALDR00", # frequency of alcohol consumption
  "BPDRUG00", # used recreational drugs
  "BPRESE00", "BPRELS00", "BPRELO00", "BPRESN00", # parental relationship (Golombok)
  "BPREIS00", "BPCOLT00", "BPHARE00", # parental relationship (Golombok)
  "BPDIIG00", "BPDISM00", "BPDISH00", "BPDIBN00", # parental discipline (Straus)
  "BPDITR00", "BPDITE00", "BPDIBR00", # parental discipline (Straus)
  "BPARGD00", # is this a good area to bring up a child
  "BPARAR00", # how safe do you feel about the area you live in
  "BPOFRE00", # how often do you read to the child
  "BPREOF00", # how often anyone else reads to the child 
  "BPOFAB00", # how often child is helped with alphabet
  "BPOFCO00", # how often child is taught counting
  "BPOFSO00", # how often child is taught songs
  "BPPAMA00", # how often child paints/draws at home
  "BPSDPB00", # CM picked on or bullied by other children
  "BOECDUK0", # household income (OECD weighted quintiles UK analysis)
  "BCCOMF00", # negative child home environment (HOME-SF) how at ease parent appeared
  # ACEs at Sweep 3 (age 5)
  "CPPHDE00", "CPPHHO00", "CPPHRF00", "CPPHEE00", "CPPHWO00", "CPPHNE00", # parental mental health (Kessler)
  "CPALDR00", # frequency of alcohol consumption
  "CPDRUG00", # used recreational drugs
  "CPRESE00", "CPRELS00", "CPRELO00", "CPRESN00", # parental relationship (Golombok)
  "CPREIS00", "CPCOLT00", "CPHARE00", # parental relationship (Golombok)
  "CPARGD00", # is this a good area to bring up a child
  "CPARAR00", # how safe do you feel about the area you live in
  "CPREOF00", "CPSITS00", # how often child is read to
  "CPPLMU00", "CPPAMA00", # how often musical activities & painting
  "CPACTI00", "CPGAME00", "CPWALK00", # how often plays games with child & playground
  "CPDIIG00", "CPDISM00", "CPDISH00", "CPDIBN00", # parental discipline (Straus)
  "CPDITR00", "CPDITE00", "CPDIBR00", # parental discipline (Straus)
  "CPSDPB00", # CM picked on or bullied by other children
  "COECDUK0", # household income (OECD weighted quintiles UK analysis)
  # ACEs at Sweep 4 (age 7)
  "DPPHDE00", "DPPHHO00", "DPPHRF00", "DPPHEE00", "DPPHWO00", "DPPHNE00", # parental mental health (Kessler)
  "DPALDR00", # frequency of alcohol consumption
  "DPREGN00", "DPCOLT00", "DPHARE00", # parental relationship (Golombok)
  "DPREOF00", "DPSITS00", # how often child is read to
  "DPPLMU00", "DPPAMA00", # how often musical activities & painting
  "DPACTI00", "DPGAME00", "DPWALK00", # how often plays games with child & playground
  "DPDIIG00", "DPDISM00", "DPDISH00", "DPDIBN00", # parental discipline (Straus)
  "DPDITR00", "DPDITE00", "DPDIBR00", # parental discipline (Straus)
  "DPSDPB00", # CM picked on or bullied by other children
  "DOECDUK0", # household income (OECD weighted quintiles UK analysis)
  "DCSC0036", # how often other children bully CM
  "DQ2189", # CM is picked on or bullied by other children (teacher report)
  # ACEs at Sweep 5 (age 11)
  "EPPHDE00", "EPPHHO00", "EPPHRF00", "EPPHEE00", "EPPHWO00", "EPPHNE00", # parental mental health (Kessler)
  "EPALDR00", # frequency of alcohol consumption
  "EPHARE00", # parental relationship (Golombok)
  "EPARGD00", # is this a good area to bring up a child
  "EPACTI00", # how often do you play physically active games with CM?  
  "EPGAME00", # frequency play INDOOR games with child  
  "EPTAIM00", # frequency talks to CM about things important to them
  "EOECDUK0", # household income (OECD weighted quintiles UK analysis)
  "ECQ23X00", # how safe is it to walk/play in this area 
  "EPSDPB00", # CM picked on or bullied by other children
  "ECQ56X00", # how often other children bully CM
  "ECQ54X00", # how often brothers/sisters pick on CM
  "EQ5S", # CM is is picked on or bullied by other children (teacher report)
  # ACEs at Sweep 6 (age 14)
  "FPPHDE00", "FPPHHO00", "FPPHRF00", "FPPHEE00", "FPPHWO00", "FPPHNE00", # parental mental health (Kessler)
  "FPALDR00", # frequency of alcohol consumption
  "FPDRUG00", # used recreational drugs
  "FPHARE00", # parental relationship (Golombok)
  "FPSDPB00", # CM picked on or bullied by other children
  "FOECDUK0", # household income (OECD weighted quintiles UK analysis)
  "FCHURT00", # how often other children bully CM
  "FCBULB00", # how often brothers/sisters pick on CM
  # Adolescent mental health at Sweep 7 (age 17)
  "GCPHDE00", # Kessler: so depressed nothing could cheer you up
  "GCPHHO00", # Kessler: hopeless
  "GCPHRF00", # Kessler: restless or fidgety
  "GCPHEE00", # Kessler: everything was an effort
  "GCPHWO00", # Kessler: worthless
  "GCPHNE00", # Kessler: nervous
  # Internalising symptoms at Sweep 7 (age 17)
  "GCSDQC00", # SDQ: headaches, stomachaches
  "GCSDQH00", # SDQ: worry a lot
  "GCSDQM00", # SDQ: often unhappy 
  "GCSDQP00", # SDQ: nervous in new situations
  "GCSDQX00", # SDQ: many fears, easily scared
  "GCSDQF00", # SDQ: usually on my own
  "GCSDQK00", # SDQ: one good friend or more
  "GCSDQN00", # SDQ: other people my age like me
  "GCSDQS00", # SDQ: others pick on me or bully me
  "GCSDQW00", # SDQ: get on better with adults
  # Externalising symptoms at Sweep 7 (age 17)
  "GCSDQE00", # SDQ: often lose my temper
  "GCSDQG00", # SDQ: usually do as told
  "GCSDQL00", # SDQ: fight a lot
  "GCSDQR00", # SDQ: often accused of lying or cheating
  "GCSDQV00", # SDQ: take things that are not mine
  "GCSDQB00", # SDQ: restless, cannot stay still 
  "GCSDQJ00", # SDQ: constantly fidgeting 
  "GCSDQO00", # SDQ: easily distracted
  "GCSDQU00", # SDQ: think before do things
  "GCSDQY00"), # SDQ: finish the work, attention is good
     function(x) as.factor(x))
    
# Only the following numeric variables shouldn't be converted to factor variables,
# but they should still be converted to integers:
MCS_merged <- MCS_merged %>% 
  mutate_at(c(
    # Auxiliary variables
    "ADDAGB00", # mother's age at birth of CM  
    "ADBWGT00", # birthweight in kilos
    "ADGEST00", # gestation time in days
    "ADWGTK00", # maternal weight in kilos
    "ADMBMI00", # maternal BMI at interview (CM age 9 months)
    "BEBDTOT", # SDQ Total Difficulties MCS2 
    "CEBDTOT", # SDQ Total Difficulties MCS3
    "DDDEBDTOT", # SDQ Total Difficulties MCS4
    "EEBDTO_T", # SDQ Total Difficulties MCS5 (teacher reported)
    "FEBDTOT"), # SDQ Total Difficulties MCS6
       function(x) as.integer(x))

str(MCS_merged, list.len=ncol(MCS_merged))
    
# Impute dataset (50 trees for each forest, 10 iterations)
imputed_MCS <- missForest(xmis = MCS_merged, # specify dataset
                        maxiter = 10, # max number of iterations
                        ntree = 50, # number of trees for each forest
                        verbose = TRUE, # track progress between iterations
                        parallelize = 'forests')$ximp

# Check imputed values
dim(imputed_MCS)
str(imputed_MCS, list.len=ncol(imputed_MCS))

# Wednesday 25 Oct 2023: started 2:25 PM, ended 7:17 PM, took around 4 hours 52 mins

# Append ID column back to dataset
imputed_MCS$MCSID <- MCS_merged$MCSID
rm(MCS_merged)

# Save workspace including missForest dataset
save(imputed_MCS, file = "MCS_imputed_missForest.RData")
```

# 5. Imputed data cleaning

We will rederive ACEs and outcome variables in the missForest imputed
dataset, in preparation for the imputed data analyses.

``` r
# Load imputed data 
setwd("/Users/athenachowruwern/Desktop")
load("MCS_imputed_missForest.RData")

## ================== Parental mental health: Rutter Malaise Inventory Scale (Sweep 1, 9 months) ==================
# Recode variables to 1/0
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("APTIRE00","APDEPR00","APWORR00","APRAGE00","APSCAR00","APUPSE00","APKEYD00","APNERV00","APHERA00"), 
            function(x) recode(x, "'Yes'=1; 'No'=0"))

# Convert variables from factor to numeric
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("APTIRE00","APDEPR00","APWORR00","APRAGE00","APSCAR00","APUPSE00","APKEYD00","APNERV00","APHERA00"), 
            function(x) as.numeric(as.character(x)))

# Sum items together into Rutter_MCS1
imputed_MCS$Rutter_MCS1 <- apply(imputed_MCS[,c("APTIRE00","APDEPR00","APWORR00","APRAGE00","APSCAR00","APUPSE00","APKEYD00","APNERV00","APHERA00")], 1, sum, na.rm=T)
table(imputed_MCS$Rutter_MCS1, useNA="always")

# If scores >=4, code to 1 for poor parental mental health, else code to 0
imputed_MCS$Rutter_MCS1 <- ifelse(imputed_MCS$Rutter_MCS1>=4, 1, 0)
table(imputed_MCS$Rutter_MCS1, useNA="always")

## ================== Parental mental health: Kessler Scale (Sweeps 2-6, ages 3, 5, 7, 11, 14) ==================
### (Sweep 2, age 3)
# Recode variables to 5-point scale
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("BPPHDE00","BPPHHO00","BPPHRF00","BPPHEE00","BPPHWO00","BPPHNE00"), 
            function(x) recode(x, "'All of the time'=4; 'Most of the time'=3; 'Some of the time'=2; 'A little of the time'=1; 'None of the time'=0"))

# Convert variables from factor to numeric
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("BPPHDE00","BPPHHO00","BPPHRF00","BPPHEE00","BPPHWO00","BPPHNE00"), 
            function(x) as.numeric(as.character(x)))

# Sum items together into Kessler_MCS2
imputed_MCS$Kessler_MCS2 <- apply(imputed_MCS[,c("BPPHDE00","BPPHHO00","BPPHRF00","BPPHEE00","BPPHWO00","BPPHNE00")], 1, sum, na.rm=T)
table(imputed_MCS$Kessler_MCS2, useNA="always")

# If scores >=13, code to 1 for poor parental mental health, else code to 0
imputed_MCS$Kessler_MCS2 <- ifelse(imputed_MCS$Kessler_MCS2>=13, 1, 0)
table(imputed_MCS$Kessler_MCS2, useNA="always")

### (Sweep 3, age 5)
# Recode variables to 5-point scale
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("CPPHDE00","CPPHHO00","CPPHRF00","CPPHEE00","CPPHWO00","CPPHNE00"), 
            function(x) recode(x, "'All of the time'=4; 'Most of the time'=3; 'Some of the time'=2; 'A little of the time'=1; 'None of the time'=0"))

# Convert variables from factor to numeric
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("CPPHDE00","CPPHHO00","CPPHRF00","CPPHEE00","CPPHWO00","CPPHNE00"), 
            function(x) as.numeric(as.character(x)))

# Sum items together into Kessler_MCS3
imputed_MCS$Kessler_MCS3 <- apply(imputed_MCS[,c("CPPHDE00","CPPHHO00","CPPHRF00","CPPHEE00","CPPHWO00","CPPHNE00")], 1, sum, na.rm=T)
table(imputed_MCS$Kessler_MCS3, useNA="always")

# If scores >=13, code to 1 for poor parental mental health, else code to 0
imputed_MCS$Kessler_MCS3 <- ifelse(imputed_MCS$Kessler_MCS3>=13, 1, 0)
table(imputed_MCS$Kessler_MCS3, useNA="always")

### (Sweep 4, age 7)
# Recode variables to 5-point scale
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("DPPHDE00","DPPHHO00","DPPHRF00","DPPHEE00","DPPHWO00","DPPHNE00"), 
            function(x) recode(x, "'All of the time'=4; 'Most of the time       '=3; 'Some of the time       '=2; 'A little of the time   '=1; 'None of the time       '=0"))

# Convert variables from factor to numeric
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("DPPHDE00","DPPHHO00","DPPHRF00","DPPHEE00","DPPHWO00","DPPHNE00"), 
            function(x) as.numeric(as.character(x)))

# Sum items together into Kessler_MCS4
imputed_MCS$Kessler_MCS4 <- apply(imputed_MCS[,c("DPPHDE00","DPPHHO00","DPPHRF00","DPPHEE00","DPPHWO00","DPPHNE00")], 1, sum, na.rm=T)
table(imputed_MCS$Kessler_MCS4, useNA="always")

# If scores >=13, code to 1 for poor parental mental health, else code to 0
imputed_MCS$Kessler_MCS4 <- ifelse(imputed_MCS$Kessler_MCS4>=13, 1, 0)
table(imputed_MCS$Kessler_MCS4, useNA="always")

### (Sweep 5, age 11)
# Recode variables to 5-point scale
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("EPPHDE00","EPPHHO00","EPPHRF00","EPPHEE00","EPPHWO00","EPPHNE00"), 
            function(x) recode(x, "'All of the time'=4; 'Most of the time       '=3; 'Some of the time       '=2; 'A little of the time   '=1; 'None of the time       '=0"))

# Convert variables from factor to numeric
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("EPPHDE00","EPPHHO00","EPPHRF00","EPPHEE00","EPPHWO00","EPPHNE00"), 
            function(x) as.numeric(as.character(x)))

# Sum items together into Kessler_MCS5
imputed_MCS$Kessler_MCS5 <- apply(imputed_MCS[,c("EPPHDE00","EPPHHO00","EPPHRF00","EPPHEE00","EPPHWO00","EPPHNE00")], 1, sum, na.rm=T)
table(imputed_MCS$Kessler_MCS5, useNA="always")

# If scores >=13, code to 1 for poor parental mental health, else code to 0
imputed_MCS$Kessler_MCS5 <- ifelse(imputed_MCS$Kessler_MCS5>=13, 1, 0)
table(imputed_MCS$Kessler_MCS5, useNA="always")

### (Sweep 6, age 14)
# Recode variables to 5-point scale
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("FPPHDE00","FPPHHO00","FPPHRF00","FPPHEE00","FPPHWO00","FPPHNE00"), 
            function(x) recode(x, "'All of the time'=4; 'Most of the time       '=3; 'Some of the time       '=2; 'A little of the time   '=1; 'None of the time       '=0"))

# Convert variables from factor to numeric
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("FPPHDE00","FPPHHO00","FPPHRF00","FPPHEE00","FPPHWO00","FPPHNE00"), 
            function(x) as.numeric(as.character(x)))

# After recoding, run tables to check we recoded it correctly 
table(imputed_MCS$FPPHDE00, useNA="always")
table(imputed_MCS$FPPHHO00, useNA="always")
table(imputed_MCS$FPPHRF00, useNA="always")
table(imputed_MCS$FPPHEE00, useNA="always")
table(imputed_MCS$FPPHWO00, useNA="always")
table(imputed_MCS$FPPHNE00, useNA="always")

# Sum items together into Kessler_MCS6
imputed_MCS$Kessler_MCS6 <- apply(imputed_MCS[,c("FPPHDE00","FPPHHO00","FPPHRF00","FPPHEE00","FPPHWO00","FPPHNE00")], 1, sum, na.rm=T)
table(imputed_MCS$Kessler_MCS6, useNA="always")

# If scores >=13, code to 1 for poor parental mental health, else code to 0
imputed_MCS$Kessler_MCS6 <- ifelse(imputed_MCS$Kessler_MCS6>=13, 1, 0)
table(imputed_MCS$Kessler_MCS6, useNA="always")

### SUM RUTTER AND KESSLER SCORES ACROSS SWEEPS 1,2,3,4,5,6
# If parents scored poor mental health for at least 1 sweep, code as 1 for poor parental mental health
imputed_MCS$Parental_mental_health <- 0
imputed_MCS$Parental_mental_health[imputed_MCS$Rutter_MCS1==1 | imputed_MCS$Kessler_MCS2==1 | 
                                    imputed_MCS$Kessler_MCS3==1 | imputed_MCS$Kessler_MCS4==1 | 
                                    imputed_MCS$Kessler_MCS5==1 | imputed_MCS$Kessler_MCS6==1] <- 1

table(imputed_MCS$Parental_mental_health, useNA="always")
prop.table(table(imputed_MCS$Parental_mental_health, useNA="always"))*100

## =============== Frequent parental alcohol use (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years) ===============
### (Sweep 1, 9 months)
imputed_MCS$APALDR00 <- recode(imputed_MCS$APALDR00, "'Every day'=1; '5-6 times per week'=1; '3-4 times per week'=0; '1-2 times per week'=0; '1-2 times per month'=0; 'Less than once a month'=0; 'Never'=0")
imputed_MCS$APALDR00 <- as.numeric(as.character(imputed_MCS$APALDR00))

### (Sweep 2, age 3)
imputed_MCS$BPALDR00 <- recode(imputed_MCS$BPALDR00, "'Every day'=1; '5-6 times per week'=1; '3-4 times per week'=0; '1-2 times per week'=0; '1-2 times per month'=0; 'Less than once a month'=0; 'Never'=0; 'Refused'=NA; 'Refused_duplicated_8'=NA")
imputed_MCS$BPALDR00 <- as.numeric(as.character(imputed_MCS$BPALDR00))

### (Sweep 3, age 5)
imputed_MCS$CPALDR00 <- recode(imputed_MCS$CPALDR00, "'Every day'=1; '5-6 times per week'=1; '3-4 times per week'=0; '1-2 times per week'=0; '1-2 times per month'=0; 'Less than once a month'=0; 'Never'=0")
imputed_MCS$CPALDR00 <- as.numeric(as.character(imputed_MCS$CPALDR00))

### (Sweep 4, age 7)
imputed_MCS$DPALDR00 <- recode(imputed_MCS$DPALDR00, "'Every day      '=1; '5-6 times per week     '=1; '3-4 times per week     '=0; '1-2 times per week     '=0; '1-2 times per month    '=0; 'Less than once a month '=0; 'Never  '=0")
imputed_MCS$DPALDR00 <- as.numeric(as.character(imputed_MCS$DPALDR00))

### (Sweep 5, age 11)
imputed_MCS$EPALDR00 <- recode(imputed_MCS$EPALDR00, "'4 or more times a week '=1; '2-3 times a week       '=0; '2-4 times per month    '=0; 'Monthly or less'=0; 'Never  '=0; 'Don’t know/don’t wish to answer'=NA")
imputed_MCS$EPALDR00 <- as.numeric(as.character(imputed_MCS$EPALDR00))

### (Sweep 6, age 14)
imputed_MCS$FPALDR00 <- recode(imputed_MCS$FPALDR00, "'4 or more times a week '=1; '2-3 times a week       '=0; '2-4 times per month    '=0; 'Monthly or less'=0; 'Never  '=0")
imputed_MCS$FPALDR00 <- as.numeric(as.character(imputed_MCS$FPALDR00))

### SUM ALCOHOL SCORES ACROSS SWEEPS 1,2,3,4,5,6
# If parents scored frequent alcohol use for at least 1 sweep, code as 1 for frequent parental alcohol use
imputed_MCS$Parental_alcohol <- 0
imputed_MCS$Parental_alcohol[imputed_MCS$APALDR00==1 | imputed_MCS$BPALDR00==1 | 
                              imputed_MCS$CPALDR00==1 | imputed_MCS$DPALDR00==1 |
                              imputed_MCS$EPALDR00==1 | imputed_MCS$FPALDR00==1] <- 1

table(imputed_MCS$Parental_alcohol, useNA="always")
prop.table(table(imputed_MCS$Parental_alcohol, useNA="always"))*100

## ================== Frequent parental drug use (Sweeps 2, 3, 6; ages 3, 5, 14) ==================
### (Sweep 2, age 3)
imputed_MCS$BPDRUG00 <- recode(imputed_MCS$BPDRUG00, "'Occasionally'=1; 'Regularly'=1; 'Never'=0")
imputed_MCS$BPDRUG00 <- as.numeric(as.character(imputed_MCS$BPDRUG00))

### (Sweep 3, age 5)
imputed_MCS$CPDRUG00 <- recode(imputed_MCS$CPDRUG00, "'Occasionally'=1; 'Regularly'=1; 'Never'=0")
imputed_MCS$CPDRUG00 <- as.numeric(as.character(imputed_MCS$CPDRUG00))

### (Sweep 6, age 14)
imputed_MCS$FPDRUG00 <- recode(imputed_MCS$FPDRUG00, "'Occasionally   '=1; 'Regularly      '=1; 'Never  '=0")
imputed_MCS$FPDRUG00 <- as.numeric(as.character(imputed_MCS$FPDRUG00))

### SUM DRUG SCORES ACROSS SWEEPS 2,3,6
# If parents scored frequent drug use for at least 1 sweep, code as 1 for frequent parental drug use
imputed_MCS$Parental_drug <- 0
imputed_MCS$Parental_drug[imputed_MCS$BPDRUG00==1 | imputed_MCS$CPDRUG00==1 | 
                           imputed_MCS$FPDRUG00==1] <- 1

table(imputed_MCS$Parental_drug, useNA="always")
prop.table(table(imputed_MCS$Parental_drug, useNA="always"))*100

## ================== Single parent status (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years) ==================
### (Sweep 1, 9 months)
imputed_MCS$APFCIN00 <- recode(imputed_MCS$APFCIN00, "'Legally separated'=1; 'Married, 1st and only marriage'=0; 'Remarried, 2nd or later marriage'=0; 'Single never married'=1; 'Divorced'=1; 'Widowed'=1")
imputed_MCS$APFCIN00 <- as.numeric(as.character(imputed_MCS$APFCIN00))

### (Sweep 2, 3 years)
imputed_MCS$BPFCIN00 <- recode(imputed_MCS$BPFCIN00, "'Legally separated'=1; 'Married, 1st and only marriage'=0; 'Remarried, 2nd or later marriage'=0; 'Single never married'=1; 'Divorced'=1; 'Widowed'=1")
imputed_MCS$BPFCIN00 <- as.numeric(as.character(imputed_MCS$BPFCIN00))

### (Sweep 3, 5 years)
imputed_MCS$CPFCIN00 <- recode(imputed_MCS$CPFCIN00, "'Legally separated'=1; 'Married, 1st and only marriage'=0; 'Remarried, 2nd or later marriage'=0; 'Single never married'=1; 'Divorced'=1; 'Widowed'=1")
imputed_MCS$CPFCIN00 <- as.numeric(as.character(imputed_MCS$CPFCIN00))

### (Sweep 4, 7 years)
imputed_MCS$DPFCIN00 <- recode(imputed_MCS$DPFCIN00, "'Legally separated      '=1; 'Married, 1st and only marriage '=0; 'Remarried, 2nd or later marriage       '=0; 'Single never married   '=1; 'Divorced       '=1; 'Widowed'=1")
imputed_MCS$DPFCIN00 <- as.numeric(as.character(imputed_MCS$DPFCIN00))

### (Sweep 5, 11 years)
imputed_MCS$EPFCIN00 <- recode(imputed_MCS$EPFCIN00, "'Legally separated      '=1; 'Married, 1st and only marriage '=0; 'Remarried, 2nd or later marriage       '=0; 'Single, never married  '=1; 'Divorced       '=1; 'Widowed'=1; 'A Civil Partner (legally recognised)   '=0; 'A former Civil Partner '=1; 'A surviving Civil Partner      '=1")
imputed_MCS$EPFCIN00 <- as.numeric(as.character(imputed_MCS$EPFCIN00))

### (Sweep 6, 14 years)
imputed_MCS$FPFCIN00 <- recode(imputed_MCS$FPFCIN00, "'Legally separated      '=1; 'Married, 1st and only marriage '=0; 'Remarried, 2nd or later marriage       '=0; 'Single, never married and never in a Civil Partnership'=1; 'Divorced       '=1; 'Widowed'=1; 'A Civil Partner in a legally recognised Civil Partnership'=0; 'A former Civil Partner (where Civil Partnership legally dissolved)'=1; 'A surviving Civil Partner (where Civil Partner has died)'=1")
imputed_MCS$FPFCIN00 <- as.numeric(as.character(imputed_MCS$FPFCIN00))

### SUM SINGLE PARENT SCORES ACROSS SWEEPS 1,2,3,4,5,6
# If parents scored no partner for at least 1 sweep, code as 1 
imputed_MCS$Single_parent <- 0
imputed_MCS$Single_parent[imputed_MCS$APFCIN00==1 | imputed_MCS$BPFCIN00==1 | 
                           imputed_MCS$CPFCIN00==1 | imputed_MCS$DPFCIN00==1 |
                           imputed_MCS$EPFCIN00==1 | imputed_MCS$FPFCIN00==1] <- 1

table(imputed_MCS$Single_parent, useNA="always")
prop.table(table(imputed_MCS$Single_parent, useNA="always"))*100

## =============== Parental relationship happiness (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years) ===============
### (Sweep 1, 9 months)
imputed_MCS$APHARE00 <- recode(imputed_MCS$APHARE00, "'Very unhappy'=1; '2'=1; '3'=1; '4'=0; '5'=0; '6'=0; 'Very happy'=0")
imputed_MCS$APHARE00 <- as.numeric(as.character(imputed_MCS$APHARE00))

### (Sweep 2, 3 years)
imputed_MCS$BPHARE00 <- recode(imputed_MCS$BPHARE00, "\"Very unhappy\"=1; \"2\"=1; \"3\"=1; \"4\"=0; \"5\"=0; \"6\"=0; \"Very happy\"=0; \"Can't say\"=NA")
imputed_MCS$BPHARE00 <- as.numeric(as.character(imputed_MCS$BPHARE00))

### (Sweep 3, 5 years)
imputed_MCS$CPHARE00 <- recode(imputed_MCS$CPHARE00, "\"Very unhappy\"=1; \"2\"=1; \"3\"=1; \"4\"=0; \"5\"=0; \"6\"=0; \"Very happy\"=0; \"Can't say\"=NA")
imputed_MCS$CPHARE00 <- as.numeric(as.character(imputed_MCS$CPHARE00))

### (Sweep 4, 7 years)
imputed_MCS$DPHARE00 <- recode(imputed_MCS$DPHARE00, "\"Very unhappy\"=1; \"2\"=1; \"3\"=1; \"4\"=0; \"5\"=0; \"6\"=0; \"Very happy\"=0; \"Can't say\"=NA")
imputed_MCS$DPHARE00 <- as.numeric(as.character(imputed_MCS$DPHARE00))

### (Sweep 5, 11 years)
imputed_MCS$EPHARE00 <- recode(imputed_MCS$EPHARE00, "'Very Unhappy   '=1; '2'=1; '3'=1; '4'=0; '5'=0; '6'=0; 'Very Happy     '=0")
imputed_MCS$EPHARE00 <- as.numeric(as.character(imputed_MCS$EPHARE00))

### (Sweep 6, 14 years)
imputed_MCS$FPHARE00 <- recode(imputed_MCS$FPHARE00, "'Very unhappy   '=1; '2'=1; '3'=1; '4'=0; '5'=0; '6'=0; 'Very happy     '=0")
imputed_MCS$FPHARE00 <- as.numeric(as.character(imputed_MCS$FPHARE00))

### SUM PARENTAL RELATIONSHIP SCORES ACROSS SWEEPS 1,2,3,4,5,6
# If parents scored unhappy relationship for at least 1 sweep, code as 1 
imputed_MCS$Parental_relationship <- 0
imputed_MCS$Parental_relationship[imputed_MCS$APHARE00==1 | imputed_MCS$BPHARE00==1 | 
                                   imputed_MCS$CPHARE00==1 | imputed_MCS$DPHARE00==1 |
                                   imputed_MCS$EPHARE00==1 | imputed_MCS$FPHARE00==1] <- 1

table(imputed_MCS$Parental_relationship, useNA="always")
prop.table(table(imputed_MCS$Parental_relationship, useNA="always"))*100

## ================== Domestic violence (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years) ==================
# Factor variable with 3 levels; originally coded as "Yes" or "No" or "Don't want to answer"
# "Yes" will be recoded to 1; "No" to 0

### (Sweep 1, 9 months)
imputed_MCS$APFORC00 <- recode(imputed_MCS$APFORC00, "'Yes'=1; 'No'=0")
imputed_MCS$APFORC00 <- as.numeric(as.character(imputed_MCS$APFORC00))

### (Sweep 2, 3 years)
imputed_MCS$BPFORC00 <- recode(imputed_MCS$BPFORC00, "'Yes'=1; 'No'=0")
imputed_MCS$BPFORC00 <- as.numeric(as.character(imputed_MCS$BPFORC00))

### (Sweep 3, 5 years)
imputed_MCS$CPFORC00 <- recode(imputed_MCS$CPFORC00, "'Yes'=1; 'No'=0")
imputed_MCS$CPFORC00 <- as.numeric(as.character(imputed_MCS$CPFORC00))

### (Sweep 4, 7 years)
imputed_MCS$DPFORC00 <- recode(imputed_MCS$DPFORC00, "'Yes    '=1; 'No     '=0")
imputed_MCS$DPFORC00 <- as.numeric(as.character(imputed_MCS$DPFORC00))

### (Sweep 5, 11 years)
imputed_MCS$EPFORC00 <- recode(imputed_MCS$EPFORC00, "'Yes    '=1; 'No     '=0")
imputed_MCS$EPFORC00 <- as.numeric(as.character(imputed_MCS$EPFORC00))

### (Sweep 6, 14 years)
imputed_MCS$FPFORC00 <- recode(imputed_MCS$FPFORC00, "'Yes    '=1; 'No     '=0")
imputed_MCS$FPFORC00 <- as.numeric(as.character(imputed_MCS$FPFORC00))

### SUM DOMESTIC VIOLENCE SCORES ACROSS SWEEPS 1,2,3,4,5,6
# If parents scored domestic violence for at least 1 sweep, code as 1
imputed_MCS$Domestic_violence <- 0
imputed_MCS$Domestic_violence[imputed_MCS$APFORC00==1 | imputed_MCS$BPFORC00==1 | 
                               imputed_MCS$CPFORC00==1 | imputed_MCS$DPFORC00==1 |
                               imputed_MCS$EPFORC00==1 | imputed_MCS$FPFORC00==1] <- 1

table(imputed_MCS$Domestic_violence, useNA="always")
prop.table(table(imputed_MCS$Domestic_violence, useNA="always"))*100

## =============== Parental discipline: Straus Conflict Tactics Scale (Sweeps 2-4, ages 3, 5, 7 years) ===============
### (Sweep 2, age 3)
# Recode variables to binary
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("BPDIIG00","BPDISH00","BPDIBN00","BPDITR00","BPDITE00","BPDIBR00"), 
            function(x) recode(x, "'Never'=0; 'Rarely'=0; 'Once a month'=1; 'Once a week or more'=1; 'Daily'=1"))

# Convert variables from factor to numeric
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("BPDIIG00","BPDISH00","BPDIBN00","BPDITR00","BPDITE00","BPDIBR00"), 
            function(x) as.numeric(as.character(x)))

# Sum items together into Straus_MCS2
imputed_MCS$Straus_MCS2 <- apply(imputed_MCS[,c("BPDIIG00","BPDISH00","BPDIBN00","BPDITR00","BPDITE00","BPDIBR00")], 1, sum, na.rm=T)
table(imputed_MCS$Straus_MCS2, useNA="always")

# If scores >=5, code to 1 for harsh parental discipline, else code to 0
imputed_MCS$Straus_MCS2 <- ifelse(imputed_MCS$Straus_MCS2>=5, 1, 0)
table(imputed_MCS$Straus_MCS2, useNA="always")

### (Sweep 3, age 5)
# Recode variables to binary
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("CPDIIG00","CPDISH00","CPDIBN00","CPDITR00","CPDITE00","CPDIBR00"), 
            function(x) recode(x, "'Never'=0; 'Rarely'=0; 'Sometimes (about once a month)'=1; 'Often (about once a week or more)'=1; 'Daily'=1"))

# Convert variables from factor to numeric
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("CPDIIG00","CPDISH00","CPDIBN00","CPDITR00","CPDITE00","CPDIBR00"), 
            function(x) as.numeric(as.character(x)))

# Sum items together into Straus_MCS3
imputed_MCS$Straus_MCS3 <- apply(imputed_MCS[,c("CPDIIG00","CPDISH00","CPDIBN00","CPDITR00","CPDITE00","CPDIBR00")], 1, sum, na.rm=T)
table(imputed_MCS$Straus_MCS3, useNA="always")

# If scores >=5, code to 1 for harsh parental discipline, else code to 0
imputed_MCS$Straus_MCS3 <- ifelse(imputed_MCS$Straus_MCS3>=5, 1, 0)
table(imputed_MCS$Straus_MCS3, useNA="always")

### (Sweep 4, age 7)
# Recode variables to binary
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("DPDIIG00","DPDISH00","DPDIBN00","DPDITR00","DPDITE00","DPDIBR00"), 
            function(x) recode(x, "'Never  '=0; 'Rarely '=0; 'Sometimes (about once a month) '=1; 'Often (about once a week or more)      '=1; 'Daily  '=1"))

# Convert variables from factor to numeric
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("DPDIIG00","DPDISH00","DPDIBN00","DPDITR00","DPDITE00","DPDIBR00"), 
            function(x) as.numeric(as.character(x)))

# Sum items together into Straus_MCS4
imputed_MCS$Straus_MCS4 <- apply(imputed_MCS[,c("DPDIIG00","DPDISH00","DPDIBN00","DPDITR00","DPDITE00","DPDIBR00")], 1, sum, na.rm=T)
table(imputed_MCS$Straus_MCS4, useNA="always")

# If scores >=5, code to 1 for harsh parental discipline, else code to 0
imputed_MCS$Straus_MCS4 <- ifelse(imputed_MCS$Straus_MCS4>=5, 1, 0)
table(imputed_MCS$Straus_MCS4, useNA="always")

### SUM PARENTAL DISCIPLINE SCORES ACROSS SWEEPS 2,3,4
# If parents scored using harsh discipline for at least 1 sweep, code as 1 
imputed_MCS$Parental_discipline <- 0
imputed_MCS$Parental_discipline[imputed_MCS$Straus_MCS2==1 | imputed_MCS$Straus_MCS3==1 | 
                                 imputed_MCS$Straus_MCS4==1] <- 1

table(imputed_MCS$Parental_discipline, useNA="always")
prop.table(table(imputed_MCS$Parental_discipline, useNA="always"))*100

## =============== Parental smacking: Straus Conflict Tactics Scale (Sweeps 2-4, ages 3, 5, 7 years) ===============
### (Sweep 2, age 3)
imputed_MCS$BPDISM00 <- recode(imputed_MCS$BPDISM00, "'Never'=0; 'Rarely'=0; 'Once a month'=1; 'Once a week or more'=1; 'Daily'=1")
imputed_MCS$BPDISM00 <- as.numeric(as.character(imputed_MCS$BPDISM00))

### (Sweep 3, age 5)
imputed_MCS$CPDISM00 <- recode(imputed_MCS$CPDISM00, "'Never'=0; 'Rarely'=0; 'Sometimes (about once a month)'=1; 'Often (about once a week or more)'=1; 'Daily'=1")
imputed_MCS$CPDISM00 <- as.numeric(as.character(imputed_MCS$CPDISM00))

### (Sweep 4, age 7)
imputed_MCS$DPDISM00 <- recode(imputed_MCS$DPDISM00, "'Never  '=0; 'Rarely '=0; 'Sometimes (about once a month) '=1; 'Often (about once a week or more)      '=1; 'Daily  '=1")
imputed_MCS$DPDISM00 <- as.numeric(as.character(imputed_MCS$DPDISM00))

### SUM PARENTAL SMACKING SCORES ACROSS SWEEPS 2,3,4
# If parents scored using frequent smacking for at least 1 sweep, code as 1 
imputed_MCS$Parental_smacking <- 0
imputed_MCS$Parental_smacking[imputed_MCS$BPDISM00==1 | imputed_MCS$CPDISM00==1 | 
                               imputed_MCS$DPDISM00==1] <- 1

table(imputed_MCS$Parental_smacking, useNA="always")
prop.table(table(imputed_MCS$Parental_smacking, useNA="always"))*100

## ==================================== Home environment: HOME-SF (Sweep 2, age 3) ====================================
## Recode variables to 1/0
# Child's in-home play environment safe
imputed_MCS$BCENVI00 <- recode(imputed_MCS$BCENVI00, "'Safe   '=0; 'Not safe       '=1")
imputed_MCS$BCENVI00 <- as.numeric(as.character(imputed_MCS$BCENVI00))

# Parents provided toys during visit
imputed_MCS$BCTOYS00 <- recode(imputed_MCS$BCTOYS00, "'Provided toys  '=0; 'Did not provide toys   '=1")
imputed_MCS$BCTOYS00 <- as.numeric(as.character(imputed_MCS$BCTOYS00))

# Parent kept child in vision
imputed_MCS$BCSEEC00 <- recode(imputed_MCS$BCSEEC00, "'In range       '=0; 'Child only present during assessment   '=0; 'Not in range   '=1")
imputed_MCS$BCSEEC00 <- as.numeric(as.character(imputed_MCS$BCSEEC00))

# How at ease did parent appear
imputed_MCS$BCCOMF00 <- recode(imputed_MCS$BCCOMF00, "'Completely comfortable and at ease     '=0; 'Moderately comfortable '=0; 'Slightly ill at ease   '=0; 'Very uncomfortable     '=1")
imputed_MCS$BCCOMF00 <- as.numeric(as.character(imputed_MCS$BCCOMF00))

# Interior of home dark
imputed_MCS$BCDARK00 <- recode(imputed_MCS$BCDARK00, "'No     '=0; 'Yes    '=1")
imputed_MCS$BCDARK00 <- as.numeric(as.character(imputed_MCS$BCDARK00))

# House/flat reasonably clean
imputed_MCS$BCRCLE00 <- recode(imputed_MCS$BCRCLE00, "'Yes    '=0; 'No     '=1")
imputed_MCS$BCRCLE00 <- as.numeric(as.character(imputed_MCS$BCRCLE00))

# House/flat reasonably uncluttered
imputed_MCS$BCUNCL00 <- recode(imputed_MCS$BCUNCL00, "'Yes    '=0; 'No     '=1")
imputed_MCS$BCUNCL00 <- as.numeric(as.character(imputed_MCS$BCUNCL00))

# Mother's voice positive when speaking to child
imputed_MCS$BCSPEA00 <- recode(imputed_MCS$BCSPEA00, c("'Positive       '=0; 'Not positive   '=1"))
imputed_MCS$BCSPEA00 <- as.numeric(as.character(imputed_MCS$BCSPEA00))

# Mother converses at least twice with child
imputed_MCS$BCMCON00 <- recode(imputed_MCS$BCMCON00, c("'Converses      '=0; 'Did not converse       '=1"))
imputed_MCS$BCMCON00 <- as.numeric(as.character(imputed_MCS$BCMCON00))

# Mother answers child's questions verbally
imputed_MCS$BCANSW00 <- recode(imputed_MCS$BCANSW00, c("'Answers'=0; 'Did not answer '=1"))
imputed_MCS$BCANSW00 <- as.numeric(as.character(imputed_MCS$BCANSW00))

# Mother praises child spontaneously
imputed_MCS$BCPRAI00 <- revalue(imputed_MCS$BCPRAI00, c("Spontaneous:praise more than once      "=0, "Not Spontaneous:praise "=1))
imputed_MCS$BCPRAI00 <- as.numeric(as.character(imputed_MCS$BCPRAI00))

# Mother caresses or kisses child
imputed_MCS$BCKISS00 <- recode(imputed_MCS$BCKISS00, "'Affectionate   '=0; 'Not affectionate       '=1")
imputed_MCS$BCKISS00 <- as.numeric(as.character(imputed_MCS$BCKISS00))

# Mother introduces interviewer to child
imputed_MCS$BCINTI00 <- recode(imputed_MCS$BCINTI00, c("'Introduce      '=0; 'Did not introduce      '=1"))
imputed_MCS$BCINTI00 <- as.numeric(as.character(imputed_MCS$BCINTI00))

# “Mother scolded child more than once”
imputed_MCS$BCSCOL00 <- recode(imputed_MCS$BCSCOL00, c("'Did not scold  '=0; 'Scolded more than once '=1"))
imputed_MCS$BCSCOL00 <- as.numeric(as.character(imputed_MCS$BCSCOL00))

# “Mother used physical restraint on child”
imputed_MCS$BCPHYS00 <- recode(imputed_MCS$BCPHYS00, "'Did not use restraint  '=0; 'Restraint      '=1")
imputed_MCS$BCPHYS00 <- as.numeric(as.character(imputed_MCS$BCPHYS00))

# “Mother slapped or spanked child”
imputed_MCS$BCSLAP00 <- recode(imputed_MCS$BCSLAP00, "'Did not slap or spank  '=0; 'Slapped or spanked     '=1")
imputed_MCS$BCSLAP00 <- as.numeric(as.character(imputed_MCS$BCSLAP00))

# Sum items together into Home_environment
imputed_MCS$Home_environment <- apply(imputed_MCS[,c("BCENVI00","BCTOYS00","BCSEEC00","BCCOMF00","BCDARK00","BCRCLE00","BCUNCL00","BCSPEA00","BCMCON00","BCANSW00","BCPRAI00","BCKISS00","BCINTI00","BCSCOL00","BCPHYS00","BCSLAP00")], 1, sum, na.rm=T)
table(imputed_MCS$Home_environment, useNA="always")

describe(imputed_MCS$Home_environment) # Mean = 1.02, SD = 1.51
# A home environment score of two standard deviations above the mean would be considered unsafe

# If scores >=4.04, code to 1 for negative home environment, else code to 0
imputed_MCS$Home_environment <- ifelse(imputed_MCS$Home_environment>=4.04, 1, 0)
table(imputed_MCS$Home_environment, useNA="always")
prop.table(table(imputed_MCS$Home_environment, useNA="always"))*100

## ================== Peer victimisation (Sweeps 2-6, ages 3, 5, 7, 11, 14 years) ==================
# PARENT REPORTS OF BULLYING 
### (Sweep 2, age 3)
imputed_MCS$BPSDPB00 <- recode(imputed_MCS$BPSDPB00, "'Not true'=0; 'Somewhat true'=0; 'Certainly true'=1")
imputed_MCS$BPSDPB00 <- as.numeric(as.character(imputed_MCS$BPSDPB00))

### (Sweep 3, age 5)
imputed_MCS$CPSDPB00 <- recode(imputed_MCS$CPSDPB00, "'Not true'=0; 'Somewhat true'=0; 'Certainly true'=1")
imputed_MCS$CPSDPB00 <- as.numeric(as.character(imputed_MCS$CPSDPB00))

### (Sweep 4, age 7)
imputed_MCS$DPSDPB00 <- recode(imputed_MCS$DPSDPB00, "'Not true       '=0; 'Somewhat true  '=0; 'Certainly true '=1; 'Can t say      '=NA")
imputed_MCS$DPSDPB00 <- as.numeric(as.character(imputed_MCS$DPSDPB00))

### (Sweep 6, age 14)
imputed_MCS$FPSDPB00 <- recode(imputed_MCS$FPSDPB00, "'Not True       '=0; 'Somewhat true  '=0; 'Certainly true '=1")
imputed_MCS$FPSDPB00 <- as.numeric(as.character(imputed_MCS$FPSDPB00))

# CHILD REPORTS OF BULLYING 
### (Sweep 4, age 7)
imputed_MCS$DCSC0036 <- recode(imputed_MCS$DCSC0036, "'All of the time'=1; 'Some of the time       '=0; 'Never  '=0")
imputed_MCS$DCSC0036 <- as.numeric(as.character(imputed_MCS$DCSC0036))

### (Sweep 5, age 11)
imputed_MCS$EPSDPB00 <- recode(imputed_MCS$EPSDPB00, "'Not true       '=0; 'Somewhat true  '=0; 'Certainly true '=1")
imputed_MCS$EPSDPB00 <- as.numeric(as.character(imputed_MCS$EPSDPB00))

### (Sweep 5, age 11)
imputed_MCS$ECQ56X00 <- recode(imputed_MCS$ECQ56X00, "'Most days      '=1; 'About once a week      '=1; 'About once a month     '=0; 'Every few months       '=0; 'Less often     '=0; 'Never  '=0")
imputed_MCS$ECQ56X00 <- as.numeric(as.character(imputed_MCS$ECQ56X00))

### (Sweep 6, age 14)
imputed_MCS$FCHURT00 <- recode(imputed_MCS$FCHURT00, "'Most days      '=1; 'About once a week      '=1; 'About once a month     '=0; 'Every few months       '=0; 'Less often     '=0; 'Never  '=0")
imputed_MCS$FCHURT00 <- as.numeric(as.character(imputed_MCS$FCHURT00))

# TEACHER REPORTS OF BULLYING
### (Sweep 4, age 7)
imputed_MCS$DQ2189 <- recode(imputed_MCS$DQ2189, "'Not true       '=0; 'Somewhat true  '=0; 'Certainly true '=1")
imputed_MCS$DQ2189 <- as.numeric(as.character(imputed_MCS$DQ2189))

### (Sweep 5, age 11)
imputed_MCS$EQ5S <- recode(imputed_MCS$EQ5S, "'Not true       '=0; 'Is somewhat true       '=0; 'Very true      '=1")
imputed_MCS$EQ5S <- as.numeric(as.character(imputed_MCS$EQ5S))

### SUM PEER VICTIMISATION SCORES ACROSS SWEEPS 2-6
# If child experienced peer victimisation for at least 1 sweep, code as 1 
imputed_MCS$Peer_victimisation <- 0
imputed_MCS$Peer_victimisation[imputed_MCS$BPSDPB00==1 | imputed_MCS$CPSDPB00==1 |
                                imputed_MCS$DPSDPB00==1 | imputed_MCS$FPSDPB00==1 | 
                                imputed_MCS$DCSC0036==1 | imputed_MCS$EPSDPB00==1 | 
                                imputed_MCS$ECQ56X00==1 | imputed_MCS$FCHURT00==1 |
                                imputed_MCS$DQ2189==1 | imputed_MCS$EQ5S==1] <- 1

table(imputed_MCS$Peer_victimisation, useNA="always")
prop.table(table(imputed_MCS$Peer_victimisation, useNA="always"))*100

## ================== Child victimisation (Sweep 6, age 14) ==================
# Recode variables to binary
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("FCVICG00","FCVICA00","FCVICC00","FCVICE00","FCVICF0A"), 
            function(x) recode(x, "'Yes    '=1; 'No     '=0"))

# Convert variables from factor to numeric
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("FCVICG00","FCVICA00","FCVICC00","FCVICE00","FCVICF0A"), 
            function(x) as.numeric(as.character(x)))

# Verbal victimisation: CM insulted/threatened/shouted at
imputed_MCS$Verbal_victimisation <- imputed_MCS$FCVICG00

table(imputed_MCS$Verbal_victimisation, useNA="always")
prop.table(table(imputed_MCS$Verbal_victimisation, useNA="always"))*100

# Physical victimisation: been physically violent towards CM; or hit or used weapon against CM
# If scored at least 1/2 for physically violent and/or weapon, code as 1
imputed_MCS$Physical_victimisation <- 0
imputed_MCS$Physical_victimisation[imputed_MCS$FCVICA00==1 | imputed_MCS$FCVICC00==1] <- 1

table(imputed_MCS$Physical_victimisation, useNA="always")
prop.table(table(imputed_MCS$Physical_victimisation, useNA="always"))*100

# Theft victimisation: stolen something from CM
imputed_MCS$Theft_victimisation <- imputed_MCS$FCVICE00

table(imputed_MCS$Theft_victimisation, useNA="always")
prop.table(table(imputed_MCS$Theft_victimisation, useNA="always"))*100

# Sexual victimisation: sexually assaulted CM
imputed_MCS$Sexual_victimisation <- imputed_MCS$FCVICF0A

table(imputed_MCS$Sexual_victimisation, useNA="always")
prop.table(table(imputed_MCS$Sexual_victimisation, useNA="always"))*100

## ============= Household income OECD Income Weighted Quintiles (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years) =============
# Recode variables to binary 
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("AOECDUK0","BOECDUK0","COECDUK0","DOECDUK0"), 
            function(x) recode(x, "'Lowest quintile'=1; 'Second quintile'=0; 'Third quintile '=0; 'Fourth quintile'=0; 'Highest quintile       '=0"))

imputed_MCS$EOECDUK0 <- recode(imputed_MCS$EOECDUK0, "'Bottom '=1; 'Second '=0; 'Third  '=0; 'Fourth '=0; 'Top    '=0")

imputed_MCS$FOECDUK0 <- recode(imputed_MCS$FOECDUK0, "'Lower quantile '=1; 'Second quantile'=0; 'Third quantile '=0; 'Fourth quantile'=0; 'Highest quantile       '=0")

# Convert variables from factor to numeric
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("AOECDUK0","BOECDUK0","COECDUK0","DOECDUK0","EOECDUK0","FOECDUK0"), 
            function(x) as.numeric(as.character(x)))

### SUM HOUSEHOLD INCOME SCORES ACROSS SWEEPS 1,2,3,4,5,6
# If income scored as lowest quintile for at least 1 sweep, code as 1 
imputed_MCS$Household_income <- 0
imputed_MCS$Household_income[imputed_MCS$AOECDUK0==1 | imputed_MCS$BOECDUK0==1 | 
                              imputed_MCS$COECDUK0==1 | imputed_MCS$DOECDUK0==1 |
                              imputed_MCS$EOECDUK0==1 | imputed_MCS$FOECDUK0==1] <- 1

table(imputed_MCS$Household_income, useNA="always")
prop.table(table(imputed_MCS$Household_income, useNA="always"))*100

## ================== Neighbourhood deprivation (Sweep 1, 9 months) ==================
# Recode variables to binary
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("APHOSA00","APAREA00"), 
            function(x) recode(x, "'Very satisfied'=0; 'Fairly satisfied'=0; 'Neither satisfied nor dissatisfied'=0; 'Fairly dissatisfied'=1; 'Very dissatisfied'=1"))

imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("APARNN00","APARRU00","APARVD00","APARRC00","APTRAN00","APARPG00"), 
            function(x) recode(x, "'Very common'=1; 'Fairly common'=1; 'Not very common'=0; 'Not at all common'=0"))

# Food shops/supermarkets in easy access needs to be reverse coded because it is phrased positively 
imputed_MCS$APSHOP00 <- recode(imputed_MCS$APSHOP00, "'Very common'=0; 'Fairly common'=0; 'Not very common'=1; 'Not at all common'=1")

imputed_MCS$APPLSA00 <- recode(imputed_MCS$APPLSA00, "'Yes'=0; 'No'=1")

# Convert variables from factor to numeric
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("APHOSA00", "APAREA00", "APARNN00", "APARRU00", "APARVD00", "APARRC00", "APTRAN00", "APSHOP00", "APARPG00", "APPLSA00"), 
            function(x) as.numeric(as.character(x)))

# Sum items together into Neighbourhood_deprivation
imputed_MCS$Neighbourhood_deprivation <- apply(imputed_MCS[,c("APHOSA00", "APAREA00", "APARNN00", "APARRU00", "APARVD00", "APARRC00", "APTRAN00", "APSHOP00", "APARPG00", "APPLSA00")], 1, sum, na.rm=T)
table(imputed_MCS$Neighbourhood_deprivation, useNA="always")

# If scores >=5, code to 1 for neighbourhood deprivation, else code to 0
imputed_MCS$Neighbourhood_deprivation <- ifelse(imputed_MCS$Neighbourhood_deprivation>=5, 1, 0)

table(imputed_MCS$Neighbourhood_deprivation, useNA="always")
prop.table(table(imputed_MCS$Neighbourhood_deprivation, useNA="always"))*100

## ================== Safety of home area (Sweeps 2,3,5, ages 3,5,11) ==================
### (Sweep 2, age 3)
imputed_MCS$BPARGD00 <- recode(imputed_MCS$BPARGD00, c("'Excellent'=0; 'Good'=0; 'Average'=0; 'Poor'=1; 'Very poor'=1"))
imputed_MCS$BPARGD00 <- as.numeric(as.character(imputed_MCS$BPARGD00))

### (Sweep 3, age 5)
imputed_MCS$CPARGD00 <- recode(imputed_MCS$CPARGD00, "'Excellent'=0; 'Good'=0; 'Average'=0; 'Poor'=1; 'Very poor'=1")
imputed_MCS$CPARGD00 <- as.numeric(as.character(imputed_MCS$CPARGD00))

### (Sweep 2, age 3)
imputed_MCS$BPARAR00 <- recode(imputed_MCS$BPARAR00, "'Very safe'=0; 'Fairly safe'=0; 'Neither safe nor unsafe'=0; 'Fairly unsafe'=1; 'Very unsafe'=1")
imputed_MCS$BPARAR00 <- as.numeric(as.character(imputed_MCS$BPARAR00))

### (Sweep 3, age 5)
imputed_MCS$CPARAR00 <- recode(imputed_MCS$CPARAR00, "'Very safe'=0; 'Fairly safe'=0; 'Neither safe nor unsafe'=0; 'Fairly unsafe'=1; 'Very unsafe'=1")
imputed_MCS$CPARAR00 <- as.numeric(as.character(imputed_MCS$CPARAR00))

### (Sweep 5, age 11)
imputed_MCS$ECQ23X00 <- recode(imputed_MCS$ECQ23X00, "'Very safe      '=0; 'Safe   '=0; 'Not very safe  '=1; 'Not at all safe'=1")
imputed_MCS$ECQ23X00 <- as.numeric(as.character(imputed_MCS$ECQ23X00))

### SUM HOME AREA SAFETY SCORES ACROSS SWEEPS 2,3,5
# If scored as unsafe home area for at least 1 sweep, code as 1 
imputed_MCS$Area_safety <- 0
imputed_MCS$Area_safety[imputed_MCS$BPARGD00==1 | imputed_MCS$CPARGD00==1 | 
                         imputed_MCS$BPARAR00==1 | imputed_MCS$CPARAR00==1 |
                         imputed_MCS$ECQ23X00==1] <- 1

table(imputed_MCS$Area_safety, useNA="always")
prop.table(table(imputed_MCS$Area_safety, useNA="always"))*100

## ================== Cognitive stimulation (Sweeps 2-5, ages 3, 5, 7, 11 years) ==================
### (Sweep 2, age 3)
# Recode variables to binary
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("BPOFRE00","BPREOF00"), 
            function(x) recode(x, "'Every day'=0; 'Several times a week'=0; 'Once or twice a week'=0; 'Once or twice a month'=0; 'Less often'=0; 'Not at all'=1"))

imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("BPREEL00","BPTOLI00","BPSDPA00","BPALPH00","BPNUMB00","BPSONG00","BPDRAW00","BPEATW00","BPYOCH00"), 
            function(x) recode(x, "'Yes'=0; 'No'=1"))

imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("BPOFCO00","BPOFSO00","BPPAMA00"), 
            function(x) recode(x, "'Occasionally or less than once a week'=1; '1 - 2 days per week'=0; '3 times a week'=0; '4 times a week'=0; '5 times a week'=0; '6 times a week'=0; '7 times a week constantly'=0"))

imputed_MCS$BPOFAB00 <- recode(imputed_MCS$BPOFAB00, "'Occasionally or less than once a week'=1; '1-2 days per week'=0; '3 times a week'=0; '4 times a week'=0; '5 times a week'=0; '6 times a week'=0; '7 times a week/constantly'=0")

imputed_MCS$BPBIRT00 <- recode(imputed_MCS$BPBIRT00, "'No'=1; 'Yes'=0; 'Not had yet'=0")

# Convert variables from factor to numeric
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("BPOFRE00", "BPREEL00", "BPREOF00", "BPTOLI00", "BPSDPA00", "BPALPH00", "BPOFAB00", "BPNUMB00", "BPOFCO00", "BPSONG00", "BPOFSO00", "BPDRAW00", "BPPAMA00", "BPEATW00", "BPBIRT00", "BPYOCH00"), 
            function(x) as.numeric(as.character(x)))

# Sum items together into Cog_MCS2
imputed_MCS$Cog_MCS2 <- apply(imputed_MCS[,c("BPOFRE00", "BPREEL00", "BPREOF00", "BPTOLI00", "BPSDPA00", "BPALPH00", "BPOFAB00", "BPNUMB00", "BPOFCO00", "BPSONG00", "BPOFSO00", "BPDRAW00", "BPPAMA00", "BPEATW00", "BPBIRT00", "BPYOCH00")], 1, sum, na.rm=T)
table(imputed_MCS$Cog_MCS2, useNA="always")

# If scores >=8, code to 1 for poor cognitive stimulation, else code to 0
imputed_MCS$Cog_MCS2 <- ifelse(imputed_MCS$Cog_MCS2>=8, 1, 0)
table(imputed_MCS$Cog_MCS2, useNA="always")

### (Sweep 3, age 5)
# Recode variables to binary
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("CPREOF00","CPSITS00","CPPLMU00","CPPAMA00","CPACTI00","CPGAME00","CPWALK00"), 
            function(x) recode(x, "'Every day'=0; 'Several times a week'=0; 'Once or twice a week'=0; 'Once or twice a month'=0; 'Less often'=0; 'Not at all'=1"))

# Convert variables from factor to numeric
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("CPREOF00","CPSITS00","CPPLMU00","CPPAMA00","CPACTI00","CPGAME00","CPWALK00"), 
            function(x) as.numeric(as.character(x)))

# Sum items together into Cog_MCS3
imputed_MCS$Cog_MCS3 <- apply(imputed_MCS[,c("CPREOF00","CPSITS00","CPPLMU00","CPPAMA00","CPACTI00","CPGAME00","CPWALK00")], 1, sum, na.rm=T)
table(imputed_MCS$Cog_MCS3, useNA="always")

# If scores >=3.5, code to 1 for poor cognitive stimulation, else code to 0
imputed_MCS$Cog_MCS3 <- ifelse(imputed_MCS$Cog_MCS3>=3.5, 1, 0)
table(imputed_MCS$Cog_MCS3, useNA="always")

### (Sweep 4, age 7)
# Recode variables to binary
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("DPREOF00","DPSITS00","DPPLMU00","DPPAMA00","DPACTI00","DPGAME00","DPWALK00"), 
            function(x) recode(x, "'Every day or almost every day  '=0; 'Several times a week   '=0; 'Once or twice a week   '=0; 'Once or twice a month  '=0; 'Less often than once a month   '=0; 'Not at all     '=1"))

# Convert variables from factor to numeric
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("DPREOF00","DPSITS00","DPPLMU00","DPPAMA00","DPACTI00","DPGAME00","DPWALK00"), 
            function(x) as.numeric(as.character(x)))

# Sum items together into Cog_MCS4
imputed_MCS$Cog_MCS4 <- apply(imputed_MCS[,c("DPREOF00","DPSITS00","DPPLMU00","DPPAMA00","DPACTI00","DPGAME00","DPWALK00")], 1, sum, na.rm=T)
table(imputed_MCS$Cog_MCS4, useNA="always")

# If scores >=3.5, code to 1 for poor cognitive stimulation, else code to 0
imputed_MCS$Cog_MCS4 <- ifelse(imputed_MCS$Cog_MCS4>=3.5, 1, 0)
table(imputed_MCS$Cog_MCS4, useNA="always")

### (Sweep 5, age 11)
# Recode variables to binary
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("EPACTI00","EPGAME00","EPTAIM00"), 
            function(x) recode(x, "\"Every day or almost every day  \"=0; \"Several times a week   \"=0; \"Once or twice a week   \"=0; \"Once or twice a month  \"=0; \"Less often than once a month   \"=0; \"Not at all     \"=1; \"Refused\"=NA; \"Don't know     \"=NA"))

# Convert variables from factor to numeric
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("EPACTI00","EPGAME00","EPTAIM00"), 
            function(x) as.numeric(as.character(x)))

# Sum items together into Cog_MCS5
imputed_MCS$Cog_MCS5 <- apply(imputed_MCS[,c("EPACTI00","EPGAME00","EPTAIM00")], 1, sum, na.rm=T)
table(imputed_MCS$Cog_MCS5, useNA="always")

# If scores >=1.5, code to 1 for poor cognitive stimulation, else code to 0
imputed_MCS$Cog_MCS5 <- ifelse(imputed_MCS$Cog_MCS5>=1.5, 1, 0)
table(imputed_MCS$Cog_MCS5, useNA="always")

### SUM COGNITIVE STIMULATION SCORES ACROSS SWEEPS 2,3,4,5

# If child had poor cognitive stimulation for at least 1 sweep, code as 1 
imputed_MCS$Cognitive_stimulation <- 0
imputed_MCS$Cognitive_stimulation[imputed_MCS$Cog_MCS2==1 | imputed_MCS$Cog_MCS3==1 | 
                                   imputed_MCS$Cog_MCS4==1 | imputed_MCS$Cog_MCS5==1] <- 1

table(imputed_MCS$Cognitive_stimulation, useNA="always")
prop.table(table(imputed_MCS$Cognitive_stimulation, useNA="always"))*100

## ================== Internalising and Externalising Symptoms (Sweep 7, age 17) ==================
# Recode variables to 3-point scale
# Emotional problems
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("GCSDQC00","GCSDQH00","GCSDQM00","GCSDQP00","GCSDQX00"), 
            function(x) recode(x, "'Certainly true'=2; 'Somewhat true'=1; 'Not true'=0"))

# Peer problems
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("GCSDQF00","GCSDQS00","GCSDQW00"), 
            function(x) recode(x, "'Certainly true'=2; 'Somewhat true'=1; 'Not true'=0"))
# Peer problems (reverse coded because they were phrased positively)
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("GCSDQK00","GCSDQN00"), 
            function(x) recode(x, "'Certainly true'=0; 'Somewhat true'=1; 'Not true'=2"))

# Conduct problems
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("GCSDQE00","GCSDQL00","GCSDQR00","GCSDQV00"), 
            function(x) recode(x, "'Certainly true'=2; 'Somewhat true'=1; 'Not true'=0"))
# Conduct problems (reverse coded because it was phrased positively)
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("GCSDQG00"), 
            function(x) recode(x, "'Certainly true'=0; 'Somewhat true'=1; 'Not true'=2"))

# Hyperactivity/inattention
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("GCSDQB00","GCSDQJ00","GCSDQO00"), 
            function(x) recode(x, "'Certainly true'=2; 'Somewhat true'=1; 'Not true'=0"))
# Hyperactivity/inattention (reverse coded because they were phrased positively)
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("GCSDQU00","GCSDQY00"), 
            function(x) recode(x, "'Certainly true'=0; 'Somewhat true'=1; 'Not true'=2"))

# Convert variables from factor to numeric
imputed_MCS <- imputed_MCS %>% 
  mutate_at(c("GCSDQC00","GCSDQH00","GCSDQM00","GCSDQP00","GCSDQX00",
              "GCSDQF00","GCSDQK00","GCSDQN00","GCSDQS00","GCSDQW00",
              "GCSDQE00","GCSDQG00","GCSDQL00","GCSDQR00","GCSDQV00",
              "GCSDQB00","GCSDQJ00","GCSDQO00","GCSDQU00","GCSDQY00"), 
            function(x) as.numeric(as.character(x)))

# Sum items together into Internalising_symptoms
imputed_MCS$Internalising_symptoms <- apply(imputed_MCS[,c("GCSDQC00","GCSDQH00","GCSDQM00","GCSDQP00","GCSDQX00","GCSDQF00","GCSDQK00","GCSDQN00","GCSDQS00","GCSDQW00")], 1, sum, na.rm=T)

describe(imputed_MCS$Internalising_symptoms)
table(imputed_MCS$Internalising_symptoms, useNA="always")

# Sum items together into Externalising_symptoms
imputed_MCS$Externalising_symptoms <- apply(imputed_MCS[,c("GCSDQE00","GCSDQG00","GCSDQL00","GCSDQR00","GCSDQV00","GCSDQB00","GCSDQJ00","GCSDQO00","GCSDQU00","GCSDQY00")], 1, sum, na.rm=T)

describe(imputed_MCS$Externalising_symptoms)
table(imputed_MCS$Externalising_symptoms, useNA="always")

## ================== DESCRIPTIVE STATISTICS OF DERIVED VARIABLES ==================
# Derived variables were all coded to binary such that 1 = risk; 0 = no risk
# Check n and percentages for each ACE

# Poor parental mental health (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years)
table(imputed_MCS$Parental_mental_health, useNA="always")
prop.table(table(imputed_MCS$Parental_mental_health, useNA="always"))*100

# Frequent parental alcohol use (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years)
table(imputed_MCS$Parental_alcohol, useNA="always") 
prop.table(table(imputed_MCS$Parental_alcohol, useNA="always"))*100

# Parental drug use (Sweeps 2,3,6; ages 3, 5, 14)
table(imputed_MCS$Parental_drug, useNA="always") 
prop.table(table(imputed_MCS$Parental_drug, useNA="always"))*100

# Single parent (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years)
table(imputed_MCS$Single_parent, useNA="always") 
prop.table(table(imputed_MCS$Single_parent, useNA="always"))*100

# Unhappy parental relationship (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years)
table(imputed_MCS$Parental_relationship, useNA="always")
prop.table(table(imputed_MCS$Parental_relationship, useNA="always"))*100

# Domestic violence (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years)
table(imputed_MCS$Domestic_violence, useNA="always")
prop.table(table(imputed_MCS$Domestic_violence, useNA="always"))*100

# Harsh parental discipline (Sweeps 2-4, ages 3, 5, 7 years)
table(imputed_MCS$Parental_discipline, useNA="always") 
prop.table(table(imputed_MCS$Parental_discipline, useNA="always"))*100

# Parental smacking (Sweeps 2-4, ages 3, 5, 7 years)
table(imputed_MCS$Parental_smacking, useNA="always") 
prop.table(table(imputed_MCS$Parental_smacking, useNA="always"))*100

# Negative home environment (Sweep 2, age 3)
table(imputed_MCS$Home_environment, useNA="always")
prop.table(table(imputed_MCS$Home_environment, useNA="always"))*100

# Peer victimisation (Sweeps 2-6, ages 3, 5, 7, 11, 14 years)
table(imputed_MCS$Peer_victimisation, useNA="always")
prop.table(table(imputed_MCS$Peer_victimisation, useNA="always"))*100

# Verbal victimisation (Sweep 6, age 14)
table(imputed_MCS$Verbal_victimisation, useNA="always")
prop.table(table(imputed_MCS$Verbal_victimisation, useNA="always"))*100

# Physical victimisation (Sweep 6, age 14)
table(imputed_MCS$Physical_victimisation, useNA="always") 
prop.table(table(imputed_MCS$Physical_victimisation, useNA="always"))*100

# Theft victimisation (Sweep 6, age 14)
table(imputed_MCS$Theft_victimisation, useNA="always") 
prop.table(table(imputed_MCS$Theft_victimisation, useNA="always"))*100

# Sexual victimisation (Sweep 6, age 14)
table(imputed_MCS$Sexual_victimisation, useNA="always") 
prop.table(table(imputed_MCS$Sexual_victimisation, useNA="always"))*100

# Low household income (Sweeps 1-6, ages 9 months, 3, 5, 7, 11, 14 years)
table(imputed_MCS$Household_income, useNA="always")
prop.table(table(imputed_MCS$Household_income, useNA="always"))*100

# Neighbourhood deprivation (Sweep 1, 9 months)
table(imputed_MCS$Neighbourhood_deprivation, useNA="always")
prop.table(table(imputed_MCS$Neighbourhood_deprivation, useNA="always"))*100

# Unsafe home area (Sweeps 2,3,5, ages 3,5,11)
table(imputed_MCS$Area_safety, useNA="always")
prop.table(table(imputed_MCS$Area_safety, useNA="always"))*100

# Low cognitive stimulation (Sweeps 2-4, ages 3, 5, 7 years)
table(imputed_MCS$Cognitive_stimulation, useNA="always")
prop.table(table(imputed_MCS$Cognitive_stimulation, useNA="always"))*100

# Internalising symptoms (Sweep 7, age 14 years)
describe(imputed_MCS$Internalising_symptoms)
table(imputed_MCS$Internalising_symptoms, useNA="always")

# Externalising symptoms (Sweep 7, age 14 years)
describe(imputed_MCS$Externalising_symptoms)
table(imputed_MCS$Externalising_symptoms, useNA="always")
```

# 6. Imputed data analyses

We will rerun the EFA and regressions on the imputed data, to check if
results are consistent with the original complete data.

``` r
# Repeat EFA and regression analyses code on imputed dataset

# Create new dataframe with all derived binary ACE variables
MCS_ACEs <- select(imputed_MCS,
                   Parental_mental_health,
                   Parental_alcohol,
                   Parental_drug,
                   Single_parent,
                   Parental_relationship,
                   Domestic_violence,
                   Parental_discipline,
                   Parental_smacking,
                   Home_environment,
                   Peer_victimisation,
                   Verbal_victimisation,
                   Physical_victimisation,
                   Theft_victimisation,
                   Sexual_victimisation,
                   Household_income,
                   Neighbourhood_deprivation,
                   Area_safety,
                   Cognitive_stimulation)
dim(MCS_ACEs)
str(MCS_ACEs)
colnames(MCS_ACEs)

# Check whether IDs are duplicated (e.g. multiple children per family) - no
n_occur <- data.frame(table(MCS_ACEs$MCSID))
n_occur[n_occur$Freq > 1,]

#------------------------------------ Exploratory Factor Analysis (EFA) ------------------------------------#
library(corrplot)
library(EFA.dimensions)

# Compute tetrachoric correlations
tet_corr <- tetrachoric(MCS_ACEs)
tet_corr

# Tetrachoric correlation matrix
tet_corr_matrix<- tet_corr$rho
tet_corr_matrix

# Visualise tetrachoric correlation matrix
corrplot(tet_corr_matrix, order="original", method="circle")

# Make correlations larger/bolder
tet_corr_matrix=tanh(5*atanh(tet_corr_matrix))
corrplot(tet_corr_matrix, order="original", method="circle")

# Cluster analysis technique to put items which are more similar closer to one another
corrplot(tet_corr_matrix, order="hclust", method="circle")

# Show lower diagonal only and change font colour to black
corrplot(tet_corr_matrix, type="lower", order="hclust", method="circle", tl.col="black")

# Check factorability of correlation matrix (Bartlett's test for sphericity, Kaiser-Meyer-Olkin test for sampling adequacy etc.)
factorability <- FACTORABILITY(MCS_ACEs, corkind="polychoric", Ncases=nrow(MCS_ACEs))
factorability

###### Parallel analysis, 1000 Monte-Carlo simulations, 99% confidence intervals

# Weighted least squares parallel analysis
parallel_analysis_wls <- fa.parallel(MCS_ACEs, fm="wls", fa="both", n.iter=1000, cor="tet", error.bars=TRUE, sim=FALSE, quant=.99, main="Parallel Analysis Scree Plot" )

parallel_analysis_wls$fa.values # eigenvalues
parallel_analysis_wls
# WLS parallel analysis recommends that the number of factors = 5 and the number of components = 5

###### Exploratory factor analysis (using item response theory (IRT) code)

## IRT one-factor model
irt_1factor <- irt.fa(tet_corr, nfactors=1, plot=FALSE, n.obs=nrow(MCS_ACEs), rotate="promax", fm="wls")
irt_1factor$fa # loadings
fa.diagram(irt_1factor, cut=0.2, rsize=.30) # factor diagram

## IRT two-factor model
irt_2factor <- irt.fa(tet_corr, nfactors=2, plot=FALSE, n.obs=nrow(MCS_ACEs), rotate="promax", fm="wls")
irt_2factor$fa # loadings
fa.diagram(irt_2factor, cut=0.2, rsize=.30) # factor diagram

## IRT three-factor model
irt_3factor <- irt.fa(tet_corr, nfactors=3, plot=FALSE, n.obs=nrow(MCS_ACEs), rotate="promax", fm="wls")
irt_3factor$fa # loadings
fa.diagram(irt_3factor, cut=0.2, rsize=.30) # factor diagram

# IRT four-factor model
irt_4factor <- irt.fa(tet_corr, nfactors=4, plot=FALSE, n.obs=nrow(MCS_ACEs), rotate="oblimin", fm="wls") # promax doesn't work
irt_4factor$fa # loadings
colnames(irt_4factor$fa$loadings) <- c("Victimisation", "Deprivation", "Parental Threat", "Parental Discipline")
fa.diagram(irt_4factor, cut=0.2, rsize=.30) # factor diagram

# IRT five-factor model
irt_5factor <- irt.fa(tet_corr, nfactors=5, plot=FALSE, n.obs=nrow(MCS_ACEs), rotate="oblimin", fm="wls") # promax doesn't work
irt_5factor$fa # loadings
fa.diagram(irt_5factor, cut=0.2, rsize=.30) # factor diagram

#------------------------------------ Plot factor analysis results ------------------------------------#
# Following guidance from Dr Dan Mirman: https://rpubs.com/danmirman/plotting_factor_analysis

# Extract loadings from 4-factor model
class(irt_4factor$fa$loadings) # psych loadings class 
factor_loadings <- unclass(irt_4factor$fa$loadings) # convert to matrix
factor_loadings <- as.data.frame(factor_loadings) # convert to dataframe
factor_loadings <- rownames_to_column(factor_loadings, "ACEs") # name first column

# Rename ACEs
factor_loadings$ACEs <- recode(factor_loadings$ACEs, "'Household_income'='Low household income'; 'Cognitive_stimulation'='Low cognitive stimulation'; 'Neighbourhood_deprivation'='Neighbourhood deprivation'; 'Home_environment'='Negative home environment'; 'Area_safety'='Unsafe home area'; 'Single_parent'='Single parent'; 'Parental_mental_health'='Poor parental mental health'; 'Physical_victimisation'='Physical victimisation'; 'Verbal_victimisation'='Verbal victimisation'; 'Theft_victimisation'='Theft victimisation'; 'Sexual_victimisation'='Sexual victimisation'; 'Peer_victimisation'='Peer victimisation'; 'Parental_drug'='Parental drug use'; 'Domestic_violence'='Domestic violence'; 'Parental_relationship'='Unhappy parental relationship'; 'Parental_alcohol'='Frequent parental alcohol use'; 'Parental_smacking'='Parental smacking'; 'Parental_discipline'='Harsh parental discipline'")

# Manually order ACEs by the 4 factor groups
factor_loadings$ACEs <- factor(factor_loadings$ACEs, levels = c("Harsh parental discipline", "Parental smacking", "Peer victimisation", "Sexual victimisation", "Theft victimisation", "Verbal victimisation", "Physical victimisation", "Poor parental mental health", "Single parent", "Unsafe home area", "Negative home environment", "Neighbourhood deprivation", "Low cognitive stimulation", "Low household income", "Frequent parental alcohol use", "Unhappy parental relationship", "Domestic violence", "Parental drug use"))
factor_loadings <- factor_loadings[order(factor_loadings$ACEs), ]
factor_loadings

# Melt data into long form for plotting
library(reshape2)
loadings.m <- melt(factor_loadings, id="ACEs", 
                   measure=c("Parental Threat", "Deprivation", "Victimisation", "Parental Discipline"), 
                   variable.name="Factor", value.name="Loading")
loadings.m

# For each ACE, plot the loading as length and fill color of a bar
# note that the length will be the absolute value of the loading but the 
# fill color will be the signed value, more on this below
loadings_plot <- ggplot(loadings.m, aes(ACEs, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Factor, nrow=1) + # place the factors in separate facets
  geom_bar(stat="identity") + # make the bars
  coord_flip() + # flip the axes so the test names can be horizontal  
  # define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "skyblue", mid = "white", low = "red", 
                       midpoint=0, guide="none") + 
  ylab("Loading Strength") + # improve y-axis label
  theme_bw(base_size=10) # use a black and white theme with set font size

loadings_plot

# Extract correlations from correlation matrix
class(tet_corr_matrix) # matrix array class
tet_corr_df <- as.data.frame(tet_corr_matrix) # convert to dataframe
tet_corr_df <- rownames_to_column(tet_corr_df, "ACEs") # name first column
head(tet_corr_df)

# Melt data into long form for plotting
corrs.m <- melt(tet_corr_df, id="ACEs", variable.name="ACEs2", value.name="Correlation")
head(corrs.m)

# Rename ACEs
corrs.m$ACEs <- recode(corrs.m$ACEs, "'Household_income'='Low household income'; 'Cognitive_stimulation'='Low cognitive stimulation'; 'Neighbourhood_deprivation'='Neighbourhood deprivation'; 'Home_environment'='Negative home environment'; 'Area_safety'='Unsafe home area'; 'Single_parent'='Single parent'; 'Parental_mental_health'='Poor parental mental health'; 'Physical_victimisation'='Physical victimisation'; 'Verbal_victimisation'='Verbal victimisation'; 'Theft_victimisation'='Theft victimisation'; 'Sexual_victimisation'='Sexual victimisation'; 'Peer_victimisation'='Peer victimisation'; 'Parental_drug'='Parental drug use'; 'Domestic_violence'='Domestic violence'; 'Parental_relationship'='Unhappy parental relationship'; 'Parental_alcohol'='Frequent parental alcohol use'; 'Parental_smacking'='Parental smacking'; 'Parental_discipline'='Harsh parental discipline'")

corrs.m$ACEs2 <- recode(corrs.m$ACEs2, "'Household_income'='Low household income'; 'Cognitive_stimulation'='Low cognitive stimulation'; 'Neighbourhood_deprivation'='Neighbourhood deprivation'; 'Home_environment'='Negative home environment'; 'Area_safety'='Unsafe home area'; 'Single_parent'='Single parent'; 'Parental_mental_health'='Poor parental mental health'; 'Physical_victimisation'='Physical victimisation'; 'Verbal_victimisation'='Verbal victimisation'; 'Theft_victimisation'='Theft victimisation'; 'Sexual_victimisation'='Sexual victimisation'; 'Peer_victimisation'='Peer victimisation'; 'Parental_drug'='Parental drug use'; 'Domestic_violence'='Domestic violence'; 'Parental_relationship'='Unhappy parental relationship'; 'Parental_alcohol'='Frequent parental alcohol use'; 'Parental_smacking'='Parental smacking'; 'Parental_discipline'='Harsh parental discipline'")

# Reorder ACEs in correlation matrix
corrs.m$ACEs <- factor(corrs.m$ACEs, levels = c("Harsh parental discipline", "Parental smacking", "Peer victimisation", "Sexual victimisation", "Theft victimisation", "Verbal victimisation", "Physical victimisation", "Poor parental mental health", "Single parent", "Unsafe home area", "Negative home environment", "Neighbourhood deprivation", "Low cognitive stimulation", "Low household income", "Frequent parental alcohol use", "Unhappy parental relationship", "Domestic violence", "Parental drug use"))
corrs.m <- corrs.m[order(corrs.m$ACEs), ]

corrs.m$ACEs2 <- factor(corrs.m$ACEs2, levels = c("Parental drug use", "Domestic violence",  "Unhappy parental relationship", "Frequent parental alcohol use", "Low household income", "Low cognitive stimulation", "Neighbourhood deprivation", "Negative home environment", "Unsafe home area", "Single parent", "Poor parental mental health", "Physical victimisation", "Verbal victimisation", "Theft victimisation", "Sexual victimisation", "Peer victimisation", "Parental smacking", "Harsh parental discipline"))
corrs.m <- corrs.m[order(corrs.m$ACEs2), ]

corrs.m

# Plot correlation matrix
library(grid) 
# for adjusting plot margins
# place the tests on the x- and y-axes, 
# fill the elements with the strength of the correlation
corr_matrix <- ggplot(corrs.m, aes(ACEs2, ACEs, fill=abs(Correlation))) + 
  geom_tile() + # rectangles for each correlation
  # add actual correlation value in the rectangle
  geom_text(aes(label = round(Correlation, 2)), size=2.5) + 
  theme_bw(base_size=10) + # black and white theme with set font size
  # rotate x-axis labels so they don't overlap, 
  # get rid of unnecessary axis titles
  # adjust plot margins
  theme(axis.text.x = element_text(angle = 90), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        plot.margin = unit(c(3, 1, 0, 0), "mm")) +
  # set correlation fill gradient
  scale_fill_gradient(low="white", high="skyblue") + 
  guides(fill=F) # omit unnecessary gradient legend

corr_matrix

# Store the correlation matrix plot object for later
p1 <- last_plot() 

# Load colour brewer palettes and choose colours
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)

display.brewer.pal(n = 12, name = "Set2")
brewer.pal(n = 12, name = "Set2") # find out hexadecimal codes
display.brewer.pal(n = 12, name = "Paired")
brewer.pal(n = 12, name = "Paired") # find out hexadecimal codes

# Plot stacked bar graph
p2 <- ggplot(loadings.m, aes(ACEs, abs(Loading), fill=Factor)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ylab("Loading Strength") + theme_bw(base_size=10) + 
  # remove labels and tweak margins for combining with the correlation matrix plot
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = unit(c(3,1,30.75,-3), "mm")) +
  scale_fill_manual(values = c("#66C2A5","#A6CEE3","#E78AC3","#CAB2D6"))
p2

#---------------- This is the same code but different colour themes ----------------#
# Load Wes Anderson colour palette
library(wesanderson)

# Load Harry Potter colour palette
library(harrypotter)

p2 <- ggplot(loadings.m, aes(ACEs, abs(Loading), fill=Factor)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ylab("Loading Strength") + theme_bw(base_size=10) + 
  # remove labels and tweak margins for combining with the correlation matrix plot
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = unit(c(3,1,30.75,-3), "mm")) +
  scale_fill_manual(values = wes_palette("Moonrise3", n = 4))

p2 <- ggplot(loadings.m, aes(ACEs, abs(Loading), fill=Factor)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ylab("Loading Strength") + theme_bw(base_size=10) + 
  # remove labels and tweak margins for combining with the correlation matrix plot
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = unit(c(3,1,30.75,-3), "mm")) +
  scale_fill_manual(values = wes_palette("GrandBudapest2", n = 4))

p2 <- ggplot(loadings.m, aes(ACEs, abs(Loading), fill=Factor)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ylab("Loading Strength") + theme_bw(base_size=10) + 
  # remove labels and tweak margins for combining with the correlation matrix plot
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = unit(c(3,1,30.75,-3), "mm")) +
  scale_fill_hp(discrete=TRUE, option = "Ravenclaw")

p2 <- ggplot(loadings.m, aes(ACEs, abs(Loading), fill=Factor)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ylab("Loading Strength") + theme_bw(base_size=10) + 
  # remove labels and tweak margins for combining with the correlation matrix plot
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = unit(c(3,1,30.75,-3), "mm")) +
  scale_fill_hp(discrete=TRUE, option = "Always")
#-----------------------------------------------------------------------------------#

# Add stacked bar graph of factor loadings to correlation matrix
# so that we can see how the factor analysis transformed these pairwise correlations into factors
library(gridExtra)
grid.arrange(p1, p2, ncol=2, widths=c(2, 1))

#------------------------- Extract factor scores ------------------------------------#
# Get factor scores 
factor_scores <- factor.scores(MCS_ACEs, irt_4factor$fa)
factor_scores

# Get columns of factor scores 
fs_cols <- factor_scores$scores
fs_cols <- as.data.frame(fs_cols)

# Add factor score columns to imputed_MCS dataset
imputed_MCS <- imputed_MCS %>% mutate(
  "Deprivation" = fs_cols$Deprivation,
  "Victimisation" = fs_cols$Victimisation,
  "Parental_Threat" = fs_cols$`Parental Threat`,
  "Parental_Discipline" = fs_cols$`Parental Discipline`,
)

head(imputed_MCS)

# Rename covariates
imputed_MCS$Sex <- as.factor(imputed_MCS$AHCSEX00)
imputed_MCS$Ethnicity <- as.factor(imputed_MCS$ADC06E00)

# Check prevalence of sex and ethnicity variables
table(imputed_MCS$Sex, useNA="always")
table(imputed_MCS$Ethnicity, useNA="always")

# Recode sex reference level to male
imputed_MCS$Sex <- relevel(imputed_MCS$Sex, ref = "Male")

# Recode race reference level to White
imputed_MCS$Ethnicity <- relevel(imputed_MCS$Ethnicity, ref = "White  ")

#------------------------- Check linear regression assumptions ------------------------------------# 

# Check correlations between factor scores to ensure no collinearity
cor(imputed_MCS[, c("Deprivation", "Victimisation", "Parental_Threat", "Parental_Discipline")], use = "complete.obs")

# Check whether the outcome variables follow a normal distribution
hist(imputed_MCS$Internalising_symptoms)
hist(imputed_MCS$Externalising_symptoms)
# Distribution is skewed to the left, but this makes sense because a higher score represents worse mental health

# Standardise predictor and outcome variables
imputed_MCS$Deprivation <- scale(imputed_MCS$Deprivation)
imputed_MCS$Victimisation <- scale(imputed_MCS$Victimisation)
imputed_MCS$Parental_Threat <- scale(imputed_MCS$Parental_Threat)
imputed_MCS$Parental_Discipline <- scale(imputed_MCS$Parental_Discipline)

imputed_MCS$Internalising_symptoms <- scale(imputed_MCS$Internalising_symptoms)
imputed_MCS$Externalising_symptoms <- scale(imputed_MCS$Externalising_symptoms)

# Now the mean is 0 and standard deviation is 1
describe(imputed_MCS$Deprivation) 
describe(imputed_MCS$Victimisation) 
describe(imputed_MCS$Parental_Threat) 
describe(imputed_MCS$Parental_Discipline)

describe(imputed_MCS$Internalising_symptoms) 
describe(imputed_MCS$Externalising_symptoms) 

#------------------------- Unadjusted Regression Models Part 1: one factor and the outcomes -------------------------#

# Internalising symptoms ~ Deprivation
dep_int <- lm(Internalising_symptoms ~ Deprivation, data = imputed_MCS)
summary(dep_int)

# Externalising symptoms ~ Deprivation
dep_ext <- lm(Externalising_symptoms ~ Deprivation, data = imputed_MCS)
summary(dep_ext)

# Internalising symptoms ~ Victimisation
vic_int <- lm(Internalising_symptoms ~ Victimisation, data = imputed_MCS)
summary(vic_int)

# Externalising symptoms ~ Victimisation
vic_ext <- lm(Externalising_symptoms ~ Victimisation, data = imputed_MCS)
summary(vic_ext)

# Internalising symptoms ~ Parental Threat
threat_int <- lm(Internalising_symptoms ~ Parental_Threat, data = imputed_MCS)
summary(threat_int)

# Externalising symptoms ~ Parental Threat
threat_ext <- lm(Externalising_symptoms ~ Parental_Threat, data = imputed_MCS)
summary(threat_ext)

# Internalising symptoms ~ Parental Discipline
disc_int <- lm(Internalising_symptoms ~ Parental_Discipline, data = imputed_MCS)
summary(disc_int)

# Externalising symptoms ~ Parental Discipline
disc_ext <- lm(Externalising_symptoms ~ Parental_Discipline, data = imputed_MCS)
summary(disc_ext)

#------------------------- Adjusted Regression Models Part 2: one factor + covariates and the outcomes -------------------------#

# Internalising symptoms ~ Deprivation + Sex + Ethnicity
int1 <- lm(Internalising_symptoms ~ Deprivation + Sex + Ethnicity, data = imputed_MCS)
summary(int1)

# Externalising symptoms ~ Deprivation + Sex + Ethnicity
ext1 <- lm(Externalising_symptoms ~ Deprivation + Sex + Ethnicity, data = imputed_MCS)
summary(ext1)

# Internalising symptoms ~ Victimisation + Sex + Ethnicity
int2 <- lm(Internalising_symptoms ~ Victimisation + Sex + Ethnicity, data = imputed_MCS)
summary(int2)

# Externalising symptoms ~ Victimisation + Sex + Ethnicity
ext2 <- lm(Externalising_symptoms ~ Victimisation + Sex + Ethnicity, data = imputed_MCS)
summary(ext2)

# Internalising symptoms ~ Parental Threat + Sex + Ethnicity
int3 <- lm(Internalising_symptoms ~ Parental_Threat + Sex + Ethnicity, data = imputed_MCS)
summary(int3)

# Externalising symptoms ~ Parental Threat + Sex + Ethnicity
ext3 <- lm(Externalising_symptoms ~ Parental_Threat + Sex + Ethnicity, data = imputed_MCS)
summary(ext3)

# Internalising symptoms ~ Parental Discipline + Sex + Ethnicity
int4 <- lm(Internalising_symptoms ~ Parental_Discipline + Sex + Ethnicity, data = imputed_MCS)
summary(int4)

# Externalising symptoms ~ Parental Discipline + Sex + Ethnicity
ext4 <- lm(Externalising_symptoms ~ Parental_Discipline + Sex + Ethnicity, data = imputed_MCS)
summary(ext4)

#----------------- Regression Model Part 3: all factors + covariates and the outcomes (multiple regression) -----------------#

multireg_lm <- lm(cbind(Internalising_symptoms, Externalising_symptoms) ~ Deprivation + Victimisation + Parental_Threat + Parental_Discipline + Sex + Ethnicity, data = imputed_MCS)
summary(multireg_lm)

mean(multireg_lm$residuals) # check the mean of residuals is approximately zero
qqnorm(multireg_lm$residuals) # check the residuals are normally distributed

# Calculate variance inflation factor (VIF)
# VIF is a measure to analyze the magnitude of multicollinearity of model terms
# VIF less than 5 indicates a low correlation of that predictor with other predictors
library(performance)

# Internalising symptoms
multi_int <- lm(Internalising_symptoms ~ Deprivation + Victimisation + Parental_Threat + Parental_Discipline + Sex + Ethnicity, data = imputed_MCS)
summary(multi_int)

check_collinearity(multi_int)
vif(multi_int)

# Externalising symptoms
multi_ext <- lm(Externalising_symptoms ~ Deprivation + Victimisation + Parental_Threat + Parental_Discipline + Sex + Ethnicity, data = imputed_MCS)
summary(multi_ext)

check_collinearity(multi_ext)
vif(multi_ext)

#------------------------- Format regression tables into word document -------------------------#

# Format unadjusted regression models 
# Internalising symptoms
int_unadjusted <- stargazer(dep_int, vic_int, threat_int, disc_int,
                            type="html",
                            title="Unadjusted associations between ACE dimensions and internalising symptoms for the MCS imputed sample",
                            covariate.labels=c("Deprivation", "Victimisation", "Parental Threat", "Parental Discipline"),
                            column.labels=c("Deprivation ~ Internalising", "Victimisation ~ Internalising", "Parental Threat ~ Internalising", "Parental Discipline ~ Internalising"),
                            column.sep.width="15pt",
                            dep.var.caption="Internalising symptoms",
                            dep.var.labels="",
                            ci=TRUE,
                            ci.level=0.95,
                            align=TRUE,
                            out="imp_mcs_unadjusted_internalising.doc",
                            digits=2, digits.extra=3,
                            omit="Constant",
                            single.row=TRUE)

# Externalising symptoms
ext_unadjusted <- stargazer(dep_ext, vic_ext, threat_ext, disc_ext,
                            type="html",
                            title="Unadjusted associations between ACE dimensions and externalising symptoms for the MCS imputed sample",
                            covariate.labels=c("Deprivation", "Victimisation", "Parental Threat", "Parental Discipline"),
                            column.labels=c("Deprivation ~ Externalising", "Victimisation ~ Externalising", "Parental Threat ~ Externalising", "Parental Discipline ~ Externalising"),
                            column.sep.width="15pt",
                            dep.var.caption="Externalising symptoms",
                            dep.var.labels="",
                            ci=TRUE,
                            ci.level=0.95,
                            align=TRUE,
                            out="imp_mcs_unadjusted_externalising.doc",
                            digits=2, digits.extra=3,
                            omit="Constant",
                            single.row=TRUE)

## Format adjusted regression models 
# Internalising symptoms
int_adjusted <- stargazer(int1, int2, int3, int4, multi_int,
                          type="html",
                          title="Adjusted associations between ACE dimensions and internalising symptoms for the MCS imputed sample",
                          covariate.labels=c("Deprivation", "Victimisation", "Parental Threat", "Parental Discipline",
                                             "Female sex", "Black or Black British", "Indian",
                                             "Mixed", "Other (inc. Chinese)",
                                             "Pakistani and Bangladeshi"),
                          column.labels=c("Deprivation ~ Internalising", "Victimisation ~ Internalising", "Parental Threat ~ Internalising", "Parental Discipline ~ Internalising", 
                                          "Deprivation + Victimisation + Parental Threat + Parental Discipline ~ Internalising"),
                          column.sep.width="15pt",
                          dep.var.caption="Internalising symptoms",
                          dep.var.labels="",
                          ci=TRUE,
                          ci.level=0.95,
                          align=TRUE,
                          out="imp_mcs_adjusted_internalising.doc",
                          digits=2, digits.extra=3,
                          omit="Constant",
                          single.row=TRUE)

# Externalising symptoms
ext_adjusted <- stargazer(ext1, ext2, ext3, ext4, multi_ext,
                          type="html",
                          title="Adjusted associations between ACE dimensions and externalising symptoms for the MCS imputed sample",
                          covariate.labels=c("Deprivation", "Victimisation", "Parental Threat", "Parental Discipline",
                                             "Female sex", "Black or Black British", "Indian",
                                             "Mixed", "Other (inc. Chinese)",
                                             "Pakistani and Bangladeshi"),
                          column.labels=c("Deprivation ~ Externalising", "Victimisation ~ Externalising", "Parental Threat ~ Externalising", "Parental Discipline ~ Externalising",
                                          "Deprivation + Victimisation + Parental Threat + Parental Discipline ~ Externalising"),
                          column.sep.width="15pt",
                          dep.var.caption="Externalising symptoms",
                          dep.var.labels="",
                          ci=TRUE,
                          ci.level=0.95,
                          align=TRUE,
                          out="imp_mcs_adjusted_externalising.doc",
                          digits=2, digits.extra=3,
                          omit="Constant",
                          single.row=TRUE)

#------------------------- Exploratory analysis: test for interactions and stratify analyses by sex  -------------------------#

# Test if there is an interaction between sex and main effects

# Internalising symptoms
multireg_int_interaction <- lm(Internalising_symptoms ~ Deprivation + Victimisation + Parental_Threat + Parental_Discipline + Sex + Ethnicity + Sex*Deprivation + Sex*Victimisation + Sex*Parental_Threat + Sex*Parental_Discipline, data=imputed_MCS)
summary(multireg_int_interaction)
# There is a significant interaction between Sex*Victimisation and Sex*Parental_Discipline for internalising symptoms

# Externalising symptoms
multireg_ext_interaction <- lm(Externalising_symptoms ~ Deprivation + Victimisation + Parental_Threat + Parental_Discipline + Sex + Ethnicity + Sex*Deprivation + Sex*Victimisation + Sex*Parental_Threat + Sex*Parental_Discipline, data=imputed_MCS)
summary(multireg_ext_interaction)
# There are no significant interactions for externalising symptoms

## Stratify dataset by sex and rerun analyses
MCS_male <- subset(imputed_MCS, imputed_MCS$Sex=="Male") 
MCS_female <- subset(imputed_MCS, imputed_MCS$Sex=="Female") 

# Males: Internalising symptoms ~ Deprivation + Victimisation + Parental Threat + Parental Discipline + Ethnicity
multi_int_m <- lm(Internalising_symptoms ~ Deprivation + Victimisation + Parental_Threat + Parental_Discipline + Ethnicity, data = MCS_male)
summary(multi_int_m)

# Females: Internalising symptoms ~ Deprivation + Victimisation + Parental Threat + Parental Discipline + Ethnicity
multi_int_f <- lm(Internalising_symptoms ~ Deprivation + Victimisation + Parental_Threat + Parental_Discipline + Ethnicity, data = MCS_female)
summary(multi_int_f)
```
