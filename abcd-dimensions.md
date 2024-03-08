``` r
library(psych)
library(dplyr)
library(ggplot2)
library(tibble)
```

# 0. Merge ABCD dataset

To begin, we will merge the measures of adverse childhood experiences
(ACEs) from their respective ABCD datasets into a single dataset.

``` r
# Following guidance from dataset merging code written by Dr Jessie Baldwin: https://github.com/jr-baldwin/ACEs_mental_health_RR/blob/main/6_ABCD_DeriveMeasures_20220203.R

## ============== Import all datasets =================
setwd("/Users/athenachowruwern/Desktop")

# KSADS PTSD (parent reported)
ksads_ptsd <- read.delim("ABCD Datafiles/abcd_ptsd01.txt", header = TRUE, sep = "\t", dec = ".")

# Family Environment Scale (parent reported)
fam_env_par <- read.delim("ABCD Datafiles/fes02.txt", header = TRUE, sep = "\t", dec = ".")

# Family Environment Scale (child reported)
fam_env_youth <- read.delim("ABCD Datafiles/abcd_fes01.txt", header = TRUE, sep = "\t", dec = ".")

# Children's Report of Parental Behavioral Inventory (child reported)
par_behav_invent_youth <- read.delim("ABCD Datafiles/crpbi01.txt", header = TRUE, sep = "\t", dec = ".")

# Family History Assessment Part 1 (parent reported)
fha_1 <- read.delim("ABCD Datafiles/fhxp102.txt", header = TRUE, sep = "\t", dec = ".")

# Family History Assessment Part 2 (parent reported)
fha_2 <- read.delim("ABCD Datafiles/fhxp201.txt", header = TRUE, sep = "\t", dec = ".")

# Parent adult self-report sum scores (parent reported)
asrs <- read.delim("ABCD Datafiles/abcd_asrs01.txt", header = TRUE, sep = "\t", dec = ".")

# Parent adult self-report raw scores (parent reported)
rasr <- read.delim("ABCD Datafiles/pasr01.txt", header = TRUE, sep = "\t", dec = ".")

# Parent Life Events (parent reported)
ple <- read.delim("ABCD Datafiles/abcd_ple01.txt", header = TRUE, sep = "\t", dec = ".")

# Parent Demographics Survey (parent reported) # this takes over 40 mins to load
par_dem <- read.delim("ABCD Datafiles/pdem02.txt", header = TRUE, sep = "\t", dec = ".")

# ABCD Longitudinal Parent Demographics Survey (parent reported)
long_par_dem <- read.delim("ABCD Datafiles/abcd_lpds01.txt", header = TRUE, sep = "\t", dec = ".")

# ABCD Developmental History Questionnaire (parent reported)
dhx01 <- read.delim("ABCD Datafiles/dhx01.txt", header = TRUE, sep = "\t", dec = ".")

# Child peer victimisation (child reported)
vic <- read.delim("ABCD Datafiles/abcd_peq01.txt", header = TRUE, sep = "\t", dec = ".")

# Child cyber victimisation (child reported)
cybervic <- read.delim("ABCD Datafiles/abcd_cb01.txt", header = TRUE, sep = "\t", dec = ".")

# Parent Neighbourhood Safety/Crime Survey (parent reported)
neighb_safe_par <- read.delim("ABCD Datafiles/abcd_pnsc01.txt", header = TRUE, sep = "\t", dec = ".")

# Parent Neighbourhood Safety/Crime Survey (child reported)
neighb_safe_youth <- read.delim("ABCD Datafiles/abcd_nsc01.txt", header = TRUE, sep = "\t", dec = ".")

# CBCL summary scores (parent reported)
cbcl <- read.delim("ABCD Datafiles/abcd_cbcls01.txt", header = TRUE, sep = "\t", dec = ".")

# CBCL raw scores (parent reported)
cbcl_raw <-  read.delim("ABCD Datafiles/abcd_cbcl01.txt", header = TRUE, sep = "\t", dec = ".")

## ============== Format datasets before merging into one =================
## Combine dataframes into a list
List <- function(...) {
  names <- as.list(substitute(list(...)))[-1L]
  setNames(list(...), names)
}

tables <- List(ksads_ptsd, par_behav_invent_youth, 
               fha_1, fha_2, ple, long_par_dem, par_dem, 
               fam_env_par, fam_env_youth, vic, cybervic, 
               neighb_safe_par, neighb_safe_youth, 
               cbcl, cbcl_raw, asrs, rasr, dhx01)

names(tables)
len.tables=length(tables)

# Sometimes the "eventname" column in datasets is called "visit" 
# Check if this is the case in any dataframes
for (p in 1:len.tables) {
  dt = tables[[p]]
  if ("visit" %in% names(dt) ){
    print(names(tables)[p]) # if visit is present, print the name of the dataframe
      }
}

# Rename "visit" to "eventname" in fha_2 and dhx01
tables$fha_2 <- rename(tables$fha_2, eventname = visit)
tables$dhx01 <- rename(tables$dhx01, eventname = visit)

# The first row in each spreadsheet is the element description, so remove from all dataframes
tables <- lapply(tables, function(x) x=x[-1,])
nrow(tables$asrs) # check that the number of rows is 1 less than the number shown in asrs (example dataset) in global environment

# Drop columns introduced by NDA, they are not required in the resulting table.
for (p in 1:len.tables) {
  dt = tables[[p]]
  dt = dt[,!(names(dt) %in% c("collection_id", "collection_title", "subjectkey",
                              "dataset_id", "interview_date", "interview_age"))]
  tables[[p]] = dt
}

# Subset each dataset to variables of interest only

# KSADS PTSD (parent reported) for physical abuse, emotional abuse, sexual abuse, domestic violence
tables$ksads_ptsd <- select(tables$ksads_ptsd,
                            src_subject_id, sex, eventname, 
                            ksads_ptsd_raw_754_p, # car accident in which your child required medical attention
                            ksads_ptsd_raw_755_p, # significant accident for which your child needed medical treatment
                            ksads_ptsd_raw_756_p, # witnessed or caught in a fire
                            ksads_ptsd_raw_757_p, # witnessed or caught in a natural disaster 
                            ksads_ptsd_raw_758_p, # witnessed or present during an act of terrorism 
                            ksads_ptsd_raw_759_p, # witnessed death or mass destruction in a war zone
                            ksads_ptsd_raw_760_p, # witnessed someone shot or stabbed in the community
                            ksads_ptsd_raw_762_p, # shot, stabbed, or beaten brutally by a grown up in the home
                            ksads_ptsd_raw_763_p, # beaten to the point of bruises by a grown up in the home
                            ksads_ptsd_raw_764_p, # non-family member threatened to kill your child
                            ksads_ptsd_raw_765_p, # family member threatened to kill your child
                            ksads_ptsd_raw_766_p, # witness grownups in the home push, shove or hit one another
                            ksads_ptsd_raw_767_p, # grown up in home touched your child in their privates
                            ksads_ptsd_raw_768_p, # adult outside family touched your child in their privates
                            ksads_ptsd_raw_769_p, # peer forced your child to do something sexually
                            ksads_ptsd_raw_770_p) # learned about the sudden unexpected death of a loved one
                          
# Family Environment Scale (parent reported) for domestic violence
tables$fam_env_par <- select(tables$fam_env_par,
                             src_subject_id, sex, eventname,
                             fam_enviro6_p) # family members sometimes hit each other

# Family Environment Scale (child reported) for domestic violence
tables$fam_env_youth <- select(tables$fam_env_youth,
                               src_subject_id, sex, eventname,
                               fes_youth_q6) # family members sometimes hit each other

# Children's Report of Parental Behavioral Inventory (child reported) for emotional neglect
tables$par_behav_invent_youth <- select(tables$par_behav_invent_youth,
                                        src_subject_id, sex, eventname, 
                                        crpbi_parent1_y, # makes me feel better after talking over my worries
                                        crpbi_parent2_y, # smiles at me very often
                                        crpbi_parent3_y, # able to make me feel better when I am upset
                                        crpbi_parent4_y, # believes in showing his/her love for me
                                        crpbi_parent5_y) # is easy to talk to
                          
# Family History Assessment Part 1 (parent reported) for parental alcohol/drug abuse and parental psychopathology 
tables$fha_1 <- select(tables$fha_1,
                       src_subject_id, sex, eventname, 
                       famhx_4_p, # relative of child ever had alcohol problems 
                       famhx4a_p___1, # biological father alcohol marital problems
                       famhx4a_p___2, # biological father alcohol work problems
                       famhx4a_p___3, # biological father alcohol arrests/DUI
                       famhx4a_p___4, # biological father alcohol treatment programme
                       famhx4a_p___6, # biological father alcohol isolated self
                       famhx4a_p___7, # biological father alcohol health problems
                       famhx_4d_p___1, # biological mother alcohol marital problems
                       famhx_4d_p___2, # biological mother alcohol work problems
                       famhx_4d_p___3, # biological mother alcohol arrests/DUI
                       famhx_4d_p___4, # biological mother alcohol treatment programme
                       famhx_4d_p___6, # biological mother alcohol isolated self
                       famhx_4d_p___7, # biological mother alcohol health problems
                       fam_history_5_yes_no, # relative of child ever had drug problems 
                       fam_history_q5a_drugs___1, # biological father drug marital problems
                       fam_history_q5a_drugs___2, # biological father drug work problems
                       fam_history_q5a_drugs___3, # biological father drug arrests/DUI
                       fam_history_q5a_drugs___4, # biological father drug treatment programme
                       fam_history_q5a_drugs___6, # biological father drug isolated self
                       fam_history_q5a_drugs___7, # biological father drug health problems
                       fam_history_q5d_drugs___1, # biological mother drug marital problems
                       fam_history_q5d_drugs___2, # biological mother drug work problems
                       fam_history_q5d_drugs___3, # biological mother drug arrests/DUI
                       fam_history_q5d_drugs___4, # biological mother drug treatment programme
                       fam_history_q5d_drugs___6, # biological mother drug isolated self
                       fam_history_q5d_drugs___7, # biological mother drug health problems
                       fam_history_6_yes_no, # relative of child ever suffered from depression
                       fam_history_q6a_depression, # biological father depression
                       fam_history_q6d_depression) # biological mother depression
                       
# Family History Assessment Part 2 (parent reported) for parental psychopathology
tables$fha_2 <- select(tables$fha_2,
                       src_subject_id, sex, eventname,
                       fam_history_7_yes_no, # relative of child ever had mania
                       fam_history_q7a_mania, # biological father mania
                       fam_history_q7d_mania, # biological mother mania 
                       fam_history_8_yes_no, # relative of child ever had visions
                       fam_history_q8a_visions, # biological father visions
                       fam_history_q8d_visions, # biological mother visions
                       fam_history_13_yes_no, # relative of child ever attempted suicide
                       fam_history_q13a_suicide, # biological father suicide
                       fam_history_q13d_suicide) # biological mother suicide

# Parent adult self-report sum scores (parent reported) for parental psychopathology 
tables$asrs <- select(tables$asrs,
                      src_subject_id, sex, eventname,
                      asr_scr_depress_t, # depressive problems DSM-5 t-score
                      asr_scr_anxdisord_t, # anxiety problems DSM-5 t-score
                      asr_scr_adhd_t, # ADHD problems DSM-5 t-score
                      asr_scr_anxdep_t, # anxious/depressed t-score
                      asr_scr_withdrawn_t, # withdrawn t-score
                      asr_scr_somatic_t, # somatic complaints t-score
                      asr_scr_thought_t, # thought problems t-score
                      asr_scr_attention_t, # attention problems t-score
                      asr_scr_aggressive_t) # aggressive behaviour t-score

# Parent adult self-report raw scores (parent reported) for parental drug abuse
tables$rasr <- select(tables$rasr,
                      src_subject_id, sex, eventname,
                      asr_q126_p) # drug use in the past 6 months

# Parent Life Events (parent reported) for parental criminality and parental separation
tables$ple <- select(tables$ple,
                     src_subject_id, sex, eventname,
                     ple_arrest_p, # someone in the family was arrested
                     ple_arrest_past_yr_p, # someone arrested in the past year
                     ple_law_p, # parents got into trouble with the law
                     ple_law_past_yr_p, # parents trouble with the law in the past year
                     ple_jail_p, # parent went to jail
                     ple_jail_past_yr_p, # parent went to jail in the past year
                     ple_separ_p, # parents separated or divorced
                     ple_separ_past_yr_p) # parents separated or divorced in the past year
                              
# Parent Demographics Survey (parent reported) for parental separation and demographics
tables$par_dem <- select(tables$par_dem,
                         src_subject_id, sex, eventname,
                         demo_prnt_marital_v2, # married, widowed, divorced, separated, never married or living with a partner
                         demo_prnt_prtnr_v2, # do you have a partner
                         demo_prnt_prtnr_bio, # is your partner the child's biological parent
                         demo_prnt_prtnr_adopt, # is your partner the child's adoptive parent
                         demo_race_a_p___10, # white 
                         demo_race_a_p___11, # black/african american
                         demo_race_a_p___12, # native american
                         demo_race_a_p___13, # alaska native
                         demo_race_a_p___14, # native hawaiian
                         demo_race_a_p___15, # guamanian
                         demo_race_a_p___16, # samoan
                         demo_race_a_p___17, # other pacific islander
                         demo_race_a_p___18, # asian indian
                         demo_race_a_p___19, # chinese
                         demo_race_a_p___20, # filipino
                         demo_race_a_p___21, # japanese
                         demo_race_a_p___22, # korean
                         demo_race_a_p___23, # vietnamese
                         demo_race_a_p___24, # other asian
                         demo_race_a_p___25, # other race
                         demo_prnt_empl_v2, # parent employment 
                         demo_prnt_ed_v2, # parent education
                         demo_prtnr_ed_v2, # partner education
                         demo_comb_income_v2, # household income
                         demo_roster_v2, # household size
                         demo_fam_exp1_v2, # difficulty affording food
                         demo_fam_exp4_v2, # evicted from home because could not pay rent/mortgage
                         demo_fam_exp5_v2) # difficulty affording gas/electricity/oil

# ABCD Longitudinal Parent Demographics Survey (parent reported) for demographics
tables$long_par_dem <- select(tables$long_par_dem,
                              src_subject_id, sex, eventname,
                              demo_prnt_marital_v2_l, # married, widowed, divorced, separated, never married or living with a partner
                              demo_prnt_prtnr_v2_l, # do you have a partner
                              demo_prnt_prtnr_bio_l, # is your partner the child's biological parent
                              demo_prnt_prtnr_adopt_l, # is your partner the child's adoptive parent
                              demo_comb_income_v2_l) # household income

# ABCD Developmental History Questionnaire (parent reported) for auxiliary variables
tables$dhx01 <- select(tables$dhx01,
                       src_subject_id, sex, eventname,
                       birth_weight_lbs, # birthweight in lbs
                       devhx_3_p, # maternal age at birth
                       devhx_8_tobacco, # maternal smoking before knowing of pregnancy
                       devhx_9_tobacco, # maternal smoking knowing of pregnancy
                       devhx_8_alcohol, # maternal alcohol before knowing of pregnancy
                       devhx_9_alcohol, # maternal alcohol knowing of pregnancy
                       devhx_12a_p, # premature birth 
                       devhx_14a3_p) # pregnancy complications 

# Child peer victimisation (child reported)
tables$vic <- select(tables$vic,
                     src_subject_id, sex, eventname,
                     peq_left_out_vic, # kids left me out of activity/conversation
                     peq_chase_vic, # kid chased me like trying to hurt me
                     peq_rumor_vic, # kid spread rumours about me
                     peq_invite_vic, # kid did not invite me to party
                     peq_exclude_vic, # kid left me out 
                     peq_gossip_vic, # kid gossiped about me
                     peq_threat_vic, # kid threatened to hurt/beat me
                     peq_loser_vic, # kid said mean things about me
                     peq_hit_vic) # kid hit/kicked/pushed me 

# Child cyber victimisation (child reported)
tables$cybervic <- select(tables$cybervic,
                          src_subject_id, sex, eventname,
                          cybb_phenx_harm, # have you ever been cyberbullied
                          cybb_phenx_harm_12mo, # cyberbullied in the past 12 months
                          cybb_phenx_harm_often) # how often cyberbullied in the past 12 months

# Parent Neighbourhood Safety/Crime Survey (parent reported) for unsafe home area
tables$neighb_safe_par <- select(tables$neighb_safe_par,
                                 src_subject_id, sex, eventname,
                                 neighborhood1r_p, # feel safe walking in my neighborhood, day or night
                                 neighborhood2r_p, # violence is not a problem in my neighborhood
                                 neighborhood3r_p) # my neighborhood is safe from crime
                                 
# Parent Neighbourhood Safety/Crime Survey (child reported) for unsafe home area
tables$neighb_safe_youth <- select(tables$neighb_safe_youth,
                                   src_subject_id, sex, eventname,
                                   neighborhood_crime_y) # my neighborhood is safe from crime
                                    
# CBCL summary scores (parent reported) for child mental health
tables$cbcl <- select(tables$cbcl,
                      src_subject_id, sex, eventname,
                      cbcl_scr_syn_internal_r,
                      cbcl_scr_syn_external_r)
tables$cbcl <- tables$cbcl[ , ! names(tables$cbcl) %in% c("abcd_cbcls01_id")] # remove unwanted IDs

# CBCL raw scores (parent reported) for child mental health
tables$cbcl_raw <- tables$cbcl_raw[ , ! names(tables$cbcl_raw) %in% c("abcd_cbcl01_id", 
                                                                      "cbcl_select_language___1", 
                                                                      "timept")] # remove unwanted IDs

# Put individual dataframes in list back as separate objects in global environment
list2env(tables,.GlobalEnv)

## ============== Merge datafiles to create one ABCD dataset =================
abcd <- Reduce(function(x,y) merge(x,y,by=c("src_subject_id", "sex", "eventname"), all=TRUE), #Outer join - preserves all rows
                    list(ksads_ptsd,
                         fam_env_par,
                         fam_env_youth,
                         par_behav_invent_youth,
                         fha_1,
                         fha_2,
                         asrs,
                         rasr,
                         ple,
                         par_dem,
                         long_par_dem,
                         dhx01,
                         vic,
                         cybervic,
                         neighb_safe_par,
                         neighb_safe_youth,
                         cbcl,
                         cbcl_raw))
colnames(abcd)
dim(abcd) # 39766 rows, because there are 4 assessments and the dataset is in long format
table(abcd$eventname)
head(abcd)
tail(abcd)

# Save ABCD merged dataset
write.table(abcd, file = "ABCD_merged.txt", sep = "\t", dec = ".")

# Remove individual datafiles 
rm(ksads_ptsd, fam_env_par, fam_env_youth, par_behav_invent_youth, 
   fha_1, fha_2, asrs, rasr, ple, par_dem, long_par_dem, dhx01, vic, cybervic, 
   neighb_safe_par, neighb_safe_youth, cbcl, cbcl_raw, 
   tables, dt, len.tables, p, List)
```

# 1. Data cleaning

Next, we will sum together measures, derive variables, and ensure all
variables are recoded to the correct format before conducting the factor
analysis.

``` r
# Load ABCD merged dataset
setwd("/Users/athenachowruwern/Desktop")
abcd <-  read.delim("ABCD_merged.txt", header = TRUE, sep = "\t", dec = ".")

####################################################################################
#----------- Derive baseline auxiliary variables for imputation -------------------#
####################################################################################

## Socio-demographic indicators
# Child sex = sex
# Child race = demo_race_a_p___10
# Parental employment = demo_prnt_empl_v2
# Parental highest education qualification = demo_prnt_ed_v2, demo_prtnr_ed_v2
# Household size = demo_roster_v2
# Maternal age at birth = devhx_3_p

## Adversity exposure before birth
# Birthweight = birth_weight_lbs
# Premature birth = devhx_12a_p
# Maternal smoking during pregnancy = devhx_8_tobacco, devhx_9_tobacco
# Maternal alcohol consumption during pregnancy = devhx_8_alcohol, devhx_9_alcohol
# Pregnancy complications = devhx_14a3_p

## Financial adversity
# Difficulty affording food = demo_fam_exp1_v2
# Difficulty affording gas/electricity/oil = demo_fam_exp5_v2
# Evicted from home because could not pay rent/mortgage = demo_fam_exp4_v2

# Parent AESBA mental health scores:
# Anxious/Depressed scale = asr_scr_anxdep_t
# Withdrawn scale = asr_scr_withdrawn_t
# Somatic Complaints scale = asr_scr_somatic_t
# Thought Problems scale = asr_scr_thought_t
# Attention Problems scale = asr_scr_attention_t
# Aggressive Behaviour scale = asr_scr_aggressive_t
# Rule-Breaking Behaviour scale = asr_scr_rulebreak_t

# Race
abcd$race <- NA
abcd$race[abcd$demo_race_a_p___10==1] <- "White"
abcd$race[abcd$demo_race_a_p___11==1] <- "Black/African American"
abcd$race[abcd$demo_race_a_p___12==1] <- "Native American"
abcd$race[abcd$demo_race_a_p___13==1] <- "Alaska Native"
abcd$race[abcd$demo_race_a_p___14==1] <- "Native Hawaiian"
abcd$race[abcd$demo_race_a_p___15==1] <- "Guamanian"
abcd$race[abcd$demo_race_a_p___16==1] <- "Samoan"
abcd$race[abcd$demo_race_a_p___17==1] <- "Other Pacific Islander"
abcd$race[abcd$demo_race_a_p___18==1] <- "Asian Indian"
abcd$race[abcd$demo_race_a_p___19==1] <- "Chinese"
abcd$race[abcd$demo_race_a_p___20==1] <- "Filipino"
abcd$race[abcd$demo_race_a_p___21==1] <- "Japanese"
abcd$race[abcd$demo_race_a_p___22==1] <- "Korean"
abcd$race[abcd$demo_race_a_p___23==1] <- "Vietnamese"
abcd$race[abcd$demo_race_a_p___24==1] <- "Other Asian"
abcd$race[abcd$demo_race_a_p___25==1] <- "Other Race"
table(abcd$race[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")
class(abcd$race)
abcd$race <- as.factor(abcd$race)

# Parent employment
abcd$employment <- as.character(abcd$demo_prnt_empl_v2)
abcd$employment[abcd$employment==1] <- "Working now"
abcd$employment[abcd$employment==2] <- "Temporarily laid off"
abcd$employment[abcd$employment==9] <- "Sick leave"
abcd$employment[abcd$employment==10] <- "Maternity leave"
abcd$employment[abcd$employment== 11] <- "Unemployed not looking for work"
abcd$employment[abcd$employment==3] <- "Looking for work"
abcd$employment[abcd$employment==4] <- "Retired"
abcd$employment[abcd$employment==5] <- "Disabled"
abcd$employment[abcd$employment==6] <- "Stay at home parent"
abcd$employment[abcd$employment==7] <- "Student"
abcd$employment[abcd$employment==8] <- "Other"
abcd$employment[abcd$employment==777] <- NA
table(abcd$employment[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")
class(abcd$employment)
abcd$employment <- as.factor(abcd$employment)

# Parental highest education qualification
# Derive var with 5 different levels that correspond to the numbers published by the American Community Survey (ACS), following example: https://github.com/ABCD-STUDY/analysis-nda/blob/master/notebooks/general/core_demographics3.0.R
high.educ1 = abcd$demo_prnt_ed_v2
high.educ2 = abcd$demo_prtnr_ed_v2
high.educ1[which(high.educ1 == "999")] = NA
high.educ2[which(high.educ2 == "999")] = NA
high.educ1[which(high.educ1 == "777")] = NA
high.educ2[which(high.educ2 == "777")] = NA
high.educ = pmax(as.numeric(as.character(high.educ1)), as.numeric(as.character(high.educ2)), na.rm=T)
idx <- which(high.educ %in% 0:12, arr.ind = TRUE)
high.educ[idx] = 1 # "< HS Diploma"
idx <- which(high.educ %in% 13:14, arr.ind = TRUE)
high.educ[idx] = 2 # "HS Diploma/GED"
idx <- which(high.educ %in% 15:17, arr.ind = TRUE)
high.educ[idx] = 3 # "Some College"
idx <- which(high.educ == 18, arr.ind = TRUE)
high.educ[idx] = 4 # "Bachelor"
idx <- which(high.educ %in% 19:21, arr.ind = TRUE)
high.educ[idx] = 5 # "Post Graduate Degree"
high.educ[which(high.educ == "999")]=NA
high.educ[which(high.educ == "777")]=NA
abcd$high.educ = factor( high.educ, levels= 1:5, labels = c("< HS Diploma","HS Diploma/GED","Some College","Bachelor","Post Graduate Degree") )
table(abcd$high.educ[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")
rm(high.educ, high.educ1, high.educ2, idx)

# Household size 
table(abcd$demo_roster_v2, useNA = "always")
abcd$household_size <- as.numeric(as.character(abcd$demo_roster_v2))
table(abcd$household_size[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# Maternal age at birth 
table(as.character(abcd$devhx_3_p), useNA = "always")
abcd$mat_age_birth <- abcd$devhx_3_p
table(as.character(abcd$mat_age_birth), useNA = "always")
abcd$mat_age_birth <- as.numeric(as.character(abcd$mat_age_birth))
table(abcd$mat_age_birth[abcd$eventname=="baseline_year_1_arm_1"], useNA = "always")

# Birthweight 
table(abcd$birth_weight_lbs[abcd$eventname=="baseline_year_1_arm_1"], useNA = "always")
class(abcd$birth_weight_lbs)
abcd$birth_weight_lbs <- as.numeric(as.character(abcd$birth_weight_lbs))

# Premature birth
table(abcd$devhx_12a_p, useNA = "always")
abcd$devhx_12a_p[abcd$devhx_12a_p==999] <- NA
abcd$premature_birth <- NA
abcd$premature_birth[abcd$devhx_12a_p==1] <- 1
abcd$premature_birth[abcd$devhx_12a_p==0] <- 0
abcd$premature_birth <- as.factor(abcd$premature_birth)
table(abcd$premature_birth[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# Maternal smoking during pregnancy 
table(abcd$devhx_8_tobacco, useNA = "always")
abcd$devhx_8_tobacco[abcd$devhx_8_tobacco==999] <- NA
table(abcd$devhx_9_tobacco, useNA = "always")
abcd$devhx_9_tobacco[abcd$devhx_9_tobacco==999] <- NA
abcd$maternal_smoking_bef_preg <- as.factor(abcd$devhx_8_tobacco) # smoking before knowing of pregnancy
abcd$maternal_smoking_aft_preg <- as.factor(abcd$devhx_9_tobacco) # smoking knowing of pregnancy
table(abcd$maternal_smoking_bef_preg[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")
table(abcd$maternal_smoking_aft_preg[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# Maternal alcohol consumption during pregnancy 
table(abcd$devhx_8_alcohol, useNA = "always")
abcd$devhx_8_alcohol[abcd$devhx_8_alcohol==999] <- NA
table(abcd$devhx_9_alcohol, useNA = "always")
abcd$devhx_9_alcohol[abcd$devhx_9_alcohol==999] <- NA
abcd$maternal_alcohol_bef_preg <- as.factor(abcd$devhx_8_alcohol) # alcohol before knowing of pregnancy
abcd$maternal_alcohol_aft_preg <- as.factor(abcd$devhx_9_alcohol) # alcohol knowing of pregnancy
table(abcd$maternal_alcohol_bef_preg[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")
table(abcd$maternal_alcohol_aft_preg[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# Pregnancy complications
table(abcd$devhx_14a3_p, useNA = "always")
abcd$devhx_14a3_p[abcd$devhx_14a3_p==999] <- NA
abcd$preg_complications <- as.factor(abcd$devhx_14a3_p)
table(abcd$preg_complications[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# Difficulty affording food 
table(abcd$demo_fam_exp1_v2, useNA = "always")
abcd$demo_fam_exp1_v2[abcd$demo_fam_exp1_v2=="777"] <- NA
abcd$diff_afford_food <- as.factor(abcd$demo_fam_exp1_v2)
table(abcd$diff_afford_food[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# Difficulty affording gas/electricity/oil 
table(abcd$demo_fam_exp5_v2, useNA = "always")
abcd$demo_fam_exp5_v2[abcd$demo_fam_exp5_v2=="777"] <- NA
abcd$diff_afford_gas <- as.factor(abcd$demo_fam_exp5_v2)
table(abcd$diff_afford_gas[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# Evicted from home because could not pay rent/mortgage 
table(abcd$demo_fam_exp4_v2, useNA = "always")
abcd$demo_fam_exp4_v2[abcd$demo_fam_exp4_v2=="777"] <- NA
abcd$evicted_home <- as.factor(abcd$demo_fam_exp4_v2)
table(abcd$evicted_home[abcd$eventname=="baseline_year_1_arm_1"], useNA="always")

# Parent AESBA mental health scores
# Convert measures to numeric
# Anxious/Depressed scale = asr_scr_anxdep_t
abcd$par_anxious_depressed <- as.numeric(as.character(abcd$asr_scr_anxdep_t))
# Withdrawn scale = asr_scr_withdrawn_t
abcd$par_withdrawn <- as.numeric(as.character(abcd$asr_scr_withdrawn_t))
# Somatic Complaints scale = asr_scr_somatic_t
abcd$par_somatic_complaints <- as.numeric(as.character(abcd$asr_scr_somatic_t))
# Thought Problems scale = asr_scr_thought_t
abcd$par_thought_problems <- as.numeric(as.character(abcd$asr_scr_thought_t))
# Attention Problems scale = asr_scr_attention_t
abcd$par_attention_problems <- as.numeric(as.character(abcd$asr_scr_attention_t))
# Aggressive Behaviour scale = asr_scr_aggressive_t
abcd$par_aggress_behav <- as.numeric(as.character(abcd$asr_scr_aggressive_t))

####################################################################################
#-------------- Reshape ABCD dataset from long to wide format ---------------------#
####################################################################################

# Note: the existing ABCD dataset is in long format, with multiple rows for each participant 
# according to the time at assessment (eventname variable), e.g. "baseline_year_1_arm_1", "1_year_follow_up_y_arm_1", 
# or "2_year_follow_up_y_arm_1" or "3_year_follow_up_y_arm_1"
# Because some ACEs span multiple assessment times, we need to convert the dataset to wide
# so each participant has one row, and variable names reference the time point of assessment 

# Exclude raw demographics and auxiliary variables
aux_vars <- names(abcd) %in% c("demo_race_a_p___10", "demo_race_a_p___11", "demo_race_a_p___12", 
                                       "demo_race_a_p___13", "demo_race_a_p___14", "demo_race_a_p___15", 
                                       "demo_race_a_p___16", "demo_race_a_p___17", "demo_race_a_p___18", 
                                       "demo_race_a_p___19", "demo_race_a_p___20", "demo_race_a_p___20", 
                                       "demo_race_a_p___21", "demo_race_a_p___21", "demo_race_a_p___22", 
                                       "demo_race_a_p___23", "demo_race_a_p___24", "demo_race_a_p___25", 
                                       "demo_prnt_empl_v2", "demo_prnt_ed_v2", "demo_prtnr_ed_v2", "demo_roster_v2", 
                                       "demo_fam_exp1_v2", "demo_fam_exp4_v2", "demo_fam_exp5_v2", 
                                       "devhx_3_p", "devhx_8_tobacco", "devhx_9_tobacco", 
                                       "devhx_8_alcohol", "devhx_9_alcohol", "devhx_12a_p", "devhx_14a3_p",
                                       "asr_scr_anxdep_t", "asr_scr_withdrawn_t", "asr_scr_somatic_t",
                                       "asr_scr_thought_t", "asr_scr_attention_t", "asr_scr_aggressive_t")

# Subset to ACE exposure and outcome variables only
abcd_small <- abcd[!aux_vars]
colnames(abcd_small)
v_names <- colnames(abcd_small)

# Check variables are the correct class
str(abcd_small, list.len = ncol(abcd_small))
abcd_small$sex <- as.factor(abcd_small$sex) # convert sex to factor

# Reshape from long to wide
abcd_wide <- reshape(data=abcd_small, idvar="src_subject_id", 
         timevar="eventname", direction="wide", 
         v.names=v_names[-c(1,3)]) # variable names = all variables except subject ID and eventname
head(abcd_wide)   
colnames(abcd_wide) # Note: now each variable has the time of assessment listed at the end
# e.g. "baseline_year_1_arm_1", "1_year_follow_up_y_arm_1", "2_year_follow_up_y_arm_1", "3_year_follow_up_y_arm_1"

####################################################################################
#-------------------------- Derive ACEs measures ----------------------------------#
####################################################################################

# We want to measure ACEs at baseline, 1 year follow up, and 2 year follow up assessments
# To predict later CBCL mental health outcomes at 3 year follow up assessment
# Therefore we will derive ACEs across the first three time points

#------------------------- Physical abuse ------------------------------------#
# Note: abuse items are from the ABCD Parent Diagnostic Interview for DSM-5 (KSADS) Traumatic Events
# https://nda.nih.gov/data_structure.html?short_name=abcd_ptsd01

# KSADS PTSD items were measured at baseline and 2 year follow up
# So we will derive it across these two time points

# ksads_ptsd_raw_762_p = Shot, stabbed, or beaten brutally by a grown up in the home
# ksads_ptsd_raw_763_p = Beaten to the point of having bruises by a grown up in the home

# Check prevalence of physical abuse per assessment 
table(abcd_wide$ksads_ptsd_raw_762_p.baseline_year_1_arm_1, useNA = "always") # baseline
table(abcd_wide$ksads_ptsd_raw_762_p.1_year_follow_up_y_arm_1, useNA = "always") # 1 year follow up
table(abcd_wide$ksads_ptsd_raw_762_p.2_year_follow_up_y_arm_1, useNA = "always") # 2 year follow up

table(abcd_wide$ksads_ptsd_raw_763_p.baseline_year_1_arm_1, useNA = "always") # baseline
table(abcd_wide$ksads_ptsd_raw_763_p.1_year_follow_up_y_arm_1, useNA = "always") # 1 year follow up
table(abcd_wide$ksads_ptsd_raw_763_p.2_year_follow_up_y_arm_1, useNA = "always") # 2 year follow up

# If physical abuse was present for at least 1 assessment, code as 1 for physical abuse
abcd_wide$physical_abuse <- 0
abcd_wide$physical_abuse[abcd_wide$ksads_ptsd_raw_762_p.baseline_year_1_arm_1==1 | abcd_wide$ksads_ptsd_raw_762_p.2_year_follow_up_y_arm_1==1 | 
                           abcd_wide$ksads_ptsd_raw_763_p.baseline_year_1_arm_1==1 | abcd_wide$ksads_ptsd_raw_763_p.2_year_follow_up_y_arm_1==1] <- 1
abcd_wide$physical_abuse[apply(apply(abcd_wide[,c("ksads_ptsd_raw_762_p.baseline_year_1_arm_1", 
                                                  "ksads_ptsd_raw_762_p.2_year_follow_up_y_arm_1", 
                                                  "ksads_ptsd_raw_763_p.baseline_year_1_arm_1", 
                                                  "ksads_ptsd_raw_763_p.2_year_follow_up_y_arm_1")],2,is.na),1,sum) > 2] <- NA # code NA if missing > 2/4 items
table(abcd_wide$physical_abuse, useNA = "always")
prop.table(table(abcd_wide$physical_abuse, useNA = "always"))*100 # 1.03%

#------------------------- Emotional abuse ------------------------------------#
# ksads_ptsd_raw_764_p = A non-family member threatened to kill your child
# ksads_ptsd_raw_765_p = A family member threatened to kill your child

# Check prevalence of emotional abuse per assessment 
table(abcd_wide$ksads_ptsd_raw_764_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$ksads_ptsd_raw_764_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$ksads_ptsd_raw_764_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

table(abcd_wide$ksads_ptsd_raw_765_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$ksads_ptsd_raw_765_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$ksads_ptsd_raw_765_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

# If emotional abuse was present for at least 1 assessment, code as 1 for emotional abuse
abcd_wide$emotional_abuse <- 0
abcd_wide$emotional_abuse[abcd_wide$ksads_ptsd_raw_764_p.baseline_year_1_arm_1==1 | abcd_wide$ksads_ptsd_raw_764_p.2_year_follow_up_y_arm_1==1 | 
                            abcd_wide$ksads_ptsd_raw_765_p.baseline_year_1_arm_1==1 | abcd_wide$ksads_ptsd_raw_765_p.2_year_follow_up_y_arm_1==1] <- 1
abcd_wide$emotional_abuse[apply(apply(abcd_wide[,c("ksads_ptsd_raw_764_p.baseline_year_1_arm_1", 
                                                  "ksads_ptsd_raw_764_p.2_year_follow_up_y_arm_1", 
                                                  "ksads_ptsd_raw_765_p.baseline_year_1_arm_1",
                                                  "ksads_ptsd_raw_765_p.2_year_follow_up_y_arm_1")],2,is.na),1,sum) > 2] <- NA # code NA if missing > 2/4 items
table(abcd_wide$emotional_abuse, useNA = "always")
prop.table(table(abcd_wide$emotional_abuse, useNA = "always"))*100 # 1.59%

#------------------------- Sexual abuse ------------------------------------#
# ksads_ptsd_raw_767_p = A grown up in the home touched your child in his or her privates, had your child touch their privates, or did other sexual things to your child
# ksads_ptsd_raw_768_p = An adult outside your family touched your child in his or her privates, had your child touch their privates or did other sexual things to your child
# ksads_ptsd_raw_769_p = A peer forced your child to do something sexually

# Check prevalence of sexual abuse per assessment
table(abcd_wide$ksads_ptsd_raw_767_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$ksads_ptsd_raw_767_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$ksads_ptsd_raw_767_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

table(abcd_wide$ksads_ptsd_raw_768_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$ksads_ptsd_raw_768_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$ksads_ptsd_raw_768_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

table(abcd_wide$ksads_ptsd_raw_769_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$ksads_ptsd_raw_769_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$ksads_ptsd_raw_769_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

# If sexual abuse was present for at least 1 assessment, code as 1 for sexual abuse
abcd_wide$sexual_abuse <- 0
abcd_wide$sexual_abuse[abcd_wide$ksads_ptsd_raw_767_p.baseline_year_1_arm_1==1 | abcd_wide$ksads_ptsd_raw_767_p.2_year_follow_up_y_arm_1==1 | 
                         abcd_wide$ksads_ptsd_raw_768_p.baseline_year_1_arm_1==1 | abcd_wide$ksads_ptsd_raw_768_p.2_year_follow_up_y_arm_1==1 |
                         abcd_wide$ksads_ptsd_raw_769_p.baseline_year_1_arm_1==1 | abcd_wide$ksads_ptsd_raw_769_p.2_year_follow_up_y_arm_1==1] <- 1
abcd_wide$sexual_abuse[apply(apply(abcd_wide[,c("ksads_ptsd_raw_767_p.baseline_year_1_arm_1", 
                                           "ksads_ptsd_raw_767_p.2_year_follow_up_y_arm_1", 
                                           "ksads_ptsd_raw_768_p.baseline_year_1_arm_1",
                                           "ksads_ptsd_raw_768_p.2_year_follow_up_y_arm_1", 
                                           "ksads_ptsd_raw_769_p.baseline_year_1_arm_1", 
                                           "ksads_ptsd_raw_769_p.2_year_follow_up_y_arm_1")],2,is.na),1,sum) > 3] <- NA # code NA if missing > 3/6 items
table(abcd_wide$sexual_abuse, useNA="always")
prop.table(table(abcd_wide$sexual_abuse, useNA="always"))*100 # 2.42%
 
#------------------------- Domestic violence -----------------------------------------#
# ksads_ptsd_raw_766_p = Witness the grownups in the home push, shove or hit one another

# Check prevalence of domestic violence per assessment 
table(abcd_wide$ksads_ptsd_raw_766_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$ksads_ptsd_raw_766_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$ksads_ptsd_raw_766_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

# If domestic violence was present for at least 1 assessment, code as 1 for domestic violence
abcd_wide$domestic_violence <- 0
abcd_wide$domestic_violence[abcd_wide$ksads_ptsd_raw_766_p.baseline_year_1_arm_1==1 | abcd_wide$ksads_ptsd_raw_766_p.2_year_follow_up_y_arm_1==1] <- 1
abcd_wide$domestic_violence[apply(apply(abcd_wide[,c("ksads_ptsd_raw_766_p.baseline_year_1_arm_1", 
                                           "ksads_ptsd_raw_766_p.2_year_follow_up_y_arm_1")],2,is.na),1,sum) > 1] <- NA # code NA if missing > 1/2 items
table(abcd_wide$domestic_violence, useNA="always")
prop.table(table(abcd_wide$domestic_violence, useNA="always"))*100 # 9.68%

#------------------------- Accident requiring medical attention ------------------------------------#
# ksads_ptsd_raw_754_p = A car accident in which your child or another person in the car was hurt bad enough to require medical attention
# ksads_ptsd_raw_755_p = Another significant accident for which your child needed specialized and intensive medical treatment

# Check prevalence of accident per assessment 
table(abcd_wide$ksads_ptsd_raw_754_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$ksads_ptsd_raw_754_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$ksads_ptsd_raw_754_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

table(abcd_wide$ksads_ptsd_raw_755_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$ksads_ptsd_raw_755_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$ksads_ptsd_raw_755_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

# If accident was present for at least 1 assessment, code as 1 for accident
abcd_wide$accident_medical <- 0
abcd_wide$accident_medical[abcd_wide$ksads_ptsd_raw_754_p.baseline_year_1_arm_1==1 | abcd_wide$ksads_ptsd_raw_754_p.2_year_follow_up_y_arm_1==1 | 
                             abcd_wide$ksads_ptsd_raw_755_p.baseline_year_1_arm_1==1 | abcd_wide$ksads_ptsd_raw_755_p.2_year_follow_up_y_arm_1==1] <- 1
abcd_wide$accident_medical[apply(apply(abcd_wide[,c("ksads_ptsd_raw_754_p.baseline_year_1_arm_1", 
                                                  "ksads_ptsd_raw_754_p.2_year_follow_up_y_arm_1", 
                                                  "ksads_ptsd_raw_755_p.baseline_year_1_arm_1",
                                                  "ksads_ptsd_raw_755_p.2_year_follow_up_y_arm_1")],2,is.na),1,sum) > 2] <- NA # code NA if missing > 2/4 items
table(abcd_wide$accident_medical, useNA = "always")
prop.table(table(abcd_wide$accident_medical, useNA = "always"))*100 # 11.3%

#------------------------- Natural disaster ------------------------------------#
# ksads_ptsd_raw_756_p = Witnessed or caught in a fire that caused significant property damage or personal injury
# ksads_ptsd_raw_757_p = Witnessed or caught in a natural disaster that caused significant property damage or personal injury

# Check prevalence of natural disaster per assessment 
table(abcd_wide$ksads_ptsd_raw_756_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$ksads_ptsd_raw_756_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$ksads_ptsd_raw_756_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

table(abcd_wide$ksads_ptsd_raw_757_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$ksads_ptsd_raw_757_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$ksads_ptsd_raw_757_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

# If natural disaster was present for at least 1 assessment, code as 1 for natural disaster
abcd_wide$natural_disaster <- 0
abcd_wide$natural_disaster[abcd_wide$ksads_ptsd_raw_756_p.baseline_year_1_arm_1==1 | abcd_wide$ksads_ptsd_raw_756_p.2_year_follow_up_y_arm_1==1 | 
                             abcd_wide$ksads_ptsd_raw_757_p.baseline_year_1_arm_1==1 | abcd_wide$ksads_ptsd_raw_757_p.2_year_follow_up_y_arm_1==1] <- 1
abcd_wide$natural_disaster[apply(apply(abcd_wide[,c("ksads_ptsd_raw_756_p.baseline_year_1_arm_1", 
                                                  "ksads_ptsd_raw_756_p.2_year_follow_up_y_arm_1", 
                                                  "ksads_ptsd_raw_757_p.baseline_year_1_arm_1",
                                                  "ksads_ptsd_raw_757_p.2_year_follow_up_y_arm_1")],2,is.na),1,sum) > 2] <- NA # code NA if missing > 2/4 items
table(abcd_wide$natural_disaster, useNA = "always")
prop.table(table(abcd_wide$natural_disaster, useNA = "always"))*100 # 6.35%

#------------------------- Community violence ------------------------------------#
# ksads_ptsd_raw_758_p = Witnessed or present during an act of terrorism (e.g., Boston marathon bombing)
# ksads_ptsd_raw_759_p = Witnessed death or mass destruction in a war zone
# ksads_ptsd_raw_760_p = Witnessed someone shot or stabbed in the community

# Check prevalence of community violence per assessment 
table(abcd_wide$ksads_ptsd_raw_758_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$ksads_ptsd_raw_758_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$ksads_ptsd_raw_758_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

table(abcd_wide$ksads_ptsd_raw_759_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$ksads_ptsd_raw_759_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$ksads_ptsd_raw_759_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

table(abcd_wide$ksads_ptsd_raw_760_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$ksads_ptsd_raw_760_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$ksads_ptsd_raw_760_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

# If community violence was present for at least 1 assessment, code as 1 for community violence
abcd_wide$community_violence <- 0
abcd_wide$community_violence[abcd_wide$ksads_ptsd_raw_758_p.baseline_year_1_arm_1==1 | abcd_wide$ksads_ptsd_raw_758_p.2_year_follow_up_y_arm_1==1 | 
                               abcd_wide$ksads_ptsd_raw_759_p.baseline_year_1_arm_1==1 | abcd_wide$ksads_ptsd_raw_759_p.2_year_follow_up_y_arm_1==1 | 
                               abcd_wide$ksads_ptsd_raw_760_p.baseline_year_1_arm_1==1 | abcd_wide$ksads_ptsd_raw_760_p.2_year_follow_up_y_arm_1==1] <- 1
abcd_wide$community_violence[apply(apply(abcd_wide[,c("ksads_ptsd_raw_758_p.baseline_year_1_arm_1", 
                                           "ksads_ptsd_raw_758_p.2_year_follow_up_y_arm_1", 
                                           "ksads_ptsd_raw_759_p.baseline_year_1_arm_1",
                                           "ksads_ptsd_raw_759_p.2_year_follow_up_y_arm_1", 
                                           "ksads_ptsd_raw_760_p.baseline_year_1_arm_1", 
                                           "ksads_ptsd_raw_760_p.2_year_follow_up_y_arm_1")],2,is.na),1,sum) > 3] <- NA # code NA if missing > 3/6 items
table(abcd_wide$community_violence, useNA="always")
prop.table(table(abcd_wide$community_violence, useNA="always"))*100 # 1.57%

#------------------------- Bereavement ------------------------------------#
# ksads_ptsd_raw_770_p = Learned about the sudden unexpected death of a loved one

# Check prevalence of bereavement per assessment 
table(abcd_wide$ksads_ptsd_raw_770_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$ksads_ptsd_raw_770_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$ksads_ptsd_raw_770_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

# If bereavement was present for at least 1 assessment, code as 1 for bereavement
abcd_wide$bereavement <- 0
abcd_wide$bereavement[abcd_wide$ksads_ptsd_raw_770_p.baseline_year_1_arm_1==1 | abcd_wide$ksads_ptsd_raw_770_p.2_year_follow_up_y_arm_1==1] <- 1
abcd_wide$bereavement[apply(apply(abcd_wide[,c("ksads_ptsd_raw_770_p.baseline_year_1_arm_1", 
                                                  "ksads_ptsd_raw_770_p.2_year_follow_up_y_arm_1")],2,is.na),1,sum) > 1] <- NA # code NA if missing > 1/2 items
table(abcd_wide$bereavement, useNA = "always")
prop.table(table(abcd_wide$bereavement, useNA = "always"))*100 # 32.6%

#------------------------- Emotional neglect -----------------------------------------#
# Note: emotional neglect items are from the ABCD Children's Report of Parental Behavioral Inventory
# https://nda.nih.gov/data_structure.html?short_name=crpbi01

# crpbi_parent1_y = First caregiver makes me feel better after talking over my worries with him/her
# crpbi_parent2_y = First caregiver smiles at me very often
# crpbi_parent3_y = First caregiver is able to make me feel better when I am upset
# crpbi_parent4_y = First caregiver believes in showing his/her love for me
# crpbi_parent5_y = First caregiver is easy to talk to

# Check prevalence of emotional neglect per assessment 
table(abcd_wide$crpbi_parent1_y.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$crpbi_parent1_y.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$crpbi_parent1_y.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

# Emotional neglect was measured at baseline and 1 year follow up
# So we will derive it across the first two time points

# Convert emotional neglect items to numeric
abcd_wide <- abcd_wide %>% 
  mutate_at(c("crpbi_parent1_y.baseline_year_1_arm_1","crpbi_parent1_y.1_year_follow_up_y_arm_1",
              "crpbi_parent2_y.baseline_year_1_arm_1", "crpbi_parent2_y.1_year_follow_up_y_arm_1",
              "crpbi_parent3_y.baseline_year_1_arm_1", "crpbi_parent3_y.1_year_follow_up_y_arm_1",
              "crpbi_parent4_y.baseline_year_1_arm_1", "crpbi_parent4_y.1_year_follow_up_y_arm_1",
              "crpbi_parent5_y.baseline_year_1_arm_1", "crpbi_parent5_y.1_year_follow_up_y_arm_1"), 
            function(x) as.numeric(as.character(x)))

# Items were originally coded as: 1 = Not like him/her; 2 = Somewhat like him/her; 3 = A lot like him/her
# Code each individual item so a score of 1 indicates neglect
# then if the child reports 2 or more items, code as overall neglect

# First caregiver makes me feel better after talking over my worries with him/her
table(abcd_wide$crpbi_parent1_y.baseline_year_1_arm_1, useNA="always")
abcd_wide$crpbi_parent1_y.baseline_year_1_arm_1 <- ifelse(abcd_wide$crpbi_parent1_y.baseline_year_1_arm_1==1, 1, 0) 
table(abcd_wide$crpbi_parent1_y.baseline_year_1_arm_1, useNA="always")

table(abcd_wide$crpbi_parent1_y.1_year_follow_up_y_arm_1, useNA="always")
abcd_wide$crpbi_parent1_y.1_year_follow_up_y_arm_1 <- ifelse(abcd_wide$crpbi_parent1_y.1_year_follow_up_y_arm_1==1, 1, 0) 
table(abcd_wide$crpbi_parent1_y.1_year_follow_up_y_arm_1, useNA="always")

# First caregiver smiles at me very often
table(abcd_wide$crpbi_parent2_y.baseline_year_1_arm_1, useNA="always")
abcd_wide$crpbi_parent2_y.baseline_year_1_arm_1 <- ifelse(abcd_wide$crpbi_parent2_y.baseline_year_1_arm_1==1, 1, 0) 
table(abcd_wide$crpbi_parent2_y.baseline_year_1_arm_1, useNA="always")

table(abcd_wide$crpbi_parent2_y.1_year_follow_up_y_arm_1, useNA="always")
abcd_wide$crpbi_parent2_y.1_year_follow_up_y_arm_1 <- ifelse(abcd_wide$crpbi_parent2_y.1_year_follow_up_y_arm_1==1, 1, 0) 
table(abcd_wide$crpbi_parent2_y.1_year_follow_up_y_arm_1, useNA="always")

# First caregiver is able to make me feel better when I am upset
table(abcd_wide$crpbi_parent3_y.baseline_year_1_arm_1, useNA="always")
abcd_wide$crpbi_parent3_y.baseline_year_1_arm_1 <- ifelse(abcd_wide$crpbi_parent3_y.baseline_year_1_arm_1==1, 1, 0) 
table(abcd_wide$crpbi_parent3_y.baseline_year_1_arm_1, useNA="always")

table(abcd_wide$crpbi_parent3_y.1_year_follow_up_y_arm_1, useNA="always")
abcd_wide$crpbi_parent3_y.1_year_follow_up_y_arm_1 <- ifelse(abcd_wide$crpbi_parent3_y.1_year_follow_up_y_arm_1==1, 1, 0) 
table(abcd_wide$crpbi_parent3_y.1_year_follow_up_y_arm_1, useNA="always")

# First caregiver believes in showing his/her love for me
table(abcd_wide$crpbi_parent4_y.baseline_year_1_arm_1, useNA="always")
abcd_wide$crpbi_parent4_y.baseline_year_1_arm_1 <- ifelse(abcd_wide$crpbi_parent4_y.baseline_year_1_arm_1==1, 1, 0) 
table(abcd_wide$crpbi_parent4_y.baseline_year_1_arm_1, useNA="always")

table(abcd_wide$crpbi_parent4_y.1_year_follow_up_y_arm_1, useNA="always")
abcd_wide$crpbi_parent4_y.1_year_follow_up_y_arm_1 <- ifelse(abcd_wide$crpbi_parent4_y.1_year_follow_up_y_arm_1==1, 1, 0) 
table(abcd_wide$crpbi_parent4_y.1_year_follow_up_y_arm_1, useNA="always")

# First caregiver is easy to talk to
table(abcd_wide$crpbi_parent5_y.baseline_year_1_arm_1, useNA="always")
abcd_wide$crpbi_parent5_y.baseline_year_1_arm_1 <- ifelse(abcd_wide$crpbi_parent5_y.baseline_year_1_arm_1==1, 1, 0) 
table(abcd_wide$crpbi_parent5_y.baseline_year_1_arm_1, useNA="always")

table(abcd_wide$crpbi_parent5_y.1_year_follow_up_y_arm_1, useNA="always")
abcd_wide$crpbi_parent5_y.1_year_follow_up_y_arm_1 <- ifelse(abcd_wide$crpbi_parent5_y.1_year_follow_up_y_arm_1==1, 1, 0) 
table(abcd_wide$crpbi_parent5_y.1_year_follow_up_y_arm_1, useNA="always")

# Generate overall emotional neglect sum score 
abcd_wide$emotional_neglect_sum <- NA
abcd_wide$emotional_neglect_sum <- apply(abcd_wide[,c("crpbi_parent1_y.baseline_year_1_arm_1","crpbi_parent1_y.1_year_follow_up_y_arm_1",
              "crpbi_parent2_y.baseline_year_1_arm_1", "crpbi_parent2_y.1_year_follow_up_y_arm_1",
              "crpbi_parent3_y.baseline_year_1_arm_1", "crpbi_parent3_y.1_year_follow_up_y_arm_1",
              "crpbi_parent4_y.baseline_year_1_arm_1", "crpbi_parent4_y.1_year_follow_up_y_arm_1",
              "crpbi_parent5_y.baseline_year_1_arm_1", "crpbi_parent5_y.1_year_follow_up_y_arm_1") ], 1, sum, na.rm=T)
abcd_wide$emotional_neglect_sum[apply(apply(abcd_wide[,c("crpbi_parent1_y.baseline_year_1_arm_1","crpbi_parent1_y.1_year_follow_up_y_arm_1",
              "crpbi_parent2_y.baseline_year_1_arm_1", "crpbi_parent2_y.1_year_follow_up_y_arm_1",
              "crpbi_parent3_y.baseline_year_1_arm_1", "crpbi_parent3_y.1_year_follow_up_y_arm_1",
              "crpbi_parent4_y.baseline_year_1_arm_1", "crpbi_parent4_y.1_year_follow_up_y_arm_1",
              "crpbi_parent5_y.baseline_year_1_arm_1", "crpbi_parent5_y.1_year_follow_up_y_arm_1")],2,is.na),1,sum) > 5] <- NA # code NA if missing > 5/10 items
table(abcd_wide$emotional_neglect_sum, useNA = "always") 

# Derive emotional neglect variable
abcd_wide$emotional_neglect <- 0
abcd_wide$emotional_neglect[abcd_wide$emotional_neglect_sum >=2] <- 1 # A score of 2 or more indicates emotional neglect
abcd_wide$emotional_neglect[abcd_wide$emotional_neglect_sum == NA] <- NA
table(abcd_wide$emotional_neglect, useNA = "always")
prop.table(table(abcd_wide$emotional_neglect, useNA = "always"))*100 # 3.65%

#------------------------- Parental psychopathology -----------------------------------------#
## Adult Self-Report (ASR) t-score scales:
# asr_scr_depress_t = Depressive Problems ASR DSM-5-Oriented Scale (t-score)
# asr_scr_anxdisord_t = Anxiety Problems ASR DSM-5-Oriented Scale (t-score)
# asr_scr_adhd_t = AD/H Problems ASR DSM-5-Oriented Scale (t-score)

## Family History variables:
# fam_history_6_yes_no - Has ANY blood relative of your child ever suffered from depression?
# fam_history_7_yes_no - Has ANY blood relative of your child ever had a period of time when others were concerned because they suddenly became more active day and night and seemed not to need any sleep and talked much more than usual for them?
# fam_history_8_yes_no - Has ANY blood relative of your child ever had a period lasting six months when they saw visions or heard voices or thought people were spying on them or plotting against them? 
# fam_history_13_yes_no - Has ANY blood relative of your child ever attempted or committed suicide? 

## Family history specifiers - relating to mother or father
# fam_history_q6a_depression - biological father - depression
# fam_history_q6d_depression - biological mother - depression
# fam_history_q7a_mania - biological father - mania
# fam_history_q7d_mania - biological mother - mania 
# fam_history_q8a_visions - biological father - visions
# fam_history_q8d_visions - biological mother - visions
# fam_history_q13a_suicide - biological father - suicide
# fam_history_q13d_suicide - biological mother - suicide

# Check prevalence of parental psychopathology per assessment
table(abcd_wide$asr_scr_depress_t.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$asr_scr_depress_t.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$asr_scr_depress_t.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

table(abcd_wide$fam_history_6_yes_no.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$fam_history_6_yes_no.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$fam_history_6_yes_no.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

# ASR was measured at baseline and 2 year follow up
# Family history was measured only at baseline

# Code ASR variables to numeric
asr_vars <- c("asr_scr_depress_t.baseline_year_1_arm_1", "asr_scr_depress_t.2_year_follow_up_y_arm_1", 
              "asr_scr_anxdisord_t.baseline_year_1_arm_1", "asr_scr_anxdisord_t.2_year_follow_up_y_arm_1",
              "asr_scr_adhd_t.baseline_year_1_arm_1", "asr_scr_adhd_t.2_year_follow_up_y_arm_1") 
class(asr_vars)
abcd_wide[,asr_vars] <- lapply(abcd_wide[,asr_vars], function(x) as.numeric(as.character(x)))

# Generate clinical cut-off scores for ASR variables
# Scores > 63 on the ASR scales are considered to be in the clinical range (Achenbach & Rescorla, 2003)

# asr_scr_depress_t = Depressive Problems ASR DSM-5-Oriented Scale (t-score)
abcd_wide$asr_depress_baseline_cutoff <- ifelse(abcd_wide$asr_scr_depress_t.baseline_year_1_arm_1 > 63, 1, 0)
abcd_wide$asr_depress_2_year_cutoff <- ifelse(abcd_wide$asr_scr_depress_t.2_year_follow_up_y_arm_1 > 63, 1, 0)

# asr_scr_anxdisord_t = Anxiety Problems ASR DSM-5-Oriented Scale (t-score)
abcd_wide$asr_anx_baseline_cutoff <- ifelse(abcd_wide$asr_scr_anxdisord_t.baseline_year_1_arm_1 > 63, 1, 0)
abcd_wide$asr_anx_2_year_cutoff <- ifelse(abcd_wide$asr_scr_anxdisord_t.2_year_follow_up_y_arm_1 > 63, 1, 0)

# asr_scr_adhd_t = ADHD Problems ASR DSM-5-Oriented Scale (t-score)
abcd_wide$asr_adhd_baseline_cutoff <- ifelse(abcd_wide$asr_scr_adhd_t.baseline_year_1_arm_1 > 63, 1, 0)
abcd_wide$asr_adhd_2_year_cutoff <- ifelse(abcd_wide$asr_scr_adhd_t.2_year_follow_up_y_arm_1 > 63, 1, 0)

# Recode family history variables to missing if values are 999 ("Do not know") or 7 ("Refuse to answer")
abcd_wide$fam_history_6_yes_no.baseline_year_1_arm_1[abcd_wide$fam_history_6_yes_no.baseline_year_1_arm_1=="999" | abcd_wide$fam_history_6_yes_no.baseline_year_1_arm_1=="7"] <- NA
abcd_wide$fam_history_7_yes_no.baseline_year_1_arm_1[abcd_wide$fam_history_7_yes_no.baseline_year_1_arm_1=="999" | abcd_wide$fam_history_7_yes_no.baseline_year_1_arm_1=="7"] <- NA
abcd_wide$fam_history_8_yes_no.baseline_year_1_arm_1[abcd_wide$fam_history_8_yes_no.baseline_year_1_arm_1=="999" | abcd_wide$fam_history_8_yes_no.baseline_year_1_arm_1=="7"] <- NA
abcd_wide$fam_history_13_yes_no.baseline_year_1_arm_1[abcd_wide$fam_history_13_yes_no.baseline_year_1_arm_1=="999" | abcd_wide$fam_history_13_yes_no.baseline_year_1_arm_1=="7"] <- NA

# Derive parental psychopathology composite variable
abcd_wide$parental_psychopathology <- 0
abcd_wide$parental_psychopathology[
    # Code as exposed if biological mother or father has depression, manic episode, psychotic experiences or suicide attempt
    # a = biological father, d = biological mother
    abcd_wide$fam_history_6_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q6a_depression.baseline_year_1_arm_1==1 |  
    abcd_wide$fam_history_6_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q6d_depression.baseline_year_1_arm_1==1 | 
    abcd_wide$fam_history_7_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q7a_mania.baseline_year_1_arm_1==1 | 
    abcd_wide$fam_history_7_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q7d_mania.baseline_year_1_arm_1==1 | 
    abcd_wide$fam_history_8_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q8a_visions.baseline_year_1_arm_1==1 | 
    abcd_wide$fam_history_8_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q8d_visions.baseline_year_1_arm_1==1 |
    abcd_wide$fam_history_13_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q13a_suicide.baseline_year_1_arm_1==1 | 
    abcd_wide$fam_history_13_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q13d_suicide.baseline_year_1_arm_1==1 |
    # Code as exposed if parent has ASR score above cut-off (63) for depression, anxiety, or ADHD
    abcd_wide$asr_depress_baseline_cutoff==1 | abcd_wide$asr_depress_2_year_cutoff==1 |
    abcd_wide$asr_anx_baseline_cutoff == 1 | abcd_wide$asr_anx_2_year_cutoff==1 |
    abcd_wide$asr_adhd_baseline_cutoff == 1 | abcd_wide$asr_adhd_2_year_cutoff==1] <- 1
abcd_wide$parental_psychopathology[apply(apply(abcd_wide[,c("fam_history_6_yes_no.baseline_year_1_arm_1", "fam_history_7_yes_no.baseline_year_1_arm_1", 
                                      "fam_history_8_yes_no.baseline_year_1_arm_1", "fam_history_13_yes_no.baseline_year_1_arm_1", 
                                      "asr_depress_baseline_cutoff", "asr_depress_2_year_cutoff", "asr_anx_baseline_cutoff", "asr_anx_2_year_cutoff", 
                                      "asr_adhd_baseline_cutoff", "asr_adhd_2_year_cutoff")],2,is.na),1,sum) > 5] <- NA # code to missing if missing >5/10 items
table(abcd_wide$parental_psychopathology, useNA="always")
prop.table(table(abcd_wide$parental_psychopathology, useNA="always"))*100 # 39.9%

#------------------------- Parental alcohol abuse -----------------------------------------#
# famhx_4_p = Has ANY blood relative of your child ever had any problems due to alcohol?

# Check prevalence of parental alcohol abuse per assessment
table(abcd_wide$famhx_4_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$famhx_4_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$famhx_4_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

# Family history of parental alcohol abuse was measured only at baseline

# Recode family history variables to missing if values are 999 ("Do not know") or 7 ("Refuse to answer")
abcd_wide$famhx_4_p.baseline_year_1_arm_1[abcd_wide$famhx_4_p.baseline_year_1_arm_1=="7" | abcd_wide$famhx_4_p.baseline_year_1_arm_1=="999"] <- NA 
table(abcd_wide$famhx_4_p.baseline_year_1_arm_1, useNA="always")

# Derive parental alcohol abuse composite variable
abcd_wide$parental_alcohol_abuse <- 0
abcd_wide$parental_alcohol_abuse[# biological father had problems due to alcohol
                          abcd_wide$famhx_4_p.baseline_year_1_arm_1==1 & abcd_wide$famhx4a_p___1.baseline_year_1_arm_1==1 | # marital problems
                          abcd_wide$famhx_4_p.baseline_year_1_arm_1==1 & abcd_wide$famhx4a_p___2.baseline_year_1_arm_1==1 | # work problems
                          abcd_wide$famhx_4_p.baseline_year_1_arm_1==1 & abcd_wide$famhx4a_p___3.baseline_year_1_arm_1==1 | # arrests/DUI
                          abcd_wide$famhx_4_p.baseline_year_1_arm_1==1 & abcd_wide$famhx4a_p___4.baseline_year_1_arm_1==1 | # alcohol treatment programme
                          abcd_wide$famhx_4_p.baseline_year_1_arm_1==1 & abcd_wide$famhx4a_p___6.baseline_year_1_arm_1==1 | # isolated self, arguments, drunk a lot 
                          abcd_wide$famhx_4_p.baseline_year_1_arm_1==1 & abcd_wide$famhx4a_p___7.baseline_year_1_arm_1==1 | # health problems
                          # biological mother had problems due to alcohol
                          abcd_wide$famhx_4_p.baseline_year_1_arm_1==1 & abcd_wide$famhx_4d_p___1.baseline_year_1_arm_1==1 | # marital problems
                          abcd_wide$famhx_4_p.baseline_year_1_arm_1==1 & abcd_wide$famhx_4d_p___2.baseline_year_1_arm_1==1 | # work problems
                          abcd_wide$famhx_4_p.baseline_year_1_arm_1==1 & abcd_wide$famhx_4d_p___3.baseline_year_1_arm_1==1 | # arrests/DUI
                          abcd_wide$famhx_4_p.baseline_year_1_arm_1==1 & abcd_wide$famhx_4d_p___4.baseline_year_1_arm_1==1 | # alcohol treatment programme
                          abcd_wide$famhx_4_p.baseline_year_1_arm_1==1 & abcd_wide$famhx_4d_p___6.baseline_year_1_arm_1==1 | # isolated self, arguments, drunk a lot 
                          abcd_wide$famhx_4_p.baseline_year_1_arm_1==1 & abcd_wide$famhx_4d_p___7.baseline_year_1_arm_1==1 ] <- 1 # health problems
table(abcd_wide$parental_alcohol_abuse, useNA = "always")
prop.table(table(abcd_wide$parental_alcohol_abuse, useNA = "always"))*100 # 14.5%

#------------------------- Parental drug abuse -----------------------------------------#
# asr_q126_p = In the past 6 months, on how many days did you use drugs for nonmedical purposes (including marijuana, cocaine, and other drugs, except alcohol and nicotine)? 
# fam_history_5_yes_no = Has ANY blood relative of your child ever had any problems due to drugs?

# Check prevalence of parental drug abuse per assessment
table(abcd_wide$asr_q126_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$asr_q126_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$asr_q126_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

table(abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1, useNA = "always") # baseline
table(abcd_wide$fam_history_5_yes_no.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$fam_history_5_yes_no.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

# ASR was measured at baseline and 2 year follow up
# Family history of parental drug abuse was measured only at baseline

# Code ASR variables to numeric
abcd_wide$asr_q126_p.baseline_year_1_arm_1 <- as.numeric(abcd_wide$asr_q126_p.baseline_year_1_arm_1)
abcd_wide$asr_q126_p.2_year_follow_up_y_arm_1 <- as.numeric(abcd_wide$asr_q126_p.2_year_follow_up_y_arm_1)

# Recode family history variables to missing if values are 999 ("Do not know") or 7 ("Refuse to answer")
abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1[abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1=="7" | abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1=="999"] <- NA
table(abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1, useNA="always")

# Assess number who used drugs multiple times per week for past 6 months: 
# 26 weeks equates to 6 months. So multiple times per week for 6 months would be >=52 times (on average 2 or more times per week)
# Baseline
psych::describe(abcd_wide$asr_q126_p.baseline_year_1_arm_1[abcd_wide$asr_q126_p.baseline_year_1_arm_1>=52]) # 131 reported that
# 2 year follow up
psych::describe(abcd_wide$asr_q126_p.2_year_follow_up_y_arm_1[abcd_wide$asr_q126_p.2_year_follow_up_y_arm_1>=52]) # 98 reported that

# Derive parental drug abuse composite variable
abcd_wide$parental_drug_abuse <- 0
abcd_wide$parental_drug_abuse[abcd_wide$asr_q126_p.baseline_year_1_arm_1>=52 | # used drugs multiple times weekly in past 6 months at baseline
                              abcd_wide$asr_q126_p.2_year_follow_up_y_arm_1>=52 | # used drugs multiple times weekly in past 6 months at 2 year follow up
                          # biological father had problems due to drugs
                          abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q5a_drugs___1.baseline_year_1_arm_1==1 | # marital problems
                          abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q5a_drugs___2.baseline_year_1_arm_1==1 | # work problems
                          abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q5a_drugs___3.baseline_year_1_arm_1==1 | # arrests/DUI
                          abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q5a_drugs___4.baseline_year_1_arm_1==1 | # drug treatment programme
                          abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q5a_drugs___6.baseline_year_1_arm_1==1 | # isolated self, arguments, high a lot 
                          abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q5a_drugs___7.baseline_year_1_arm_1==1 | # health problems
                          # biological mother had problems due to drugs
                          abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q5d_drugs___1.baseline_year_1_arm_1==1 | # marital problems
                          abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q5d_drugs___2.baseline_year_1_arm_1==1 | # work problems
                          abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q5d_drugs___3.baseline_year_1_arm_1==1 | # arrests/DUI
                          abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q5d_drugs___4.baseline_year_1_arm_1==1 | # drug treatment programme
                          abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q5d_drugs___6.baseline_year_1_arm_1==1 | # isolated self, arguments, high a lot 
                          abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q5d_drugs___7.baseline_year_1_arm_1==1] <- 1
abcd_wide$parental_drug_abuse[apply(apply(abcd_wide[,c("asr_q126_p.baseline_year_1_arm_1", "asr_q126_p.2_year_follow_up_y_arm_1", 
                                                  "fam_history_5_yes_no.baseline_year_1_arm_1")],2,is.na),1,sum) > 1] <- NA # code NA if missing > 1/3 items
table(abcd_wide$parental_drug_abuse, useNA = "always")
prop.table(table(abcd_wide$parental_drug_abuse, useNA = "always"))*100 # 11.4%

#------------------------- Parental criminality -----------------------------------------#
# ple_arrest_p = Someone in the family was arrested?
# ple_arrest_past_yr_p = Did this happen in the past year? 
# ple_law_p = Parents/caregiver got into trouble with the law?
# ple_law_past_yr_p = Did this happen in the past year? 
# ple_jail_p = One of the parents/caregivers went to jail?
# ple_jail_past_yr_p = Did this happen in the past year? 

# Check prevalence of parental criminality per assessment
table(abcd_wide$ple_arrest_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$ple_arrest_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$ple_arrest_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

# Parental criminality was measured at 1 year and 2 year follow up

# Derive parental criminality composite variable
abcd_wide$parental_criminality <- 0
abcd_wide$parental_criminality[abcd_wide$ple_arrest_p.1_year_follow_up_y_arm_1==1 | abcd_wide$ple_arrest_past_yr_p.1_year_follow_up_y_arm_1==1 | 
                               abcd_wide$ple_arrest_p.2_year_follow_up_y_arm_1==1 | abcd_wide$ple_arrest_past_yr_p.2_year_follow_up_y_arm_1==1 | 
                               abcd_wide$ple_law_p.1_year_follow_up_y_arm_1==1 | abcd_wide$ple_law_past_yr_p.1_year_follow_up_y_arm_1==1 | 
                               abcd_wide$ple_law_p.2_year_follow_up_y_arm_1==1 | abcd_wide$ple_law_past_yr_p.2_year_follow_up_y_arm_1==1 | 
                               abcd_wide$ple_jail_p.1_year_follow_up_y_arm_1==1 | abcd_wide$ple_jail_past_yr_p.1_year_follow_up_y_arm_1==1 | 
                               abcd_wide$ple_jail_p.2_year_follow_up_y_arm_1==1 | abcd_wide$ple_jail_past_yr_p.2_year_follow_up_y_arm_1==1] <- 1 
abcd_wide$parental_criminality[apply(apply(abcd_wide[,c("ple_arrest_p.1_year_follow_up_y_arm_1", "ple_arrest_p.2_year_follow_up_y_arm_1", 
                                                      "ple_law_p.1_year_follow_up_y_arm_1", "ple_law_p.2_year_follow_up_y_arm_1", 
                                                      "ple_jail_p.1_year_follow_up_y_arm_1", "ple_jail_p.2_year_follow_up_y_arm_1")],2,is.na),1,sum) > 3] <- NA # code NA if missing > 3/6 items
table(abcd_wide$parental_criminality, useNA="always")
prop.table(table(abcd_wide$parental_criminality, useNA="always"))*100 # 9.65%

#------------------------- Parental separation -----------------------------------------#
# demo_prnt_marital_v2 = Are you now married, widowed, divorced, separated, never married or living with a partner? 
# demo_prnt_marital_v2_l = Same question from follow up longitudinal parent demographics survey
# ple_separ_p = Parents separated or divorced? 
# ple_separ_past_yr_p = Did this happen in the past year?

# Check prevalence of parental separation per assessment
table(abcd_wide$demo_prnt_marital_v2.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$demo_prnt_marital_v2.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$demo_prnt_marital_v2.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

table(abcd_wide$demo_prnt_marital_v2_l.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$demo_prnt_marital_v2_l.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$demo_prnt_marital_v2_l.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

table(abcd_wide$ple_separ_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$ple_separ_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$ple_separ_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

# Parent demographics survey was measured only at baseline
# Longitudinal parent demographics survey was measured at 1 year and 2 year follow up 
# Parent life events was measured at 1 year and 2 year follow up

# Recode parent demographic variables to missing if values are 777 = Refused to answer 

# Are you now married, widowed, divorced, separated, never married or living with a partner? 
# 1 = Married; 2 = Widowed; 3 = Divorced; 4 = Separated; 5 = Never married; 6 = Living with partner
abcd_wide$demo_prnt_marital_v2.baseline_year_1_arm_1[abcd_wide$demo_prnt_marital_v2.baseline_year_1_arm_1=="777"] <- NA
table(abcd_wide$demo_prnt_marital_v2.baseline_year_1_arm_1, useNA="always")

abcd_wide$demo_prnt_marital_v2_l.1_year_follow_up_y_arm_1[abcd_wide$demo_prnt_marital_v2_l.1_year_follow_up_y_arm_1=="777"] <- NA
table(abcd_wide$demo_prnt_marital_v2_l.1_year_follow_up_y_arm_1, useNA = "always")

abcd_wide$demo_prnt_marital_v2_l.2_year_follow_up_y_arm_1[abcd_wide$demo_prnt_marital_v2_l.2_year_follow_up_y_arm_1=="777"] <- NA
table(abcd_wide$demo_prnt_marital_v2_l.2_year_follow_up_y_arm_1, useNA = "always")

# Parents separated or divorced? (1 = Yes; 0 = No)
table(abcd_wide$ple_separ_p.1_year_follow_up_y_arm_1, useNA="always")
table(abcd_wide$ple_separ_p.2_year_follow_up_y_arm_1, useNA="always")

# Did this happen in the past year? (1 = Yes; 0 = No)
table(abcd_wide$ple_separ_past_yr_p.1_year_follow_up_y_arm_1, useNA="always")
table(abcd_wide$ple_separ_past_yr_p.2_year_follow_up_y_arm_1, useNA="always")

# Derive parental separation composite variable
abcd_wide$parental_separation <- 0
abcd_wide$parental_separation[abcd_wide$demo_prnt_marital_v2.baseline_year_1_arm_1==3 | # parent divorced at baseline
                                abcd_wide$demo_prnt_marital_v2.baseline_year_1_arm_1==4 | # parent separated at baseline
                                abcd_wide$demo_prnt_marital_v2_l.1_year_follow_up_y_arm_1==3 | # parent divorced at 1 year follow up
                                abcd_wide$demo_prnt_marital_v2_l.1_year_follow_up_y_arm_1==4 | # parent separated at 1 year follow up
                                abcd_wide$demo_prnt_marital_v2_l.2_year_follow_up_y_arm_1==3 | # parent divorced at 2 year follow up
                                abcd_wide$demo_prnt_marital_v2_l.2_year_follow_up_y_arm_1==4 | # parent separated at 2 year follow up
                                abcd_wide$ple_separ_p.1_year_follow_up_y_arm_1==1 | # parents separated/divorced at 1 year follow up
                                abcd_wide$ple_separ_past_yr_p.1_year_follow_up_y_arm_1==1 | # or this happened in the past year 
                                abcd_wide$ple_separ_p.2_year_follow_up_y_arm_1==1 | # parents separated/divorced at 2 year follow up
                                abcd_wide$ple_separ_past_yr_p.2_year_follow_up_y_arm_1==1] <- 1 # or this happened in the past year 
abcd_wide$parental_separation[apply(apply(abcd_wide[,c("demo_prnt_marital_v2.baseline_year_1_arm_1", 
                                                       "demo_prnt_marital_v2_l.1_year_follow_up_y_arm_1",
                                                       "demo_prnt_marital_v2_l.2_year_follow_up_y_arm_1",
                                                       "ple_separ_p.1_year_follow_up_y_arm_1",
                                                       "ple_separ_p.2_year_follow_up_y_arm_1")],2,is.na),1,sum) > 2] <- NA # code NA if missing > 2/5 items
table(abcd_wide$parental_separation, useNA="always")
prop.table(table(abcd_wide$parental_separation, useNA="always"))*100 # 23.2%

#------------------------- Peer victimisation -----------------------------------------#
# peq_left_out_vic = kids left me out of activity/conversation
# peq_chase_vic = kid chased me like trying to hurt me
# peq_rumor_vic = kid spread rumours about me
# peq_invite_vic = kid did not invite me to party
# peq_exclude_vic = kid left me out 
# peq_gossip_vic = kid gossiped about me
# peq_threat_vic = kid threatened to hurt/beat me
# peq_loser_vic = kid said mean things about me
# peq_hit_vic = kid hit/kicked/pushed me 

# Check prevalence of peer victimisation per assessment
table(abcd_wide$peq_left_out_vic.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$peq_left_out_vic.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$peq_left_out_vic.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

# Peer victimisation was measured only at 2 year follow up

# Convert peer victimisation items to numeric
abcd_wide <- abcd_wide %>% 
  mutate_at(c("peq_left_out_vic.2_year_follow_up_y_arm_1","peq_chase_vic.2_year_follow_up_y_arm_1","peq_rumor_vic.2_year_follow_up_y_arm_1",
              "peq_invite_vic.2_year_follow_up_y_arm_1","peq_exclude_vic.2_year_follow_up_y_arm_1","peq_gossip_vic.2_year_follow_up_y_arm_1",
              "peq_threat_vic.2_year_follow_up_y_arm_1","peq_loser_vic.2_year_follow_up_y_arm_1","peq_hit_vic.2_year_follow_up_y_arm_1"), 
            function(x) as.numeric(as.character(x)))

# Items were originally coded as: 1=Never; 2=Once or twice; 3=A few times; 4=About once a week; 5=A few times a week
# Code each individual item so a score of 4 or 5 indicates being victimised
# then if the child reports at least 1 type of victimisation, code as 1 for peer victimisation

# Kids left me out of activity/conversation
table(abcd_wide$peq_left_out_vic.2_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$peq_left_out_vic.2_year_follow_up_y_arm_1 <- ifelse(abcd_wide$peq_left_out_vic.2_year_follow_up_y_arm_1>=4, 1, 0) 
table(abcd_wide$peq_left_out_vic.2_year_follow_up_y_arm_1, useNA = "always")

# Kid chased me like trying to hurt me
table(abcd_wide$peq_chase_vic.2_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$peq_chase_vic.2_year_follow_up_y_arm_1 <- ifelse(abcd_wide$peq_chase_vic.2_year_follow_up_y_arm_1>=4, 1, 0) 
table(abcd_wide$peq_chase_vic.2_year_follow_up_y_arm_1, useNA = "always")

# Kid spread rumours about me
table(abcd_wide$peq_rumor_vic.2_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$peq_rumor_vic.2_year_follow_up_y_arm_1 <- ifelse(abcd_wide$peq_rumor_vic.2_year_follow_up_y_arm_1>=4, 1, 0) 
table(abcd_wide$peq_rumor_vic.2_year_follow_up_y_arm_1, useNA = "always")

# Kid did not invite me to party
table(abcd_wide$peq_invite_vic.2_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$peq_invite_vic.2_year_follow_up_y_arm_1 <- ifelse(abcd_wide$peq_invite_vic.2_year_follow_up_y_arm_1>=4, 1, 0) 
table(abcd_wide$peq_invite_vic.2_year_follow_up_y_arm_1, useNA = "always")

# Kid left me out
table(abcd_wide$peq_exclude_vic.2_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$peq_exclude_vic.2_year_follow_up_y_arm_1 <- ifelse(abcd_wide$peq_exclude_vic.2_year_follow_up_y_arm_1>=4, 1, 0) 
table(abcd_wide$peq_exclude_vic.2_year_follow_up_y_arm_1, useNA = "always")

# Kid gossiped about me
table(abcd_wide$peq_gossip_vic.2_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$peq_gossip_vic.2_year_follow_up_y_arm_1 <- ifelse(abcd_wide$peq_gossip_vic.2_year_follow_up_y_arm_1>=4, 1, 0) 
table(abcd_wide$peq_gossip_vic.2_year_follow_up_y_arm_1, useNA = "always")

# Kid threatened to hurt/beat me
table(abcd_wide$peq_threat_vic.2_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$peq_threat_vic.2_year_follow_up_y_arm_1 <- ifelse(abcd_wide$peq_threat_vic.2_year_follow_up_y_arm_1>=4, 1, 0) 
table(abcd_wide$peq_threat_vic.2_year_follow_up_y_arm_1, useNA = "always")

# Kid said mean things about me
table(abcd_wide$peq_loser_vic.2_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$peq_loser_vic.2_year_follow_up_y_arm_1 <- ifelse(abcd_wide$peq_loser_vic.2_year_follow_up_y_arm_1>=4, 1, 0) 
table(abcd_wide$peq_loser_vic.2_year_follow_up_y_arm_1, useNA = "always")

# Kid hit/kicked/pushed me 
table(abcd_wide$peq_hit_vic.2_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$peq_hit_vic.2_year_follow_up_y_arm_1 <- ifelse(abcd_wide$peq_hit_vic.2_year_follow_up_y_arm_1>=4, 1, 0) 
table(abcd_wide$peq_hit_vic.2_year_follow_up_y_arm_1, useNA = "always")

# Derive peer victimisation composite variable
abcd_wide$peer_victimisation <- 0
abcd_wide$peer_victimisation[abcd_wide$peq_left_out_vic.2_year_follow_up_y_arm_1==1 | abcd_wide$peq_chase_vic.2_year_follow_up_y_arm_1==1 |
                                abcd_wide$peq_rumor_vic.2_year_follow_up_y_arm_1==1 | abcd_wide$peq_invite_vic.2_year_follow_up_y_arm_1==1 | 
                                abcd_wide$peq_exclude_vic.2_year_follow_up_y_arm_1==1 | abcd_wide$peq_gossip_vic.2_year_follow_up_y_arm_1==1 | 
                                abcd_wide$peq_threat_vic.2_year_follow_up_y_arm_1==1 | abcd_wide$peq_loser_vic.2_year_follow_up_y_arm_1==1 | 
                                abcd_wide$peq_hit_vic.2_year_follow_up_y_arm_1==1] <- 1 
abcd_wide$peer_victimisation[apply(apply(abcd_wide[,c("peq_left_out_vic.2_year_follow_up_y_arm_1","peq_chase_vic.2_year_follow_up_y_arm_1","peq_rumor_vic.2_year_follow_up_y_arm_1",
              "peq_invite_vic.2_year_follow_up_y_arm_1","peq_exclude_vic.2_year_follow_up_y_arm_1","peq_gossip_vic.2_year_follow_up_y_arm_1",
              "peq_threat_vic.2_year_follow_up_y_arm_1","peq_loser_vic.2_year_follow_up_y_arm_1","peq_hit_vic.2_year_follow_up_y_arm_1")],2,is.na),1,sum) > 4] <- NA # code NA if missing > 4/9 items
table(abcd_wide$peer_victimisation, useNA="always")
prop.table(table(abcd_wide$peer_victimisation, useNA="always"))*100 # 6.98%

#------------------------- Cyber victimisation -----------------------------------------#
# cybb_phenx_harm = have you ever been cyberbullied 
# cybb_phenx_harm_12mo = cyberbullied in the past 12 months 
# cybb_phenx_harm_often = how often cyberbullied in the past 12 months 

# Check prevalence of cyber victimisation per assessment
table(abcd_wide$cybb_phenx_harm.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$cybb_phenx_harm.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$cybb_phenx_harm.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

# Cyber victimisation was measured only at 2 year follow up

# Recode cyber victimisation variables to missing if values are 777 = Refused to answer 

# Have you ever been cyberbullied (1=Yes; 0=No)
table(abcd_wide$cybb_phenx_harm.2_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$cybb_phenx_harm.2_year_follow_up_y_arm_1[abcd_wide$cybb_phenx_harm.2_year_follow_up_y_arm_1=="777"] <- NA 
table(abcd_wide$cybb_phenx_harm.2_year_follow_up_y_arm_1, useNA = "always")

# Cyberbullied in the past 12 months (1=Yes; 0=No)
table(abcd_wide$cybb_phenx_harm_12mo.2_year_follow_up_y_arm_1, useNA = "always")

# How often cyberbullied in the past 12 months (1=1 time; 2=2 times; 3=3 times; 4=4-9 times; 5=10-19 times; 6=20-39 times; 7=40-49 times; 8=50+ times)
table(abcd_wide$cybb_phenx_harm_often.2_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$cybb_phenx_harm_often.2_year_follow_up_y_arm_1 <- ifelse(abcd_wide$cybb_phenx_harm_often.2_year_follow_up_y_arm_1>=5, 1, 0) # code to 1 if >=10 times in past 12 months
table(abcd_wide$cybb_phenx_harm_often.2_year_follow_up_y_arm_1, useNA = "always")

# Derive cyber victimisation composite variable 
abcd_wide$cyber_victimisation <- 0
abcd_wide$cyber_victimisation[abcd_wide$cybb_phenx_harm.2_year_follow_up_y_arm_1==1 | abcd_wide$cybb_phenx_harm_12mo.2_year_follow_up_y_arm_1==1 | 
                                abcd_wide$cybb_phenx_harm_often.2_year_follow_up_y_arm_1==1 ] <- 1 
abcd_wide$cyber_victimisation[apply(apply(abcd_wide[,c("cybb_phenx_harm.2_year_follow_up_y_arm_1", "cybb_phenx_harm_12mo.2_year_follow_up_y_arm_1")],2,is.na),1,sum) > 1] <- NA # code NA if missing > 1/2 items
table(abcd_wide$cyber_victimisation, useNA="always")
prop.table(table(abcd_wide$cyber_victimisation, useNA="always"))*100 # 7.83%

#------------------------- Unsafe neighbourhood -----------------------------------------#
# neighborhood1r_p = feel safe walking in my neighborhood, day or night
# neighborhood2r_p = violence is not a problem in my neighborhood
# neighborhood3r_p = my neighborhood is safe from crime (parent reported)
# neighborhood_crime_y = my neighborhood is safe from crime (child reported)

# Check prevalence of unsafe neighbourhood per assessment
table(abcd_wide$neighborhood1r_p.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$neighborhood1r_p.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$neighborhood1r_p.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

table(abcd_wide$neighborhood_crime_y.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$neighborhood_crime_y.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$neighborhood_crime_y.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

# Parent and youth neighbourhood safety/crime surveys were measured at baseline, 1 year, and 2 year follow up

# Items were originally coded as: 1 = Strongly Disagree; 2 = Disagree; 3 = Neutral (neither agree nor disagree); 4 = Agree; 5 = Strongly Agree
# Code each individual item so a score of 1 indicates feeling unsafe

# Feel safe walking in my neighborhood, day or night
table(abcd_wide$neighborhood1r_p.baseline_year_1_arm_1, useNA = "always")
abcd_wide$neighborhood1r_p.baseline_year_1_arm_1 <- ifelse(abcd_wide$neighborhood1r_p.baseline_year_1_arm_1==1, 1, 0) 
table(abcd_wide$neighborhood1r_p.baseline_year_1_arm_1, useNA = "always")

table(abcd_wide$neighborhood1r_p.1_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$neighborhood1r_p.1_year_follow_up_y_arm_1 <- ifelse(abcd_wide$neighborhood1r_p.1_year_follow_up_y_arm_1==1, 1, 0) 
table(abcd_wide$neighborhood1r_p.1_year_follow_up_y_arm_1, useNA = "always")

table(abcd_wide$neighborhood1r_p.2_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$neighborhood1r_p.2_year_follow_up_y_arm_1 <- ifelse(abcd_wide$neighborhood1r_p.2_year_follow_up_y_arm_1==1, 1, 0) 
table(abcd_wide$neighborhood1r_p.2_year_follow_up_y_arm_1, useNA = "always")

# Violence is not a problem in my neighborhood
table(abcd_wide$neighborhood2r_p.baseline_year_1_arm_1, useNA = "always")
abcd_wide$neighborhood2r_p.baseline_year_1_arm_1 <- ifelse(abcd_wide$neighborhood2r_p.baseline_year_1_arm_1==1, 1, 0) 
table(abcd_wide$neighborhood2r_p.baseline_year_1_arm_1, useNA = "always")

table(abcd_wide$neighborhood2r_p.1_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$neighborhood2r_p.1_year_follow_up_y_arm_1 <- ifelse(abcd_wide$neighborhood2r_p.1_year_follow_up_y_arm_1==1, 1, 0) 
table(abcd_wide$neighborhood2r_p.1_year_follow_up_y_arm_1, useNA = "always")

table(abcd_wide$neighborhood2r_p.2_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$neighborhood2r_p.2_year_follow_up_y_arm_1 <- ifelse(abcd_wide$neighborhood2r_p.2_year_follow_up_y_arm_1==1, 1, 0) 
table(abcd_wide$neighborhood2r_p.2_year_follow_up_y_arm_1, useNA = "always")

# My neighborhood is safe from crime (parent reported)
table(abcd_wide$neighborhood3r_p.baseline_year_1_arm_1, useNA = "always")
abcd_wide$neighborhood3r_p.baseline_year_1_arm_1 <- ifelse(abcd_wide$neighborhood3r_p.baseline_year_1_arm_1==1, 1, 0) 
table(abcd_wide$neighborhood3r_p.baseline_year_1_arm_1, useNA = "always")

table(abcd_wide$neighborhood3r_p.1_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$neighborhood3r_p.1_year_follow_up_y_arm_1 <- ifelse(abcd_wide$neighborhood3r_p.1_year_follow_up_y_arm_1==1, 1, 0) 
table(abcd_wide$neighborhood3r_p.1_year_follow_up_y_arm_1, useNA = "always")

table(abcd_wide$neighborhood3r_p.2_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$neighborhood3r_p.2_year_follow_up_y_arm_1 <- ifelse(abcd_wide$neighborhood3r_p.2_year_follow_up_y_arm_1==1, 1, 0) 
table(abcd_wide$neighborhood3r_p.2_year_follow_up_y_arm_1, useNA = "always")

# My neighborhood is safe from crime (youth reported)
table(abcd_wide$neighborhood_crime_y.baseline_year_1_arm_1, useNA = "always")
abcd_wide$neighborhood_crime_y.baseline_year_1_arm_1 <- ifelse(abcd_wide$neighborhood_crime_y.baseline_year_1_arm_1==1, 1, 0) 
table(abcd_wide$neighborhood_crime_y.baseline_year_1_arm_1, useNA = "always")

table(abcd_wide$neighborhood_crime_y.1_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$neighborhood_crime_y.1_year_follow_up_y_arm_1 <- ifelse(abcd_wide$neighborhood_crime_y.1_year_follow_up_y_arm_1==1, 1, 0) 
table(abcd_wide$neighborhood_crime_y.1_year_follow_up_y_arm_1, useNA = "always")

table(abcd_wide$neighborhood_crime_y.2_year_follow_up_y_arm_1, useNA = "always")
abcd_wide$neighborhood_crime_y.2_year_follow_up_y_arm_1 <- ifelse(abcd_wide$neighborhood_crime_y.2_year_follow_up_y_arm_1==1, 1, 0) 
table(abcd_wide$neighborhood_crime_y.2_year_follow_up_y_arm_1, useNA = "always")

# Derive unsafe neighbourhood composite variable 
abcd_wide$unsafe_neighbourhood <- 0
abcd_wide$unsafe_neighbourhood[abcd_wide$neighborhood1r_p.baseline_year_1_arm_1==1 | abcd_wide$neighborhood1r_p.1_year_follow_up_y_arm_1==1 | abcd_wide$neighborhood1r_p.2_year_follow_up_y_arm_1==1 | 
                                 abcd_wide$neighborhood2r_p.baseline_year_1_arm_1==1 | abcd_wide$neighborhood2r_p.1_year_follow_up_y_arm_1==1 | abcd_wide$neighborhood2r_p.2_year_follow_up_y_arm_1==1 | 
                                 abcd_wide$neighborhood3r_p.baseline_year_1_arm_1==1 | abcd_wide$neighborhood3r_p.1_year_follow_up_y_arm_1==1 | abcd_wide$neighborhood3r_p.2_year_follow_up_y_arm_1==1 | 
                                 abcd_wide$neighborhood_crime_y.baseline_year_1_arm_1==1 | abcd_wide$neighborhood_crime_y.1_year_follow_up_y_arm_1==1 | abcd_wide$neighborhood_crime_y.2_year_follow_up_y_arm_1==1] <- 1 
abcd_wide$unsafe_neighbourhood[apply(apply(abcd_wide[,c("neighborhood1r_p.baseline_year_1_arm_1", "neighborhood1r_p.1_year_follow_up_y_arm_1", "neighborhood1r_p.2_year_follow_up_y_arm_1", 
                                                        "neighborhood2r_p.baseline_year_1_arm_1", "neighborhood2r_p.1_year_follow_up_y_arm_1", "neighborhood2r_p.2_year_follow_up_y_arm_1",
                                                        "neighborhood3r_p.baseline_year_1_arm_1", "neighborhood3r_p.1_year_follow_up_y_arm_1", "neighborhood3r_p.2_year_follow_up_y_arm_1",
                                                        "neighborhood_crime_y.baseline_year_1_arm_1", "neighborhood_crime_y.1_year_follow_up_y_arm_1", 
                                                        "neighborhood_crime_y.2_year_follow_up_y_arm_1")],2,is.na),1,sum) > 6] <- NA # code NA if missing > 6/12 items
table(abcd_wide$unsafe_neighbourhood, useNA="always")
prop.table(table(abcd_wide$unsafe_neighbourhood, useNA="always"))*100 # 17.3%

#------------------------- Low household income -----------------------------------------#
# demo_comb_income_v2 = What is your total combined family income for the past 12 months?
# demo_comb_income_v2_l = Same question from follow up longitudinal parent demographics survey

# 1= Less than $5,000; 2=$5,000 through $11,999; 3=$12,000 through $15,999; 
# 4=$16,000 through $24,999; 5=$25,000 through $34,999; 6=$35,000 through $49,999; 
# 7=$50,000 through $74,999; 8= $75,000 through $99,999; 9=$100,000 through $199,999; 
# 10=$200,000 and greater

# US Census Bureau: in 2021, households in the lowest quintile had incomes of $28,007 or less
# https://www.census.gov/content/dam/Census/library/publications/2022/demo/p60-276.pdf

# Check prevalence of household income per assessment
table(abcd_wide$demo_comb_income_v2.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$demo_comb_income_v2.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$demo_comb_income_v2.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

table(abcd_wide$demo_comb_income_v2_l.baseline_year_1_arm_1, useNA="always") # baseline
table(abcd_wide$demo_comb_income_v2_l.1_year_follow_up_y_arm_1, useNA="always") # 1 year follow up
table(abcd_wide$demo_comb_income_v2_l.2_year_follow_up_y_arm_1, useNA="always") # 2 year follow up

# Parent demographics survey was measured only at baseline
# Longitudinal parent demographics survey was measured at 1 year and 2 year follow up 

# Recode household income variables to missing if values are 777 = Refused to answer or 999 = Don't know
table(abcd_wide$demo_comb_income_v2.baseline_year_1_arm_1, useNA="always")
abcd_wide$demo_comb_income_v2.baseline_year_1_arm_1[abcd_wide$demo_comb_income_v2.baseline_year_1_arm_1=="777" | abcd_wide$demo_comb_income_v2.baseline_year_1_arm_1=="999"] <- NA 
table(abcd_wide$demo_comb_income_v2.baseline_year_1_arm_1, useNA="always")

table(abcd_wide$demo_comb_income_v2_l.1_year_follow_up_y_arm_1, useNA="always")
abcd_wide$demo_comb_income_v2_l.1_year_follow_up_y_arm_1[abcd_wide$demo_comb_income_v2_l.1_year_follow_up_y_arm_1=="777" | abcd_wide$demo_comb_income_v2_l.1_year_follow_up_y_arm_1=="999"] <- NA 
table(abcd_wide$demo_comb_income_v2_l.1_year_follow_up_y_arm_1, useNA="always")

table(abcd_wide$demo_comb_income_v2_l.2_year_follow_up_y_arm_1, useNA="always")
abcd_wide$demo_comb_income_v2_l.2_year_follow_up_y_arm_1[abcd_wide$demo_comb_income_v2_l.2_year_follow_up_y_arm_1=="777" | abcd_wide$demo_comb_income_v2_l.2_year_follow_up_y_arm_1=="999"] <- NA 
table(abcd_wide$demo_comb_income_v2_l.2_year_follow_up_y_arm_1, useNA="always")

# Derive low household income composite variable
abcd_wide$low_household_income <- 0
abcd_wide$low_household_income[abcd_wide$demo_comb_income_v2.baseline_year_1_arm_1<=4 | abcd_wide$demo_comb_income_v2_l.1_year_follow_up_y_arm_1<=4 | 
                                 abcd_wide$demo_comb_income_v2_l.2_year_follow_up_y_arm_1<=4] <- 1 # Income below $25k indicates low household income
abcd_wide$low_household_income[apply(apply(abcd_wide[,c("demo_comb_income_v2.baseline_year_1_arm_1", "demo_comb_income_v2_l.1_year_follow_up_y_arm_1", 
                                                       "demo_comb_income_v2_l.2_year_follow_up_y_arm_1")],2,is.na),1,sum) > 1] <- NA # code NA if missing > 1/3 items
table(abcd_wide$low_household_income, useNA="always")
prop.table(table(abcd_wide$low_household_income, useNA="always"))*100 # 19.2%

####################################################################################
#--------------------- Derive mental health measures ------------------------------#
####################################################################################

#------------------------- Derive raw CBCL internalising -----------------------------------------#
### This measure includes the following items:
## Anxious/depressed
#cbcl_q14_p = Cries a lot
#cbcl_q29_p = Fears certain animals, situations, or places, other than school
#cbcl_q30_p = Fears going to school 
#cbcl_q31_p = Fears he/she might think or do something bad
#cbcl_q32_p = Feels he/she has to be perfect
#cbcl_q33_p = Feels or complains that no one loves him/her
#cbcl_q35_p = Feels worthless or inferior 
#cbcl_q45_p = Nervous, highstrung, or tense 
#cbcl_q50_p = Too fearful or anxious
#cbcl_q52_p = Feels too guilty
#cbcl_q71_p = Self-conscious or easily embarrassed
#cbcl_q91_p = Talks about killing self 
#cbcl_q112_p = Worries 

## Withdrawn/depressed
#cbcl_q05_p = There is very little he/she enjoys 
#cbcl_q42_p = Would rather be alone than with others
#cbcl_q65_p = Refuses to talk 
#cbcl_q69_p = Secretive, keeps things to self
#cbcl_q75_p = Too shy or timid 
#cbcl_q102_p = Underactive, slow moving, or lacks energy 
#cbcl_q103_p = Unhappy, sad, or depressed 
#cbcl_q111_p = Withdrawn, doesn't get involved with others 

## Somatic complaints
#cbcl_q47_p = Nightmares 
#cbcl_q49_p = Constipated, doesn't move bowels
#cbcl_q51_p = Feels dizzy or lightheaded 
#cbcl_q54_p = Overtired without good reason
#cbcl_q56a_p = Aches or pains (not stomach or headaches) 
#cbcl_q56b_p = Headaches 
#cbcl_q56c_p = Nausea, feels sick 
#cbcl_q56d_p = Problems with eyes (not if corrected by glasses)
#cbcl_q56e_p = Rashes or other skin problems 
#cbcl_q56f_p = Stomachaches 
#cbcl_q56g_p = Vomiting, throwing up 

# Convert item variables included in measure from factor to numeric
internalising_cols <- c("cbcl_q14_p.3_year_follow_up_y_arm_1", "cbcl_q29_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q30_p.3_year_follow_up_y_arm_1", "cbcl_q31_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q32_p.3_year_follow_up_y_arm_1", "cbcl_q33_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q35_p.3_year_follow_up_y_arm_1", "cbcl_q45_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q50_p.3_year_follow_up_y_arm_1", "cbcl_q52_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q71_p.3_year_follow_up_y_arm_1", "cbcl_q91_p.3_year_follow_up_y_arm_1",
                        "cbcl_q112_p.3_year_follow_up_y_arm_1", "cbcl_q05_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q42_p.3_year_follow_up_y_arm_1", "cbcl_q65_p.3_year_follow_up_y_arm_1",
                        "cbcl_q69_p.3_year_follow_up_y_arm_1", "cbcl_q75_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q102_p.3_year_follow_up_y_arm_1", "cbcl_q103_p.3_year_follow_up_y_arm_1",
                        "cbcl_q111_p.3_year_follow_up_y_arm_1", "cbcl_q47_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q49_p.3_year_follow_up_y_arm_1", "cbcl_q51_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q54_p.3_year_follow_up_y_arm_1", "cbcl_q56a_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q56b_p.3_year_follow_up_y_arm_1", "cbcl_q56c_p.3_year_follow_up_y_arm_1",
                        "cbcl_q56d_p.3_year_follow_up_y_arm_1", "cbcl_q56e_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q56f_p.3_year_follow_up_y_arm_1", "cbcl_q56g_p.3_year_follow_up_y_arm_1") 
abcd_wide[,internalising_cols] <- lapply(abcd_wide[,internalising_cols], function(x) as.numeric(as.character(x)))

# Derive raw CBCL internalising sum score
abcd_wide$cbcl_internalising <- apply(abcd_wide[,internalising_cols], 1, sum, na.rm=T) # sum items
abcd_wide$cbcl_internalising[apply(apply(abcd_wide[,internalising_cols],2,is.na),1,sum) > 16] <- NA # set to missing if more than 50% missing

# Check against pre-derived CBCL internalising raw score
describe(abcd_wide$cbcl_internalising) # derived measure
describe(as.numeric(abcd_wide$cbcl_scr_syn_internal_r.3_year_follow_up_y_arm_1)) # original measure

#------------------------- Derive raw CBCL externalising -----------------------------------------#
# Note this sum score will include the following items, from the subscales of 
# rule-Breaking Behavior, Aggressive Behavior and Attention Problems

## Rule-Breaking Behavior
#cbcl_q02_p = Drinks alcohol without parents' approval
#cbcl_q26_p = Doesn't seem to feel guilty after misbehaving 
#cbcl_q28_p = Breaks rules at home, school or elsewhere
#cbcl_q39_p = Hangs around with others who get in trouble
#cbcl_q43_p = Lying or cheating
#cbcl_q63_p = Prefers being with older kids 
#cbcl_q67_p = Runs away from home
#cbcl_q72_p = Sets fires
#cbcl_q73_p = Sexual problems
#cbcl_q81_p = Steals at home 
#cbcl_q82_p = Steals outside the home
#cbcl_q90_p = Swearing or obscene language
#cbcl_q96_p = Thinks about sex too much
#cbcl_q99_p = Smokes, chews, or sniffs tobacco
#cbcl_q101_p = Truancy, skips school 
#cbcl_q105_p = Uses drugs for non medical purposes (don't include alcohol or tobacco) 
#cbcl_q106_p = Vandalism

## Aggressive Behavior
#cbcl_q03_p = Argues a lot 
#cbcl_q16_p = Cruelty, bullying, or meanness to others
#cbcl_q19_p = Demands a lot of attention
#cbcl_q20_p = Destroys his/her own things 
#cbcl_q21_p = Destroys things belonging to his/her family or others
#cbcl_q22_p = Disobedient at home 
#cbcl_q23_p = Disobedient at school 
#cbcl_q37_p = Gets in many fights
#cbcl_q57_p = Physically attacks people
#cbcl_q68_p = Screams a lot 
#cbcl_q86_p = Stubborn, sullen, or irritable
#cbcl_q87_p = Sudden changes in mood or feelings 
#cbcl_q88_p = Sulks a lot 
#cbcl_q89_p = Suspicious
#cbcl_q94_p = Teases a lot
#cbcl_q95_p = Temper tantrums or hot temper 
#cbcl_q97_p = Threatens people
#cbcl_q104_p = Unusually loud

## Attention problems
#cbcl_q01_p = Acts too young for his/her age 
#cbcl_q04_p = Fails to finish things he/she starts 
#cbcl_q08_p = Can't concentrate, can't pay attention for long
#cbcl_q10_p = Can't sit still, restless, or hyperactive 
#cbcl_q13_p = Confused or seems to be in a fog 
#cbcl_q17_p = Daydreams or gets lost in his/her thoughts
#cbcl_q41_p = Impulsive or acts without thinking
#cbcl_q61_p = Poor school work 
#cbcl_q78_p = Inattentive or easily distracted 
#cbcl_q80_p = Stares blankly

# Convert variables from factor to numeric 
externalising_cols <- c("cbcl_q02_p.3_year_follow_up_y_arm_1", "cbcl_q26_p.3_year_follow_up_y_arm_1",
                        "cbcl_q28_p.3_year_follow_up_y_arm_1", "cbcl_q39_p.3_year_follow_up_y_arm_1",
                        "cbcl_q43_p.3_year_follow_up_y_arm_1", "cbcl_q63_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q67_p.3_year_follow_up_y_arm_1", "cbcl_q72_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q73_p.3_year_follow_up_y_arm_1", "cbcl_q81_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q82_p.3_year_follow_up_y_arm_1", "cbcl_q90_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q96_p.3_year_follow_up_y_arm_1", "cbcl_q99_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q101_p.3_year_follow_up_y_arm_1", "cbcl_q105_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q106_p.3_year_follow_up_y_arm_1", "cbcl_q03_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q16_p.3_year_follow_up_y_arm_1","cbcl_q19_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q20_p.3_year_follow_up_y_arm_1", "cbcl_q21_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q22_p.3_year_follow_up_y_arm_1", "cbcl_q23_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q37_p.3_year_follow_up_y_arm_1", "cbcl_q57_p.3_year_follow_up_y_arm_1",
                        "cbcl_q68_p.3_year_follow_up_y_arm_1", "cbcl_q86_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q87_p.3_year_follow_up_y_arm_1", "cbcl_q88_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q89_p.3_year_follow_up_y_arm_1", "cbcl_q94_p.3_year_follow_up_y_arm_1",
                        "cbcl_q95_p.3_year_follow_up_y_arm_1",  "cbcl_q97_p.3_year_follow_up_y_arm_1",
                        "cbcl_q104_p.3_year_follow_up_y_arm_1", "cbcl_q01_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q04_p.3_year_follow_up_y_arm_1", "cbcl_q08_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q10_p.3_year_follow_up_y_arm_1", "cbcl_q13_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q17_p.3_year_follow_up_y_arm_1", "cbcl_q41_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q61_p.3_year_follow_up_y_arm_1", "cbcl_q78_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q80_p.3_year_follow_up_y_arm_1")
abcd_wide[,externalising_cols] <- lapply(abcd_wide[,externalising_cols], function(x) as.numeric(as.character(x)))

# Derive raw CBCL externalising sum score
abcd_wide$cbcl_externalising <- apply(abcd_wide [,externalising_cols], 1, sum, na.rm=T) # sum items
abcd_wide$cbcl_externalising[apply(apply(abcd_wide[,externalising_cols],2,is.na),1,sum) > 22] <- NA #set missing if >50% items NA

# Check against pre-derived CBCL externalising raw score 
# note: the distributions are different because the pre-derived CBCL externalising does not include attention problems
describe(abcd_wide$cbcl_externalising)  # derived measure
describe(as.numeric(abcd_wide$cbcl_scr_syn_external_r.3_year_follow_up_y_arm_1)) # original measure

###############################################################################
#----------------------- Descriptive statistics ------------------------------#
###############################################################################

# ACE variables were all coded to binary such that 1 = risk; 0 = no risk
# Check n and percentages for each ACE

# Physical abuse (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(abcd_wide$physical_abuse, useNA="always")
prop.table(table(abcd_wide$physical_abuse, useNA="always"))*100

# Emotional abuse (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(abcd_wide$emotional_abuse, useNA="always") 
prop.table(table(abcd_wide$emotional_abuse, useNA="always"))*100

# Sexual abuse (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(abcd_wide$sexual_abuse, useNA="always") 
prop.table(table(abcd_wide$sexual_abuse, useNA="always"))*100

# Domestic violence (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(abcd_wide$domestic_violence, useNA="always") 
prop.table(table(abcd_wide$domestic_violence, useNA="always"))*100

# Accident requiring medical attention (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(abcd_wide$accident_medical, useNA="always")
prop.table(table(abcd_wide$accident_medical, useNA="always"))*100

# Natural disaster (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(abcd_wide$natural_disaster, useNA="always")
prop.table(table(abcd_wide$natural_disaster, useNA="always"))*100

# Community violence (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(abcd_wide$community_violence, useNA="always") 
prop.table(table(abcd_wide$community_violence, useNA="always"))*100

# Bereavement (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(abcd_wide$bereavement, useNA="always") 
prop.table(table(abcd_wide$bereavement, useNA="always"))*100

# Emotional neglect (Baseline, 1y follow up; ages 0-9/10y, 10/11y)
table(abcd_wide$emotional_neglect, useNA="always")
prop.table(table(abcd_wide$emotional_neglect, useNA="always"))*100

# Parental psychopathology (Baseline, 2y follow up; ages 0-9/10y, 11/12y)
table(abcd_wide$parental_psychopathology, useNA="always")
prop.table(table(abcd_wide$parental_psychopathology, useNA="always"))*100

# Parental alcohol abuse (Baseline; ages 0-9/10y)
table(abcd_wide$parental_alcohol_abuse, useNA="always")
prop.table(table(abcd_wide$parental_alcohol_abuse, useNA="always"))*100

# Parental drug abuse (Baseline, 2y follow up; ages 0-9/10y, 11/12y)
table(abcd_wide$parental_drug_abuse, useNA="always")
prop.table(table(abcd_wide$parental_drug_abuse, useNA="always"))*100

# Parental criminality (1y and 2y follow up; ages 10/11y, 11/12y)
table(abcd_wide$parental_criminality, useNA="always") 
prop.table(table(abcd_wide$parental_criminality, useNA="always"))*100

# Parental separation (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(abcd_wide$parental_separation, useNA="always") 
prop.table(table(abcd_wide$parental_separation, useNA="always"))*100

# Peer victimisation (2y follow up; ages 11/12y)
table(abcd_wide$peer_victimisation, useNA="always") 
prop.table(table(abcd_wide$peer_victimisation, useNA="always"))*100

# Cyber victimisation (2y follow up; ages 11/12y)
table(abcd_wide$cyber_victimisation, useNA="always")
prop.table(table(abcd_wide$cyber_victimisation, useNA="always"))*100

# Unsafe neighbourhood (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(abcd_wide$unsafe_neighbourhood, useNA="always")
prop.table(table(abcd_wide$unsafe_neighbourhood, useNA="always"))*100

# Low household income (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(abcd_wide$low_household_income, useNA="always")
prop.table(table(abcd_wide$low_household_income, useNA="always"))*100

# Adolescent mental health outcomes (3y follow up; ages 12/13y)
describe(abcd_wide$cbcl_internalising) # internalising symptoms
describe(abcd_wide$cbcl_externalising) # externalising symptoms

##################################################################################
#------------------------ Prepare auxiliary variables -------------------------#
##################################################################################

# We only want baseline measures for auxiliary variables, because they have the most complete data
abcd_wide$sex <- abcd_wide$sex.baseline_year_1_arm_1

abcd_wide$race <- abcd_wide$race.baseline_year_1_arm_1

abcd_wide$employment <- abcd_wide$employment.baseline_year_1_arm_1

abcd_wide$high_educ <- abcd_wide$high.educ.baseline_year_1_arm_1

abcd_wide$household_size <- abcd_wide$household_size.baseline_year_1_arm_1

abcd_wide$mat_age_birth <- abcd_wide$mat_age_birth.baseline_year_1_arm_1

abcd_wide$birthweight <- abcd_wide$birth_weight_lbs.baseline_year_1_arm_1

abcd_wide$premature_birth <- abcd_wide$premature_birth.baseline_year_1_arm_1

abcd_wide$maternal_smoking_bef_preg <- abcd_wide$maternal_smoking_bef_preg.baseline_year_1_arm_1

abcd_wide$maternal_smoking_aft_preg <- abcd_wide$maternal_smoking_aft_preg.baseline_year_1_arm_1

abcd_wide$maternal_alcohol_bef_preg <- abcd_wide$maternal_alcohol_bef_preg.baseline_year_1_arm_1

abcd_wide$maternal_alcohol_aft_preg <- abcd_wide$maternal_alcohol_aft_preg.baseline_year_1_arm_1

abcd_wide$preg_complications <- abcd_wide$preg_complications.baseline_year_1_arm_1

abcd_wide$diff_afford_food <- abcd_wide$diff_afford_food.baseline_year_1_arm_1

abcd_wide$diff_afford_gas <- abcd_wide$diff_afford_gas.baseline_year_1_arm_1

abcd_wide$evicted_home <- abcd_wide$evicted_home.baseline_year_1_arm_1

# Parent AESBA mental health scores were measured at baseline and 2 year follow up
# We only want baseline data because 2 year follow up has more missing data 
abcd_wide$par_anxious_depressed <- abcd_wide$par_anxious_depressed.baseline_year_1_arm_1

abcd_wide$par_withdrawn <- abcd_wide$par_withdrawn.baseline_year_1_arm_1

abcd_wide$par_somatic_complaints <- abcd_wide$par_somatic_complaints.baseline_year_1_arm_1

abcd_wide$par_thought_problems <- abcd_wide$par_thought_problems.baseline_year_1_arm_1

abcd_wide$par_attention_problems <- abcd_wide$par_attention_problems.baseline_year_1_arm_1

abcd_wide$par_aggress_behav <- abcd_wide$par_aggress_behav.baseline_year_1_arm_1

# Check descriptive statistics for sex and race 
table(abcd_wide$sex, useNA="always")
prop.table(table(abcd_wide$sex, useNA="always"))*100

table(abcd_wide$race, useNA="always")
prop.table(table(abcd_wide$race, useNA="always"))*100

# Subset to a reduced dataset only including the required variables for EFA and regressions 
required_vars <- names(abcd_wide) %in% c("src_subject_id",
         # ACEs
         "physical_abuse", 
         "emotional_abuse",
         "sexual_abuse", 
         "domestic_violence", 
         "accident_medical",
         "natural_disaster",
         "community_violence",
         "bereavement",
         "emotional_neglect",
         "parental_psychopathology", 
         "parental_alcohol_abuse",
         "parental_drug_abuse",
         "parental_criminality",
         "parental_separation",
         "peer_victimisation",
         "cyber_victimisation",
         "unsafe_neighbourhood",
         "low_household_income",
         # Outcome variables
         "cbcl_internalising",
         "cbcl_externalising",
         # Auxiliary variables
         "sex",
         "race", 
         "employment", 
         "high_educ", 
         "household_size", 
         "mat_age_birth",
         "birthweight", 
         "premature_birth", 
         "maternal_smoking_bef_preg", 
         "maternal_smoking_aft_preg", 
         "maternal_alcohol_bef_preg", 
         "maternal_alcohol_aft_preg", 
         "preg_complications", 
         "diff_afford_food", 
         "diff_afford_gas", 
         "evicted_home", 
         "par_anxious_depressed", 
         "par_withdrawn",
         "par_somatic_complaints",
         "par_thought_problems", 
         "par_attention_problems", 
         "par_aggress_behav") 

abcd_pre_imputation <- abcd_wide[required_vars]
colnames(abcd_pre_imputation)
head(abcd_pre_imputation)
dim(abcd_pre_imputation)

# Save dataset
setwd("/Users/athenachowruwern/Desktop")
save(abcd_pre_imputation, file = "ABCDpreImputation.RData")

# Remove individual datafiles
rm(abcd, abcd_small, abcd_wide, asr_vars, aux_vars, externalising_cols, internalising_cols, required_vars, v_names)
```

# 2. Exploratory factor analysis

We will visualise the correlation matrix, conduct parallel analysis, and
examine fit coefficients to inform the number of factors to retain in
the exploratory factor analysis.

``` r
# Load ABCD pre-imputation dataset
setwd("/Users/athenachowruwern/Desktop")
load("ABCDpreImputation.RData")

# Create new dataframe with all derived binary ACE variables
abcd_aces <- select(abcd_pre_imputation,
                   physical_abuse,
                   emotional_abuse,
                   sexual_abuse,
                   domestic_violence,
                   accident_medical,
                   natural_disaster,
                   community_violence,
                   bereavement,
                   emotional_neglect,
                   parental_psychopathology,
                   parental_alcohol_abuse,
                   parental_drug_abuse,
                   parental_criminality,
                   parental_separation,
                   peer_victimisation,
                   cyber_victimisation,
                   unsafe_neighbourhood,
                   low_household_income)
dim(abcd_aces)
str(abcd_aces)
colnames(abcd_aces)

# Check whether IDs are duplicated - no
n_occur <- data.frame(table(abcd_aces$src_subject_id))
n_occur[n_occur$Freq > 1,]

#------------------------------------ Exploratory Factor Analysis (EFA) ------------------------------------#
library(corrplot)
library(EFA.dimensions)

# Compute tetrachoric correlations
tet_corr <- tetrachoric(abcd_aces)
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
factorability <- FACTORABILITY(abcd_aces, corkind="polychoric", Ncases=nrow(abcd_aces))
factorability

###### Parallel analysis, 1000 Monte-Carlo simulations, 99% confidence intervals

# Weighted least squares parallel analysis
parallel_analysis_wls <- fa.parallel(abcd_aces, fm="wls", fa="both", n.iter=1000, cor="tet", error.bars=TRUE, sim=FALSE, quant=.99, main="Parallel Analysis Scree Plot" )

parallel_analysis_wls$fa.values # eigenvalues
parallel_analysis_wls
# WLS parallel analysis recommends that the number of factors = 5 and the number of components = 4

###### Exploratory factor analysis (using item response theory (IRT) code)

## IRT one-factor model
irt_1factor <- irt.fa(tet_corr, nfactors=1, plot=FALSE, n.obs=nrow(abcd_aces), rotate="promax", fm="wls")
irt_1factor$fa # loadings
fa.diagram(irt_1factor, cut=0.2, rsize=.30) # factor diagram

## IRT two-factor model
irt_2factor <- irt.fa(tet_corr, nfactors=2, plot=FALSE, n.obs=nrow(abcd_aces), rotate="promax", fm="wls")
irt_2factor$fa # loadings
fa.diagram(irt_2factor, cut=0.2, rsize=.30) # factor diagram

## IRT three-factor model
irt_3factor <- irt.fa(tet_corr, nfactors=3, plot=FALSE, n.obs=nrow(abcd_aces), rotate="oblimin", fm="wls") # promax doesn't work
irt_3factor$fa # loadings
fa.diagram(irt_3factor, cut=0.2, rsize=.30) # factor diagram

# IRT four-factor model
irt_4factor <- irt.fa(tet_corr, nfactors=4, plot=FALSE, n.obs=nrow(abcd_aces), rotate="oblimin", fm="wls") # promax doesn't work
irt_4factor$fa # loadings
colnames(irt_4factor$fa$loadings) <- c("Traumatic Events", "Parental Threat", "Deprivation", "Victimisation")
fa.diagram(irt_4factor, cut=0.2, rsize=.30) # factor diagram

#------------------------------------ Plot factor analysis results ------------------------------------#
# Following guidance from Dr Dan Mirman: https://rpubs.com/danmirman/plotting_factor_analysis

# Extract loadings from 4-factor model
class(irt_4factor$fa$loadings) # psych loadings class 
factor_loadings <- unclass(irt_4factor$fa$loadings) # convert to matrix
factor_loadings <- as.data.frame(factor_loadings) # convert to dataframe
factor_loadings <- rownames_to_column(factor_loadings, "aces") # name first column

# Rename ACEs
factor_loadings$aces <- car::recode(factor_loadings$aces, "'low_household_income'='Low household income'; 'unsafe_neighbourhood'='Unsafe neighbourhood'; 'emotional_neglect'='Emotional neglect'; 'peer_victimisation'='Peer victimisation'; 'cyber_victimisation'='Cyber victimisation'; 'parental_alcohol_abuse'='Parental alcohol abuse'; 'parental_drug_abuse'='Parental drug abuse'; 'parental_separation'='Parental separation'; 'parental_criminality'='Parental criminality'; 'domestic_violence'='Domestic violence'; 'parental_psychopathology'='Parental psychopathology'; 'physical_abuse'='Physical abuse'; 'emotional_abuse'='Emotional abuse'; 'community_violence'='Community violence'; 'sexual_abuse'='Sexual abuse'; 'natural_disaster'='Natural disaster'; 'accident_medical'='Accident requiring medical'; 'bereavement'='Bereavement'")

# Manually order ACEs by the 4 factor groups
factor_loadings$aces <- factor(factor_loadings$aces, levels = c("Bereavement", "Accident requiring medical", "Natural disaster", "Sexual abuse", "Community violence", "Emotional abuse", "Physical abuse", "Cyber victimisation", "Peer victimisation", "Emotional neglect", "Unsafe neighbourhood", "Low household income", "Parental psychopathology", "Domestic violence", "Parental criminality", "Parental separation", "Parental drug abuse", "Parental alcohol abuse"))
factor_loadings <- factor_loadings[order(factor_loadings$aces), ]
factor_loadings

# Melt data into long form for plotting
library(reshape2)
loadings.m <- melt(factor_loadings, id="aces", 
                   measure=c("Parental Threat", "Deprivation", "Victimisation", "Traumatic Events"), 
                   variable.name="Factor", value.name="Loading")
loadings.m

# Order the factors
f = c("Parental Threat", "Deprivation", "Victimisation", "Traumatic Events")
loadings.m <- within(loadings.m, Factor <- factor(Factor, levels=f))

# For each ACE, plot the loading as length and fill color of a bar
# note that the length will be the absolute value of the loading but the 
# fill color will be the signed value, more on this below
loadings_plot <- ggplot(loadings.m, aes(aces, abs(Loading), fill=Loading)) + 
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
tet_corr_df <- rownames_to_column(tet_corr_df, "aces") # name first column
head(tet_corr_df)

# Melt data into long form for plotting
corrs.m <- melt(tet_corr_df, id="aces", variable.name="aces2", value.name="Correlation")
head(corrs.m)

# Rename ACEs
corrs.m$aces <- car::recode(corrs.m$aces, "'low_household_income'='Low household income'; 'unsafe_neighbourhood'='Unsafe neighbourhood'; 'emotional_neglect'='Emotional neglect'; 'peer_victimisation'='Peer victimisation'; 'cyber_victimisation'='Cyber victimisation'; 'parental_alcohol_abuse'='Parental alcohol abuse'; 'parental_drug_abuse'='Parental drug abuse'; 'parental_separation'='Parental separation'; 'parental_criminality'='Parental criminality'; 'domestic_violence'='Domestic violence'; 'parental_psychopathology'='Parental psychopathology'; 'physical_abuse'='Physical abuse'; 'emotional_abuse'='Emotional abuse'; 'community_violence'='Community violence'; 'sexual_abuse'='Sexual abuse'; 'natural_disaster'='Natural disaster'; 'accident_medical'='Accident requiring medical'; 'bereavement'='Bereavement'")

corrs.m$aces2 <- car::recode(corrs.m$aces2, "'low_household_income'='Low household income'; 'unsafe_neighbourhood'='Unsafe neighbourhood'; 'emotional_neglect'='Emotional neglect'; 'peer_victimisation'='Peer victimisation'; 'cyber_victimisation'='Cyber victimisation'; 'parental_alcohol_abuse'='Parental alcohol abuse'; 'parental_drug_abuse'='Parental drug abuse'; 'parental_separation'='Parental separation'; 'parental_criminality'='Parental criminality'; 'domestic_violence'='Domestic violence'; 'parental_psychopathology'='Parental psychopathology'; 'physical_abuse'='Physical abuse'; 'emotional_abuse'='Emotional abuse'; 'community_violence'='Community violence'; 'sexual_abuse'='Sexual abuse'; 'natural_disaster'='Natural disaster'; 'accident_medical'='Accident requiring medical'; 'bereavement'='Bereavement'")

# Reorder ACEs in correlation matrix
corrs.m$aces <- factor(corrs.m$aces, levels = c("Bereavement", "Accident requiring medical", "Natural disaster", "Sexual abuse", "Community violence", "Emotional abuse", "Physical abuse", "Cyber victimisation", "Peer victimisation", "Emotional neglect", "Unsafe neighbourhood", "Low household income", "Parental psychopathology", "Domestic violence", "Parental criminality", "Parental separation", "Parental drug abuse", "Parental alcohol abuse"))
corrs.m <- corrs.m[order(corrs.m$aces), ]

corrs.m$aces2 <- factor(corrs.m$aces2, levels = c("Parental alcohol abuse", "Parental drug abuse", "Parental separation", "Parental criminality", "Domestic violence", "Parental psychopathology", "Low household income", "Unsafe neighbourhood", "Emotional neglect", "Peer victimisation", "Cyber victimisation", "Physical abuse", "Emotional abuse", "Community violence", "Sexual abuse", "Natural disaster", "Accident requiring medical", "Bereavement"))
corrs.m <- corrs.m[order(corrs.m$aces2), ]

corrs.m

# Plot correlation matrix
library(grid) 
# for adjusting plot margins
# place the tests on the x- and y-axes, 
# fill the elements with the strength of the correlation
corr_matrix <- ggplot(corrs.m, aes(aces2, aces, fill=abs(Correlation))) + 
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

# Plot stacked bar graph
p2 <- ggplot(loadings.m, aes(aces, abs(Loading), fill=Factor)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ylab("Loading Strength") + theme_bw(base_size=10) + 
  # remove labels and tweak margins for combining with the correlation matrix plot
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = unit(c(3,1,26.4,-3), "mm")) +
  scale_fill_manual(values = c("#66C2A5","#A6CEE3","#E78AC3","#B2DF8A"))
p2

# Add stacked bar graph of factor loadings to correlation matrix
# so that we can see how the factor analysis transformed these pairwise correlations into factors
library(gridExtra)
grid.arrange(p1, p2, ncol=2, widths=c(2, 1))

#---------------- This is the same code but different colour themes ----------------#
# Load Wes Anderson color palette
library(wesanderson)

# Load Harry Potter colour palette
library(harrypotter)

p2 <- ggplot(loadings.m, aes(aces, abs(Loading), fill=Factor)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ylab("Loading Strength") + theme_bw(base_size=10) + 
  # remove labels and tweak margins for combining with the correlation matrix plot
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = unit(c(3,1,26.4,-3), "mm")) +
  scale_fill_manual(values = wes_palette("Moonrise3", n = 4))

p2 <- ggplot(loadings.m, aes(aces, abs(Loading), fill=Factor)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ylab("Loading Strength") + theme_bw(base_size=10) + 
  # remove labels and tweak margins for combining with the correlation matrix plot
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = unit(c(3,1,26.4,-3), "mm")) +
  scale_fill_manual(values = wes_palette("GrandBudapest2", n = 4))

p2 <- ggplot(loadings.m, aes(aces, abs(Loading), fill=Factor)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ylab("Loading Strength") + theme_bw(base_size=10) + 
  # remove labels and tweak margins for combining with the correlation matrix plot
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = unit(c(3,1,26.4,-3), "mm")) +
  scale_fill_hp(discrete=TRUE, option = "Ravenclaw")

p2 <- ggplot(loadings.m, aes(aces, abs(Loading), fill=Factor)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ylab("Loading Strength") + theme_bw(base_size=10) + 
  # remove labels and tweak margins for combining with the correlation matrix plot
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = unit(c(3,1,26.4,-3), "mm")) +
  scale_fill_hp(discrete=TRUE, option = "Always")
```

# 3. Regression analysis

We will extract the factor scores as predictors for the regression
analysis, with internalising and externalising symptoms as the outcome
variables, as well as gender and ethnicity as covariates.

``` r
#------------------------- Extract factor scores ------------------------------------#
# Get factor scores 
factor_scores <- factor.scores(abcd_aces, irt_4factor$fa)
factor_scores

# Get columns of factor scores 
fs_cols <- factor_scores$scores
fs_cols <- as.data.frame(fs_cols)

# Add factor score columns to abcd_pre_imputation dataset
abcd_pre_imputation <- abcd_pre_imputation %>% mutate(
  "Traumatic_Events" = fs_cols$`Traumatic Events`,
  "Parental_Threat" = fs_cols$`Parental Threat`,
  "Deprivation" = fs_cols$`Deprivation`,
  "Victimisation" = fs_cols$`Victimisation`,
)

head(abcd_pre_imputation)

# Check prevalence of sex and race variables
table(abcd_pre_imputation$sex, useNA="always")
table(abcd_pre_imputation$race, useNA="always") 

# Condense race variable to six levels: White, Black/African American, American Indian/Alaska Native, Asian, Native Hawaiian/Other Pacific Islander, Other 
# Following guidance from US Census Bureau: https://www.census.gov/quickfacts/fact/note/US/RHI625221
table(abcd_pre_imputation$race, useNA = "always")
prop.table(table(abcd_pre_imputation$race, useNA = "always"))*100

abcd_pre_imputation$race.r <- car::recode(abcd_pre_imputation$race, "'Alaska Native'='American Indian/Alaska Native'; 'Asian Indian'='Asian'; 'Chinese'='Asian'; 'Filipino'='Asian'; 'Guamanian'='Native Hawaiian/Pacific Islander'; 'Japanese'='Asian'; 'Korean'='Asian'; 'Native American'='American Indian/Alaska Native'; 'Native Hawaiian'='Native Hawaiian/Pacific Islander'; 'Other Asian'='Asian'; 'Other Pacific Islander'='Native Hawaiian/Pacific Islander'; 'Other Race'='Other'; 'Samoan'='Native Hawaiian/Pacific Islander'; 'Vietnamese'='Asian'")

table(abcd_pre_imputation$race.r, useNA = "always")
prop.table(table(abcd_pre_imputation$race.r, useNA = "always"))*100

# Recode sex reference level to male
abcd_pre_imputation$sex <- relevel(abcd_pre_imputation$sex, ref = "M")

# Recode race reference level to White
abcd_pre_imputation$race <- relevel(abcd_pre_imputation$race, ref = "White")
abcd_pre_imputation$race.r <- relevel(abcd_pre_imputation$race.r, ref = "White")

## Check prevalence of missing data

# ACE dimensions
summary(abcd_pre_imputation$Traumatic_Events, useNA="always") 
summary(abcd_pre_imputation$Parental_Threat, useNA="always") 
summary(abcd_pre_imputation$Deprivation, useNA="always") 
summary(abcd_pre_imputation$Victimisation, useNA="always") 

# Adolescent mental health outcomes
summary(abcd_pre_imputation$cbcl_internalising, useNA="always")
summary(abcd_pre_imputation$cbcl_externalising, useNA="always")

#------------------------- Check linear regression assumptions ------------------------------------# 

# Check correlations between factor scores to ensure no collinearity
cor(abcd_pre_imputation[, c("Traumatic_Events", "Parental_Threat", "Deprivation", "Victimisation")], use = "complete.obs")

# Check whether the outcome variables follow a normal distribution
hist(abcd_pre_imputation$cbcl_internalising)
hist(abcd_pre_imputation$cbcl_externalising)
# Distributions are skewed to the left, but this makes sense because a higher score represents worse mental health

# Standardise predictor and outcome variables
abcd_pre_imputation$Traumatic_Events <- scale(abcd_pre_imputation$Traumatic_Events)
abcd_pre_imputation$Parental_Threat <- scale(abcd_pre_imputation$Parental_Threat)
abcd_pre_imputation$Deprivation <- scale(abcd_pre_imputation$Deprivation)
abcd_pre_imputation$Victimisation <- scale(abcd_pre_imputation$Victimisation)

abcd_pre_imputation$cbcl_internalising <- scale(abcd_pre_imputation$cbcl_internalising)
abcd_pre_imputation$cbcl_externalising <- scale(abcd_pre_imputation$cbcl_externalising)

# Now the mean is 0 and standard deviation is 1
describe(abcd_pre_imputation$Traumatic_Events) 
describe(abcd_pre_imputation$Parental_Threat) 
describe(abcd_pre_imputation$Deprivation) 
describe(abcd_pre_imputation$Victimisation) 

describe(abcd_pre_imputation$cbcl_internalising) 
describe(abcd_pre_imputation$cbcl_externalising) 

#------------------------- Unadjusted Regression Models Part 1: one factor and the outcomes -------------------------#

# Internalising symptoms ~ Traumatic Events
trauma_int <- lm(cbcl_internalising ~ Traumatic_Events, data = abcd_pre_imputation)
summary(trauma_int)

# Externalising symptoms ~ Traumatic Events
trauma_ext <- lm(cbcl_externalising ~ Traumatic_Events, data = abcd_pre_imputation)
summary(trauma_ext)

# Internalising symptoms ~ Parental Threat
threat_int <- lm(cbcl_internalising ~ Parental_Threat, data = abcd_pre_imputation)
summary(threat_int)

# Externalising symptoms ~ Parental Threat
threat_ext <- lm(cbcl_externalising ~ Parental_Threat, data = abcd_pre_imputation)
summary(threat_ext)

# Internalising symptoms ~ Deprivation
dep_int <- lm(cbcl_internalising ~ Deprivation, data = abcd_pre_imputation)
summary(dep_int)

# Externalising symptoms ~ Deprivation
dep_ext <- lm(cbcl_externalising ~ Deprivation, data = abcd_pre_imputation)
summary(dep_ext)

# Internalising symptoms ~ Victimisation
vic_int <- lm(cbcl_internalising ~ Victimisation, data = abcd_pre_imputation)
summary(vic_int)

# Externalising symptoms ~ Victimisation
vic_ext <- lm(cbcl_externalising ~ Victimisation, data = abcd_pre_imputation)
summary(vic_ext)

#------------------------- Adjusted Regression Models Part 2: one factor + covariates and the outcomes -------------------------#

# Internalising symptoms ~ Traumatic Events + Sex + Race
int1 <- lm(cbcl_internalising ~ Traumatic_Events + sex + race.r, data = abcd_pre_imputation)
summary(int1)

# Externalising symptoms ~ Traumatic Events + Sex + Race
ext1 <- lm(cbcl_externalising ~ Traumatic_Events + sex + race.r, data = abcd_pre_imputation)
summary(ext1)

# Internalising symptoms ~ Parental Threat + Sex + Race
int2 <- lm(cbcl_internalising ~ Parental_Threat + sex + race.r, data = abcd_pre_imputation)
summary(int2)

# Externalising symptoms ~ Parental Threat + Sex + Race
ext2 <- lm(cbcl_externalising ~ Parental_Threat + sex + race.r, data = abcd_pre_imputation)
summary(ext2)

# Internalising symptoms ~ Deprivation + Sex + Race
int3 <- lm(cbcl_internalising ~ Deprivation + sex + race.r, data = abcd_pre_imputation)
summary(int3)

# Externalising symptoms ~ Deprivation + Sex + Race
ext3 <- lm(cbcl_externalising ~ Deprivation + sex + race.r, data = abcd_pre_imputation)
summary(ext3)

# Internalising symptoms ~ Victimisation + Sex + Race
int4 <- lm(cbcl_internalising ~ Victimisation + sex + race.r, data = abcd_pre_imputation)
summary(int4)

# Externalising symptoms ~ Victimisation + Sex + Race
ext4 <- lm(cbcl_externalising ~ Victimisation + sex + race.r, data = abcd_pre_imputation)
summary(ext4)

#----------------- Regression Model Part 3: all factors + covariates and the outcomes (multiple regression) -----------------#

multireg_lm <- lm(cbind(cbcl_internalising, cbcl_externalising) ~ Traumatic_Events + Parental_Threat + Deprivation + Victimisation + sex + race.r, data = abcd_pre_imputation)
summary(multireg_lm)

mean(multireg_lm$residuals) # check the mean of residuals is approximately zero
qqnorm(multireg_lm$residuals) # check the residuals are normally distributed

# Find number of observations used in regression (i.e., complete case sample)
nobs(multireg_lm)

# Calculate variance inflation factor (VIF)
# VIF is a measure to analyze the magnitude of multicollinearity of model terms
# VIF less than 5 indicates a low correlation of that predictor with other predictors
library(performance)
library(car)

# Internalising symptoms
multi_int <- lm(cbcl_internalising ~ Traumatic_Events + Parental_Threat + Deprivation + Victimisation + sex + race.r, data = abcd_pre_imputation)
summary(multi_int)

check_collinearity(multi_int)
vif(multi_int)

# Externalising symptoms
multi_ext <- lm(cbcl_externalising ~ Traumatic_Events + Parental_Threat + Deprivation + Victimisation + sex + race.r, data = abcd_pre_imputation)
summary(multi_ext)

check_collinearity(multi_ext)
vif(multi_ext)

#------------------------- Format regression tables into word document -------------------------#
library(stargazer)

## Format unadjusted regression models 
# Internalising symptoms
int_unadjusted <- stargazer(trauma_int, threat_int, dep_int, vic_int,
                     type="html",
                     title="Unadjusted associations between ACE dimensions and internalising symptoms for the ABCD complete sample",
                     covariate.labels=c("Traumatic Events", "Parental Threat", "Deprivation", "Victimisation"),
                     column.labels=c("Traumatic Events ~ Internalising", "Parental Threat ~ Internalising", "Deprivation ~ Internalising", "Victimisation ~ Internalising"),
                     column.sep.width="15pt",
                     dep.var.caption="Internalising symptoms",
                     dep.var.labels="",
                     ci=TRUE,
                     ci.level=0.95,
                     align=TRUE,
                     out="abcd_unadjusted_internalising.doc",
                     digits=2, digits.extra=3,
                     omit="Constant",
                     single.row=TRUE)

# Externalising symptoms
ext_unadjusted <- stargazer(trauma_ext, threat_ext, dep_ext, vic_ext,
                     type="html",
                     title="Unadjusted associations between ACE dimensions and externalising symptoms for the ABCD complete sample",
                     covariate.labels=c("Traumatic Events", "Parental Threat", "Deprivation", "Victimisation"),
                     column.labels=c("Traumatic Events ~ Externalising", "Parental Threat ~ Externalising", "Deprivation ~ Externalising", "Victimisation ~ Externalising"),
                     column.sep.width="15pt",
                     dep.var.caption="Externalising symptoms",
                     dep.var.labels="",
                     ci=TRUE,
                     ci.level=0.95,
                     align=TRUE,
                     out="abcd_unadjusted_externalising.doc",
                     digits=2, digits.extra=3,
                     omit="Constant",
                     single.row=TRUE)

## Format adjusted regression models 
# Internalising symptoms
int_adjusted <- stargazer(int1, int2, int3, int4, multi_int,
                     type="html",
                     title="Adjusted associations between ACE dimensions and internalising symptoms for the ABCD complete sample",
                     covariate.labels=c("Traumatic Events", "Parental Threat", "Deprivation", "Victimisation",
                                        "Female sex", "American Indian/Alaska Native", "Asian", 
                                        "Black/African American", "Native Hawaiian/Pacific Islander", "Other"),
                     column.labels=c("Traumatic Events ~ Internalising", "Parental Threat ~ Internalising", "Deprivation ~ Internalising", "Victimisation ~ Internalising", 
                                     "Traumatic Events + Parental Threat + Deprivation + Victimisation ~ Internalising"),
                     column.sep.width="15pt",
                     dep.var.caption="Internalising symptoms",
                     dep.var.labels="",
                     ci=TRUE,
                     ci.level=0.95,
                     align=TRUE,
                     out="abcd_adjusted_internalising.doc",
                     digits=2, digits.extra=3,
                     omit="Constant",
                     single.row=TRUE)

# Externalising symptoms
ext_adjusted <- stargazer(ext1, ext2, ext3, ext4, multi_ext,
                     type="html",
                     title="Adjusted associations between ACE dimensions and externalising symptoms for the ABCD complete sample",
                     covariate.labels=c("Traumatic Events", "Parental Threat", "Deprivation", "Victimisation",
                                        "Female sex", "American Indian/Alaska Native", "Asian", 
                                        "Black/African American", "Native Hawaiian/Pacific Islander", "Other"),
                     column.labels=c("Traumatic Events ~ Externalising", "Parental Threat ~ Externalising", "Deprivation ~ Externalising", "Victimisation ~ Externalising", 
                                     "Traumatic Events + Parental Threat + Deprivation + Victimisation ~ Externalising"),
                     column.sep.width="15pt",
                     dep.var.caption="Externalising symptoms",
                     dep.var.labels="",
                     ci=TRUE,
                     ci.level=0.95,
                     align=TRUE,
                     out="abcd_adjusted_externalising.doc",
                     digits=2, digits.extra=3,
                     omit="Constant",
                     single.row=TRUE)

#------------------------- Exploratory analysis: test for interactions and stratify analyses by sex  -------------------------#

# Test if there is an interaction between sex and main effects

# Internalising symptoms
multireg_int_interaction <- lm(cbcl_internalising ~ Traumatic_Events + Parental_Threat + Deprivation + Victimisation + sex + race.r + sex*Traumatic_Events + sex*Parental_Threat + sex*Deprivation + sex*Victimisation, data = abcd_pre_imputation)
summary(multireg_int_interaction)
# There are no significant interactions for internalising symptoms

# Externalising symptoms
multireg_ext_interaction <- lm(cbcl_externalising ~ Traumatic_Events + Parental_Threat + Deprivation + Victimisation + sex + race.r + sex*Traumatic_Events + sex*Parental_Threat + sex*Deprivation + sex*Victimisation, data = abcd_pre_imputation)
summary(multireg_ext_interaction)
# There is a significant interaction between sex*Parental_Threat for externalising symptoms

# Recode sex reference level to female and retest interaction 
abcd_pre_imputation$sex <- relevel(abcd_pre_imputation$sex, ref = "F")

multireg_ext_interaction <- lm(cbcl_externalising ~ Traumatic_Events + Parental_Threat + Deprivation + Victimisation + sex + race.r + sex*Traumatic_Events + sex*Parental_Threat + sex*Deprivation + sex*Victimisation, data = abcd_pre_imputation)
summary(multireg_ext_interaction)
# Boys who experienced parental threat are more likely to have worse externalising symptoms 

## Stratify dataset by sex and rerun analyses
abcd_male <- subset(abcd_pre_imputation, abcd_pre_imputation$sex=="M") 
abcd_female <- subset(abcd_pre_imputation, abcd_pre_imputation$sex=="F") 

# Males: Externalising symptoms ~ Traumatic Events + Parental Threat + Deprivation + Victimisation + Race
multi_ext_m <- lm(cbcl_externalising ~ Traumatic_Events + Parental_Threat + Deprivation + Victimisation + race.r, data = abcd_male)
summary(multi_ext_m)

# Females: Externalising symptoms ~ Traumatic Events + Parental Threat + Deprivation + Victimisation + Race
multi_ext_f <- lm(cbcl_externalising ~ Traumatic_Events + Parental_Threat + Deprivation + Victimisation + race.r, data = abcd_female)
summary(multi_ext_f)
```

# 4. Multiple imputation

We will conduct multiple imputation using the missForest package, a
random forest imputation algorithm for missing data.

``` r
library(missForest)
library(doParallel)

#------------------------- Impute, then transform  -------------------------#
# Raw variables used to create the derived variables will be imputed individually, 
# and after imputation the variables will be re-derived from the imputed dataset

# We will work from the wide format dataset abcd_wide (from code line 401 to 627)
# This is the dataset containing raw variables, before composite variables for ACEs and internalising/externalising outcomes are derived

### Prepare dataset for imputation

# We only want baseline measures for auxiliary variables, because they have the most complete data
abcd_wide$sex <- abcd_wide$sex.baseline_year_1_arm_1

abcd_wide$race <- abcd_wide$race.baseline_year_1_arm_1

abcd_wide$employment <- abcd_wide$employment.baseline_year_1_arm_1

abcd_wide$high_educ <- abcd_wide$high.educ.baseline_year_1_arm_1

abcd_wide$household_size <- abcd_wide$household_size.baseline_year_1_arm_1

abcd_wide$mat_age_birth <- abcd_wide$mat_age_birth.baseline_year_1_arm_1

abcd_wide$birthweight <- abcd_wide$birth_weight_lbs.baseline_year_1_arm_1

abcd_wide$premature_birth <- abcd_wide$premature_birth.baseline_year_1_arm_1

abcd_wide$maternal_smoking_bef_preg <- abcd_wide$maternal_smoking_bef_preg.baseline_year_1_arm_1

abcd_wide$maternal_smoking_aft_preg <- abcd_wide$maternal_smoking_aft_preg.baseline_year_1_arm_1

abcd_wide$maternal_alcohol_bef_preg <- abcd_wide$maternal_alcohol_bef_preg.baseline_year_1_arm_1

abcd_wide$maternal_alcohol_aft_preg <- abcd_wide$maternal_alcohol_aft_preg.baseline_year_1_arm_1

abcd_wide$preg_complications <- abcd_wide$preg_complications.baseline_year_1_arm_1

abcd_wide$diff_afford_food <- abcd_wide$diff_afford_food.baseline_year_1_arm_1

abcd_wide$diff_afford_gas <- abcd_wide$diff_afford_gas.baseline_year_1_arm_1

abcd_wide$evicted_home <- abcd_wide$evicted_home.baseline_year_1_arm_1

# Parent AESBA mental health scores were measured at baseline and 2 year follow up
# We only want baseline data because 2 year follow up has more missing data 
abcd_wide$par_anxious_depressed <- abcd_wide$par_anxious_depressed.baseline_year_1_arm_1

abcd_wide$par_withdrawn <- abcd_wide$par_withdrawn.baseline_year_1_arm_1

abcd_wide$par_somatic_complaints <- abcd_wide$par_somatic_complaints.baseline_year_1_arm_1

abcd_wide$par_thought_problems <- abcd_wide$par_thought_problems.baseline_year_1_arm_1

abcd_wide$par_attention_problems <- abcd_wide$par_attention_problems.baseline_year_1_arm_1

abcd_wide$par_aggress_behav <- abcd_wide$par_aggress_behav.baseline_year_1_arm_1

# Recode categorical auxiliary variables to have fewer categories

# Parental employement will be recoded to working or not (yes/no)
table(abcd_wide$employment, useNA="always")
abcd_wide$employment <- as.factor(ifelse(abcd_wide$employment=="Working now",0,1))
table(abcd_wide$employment, useNA="always")

# Condense race variable to six levels: White, Black/African American, American Indian/Alaska Native, Asian, Native Hawaiian/Other Pacific Islander, Other 
# Following guidance from US Census Bureau: https://www.census.gov/quickfacts/fact/note/US/RHI625221
table(abcd_wide$race, useNA = "always")
abcd_wide$race <- car::recode(abcd_wide$race, "'Alaska Native'='American Indian/Alaska Native'; 'Asian Indian'='Asian'; 'Chinese'='Asian'; 'Filipino'='Asian'; 'Guamanian'='Native Hawaiian/Pacific Islander'; 'Japanese'='Asian'; 'Korean'='Asian'; 'Native American'='American Indian/Alaska Native'; 'Native Hawaiian'='Native Hawaiian/Pacific Islander'; 'Other Asian'='Asian'; 'Other Pacific Islander'='Native Hawaiian/Pacific Islander'; 'Other Race'='Other'; 'Samoan'='Native Hawaiian/Pacific Islander'; 'Vietnamese'='Asian'")
table(abcd_wide$race, useNA = "always")
prop.table(table(abcd_wide$race, useNA = "always"))*100

# Recode responses of 999 ("Do not know") or 7 ("Refuse to answer") to NA

# Parental psychopathology (baseline)
abcd_wide$fam_history_6_yes_no.baseline_year_1_arm_1[abcd_wide$fam_history_6_yes_no.baseline_year_1_arm_1=="999" | abcd_wide$fam_history_6_yes_no.baseline_year_1_arm_1=="7"] <- NA
abcd_wide$fam_history_7_yes_no.baseline_year_1_arm_1[abcd_wide$fam_history_7_yes_no.baseline_year_1_arm_1=="999" | abcd_wide$fam_history_7_yes_no.baseline_year_1_arm_1=="7"] <- NA
abcd_wide$fam_history_8_yes_no.baseline_year_1_arm_1[abcd_wide$fam_history_8_yes_no.baseline_year_1_arm_1=="999" | abcd_wide$fam_history_8_yes_no.baseline_year_1_arm_1=="7"] <- NA
abcd_wide$fam_history_13_yes_no.baseline_year_1_arm_1[abcd_wide$fam_history_13_yes_no.baseline_year_1_arm_1=="999" | abcd_wide$fam_history_13_yes_no.baseline_year_1_arm_1=="7"] <- NA
abcd_wide$fam_history_q6a_depression.baseline_year_1_arm_1[abcd_wide$fam_history_q6a_depression.baseline_year_1_arm_1=="999"] <- NA
abcd_wide$fam_history_q6d_depression.baseline_year_1_arm_1[abcd_wide$fam_history_q6d_depression.baseline_year_1_arm_1=="999"] <- NA
abcd_wide$fam_history_q7a_mania.baseline_year_1_arm_1[abcd_wide$fam_history_q7a_mania.baseline_year_1_arm_1=="999"] <- NA
abcd_wide$fam_history_q7d_mania.baseline_year_1_arm_1[abcd_wide$fam_history_q7d_mania.baseline_year_1_arm_1=="999"] <- NA
abcd_wide$fam_history_q8a_visions.baseline_year_1_arm_1[abcd_wide$fam_history_q8a_visions.baseline_year_1_arm_1=="999"] <- NA
abcd_wide$fam_history_q8d_visions.baseline_year_1_arm_1[abcd_wide$fam_history_q8d_visions.baseline_year_1_arm_1=="999"] <- NA
abcd_wide$fam_history_q13a_suicide.baseline_year_1_arm_1[abcd_wide$fam_history_q13a_suicide.baseline_year_1_arm_1=="999"] <- NA
abcd_wide$fam_history_q13d_suicide.baseline_year_1_arm_1[abcd_wide$fam_history_q13d_suicide.baseline_year_1_arm_1=="999"] <- NA

# Parental alcohol abuse (baseline)
abcd_wide$famhx_4_p.baseline_year_1_arm_1[abcd_wide$famhx_4_p.baseline_year_1_arm_1=="7" | abcd_wide$famhx_4_p.baseline_year_1_arm_1=="999"] <- NA 

# Parental drug abuse (baseline)
abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1[abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1=="7" | abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1=="999"] <- NA
table(abcd_wide$fam_history_5_yes_no.baseline_year_1_arm_1, useNA="always")

# Parental separation (baseline, 1 year follow up, 2 year follow up)
abcd_wide$demo_prnt_marital_v2.baseline_year_1_arm_1[abcd_wide$demo_prnt_marital_v2.baseline_year_1_arm_1=="777"] <- NA
abcd_wide$demo_prnt_marital_v2_l.1_year_follow_up_y_arm_1[abcd_wide$demo_prnt_marital_v2_l.1_year_follow_up_y_arm_1=="777"] <- NA
abcd_wide$demo_prnt_marital_v2_l.2_year_follow_up_y_arm_1[abcd_wide$demo_prnt_marital_v2_l.2_year_follow_up_y_arm_1=="777"] <- NA

# Cyber victimisation (2 year follow up)
abcd_wide$cybb_phenx_harm.2_year_follow_up_y_arm_1[abcd_wide$cybb_phenx_harm.2_year_follow_up_y_arm_1=="777"] <- NA 

# Low household income (baseline, 1 year follow up, 2 year follow up)
abcd_wide$demo_comb_income_v2.baseline_year_1_arm_1[abcd_wide$demo_comb_income_v2.baseline_year_1_arm_1=="777" | abcd_wide$demo_comb_income_v2.baseline_year_1_arm_1=="999"] <- NA 
abcd_wide$demo_comb_income_v2_l.1_year_follow_up_y_arm_1[abcd_wide$demo_comb_income_v2_l.1_year_follow_up_y_arm_1=="777" | abcd_wide$demo_comb_income_v2_l.1_year_follow_up_y_arm_1=="999"] <- NA 
abcd_wide$demo_comb_income_v2_l.2_year_follow_up_y_arm_1[abcd_wide$demo_comb_income_v2_l.2_year_follow_up_y_arm_1=="777" | abcd_wide$demo_comb_income_v2_l.2_year_follow_up_y_arm_1=="999"] <- NA 

dim(abcd_wide)
colnames(abcd_wide)

# Subset to a reduced dataset only including the required variables for imputation 
required_vars <- names(abcd_wide) %in% c("src_subject_id",
         ### ACEs
         # KSADS PTSD at baseline and 2 year follow up
         "ksads_ptsd_raw_762_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_762_p.2_year_follow_up_y_arm_1", 
         "ksads_ptsd_raw_763_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_763_p.2_year_follow_up_y_arm_1",
         "ksads_ptsd_raw_764_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_764_p.2_year_follow_up_y_arm_1", 
         "ksads_ptsd_raw_765_p.baseline_year_1_arm_1",
         "ksads_ptsd_raw_765_p.2_year_follow_up_y_arm_1",
         "ksads_ptsd_raw_767_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_767_p.2_year_follow_up_y_arm_1", 
         "ksads_ptsd_raw_768_p.baseline_year_1_arm_1",
         "ksads_ptsd_raw_768_p.2_year_follow_up_y_arm_1", 
         "ksads_ptsd_raw_769_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_769_p.2_year_follow_up_y_arm_1",
         "ksads_ptsd_raw_766_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_766_p.2_year_follow_up_y_arm_1",
         "ksads_ptsd_raw_754_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_754_p.2_year_follow_up_y_arm_1", 
         "ksads_ptsd_raw_755_p.baseline_year_1_arm_1",
         "ksads_ptsd_raw_755_p.2_year_follow_up_y_arm_1",
         "ksads_ptsd_raw_756_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_756_p.2_year_follow_up_y_arm_1", 
         "ksads_ptsd_raw_757_p.baseline_year_1_arm_1",
         "ksads_ptsd_raw_757_p.2_year_follow_up_y_arm_1",
         "ksads_ptsd_raw_758_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_758_p.2_year_follow_up_y_arm_1", 
         "ksads_ptsd_raw_759_p.baseline_year_1_arm_1",
         "ksads_ptsd_raw_759_p.2_year_follow_up_y_arm_1", 
         "ksads_ptsd_raw_760_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_760_p.2_year_follow_up_y_arm_1",
         "ksads_ptsd_raw_770_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_770_p.2_year_follow_up_y_arm_1",
         # CRPBI emotional neglect at baseline and 1 year follow up
         "crpbi_parent1_y.baseline_year_1_arm_1",
         "crpbi_parent1_y.1_year_follow_up_y_arm_1",
         "crpbi_parent2_y.baseline_year_1_arm_1", 
         "crpbi_parent2_y.1_year_follow_up_y_arm_1",
         "crpbi_parent3_y.baseline_year_1_arm_1", 
         "crpbi_parent3_y.1_year_follow_up_y_arm_1",
         "crpbi_parent4_y.baseline_year_1_arm_1", 
         "crpbi_parent4_y.1_year_follow_up_y_arm_1",
         "crpbi_parent5_y.baseline_year_1_arm_1", 
         "crpbi_parent5_y.1_year_follow_up_y_arm_1",
         # Family history parental psychopathology at baseline 
         "fam_history_6_yes_no.baseline_year_1_arm_1", 
         "fam_history_7_yes_no.baseline_year_1_arm_1", 
         "fam_history_8_yes_no.baseline_year_1_arm_1", 
         "fam_history_13_yes_no.baseline_year_1_arm_1",
         "fam_history_q6a_depression.baseline_year_1_arm_1",
         "fam_history_q6d_depression.baseline_year_1_arm_1",
         "fam_history_q7a_mania.baseline_year_1_arm_1",
         "fam_history_q7d_mania.baseline_year_1_arm_1",
         "fam_history_q8a_visions.baseline_year_1_arm_1",
         "fam_history_q8d_visions.baseline_year_1_arm_1",
         "fam_history_q13a_suicide.baseline_year_1_arm_1",
         "fam_history_q13d_suicide.baseline_year_1_arm_1",
         # ASR parental psychopathology at baseline and 2 year follow up
         "asr_scr_depress_t.baseline_year_1_arm_1", 
         "asr_scr_depress_t.2_year_follow_up_y_arm_1", 
         "asr_scr_anxdisord_t.baseline_year_1_arm_1", 
         "asr_scr_anxdisord_t.2_year_follow_up_y_arm_1", 
         "asr_scr_adhd_t.baseline_year_1_arm_1", 
         "asr_scr_adhd_t.2_year_follow_up_y_arm_1",
         # Family history parental alcohol abuse at baseline
         "famhx_4_p.baseline_year_1_arm_1",
         "famhx4a_p___1.baseline_year_1_arm_1",
         "famhx4a_p___2.baseline_year_1_arm_1",
         "famhx4a_p___3.baseline_year_1_arm_1",
         "famhx4a_p___4.baseline_year_1_arm_1",
         "famhx4a_p___6.baseline_year_1_arm_1",
         "famhx4a_p___7.baseline_year_1_arm_1",
         "famhx_4d_p___1.baseline_year_1_arm_1",
         "famhx_4d_p___2.baseline_year_1_arm_1",
         "famhx_4d_p___3.baseline_year_1_arm_1",
         "famhx_4d_p___4.baseline_year_1_arm_1",
         "famhx_4d_p___6.baseline_year_1_arm_1",
         "famhx_4d_p___7.baseline_year_1_arm_1",
         # Family history parental drug abuse at baseline
         "fam_history_5_yes_no.baseline_year_1_arm_1",
         "fam_history_q5a_drugs___1.baseline_year_1_arm_1",
         "fam_history_q5a_drugs___2.baseline_year_1_arm_1",
         "fam_history_q5a_drugs___3.baseline_year_1_arm_1",
         "fam_history_q5a_drugs___4.baseline_year_1_arm_1",
         "fam_history_q5a_drugs___6.baseline_year_1_arm_1",
         "fam_history_q5a_drugs___7.baseline_year_1_arm_1",
         "fam_history_q5d_drugs___1.baseline_year_1_arm_1",
         "fam_history_q5d_drugs___2.baseline_year_1_arm_1",
         "fam_history_q5d_drugs___3.baseline_year_1_arm_1",
         "fam_history_q5d_drugs___4.baseline_year_1_arm_1",
         "fam_history_q5d_drugs___6.baseline_year_1_arm_1",
         "fam_history_q5d_drugs___7.baseline_year_1_arm_1",
         # ASR parental drug abuse at baseline and 2 year follow up
         "asr_q126_p.baseline_year_1_arm_1",
         "asr_q126_p.2_year_follow_up_y_arm_1",
         # Parental criminality at 1 year and 2 year follow up
         "ple_arrest_p.1_year_follow_up_y_arm_1", 
         "ple_arrest_past_yr_p.1_year_follow_up_y_arm_1",
         "ple_arrest_p.2_year_follow_up_y_arm_1", 
         "ple_arrest_past_yr_p.2_year_follow_up_y_arm_1",
         "ple_law_p.1_year_follow_up_y_arm_1", 
         "ple_law_past_yr_p.1_year_follow_up_y_arm_1",
         "ple_law_p.2_year_follow_up_y_arm_1", 
         "ple_law_past_yr_p.2_year_follow_up_y_arm_1",
         "ple_jail_p.1_year_follow_up_y_arm_1", 
         "ple_jail_past_yr_p.1_year_follow_up_y_arm_1",
         "ple_jail_p.2_year_follow_up_y_arm_1",
         "ple_jail_past_yr_p.2_year_follow_up_y_arm_1",
         # Parental separation at baseline, 1 year follow up, 2 year follow up
         "demo_prnt_marital_v2.baseline_year_1_arm_1",
         "demo_prnt_marital_v2_l.1_year_follow_up_y_arm_1",
         "demo_prnt_marital_v2_l.2_year_follow_up_y_arm_1",
         "ple_separ_p.1_year_follow_up_y_arm_1",
         "ple_separ_p.2_year_follow_up_y_arm_1",
         "ple_separ_past_yr_p.1_year_follow_up_y_arm_1",
         "ple_separ_past_yr_p.2_year_follow_up_y_arm_1",
         # Peer victimisation at 2 year follow up
         "peq_left_out_vic.2_year_follow_up_y_arm_1",
         "peq_chase_vic.2_year_follow_up_y_arm_1",
         "peq_rumor_vic.2_year_follow_up_y_arm_1",
         "peq_invite_vic.2_year_follow_up_y_arm_1",
         "peq_exclude_vic.2_year_follow_up_y_arm_1",
         "peq_gossip_vic.2_year_follow_up_y_arm_1",
         "peq_threat_vic.2_year_follow_up_y_arm_1",
         "peq_loser_vic.2_year_follow_up_y_arm_1",
         "peq_hit_vic.2_year_follow_up_y_arm_1",
         # Cyber victimisation at 2 year follow up
         "cybb_phenx_harm.2_year_follow_up_y_arm_1",
         "cybb_phenx_harm_12mo.2_year_follow_up_y_arm_1",
         "cybb_phenx_harm_often.2_year_follow_up_y_arm_1",
         # Unsafe neighbourhood at baseline, 1 year follow up, 2 year follow up
         "neighborhood1r_p.baseline_year_1_arm_1",
         "neighborhood1r_p.1_year_follow_up_y_arm_1",
         "neighborhood1r_p.2_year_follow_up_y_arm_1",
         "neighborhood2r_p.baseline_year_1_arm_1",
         "neighborhood2r_p.1_year_follow_up_y_arm_1",
         "neighborhood2r_p.2_year_follow_up_y_arm_1",
         "neighborhood3r_p.baseline_year_1_arm_1",
         "neighborhood3r_p.1_year_follow_up_y_arm_1",
         "neighborhood3r_p.2_year_follow_up_y_arm_1",
         "neighborhood_crime_y.baseline_year_1_arm_1",
         "neighborhood_crime_y.1_year_follow_up_y_arm_1",
         "neighborhood_crime_y.2_year_follow_up_y_arm_1",
         # Low household income at baseline, 1 year follow up, 2 year follow up
         "demo_comb_income_v2.baseline_year_1_arm_1", 
         "demo_comb_income_v2_l.1_year_follow_up_y_arm_1", 
         "demo_comb_income_v2_l.2_year_follow_up_y_arm_1",
         ### Outcome variables
         # CBCL internalising
         "cbcl_q14_p.3_year_follow_up_y_arm_1", "cbcl_q29_p.3_year_follow_up_y_arm_1", 
         "cbcl_q30_p.3_year_follow_up_y_arm_1", "cbcl_q31_p.3_year_follow_up_y_arm_1", 
         "cbcl_q32_p.3_year_follow_up_y_arm_1", "cbcl_q33_p.3_year_follow_up_y_arm_1", 
         "cbcl_q35_p.3_year_follow_up_y_arm_1", "cbcl_q45_p.3_year_follow_up_y_arm_1", 
         "cbcl_q50_p.3_year_follow_up_y_arm_1", "cbcl_q52_p.3_year_follow_up_y_arm_1", 
         "cbcl_q71_p.3_year_follow_up_y_arm_1", "cbcl_q91_p.3_year_follow_up_y_arm_1",
         "cbcl_q112_p.3_year_follow_up_y_arm_1", "cbcl_q05_p.3_year_follow_up_y_arm_1", 
         "cbcl_q42_p.3_year_follow_up_y_arm_1", "cbcl_q65_p.3_year_follow_up_y_arm_1",
         "cbcl_q69_p.3_year_follow_up_y_arm_1", "cbcl_q75_p.3_year_follow_up_y_arm_1", 
         "cbcl_q102_p.3_year_follow_up_y_arm_1", "cbcl_q103_p.3_year_follow_up_y_arm_1",
         "cbcl_q111_p.3_year_follow_up_y_arm_1", "cbcl_q47_p.3_year_follow_up_y_arm_1", 
         "cbcl_q49_p.3_year_follow_up_y_arm_1", "cbcl_q51_p.3_year_follow_up_y_arm_1", 
         "cbcl_q54_p.3_year_follow_up_y_arm_1", "cbcl_q56a_p.3_year_follow_up_y_arm_1", 
         "cbcl_q56b_p.3_year_follow_up_y_arm_1", "cbcl_q56c_p.3_year_follow_up_y_arm_1",
         "cbcl_q56d_p.3_year_follow_up_y_arm_1", "cbcl_q56e_p.3_year_follow_up_y_arm_1", 
         "cbcl_q56f_p.3_year_follow_up_y_arm_1", "cbcl_q56g_p.3_year_follow_up_y_arm_1",
         # CBCL externalising
         "cbcl_q02_p.3_year_follow_up_y_arm_1", "cbcl_q26_p.3_year_follow_up_y_arm_1",
         "cbcl_q28_p.3_year_follow_up_y_arm_1", "cbcl_q39_p.3_year_follow_up_y_arm_1",
         "cbcl_q43_p.3_year_follow_up_y_arm_1", "cbcl_q63_p.3_year_follow_up_y_arm_1", 
         "cbcl_q67_p.3_year_follow_up_y_arm_1", "cbcl_q72_p.3_year_follow_up_y_arm_1", 
         "cbcl_q73_p.3_year_follow_up_y_arm_1", "cbcl_q81_p.3_year_follow_up_y_arm_1", 
         "cbcl_q82_p.3_year_follow_up_y_arm_1", "cbcl_q90_p.3_year_follow_up_y_arm_1", 
         "cbcl_q96_p.3_year_follow_up_y_arm_1", "cbcl_q99_p.3_year_follow_up_y_arm_1", 
         "cbcl_q101_p.3_year_follow_up_y_arm_1", "cbcl_q105_p.3_year_follow_up_y_arm_1", 
         "cbcl_q106_p.3_year_follow_up_y_arm_1", "cbcl_q03_p.3_year_follow_up_y_arm_1", 
         "cbcl_q16_p.3_year_follow_up_y_arm_1","cbcl_q19_p.3_year_follow_up_y_arm_1", 
         "cbcl_q20_p.3_year_follow_up_y_arm_1", "cbcl_q21_p.3_year_follow_up_y_arm_1", 
         "cbcl_q22_p.3_year_follow_up_y_arm_1", "cbcl_q23_p.3_year_follow_up_y_arm_1", 
         "cbcl_q37_p.3_year_follow_up_y_arm_1", "cbcl_q57_p.3_year_follow_up_y_arm_1",
         "cbcl_q68_p.3_year_follow_up_y_arm_1", "cbcl_q86_p.3_year_follow_up_y_arm_1", 
         "cbcl_q87_p.3_year_follow_up_y_arm_1", "cbcl_q88_p.3_year_follow_up_y_arm_1", 
         "cbcl_q89_p.3_year_follow_up_y_arm_1", "cbcl_q94_p.3_year_follow_up_y_arm_1",
         "cbcl_q95_p.3_year_follow_up_y_arm_1",  "cbcl_q97_p.3_year_follow_up_y_arm_1",
         "cbcl_q104_p.3_year_follow_up_y_arm_1", "cbcl_q01_p.3_year_follow_up_y_arm_1", 
         "cbcl_q04_p.3_year_follow_up_y_arm_1", "cbcl_q08_p.3_year_follow_up_y_arm_1", 
         "cbcl_q10_p.3_year_follow_up_y_arm_1", "cbcl_q13_p.3_year_follow_up_y_arm_1", 
         "cbcl_q17_p.3_year_follow_up_y_arm_1", "cbcl_q41_p.3_year_follow_up_y_arm_1", 
         "cbcl_q61_p.3_year_follow_up_y_arm_1", "cbcl_q78_p.3_year_follow_up_y_arm_1", 
         "cbcl_q80_p.3_year_follow_up_y_arm_1",
         ### Auxiliary variables that are associated with both missingness and ACEs
         # Following guidance from Houtepen et al. (2018): https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6281007/
         "sex",
         "race", 
         "employment", 
         "high_educ", 
         "household_size", 
         "mat_age_birth",
         "birthweight", 
         "premature_birth", 
         "maternal_smoking_bef_preg", 
         "maternal_smoking_aft_preg", 
         "maternal_alcohol_bef_preg", 
         "maternal_alcohol_aft_preg", 
         "preg_complications", 
         "diff_afford_food", 
         "diff_afford_gas", 
         "evicted_home", 
         "par_anxious_depressed", 
         "par_withdrawn",
         "par_somatic_complaints",
         "par_thought_problems", 
         "par_attention_problems", 
         "par_aggress_behav") 

abcd_wide <- abcd_wide[required_vars]
dim(abcd_wide)
colnames(abcd_wide)

### Check average percentage of missing data 

# Percentage of missingness of the whole dataset 
pct_miss(abcd_wide) # 25.9%

# Check amount of complete data
describe(abcd_wide)$n

# Check amount of missing data per variable
colSums(is.na(abcd_wide)) 

# Find percentage of missing data per variable 
missing_pct <- as.data.frame((colMeans(is.na(abcd_wide)))*100)

missing_pct <- missing_pct %>% 
  rename(percentage_missing = "(colMeans(is.na(abcd_wide))) * 100") # rename column name

missing_pct %>% arrange(desc(percentage_missing)) # descending order

# Parental criminality, parental psychopathology, parental separation, cyber victimisation variables 
# have > 80% missingness so we will derive them as binary before entering into the imputation model
# using the same code as we did for complete case analysis

# These particular items have high missingness because they were "if... then" questions
# e.g., if respondent answered yes to ple_law_p = Parents/caregiver got into trouble with the law?
# then ple_law_past_yr_p = Did this happen in the past year? 
# e.g., if respondent answered yes to cybb_phenx_harm = have you ever been cyberbullied? 
# then cybb_phenx_harm_often = how often cyberbullied in the past 12 months?
# e.g., if respondent answered yes to fam_history_8_yes_no = had a period lasting six months when they saw visions or heard voices? 
# then fam_history_q8a_visions = biological father has psychotic experiences?

# Derive parental criminality composite variable
abcd_wide$parental_criminality <- 0
abcd_wide$parental_criminality[abcd_wide$ple_arrest_p.1_year_follow_up_y_arm_1==1 | abcd_wide$ple_arrest_past_yr_p.1_year_follow_up_y_arm_1==1 | 
                               abcd_wide$ple_arrest_p.2_year_follow_up_y_arm_1==1 | abcd_wide$ple_arrest_past_yr_p.2_year_follow_up_y_arm_1==1 | 
                               abcd_wide$ple_law_p.1_year_follow_up_y_arm_1==1 | abcd_wide$ple_law_past_yr_p.1_year_follow_up_y_arm_1==1 | 
                               abcd_wide$ple_law_p.2_year_follow_up_y_arm_1==1 | abcd_wide$ple_law_past_yr_p.2_year_follow_up_y_arm_1==1 | 
                               abcd_wide$ple_jail_p.1_year_follow_up_y_arm_1==1 | abcd_wide$ple_jail_past_yr_p.1_year_follow_up_y_arm_1==1 | 
                               abcd_wide$ple_jail_p.2_year_follow_up_y_arm_1==1 | abcd_wide$ple_jail_past_yr_p.2_year_follow_up_y_arm_1==1] <- 1 
abcd_wide$parental_criminality[apply(apply(abcd_wide[,c("ple_arrest_p.1_year_follow_up_y_arm_1", "ple_arrest_p.2_year_follow_up_y_arm_1", 
                                                      "ple_law_p.1_year_follow_up_y_arm_1", "ple_law_p.2_year_follow_up_y_arm_1", 
                                                      "ple_jail_p.1_year_follow_up_y_arm_1", "ple_jail_p.2_year_follow_up_y_arm_1")],2,is.na),1,sum) > 3] <- NA # code NA if missing > 3/6 items
table(abcd_wide$parental_criminality, useNA="always")

# Derive parental psychopathology composite variable
abcd_wide$parental_psychopathology <- 0
abcd_wide$parental_psychopathology[
    # Code as exposed if biological mother or father has depression, manic episode, psychotic experiences or suicide attempt
    # a = biological father, d = biological mother
    abcd_wide$fam_history_6_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q6a_depression.baseline_year_1_arm_1==1 |  
    abcd_wide$fam_history_6_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q6d_depression.baseline_year_1_arm_1==1 | 
    abcd_wide$fam_history_7_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q7a_mania.baseline_year_1_arm_1==1 | 
    abcd_wide$fam_history_7_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q7d_mania.baseline_year_1_arm_1==1 | 
    abcd_wide$fam_history_8_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q8a_visions.baseline_year_1_arm_1==1 | 
    abcd_wide$fam_history_8_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q8d_visions.baseline_year_1_arm_1==1 |
    abcd_wide$fam_history_13_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q13a_suicide.baseline_year_1_arm_1==1 | 
    abcd_wide$fam_history_13_yes_no.baseline_year_1_arm_1==1 & abcd_wide$fam_history_q13d_suicide.baseline_year_1_arm_1==1 |
    # Code as exposed if parent has ASR score above cut-off (63) for depression, anxiety, or ADHD
    abcd_wide$asr_scr_depress_t.baseline_year_1_arm_1 > 63 | abcd_wide$asr_scr_depress_t.2_year_follow_up_y_arm_1 > 63 |
    abcd_wide$asr_scr_anxdisord_t.baseline_year_1_arm_1 > 63 | abcd_wide$asr_scr_anxdisord_t.2_year_follow_up_y_arm_1 > 63 |
    abcd_wide$asr_scr_adhd_t.baseline_year_1_arm_1 > 63 | abcd_wide$asr_scr_adhd_t.2_year_follow_up_y_arm_1 > 63] <- 1
abcd_wide$parental_psychopathology[apply(apply(abcd_wide[,c("fam_history_6_yes_no.baseline_year_1_arm_1", "fam_history_7_yes_no.baseline_year_1_arm_1", 
                                      "fam_history_8_yes_no.baseline_year_1_arm_1", "fam_history_13_yes_no.baseline_year_1_arm_1", 
                                      "asr_scr_depress_t.baseline_year_1_arm_1", "asr_scr_depress_t.2_year_follow_up_y_arm_1", 
                                      "asr_scr_anxdisord_t.baseline_year_1_arm_1", "asr_scr_anxdisord_t.2_year_follow_up_y_arm_1", 
                                      "asr_scr_adhd_t.baseline_year_1_arm_1", "asr_scr_adhd_t.2_year_follow_up_y_arm_1")],2,is.na),1,sum) > 5] <- NA # code to missing if missing >5/10 items
table(abcd_wide$parental_psychopathology, useNA="always")

# Derive parental separation composite variable
abcd_wide$parental_separation <- 0
abcd_wide$parental_separation[abcd_wide$demo_prnt_marital_v2.baseline_year_1_arm_1==3 | # parent divorced at baseline
                                abcd_wide$demo_prnt_marital_v2.baseline_year_1_arm_1==4 | # parent separated at baseline
                                abcd_wide$demo_prnt_marital_v2_l.1_year_follow_up_y_arm_1==3 | # parent divorced at 1 year follow up
                                abcd_wide$demo_prnt_marital_v2_l.1_year_follow_up_y_arm_1==4 | # parent separated at 1 year follow up
                                abcd_wide$demo_prnt_marital_v2_l.2_year_follow_up_y_arm_1==3 | # parent divorced at 2 year follow up
                                abcd_wide$demo_prnt_marital_v2_l.2_year_follow_up_y_arm_1==4 | # parent separated at 2 year follow up
                                abcd_wide$ple_separ_p.1_year_follow_up_y_arm_1==1 | # parents separated/divorced at 1 year follow up
                                abcd_wide$ple_separ_past_yr_p.1_year_follow_up_y_arm_1==1 | # or this happened in the past year 
                                abcd_wide$ple_separ_p.2_year_follow_up_y_arm_1==1 | # parents separated/divorced at 2 year follow up
                                abcd_wide$ple_separ_past_yr_p.2_year_follow_up_y_arm_1==1] <- 1 # or this happened in the past year 
abcd_wide$parental_separation[apply(apply(abcd_wide[,c("demo_prnt_marital_v2.baseline_year_1_arm_1", 
                                                       "demo_prnt_marital_v2_l.1_year_follow_up_y_arm_1",
                                                       "demo_prnt_marital_v2_l.2_year_follow_up_y_arm_1",
                                                       "ple_separ_p.1_year_follow_up_y_arm_1",
                                                       "ple_separ_p.2_year_follow_up_y_arm_1")],2,is.na),1,sum) > 2] <- NA # code NA if missing > 2/5 items
table(abcd_wide$parental_separation, useNA="always")

# Derive cyber victimisation composite variable 
abcd_wide$cyber_victimisation <- 0
abcd_wide$cyber_victimisation[abcd_wide$cybb_phenx_harm.2_year_follow_up_y_arm_1==1 | abcd_wide$cybb_phenx_harm_12mo.2_year_follow_up_y_arm_1==1 | 
                                abcd_wide$cybb_phenx_harm_often.2_year_follow_up_y_arm_1>=5] <- 1 
abcd_wide$cyber_victimisation[apply(apply(abcd_wide[,c("cybb_phenx_harm.2_year_follow_up_y_arm_1", "cybb_phenx_harm_12mo.2_year_follow_up_y_arm_1")],2,is.na),1,sum) > 1] <- NA # code NA if missing > 1/2 items
table(abcd_wide$cyber_victimisation, useNA="always")

# Remove variables with overly high rates of missingness
drops <- c("ple_law_past_yr_p.2_year_follow_up_y_arm_1", # 97%, parents got into trouble with the law
"ple_law_past_yr_p.1_year_follow_up_y_arm_1", # 97%, parents got into trouble with the law 
"ple_jail_past_yr_p.2_year_follow_up_y_arm_1", # 97%, one of the parents went to jail
"ple_jail_past_yr_p.1_year_follow_up_y_arm_1", # 97%, one of the parents went to jail
"ple_arrest_past_yr_p.2_year_follow_up_y_arm_1", # 95%, someone in the family was arrested
"ple_arrest_past_yr_p.1_year_follow_up_y_arm_1", # 95%, someone in the family was arrested
"cybb_phenx_harm_often.2_year_follow_up_y_arm_1", # 95%, how often cyberbullied in the past 12 months
"cybb_phenx_harm_12mo.2_year_follow_up_y_arm_1", # 92%, cyberbullied in the past 12 months
"fam_history_q8a_visions.baseline_year_1_arm_1", # 90%, biological father - visions
"fam_history_q8d_visions.baseline_year_1_arm_1", # 90%, biological mother - visions
"fam_history_q7a_mania.baseline_year_1_arm_1", # 88%, biological father - mania
"fam_history_q7d_mania.baseline_year_1_arm_1", # 87%, biological mother - mania 
"ple_separ_past_yr_p.2_year_follow_up_y_arm_1", # 87%, parents separated/divorced in past year
"ple_separ_past_yr_p.1_year_follow_up_y_arm_1", # 87%, parents separated/divorced in past year
"fam_history_q13a_suicide.baseline_year_1_arm_1", # 82%, biological father - suicide
"fam_history_q13d_suicide.baseline_year_1_arm_1", # 82%, biological mother - suicide
"fam_history_q6a_depression.baseline_year_1_arm_1", # 51%, biological father - depression
"fam_history_q6d_depression.baseline_year_1_arm_1", # 49%, biological mother - depression
# Although the following variables did not have high missingness, 
# they were included to create the composite variables of parental criminality,
# parental psychopathology, parental separation, and cyber victimisation
# so they will now be removed from the imputation model 
"ple_arrest_p.1_year_follow_up_y_arm_1",
"ple_arrest_p.2_year_follow_up_y_arm_1",
"ple_law_p.1_year_follow_up_y_arm_1",
"ple_law_p.2_year_follow_up_y_arm_1",
"ple_jail_p.1_year_follow_up_y_arm_1",
"ple_jail_p.2_year_follow_up_y_arm_1",
"fam_history_6_yes_no.baseline_year_1_arm_1",
"fam_history_7_yes_no.baseline_year_1_arm_1",
"fam_history_8_yes_no.baseline_year_1_arm_1",
"fam_history_13_yes_no.baseline_year_1_arm_1",
"asr_scr_depress_t.baseline_year_1_arm_1", 
"asr_scr_depress_t.2_year_follow_up_y_arm_1", 
"asr_scr_anxdisord_t.baseline_year_1_arm_1",
"asr_scr_anxdisord_t.2_year_follow_up_y_arm_1", 
"asr_scr_adhd_t.baseline_year_1_arm_1", 
"asr_scr_adhd_t.2_year_follow_up_y_arm_1",
"demo_prnt_marital_v2.baseline_year_1_arm_1",
"demo_prnt_marital_v2_l.1_year_follow_up_y_arm_1",
"demo_prnt_marital_v2_l.2_year_follow_up_y_arm_1",
"ple_separ_p.1_year_follow_up_y_arm_1",
"ple_separ_p.2_year_follow_up_y_arm_1",
"cybb_phenx_harm.2_year_follow_up_y_arm_1")

abcd_wide <- abcd_wide[ , !(names(abcd_wide) %in% drops)]

str(abcd_wide, list.len=ncol(abcd_wide))

# Check percentage of missing data per variable again
missing_pct <- as.data.frame((colMeans(is.na(abcd_wide)))*100)

missing_pct <- missing_pct %>% 
  rename(percentage_missing = "(colMeans(is.na(abcd_wide))) * 100") # rename column name

missing_pct %>% arrange(desc(percentage_missing)) # descending order

# Now the highest missingness is 48% for CBCL internalising and externalising items,
# which is acceptable and appropriate to be imputed 

# Check that variables have been recoded correctly
str(abcd_wide, list.len=ncol(abcd_wide))

# Next we will run the imputation using missForest, a random forest imputation algorithm for missing data
# MissForest initially imputes all missing data using the mean/mode, then for each variable with missing values, 
# it fits a random forest on the observed part and then predicts the missing part
# This process of training and predicting repeats in an iterative process until a stopping criterion is met

# Following guidance from Liam Morgan: https://rpubs.com/lmorgan95/MissForest

# MacBook Pro 2021 Total Number of Cores:   8 (6 performance and 2 efficiency)
doParallel::registerDoParallel(cores = 8) # set based on number of CPU cores
doRNG::registerDoRNG(seed = 123) # set seed for reproducible results

# Temporarily remove ID column, we will append it back later
# ID column has 11,876 cases - 100% complete, no missingness
abcd_wide <- abcd_wide[, -which(names(abcd_wide) == "src_subject_id")]

# Convert nominal and ordinal items to factor so that missForest imputes them as integers
# Otherwise the binary KSADS PTSD items (0, 1) will be imputed as fractions  
str(abcd_wide, list.len=ncol(abcd_wide))

# The following items are all supposed to be integers (whole numbers) or binary (0,1) 
# or they are ranked in ordered categories ranging from 0 - 5 
abcd_wide <- abcd_wide %>% 
  mutate_at(c(# Derived ACEs 
         "parental_criminality", "parental_psychopathology",
         "parental_separation", "cyber_victimisation",
         # KSADS PTSD at baseline and 2 year follow up
         "ksads_ptsd_raw_762_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_762_p.2_year_follow_up_y_arm_1", 
         "ksads_ptsd_raw_763_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_763_p.2_year_follow_up_y_arm_1",
         "ksads_ptsd_raw_764_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_764_p.2_year_follow_up_y_arm_1", 
         "ksads_ptsd_raw_765_p.baseline_year_1_arm_1",
         "ksads_ptsd_raw_765_p.2_year_follow_up_y_arm_1",
         "ksads_ptsd_raw_767_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_767_p.2_year_follow_up_y_arm_1", 
         "ksads_ptsd_raw_768_p.baseline_year_1_arm_1",
         "ksads_ptsd_raw_768_p.2_year_follow_up_y_arm_1", 
         "ksads_ptsd_raw_769_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_769_p.2_year_follow_up_y_arm_1",
         "ksads_ptsd_raw_766_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_766_p.2_year_follow_up_y_arm_1",
         "ksads_ptsd_raw_754_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_754_p.2_year_follow_up_y_arm_1", 
         "ksads_ptsd_raw_755_p.baseline_year_1_arm_1",
         "ksads_ptsd_raw_755_p.2_year_follow_up_y_arm_1",
         "ksads_ptsd_raw_756_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_756_p.2_year_follow_up_y_arm_1", 
         "ksads_ptsd_raw_757_p.baseline_year_1_arm_1",
         "ksads_ptsd_raw_757_p.2_year_follow_up_y_arm_1",
         "ksads_ptsd_raw_758_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_758_p.2_year_follow_up_y_arm_1", 
         "ksads_ptsd_raw_759_p.baseline_year_1_arm_1",
         "ksads_ptsd_raw_759_p.2_year_follow_up_y_arm_1", 
         "ksads_ptsd_raw_760_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_760_p.2_year_follow_up_y_arm_1",
         "ksads_ptsd_raw_770_p.baseline_year_1_arm_1", 
         "ksads_ptsd_raw_770_p.2_year_follow_up_y_arm_1",
         # Family history parental alcohol abuse at baseline
         "famhx_4_p.baseline_year_1_arm_1",
         "famhx4a_p___1.baseline_year_1_arm_1",
         "famhx4a_p___2.baseline_year_1_arm_1",
         "famhx4a_p___3.baseline_year_1_arm_1",
         "famhx4a_p___4.baseline_year_1_arm_1",
         "famhx4a_p___6.baseline_year_1_arm_1",
         "famhx4a_p___7.baseline_year_1_arm_1",
         "famhx_4d_p___1.baseline_year_1_arm_1",
         "famhx_4d_p___2.baseline_year_1_arm_1",
         "famhx_4d_p___3.baseline_year_1_arm_1",
         "famhx_4d_p___4.baseline_year_1_arm_1",
         "famhx_4d_p___6.baseline_year_1_arm_1",
         "famhx_4d_p___7.baseline_year_1_arm_1",
         # Family history parental drug abuse at baseline
         "fam_history_5_yes_no.baseline_year_1_arm_1",
         "fam_history_q5a_drugs___1.baseline_year_1_arm_1",
         "fam_history_q5a_drugs___2.baseline_year_1_arm_1",
         "fam_history_q5a_drugs___3.baseline_year_1_arm_1",
         "fam_history_q5a_drugs___4.baseline_year_1_arm_1",
         "fam_history_q5a_drugs___6.baseline_year_1_arm_1",
         "fam_history_q5a_drugs___7.baseline_year_1_arm_1",
         "fam_history_q5d_drugs___1.baseline_year_1_arm_1",
         "fam_history_q5d_drugs___2.baseline_year_1_arm_1",
         "fam_history_q5d_drugs___3.baseline_year_1_arm_1",
         "fam_history_q5d_drugs___4.baseline_year_1_arm_1",
         "fam_history_q5d_drugs___6.baseline_year_1_arm_1",
         "fam_history_q5d_drugs___7.baseline_year_1_arm_1",
         # CRPBI emotional neglect at baseline and 1 year follow up
         "crpbi_parent1_y.baseline_year_1_arm_1",
         "crpbi_parent1_y.1_year_follow_up_y_arm_1",
         "crpbi_parent2_y.baseline_year_1_arm_1", 
         "crpbi_parent2_y.1_year_follow_up_y_arm_1",
         "crpbi_parent3_y.baseline_year_1_arm_1", 
         "crpbi_parent3_y.1_year_follow_up_y_arm_1",
         "crpbi_parent4_y.baseline_year_1_arm_1", 
         "crpbi_parent4_y.1_year_follow_up_y_arm_1",
         "crpbi_parent5_y.baseline_year_1_arm_1", 
         "crpbi_parent5_y.1_year_follow_up_y_arm_1",
         # Peer victimisation at 2 year follow up
         "peq_left_out_vic.2_year_follow_up_y_arm_1",
         "peq_chase_vic.2_year_follow_up_y_arm_1",
         "peq_rumor_vic.2_year_follow_up_y_arm_1",
         "peq_invite_vic.2_year_follow_up_y_arm_1",
         "peq_exclude_vic.2_year_follow_up_y_arm_1",
         "peq_gossip_vic.2_year_follow_up_y_arm_1",
         "peq_threat_vic.2_year_follow_up_y_arm_1",
         "peq_loser_vic.2_year_follow_up_y_arm_1",
         "peq_hit_vic.2_year_follow_up_y_arm_1",
         # Unsafe neighbourhood at baseline, 1 year follow up, 2 year follow up
         "neighborhood1r_p.baseline_year_1_arm_1",
         "neighborhood1r_p.1_year_follow_up_y_arm_1",
         "neighborhood1r_p.2_year_follow_up_y_arm_1",
         "neighborhood2r_p.baseline_year_1_arm_1",
         "neighborhood2r_p.1_year_follow_up_y_arm_1",
         "neighborhood2r_p.2_year_follow_up_y_arm_1",
         "neighborhood3r_p.baseline_year_1_arm_1",
         "neighborhood3r_p.1_year_follow_up_y_arm_1",
         "neighborhood3r_p.2_year_follow_up_y_arm_1",
         "neighborhood_crime_y.baseline_year_1_arm_1",
         "neighborhood_crime_y.1_year_follow_up_y_arm_1",
         "neighborhood_crime_y.2_year_follow_up_y_arm_1",
         # Low household income at baseline, 1 year follow up, 2 year follow up
         "demo_comb_income_v2.baseline_year_1_arm_1", 
         "demo_comb_income_v2_l.1_year_follow_up_y_arm_1", 
         "demo_comb_income_v2_l.2_year_follow_up_y_arm_1",
         # CBCL internalising
         "cbcl_q14_p.3_year_follow_up_y_arm_1", "cbcl_q29_p.3_year_follow_up_y_arm_1", 
         "cbcl_q30_p.3_year_follow_up_y_arm_1", "cbcl_q31_p.3_year_follow_up_y_arm_1", 
         "cbcl_q32_p.3_year_follow_up_y_arm_1", "cbcl_q33_p.3_year_follow_up_y_arm_1", 
         "cbcl_q35_p.3_year_follow_up_y_arm_1", "cbcl_q45_p.3_year_follow_up_y_arm_1", 
         "cbcl_q50_p.3_year_follow_up_y_arm_1", "cbcl_q52_p.3_year_follow_up_y_arm_1", 
         "cbcl_q71_p.3_year_follow_up_y_arm_1", "cbcl_q91_p.3_year_follow_up_y_arm_1",
         "cbcl_q112_p.3_year_follow_up_y_arm_1", "cbcl_q05_p.3_year_follow_up_y_arm_1", 
         "cbcl_q42_p.3_year_follow_up_y_arm_1", "cbcl_q65_p.3_year_follow_up_y_arm_1",
         "cbcl_q69_p.3_year_follow_up_y_arm_1", "cbcl_q75_p.3_year_follow_up_y_arm_1", 
         "cbcl_q102_p.3_year_follow_up_y_arm_1", "cbcl_q103_p.3_year_follow_up_y_arm_1",
         "cbcl_q111_p.3_year_follow_up_y_arm_1", "cbcl_q47_p.3_year_follow_up_y_arm_1", 
         "cbcl_q49_p.3_year_follow_up_y_arm_1", "cbcl_q51_p.3_year_follow_up_y_arm_1", 
         "cbcl_q54_p.3_year_follow_up_y_arm_1", "cbcl_q56a_p.3_year_follow_up_y_arm_1", 
         "cbcl_q56b_p.3_year_follow_up_y_arm_1", "cbcl_q56c_p.3_year_follow_up_y_arm_1",
         "cbcl_q56d_p.3_year_follow_up_y_arm_1", "cbcl_q56e_p.3_year_follow_up_y_arm_1", 
         "cbcl_q56f_p.3_year_follow_up_y_arm_1", "cbcl_q56g_p.3_year_follow_up_y_arm_1",
         # CBCL externalising
         "cbcl_q02_p.3_year_follow_up_y_arm_1", "cbcl_q26_p.3_year_follow_up_y_arm_1",
         "cbcl_q28_p.3_year_follow_up_y_arm_1", "cbcl_q39_p.3_year_follow_up_y_arm_1",
         "cbcl_q43_p.3_year_follow_up_y_arm_1", "cbcl_q63_p.3_year_follow_up_y_arm_1", 
         "cbcl_q67_p.3_year_follow_up_y_arm_1", "cbcl_q72_p.3_year_follow_up_y_arm_1", 
         "cbcl_q73_p.3_year_follow_up_y_arm_1", "cbcl_q81_p.3_year_follow_up_y_arm_1", 
         "cbcl_q82_p.3_year_follow_up_y_arm_1", "cbcl_q90_p.3_year_follow_up_y_arm_1", 
         "cbcl_q96_p.3_year_follow_up_y_arm_1", "cbcl_q99_p.3_year_follow_up_y_arm_1", 
         "cbcl_q101_p.3_year_follow_up_y_arm_1", "cbcl_q105_p.3_year_follow_up_y_arm_1", 
         "cbcl_q106_p.3_year_follow_up_y_arm_1", "cbcl_q03_p.3_year_follow_up_y_arm_1", 
         "cbcl_q16_p.3_year_follow_up_y_arm_1","cbcl_q19_p.3_year_follow_up_y_arm_1", 
         "cbcl_q20_p.3_year_follow_up_y_arm_1", "cbcl_q21_p.3_year_follow_up_y_arm_1", 
         "cbcl_q22_p.3_year_follow_up_y_arm_1", "cbcl_q23_p.3_year_follow_up_y_arm_1", 
         "cbcl_q37_p.3_year_follow_up_y_arm_1", "cbcl_q57_p.3_year_follow_up_y_arm_1",
         "cbcl_q68_p.3_year_follow_up_y_arm_1", "cbcl_q86_p.3_year_follow_up_y_arm_1", 
         "cbcl_q87_p.3_year_follow_up_y_arm_1", "cbcl_q88_p.3_year_follow_up_y_arm_1", 
         "cbcl_q89_p.3_year_follow_up_y_arm_1", "cbcl_q94_p.3_year_follow_up_y_arm_1",
         "cbcl_q95_p.3_year_follow_up_y_arm_1",  "cbcl_q97_p.3_year_follow_up_y_arm_1",
         "cbcl_q104_p.3_year_follow_up_y_arm_1", "cbcl_q01_p.3_year_follow_up_y_arm_1", 
         "cbcl_q04_p.3_year_follow_up_y_arm_1", "cbcl_q08_p.3_year_follow_up_y_arm_1", 
         "cbcl_q10_p.3_year_follow_up_y_arm_1", "cbcl_q13_p.3_year_follow_up_y_arm_1", 
         "cbcl_q17_p.3_year_follow_up_y_arm_1", "cbcl_q41_p.3_year_follow_up_y_arm_1", 
         "cbcl_q61_p.3_year_follow_up_y_arm_1", "cbcl_q78_p.3_year_follow_up_y_arm_1", 
         "cbcl_q80_p.3_year_follow_up_y_arm_1"), 
            function(x) as.factor(x))

# Only the following numeric variables shouldn't be converted to factor variables,
# but they should still be converted to integers:
abcd_wide <- abcd_wide %>% 
  mutate_at(c(# Auxiliary variables
         "household_size",
         "mat_age_birth",
         "birthweight",
         # Parent baseline mental health scores
         "par_anxious_depressed",
         "par_withdrawn",
         "par_somatic_complaints",
         "par_thought_problems",
         "par_attention_problems",
         "par_aggress_behav",
         # ASR parental drug abuse at baseline and 2 year follow up
         "asr_q126_p.baseline_year_1_arm_1",
         "asr_q126_p.2_year_follow_up_y_arm_1"), 
            function(x) as.integer(x))

str(abcd_wide, list.len=ncol(abcd_wide))

# Impute dataset (50 trees for each forest, 10 iterations)
imputed_abcd <- missForest(xmis = abcd_wide, # specify dataset
                        maxiter = 10, # max number of iterations
                        ntree = 50, # number of trees for each forest
                        verbose = TRUE, # track progress between iterations
                        parallelize = 'forests')$ximp

# Check imputed values
dim(imputed_abcd)
str(imputed_abcd, list.len=ncol(imputed_abcd))

# Friday 11 Aug 2023: started 8:33PM, ended 9:04PM, took around 31 minutes
# This first attempt worked but the binary variables were imputed as fractions
# Second attempt started 10:20PM, ended 12:25AM, took 2 hours 5 minutes

# Append ID column back to dataset
imputed_abcd$src_subject_id <- abcd_wide$src_subject_id
rm(abcd_wide)

# Save workspace including missForest dataset
save(imputed_abcd, file = "ABCD_imputed_missForest.RData")

# Remove individual datafiles
rm(abcd_wide, required_vars, missing_pct, drops, noms, ords, idvars)
```

# 5. Imputed data cleaning

We will rederive ACEs and outcome variables in the missForest imputed
dataset, in preparation for the imputed data analyses.

``` r
# Load imputed data 
setwd("/Users/athenachowruwern/Desktop")
load("ABCD_imputed_missForest.RData")

#------------------------- Physical abuse ------------------------------------#
# If physical abuse was present for at least 1 assessment, code as 1 for physical abuse
imputed_abcd$physical_abuse <- 0
imputed_abcd$physical_abuse[imputed_abcd$ksads_ptsd_raw_762_p.baseline_year_1_arm_1==1 | imputed_abcd$ksads_ptsd_raw_762_p.2_year_follow_up_y_arm_1==1 | 
                           imputed_abcd$ksads_ptsd_raw_763_p.baseline_year_1_arm_1==1 | imputed_abcd$ksads_ptsd_raw_763_p.2_year_follow_up_y_arm_1==1] <- 1

table(imputed_abcd$physical_abuse, useNA = "always")
prop.table(table(imputed_abcd$physical_abuse, useNA = "always"))*100 # 1.03%

#------------------------- Emotional abuse ------------------------------------#
# If emotional abuse was present for at least 1 assessment, code as 1 for emotional abuse
imputed_abcd$emotional_abuse <- 0
imputed_abcd$emotional_abuse[imputed_abcd$ksads_ptsd_raw_764_p.baseline_year_1_arm_1==1 | imputed_abcd$ksads_ptsd_raw_764_p.2_year_follow_up_y_arm_1==1 | 
                            imputed_abcd$ksads_ptsd_raw_765_p.baseline_year_1_arm_1==1 | imputed_abcd$ksads_ptsd_raw_765_p.2_year_follow_up_y_arm_1==1] <- 1

table(imputed_abcd$emotional_abuse, useNA = "always")
prop.table(table(imputed_abcd$emotional_abuse, useNA = "always"))*100 # 1.59%

#------------------------- Sexual abuse ------------------------------------#
# If sexual abuse was present for at least 1 assessment, code as 1 for sexual abuse
imputed_abcd$sexual_abuse <- 0
imputed_abcd$sexual_abuse[imputed_abcd$ksads_ptsd_raw_767_p.baseline_year_1_arm_1==1 | imputed_abcd$ksads_ptsd_raw_767_p.2_year_follow_up_y_arm_1==1 | 
                         imputed_abcd$ksads_ptsd_raw_768_p.baseline_year_1_arm_1==1 | imputed_abcd$ksads_ptsd_raw_768_p.2_year_follow_up_y_arm_1==1 |
                         imputed_abcd$ksads_ptsd_raw_769_p.baseline_year_1_arm_1==1 | imputed_abcd$ksads_ptsd_raw_769_p.2_year_follow_up_y_arm_1==1] <- 1
table(imputed_abcd$sexual_abuse, useNA="always")
prop.table(table(imputed_abcd$sexual_abuse, useNA="always"))*100 # 2.42%

#------------------------- Domestic violence -----------------------------------------#
# If domestic violence was present for at least 1 assessment, code as 1 for domestic violence
imputed_abcd$domestic_violence <- 0
imputed_abcd$domestic_violence[imputed_abcd$ksads_ptsd_raw_766_p.baseline_year_1_arm_1==1 | imputed_abcd$ksads_ptsd_raw_766_p.2_year_follow_up_y_arm_1==1] <- 1
table(imputed_abcd$domestic_violence, useNA="always")
prop.table(table(imputed_abcd$domestic_violence, useNA="always"))*100 # 9.68%

#------------------------- Accident requiring medical attention ------------------------------------#
# If accident was present for at least 1 assessment, code as 1 for accident
imputed_abcd$accident_medical <- 0
imputed_abcd$accident_medical[imputed_abcd$ksads_ptsd_raw_754_p.baseline_year_1_arm_1==1 | imputed_abcd$ksads_ptsd_raw_754_p.2_year_follow_up_y_arm_1==1 | 
                             imputed_abcd$ksads_ptsd_raw_755_p.baseline_year_1_arm_1==1 | imputed_abcd$ksads_ptsd_raw_755_p.2_year_follow_up_y_arm_1==1] <- 1
table(imputed_abcd$accident_medical, useNA = "always")
prop.table(table(imputed_abcd$accident_medical, useNA = "always"))*100 # 11.3%

#------------------------- Natural disaster ------------------------------------#
# If natural disaster was present for at least 1 assessment, code as 1 for natural disaster
imputed_abcd$natural_disaster <- 0
imputed_abcd$natural_disaster[imputed_abcd$ksads_ptsd_raw_756_p.baseline_year_1_arm_1==1 | imputed_abcd$ksads_ptsd_raw_756_p.2_year_follow_up_y_arm_1==1 | 
                             imputed_abcd$ksads_ptsd_raw_757_p.baseline_year_1_arm_1==1 | imputed_abcd$ksads_ptsd_raw_757_p.2_year_follow_up_y_arm_1==1] <- 1
table(imputed_abcd$natural_disaster, useNA = "always")
prop.table(table(imputed_abcd$natural_disaster, useNA = "always"))*100 # 6.35%

#------------------------- Community violence ------------------------------------#
# If community violence was present for at least 1 assessment, code as 1 for community violence
imputed_abcd$community_violence <- 0
imputed_abcd$community_violence[imputed_abcd$ksads_ptsd_raw_758_p.baseline_year_1_arm_1==1 | imputed_abcd$ksads_ptsd_raw_758_p.2_year_follow_up_y_arm_1==1 | 
                               imputed_abcd$ksads_ptsd_raw_759_p.baseline_year_1_arm_1==1 | imputed_abcd$ksads_ptsd_raw_759_p.2_year_follow_up_y_arm_1==1 | 
                               imputed_abcd$ksads_ptsd_raw_760_p.baseline_year_1_arm_1==1 | imputed_abcd$ksads_ptsd_raw_760_p.2_year_follow_up_y_arm_1==1] <- 1
table(imputed_abcd$community_violence, useNA="always")
prop.table(table(imputed_abcd$community_violence, useNA="always"))*100 # 1.57%

#------------------------- Bereavement ------------------------------------#
# If bereavement was present for at least 1 assessment, code as 1 for bereavement
imputed_abcd$bereavement <- 0
imputed_abcd$bereavement[imputed_abcd$ksads_ptsd_raw_770_p.baseline_year_1_arm_1==1 | imputed_abcd$ksads_ptsd_raw_770_p.2_year_follow_up_y_arm_1==1] <- 1
table(imputed_abcd$bereavement, useNA = "always")
prop.table(table(imputed_abcd$bereavement, useNA = "always"))*100 # 32.7%

#------------------------- Emotional neglect -----------------------------------------#
# Items were originally coded as: 1 = Not like him/her; 2 = Somewhat like him/her; 3 = A lot like him/her
# Code each individual item so a score of 1 indicates neglect
# then if the child reports 2 or more items, code as overall neglect
imputed_abcd <- imputed_abcd %>% 
  mutate_at(c("crpbi_parent1_y.baseline_year_1_arm_1", "crpbi_parent1_y.1_year_follow_up_y_arm_1",
            "crpbi_parent2_y.baseline_year_1_arm_1", "crpbi_parent2_y.1_year_follow_up_y_arm_1",
            "crpbi_parent3_y.baseline_year_1_arm_1", "crpbi_parent3_y.1_year_follow_up_y_arm_1",
            "crpbi_parent4_y.baseline_year_1_arm_1", "crpbi_parent4_y.1_year_follow_up_y_arm_1",
            "crpbi_parent5_y.baseline_year_1_arm_1", "crpbi_parent5_y.1_year_follow_up_y_arm_1"), 
            function(x) as.numeric(x))

crpbi_cols <- c("crpbi_parent1_y.baseline_year_1_arm_1", "crpbi_parent1_y.1_year_follow_up_y_arm_1",
            "crpbi_parent2_y.baseline_year_1_arm_1", "crpbi_parent2_y.1_year_follow_up_y_arm_1",
            "crpbi_parent3_y.baseline_year_1_arm_1", "crpbi_parent3_y.1_year_follow_up_y_arm_1",
            "crpbi_parent4_y.baseline_year_1_arm_1", "crpbi_parent4_y.1_year_follow_up_y_arm_1",
            "crpbi_parent5_y.baseline_year_1_arm_1", "crpbi_parent5_y.1_year_follow_up_y_arm_1")
imputed_abcd[,crpbi_cols] <- lapply(imputed_abcd[,crpbi_cols], function(x) ifelse(x==1, 1, 0))

imputed_abcd$emotional_neglect_sum <- apply(imputed_abcd[,crpbi_cols], 1, sum, na.rm=T) 

imputed_abcd$emotional_neglect <- 0
imputed_abcd$emotional_neglect[imputed_abcd$emotional_neglect_sum >=2] <- 1 
table(imputed_abcd$emotional_neglect, useNA = "always")
prop.table(table(imputed_abcd$emotional_neglect, useNA = "always"))*100 # 3.65%

#------------------------- Parental psychopathology -----------------------------------------#
# Parental psychopathology was already derived before imputation, so check imputed prevalence
table(imputed_abcd$parental_psychopathology, useNA = "always")
prop.table(table(imputed_abcd$parental_psychopathology, useNA = "always"))*100 # 40.2%
imputed_abcd$parental_psychopathology <- as.numeric(imputed_abcd$parental_psychopathology)

#------------------------- Parental alcohol abuse -----------------------------------------#
# If any blood relative of your child ever had any problems due to alcohol,
# and if this led to marital problems or work problems or arrests/DUI etc
# code as 1 for parental alcohol abuse
imputed_abcd$parental_alcohol_abuse <- 0
imputed_abcd$parental_alcohol_abuse[# biological father had problems due to alcohol
                          imputed_abcd$famhx_4_p.baseline_year_1_arm_1==1 & imputed_abcd$famhx4a_p___1.baseline_year_1_arm_1==1 | # marital problems
                          imputed_abcd$famhx_4_p.baseline_year_1_arm_1==1 & imputed_abcd$famhx4a_p___2.baseline_year_1_arm_1==1 | # work problems
                          imputed_abcd$famhx_4_p.baseline_year_1_arm_1==1 & imputed_abcd$famhx4a_p___3.baseline_year_1_arm_1==1 | # arrests/DUI
                          imputed_abcd$famhx_4_p.baseline_year_1_arm_1==1 & imputed_abcd$famhx4a_p___4.baseline_year_1_arm_1==1 | # alcohol treatment programme
                          imputed_abcd$famhx_4_p.baseline_year_1_arm_1==1 & imputed_abcd$famhx4a_p___6.baseline_year_1_arm_1==1 | # isolated self, arguments, drunk a lot 
                          imputed_abcd$famhx_4_p.baseline_year_1_arm_1==1 & imputed_abcd$famhx4a_p___7.baseline_year_1_arm_1==1 | # health problems
                          # biological mother had problems due to alcohol
                          imputed_abcd$famhx_4_p.baseline_year_1_arm_1==1 & imputed_abcd$famhx_4d_p___1.baseline_year_1_arm_1==1 | # marital problems
                          imputed_abcd$famhx_4_p.baseline_year_1_arm_1==1 & imputed_abcd$famhx_4d_p___2.baseline_year_1_arm_1==1 | # work problems
                          imputed_abcd$famhx_4_p.baseline_year_1_arm_1==1 & imputed_abcd$famhx_4d_p___3.baseline_year_1_arm_1==1 | # arrests/DUI
                          imputed_abcd$famhx_4_p.baseline_year_1_arm_1==1 & imputed_abcd$famhx_4d_p___4.baseline_year_1_arm_1==1 | # alcohol treatment programme
                          imputed_abcd$famhx_4_p.baseline_year_1_arm_1==1 & imputed_abcd$famhx_4d_p___6.baseline_year_1_arm_1==1 | # isolated self, arguments, drunk a lot 
                          imputed_abcd$famhx_4_p.baseline_year_1_arm_1==1 & imputed_abcd$famhx_4d_p___7.baseline_year_1_arm_1==1 ] <- 1 # health problems
table(imputed_abcd$parental_alcohol_abuse, useNA = "always")
prop.table(table(imputed_abcd$parental_alcohol_abuse, useNA = "always"))*100 # 14.5%

#------------------------- Parental drug abuse -----------------------------------------#
# If any blood relative of your child ever had any problems due to drugs,
# and if this led to marital problems or work problems or arrests/DUI etc,
# or if parent used drugs multiple times per week for past 6 months
# code as 1 for parental drug abuse
imputed_abcd$parental_drug_abuse <- 0
imputed_abcd$parental_drug_abuse[imputed_abcd$asr_q126_p.baseline_year_1_arm_1>=52 | # used drugs multiple times weekly in past 6 months at baseline
                              imputed_abcd$asr_q126_p.2_year_follow_up_y_arm_1>=52 | # used drugs multiple times weekly in past 6 months at 2 year follow up
                          # biological father had problems due to drugs
                          imputed_abcd$fam_history_5_yes_no.baseline_year_1_arm_1==1 & imputed_abcd$fam_history_q5a_drugs___1.baseline_year_1_arm_1==1 | # marital problems
                          imputed_abcd$fam_history_5_yes_no.baseline_year_1_arm_1==1 & imputed_abcd$fam_history_q5a_drugs___2.baseline_year_1_arm_1==1 | # work problems
                          imputed_abcd$fam_history_5_yes_no.baseline_year_1_arm_1==1 & imputed_abcd$fam_history_q5a_drugs___3.baseline_year_1_arm_1==1 | # arrests/DUI
                          imputed_abcd$fam_history_5_yes_no.baseline_year_1_arm_1==1 & imputed_abcd$fam_history_q5a_drugs___4.baseline_year_1_arm_1==1 | # drug treatment programme
                          imputed_abcd$fam_history_5_yes_no.baseline_year_1_arm_1==1 & imputed_abcd$fam_history_q5a_drugs___6.baseline_year_1_arm_1==1 | # isolated self, arguments, high a lot 
                          imputed_abcd$fam_history_5_yes_no.baseline_year_1_arm_1==1 & imputed_abcd$fam_history_q5a_drugs___7.baseline_year_1_arm_1==1 | # health problems
                          # biological mother had problems due to drugs
                          imputed_abcd$fam_history_5_yes_no.baseline_year_1_arm_1==1 & imputed_abcd$fam_history_q5d_drugs___1.baseline_year_1_arm_1==1 | # marital problems
                          imputed_abcd$fam_history_5_yes_no.baseline_year_1_arm_1==1 & imputed_abcd$fam_history_q5d_drugs___2.baseline_year_1_arm_1==1 | # work problems
                          imputed_abcd$fam_history_5_yes_no.baseline_year_1_arm_1==1 & imputed_abcd$fam_history_q5d_drugs___3.baseline_year_1_arm_1==1 | # arrests/DUI
                          imputed_abcd$fam_history_5_yes_no.baseline_year_1_arm_1==1 & imputed_abcd$fam_history_q5d_drugs___4.baseline_year_1_arm_1==1 | # drug treatment programme
                          imputed_abcd$fam_history_5_yes_no.baseline_year_1_arm_1==1 & imputed_abcd$fam_history_q5d_drugs___6.baseline_year_1_arm_1==1 | # isolated self, arguments, high a lot 
                          imputed_abcd$fam_history_5_yes_no.baseline_year_1_arm_1==1 & imputed_abcd$fam_history_q5d_drugs___7.baseline_year_1_arm_1==1] <- 1
imputed_abcd$parental_drug_abuse[apply(apply(imputed_abcd[,c("asr_q126_p.baseline_year_1_arm_1", "asr_q126_p.2_year_follow_up_y_arm_1", 
                                                  "fam_history_5_yes_no.baseline_year_1_arm_1")],2,is.na),1,sum) > 1] <- NA # code NA if missing > 1/3 items
table(imputed_abcd$parental_drug_abuse, useNA = "always")
prop.table(table(imputed_abcd$parental_drug_abuse, useNA = "always"))*100 # 10.6%

#------------------------- Parental criminality -----------------------------------------#
# Parental criminality was already derived before imputation, so check imputed prevalence
table(imputed_abcd$parental_criminality, useNA = "always")
prop.table(table(imputed_abcd$parental_criminality, useNA = "always"))*100 # 9.74%
imputed_abcd$parental_criminality <- as.numeric(imputed_abcd$parental_criminality)

#------------------------- Parental separation -----------------------------------------#
# # Parental separation was already derived before imputation, so check imputed prevalence
table(imputed_abcd$parental_separation, useNA = "always")
prop.table(table(imputed_abcd$parental_separation, useNA = "always"))*100 # 23.7%
imputed_abcd$parental_separation <- as.numeric(imputed_abcd$parental_separation)

#------------------------- Peer victimisation -----------------------------------------#
# Items were originally coded as: 1=Never; 2=Once or twice; 3=A few times; 4=About once a week; 5=A few times a week
# Code each individual item so a score of 4 or 5 indicates being victimised
# then if the child reports at least 1 type of victimisation, code as 1 for peer victimisation
imputed_abcd <- imputed_abcd %>% 
  mutate_at(c("peq_left_out_vic.2_year_follow_up_y_arm_1","peq_chase_vic.2_year_follow_up_y_arm_1","peq_rumor_vic.2_year_follow_up_y_arm_1",
              "peq_invite_vic.2_year_follow_up_y_arm_1","peq_exclude_vic.2_year_follow_up_y_arm_1","peq_gossip_vic.2_year_follow_up_y_arm_1",
              "peq_threat_vic.2_year_follow_up_y_arm_1","peq_loser_vic.2_year_follow_up_y_arm_1","peq_hit_vic.2_year_follow_up_y_arm_1"), 
            function(x) as.numeric(x))

imputed_abcd$peer_victimisation <- 0
imputed_abcd$peer_victimisation[imputed_abcd$peq_left_out_vic.2_year_follow_up_y_arm_1>=4 | imputed_abcd$peq_chase_vic.2_year_follow_up_y_arm_1>=4 |
                                imputed_abcd$peq_rumor_vic.2_year_follow_up_y_arm_1>=4 | imputed_abcd$peq_invite_vic.2_year_follow_up_y_arm_1>=4 | 
                                imputed_abcd$peq_exclude_vic.2_year_follow_up_y_arm_1>=4 | imputed_abcd$peq_gossip_vic.2_year_follow_up_y_arm_1>=4 | 
                                imputed_abcd$peq_threat_vic.2_year_follow_up_y_arm_1>=4 | imputed_abcd$peq_loser_vic.2_year_follow_up_y_arm_1>=4 | 
                                imputed_abcd$peq_hit_vic.2_year_follow_up_y_arm_1>=4] <- 1 
table(imputed_abcd$peer_victimisation, useNA = "always")
prop.table(table(imputed_abcd$peer_victimisation, useNA = "always"))*100 # 6.98%

#------------------------- Cyber victimisation -----------------------------------------#
# Cyber victimisation was already derived before imputation, so check imputed prevalence
table(imputed_abcd$cyber_victimisation, useNA = "always")
prop.table(table(imputed_abcd$cyber_victimisation, useNA = "always"))*100 # 7.83%
imputed_abcd$cyber_victimisation <- as.numeric(imputed_abcd$cyber_victimisation)

#------------------------- Unsafe neighbourhood -----------------------------------------#
# Items were originally coded as: 1 = Strongly Disagree; 2 = Disagree; 3 = Neutral (neither agree nor disagree); 4 = Agree; 5 = Strongly Agree
# Code each individual item so a score of 1 indicates feeling unsafe
# then if the parent/child reports at least 1 item, code as unsafe neighbourhood 
imputed_abcd$unsafe_neighbourhood <- 0
imputed_abcd$unsafe_neighbourhood[imputed_abcd$neighborhood1r_p.baseline_year_1_arm_1==1 | imputed_abcd$neighborhood1r_p.1_year_follow_up_y_arm_1==1 | imputed_abcd$neighborhood1r_p.2_year_follow_up_y_arm_1==1 | 
                                 imputed_abcd$neighborhood2r_p.baseline_year_1_arm_1==1 | imputed_abcd$neighborhood2r_p.1_year_follow_up_y_arm_1==1 | imputed_abcd$neighborhood2r_p.2_year_follow_up_y_arm_1==1 | 
                                 imputed_abcd$neighborhood3r_p.baseline_year_1_arm_1==1 | imputed_abcd$neighborhood3r_p.1_year_follow_up_y_arm_1==1 | imputed_abcd$neighborhood3r_p.2_year_follow_up_y_arm_1==1 | 
                                 imputed_abcd$neighborhood_crime_y.baseline_year_1_arm_1==1 | imputed_abcd$neighborhood_crime_y.1_year_follow_up_y_arm_1==1 | imputed_abcd$neighborhood_crime_y.2_year_follow_up_y_arm_1==1] <- 1 
table(imputed_abcd$unsafe_neighbourhood, useNA="always")
prop.table(table(imputed_abcd$unsafe_neighbourhood, useNA="always"))*100 # 18.1%

#------------------------- Low household income -----------------------------------------#
# Low household income was originally coded as:
# 1= Less than $5,000; 2=$5,000 through $11,999; 3=$12,000 through $15,999; 
# 4=$16,000 through $24,999; 5=$25,000 through $34,999; 6=$35,000 through $49,999; 
# 7=$50,000 through $74,999; 8= $75,000 through $99,999; 9=$100,000 through $199,999; 
# 10=$200,000 and greater

# US Census Bureau: in 2021, households in the lowest quintile had incomes of $28,007 or less
# https://www.census.gov/content/dam/Census/library/publications/2022/demo/p60-276.pdf

imputed_abcd <- imputed_abcd %>% 
  mutate_at(c("demo_comb_income_v2.baseline_year_1_arm_1", "demo_comb_income_v2_l.1_year_follow_up_y_arm_1", 
              "demo_comb_income_v2_l.2_year_follow_up_y_arm_1"), 
            function(x) as.numeric(x))

imputed_abcd$low_household_income <- 0
imputed_abcd$low_household_income[imputed_abcd$demo_comb_income_v2.baseline_year_1_arm_1<=4 | imputed_abcd$demo_comb_income_v2_l.1_year_follow_up_y_arm_1<=4 | 
                                 imputed_abcd$demo_comb_income_v2_l.2_year_follow_up_y_arm_1<=4] <- 1 # Income below $25k indicates low household income
table(imputed_abcd$low_household_income, useNA="always")
prop.table(table(imputed_abcd$low_household_income, useNA="always"))*100 # 21.0%

#------------------------- Internalising symptoms -----------------------------------------#
# Sum items together into cbcl_internalising
internalising_cols <- c("cbcl_q14_p.3_year_follow_up_y_arm_1", "cbcl_q29_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q30_p.3_year_follow_up_y_arm_1", "cbcl_q31_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q32_p.3_year_follow_up_y_arm_1", "cbcl_q33_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q35_p.3_year_follow_up_y_arm_1", "cbcl_q45_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q50_p.3_year_follow_up_y_arm_1", "cbcl_q52_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q71_p.3_year_follow_up_y_arm_1", "cbcl_q91_p.3_year_follow_up_y_arm_1",
                        "cbcl_q112_p.3_year_follow_up_y_arm_1", "cbcl_q05_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q42_p.3_year_follow_up_y_arm_1", "cbcl_q65_p.3_year_follow_up_y_arm_1",
                        "cbcl_q69_p.3_year_follow_up_y_arm_1", "cbcl_q75_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q102_p.3_year_follow_up_y_arm_1", "cbcl_q103_p.3_year_follow_up_y_arm_1",
                        "cbcl_q111_p.3_year_follow_up_y_arm_1", "cbcl_q47_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q49_p.3_year_follow_up_y_arm_1", "cbcl_q51_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q54_p.3_year_follow_up_y_arm_1", "cbcl_q56a_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q56b_p.3_year_follow_up_y_arm_1", "cbcl_q56c_p.3_year_follow_up_y_arm_1",
                        "cbcl_q56d_p.3_year_follow_up_y_arm_1", "cbcl_q56e_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q56f_p.3_year_follow_up_y_arm_1", "cbcl_q56g_p.3_year_follow_up_y_arm_1") 
imputed_abcd[,internalising_cols] <- lapply(imputed_abcd[,internalising_cols], function(x) as.numeric(as.character(x)))

imputed_abcd$cbcl_internalising <- apply(imputed_abcd[,internalising_cols], 1, sum, na.rm=T)
table(imputed_abcd$cbcl_internalising, useNA="always")
describe(imputed_abcd$cbcl_internalising) # M = 2.67, SD = 4.89

#------------------------- Externalising symptoms -----------------------------------------#
# Sum items together into cbcl_externalising
externalising_cols <- c("cbcl_q02_p.3_year_follow_up_y_arm_1", "cbcl_q26_p.3_year_follow_up_y_arm_1",
                        "cbcl_q28_p.3_year_follow_up_y_arm_1", "cbcl_q39_p.3_year_follow_up_y_arm_1",
                        "cbcl_q43_p.3_year_follow_up_y_arm_1", "cbcl_q63_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q67_p.3_year_follow_up_y_arm_1", "cbcl_q72_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q73_p.3_year_follow_up_y_arm_1", "cbcl_q81_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q82_p.3_year_follow_up_y_arm_1", "cbcl_q90_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q96_p.3_year_follow_up_y_arm_1", "cbcl_q99_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q101_p.3_year_follow_up_y_arm_1", "cbcl_q105_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q106_p.3_year_follow_up_y_arm_1", "cbcl_q03_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q16_p.3_year_follow_up_y_arm_1","cbcl_q19_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q20_p.3_year_follow_up_y_arm_1", "cbcl_q21_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q22_p.3_year_follow_up_y_arm_1", "cbcl_q23_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q37_p.3_year_follow_up_y_arm_1", "cbcl_q57_p.3_year_follow_up_y_arm_1",
                        "cbcl_q68_p.3_year_follow_up_y_arm_1", "cbcl_q86_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q87_p.3_year_follow_up_y_arm_1", "cbcl_q88_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q89_p.3_year_follow_up_y_arm_1", "cbcl_q94_p.3_year_follow_up_y_arm_1",
                        "cbcl_q95_p.3_year_follow_up_y_arm_1",  "cbcl_q97_p.3_year_follow_up_y_arm_1",
                        "cbcl_q104_p.3_year_follow_up_y_arm_1", "cbcl_q01_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q04_p.3_year_follow_up_y_arm_1", "cbcl_q08_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q10_p.3_year_follow_up_y_arm_1", "cbcl_q13_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q17_p.3_year_follow_up_y_arm_1", "cbcl_q41_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q61_p.3_year_follow_up_y_arm_1", "cbcl_q78_p.3_year_follow_up_y_arm_1", 
                        "cbcl_q80_p.3_year_follow_up_y_arm_1")
imputed_abcd[,externalising_cols] <- lapply(imputed_abcd[,externalising_cols], function(x) as.numeric(as.character(x)))

imputed_abcd$cbcl_externalising <- apply(imputed_abcd [,externalising_cols], 1, sum, na.rm=T)
table(imputed_abcd$cbcl_externalising, useNA="always")
describe(imputed_abcd$cbcl_externalising) # M = 3.48, SD = 6.59)

###############################################################################
#------------------- Imputed descriptive statistics --------------------------#
###############################################################################

# Physical abuse (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(imputed_abcd$physical_abuse, useNA="always")
prop.table(table(imputed_abcd$physical_abuse, useNA="always"))*100

# Emotional abuse (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(imputed_abcd$emotional_abuse, useNA="always") 
prop.table(table(imputed_abcd$emotional_abuse, useNA="always"))*100

# Sexual abuse (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(imputed_abcd$sexual_abuse, useNA="always") 
prop.table(table(imputed_abcd$sexual_abuse, useNA="always"))*100

# Domestic violence (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(imputed_abcd$domestic_violence, useNA="always") 
prop.table(table(imputed_abcd$domestic_violence, useNA="always"))*100

# Accident requiring medical attention (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(imputed_abcd$accident_medical, useNA="always")
prop.table(table(imputed_abcd$accident_medical, useNA="always"))*100

# Natural disaster (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(imputed_abcd$natural_disaster, useNA="always")
prop.table(table(imputed_abcd$natural_disaster, useNA="always"))*100

# Community violence (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(imputed_abcd$community_violence, useNA="always") 
prop.table(table(imputed_abcd$community_violence, useNA="always"))*100

# Bereavement (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(imputed_abcd$bereavement, useNA="always") 
prop.table(table(imputed_abcd$bereavement, useNA="always"))*100

# Emotional neglect (Baseline, 1y follow up; ages 0-9/10y, 10/11y)
table(imputed_abcd$emotional_neglect, useNA="always")
prop.table(table(imputed_abcd$emotional_neglect, useNA="always"))*100

# Parental psychopathology (Baseline, 2y follow up; ages 0-9/10y, 11/12y)
table(imputed_abcd$parental_psychopathology, useNA="always")
prop.table(table(imputed_abcd$parental_psychopathology, useNA="always"))*100

# Parental alcohol abuse (Baseline; ages 0-9/10y)
table(imputed_abcd$parental_alcohol_abuse, useNA="always")
prop.table(table(imputed_abcd$parental_alcohol_abuse, useNA="always"))*100

# Parental drug abuse (Baseline, 2y follow up; ages 0-9/10y, 11/12y)
table(imputed_abcd$parental_drug_abuse, useNA="always")
prop.table(table(imputed_abcd$parental_drug_abuse, useNA="always"))*100

# Parental criminality (1y and 2y follow up; ages 10/11y, 11/12y)
table(imputed_abcd$parental_criminality, useNA="always") 
prop.table(table(imputed_abcd$parental_criminality, useNA="always"))*100

# Parental separation (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(imputed_abcd$parental_separation, useNA="always") 
prop.table(table(imputed_abcd$parental_separation, useNA="always"))*100

# Peer victimisation (2y follow up; ages 11/12y)
table(imputed_abcd$peer_victimisation, useNA="always") 
prop.table(table(imputed_abcd$peer_victimisation, useNA="always"))*100

# Cyber victimisation (2y follow up; ages 11/12y)
table(imputed_abcd$cyber_victimisation, useNA="always")
prop.table(table(imputed_abcd$cyber_victimisation, useNA="always"))*100

# Unsafe neighbourhood (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(imputed_abcd$unsafe_neighbourhood, useNA="always")
prop.table(table(imputed_abcd$unsafe_neighbourhood, useNA="always"))*100

# Low household income (Baseline, 1y and 2y follow up; ages 0-9/10y, 10/11y, 11/12y)
table(imputed_abcd$low_household_income, useNA="always")
prop.table(table(imputed_abcd$low_household_income, useNA="always"))*100

# Adolescent mental health outcomes (3y follow up; ages 12/13y)
describe(imputed_abcd$cbcl_internalising) # internalising symptoms
describe(imputed_abcd$cbcl_externalising) # externalising symptoms

# Covariates
table(imputed_abcd$sex, useNA = "always")
prop.table(table(imputed_abcd$sex, useNA = "always"))*100

table(imputed_abcd$race, useNA = "always")
prop.table(table(imputed_abcd$race, useNA = "always"))*100

# Remove individual datafiles
rm(crpbi_cols, externalising_cols, internalising_cols)
```

# 6. Imputed data analyses

We will rerun the EFA and regressions on the imputed data, to check if
results are consistent with the original complete data.

``` r
# Repeat EFA and regression analyses code on imputed dataset

# Create new dataframe with all derived binary ACE variables
abcd_aces <- select(imputed_abcd,
                    physical_abuse,
                    emotional_abuse,
                    sexual_abuse,
                    domestic_violence,
                    accident_medical,
                    natural_disaster,
                    community_violence,
                    bereavement,
                    emotional_neglect,
                    parental_psychopathology,
                    parental_alcohol_abuse,
                    parental_drug_abuse,
                    parental_criminality,
                    parental_separation,
                    peer_victimisation,
                    cyber_victimisation,
                    unsafe_neighbourhood,
                    low_household_income)
dim(abcd_aces)
str(abcd_aces)
colnames(abcd_aces)

# Check whether IDs are duplicated - no
n_occur <- data.frame(table(abcd_aces$src_subject_id))
n_occur[n_occur$Freq > 1,]

#------------------------------------ Exploratory Factor Analysis (EFA) ------------------------------------#
library(corrplot)
library(EFA.dimensions)

# Compute tetrachoric correlations
tet_corr <- tetrachoric(abcd_aces)
tet_corr

# Tetrachoric correlation matrix
tet_corr_matrix<- tet_corr$rho
tet_corr_matrix

###### Exploratory factor analysis (using item response theory (IRT) code)

## IRT one-factor model
irt_1factor <- irt.fa(tet_corr, nfactors=1, plot=FALSE, n.obs=nrow(abcd_aces), rotate="promax", fm="wls")
irt_1factor$fa # loadings
fa.diagram(irt_1factor, cut=0.2, rsize=.30) # factor diagram

## IRT two-factor model
irt_2factor <- irt.fa(tet_corr, nfactors=2, plot=FALSE, n.obs=nrow(abcd_aces), rotate="promax", fm="wls")
irt_2factor$fa # loadings
fa.diagram(irt_2factor, cut=0.2, rsize=.30) # factor diagram

## IRT three-factor model
irt_3factor <- irt.fa(tet_corr, nfactors=3, plot=FALSE, n.obs=nrow(abcd_aces), rotate="oblimin", fm="wls") # promax doesn't work
irt_3factor$fa # loadings
fa.diagram(irt_3factor, cut=0.2, rsize=.30) # factor diagram

# IRT four-factor model
irt_4factor <- irt.fa(tet_corr, nfactors=4, plot=FALSE, n.obs=nrow(abcd_aces), rotate="oblimin", fm="wls") # promax doesn't work
irt_4factor$fa # loadings
colnames(irt_4factor$fa$loadings) <- c("Traumatic Events", "Parental Threat", "Deprivation", "Victimisation")
fa.diagram(irt_4factor, cut=0.2, rsize=.30) # factor diagram

#------------------------------------ Plot factor analysis results ------------------------------------#
# Following guidance from Dr Dan Mirman: https://rpubs.com/danmirman/plotting_factor_analysis

# Extract loadings from 4-factor model
class(irt_4factor$fa$loadings) # psych loadings class 
factor_loadings <- unclass(irt_4factor$fa$loadings) # convert to matrix
factor_loadings <- as.data.frame(factor_loadings) # convert to dataframe
factor_loadings <- rownames_to_column(factor_loadings, "aces") # name first column

# Rename ACEs
factor_loadings$aces <- car::recode(factor_loadings$aces, "'low_household_income'='Low household income'; 'unsafe_neighbourhood'='Unsafe neighbourhood'; 'emotional_neglect'='Emotional neglect'; 'peer_victimisation'='Peer victimisation'; 'cyber_victimisation'='Cyber victimisation'; 'parental_alcohol_abuse'='Parental alcohol abuse'; 'parental_drug_abuse'='Parental drug abuse'; 'parental_separation'='Parental separation'; 'parental_criminality'='Parental criminality'; 'parental_psychopathology'='Parental psychopathology'; 'physical_abuse'='Physical abuse'; 'emotional_abuse'='Emotional abuse'; 'community_violence'='Community violence'; 'sexual_abuse'='Sexual abuse'; 'natural_disaster'='Natural disaster'; 'domestic_violence'='Domestic violence'; 'accident_medical'='Accident requiring medical'; 'bereavement'='Bereavement'")

# Manually order ACEs by the 4 factor groups
factor_loadings$aces <- factor(factor_loadings$aces, levels = c("Bereavement", "Accident requiring medical", "Domestic violence", "Natural disaster", "Sexual abuse", "Community violence", "Emotional abuse", "Physical abuse", "Cyber victimisation", "Peer victimisation", "Emotional neglect", "Unsafe neighbourhood", "Low household income", "Parental psychopathology", "Parental criminality", "Parental separation", "Parental drug abuse", "Parental alcohol abuse"))
factor_loadings <- factor_loadings[order(factor_loadings$aces), ]
factor_loadings

# Melt data into long form for plotting
library(reshape2)
loadings.m <- melt(factor_loadings, id="aces", 
                   measure=c("Parental Threat", "Deprivation", "Victimisation", "Traumatic Events"), 
                   variable.name="Factor", value.name="Loading")
loadings.m

# Order the factors
f = c("Parental Threat", "Deprivation", "Victimisation", "Traumatic Events")
loadings.m <- within(loadings.m, Factor <- factor(Factor, levels=f))

# For each ACE, plot the loading as length and fill color of a bar
# note that the length will be the absolute value of the loading but the 
# fill color will be the signed value, more on this below
loadings_plot <- ggplot(loadings.m, aes(aces, abs(Loading), fill=Loading)) + 
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
tet_corr_df <- rownames_to_column(tet_corr_df, "aces") # name first column
head(tet_corr_df)

# Melt data into long form for plotting
corrs.m <- melt(tet_corr_df, id="aces", variable.name="aces2", value.name="Correlation")
head(corrs.m)

# Rename ACEs
corrs.m$aces <- car::recode(corrs.m$aces, "'low_household_income'='Low household income'; 'unsafe_neighbourhood'='Unsafe neighbourhood'; 'emotional_neglect'='Emotional neglect'; 'peer_victimisation'='Peer victimisation'; 'cyber_victimisation'='Cyber victimisation'; 'parental_alcohol_abuse'='Parental alcohol abuse'; 'parental_drug_abuse'='Parental drug abuse'; 'parental_separation'='Parental separation'; 'parental_criminality'='Parental criminality'; 'parental_psychopathology'='Parental psychopathology'; 'physical_abuse'='Physical abuse'; 'emotional_abuse'='Emotional abuse'; 'community_violence'='Community violence'; 'sexual_abuse'='Sexual abuse'; 'natural_disaster'='Natural disaster'; 'domestic_violence'='Domestic violence'; 'accident_medical'='Accident requiring medical'; 'bereavement'='Bereavement'")

corrs.m$aces2 <- car::recode(corrs.m$aces2, "'low_household_income'='Low household income'; 'unsafe_neighbourhood'='Unsafe neighbourhood'; 'emotional_neglect'='Emotional neglect'; 'peer_victimisation'='Peer victimisation'; 'cyber_victimisation'='Cyber victimisation'; 'parental_alcohol_abuse'='Parental alcohol abuse'; 'parental_drug_abuse'='Parental drug abuse'; 'parental_separation'='Parental separation'; 'parental_criminality'='Parental criminality'; 'parental_psychopathology'='Parental psychopathology'; 'physical_abuse'='Physical abuse'; 'emotional_abuse'='Emotional abuse'; 'community_violence'='Community violence'; 'sexual_abuse'='Sexual abuse'; 'natural_disaster'='Natural disaster'; 'domestic_violence'='Domestic violence'; 'accident_medical'='Accident requiring medical'; 'bereavement'='Bereavement'")

# Reorder ACEs in correlation matrix
corrs.m$aces <- factor(corrs.m$aces, levels = c("Bereavement", "Accident requiring medical", "Domestic violence", "Natural disaster", "Sexual abuse", "Community violence", "Emotional abuse", "Physical abuse", "Cyber victimisation", "Peer victimisation", "Emotional neglect", "Unsafe neighbourhood", "Low household income", "Parental psychopathology", "Parental criminality", "Parental separation", "Parental drug abuse", "Parental alcohol abuse"))
corrs.m <- corrs.m[order(corrs.m$aces), ]

corrs.m$aces2 <- factor(corrs.m$aces2, levels = c("Parental alcohol abuse", "Parental drug abuse", "Parental separation", "Parental criminality", "Parental psychopathology", "Low household income", "Unsafe neighbourhood", "Emotional neglect", "Peer victimisation", "Cyber victimisation", "Physical abuse", "Emotional abuse", "Community violence", "Sexual abuse", "Natural disaster", "Domestic violence", "Accident requiring medical", "Bereavement"))
corrs.m <- corrs.m[order(corrs.m$aces2), ]

corrs.m

# Plot correlation matrix
library(grid) 
# for adjusting plot margins
# place the tests on the x- and y-axes, 
# fill the elements with the strength of the correlation
corr_matrix <- ggplot(corrs.m, aes(aces2, aces, fill=abs(Correlation))) + 
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

# Plot stacked bar graph
p2 <- ggplot(loadings.m, aes(aces, abs(Loading), fill=Factor)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ylab("Loading Strength") + theme_bw(base_size=10) + 
  # remove labels and tweak margins for combining with the correlation matrix plot
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = unit(c(3,1,26.4,-3), "mm")) +
  scale_fill_manual(values = c("#66C2A5","#A6CEE3","#E78AC3","#B2DF8A"))
p2

# Add stacked bar graph of factor loadings to correlation matrix
# so that we can see how the factor analysis transformed these pairwise correlations into factors
library(gridExtra)
grid.arrange(p1, p2, ncol=2, widths=c(2, 1))

#------------------------- Extract factor scores ------------------------------------#
# Get factor scores 
factor_scores <- factor.scores(abcd_aces, irt_4factor$fa)
factor_scores

# Get columns of factor scores 
fs_cols <- factor_scores$scores
fs_cols <- as.data.frame(fs_cols)

# Add factor score columns to imputed_abcd dataset
imputed_abcd <- imputed_abcd %>% mutate(
  "Traumatic_Events" = fs_cols$`Traumatic Events`,
  "Parental_Threat" = fs_cols$`Parental Threat`,
  "Deprivation" = fs_cols$`Deprivation`,
  "Victimisation" = fs_cols$`Victimisation`,
)

head(imputed_abcd)

# Check prevalence of sex and race variables
table(imputed_abcd$sex, useNA="always")
table(imputed_abcd$race, useNA="always") 

# Recode sex reference level to male
imputed_abcd$sex <- relevel(imputed_abcd$sex, ref = "M")

# Recode race reference level to White
imputed_abcd$race <- relevel(imputed_abcd$race, ref = "White")

#------------------------- Check linear regression assumptions ------------------------------------# 

# Check correlations between factor scores to ensure no collinearity
cor(imputed_abcd[, c("Traumatic_Events", "Parental_Threat", "Deprivation", "Victimisation")], use = "complete.obs")

# Check whether the outcome variables follow a normal distribution
hist(imputed_abcd$cbcl_internalising)
hist(imputed_abcd$cbcl_externalising)
# Distributions are skewed to the left, but this makes sense because a higher score represents worse mental health

# Standardise predictor and outcome variables
imputed_abcd$Traumatic_Events <- scale(imputed_abcd$Traumatic_Events)
imputed_abcd$Parental_Threat <- scale(imputed_abcd$Parental_Threat)
imputed_abcd$Deprivation <- scale(imputed_abcd$Deprivation)
imputed_abcd$Victimisation <- scale(imputed_abcd$Victimisation)

imputed_abcd$cbcl_internalising <- scale(imputed_abcd$cbcl_internalising)
imputed_abcd$cbcl_externalising <- scale(imputed_abcd$cbcl_externalising)

# Now the mean is 0 and standard deviation is 1
describe(imputed_abcd$Traumatic_Events) 
describe(imputed_abcd$Parental_Threat) 
describe(imputed_abcd$Deprivation) 
describe(imputed_abcd$Victimisation) 

describe(imputed_abcd$cbcl_internalising) 
describe(imputed_abcd$cbcl_externalising) 

#------------------------- Unadjusted Regression Models Part 1: one factor and the outcomes -------------------------#

# Internalising symptoms ~ Traumatic Events
trauma_int <- lm(cbcl_internalising ~ Traumatic_Events, data = imputed_abcd)
summary(trauma_int)

# Externalising symptoms ~ Traumatic Events
trauma_ext <- lm(cbcl_externalising ~ Traumatic_Events, data = imputed_abcd)
summary(trauma_ext)

# Internalising symptoms ~ Parental Threat
threat_int <- lm(cbcl_internalising ~ Parental_Threat, data = imputed_abcd)
summary(threat_int)

# Externalising symptoms ~ Parental Threat
threat_ext <- lm(cbcl_externalising ~ Parental_Threat, data = imputed_abcd)
summary(threat_ext)

# Internalising symptoms ~ Deprivation
dep_int <- lm(cbcl_internalising ~ Deprivation, data = imputed_abcd)
summary(dep_int)

# Externalising symptoms ~ Deprivation
dep_ext <- lm(cbcl_externalising ~ Deprivation, data = imputed_abcd)
summary(dep_ext)

# Internalising symptoms ~ Victimisation
vic_int <- lm(cbcl_internalising ~ Victimisation, data = imputed_abcd)
summary(vic_int)

# Externalising symptoms ~ Victimisation
vic_ext <- lm(cbcl_externalising ~ Victimisation, data = imputed_abcd)
summary(vic_ext)

#------------------------- Adjusted Regression Models Part 2: one factor + covariates and the outcomes -------------------------#

# Internalising symptoms ~ Traumatic Events + Sex + Race
int1 <- lm(cbcl_internalising ~ Traumatic_Events + sex + race, data = imputed_abcd)
summary(int1)

# Externalising symptoms ~ Traumatic Events + Sex + Race
ext1 <- lm(cbcl_externalising ~ Traumatic_Events + sex + race, data = imputed_abcd)
summary(ext1)

# Internalising symptoms ~ Parental Threat + Sex + Race
int2 <- lm(cbcl_internalising ~ Parental_Threat + sex + race, data = imputed_abcd)
summary(int2)

# Externalising symptoms ~ Parental Threat + Sex + Race
ext2 <- lm(cbcl_externalising ~ Parental_Threat + sex + race, data = imputed_abcd)
summary(ext2)

# Internalising symptoms ~ Deprivation + Sex + Race
int3 <- lm(cbcl_internalising ~ Deprivation + sex + race, data = imputed_abcd)
summary(int3)

# Externalising symptoms ~ Deprivation + Sex + Race
ext3 <- lm(cbcl_externalising ~ Deprivation + sex + race, data = imputed_abcd)
summary(ext3)

# Internalising symptoms ~ Victimisation + Sex + Race
int4 <- lm(cbcl_internalising ~ Victimisation + sex + race, data = imputed_abcd)
summary(int4)

# Externalising symptoms ~ Victimisation + Sex + Race
ext4 <- lm(cbcl_externalising ~ Victimisation + sex + race, data = imputed_abcd)
summary(ext4)

#----------------- Regression Model Part 3: all factors + covariates and the outcomes (multiple regression) -----------------#

multireg_lm <- lm(cbind(cbcl_internalising, cbcl_externalising) ~ Traumatic_Events + Parental_Threat + Deprivation + Victimisation + sex + race, data = imputed_abcd)
summary(multireg_lm)

mean(multireg_lm$residuals) # check the mean of residuals is approximately zero
qqnorm(multireg_lm$residuals) # check the residuals are normally distributed

# Calculate variance inflation factor (VIF)
# VIF is a measure to analyze the magnitude of multicollinearity of model terms
# VIF less than 5 indicates a low correlation of that predictor with other predictors
library(performance)
library(car)

# Internalising symptoms
multi_int <- lm(cbcl_internalising ~ Traumatic_Events + Parental_Threat + Deprivation + Victimisation + sex + race, data = imputed_abcd)
summary(multi_int)

check_collinearity(multi_int)
vif(multi_int)

# Externalising symptoms
multi_ext <- lm(cbcl_externalising ~ Traumatic_Events + Parental_Threat + Deprivation + Victimisation + sex + race, data = imputed_abcd)
summary(multi_ext)

check_collinearity(multi_ext)
vif(multi_ext)

#------------------------- Format regression tables into word document -------------------------#
library(stargazer)

## Format unadjusted regression models 
# Internalising symptoms
int_unadjusted <- stargazer(trauma_int, threat_int, dep_int, vic_int,
                            type="html",
                            title="Unadjusted associations between ACE dimensions and internalising symptoms for the ABCD imputed sample",
                            covariate.labels=c("Traumatic Events", "Parental Threat", "Deprivation", "Victimisation"),
                            column.labels=c("Traumatic Events ~ Internalising", "Parental Threat ~ Internalising", "Deprivation ~ Internalising", "Victimisation ~ Internalising"),
                            column.sep.width="15pt",
                            dep.var.caption="Internalising symptoms",
                            dep.var.labels="",
                            ci=TRUE,
                            ci.level=0.95,
                            align=TRUE,
                            out="imp_abcd_unadjusted_internalising.doc",
                            digits=2, digits.extra=3,
                            omit="Constant",
                            single.row=TRUE)

# Externalising symptoms
ext_unadjusted <- stargazer(trauma_ext, threat_ext, dep_ext, vic_ext,
                            type="html",
                            title="Unadjusted associations between ACE dimensions and externalising symptoms for the ABCD imputed sample",
                            covariate.labels=c("Traumatic Events", "Parental Threat", "Deprivation", "Victimisation"),
                            column.labels=c("Traumatic Events ~ Externalising", "Parental Threat ~ Externalising", "Deprivation ~ Externalising", "Victimisation ~ Externalising"),
                            column.sep.width="15pt",
                            dep.var.caption="Externalising symptoms",
                            dep.var.labels="",
                            ci=TRUE,
                            ci.level=0.95,
                            align=TRUE,
                            out="imp_abcd_unadjusted_externalising.doc",
                            digits=2, digits.extra=3,
                            omit="Constant",
                            single.row=TRUE)

## Format adjusted regression models 
# Internalising symptoms
int_adjusted <- stargazer(int1, int2, int3, int4, multi_int,
                          type="html",
                          title="Adjusted associations between ACE dimensions and internalising symptoms for the ABCD imputed sample",
                          covariate.labels=c("Traumatic Events", "Parental Threat", "Deprivation", "Victimisation",
                                             "Female sex", "American Indian/Alaska Native", "Asian", 
                                             "Black/African American", "Native Hawaiian/Pacific Islander", "Other"),
                          column.labels=c("Traumatic Events ~ Internalising", "Parental Threat ~ Internalising", "Deprivation ~ Internalising", "Victimisation ~ Internalising", 
                                          "Traumatic Events + Parental Threat + Deprivation + Victimisation ~ Internalising"),
                          column.sep.width="15pt",
                          dep.var.caption="Internalising symptoms",
                          dep.var.labels="",
                          ci=TRUE,
                          ci.level=0.95,
                          align=TRUE,
                          out="imp_abcd_adjusted_internalising.doc",
                          digits=2, digits.extra=3,
                          omit="Constant",
                          single.row=TRUE)

# Externalising symptoms
ext_adjusted <- stargazer(ext1, ext2, ext3, ext4, multi_ext,
                          type="html",
                          title="Adjusted associations between ACE dimensions and externalising symptoms for the ABCD imputed sample",
                          covariate.labels=c("Traumatic Events", "Parental Threat", "Deprivation", "Victimisation",
                                             "Female sex", "American Indian/Alaska Native", "Asian", 
                                             "Black/African American", "Native Hawaiian/Pacific Islander", "Other"),
                          column.labels=c("Traumatic Events ~ Externalising", "Parental Threat ~ Externalising", "Deprivation ~ Externalising", "Victimisation ~ Externalising", 
                                          "Traumatic Events + Parental Threat + Deprivation + Victimisation ~ Externalising"),
                          column.sep.width="15pt",
                          dep.var.caption="Externalising symptoms",
                          dep.var.labels="",
                          ci=TRUE,
                          ci.level=0.95,
                          align=TRUE,
                          out="imp_abcd_adjusted_externalising.doc",
                          digits=2, digits.extra=3,
                          omit="Constant",
                          single.row=TRUE)

#------------------------- Exploratory analysis: test for interactions and stratify analyses by sex  -------------------------#

# Test if there is an interaction between sex and main effects

# Internalising symptoms
multireg_int_interaction <- lm(cbcl_internalising ~ Traumatic_Events + Parental_Threat + Deprivation + Victimisation + sex + race + sex*Traumatic_Events + sex*Parental_Threat + sex*Deprivation + sex*Victimisation, data = imputed_abcd)
summary(multireg_int_interaction)
# There are no significant interactions for internalising symptoms

# Externalising symptoms
multireg_ext_interaction <- lm(cbcl_externalising ~ Traumatic_Events + Parental_Threat + Deprivation + Victimisation + sex + race + sex*Traumatic_Events + sex*Parental_Threat + sex*Deprivation + sex*Victimisation, data = imputed_abcd)
summary(multireg_ext_interaction)
# There are no significant interactions for externalising symptoms
```
