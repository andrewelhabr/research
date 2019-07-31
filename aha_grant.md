
## Read in Packages and Data

Clear workspace and load in packages that we will need. (Note: visualizations are at the very end of the document since the plotting functions are a little long.)


```R
rm(list = ls())


suppressMessages(library("tidyverse"))
suppressMessages(library("ggforce"))
suppressMessages(library("ggpubr"))
suppressMessages(library("ggalluvial"))
suppressMessages(library("corrplot"))
suppressMessages(library("psych"))
suppressMessages(library("miscset"))
suppressMessages(library("naniar"))
suppressMessages(library("vcd"))
suppressMessages(library("caret"))
suppressMessages(library("vroom"))
```

    Warning message:
    "package 'ggalluvial' was built under R version 3.6.1"Warning message:
    "package 'psych' was built under R version 3.6.1"Warning message:
    "package 'caret' was built under R version 3.6.1"Warning message:
    "package 'vroom' was built under R version 3.6.1"

Load in the REDCap data (same patients that are in the SHOUT database).


```R
redcap <- vroom::vroom("R:/andrew-elhabr/AcuteStrokeDatabase_DATA_2019-06-19_1501.csv")
```

    [1mObservations:[22m 8,750
    [1mVariables:[22m 2,164
    [31mchr[39m  [ 193]: record_id, stroke_id, first_name, last_name, symptoms, disc_same_well, physician,...
    [32mdbl[39m  [ 804]: mr_number, epi, reason_no_admit, discharge_dispo, age_admission, gender, ethnicit...
    [33mlgl[39m  [1127]: race_other, religion_other, nihss_admission_1a, nihss_admission_1b, nihss_admissi...
    [34mdttm[39m [  34]: arrival_date_time, discharge_date_time, datetime_symptoms, datetime_admitted, las...
    [34mdate[39m [   6]: admit_date, dob, nihss_admission_date, fu_datefollowcomp, mrs_disch_date_time, mr...
    
    [90mCall `spec()` for a copy-pastable column specification[39m
    [90mSpecify the column types with `col_types` to quiet this message[39m
    

## Data Cleaning and Mutation

Now, this is the point that we mutate variables that we currently have into new variables that we can potentially use for modeling. These mutations can be conversions of continuous variables into categorical ones or combinations of various given variables. First, we create an arrival year variable, which makes it is easier to stratify our analysis by time.


```R
redcap$arrival_year <- as.factor(lubridate::year(redcap$arrival_date_time))
```

Now, we move on to creating a categorical age variable based on the continuous age variable that we have in our data. The thresholds for the age groups can easily be manipulated. However, if more groups want to be added, extra lines of code will need to be added. Also, for any persons missing an age, we impute their age using the difference between their arrival time and date of birth. (This method has an accuracy of 99.8% when examing the non-missing data.)


```R
redcap$age_alt <- as.numeric(floor((lubridate::date(lubridate::ymd_hms(redcap$arrival_date_time)) 
                                    - lubridate::ymd(redcap$dob))/365.25))
#sum(redcap$age_admission==redcap$age_alt,na.rm=TRUE)/sum(!is.na(redcap$age_admission))*100
redcap <- redcap %>% 
  mutate(age_admission = case_when(is.na(age_admission) & !is.na(age_alt) ~ age_alt,
                                   TRUE ~ age_admission))

threshold_age_1 <- 40
threshold_age_2 <- 50
threshold_age_3 <- 60
threshold_age_4 <- 70
redcap$age_groups <- ifelse(redcap$age_admission<=threshold_age_1,0,
                            ifelse(redcap$age_admission>threshold_age_1 & redcap$age_admission<=threshold_age_2,1,
                                   ifelse(redcap$age_admission>threshold_age_2 & redcap$age_admission<=threshold_age_3,2,
                                          ifelse(redcap$age_admission>threshold_age_3 & redcap$age_admission<=threshold_age_4,3,
                                                 ifelse(redcap$age_admission>threshold_age_4 & redcap$age_admission<=130,4,"NA")))))
redcap$age_groups <- factor(redcap$age_groups)
redcap$age_groups <- plyr::mapvalues(redcap$age_groups,
                                     from=c("0","1","2","3","4"),
                                     to=c(paste0("0-",threshold_age_1),
                                          paste0(threshold_age_1+1,"-",threshold_age_2),
                                          paste0(threshold_age_2+1,"-",threshold_age_3),
                                          paste0(threshold_age_3+1,"-",threshold_age_4),
                                          paste0(">",threshold_age_4)))

threshold_nihss_1 <- 1
threshold_nihss_2 <- 5
threshold_nihss_3 <- 16
threshold_nihss_4 <- 21
redcap$nihss_admit_groups <- ifelse(redcap$nihss_admission_total<threshold_nihss_1,0,
                                    ifelse(redcap$nihss_admission_total>=threshold_nihss_1 & redcap$nihss_admission_total<threshold_nihss_2,1,
                                           ifelse(redcap$nihss_admission_total>=threshold_nihss_2 & redcap$nihss_admission_total<threshold_nihss_3,2,
                                                  ifelse(redcap$nihss_admission_total>=threshold_nihss_3 & redcap$nihss_admission_total<threshold_nihss_4,3,
                                                         ifelse(redcap$nihss_admission_total>=threshold_nihss_4 & redcap$nihss_admission_total<=42,4,"NA")))))
redcap$nihss_admit_groups <- factor(redcap$nihss_admit_groups)
redcap$nihss_admit_groups <- plyr::mapvalues(redcap$nihss_admit_groups,
                                             from=c("0","1","2","3","4"),
                                             to=c("0",
                                                  paste0(threshold_nihss_1,"-",threshold_nihss_2-1),
                                                  paste0(threshold_nihss_2,"-",threshold_nihss_3-1),
                                                  paste0(threshold_nihss_3,"-",threshold_nihss_4-1),
                                                  paste0(threshold_nihss_4,"-42")))
```

Grouping some variables to make mutations easier later on. All following mutations will be performed by code cell.


```R
colnames(redcap)[colnames(redcap)=="brain_imaging_type"] <- "brain_imaging_type_0"
cols_brain_imaging_types <- c("brain_imaging_type_0","brain_imaging_type_1","brain_imaging_type_2","brain_imaging_type_3",
                              "brain_imaging_type_4","brain_imaging_type_5","brain_imaging_type_6","brain_imaging_type_7",
                              "brain_imaging_type_8","brain_imaging_type_9","brain_imaging_type_10","brain_imaging_type_11",
                              "brain_imaging_type_12","brain_imaging_type_13","brain_imaging_type_14","brain_imaging_type_15",
                              "brain_imaging_type_16","brain_imaging_type_17","brain_imaging_type_18","brain_imaging_type_19",
                              "brain_imaging_type_20","brain_imaging_type_21","brain_imaging_type_22","brain_imaging_type_23",
                              "brain_imaging_type_24","brain_imaging_type_25","brain_imaging_type_26","brain_imaging_type_27",
                              "brain_imaging_type_28","brain_imaging_type_29","brain_imaging_type_30","brain_imaging_type_31",
                              "brain_imaging_type_32","brain_imaging_type_33","brain_imaging_type_34","brain_imaging_type_35",
                              "brain_imaging_type_36","brain_imaging_type_37","brain_imaging_type_38","brain_imaging_type_39",
                              "brain_imaging_type_40","brain_imaging_type_41","brain_imaging_type_42","brain_imaging_type_43",
                              "brain_imaging_type_44","brain_imaging_type_45","brain_imaging_type_46","brain_imaging_type_47",
                              "brain_imaging_type_48","brain_imaging_type_49","brain_imaging_type_50")

cols_insurance <- c("insurance___1","insurance___2","insurance___3","insurance___4","insurance___5")
cols_i2f <- c("discharge_dispo","gender","ethnicity","race",
              cols_insurance,"payment_care",
              "stroke_location","ed_patient","first_received","ambulatory_status",cols_brain_imaging_types,"image_interp",
              "med_hx___1","med_hx___2","med_hx___3","med_hx___4","med_hx___5","med_hx___6","med_hx___7","med_hx___8",
              "med_hx___9","med_hx___10","med_hx___11","med_hx___12","med_hx___13","med_hx___14","med_hx___15","med_hx___16",
              "med_hx___17","med_hx___18","med_hx___19","med_hx___20","med_hx___21","med_hx___22","med_hx___23",
              "antiplate_anticoag_med","antihypertensive","cholesterol_reduc","diabetic_med","antidepressant_med",
              "ivtpa_given_here","ivtpa_outside","ia_here","endo_therapy","ivtpa_prior_ia_mech")

redcap[,cols_i2f] <- lapply(redcap[,cols_i2f],factor)

```

Race, ethnicity, gender, final diagnosis


```R
redcap$race_ethn <- ifelse(redcap$ethnicity==0,0,
                           ifelse(!redcap$ethnicity==0 & redcap$race==1,1,
                                  ifelse(!redcap$ethnicity==0 & redcap$race==3,2,
                                         ifelse(!redcap$ethnicity==0 & redcap$race==4,3,
                                                ifelse(!redcap$ethnicity==0 & (redcap$race==0 | redcap$race==2 | redcap$race==5),4,
                                                       ifelse(!redcap$ethnicity==0 & redcap$race==6,5,NA))))))
redcap$race_ethn <- plyr::mapvalues(redcap$race_ethn,
                                    from=c("0","1","2","3","4","5"),
                                    to=c("Hispanic","Asian","Black","White","Other","Unknown"))

redcap$gender<- plyr::mapvalues(redcap$gender,
                                from=c("0","1"),
                                to=c("Female","Male"))

redcap$final_dx <- plyr::mapvalues(redcap$final_dx,
                                   from=c(1,2,3,4,5,6,7),
                                   to=c("Ischemic",
                                        "TIA",
                                        "Sub Hem.",
                                        "Intra Hem.",
                                        "Stroke (Not Specified)",
                                        "No Stroke Diagnosis",
                                        "Carotid Only"))

colnames(redcap)[match(cols_insurance,colnames(redcap))] <- c("medicare","medicaid","selfpay","insurance_na","private")
cols_com <- c("med_hx___14","med_hx___18","med_hx___20","med_hx___21")
colnames(redcap)[match(cols_com,colnames(redcap))] <- c("com_obesity","com_diabetes","com_hypertension","com_prevstroke")
```

Imaging


```R
redcap$NCCT <- apply(redcap[,cols_brain_imaging_types]==0,1,any)
redcap$CTA <- apply(redcap[,cols_brain_imaging_types]==1,1,any)
redcap$CTP <- apply(redcap[,cols_brain_imaging_types]==2,1,any)
redcap$MRI <- apply(redcap[,cols_brain_imaging_types]==3,1,any)
redcap$MRA <- apply(redcap[,cols_brain_imaging_types]==4,1,any)
redcap$MRP <- apply(redcap[,cols_brain_imaging_types]==5,1,any)
redcap$CTwwo <- apply(redcap[,cols_brain_imaging_types]==6,1,any)
redcap$MRIwwo <- apply(redcap[,cols_brain_imaging_types]==7,1,any)
redcap$other <- apply(redcap[,cols_brain_imaging_types]==8,1,any)
redcap$DSA <- apply(redcap[,cols_brain_imaging_types]==9,1,any)

imaging_types <- c("NCCT","CTA","CTP","MRI","MRA","MRP","CTwwo","MRIwwo","other","DSA")
redcap[,imaging_types][is.na(redcap[,imaging_types])] <- FALSE
redcap$imaging_na <- !apply(redcap[,imaging_types]==TRUE,1,any)

redcap[,c(imaging_types,"imaging_na")] <- lapply(redcap[,c(imaging_types,"imaging_na")],factor)
redcap[,c(imaging_types,"imaging_na")] <- lapply(redcap[,c(imaging_types,"imaging_na")],plyr::mapvalues,from=c("FALSE","TRUE"),to=c("0","1"))


```

Treatment


```R
redcap$ivtpa <- ifelse(redcap$ivtpa_given_here==0 & redcap$ivtpa_outside==0 | 
                         redcap$ivtpa_given_here==0 & is.na(redcap$ivtpa_outside) |
                         is.na(redcap$ivtpa_given_here) & redcap$ivtpa_outside==0,0,
                       ifelse(redcap$ivtpa_given_here==1 & redcap$ivtpa_outside==0 |
                                redcap$ivtpa_given_here==0 & redcap$ivtpa_outside==1 |
                                redcap$ivtpa_given_here==1 & redcap$ivtpa_outside==1 |
                                redcap$ivtpa_given_here==1 & is.na(redcap$ivtpa_outside) |
                                is.na(redcap$ivtpa_given_here) & redcap$ivtpa_outside==1,1,"NA"))

redcap$evt <- ifelse(redcap$ia_here==0 & redcap$endo_therapy==0,0,
                     ifelse(redcap$ia_here==1 & (redcap$endo_therapy==1 | redcap$endo_therapy==3),1,"NA"))

redcap$ivtpa_alt <- replace(redcap$ivtpa,is.na(redcap$ivtpa),2)
redcap$evt_alt <- replace(redcap$evt,is.na(redcap$evt),2)
redcap$treatment_4_levels <- ifelse(redcap$ivtpa==0 & redcap$evt==0,"nn",
                                    ifelse(redcap$ivtpa==0 & redcap$evt==1,"ny",
                                           ifelse(redcap$ivtpa==1 & redcap$evt==0,"yn",
                                                  ifelse(redcap$ivtpa==1 & redcap$evt==1,"yy","NA"))))
redcap$treatment_8_levels <- ifelse(redcap$ivtpa_alt==0 & redcap$evt_alt==0,"nn",
                                    ifelse(redcap$ivtpa_alt==0 & redcap$evt_alt==1,"ny",
                                           ifelse(redcap$ivtpa_alt==0 & redcap$evt_alt==2,"nm",
                                                  ifelse(redcap$ivtpa_alt==1 & redcap$evt_alt==0,"yn",
                                                         ifelse(redcap$ivtpa_alt==1 & redcap$evt_alt==1,"yy",
                                                                ifelse(redcap$ivtpa_alt==1 & redcap$evt_alt==2,"ym",
                                                                       ifelse(redcap$ivtpa_alt==2 & redcap$evt_alt==0,"mn",
                                                                              ifelse(redcap$ivtpa_alt==2 & redcap$evt_alt==1,"my","NA"))))))))
redcap$treatment <- ifelse(redcap$treatment_8_levels=="yy" | redcap$treatment_8_levels=="yn" |
                             redcap$treatment_8_levels=="ny" | redcap$treatment_8_levels=="ym" |
                             redcap$treatment_8_levels=="my",1,
                           ifelse(redcap$treatment_8_levels=="nn" | redcap$treatment_8_levels=="nm" |
                                    redcap$treatment_8_levels=="mn",0,"NA"))
cols_treatment <- c("treatment","treatment_4_levels","treatment_8_levels")
redcap[,cols_treatment] <- lapply(redcap[,cols_treatment],factor)
redcap[,"treatment"][redcap[,"treatment"]=="NA"] <- NA
redcap[,"treatment_8_levels"][redcap[,"treatment_8_levels"]=="NA"] <- NA
```

Outcomes


```R
redcap$mrs_pdischarge <- factor(redcap$mrs_pdischarge,
                                levels = c("0","0 - No symptoms at all",
                                           "1","1 - No significant disability despite symptoms: Able to carry out all usual activities",
                                           "2","2 - Slight disability",
                                           "3","3 - Moderate disability: Requiring some help but able to walk without assistance",
                                           "4","4 - Moderate to severe disability: Unable to walk without assistance and unable to attend to own bodily needs without assistance",
                                           "5","5 - Severe disability: Bedridden, incontinent and requiring constant nursing care and attention",
                                           "6"),
                                labels = c(0,0,1,1,2,2,3,3,4,4,5,5,6))
redcap$mrs_pdischarge <- as.integer(redcap$mrs_pdischarge)-1

redcap <- naniar::replace_with_na(redcap,replace=list(mrs_pdischarge_90=c("unable to determine mRS","unable to reach")))
redcap$mrs_pdischarge_90 <- as.factor(redcap$mrs_pdischarge_90)
redcap$mrs_pdischarge_90 <- droplevels(redcap$mrs_pdischarge_90)
redcap$mrs_pdischarge_90 <- as.integer(redcap$mrs_pdischarge_90)-1

redcap <- redcap %>% 
  mutate(mrs_pdischarge = case_when(is.na(mrs_pdischarge) & tot_mrs_discharge==6 ~ 6,
                                    TRUE ~ mrs_pdischarge))
redcap <- redcap %>% 
  mutate(mrs_pdischarge_90 = case_when(is.na(mrs_pdischarge_90) & tot_mrs_discharge==6 ~ 6,
                                       TRUE ~ mrs_pdischarge_90))

threshold_mrs <- 3

redcap$mrs_dc_2_levels <- ifelse(redcap$tot_mrs_discharge<threshold_mrs,0,
                                 ifelse(redcap$tot_mrs_discharge>=threshold_mrs & redcap$tot_mrs_discharge<=6,1,"NA"))
redcap$mrs_30_2_levels <- ifelse(redcap$mrs_pdischarge<3,0,
                                 ifelse(redcap$mrs_pdischarge>=3 & redcap$mrs_pdischarge<=6,1,"NA"))
redcap$mrs_90_2_levels <- ifelse(redcap$mrs_pdischarge_90<3,0,
                                 ifelse(redcap$mrs_pdischarge_90>=3 & redcap$mrs_pdischarge_90<=6,1,"NA"))

redcap$mrs_dc <- ifelse(redcap$tot_mrs_discharge<3,0,
                        ifelse(redcap$tot_mrs_discharge>=3 & redcap$tot_mrs_discharge<=5,1,
                               ifelse(redcap$tot_mrs_discharge==6,2,"NA")))
redcap$mrs_30 <- ifelse(redcap$mrs_pdischarge<3,0,
                        ifelse(redcap$mrs_pdischarge>=3 & redcap$mrs_pdischarge<=5,1,
                               ifelse(redcap$mrs_pdischarge==6,2,"NA")))
redcap$mrs_90 <- ifelse(redcap$mrs_pdischarge_90<3,0,
                        ifelse(redcap$mrs_pdischarge_90>=3 & redcap$mrs_pdischarge_90<=5,1,
                               ifelse(redcap$mrs_pdischarge_90==6,2,"NA")))

redcap$mrs_dc_mort <- ifelse(redcap$tot_mrs_discharge<6,0,
                             ifelse(redcap$tot_mrs_discharge==6,1,"NA"))
redcap$mrs_30_mort <- ifelse(redcap$mrs_pdischarge<6,0,
                             ifelse(redcap$mrs_pdischarge==6,1,"NA"))
redcap$mrs_90_mort <- ifelse(redcap$mrs_pdischarge_90<6,0,
                             ifelse(redcap$mrs_pdischarge_90==6,1,"NA"))

cols_c2f <- c("age_groups","final_dx","ivtpa","evt","tot_mrs_discharge","mrs_pdischarge","mrs_pdischarge_90",
              "mrs_dc_2_levels","mrs_30_2_levels","mrs_90_2_levels","mrs_dc","mrs_30","mrs_90",
              "mrs_dc_mort","mrs_30_mort","mrs_90_mort")
redcap[,cols_c2f] <- lapply(redcap[,cols_c2f],factor)

redcap$mrs_dc_2_levels <- plyr::mapvalues(redcap$mrs_dc_2_levels,from=c("0","1"),to=c("0-2","3-6"))
redcap$mrs_30_2_levels <- plyr::mapvalues(redcap$mrs_30_2_levels,from=c("0","1"),to=c("0-2","3-6"))
redcap$mrs_90_2_levels <- plyr::mapvalues(redcap$mrs_90_2_levels,from=c("0","1"),to=c("0-2","3-6"))
redcap$mrs_dc <- plyr::mapvalues(redcap$mrs_dc,from=c("0","1","2"),to=c("0-2","3-5","6"))
redcap$mrs_30 <- plyr::mapvalues(redcap$mrs_30,from=c("0","1","2"),to=c("0-2","3-5","6"))
redcap$mrs_90 <- plyr::mapvalues(redcap$mrs_90,from=c("0","1","2"),to=c("0-2","3-5","6"))
redcap$discharge_dispo <- plyr::mapvalues(redcap$discharge_dispo,
                                          from=c("1","2","3","4","5","6","7","8"),
                                          to=c("Home","Hospice-Home","Hospice-Health Care Facility",
                                               "Acute Care Facility","Other Health Care Facility","Expired","AMA","UTD"))

redcap$discharge_dispo_alt <- factor(ifelse(redcap$discharge_dispo=="Home","Home",
                                            ifelse(redcap$discharge_dispo=="Other Health Care Facility",
                                                   "Other Health Care Facility","Other")))
```

Transfer status


```R
redcap$how_arrived <- factor(redcap$how_arrived)
redcap$how_arrived <- plyr::mapvalues(redcap$how_arrived,
                                      from=c("1","2","3","4","5"),
                                      to=c("EMS","Priv. Transp.","Transfer","ND/Unknown","Unknown Transp."))
redcap$how_arrived_alt <- ifelse(redcap$how_arrived=="Transfer","Transfer","Not Transfer")
```

Stroke variables that we are interested in using for analysis.


```R
cols_consider <- c("record_id","arrival_date_time","discharge_date_time","how_arrived","arrival_year","nihss_admit_groups",
                   "age_admission","age_groups","gender","ethnicity","race","race_ethn",
                   "medicare","medicaid","selfpay","insurance_na","private",
                   "payment_care","nihss_admission_total","stroke_location","ed_patient","first_received","ambulatory_status",
                   "principal_dx_code","final_dx","image_interp","brain_imaging_type_0","imaging_complete",
                   "NCCT","CTA","CTP","MRI","MRA","MRP","CTwwo","MRIwwo","other","DSA","imaging_na",
                   "com_obesity","com_diabetes","com_hypertension","com_prevstroke",
                   "antiplate_anticoag_med","antihypertensive","cholesterol_reduc","diabetic_med","antidepressant_med",
                   "ivtpa_given_here","ivtpa_outside","ivtpa_prior_ia_mech","ivtpa",
                   "ia_here","endo_therapy","evt",
                   "ivtpa_prior_ia_mech","treatment","treatment_4_levels","treatment_8_levels",
                   "discharge_dispo","mrs_pdischarge","mrs_pdischarge_90","tot_mrs_discharge",
                   "mrs_dc","mrs_30","mrs_90","mrs_dc_2_levels","mrs_30_2_levels","mrs_90_2_levels",
                   "mrs_dc_mort","mrs_30_mort","mrs_90_mort",
                   "discharge_dispo_alt","how_arrived_alt")
```

Limit the data to two sets: diagnosis of stroke and diagnosis of stroke PLUS not expired by discharge.


```R
stroke <- redcap %>%
    filter(final_dx=="Ischemic") %>%
    select(cols_consider)
stroke_alive_at_dc <- stroke %>%
    filter(mrs_dc!=6 | is.na(mrs_dc)) %>%
    select(everything())

stroke$discharge_dispo <- droplevels(stroke$discharge_dispo)
stroke_alive_at_dc$discharge_dispo <- droplevels(stroke_alive_at_dc$discharge_dispo)
```

## Example of Machine Learning

Application of a classification tree with the caret() package.


```R
fmla <- formula(mrs_dc_2_levels ~ 
  nihss_admission_total +
  race_ethn +
  treatment_4_levels +
  discharge_dispo_alt +
  age_groups +
  gender)

fit_rpart <- caret::train(form = fmla, data = stroke, method = "rpart",
                          trControl = trainControl(method = "cv"),
                          metric = "Accuracy",
#                          tuneGrid = expand.grid(cp=c(0.1,0.01,0.001,0.0001,0.00001)),
                          na.action = na.omit)
fit_rpart
fit_rpart$finalModel
```


    CART 
    
    4659 samples
       6 predictor
       2 classes: '0-2', '3-6' 
    
    No pre-processing
    Resampling: Cross-Validated (10 fold) 
    Summary of sample sizes: 3439, 3439, 3438, 3439, 3439, 3440, ... 
    Resampling results across tuning parameters:
    
      cp          Accuracy   Kappa    
      0.01632653  0.7762194  0.5560452
      0.01865889  0.7649580  0.5269710
      0.44956268  0.6441441  0.2309687
    
    Accuracy was used to select the optimal model using the largest value.
    The final value used for the model was cp = 0.01632653.



    n= 3821 
    
    node), split, n, loss, yval, (yprob)
          * denotes terminal node
    
    1) root 3821 1715 0-2 (0.5511646 0.4488354)  
      2) nihss_admission_total< 5.5 2296  567 0-2 (0.7530488 0.2469512)  
        4) discharge_dispo_altOther Health Care Facility< 0.5 1625  224 0-2 (0.8621538 0.1378462)  
          8) discharge_dispo_altOther< 0.5 1540  157 0-2 (0.8980519 0.1019481) *
          9) discharge_dispo_altOther>=0.5 85   18 3-6 (0.2117647 0.7882353) *
        5) discharge_dispo_altOther Health Care Facility>=0.5 671  328 3-6 (0.4888227 0.5111773) *
      3) nihss_admission_total>=5.5 1525  377 3-6 (0.2472131 0.7527869) *



```R
plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)
```


![png](output_26_0.png)


## Examples of Data Visualization


```R
plot_one_var <- function(df,var,var_with_na=TRUE,vert=FALSE,bar_width=0.5){
  if (var_with_na==TRUE){
    if (vert==FALSE){                          
      p1 <- df %>%
        ggplot(aes_string(x=var)) +
        geom_bar(width=bar_width,fill="blue") +
        geom_text(stat="count",aes(label=..count..),vjust=-0.2)
    } else {
      p1 <- df %>%
        ggplot(aes_string(x=var)) +
        geom_bar(width=bar_width,fill="blue") +
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
        geom_text(stat="count",aes(label=..count..),vjust=-0.2)
    }
    suppressWarnings(var_df <- df %>%
                       count_(var) %>%
                       mutate(freq=n/sum(n)*100) %>%
                       mutate(end=2*pi*cumsum(freq)/sum(freq),
                              start=lag(end,default=0),
                              middle=0.5*(start+end),
                              hjust=ifelse(middle>pi,1,0),
                              vjust=ifelse(middle<pi/2 | middle>3*pi/2,0,1)))
    p2 <- var_df %>%
      ggplot(aes(x0=0,y0=0,r0=0,r=1.5,start=start,end=end)) +
      geom_arc_bar(aes_string(fill=var)) +
      geom_text(aes(x=1.55*sin(middle),y=1.55*cos(middle),label=paste0(round(freq),"%"),hjust=hjust,vjust=vjust),
                size=3) +
      coord_fixed() +
      scale_x_continuous(limits=c(-2,2),name="",breaks=NULL,labels=NULL) +
      scale_y_continuous(limits=c(-2,2),name="",breaks = NULL,labels = NULL) +
      theme_minimal()
    ggpubr::ggarrange(p1,p2,ncol=2)
  } else {
    if (vert==FALSE){                          
      p1 <- df %>%
        drop_na(var) %>%
        ggplot(aes_string(x=var)) +
        geom_bar(width=bar_width,fill="blue") +
        geom_text(stat="count",aes(label=..count..),vjust=-0.2)
    } else {
      p1 <- df %>%
        ggplot(aes_string(x=var)) +
        geom_bar(width=bar_width,fill="blue") +
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
        geom_text(stat="count",aes(label=..count..),vjust=-0.2)
    }
    var_df <- df %>%
      drop_na(var) %>%
      count_(var) %>%
      mutate(freq=n/sum(n)*100) %>%    
      mutate(end=2*pi*cumsum(freq)/sum(freq),
             start=lag(end,default=0),
             middle=0.5*(start+end),
             hjust=ifelse(middle>pi,1,0),
             vjust = ifelse(middle<pi/2 | middle>3*pi/2,0,1))
    p2 <- var_df %>%
      ggplot(aes(x0=0,y0=0,r0=0,r=1.5,start=start,end=end)) +
      geom_arc_bar(aes_string(fill=var)) +
      geom_text(aes(x=1.55*sin(middle),y=1.55*cos(middle),label=paste0(round(freq),"%"),hjust=hjust,vjust=vjust),
                size=3) +
      coord_fixed() +
      scale_x_continuous(limits=c(-2,2),name="",breaks=NULL,labels=NULL) +
      scale_y_continuous(limits=c(-2,2),name="",breaks = NULL,labels = NULL) +
      theme_minimal()
    ggpubr::ggarrange(p1,p2,ncol=2)
  }
}

plot_two_var <- function(df,var_1,var_2,var_1_with_na=TRUE,var_2_with_na=TRUE,bar_width=0.9,label_type="each"){
  if(var_1_with_na==TRUE){
    if(var_2_with_na==TRUE){
      if(label_type=="each"){
        p1 <- df %>%
          ggplot(aes_string(x=var_1,fill=var_2)) + 
          geom_bar(width=bar_width,position="stack") +
          geom_text(stat="count",aes(label=..count..),size=2.5,position=position_stack(vjust=0.5)) +
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
          labs(y="count")
      } else {
        totals <- df %>%
          count_(var_1)
        if(label_type=="total"){
          p1 <- df %>%
            ggplot(aes_string(x=var_1,fill=var_2)) + 
            geom_bar(width=bar_width,position="stack") +
            geom_text(aes_string(x=var_1,y="n",label="n",fill=NULL),data=totals,size=3,vjust=-0.2) +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
            labs(y="count")
        } else {
          p1 <- df %>%
            ggplot(aes_string(x=var_1,fill=var_2)) + 
            geom_bar(width=bar_width,position="stack") +
            geom_text(stat="count",aes(label=..count..),size=2.5,position=position_stack(vjust=0.5)) +
            geom_text(aes_string(x=var_1,y="n",label="n",fill=NULL),data=totals,size=3,vjust=-0.2) +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
            labs(y="count")
        }
      }
      p2 <- df %>%
        ggplot(aes_string(x=var_1,fill=var_2)) + 
        geom_bar(width=bar_width,position="fill") +
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
        labs(y="proportion") +
        scale_y_continuous(labels=scales::percent)
      ggpubr::ggarrange(p1,p2,ncol=2,common.legend=TRUE,legend="bottom")  
    } else {
      if(label_type=="each"){
        p1 <- df %>%
          drop_na(var_2) %>%
          ggplot(aes_string(x=var_1,fill=var_2)) + 
          geom_bar(width=bar_width,position="stack") +
          geom_text(stat="count",aes(label=..count..),size=2.5,position=position_stack(vjust=0.5)) +
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
          labs(y="count")
      } else {
        totals <- df %>%
          drop_na(var_2) %>%
          count_(var_1)
        if(label_type=="total"){
          p1 <- df %>%
            drop_na(var_2) %>%
            ggplot(aes_string(x=var_1,fill=var_2)) + 
            geom_bar(width=bar_width,position="stack") +
            geom_text(aes_string(x=var_1,y="n",label="n",fill=NULL),data=totals,size=3,vjust=-0.2) +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
            labs(y="count")
        } else {
          p1 <- df %>%
            drop_na(var_2) %>%
            ggplot(aes_string(x=var_1,fill=var_2)) + 
            geom_bar(width=bar_width,position="stack") +
            geom_text(stat="count",aes(label=..count..),size=2.5,position=position_stack(vjust=0.5)) +
            geom_text(aes_string(x=var_1,y="n",label="n",fill=NULL),data=totals,size=3,vjust=-0.2) +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
            labs(y="count")
        }
      }
      p2 <- df %>%
        drop_na(var_2) %>%
        ggplot(aes_string(x=var_1,fill=var_2)) + 
        geom_bar(width=bar_width,position="fill") +
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
        labs(y="proportion") +
        scale_y_continuous(labels=scales::percent)
      ggpubr::ggarrange(p1,p2,ncol=2,common.legend=TRUE,legend="bottom")
    }
  } else {
    if(var_2_with_na==TRUE){
      if(label_type=="each"){
        p1 <- df %>%
          drop_na(var_1) %>%
          ggplot(aes_string(x=var_1,fill=var_2)) + 
          geom_bar(width=bar_width,position="stack") +
          geom_text(stat="count",aes(label=..count..),size=2.5,position=position_stack(vjust=0.5)) +
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
          labs(y="count")
      } else {
        totals <- df %>%
          drop_na(var_1) %>%
          count_(var_1)
        if(label_type=="total"){
          p1 <- df %>%
            drop_na(var_1) %>%
            ggplot(aes_string(x=var_1,fill=var_2)) + 
            geom_bar(width=bar_width,position="stack") +
            geom_text(aes_string(x=var_1,y="n",label="n",fill=NULL),data=totals,size=3,vjust=-0.2) +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
            labs(y="count")
        } else {
          p1 <- df %>%
            drop_na(var_1) %>%
            ggplot(aes_string(x=var_1,fill=var_2)) + 
            geom_bar(width=bar_width,position="stack") +
            geom_text(stat="count",aes(label=..count..),size=2.5,position=position_stack(vjust=0.5)) +
            geom_text(aes_string(x=var_1,y="n",label="n",fill=NULL),data=totals,size=3,vjust=-0.2) +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
            labs(y="count")
        }
      }
      p2 <- df %>%
        drop_na(var_1) %>%
        ggplot(aes_string(x=var_1,fill=var_2)) + 
        geom_bar(width=bar_width,position="fill") +
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
        labs(y="proportion") +
        scale_y_continuous(labels=scales::percent)
      ggpubr::ggarrange(p1,p2,ncol=2,common.legend=TRUE,legend="bottom")  
    } else {
      if(label_type=="each"){
        p1 <- df %>%
          drop_na(var_1,var_2) %>%
          ggplot(aes_string(x=var_1,fill=var_2)) + 
          geom_bar(width=bar_width,position="stack") +
          geom_text(stat="count",aes(label=..count..),size=2.5,position=position_stack(vjust=0.5)) +
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
          labs(y="count")
      } else {
        totals <- df %>%
          drop_na(var_1,var_2) %>%
          count_(var_1)
        if(label_type=="total"){
          p1 <- df %>%
            drop_na(var_1,var_2) %>%
            ggplot(aes_string(x=var_1,fill=var_2)) + 
            geom_bar(width=bar_width,position="stack") +
            geom_text(aes_string(x=variable_1,y="n",label="n",fill=NULL),data=totals,size=3,vjust=-0.2) +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
            labs(y="count")
        } else {
          p1 <- df %>%
            drop_na(var_1,var_2) %>%
            ggplot(aes_string(x=var_1,fill=var_2)) + 
            geom_bar(width=bar_width,position="stack") +
            geom_text(stat="count",aes(label=..count..),size=2.5,position=position_stack(vjust=0.5)) +
            geom_text(aes_string(x=var_1,y="n",label="n",fill=NULL),data=totals,size=3,vjust=-0.2) +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
            labs(y="count")
        }
      }
      p2 <- df %>%
        drop_na(var_1,var_2) %>%
        ggplot(aes_string(x=var_1,fill=var_2)) + 
        geom_bar(width=bar_width,position="fill") +
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
        labs(y="proportion") +
        scale_y_continuous(labels=scales::percent)
      ggpubr::ggarrange(p1,p2,ncol=2,common.legend=TRUE,legend="bottom")
    }
  }
}
```


```R
options(repr.plot.width=8, repr.plot.height=5)
plot_one_var(stroke,"arrival_year",var_with_na=TRUE,bar_width=0.5)
```


![png](output_29_0.png)



```R
plot_two_var(stroke,"race_ethn","gender",var_1_with_na=TRUE)
```


![png](output_30_0.png)



```R
plot_two_var(stroke,"age_groups","nihss_admit_groups",var_2_with_na=FALSE)
```


![png](output_31_0.png)



```R
plot_two_var(stroke,"discharge_dispo","mrs_dc",var_2_with_na=FALSE)
```


![png](output_32_0.png)



```R
totals <- stroke_alive_at_dc %>%
                drop_na(mrs_pdischarge,mrs_pdischarge_90) %>%
                count(tot_mrs_discharge,mrs_pdischarge,mrs_pdischarge_90)

totals %>% 
    ggplot(aes(y=n,axis1=tot_mrs_discharge,axis2=mrs_pdischarge,axis3=mrs_pdischarge_90)) +
    geom_alluvium(aes(fill=tot_mrs_discharge),width=0,reverse=FALSE) +
    guides(fill=FALSE) +
    geom_stratum(width=1/8,reverse=FALSE) +
    geom_text(stat="stratum",label.strata=TRUE,reverse=FALSE) +
    scale_x_continuous(breaks=1:3,labels=c("mrs_dc","mrs_30","mrs_90")) +
    ggtitle("mRS Flow from Discharge to 90 Days Out")
```

    Warning message in to_lodes_form(data = data, axes = axis_ind, discern = params$discern):
    "Some strata appear at multiple axes."Warning message in to_lodes_form(data = data, axes = axis_ind, discern = params$discern):
    "Some strata appear at multiple axes."Warning message in to_lodes_form(data = data, axes = axis_ind, discern = params$discern):
    "Some strata appear at multiple axes."


![png](output_33_1.png)

