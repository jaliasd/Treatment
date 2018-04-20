#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Program Details      ~~~~~~~====================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#============================================================
#   Programmer: Imran Mujawar
#   Date started: 11/01/2017
#   Date updated: 04/12/2018
#   Program: Integrate the following datasets for Excel iTSD tool
#           1) DATIM Genie extract 
#           2) FactView data 
#           3) NAT_SUBNAT data
#           4) EA data
#           5) SIMS data
#============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: Program Details ~~~~~~~====================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Loading the Packages ~~~~~~~====================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())


# Make library call for packages 
library(tidyverse)
library(readr)
library(eply)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: Loading the Packages ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Choose OU name      ~~~~~~~=====================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ou <- "Nigeria" # <--- Specify Operating Unit
oux <- "Nigeria" # <--- Specify name in FactView file and output
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: Choose OU name      ~~~~~~~================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= folder and file locations ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd
("C:/Mujawar/lrz5/AAA_Recent/Temp_Workstream/CnT/PTSD/TxSD_next/R code")

raw_data <- "C:/Mujawar/lrz5/AAA_Recent/Temp_Workstream/CnT/PTSD/TxSD_next/Raw_Data/"

out_put <- "C:/Mujawar/lrz5/AAA_Recent/Temp_Workstream/CnT/PTSD/TxSD_next/Output/"

datastore_site <- "C:/Mujawar/lrz5/AAA_Recent/DataStore/March_23/Site_IM/"

datastore_nat <- "C:/Mujawar/lrz5/AAA_Recent/DataStore/Dec_22/"

datastore_sims <- "C:/Mujawar/lrz5/AAA_Recent/DataStore/March_23/SIMS/"

datastore_ea <- "C:/Mujawar/lrz5/AAA_Recent/DataStore/EA_2017/"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: folder and file locations ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Creating basic functions to show top few rows of data
View50 <- function(x){View(x[1:50,])}
View100 <- function(x){View(x[1:100,])}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Pulling in the datasets  ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

factloc <- paste(datastore_site, 
    "ICPI_MER_Structured_Dataset_Site_IM_", oux,
    "_20180323_v2_1.txt", sep="")

# Pulling in FactView Site-IM data ~~~~~~~~~~~~~~~~~~~~~~~~
fact <- read_tsv(file = factloc, 
                    col_types = cols(MechanismID = "c",
                                     FY2015Q2 = "d",      
                                     FY2015Q3 = "d",      
                                     FY2015Q4 = "d",      
                                     FY2015APR = "d",     
                                     FY2016_TARGETS = "d",
                                     FY2016Q1 = "d",      
                                     FY2016Q2 = "d",      
                                     FY2016Q3 = "d",      
                                     FY2016Q4 = "d",      
                                     FY2016APR = "d",     
                                     FY2017_TARGETS = "d",
                                     FY2017Q1 = "d",      
                                     FY2017Q2 = "d",      
                                     FY2017Q3 = "d",      
                                     FY2017Q4 = "d",      
                                     FY2017APR = "d",
                                     FY2018Q1 = "d",
                                     FY2017_TARGETS = "d",
                                     FY2018_TARGETS = "d"))

# Checking class of the variables
sapply(fact, class)


# Pulling in NAT_SUBNAT data ~~~~~~~~~~~~~~~~~
nat <- read_tsv(file=paste(datastore_nat, "ICPI_FactView_NAT_SUBNAT_20171222_v2_1.txt", sep=""), 
                   col_types = cols(FY2016 = "d",                 
                                    FY2017 = "d"))                     

# Keeping only data for country of interest
nat <- nat  %>% filter(OperatingUnit==ou)


# Pulling in the SIMS data
sims <- 
  read_tsv(
    file=paste(datastore_sims, 
    "ICPI_SIMS_Structured_Dataset_20180323_v2_2_", ou, ".txt", sep=""), 
    col_names = TRUE,
    col_types = cols(MechanismID = "c",
                     CEE_ID = "c",
                     CEE_SCORE_VALUE = "n",
                     ASMT_NEST_RANKING = "n",
                     ASMT_CHRON_RANKING = "n"))


# Pulling in the EA data
ea_1 <- read_csv(
  file=paste(datastore_ea, "2015-2017 ICPI Batch1 Countries FV 10JAN18.csv", sep=""),
  col_names = TRUE,
  col_types = cols(mechanismid = "c",
                   fy15 = "d",
                   fy16 = "d",
                   fy17 = "d"))

ea_2 <- read_csv(
  file=paste(datastore_ea, "2015-2017 ICPI Batch2 Countries FV 10JAN18.csv", sep=""),
  col_names = TRUE,
  col_types = cols(mechanismid = "c",
                   fy15 = "d",
                   fy16 = "d",
                   fy17 = "d"))

eax <- bind_rows(ea_1, ea_2)

# Checking class of the variables
sapply(eax, class)

# Keeping only data for country of interest
ea <- eax  %>% filter(OU==oux)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: Pulling in the datasets  ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Restructuring MER dataset  ~~~~~~~=========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### restructuring FactView dataset ~~~~~~~~~~~~~~~~

fact1 <- fact %>% 
  # filter for indicators needed for TSD
  filter(indicator %in% 
         c("HTS_TST",
           "HTS_TST_POS", 
           "TX_CURR", 
           "TX_NEW", 
           "TX_RET",
           "HRH_STAFF",
           "HRH_CURR",
           "HRH_CURR_ClinicalCadre",
           "SC_STOCK")) %>% 
  # Keeping only the FY16APR and target data
  select(-c(FY2015Q2, FY2015Q3, FY2015Q4, FY2015APR,
            FY2016Q1, FY2016Q2, FY2016Q3, FY2016Q4, FY2016APR, FY2016_TARGETS,
            FY2017Q1, FY2017Q2, FY2017Q3, FY2017Q4, CurrentSNUPrioritization)) %>% 
  # Converting to long format by period
  gather(period, valuex, 42:45) %>% 
  # Removing null values 
  filter(!is.na(valuex)) %>% 
  # creating variable to differentiate the two SC_STOCK variables
  mutate(stock_ind = case_when(
            indicator == "SC_STOCK" & 
            standardizedDisaggregate == "ObservedCommodity" &
            otherDisaggregate == "Rapid Test Kit"               ~ "hts",
            indicator == "SC_STOCK" & 
            standardizedDisaggregate == "ObservedCommodity" &
            otherDisaggregate == "First Line ARV"               ~ "txx"))


# Creating the F_C variable with No_disagg, Fine, MCAD choices
fact2 <-  fact1 %>%
  mutate(F_C =  
           case_when(
             indicator %in% c("HTS_TST",
                              "HTS_TST_POS", 
                              "TX_CURR", 
                              "TX_NEW")                                    ~
               case_when(
                 standardizedDisaggregate %in% c("Total Numerator")                  ~ "N",
                 standardizedDisaggregate %in% c("Modality/MostCompleteAgeDisagg",  
                                                 "MostCompleteAgeDisagg")              ~ "M",
                 standardizedDisaggregate %in% c("Modality/Age/Sex/Result",  
                                                 "AgeAboveTen/Sex/Positive",  
                                                 "AgeLessThanTen/Positive",  
                                                 "Age/Sex/Result",  
                                                 "Age/Sex/HIVStatus")                ~ "F"),
             
             indicator %in% c("TX_RET")          ~
               case_when(
                 standardizedDisaggregate %in% c("Total Numerator",
                                                 "Total Denominator")                ~ "N",
                 standardizedDisaggregate %in% c("Age Aggregated/Sex")               ~ "M",
                 standardizedDisaggregate %in% c("Age/Sex",
                                                 "Age/Sex/HIVStatus")                ~ "F"),
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

             indicator %in% 
               c("HRH_STAFF", "HRH_CURR", "HRH_CURR_ClinicalCadre")                  ~
               case_when(
               standardizedDisaggregate == "Total Numerator" ~ "N"),
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

             indicator == "SC_STOCK"                         ~
               case_when(
               standardizedDisaggregate == "ObservedCommodity" &
               otherDisaggregate %in% c("Rapid Test Kit",
                                        "First Line ARV")     ~ "N")))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Molding the dataset to TSD format ~~~~~~~~~~~~~~~~
fact3 <- fact2 %>% 
  # removing values which have NULL for F_C
  filter(F_C != "NULL") %>% 
  # Reconciling and recoding Age and Sex variables
  mutate(
Age = 
  case_when(
    F_C=="N"                                      ~ "",
    F_C %in% c("M","F") & Age==""                 ~ "Unknown Age",
    TRUE                                          ~ Age)) %>% 
  mutate(  
Sex = 
  case_when(
    F_C=="N"                                      ~ "",
    F_C %in% c("M","F") &    
      Age %in% 
      c("01-04", 
        "01-09", 
        "05-09", 
        "05-14", 
        "10-14",
        "<01", 
        "<05",
        "<15")                                    ~ "Unknown Sex",
    F_C %in% c("M","F") &    
    Age==""                                       ~ "Unknown Sex",
    TRUE                                          ~ Sex)
) %>% 
  
# Creating site_name, site_id and site_type variables ~~~~~~~~~~~~~~~~~~
  mutate(
site_type = 
  case_when(
    orgUnitUID==FacilityUID   ~ "Facility",
    orgUnitUID==CommunityUID  ~"Community",
    typeMilitary=="Y"         ~"Military",
    TRUE                      ~ "") ) 


  
# Creating the site name variable
fact3$site_name = with(fact3, 
    ifelse(site_type=="Facility", Facility,
    ifelse(site_type=="Community", Community,
    ifelse(site_type=="Military", PSNU, ""))))

rm(fact1, fact2)

# Aggregating the data for Epoch FY_17 ~~~~~~~~~~~~~~
fact4 <- fact3 %>% 
  group_by(
    orgUnitUID,
    site_type,
    site_name,
    Region,
    RegionUID,
    OperatingUnit,
    OperatingUnitUID,
    CountryName,
    SNU1,
    # CurrentSNUPrioritization,
    typeMilitary,
    PSNU,
    PSNUuid,
    FundingAgency,
    PrimePartner,
    ImplementingMechanismName,
    MechanismID,
    indicator,
    stock_ind,
    numeratorDenom,
    period,
    F_C,
    Age,
    Sex) %>% 
  summarize(val = sum(valuex, na.rm=T)) %>% 
  ungroup()


# Coding in the suffixes of the variables
fact4x <-  fact4 %>% 
  mutate(var_suffix = 
           case_when(
            indicator %in% c("HTS_TST",
                              "HTS_TST_POS", 
                              "TX_CURR", 
                              "TX_NEW",
                             "HRH_STAFF",
                             "HRH_CURR")          ~
               case_when (
                 period=="FY2018Q1"             ~ "Now_R",
                 period=="FY2018_TARGETS"        ~ "Now_T",
                 period=="FY2017APR"             ~ "Prev_R"),
            indicator %in% c("TX_RET")  ~
               case_when (
                 numeratorDenom=="N" & period=="FY2017APR"     ~ "Now_N",
                 numeratorDenom=="D" & period=="FY2017APR"     ~ "Now_D"),
            indicator %in% c("SC_STOCK")  ~
               case_when (
                 numeratorDenom=="N" & period=="FY2017APR" 
                 ~ paste(stock_ind, "Now_N", sep="_"), 
                 numeratorDenom=="D" & period=="FY2017APR" 
                 ~ paste(stock_ind, "Now_D", sep="_"))            
                    )
          ) %>% 
  # Removing non-epoch 17 values 
  filter(!is.na(var_suffix)) %>% 
  mutate(Epoch = "FY18 Cum.")


rm(fact3, fact4)

fact5 <- fact4x %>% ungroup() %>% 
  mutate(varname = paste(indicator, var_suffix, sep="_")) %>% 
  # rename(FY17SNUPrioritization = CurrentSNUPrioritization) %>% 
  filter(varname %in% c(
    "HTS_TST_POS_Now_R",
    "HTS_TST_POS_Now_T",
    "HTS_TST_Now_R",
    "HTS_TST_Now_T",
    "TX_CURR_Now_R",
    "TX_CURR_Now_T",
    "TX_CURR_Prev_R",
    "TX_NEW_Now_R",
    "TX_NEW_Now_T",
    "TX_RET_Now_D",
    "TX_RET_Now_N",
    "HRH_STAFF_Prev_R", 
    "HRH_CURR_Prev_R",
    "SC_STOCK_hts_Now_D",
    "SC_STOCK_hts_Now_N",
    "SC_STOCK_txx_Now_D",
    "SC_STOCK_txx_Now_N"
    )) %>% 
  select(
    Region,
    RegionUID,
    OperatingUnit,
    OperatingUnitUID,
    CountryName,
    SNU1,
    typeMilitary,
    PSNU,
    PSNUuid,
    FundingAgency,
    PrimePartner,
    ImplementingMechanismName,
    MechanismID,
    F_C,
    Age,
    Sex,
    Epoch,
    # FY17SNUPrioritization,
    orgUnitUID,
    site_type,
    site_name,
    varname,
    val
  ) %>% 
  group_by(
    Region,
    RegionUID,
    OperatingUnit,
    OperatingUnitUID,
    CountryName,
    SNU1,
    typeMilitary,
    PSNU,
    PSNUuid,
    FundingAgency,
    PrimePartner,
    ImplementingMechanismName,
    MechanismID,
    F_C,
    Age,
    Sex,
    Epoch,
    # FY17SNUPrioritization,
    orgUnitUID,
    site_type,
    site_name,
    varname
  ) %>% 
  summarize(vals = sum(val, na.rm=T)) %>% 
  ungroup()

# Final wide dataset  
fact6 <- fact5 %>% 
  spread(varname, vals)

rm(fact5)


dummy_fact <- fact6[FALSE,] %>% 
  mutate(
    HTS_TST_Now_R = NA,
    HTS_TST_Now_T= NA,
    HRH_STAFF_Prev_R= NA, 
    HRH_CURR_Prev_R= NA,
    SC_STOCK_hts_Now_D= NA,
    SC_STOCK_hts_Now_N= NA,
    SC_STOCK_txx_Now_D= NA,
    SC_STOCK_txx_Now_N= NA
  )

fact6x <- bind_rows(fact6, dummy_fact)


# Creating additional variables to match TSD output  ~~~~~~~~~~~~~~
fact7 <- fact6x %>% 
  mutate(datatype = "MER",
         PLHIV = NA,
         TX_CURR_NAT = NA,
         TX_CURR_SUBNAT = NA) %>%
  select(
    Region,
    RegionUID,
    OperatingUnit,
    OperatingUnitUID,
    CountryName,
    SNU1,
    typeMilitary,
    PSNU,
    PSNUuid,
    FundingAgency,
    PrimePartner,
    ImplementingMechanismName,
    MechanismID,
    PLHIV,
    TX_CURR_NAT,
    TX_CURR_SUBNAT,
    TX_CURR_Prev_R,
    TX_CURR_Now_R,
    TX_CURR_Now_T,
    HTS_TST_POS_Now_R,
    HTS_TST_POS_Now_T,
    TX_NEW_Now_R,
    TX_NEW_Now_T,
    TX_RET_Now_N,
    TX_RET_Now_D,
    F_C,
    Age,
    Sex,
    Epoch,
    datatype,
    # FY17SNUPrioritization,
    orgUnitUID,
    site_type,
    site_name,
    HTS_TST_Now_R,
    HTS_TST_Now_T,
    HRH_STAFF_Prev_R, 
    HRH_CURR_Prev_R,
    SC_STOCK_hts_Now_D,
    SC_STOCK_hts_Now_N,
    SC_STOCK_txx_Now_D,
    SC_STOCK_txx_Now_N  )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: Restructuring MER dataset  ~~~~~~~=========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Restructuring NAT_SUBNAT dat    ~~~~~~~=========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Checking NAT data structure
chex <- nat %>% 
  group_by(indicator, disaggregate) %>% 
  summarize(FY2016X = sum(FY2016, na.rm=T),
            FY2017X = sum(FY2017, na.rm=T))


nat1 <- nat %>% 
  # filter for indicators needed for TSD
  filter(indicator %in% 
         c("TX_CURR_NAT",
           "TX_CURR_SUBNAT",
           "PLHIV") &
           disaggregate %in% 
           c("Total Numerator", "Age/Sex") 
           ) %>% 
  # Keeping only the FY16APR and target data
  select(-c(FY2016)) %>% 
  # Converting to long format by period
  filter(!is.na(FY2017))


# Creating the F_C variable with No_disagg, Fine, MCAD choices
nat2 <-  nat1 %>%
  mutate(F_C =  
    case_when(disaggregate == "Age/Sex"                  ~ "M",
              disaggregate == "Total Numerator"          ~ "N"),
    Epoch = "FY17 Cum.",
    datatype = "NAT_SUBNAT",
    orgUnitUID = PSNUuid) %>% 
  mutate(xvar = indicator) %>% 
  mutate(  
Sex = 
  case_when(
    F_C=="N"                                      ~ "",
    F_C %in% c("M") &Age %in% c("<15")            ~ "Unknown Sex",
    F_C %in% c("M") & Age==""                     ~ "Unknown Sex",
    TRUE                                          ~ Sex)
)


nat3 <- nat2 %>%   
    group_by (
   Region,
   RegionUID,
   OperatingUnit,
   OperatingUnitUID,
   CountryName,
   SNU1,
   PSNU,
   PSNUuid,
   F_C,
   Age,
   Sex,
   Epoch,
   datatype,
   # FY17SNUPrioritization,
   orgUnitUID, 
   xvar) %>% 
  summarize(valuex = sum(FY2017, na.rm = T)) %>% 
  ungroup() %>% 
  spread(xvar, valuex)


tsd <- bind_rows(fact7, nat3)
  
rm(fact1, fact2, fact3, fact4, fact4x, fact5, fact6)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: Restructuring NAT_SUBNAT dat    ~~~~~~~=========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= SIMS-MER relationship variables ~~~~~~~=========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating the SIMS denominator column ~~~~~~~~~~~~~~~~
tsdxsims <- fact7 %>% ungroup() %>% 
  mutate(tx_curr_sims = 
           case_when(
             !is.na(TX_CURR_Now_R) | !is.na(TX_CURR_Now_T) ~ 1),
         tx_new_sims = 
           case_when(
             !is.na(TX_NEW_Now_R) | !is.na(TX_NEW_Now_T) ~ 1),
         hts_sims = 
           case_when(
             !is.na(HTS_TST_POS_Now_R) | !is.na(HTS_TST_POS_Now_T) |
             !is.na(HTS_TST_Now_R) | !is.na(HTS_TST_Now_T) ~ 1),
         link_sims = 
           case_when(
             !is.na(TX_NEW_Now_R) | !is.na(TX_NEW_Now_T) |
             !is.na(HTS_TST_POS_Now_R) | !is.na(HTS_TST_POS_Now_T) ~ 1),
         ret_sims = 
           case_when(
             !is.na(TX_RET_Now_N) | !is.na(TX_RET_Now_D) ~ 1))   %>%  
  mutate(F_C = "N",
       Age = "",
       Sex = "",
       Epoch = "FY18 Cum.",
       datatype = "SIMS") %>% 
  select(OperatingUnit,
         OperatingUnitUID,
         CountryName,
         SNU1,
         typeMilitary,
         PSNU,
         PSNUuid,
         FundingAgency,
         PrimePartner,
         ImplementingMechanismName,
         MechanismID,
         F_C,
         Age,
         Sex,
         Epoch,
         datatype,
         # FY17SNUPrioritization,
         orgUnitUID,
         site_type,
         site_name,
  tx_curr_sims,
  #tx_new_sims,
  link_sims,
  hts_sims,
  ret_sims
  ) %>% 
  group_by(OperatingUnit,
         OperatingUnitUID,
         CountryName,
         SNU1,
         typeMilitary,
         PSNU,
         PSNUuid,
         FundingAgency,
         PrimePartner,
         ImplementingMechanismName,
         MechanismID,
         F_C,
         Age,
         Sex,
         Epoch,
         datatype,
         # FY17SNUPrioritization,
         orgUnitUID,
         site_type,
         site_name) %>% 
  summarize(tx_curr_simsx = sum(tx_curr_sims, na.rm=T),
            #tx_new_simsx = sum(tx_new_sims, na.rm=T),
            link_simsx = sum(link_sims, na.rm=T),
            hts_simsx = sum(hts_sims, na.rm=T),
            ret_simsx = sum(ret_sims, na.rm=T)) %>% 
  ungroup() %>% 
  gather(xvarf, binary, 20:23) %>% 
  mutate(binaryx = ifelse(binary>=1, 1, NA)) %>% 
  filter(binaryx==1) %>% 
  select(-binary) %>% 
  rename(binary=binaryx)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: SIMS-MER relationship variables ~~~~~~~=========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Restructuring the SIMS dataset  ~~~~~~~=========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sims_tsd <- sims %>% 
  filter(CEE_ID %in%
           c(
# 1st SIMS block             
             "F_01_11 [011]",
# 2nd SIMS block             
             "F_01_20 [020]",
             "C_01_21 [221]",
# 3rd SIMS block             
             "F_07_04 [079]",
             "F_07_01 [076]",
             "F_07_02 [077]",
             "C_01_20 [220]",
             "C_01_23 [223]",
             "C_01_34 [234]",
# 4th SIMS block             
             "F_07_03 [078]",
             # "F_02_08 [028]",
             # "F_03_11 [028]",
             "C_01_19 [219]",
# 5th SIMS block
             "F_12_02 [109]",
# 6th SIMS block
             "F_02_03 [023]",
             "F_03_06 [023]",
# 7th SIMS block            
             "F_01_10 [010]",
# 8th SIMS block
             "F_02_04 [024]",
             "F_03_07 [024]",
             "F_04_07 [024]",
             "F_02_05 [025]",
             "F_03_08 [025]",
             "F_04_08 [025]",
# 9th SIMS block
             "F_01_16 [017]",
             "F_01_18 [018]",
             "A_10_01 [490]",
             "A_10_02 [491]",
             "A_10_03 [492]",
             "A_10_04 [493]",
# 10th SIMS block             
             "F_02_02 [022]",
             "F_03_04 [022]",
             "F_04_05 [056]",
# 11th SIMS block
             "F_02_10 [031]",
             "F_03_13 [031]",
             "F_04_09 [031]",
             "C_02_01 [243]") 
          ) %>% 
  filter(ASMT_PERIOD %in% c("FY2017Q1",
                          "FY2017Q2",
                          "FY2017Q3",
                          "FY2017Q4",
                          "FY2018Q1")) %>% 
  # Creating site_name, site_id and site_type variables  ~~~~~~~~~~~~~~~~~~~
  mutate(
    site_type = 
      case_when(
        orgUnitUID ==FacilityUID   ~ "Facility",
        orgUnitUID==CommunityUID  ~"Community",
        typeMilitary=="Y"         ~"Military",
        TRUE                      ~ "") ) %>%
  mutate(F_C = "N",
         Age = "",
         Sex = "",
         Epoch = "FY17 Cum.",
         datatype = "SIMS")


# Creating the site name variable
sims_tsd$site_name = with(sims_tsd, 
                          ifelse(site_type=="Facility", Facility,
                                 ifelse(site_type=="Community", Community,
                                        ifelse(site_type=="Military", PSNU, ""))))
sims_frame <- sims_tsd %>% 
  select(orgUnitUID,
         FundingAgency,
         MechanismID,
         OperatingUnit,
         OperatingUnitUID,
         CountryName,
         SNU1,
         typeMilitary,
         PSNU,
         PSNUuid,
         PrimePartner,
         ImplementingMechanismName,
         F_C,
         Age,
         Sex,
         Epoch,
         datatype,
         # FY17SNUPrioritization,
         site_type,
         site_name) %>% 
  mutate(matchvar <- paste(orgUnitUID,"_", MechanismID))

sims_frame1 <- unique(sims_frame)
                      

orgh <- list("orgUnitUID",
             "FundingAgency",
             "MechanismID",
             "OperatingUnit",
             "OperatingUnitUID",
             "CountryName",
             "SNU1",
             "typeMilitary",
             "PSNU",
             "PSNUuid",
             "PrimePartner",
             "ImplementingMechanismName",
             "F_C",
             "Age",
             "Sex",
             "Epoch",
             "datatype",
             # "FY17SNUPrioritization",
             "site_type",
             "site_name")

sims_tsdx <- sims_tsd %>% 
    
  select(unquote(orgh),
         CS_ASMT_TOOL_TYPE_DESCRIPTION, CS_ASMT_TYPE_DESCRIPTION,
         CS_ASMT_ID, CEE_ID,  
         ASMT_PERIOD, ASMT_CHRON_RANKING, 
         CEE_SCORE_VALUE
         )  %>% 
  arrange(orgUnitUID, FundingAgency, MechanismID,
          CS_ASMT_TOOL_TYPE_DESCRIPTION, CS_ASMT_TYPE_DESCRIPTION,
          CS_ASMT_ID, CEE_ID, ASMT_CHRON_RANKING)

 
# Create binary variables for 
#       'assessed' (denominator)
#       'met standards' (numerator)
sims_tsd2 <- sims_tsdx %>% 
  mutate(cee_assd = 
           case_when(
             CEE_SCORE_VALUE %in% c(1,2,3,4) ~ 1)
           ) %>% 
  mutate(cee_met = 
           case_when(
             CEE_SCORE_VALUE %in% c(3,4) ~ 1)
           ) %>% 
  #filter out CEEs which had a score of null or zero, i.e CEEs not assessed
  filter(cee_assd == 1)


sims_tsd3 <- unique(sims_tsd2)


# Selecting the most latest assessment irrespective of whether initial or follow-up
sims_tsd4 <-  sims_tsd3 %>% 
  select(unquote(orgh),
         CEE_ID, ASMT_CHRON_RANKING, 
         cee_assd, cee_met) %>% 
  group_by(orgUnitUID,
           FundingAgency,
           MechanismID,
           OperatingUnit,
           OperatingUnitUID,
           CountryName,
           SNU1,
           typeMilitary,
           PSNU,
           PSNUuid,
           PrimePartner,
           ImplementingMechanismName,
           F_C,
           Age,
           Sex,
           Epoch,
           datatype,
           # FY17SNUPrioritization,
           site_type,
           site_name,
           CEE_ID) %>%
  arrange(orgUnitUID,
          FundingAgency,
          MechanismID,
          OperatingUnit,
          OperatingUnitUID,
          CountryName,
          SNU1,
          typeMilitary,
          PSNU,
          PSNUuid,
          PrimePartner,
          ImplementingMechanismName,
          F_C,
          Age,
          Sex,
          Epoch,
          datatype,
          # FY17SNUPrioritization,
          site_type,
          site_name,
          CEE_ID, 
          desc(ASMT_CHRON_RANKING)) %>% 
  mutate(rxvar = row_number()) %>% 
  ungroup() %>% 
  filter(rxvar==1)



sims_tsd5 <-  sims_tsd4 %>% select(-rxvar) %>% 
  tidyr::gather(ass_met, binary, 22:23) %>% 
  mutate(xvar = paste(CEE_ID, ass_met, sep="_")) 


sims_tsd5$xvar <- gsub(" ", "_", sims_tsd5$xvar)
sims_tsd5$xvarfx <- gsub("\\[|\\]", "", sims_tsd5$xvar)


sims_tsd6 <- sims_tsd5 %>%
  select(unquote(orgh),
            xvarfx, binary) %>% 
  mutate(xvarf = 
           case_when
         (
xvarfx ==  "F_01_11_011_cee_assd"                ~ "sims1_assd",

xvarfx ==  "F_01_20_020_cee_assd"                ~ "sims2_assd",
xvarfx ==  "C_01_21_221_cee_assd"                ~ "sims2_assd",

xvarfx ==  "F_07_04_079_cee_assd"                ~ "sims3_assd",
xvarfx ==  "F_07_01_076_cee_assd"                ~ "sims3_assd",
xvarfx ==  "F_07_02_077_cee_assd"                ~ "sims3_assd",
xvarfx ==  "C_01_20_220_cee_assd"                ~ "sims3_assd",
xvarfx ==  "C_01_23_223_cee_assd"                ~ "sims3_assd",
xvarfx ==  "C_01_34_234_cee_assd"                ~ "sims3_assd",

xvarfx ==  "F_07_03_078_cee_assd"                ~ "sims4_assd",
xvarfx ==  "F_02_08_028_cee_assd"                ~ "sims4_assd",
xvarfx ==  "F_03_11_028_cee_assd"                ~ "sims4_assd",
xvarfx ==  "C_01_19_219_cee_assd"                ~ "sims4_assd",

xvarfx ==  "F_12_02_109_cee_assd"                ~ "sims5_assd",
       
xvarfx ==  "F_02_03_023_cee_assd"                ~ "sims6_assd",
xvarfx ==  "F_03_06_023_cee_assd"                ~ "sims6_assd",
       
xvarfx ==  "F_01_10_010_cee_assd"                ~ "sims7_assd",
       
xvarfx ==  "F_02_04_024_cee_assd"                ~ "sims8_assd",
xvarfx ==  "F_03_07_024_cee_assd"                ~ "sims8_assd",
xvarfx ==  "F_04_07_024_cee_assd"                ~ "sims8_assd",
xvarfx ==  "F_02_05_025_cee_assd"                ~ "sims8_assd",
xvarfx ==  "F_03_08_025_cee_assd"                ~ "sims8_assd",
xvarfx ==  "F_04_08_025_cee_assd"                ~ "sims8_assd",
       
xvarfx ==  "F_01_16_017_cee_assd"                ~ "sims9_assd",
xvarfx ==  "F_01_18_018_cee_assd"                ~ "sims9_assd",
xvarfx ==  "A_10_01_490_cee_assd"                ~ "sims9_assd",
xvarfx ==  "A_10_02_491_cee_assd"                ~ "sims9_assd",
xvarfx ==  "A_10_03_492_cee_assd"                ~ "sims9_assd",
xvarfx ==  "A_10_04_493_cee_assd"                ~ "sims9_assd",
       
xvarfx ==  "F_02_02_022_cee_assd"                ~ "sims10_assd",
xvarfx ==  "F_03_04_022_cee_assd"                ~ "sims10_assd",
xvarfx ==  "F_04_05_056_cee_assd"                ~ "sims10_assd",
       
xvarfx ==  "F_02_10_031_cee_assd"                ~ "sims11_assd",
xvarfx ==  "F_03_13_031_cee_assd"                ~ "sims11_assd",
xvarfx ==  "F_04_09_031_cee_assd"                ~ "sims11_assd",
xvarfx ==  "C_02_01_243_cee_assd"                ~ "sims11_assd",
           
# The SIMS met data           
xvarfx ==  "F_01_11_011_cee_met"                ~ "sims1_met",

xvarfx ==  "F_01_20_020_cee_met"                ~ "sims2_met",
xvarfx ==  "C_01_21_221_cee_met"                ~ "sims2_met",

xvarfx ==  "F_07_04_079_cee_met"                ~ "sims3_met",
xvarfx ==  "F_07_01_076_cee_met"                ~ "sims3_met",
xvarfx ==  "F_07_02_077_cee_met"                ~ "sims3_met",
xvarfx ==  "C_01_20_220_cee_met"                ~ "sims3_met",
xvarfx ==  "C_01_23_223_cee_met"                ~ "sims3_met",
xvarfx ==  "C_01_34_234_cee_met"                ~ "sims3_met",

xvarfx ==  "F_07_03_078_cee_met"                ~ "sims4_met",
xvarfx ==  "F_02_08_028_cee_met"                ~ "sims4_met",
xvarfx ==  "F_03_11_028_cee_met"                ~ "sims4_met",
xvarfx ==  "C_01_19_219_cee_met"                ~ "sims4_met",

xvarfx ==  "F_12_02_109_cee_met"                ~ "sims5_met",
       
xvarfx ==  "F_02_03_023_cee_met"                ~ "sims6_met",
xvarfx ==  "F_03_06_023_cee_met"                ~ "sims6_met",
       
xvarfx ==  "F_01_10_010_cee_met"                ~ "sims7_met",
       
xvarfx ==  "F_02_04_024_cee_met"                ~ "sims8_met",
xvarfx ==  "F_03_07_024_cee_met"                ~ "sims8_met",
xvarfx ==  "F_04_07_024_cee_met"                ~ "sims8_met",
xvarfx ==  "F_02_05_025_cee_met"                ~ "sims8_met",
xvarfx ==  "F_03_08_025_cee_met"                ~ "sims8_met",
xvarfx ==  "F_04_08_025_cee_met"                ~ "sims8_met",
       
xvarfx ==  "F_01_16_017_cee_met"                ~ "sims9_met",
xvarfx ==  "F_01_18_018_cee_met"                ~ "sims9_met",
xvarfx ==  "A_10_01_490_cee_met"                ~ "sims9_met",
xvarfx ==  "A_10_02_491_cee_met"                ~ "sims9_met",
xvarfx ==  "A_10_03_492_cee_met"                ~ "sims9_met",
xvarfx ==  "A_10_04_493_cee_met"                ~ "sims9_met",
       
xvarfx ==  "F_02_02_022_cee_met"                ~ "sims10_met",
xvarfx ==  "F_03_04_022_cee_met"                ~ "sims10_met",
xvarfx ==  "F_04_05_056_cee_met"                ~ "sims10_met",
       
xvarfx ==  "F_02_10_031_cee_met"                ~ "sims11_met",
xvarfx ==  "F_03_13_031_cee_met"                ~ "sims11_met",
xvarfx ==  "F_04_09_031_cee_met"                ~ "sims11_met",
xvarfx ==  "C_02_01_243_cee_met"                ~ "sims11_met"
         )) 

sims_tsd6x <- sims_tsd6 %>% 
  group_by (
            orgUnitUID,
            FundingAgency,
            MechanismID,
            OperatingUnit,
            OperatingUnitUID,
            CountryName,
            SNU1,
            typeMilitary,
            PSNU,
            PSNUuid,
            PrimePartner,
            ImplementingMechanismName,
            F_C,
            Age,
            Sex,
            Epoch,
            datatype,
            # FY17SNUPrioritization,
            site_type,
            site_name,
            xvarf) %>% 
  summarize(binary=sum(binary, na.rm=T)) %>% ungroup()   %>% 
  mutate(binaryx = ifelse(binary>=1, 1, NA)) %>% 
  filter(binaryx==1) %>% 
  select(-binary) %>% 
  rename(binary=binaryx)


sims_tsd7 <- sims_tsd5 %>%
  select(unquote(orgh),
            xvarfx, binary) %>% 
  rename(xvarf = xvarfx) %>% 
  group_by (
            orgUnitUID,
            FundingAgency,
            MechanismID,
            OperatingUnit,
            OperatingUnitUID,
            CountryName,
            SNU1,
            typeMilitary,
            PSNU,
            PSNUuid,
            PrimePartner,
            ImplementingMechanismName,
            F_C,
            Age,
            Sex,
            Epoch,
            datatype,
            # FY17SNUPrioritization,
            site_type,
            site_name,
            xvarf) %>% 
  summarize(binary=sum(binary, na.rm=T)) %>% ungroup()   %>% 
  mutate(binaryx = ifelse(binary>=1, 1, NA)) %>% 
  filter(binaryx==1) %>% 
  select(-binary) %>% 
  rename(binary=binaryx)


# Stack on/append the MER-SIMS relational variables
sims_nofdup <- bind_rows(tsdxsims, sims_tsd6x, sims_tsd7)


fdups1 <- sims_nofdup %>% 
  group_by(
           orgUnitUID,
           FundingAgency,
           OperatingUnit,
           OperatingUnitUID,
           CountryName,
           SNU1,
           typeMilitary,
           PSNU,
           PSNUuid,
           F_C,
           Age,
           Sex,
           Epoch,
           datatype,
           # FY17SNUPrioritization,
           site_type,
           site_name,
           xvarf) %>% 
  summarize(binaryx=(sum(binary, na.rm=T) - 1)*-1) %>% 
  filter(!is.na(binaryx) & binaryx != 0) %>% 
  rename(binary = binaryx)

fdups_site <- sims_nofdup %>% 
  group_by(
           orgUnitUID,
           OperatingUnit,
           OperatingUnitUID,
           CountryName,
           SNU1,
           typeMilitary,
           PSNU,
           PSNUuid,
           F_C,
           Age,
           Sex,
           Epoch,
           datatype,
           # FY17SNUPrioritization,
           site_type,
           site_name,
           xvarf) %>% 
  summarize(binaryx=(n_distinct(FundingAgency)- 1)*-1) %>% 
  filter(!is.na(binaryx) & binaryx != 0) %>% 
  rename(binary = binaryx)


all_fdups <- bind_rows(fdups1, fdups_site) %>% 
  mutate(fdups = "Y")

sims_nofdup$fdups <- "N" 

# stack 'em up all
sims_fdup <- bind_rows(sims_nofdup, all_fdups) %>% 
  select(OperatingUnit:site_name, fdups,
         xvarf, binary) %>% 
  tidyr::spread(xvarf, binary) %>% 
  arrange(orgUnitUID, MechanismID)


# Creating dummy dataset with all possible SIMS variables
dummy <- sims_fdup[FALSE,]

dummy1 <- dummy %>% 
  mutate(
OperatingUnit          = "dummy",

hts_simsx              = NA,       

sims1_assd             = NA,     
sims1_met              = NA, 
F_01_11_011_cee_assd   = NA,
F_01_11_011_cee_met    = NA,

sims2_assd             = NA,   
sims2_met              = NA, 
F_01_20_020_cee_assd   = NA,
F_01_20_020_cee_met    = NA,
C_01_21_221_cee_assd   = NA,
C_01_21_221_cee_met    = NA,

sims3_assd             = NA,  
sims3_met              = NA,  
F_07_04_079_cee_assd   = NA,
F_07_04_079_cee_met    = NA,
F_07_01_076_cee_assd   = NA,
F_07_01_076_cee_met    = NA,
F_07_02_077_cee_assd   = NA,
F_07_02_077_cee_met    = NA,
C_01_20_220_cee_assd   = NA,
C_01_20_220_cee_met    = NA,
C_01_23_223_cee_assd   = NA,
C_01_23_223_cee_met    = NA,
C_01_34_234_cee_assd   = NA,
C_01_34_234_cee_met    = NA,


   
link_simsx             = NA,   

sims4_assd             = NA,     
sims4_met              = NA,  
F_07_03_078_cee_assd   = NA,
F_07_03_078_cee_met    = NA,
F_02_08_028_cee_assd   = NA,
F_02_08_028_cee_met    = NA,
F_03_11_028_cee_assd   = NA,
F_03_11_028_cee_met    = NA,
C_01_19_219_cee_assd   = NA,
C_01_19_219_cee_met    = NA,
  

 
tx_curr_simsx          = NA, 

sims5_assd             = NA,     
sims5_met              = NA, 
F_12_02_109_cee_assd   = NA,
F_12_02_109_cee_met    = NA,

sims6_assd             = NA,   
sims6_met              = NA, 
F_02_03_023_cee_assd   = NA,      
F_02_03_023_cee_met    = NA,    
F_03_06_023_cee_assd   = NA,     
F_03_06_023_cee_met    = NA,    

sims7_assd             = NA,  
sims7_met              = NA,
F_01_10_010_cee_assd   = NA,
F_01_10_010_cee_met    = NA,

sims8_assd             = NA,  
sims8_met              = NA, 
F_02_04_024_cee_assd   = NA,   
F_02_04_024_cee_met    = NA,      
F_03_07_024_cee_assd   = NA,   
F_03_07_024_cee_met    = NA,      
F_04_07_024_cee_assd   = NA,   
F_04_07_024_cee_met    = NA,      
F_02_05_025_cee_assd   = NA,   
F_02_05_025_cee_met    = NA,      
F_03_08_025_cee_assd   = NA,   
F_03_08_025_cee_met    = NA,      
F_04_08_025_cee_assd   = NA,   
F_04_08_025_cee_met    = NA,      

sims9_assd             = NA,  
sims9_met              = NA, 
F_01_16_017_cee_assd   = NA,   
F_01_16_017_cee_met    = NA,
F_01_18_018_cee_assd   = NA,  
F_01_18_018_cee_met    = NA,
A_10_01_490_cee_assd   = NA,  
A_10_01_490_cee_met    = NA,
A_10_02_491_cee_assd   = NA,  
A_10_02_491_cee_met    = NA,
A_10_03_492_cee_assd   = NA,  
A_10_03_492_cee_met    = NA,
A_10_04_493_cee_assd   = NA,  
A_10_04_493_cee_met    = NA,



ret_simsx              = NA,     

sims10_assd            = NA,  
sims10_met             = NA,  
F_02_02_022_cee_assd   = NA,  
F_02_02_022_cee_met    = NA,     
F_03_04_022_cee_assd   = NA,  
F_03_04_022_cee_met    = NA,     
F_04_05_056_cee_assd   = NA,  
F_04_05_056_cee_met    = NA,     

sims11_assd             = NA,  
sims11_met              = NA,
F_02_10_031_cee_assd    = NA,
F_02_10_031_cee_met     = NA,       
F_03_13_031_cee_assd    = NA,
F_03_13_031_cee_met     = NA,       
F_04_09_031_cee_assd    = NA,
F_04_09_031_cee_met     = NA,       
C_02_01_243_cee_assd    = NA,
C_02_01_243_cee_met     = NA       

)     


sims_allvars <- bind_rows(sims_fdup, dummy1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: Restructuring the SIMS dataset  ~~~~~~~=========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ====== Adding MER, NAT-SUBNAT, and SIMS dataset  ~~~~=========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Stack below the FactView dataset
fact_nat_sims <- bind_rows(tsd, sims_allvars) %>% 
  select(OperatingUnit,
         OperatingUnitUID,
         CountryName,
         SNU1,
         typeMilitary,
         PSNU,
         PSNUuid,
         FundingAgency,
         PrimePartner,
         ImplementingMechanismName,
         MechanismID,
         TX_CURR_NAT,
         TX_CURR_SUBNAT,
         PLHIV,
         TX_CURR_Prev_R,
         TX_CURR_Now_R,
         TX_CURR_Now_T,
         HTS_TST_POS_Now_R,
         HTS_TST_POS_Now_T,
         TX_NEW_Now_R,
         TX_NEW_Now_T,
         TX_RET_Now_N,
         TX_RET_Now_D,
         F_C,
         Age,
         Sex,
         Epoch,
         datatype,
         # FY17SNUPrioritization,
         site_type,
         site_name,
         orgUnitUID,
hts_simsx            ,
sims1_assd           ,
sims1_met            ,
F_01_11_011_cee_assd ,
F_01_11_011_cee_met  ,
sims2_assd           ,
sims2_met            ,
F_01_20_020_cee_assd ,
F_01_20_020_cee_met  ,
C_01_21_221_cee_assd ,
C_01_21_221_cee_met  ,
sims3_assd           ,
sims3_met            ,
F_07_04_079_cee_assd ,
F_07_04_079_cee_met  ,
F_07_01_076_cee_assd ,
F_07_01_076_cee_met  ,
F_07_02_077_cee_assd ,
F_07_02_077_cee_met  ,
C_01_20_220_cee_assd ,
C_01_20_220_cee_met  ,
C_01_23_223_cee_assd ,
C_01_23_223_cee_met  ,
C_01_34_234_cee_assd ,
C_01_34_234_cee_met  ,

link_simsx           ,
sims4_assd           ,
sims4_met            ,
F_07_03_078_cee_assd ,
F_07_03_078_cee_met  ,
F_02_08_028_cee_assd ,
F_02_08_028_cee_met  ,
F_03_11_028_cee_assd ,
F_03_11_028_cee_met  ,
C_01_19_219_cee_assd ,
C_01_19_219_cee_met  ,

tx_curr_simsx        ,
sims5_assd           ,
sims5_met            ,
F_12_02_109_cee_assd ,
F_12_02_109_cee_met  ,
sims6_assd           ,
sims6_met            ,
F_02_03_023_cee_assd ,
F_02_03_023_cee_met  ,
F_03_06_023_cee_assd ,
F_03_06_023_cee_met  ,
sims7_assd           ,
sims7_met            ,
F_01_10_010_cee_assd ,
F_01_10_010_cee_met  ,
sims8_assd           ,
sims8_met            ,
F_02_04_024_cee_assd ,
F_02_04_024_cee_met  ,
F_03_07_024_cee_assd ,
F_03_07_024_cee_met  ,
F_04_07_024_cee_assd ,
F_04_07_024_cee_met  ,
F_02_05_025_cee_assd ,
F_02_05_025_cee_met  ,
F_03_08_025_cee_assd ,
F_03_08_025_cee_met  ,
F_04_08_025_cee_assd ,
F_04_08_025_cee_met  ,
sims9_assd           ,
sims9_met            ,
F_01_16_017_cee_assd ,
F_01_16_017_cee_met  ,
F_01_18_018_cee_assd ,
F_01_18_018_cee_met  ,
A_10_01_490_cee_assd ,
A_10_01_490_cee_met  ,
A_10_02_491_cee_assd ,
A_10_02_491_cee_met  ,
A_10_03_492_cee_assd ,
A_10_03_492_cee_met  ,
A_10_04_493_cee_assd ,
A_10_04_493_cee_met  ,

ret_simsx            ,
sims10_assd          ,
sims10_met           ,
F_02_02_022_cee_assd ,
F_02_02_022_cee_met  ,
F_03_04_022_cee_assd ,
F_03_04_022_cee_met  ,
F_04_05_056_cee_assd ,
F_04_05_056_cee_met  ,
sims11_assd          ,
sims11_met           ,
F_02_10_031_cee_assd ,
F_02_10_031_cee_met  ,
F_03_13_031_cee_assd ,
F_03_13_031_cee_met  ,
F_04_09_031_cee_assd ,
F_04_09_031_cee_met  ,
C_02_01_243_cee_assd ,
C_02_01_243_cee_met,
    HTS_TST_Now_R,
    HTS_TST_Now_T,
    HRH_STAFF_Prev_R, 
    HRH_CURR_Prev_R,
SC_STOCK_hts_Now_D,
SC_STOCK_hts_Now_N,
SC_STOCK_txx_Now_D,
SC_STOCK_txx_Now_N,
fdups
)


# final step for variable names
fv_nat_sims <- fact_nat_sims %>% 
  mutate(F_C = 
           case_when(
             F_C == "N"   ~ "No disaggregation",
             F_C == "F"   ~ "Fine",
             F_C == "M"   ~ "MCAD/Coarse"
           ))



# write_tsv(fv_nat_sims, path = "2017_12_22_TSD_SIMS.txt", na="")
# 
# write_csv(fv_nat_sims, path = paste(out_put, "2018_01_07_TSD_SIMS (test).csv", sep=""), 
#           na="")
# 
# 
# simsonly <- fv_nat_sim %>% filter(datatype=="SIMS")
# write_tsv(simsonly, path = "2017_12_01_SIMS_only.txt", na="")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ====== END: Adding MER, NAT-SUBNAT, and SIMS dataset  ~~~~=========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#============================================================
# Modifying the EA dataset =====
#============================================================

`%ni%` <- Negate(`%in%`) 

ea1 <- ea %>% 
  filter(variable %in% c(
    "ART_UE",
    "HTC_UMB_TST_UE",
    "CBCTS_LOADED_TOT",     # EA 3
    "CBCTS_LNKG_EXP",       # EA 3
    "CBCTS_RTNADHR_EXP",    # EA 4
    "CBCTS_OTHCARE_EXP",
    "CBCTS_NONFBT_EXP")     # EA 5
  & ea_snu_level1 %ni% c("National", "Above National")
) %>% 
  gather(period, value, 19:21) %>% 
  # create column headers for the EA columns
  mutate(colvar = paste(variable, period, sep="_")) %>% 
  filter(colvar %in% c(
    "ART_UE_fy15",
    "ART_UE_fy16",
    "ART_UE_fy17",
    "HTC_UMB_TST_UE_fy17",
    "CBCTS_LOADED_TOT_fy17",
    "CBCTS_LNKG_EXP_fy17",
    "CBCTS_RTNADHR_EXP_fy17",
    "CBCTS_MEDCARE_EXP_fy17"  ) & !is.na(value)) %>% 
  select(
    OU             ,
    country        ,
    mech_agency    ,
    mech_partner  ,
    mech_name      ,
    mechanismid    ,
    mech_promis_id ,
    ea_snu_level1 ,
    datim_snu      ,
    colvar,
    value) 


# Creating dummy dataset to include all columns
dummy_ea<- ea1[FALSE,]

dummy_ea1 <- dummy_ea %>% 
  mutate(
    OperatingUnit = "dummy",
            ART_UE_fy15_imp = NA,             ART_UE_fy15_dup = NA,              ART_UE_fy15_nat = NA,  
            ART_UE_fy16_imp = NA,             ART_UE_fy16_dup = NA,              ART_UE_fy16_nat = NA,
            ART_UE_fy17_imp = NA,             ART_UE_fy17_dup = NA,              ART_UE_fy17_nat = NA,
    HTC_UMB_TST_UE_fy17_imp = NA,     HTC_UMB_TST_UE_fy17_dup = NA,      HTC_UMB_TST_UE_fy17_nat = NA,
  CBCTS_LOADED_TOT_fy17_imp = NA,   CBCTS_LOADED_TOT_fy17_dup = NA,    CBCTS_LOADED_TOT_fy17_nat = NA,
    CBCTS_LNKG_EXP_fy17_imp = NA,     CBCTS_LNKG_EXP_fy17_dup = NA,      CBCTS_LNKG_EXP_fy17_nat = NA,
 CBCTS_RTNADHR_EXP_fy17_imp = NA,  CBCTS_RTNADHR_EXP_fy17_dup = NA,   CBCTS_RTNADHR_EXP_fy17_nat = NA,
 CBCTS_MEDCARE_EXP_fy17_imp = NA,  CBCTS_MEDCARE_EXP_fy17_dup = NA,   CBCTS_MEDCARE_EXP_fy17_nat = NA
 ) %>% 
select(     ART_UE_fy15_imp,
            ART_UE_fy16_imp,
            ART_UE_fy17_imp,
    HTC_UMB_TST_UE_fy17_imp,
  CBCTS_LOADED_TOT_fy17_imp,
    CBCTS_LNKG_EXP_fy17_imp,
 CBCTS_RTNADHR_EXP_fy17_imp,
 CBCTS_MEDCARE_EXP_fy17_imp,
            ART_UE_fy15_dup,
            ART_UE_fy16_dup,
            ART_UE_fy17_dup,
    HTC_UMB_TST_UE_fy17_dup,
  CBCTS_LOADED_TOT_fy17_dup,
    CBCTS_LNKG_EXP_fy17_dup,
 CBCTS_RTNADHR_EXP_fy17_dup,
 CBCTS_MEDCARE_EXP_fy17_dup,
            ART_UE_fy15_nat,
            ART_UE_fy16_nat,
            ART_UE_fy17_nat,
    HTC_UMB_TST_UE_fy17_nat,
  CBCTS_LOADED_TOT_fy17_nat,
    CBCTS_LNKG_EXP_fy17_nat,
 CBCTS_RTNADHR_EXP_fy17_nat,
 CBCTS_MEDCARE_EXP_fy17_nat
      )


# Checking for unique rows of data
chex <- ea1 %>% 
  select(
    OU             ,
    country        ,
    mech_agency    ,
    mech_partner  ,
    mech_name      ,
    mechanismid    ,
    mech_promis_id ,
    ea_snu_level1 ,
    # ea_snu_level2  ,
    datim_snu      ,
    colvar
  )

nrow(chex)
nrow(unique(chex))
nrow(ea1)


# Making the dataset wide 
ea2 <- ea1 %>%
  # Creating variable for Deduped (geo data) vs non-deduped data
  mutate(dedup = ifelse(
    mechanismid=="0" & ea_snu_level1==OU, "nat",
    ifelse(mechanismid=="0" & ea_snu_level1!=OU, "dup","imp"))) %>% 
  mutate(colvarx = paste(colvar, dedup, sep="_")) %>% 
  select(-c(colvar, dedup, mech_promis_id)) %>% 
  spread(colvarx, value) %>% 
  filter(ea_snu_level1==OU | !is.na(datim_snu)) %>% 
  mutate(mech_agency = 
           case_when (
             mech_agency=="CDC"           ~  "HHS/CDC",
             mech_agency=="DoD"           ~  "DOD",
             mech_agency=="Peace Corps"   ~  "PC",
             mech_agency=="State_AF"      ~  "State/AF",
             mech_agency=="State_PRM"     ~  "State/PRM",
             mech_agency=="USAID"         ~  "USAID"
             )) %>% 
  mutate(OU = case_when(
    OU == "CotedIvoire"             ~ "Cote d'Ivoire",
    OU == "DRC"                     ~ "Democratic Republic of the Congo",
    OU == "SouthAfrica"             ~ "South Africa",
    OU == "SouthSudan"              ~ "South Sudan",
    TRUE                            ~ OU
  )) %>% 
  # Renaming columns and removing some of the columns
  rename(
    OperatingUnit = OU,
    CountryName = country,
    FundingAgency = mech_agency,
    PrimePartner = mech_partner,
    ImplementingMechanismName = mech_name,
    MechanismID = mechanismid,
    SNU1 = ea_snu_level1,
    # PSNU = ea_snu_level2,
    PSNUuid = datim_snu) %>% 
  mutate(
         F_C = "No disaggregation",
         Age = "",
         Sex = "",
         Epoch = "FY17 Cum.",
         datatype = "EA",
         site_type = "EA_type",
         site_name = "_EA_data")
  

EA_final <- bind_rows(dummy_ea1, ea2)   

# Adding SNUprioritization for most recent period 
 ea_psnu <- fact %>% 
  select(PSNUuid, PSNU) %>%
  unique() %>% 
  filter(!is.na(PSNUuid))
 
 EA_finalx <- left_join(EA_final, ea_psnu)



#============================================================
# END: Modifying the EA dataset =====
#============================================================


#============================================================
##### FINAL data additions =====
#============================================================


final_sanspr <- bind_rows (fv_nat_sims, EA_finalx) 
  

# Adding SNUprioritization for most recent period 
 geo_frame <- fact %>% 
  select(PSNUuid, CurrentSNUPrioritization, FY2018Q1) %>%
  group_by(PSNUuid, CurrentSNUPrioritization) %>%
  summarize(FY2018Q1x  = sum(FY2018Q1, na.rm=T)) %>% 
  arrange(PSNUuid, CurrentSNUPrioritization, desc(FY2018Q1x)) %>% 
  mutate(ordernum = row_number()) %>% 
  filter(ordernum == 1) %>% 
  select(PSNUuid, CurrentSNUPrioritization) %>% 
  filter(!is.na(PSNUuid))


# Adding SNU prioritization 
fv_nat_sims_ea <- left_join(final_sanspr, geo_frame)


fv_nat_sims_eax <- fv_nat_sims_ea %>% 
  select(OperatingUnit:datatype, CurrentSNUPrioritization,
         site_type:C_02_01_243_cee_met,
         ART_UE_fy15_imp           ,
         ART_UE_fy16_imp           ,
         ART_UE_fy17_imp           ,
         HTC_UMB_TST_UE_fy17_imp   ,
         CBCTS_LOADED_TOT_fy17_imp ,
         CBCTS_LNKG_EXP_fy17_imp   ,
         CBCTS_RTNADHR_EXP_fy17_imp,
         CBCTS_MEDCARE_EXP_fy17_imp,
         ART_UE_fy15_dup           ,
         ART_UE_fy16_dup           ,
         ART_UE_fy17_dup           ,
         HTC_UMB_TST_UE_fy17_dup   ,
         CBCTS_LOADED_TOT_fy17_dup ,
         CBCTS_LNKG_EXP_fy17_dup   ,
         CBCTS_RTNADHR_EXP_fy17_dup,
         CBCTS_MEDCARE_EXP_fy17_dup,
         ART_UE_fy15_nat           ,
         ART_UE_fy16_nat           ,
         ART_UE_fy17_nat           ,
         HTC_UMB_TST_UE_fy17_nat   ,
         CBCTS_LOADED_TOT_fy17_nat ,
         CBCTS_LNKG_EXP_fy17_nat   ,
         CBCTS_RTNADHR_EXP_fy17_nat,
         CBCTS_MEDCARE_EXP_fy17_nat,
         HTS_TST_Now_R    ,         
         HTS_TST_Now_T    ,         
         HRH_STAFF_Prev_R  ,         
         HRH_CURR_Prev_R   ,         
          SC_STOCK_hts_Now_D,
          SC_STOCK_hts_Now_N,
          SC_STOCK_txx_Now_D,
          SC_STOCK_txx_Now_N,
          fdups
          )



### Output the final dataset =====
write_tsv(fv_nat_sims_eax, path = paste(out_put, 
                                        "2018_04_18_TSD_", oux, "_v1.txt", 
                                        sep=""), na="")





