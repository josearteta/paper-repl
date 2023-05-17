# Replication project 
# Zé and Igor


# Libraries
library(tidyverse)
library(haven)
library(estimatr)

# Reading the DATA
aej_maindata <- read_dta("aej_maindata.dta")
table_5 <- read_dta("table_5.dta")
table_A1 <- read_dta("table_A1.dta")


# Table 1
tab1_reg1 <- lm_robust(dism1990 ~ herf + lenper,se_type = "stata", data = aej_maindata)
tab1_reg2 <- lm_robust(area1910 ~ herf + lenper,se_type = "stata", data = aej_maindata)
tab1_reg3 <- lm_robust(count1910 ~ herf + lenper,se_type = "stata", data = aej_maindata)
tab1_reg4 <- lm_robust(ethseg10 ~ herf + lenper,se_type = "stata", data = aej_maindata)
tab1_reg5 <- lm_robust(ethiso10 ~ herf + lenper,se_type = "stata", data = aej_maindata)
tab1_reg6 <- lm_robust(black1910 ~ herf + lenper,se_type = "stata", data = aej_maindata)
tab1_reg7 <- lm_robust(passpc ~ herf + lenper,se_type = "stata", data = aej_maindata)
tab1_reg8 <- lm_robust(black1920 ~ herf + lenper,se_type = "stata", data = aej_maindata)
tab1_reg9 <- lm_robust(ctyliterate1920 ~ herf + lenper,se_type = "stata", data = aej_maindata)
tab1_reg10 <- lm_robust(lfp1920 ~ herf + lenper,se_type = "stata", data = aej_maindata)
tab1_reg11 <- lm_robust(ctytrade_wkrs1920 ~ herf + lenper,se_type = "stata", data = aej_maindata)
tab1_reg12 <- lm_robust(ctymanuf_wkrs1920 ~ herf + lenper,se_type = "stata", data = aej_maindata)
tab1_reg13 <- lm_robust(ctyrail_wkrs1920 ~ herf + lenper,se_type = "stata", data = aej_maindata)
tab1_reg14 <- lm_robust(incseg ~ herf + lenper,se_type = "stata", data = aej_maindata)


# Table 2 panel 1
tab2_p1_reg1 <- lm_robust(lngini_w ~ dism1990,se_type = "stata",data = aej_maindata)
tab2_p1_reg2 <- lm_robust(lngini_b ~ dism1990,se_type = "stata",data = aej_maindata)
tab2_p1_reg3 <- lm_robust(povrate_w ~ dism1990,se_type = "stata",data = aej_maindata)
tab2_p1_reg4 <- lm_robust(povrate_b ~ dism1990,se_type = "stata",data = aej_maindata)

tab2_p1_ivreg1 <- iv_robust(lngini_w ~ dism1990 + lenper | herf + lenper, se_type = "stata",data = aej_maindata)
tab2_p1_ivreg2 <- iv_robust(lngini_b ~ dism1990 + lenper | herf + lenper, se_type = "stata",data = aej_maindata)
tab2_p1_ivreg3 <- iv_robust(povrate_w ~ dism1990 + lenper | herf + lenper, se_type = "stata",data = aej_maindata)
tab2_p1_ivreg4 <- iv_robust(povrate_b ~ dism1990 + lenper | herf + lenper, se_type = "stata",data = aej_maindata)

# Closeness < -400 --------------------------------------------------------------------------------------------------
aej_maindata_tab2 <- aej_maindata %>%
  filter(closeness < -400)
#--------------------------------------------------------------------------------------------------------------------
tab2_p1_reg5 <- lm_robust(lngini_w ~ herf + lenper,se_type = "stata",data = aej_maindata_tab2)
tab2_p1_reg6 <- lm_robust(lngini_b ~ herf + lenper,se_type = "stata",data = aej_maindata_tab2)
tab2_p1_reg7 <- lm_robust(povrate_w ~ herf + lenper,se_type = "stata",data = aej_maindata_tab2)
tab2_p1_reg8 <- lm_robust(povrate_b ~ herf + lenper,se_type = "stata",data = aej_maindata_tab2)


# Table 2 panel 2

tab2_p2_reg1 <- lm_robust(ln90w90b ~ dism1990,se_type = "stata",data = aej_maindata)
tab2_p2_reg2 <- lm_robust(ln10w10b ~ dism1990,se_type = "stata",data = aej_maindata)
tab2_p2_reg3 <- lm_robust(ln90w10b ~ dism1990,se_type = "stata",data = aej_maindata)
tab2_p2_reg4 <- lm_robust(ln90b10w ~ dism1990,se_type = "stata",data = aej_maindata)

tab2_p2_ivreg1 <- iv_robust(ln90w90b ~ dism1990 + lenper | herf + lenper, se_type = "stata",data = aej_maindata)
tab2_p2_ivreg2 <- iv_robust(ln10w10b ~ dism1990 + lenper | herf + lenper, se_type = "stata",data = aej_maindata)
tab2_p2_ivreg3 <- iv_robust(ln90w10b ~ dism1990 + lenper | herf + lenper, se_type = "stata",data = aej_maindata)
tab2_p2_ivreg4 <- iv_robust(ln90b10w ~ dism1990 + lenper | herf + lenper, se_type = "stata",data = aej_maindata)

tab2_p2_reg5 <- lm_robust(ln90w90b ~ herf + lenper, se_type = "stata",data = aej_maindata_tab2)
tab2_p2_reg6 <- lm_robust(ln10w10b ~ herf + lenper, se_type = "stata",data = aej_maindata_tab2)
tab2_p2_reg7 <- lm_robust(ln90w10b ~ herf + lenper, se_type = "stata",data = aej_maindata_tab2)
tab2_p2_reg8 <- lm_robust(ln90b10w ~ herf + lenper, se_type = "stata",data = aej_maindata_tab2)


# Table 3 

tab3_ivreg1 <- iv_robust(lngini_w ~ dism1990 + lenper + pop1990 |herf + lenper + pop1990 ,se_type = "stata",data = aej_maindata)
tab3_ivreg2 <- iv_robust(lngini_b ~ dism1990 + lenper + pop1990 |herf + lenper + pop1990 ,se_type = "stata",data = aej_maindata)
tab3_ivreg3 <- iv_robust(povrate_w ~ dism1990 + lenper + pop1990 |herf + lenper + pop1990 ,se_type = "stata",data = aej_maindata)
tab3_ivreg4 <- iv_robust(povrate_b ~ dism1990 + lenper + pop1990 |herf + lenper + pop1990 ,se_type = "stata",data = aej_maindata)

tab3_ivreg5 <- iv_robust(lngini_w ~ dism1990 + lenper + pctbk1990 |herf + lenper + pctbk1990 ,se_type = "stata",data = aej_maindata)
tab3_ivreg6 <- iv_robust(lngini_b ~ dism1990 + lenper + pctbk1990 |herf + lenper + pctbk1990 ,se_type = "stata",data = aej_maindata)
tab3_ivreg7 <- iv_robust(povrate_w ~ dism1990 + lenper + pctbk1990 |herf + lenper + pctbk1990 ,se_type = "stata",data = aej_maindata)
tab3_ivreg8 <- iv_robust(povrate_b ~ dism1990 + lenper + pctbk1990 |herf + lenper + pctbk1990 ,se_type = "stata",data = aej_maindata)

tab3_ivreg9 <- iv_robust(lngini_w ~ dism1990 + lenper + hsdrop_w + hsgrad_w + somecoll_w + collgrad_w + hsdrop_b + hsgrad_b + somecoll_b + collgrad_b |
                           herf + lenper + hsdrop_w + hsgrad_w + somecoll_w + collgrad_w + hsdrop_b + hsgrad_b + somecoll_b + collgrad_b ,
                         se_type = "stata",data = aej_maindata)

tab3_ivreg10 <- iv_robust(lngini_b ~ dism1990 + lenper + hsdrop_w + hsgrad_w + somecoll_w + collgrad_w + hsdrop_b + hsgrad_b + somecoll_b + collgrad_b |
                           herf + lenper + hsdrop_w + hsgrad_w + somecoll_w + collgrad_w + hsdrop_b + hsgrad_b + somecoll_b + collgrad_b ,
                         se_type = "stata",data = aej_maindata)

tab3_ivreg11 <- iv_robust(povrate_w ~ dism1990 + lenper + hsdrop_w + hsgrad_w + somecoll_w + collgrad_w + hsdrop_b + hsgrad_b + somecoll_b + collgrad_b |
                           herf + lenper + hsdrop_w + hsgrad_w + somecoll_w + collgrad_w + hsdrop_b + hsgrad_b + somecoll_b + collgrad_b ,
                         se_type = "stata",data = aej_maindata)

tab3_ivreg12 <- iv_robust(povrate_b ~ dism1990 + lenper + hsdrop_w + hsgrad_w + somecoll_w + collgrad_w + hsdrop_b + hsgrad_b + somecoll_b + collgrad_b |
                           herf + lenper + hsdrop_w + hsgrad_w + somecoll_w + collgrad_w + hsdrop_b + hsgrad_b + somecoll_b + collgrad_b ,
                         se_type = "stata",data = aej_maindata)

tab3_ivreg13 <- iv_robust(lngini_w ~ dism1990 + lenper + manshr |herf + lenper + manshr ,se_type = "stata",data = aej_maindata)
tab3_ivreg14 <- iv_robust(lngini_b ~ dism1990 + lenper + manshr |herf + lenper + manshr ,se_type = "stata",data = aej_maindata)
tab3_ivreg15 <- iv_robust(povrate_w ~ dism1990 + lenper + manshr |herf + lenper + manshr ,se_type = "stata",data = aej_maindata)
tab3_ivreg16 <- iv_robust(povrate_b ~ dism1990 + lenper + manshr |herf + lenper + manshr ,se_type = "stata",data = aej_maindata)

tab3_ivreg17 <- iv_robust(lngini_w ~ dism1990 + lenper + lfp_w + lfp_b |herf + lenper + lfp_w + lfp_b ,se_type = "stata",data = aej_maindata)
tab3_ivreg18 <- iv_robust(lngini_b ~ dism1990 + lenper + lfp_w + lfp_b |herf + lenper + lfp_w + lfp_b ,se_type = "stata",data = aej_maindata)
tab3_ivreg19 <- iv_robust(povrate_w ~ dism1990 + lenper + lfp_w + lfp_b |herf + lenper + lfp_w + lfp_b ,se_type = "stata",data = aej_maindata)
tab3_ivreg20 <- iv_robust(povrate_b ~ dism1990 + lenper + lfp_w + lfp_b |herf + lenper + lfp_w + lfp_b ,se_type = "stata",data = aej_maindata)


tab3_ivreg21 <- iv_robust(lngini_w ~ dism1990 + lenper + ngov |herf + lenper + ngov ,se_type = "stata",data = aej_maindata)
tab3_ivreg22 <- iv_robust(lngini_b ~ dism1990 + lenper + ngov |herf + lenper + ngov ,se_type = "stata",data = aej_maindata)
tab3_ivreg23 <- iv_robust(povrate_w ~ dism1990 + lenper + ngov |herf + lenper + ngov ,se_type = "stata",data = aej_maindata)
tab3_ivreg24 <- iv_robust(povrate_b ~ dism1990 + lenper + ngov |herf + lenper + ngov ,se_type = "stata",data = aej_maindata)


tab3_ivreg25 <- iv_robust(lngini_w ~ dism1990 + lenper + count1920 |herf + lenper + count1920 ,se_type = "stata",data = aej_maindata)
tab3_ivreg26 <- iv_robust(lngini_b ~ dism1990 + lenper + count1920 |herf + lenper + count1920 ,se_type = "stata",data = aej_maindata)
tab3_ivreg27 <- iv_robust(povrate_w ~ dism1990 + lenper + count1920 |herf + lenper + count1920 ,se_type = "stata",data = aej_maindata)
tab3_ivreg28 <- iv_robust(povrate_b ~ dism1990 + lenper + count1920 |herf + lenper + count1920 ,se_type = "stata",data = aej_maindata)


tab3_ivreg29 <- iv_robust(lngini_w ~ dism1990 + lenper + black1920 |herf + lenper + black1920 ,se_type = "stata",data = aej_maindata)
tab3_ivreg30 <- iv_robust(lngini_b ~ dism1990 + lenper + black1920 |herf + lenper + black1920 ,se_type = "stata",data = aej_maindata)
tab3_ivreg31 <- iv_robust(povrate_w ~ dism1990 + lenper + black1920 |herf + lenper + black1920 ,se_type = "stata",data = aej_maindata)
tab3_ivreg32 <- iv_robust(povrate_b ~ dism1990 + lenper + black1920 |herf + lenper + black1920 ,se_type = "stata",data = aej_maindata)


tab3_ivreg33 <- iv_robust(lngini_w ~ dism1990 + lenper + ctyliterate1920 |herf + lenper + ctyliterate1920 ,se_type = "stata",data = aej_maindata)
tab3_ivreg34 <- iv_robust(lngini_b ~ dism1990 + lenper + ctyliterate1920 |herf + lenper + ctyliterate1920 ,se_type = "stata",data = aej_maindata)
tab3_ivreg35 <- iv_robust(povrate_w ~ dism1990 + lenper + ctyliterate1920 |herf + lenper + ctyliterate1920 ,se_type = "stata",data = aej_maindata)
tab3_ivreg36 <- iv_robust(povrate_b ~ dism1990 + lenper + ctyliterate1920 |herf + lenper + ctyliterate1920 ,se_type = "stata",data = aej_maindata)


tab3_ivreg37 <- iv_robust(lngini_w ~ dism1990 + lenper + ctymanuf_wkrs1920 |herf + lenper + ctymanuf_wkrs1920 ,se_type = "stata",data = aej_maindata)
tab3_ivreg38 <- iv_robust(lngini_b ~ dism1990 + lenper + ctymanuf_wkrs1920 |herf + lenper + ctymanuf_wkrs1920 ,se_type = "stata",data = aej_maindata)
tab3_ivreg39 <- iv_robust(povrate_w ~ dism1990 + lenper + ctymanuf_wkrs1920 |herf + lenper + ctymanuf_wkrs1920 ,se_type = "stata",data = aej_maindata)
tab3_ivreg40 <- iv_robust(povrate_b ~ dism1990 + lenper + ctymanuf_wkrs1920 |herf + lenper + ctymanuf_wkrs1920 ,se_type = "stata",data = aej_maindata)

tab3_ivreg41 <- iv_robust(lngini_w ~ dism1990 + lenper + lfp1920 |herf + lenper + lfp1920 ,se_type = "stata",data = aej_maindata)
tab3_ivreg42 <- iv_robust(lngini_b ~ dism1990 + lenper + lfp1920 |herf + lenper + lfp1920 ,se_type = "stata",data = aej_maindata)
tab3_ivreg43 <- iv_robust(povrate_w ~ dism1990 + lenper + lfp1920 |herf + lenper + lfp1920 ,se_type = "stata",data = aej_maindata)
tab3_ivreg44 <- iv_robust(povrate_b ~ dism1990 + lenper + lfp1920 |herf + lenper + lfp1920 ,se_type = "stata",data = aej_maindata)

tab3_ivreg45 <- iv_robust(lngini_w ~ dism1990 + lenper + herfscore |herf + lenper + herfscore ,se_type = "stata",data = aej_maindata)
tab3_ivreg46 <- iv_robust(lngini_b ~ dism1990 + lenper + herfscore |herf + lenper + herfscore ,se_type = "stata",data = aej_maindata)
tab3_ivreg47 <- iv_robust(povrate_w ~ dism1990 + lenper + herfscore |herf + lenper + herfscore ,se_type = "stata",data = aej_maindata)
tab3_ivreg48 <- iv_robust(povrate_b ~ dism1990 + lenper + herfscore |herf + lenper + herfscore ,se_type = "stata",data = aej_maindata)


# Table 4 

tab4_reg1 <- lm_robust(mv_st_minus_w ~ dism1990, se_type = "stata",data = aej_maindata)
tab4_reg2 <- lm_robust(mv_st_minus_b ~ dism1990, se_type = "stata",data = aej_maindata)
tab4_reg3 <- lm_robust(medgrent_w ~ dism1990, se_type = "stata",data = aej_maindata)
tab4_reg4 <- lm_robust(medgrent_b ~ dism1990, se_type = "stata",data = aej_maindata)
tab4_reg5 <- lm_robust(medgrentpinc_w ~ dism1990, se_type = "stata",data = aej_maindata)
tab4_reg6 <- lm_robust(medgrentpinc_b ~ dism1990, se_type = "stata",data = aej_maindata)
tab4_reg7 <- lm_robust(mt1proom_w ~ dism1990, se_type = "stata",data = aej_maindata)
tab4_reg8 <- lm_robust(mt1proom_b ~ dism1990, se_type = "stata",data = aej_maindata)

tab4_ivreg1 <- iv_robust(mv_st_minus_w ~ dism1990 + lenper | herf + lenper, se_type = "stata",data = aej_maindata)
tab4_ivreg2 <- iv_robust(mv_st_minus_b ~ dism1990 + lenper | herf + lenper, se_type = "stata",data = aej_maindata)
tab4_ivreg3 <- iv_robust(medgrent_w ~ dism1990 + lenper | herf + lenper, se_type = "stata",data = aej_maindata)
tab4_ivreg4 <- iv_robust(medgrent_b ~ dism1990 + lenper | herf + lenper, se_type = "stata",data = aej_maindata)
tab4_ivreg5 <- iv_robust(medgrentpinc_w ~ dism1990 + lenper | herf + lenper, se_type = "stata",data = aej_maindata)
tab4_ivreg6 <- iv_robust(medgrentpinc_b ~ dism1990 + lenper | herf + lenper, se_type = "stata",data = aej_maindata)
tab4_ivreg7 <- iv_robust(mt1proom_w ~ dism1990 + lenper | herf + lenper, se_type = "stata",data = aej_maindata)
tab4_ivreg8 <- iv_robust(mt1proom_b ~ dism1990 + lenper | herf + lenper, se_type = "stata",data = aej_maindata)

tab4_reg9 <- lm_robust(mv_st_minus_w ~ herf + lenper, se_type = "stata", data = aej_maindata_tab2)
tab4_reg10 <- lm_robust(mv_st_minus_b ~ herf + lenper, se_type = "stata", data = aej_maindata_tab2)
tab4_reg11 <- lm_robust(medgrent_w ~ herf + lenper, se_type = "stata", data = aej_maindata_tab2)
tab4_reg12 <- lm_robust(medgrent_b ~ herf + lenper, se_type = "stata", data = aej_maindata_tab2)
tab4_reg13 <- lm_robust(medgrentpinc_w ~ herf + lenper, se_type = "stata", data = aej_maindata_tab2)
tab4_reg14 <- lm_robust(medgrentpinc_b ~ herf + lenper, se_type = "stata", data = aej_maindata_tab2)
tab4_reg15 <- lm_robust(mt1proom_w ~ herf + lenper, se_type = "stata", data = aej_maindata_tab2)
tab4_reg16 <- lm_robust(mt1promm_b ~ herf + lenper, se_type = "stata", data = aej_maindata_tab2)

# Table 5

# Separating the table 5 data into the two desired groups --------------------------------------------------------

table_5_black <- table_5 %>%
  filter(black == 1)

table_5_nonblack <- table_5 %>%
  filter(black == 0)

#-----------------------------------------------------------------------------------------------------------------

tab5_reg1 <- lm_robust(hsdrop ~ dism1980 + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                       clusters = name,data = table_5_nonblack)
tab5_reg2 <- lm_robust(hsdrop ~ dism1980 + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                       clusters = name,data = table_5_black)
tab5_reg3 <- lm_robust(hsgrad ~ dism1980 + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                       clusters = name,data = table_5_nonblack)
tab5_reg4 <- lm_robust(hsgrad ~ dism1980 + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                       clusters = name,data = table_5_black)
tab5_reg5 <- lm_robust(somecoll ~ dism1980 + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                       clusters = name,data = table_5_nonblack)
tab5_reg6 <- lm_robust(somecoll ~ dism1980 + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                       clusters = name,data = table_5_black)
tab5_reg7 <- lm_robust(collgrad ~ dism1980 + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                       clusters = name,data = table_5_nonblack)
tab5_reg8 <- lm_robust(collgrad ~ dism1980 + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                       clusters = name,data = table_5_black)

tab5_ivreg1 <- iv_robust(hsdrop ~ dism1980 + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9 |
                           herf + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                         clusters = name, data = table_5_nonblack)

tab5_ivreg2 <- iv_robust(hsdrop ~ dism1980 + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9 |
                           herf + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                         clusters = name, data = table_5_black)

tab5_ivreg3 <- iv_robust(hsgrad ~ dism1980 + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9 |
                           herf + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                         clusters = name, data = table_5_nonblack)

tab5_ivreg4 <- iv_robust(hsgrad ~ dism1980 + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9 |
                           herf + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                         clusters = name, data = table_5_black)

tab5_ivreg5 <- iv_robust(somecoll ~ dism1980 + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9 |
                           herf + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                         clusters = name, data = table_5_nonblack)

tab5_ivreg6 <- iv_robust(somecoll ~ dism1980 + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9 |
                           herf + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                         clusters = name, data = table_5_black)

tab5_ivreg7 <- iv_robust(collgrad ~ dism1980 + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9 |
                           herf + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                         clusters = name, data = table_5_nonblack)

tab5_ivreg8 <- iv_robust(collgrad ~ dism1980 + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9 |
                           herf + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                         clusters = name, data = table_5_black)

# Filtering the close data ---------------------------------------------------------------------------------------------

table5_black_close <- table_5_black %>%
  filter(closeness < -400)
table5_nonblack_close <- table_5_nonblack %>%
  filter(closeness < -400)
# ----------------------------------------------------------------------------------------------------------------------
tab5_reg9 <- lm_robust(hsdrop ~ herf + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                       clusters = name, data = table5_nonblack_close)

tab5_reg10 <- lm_robust(hsdrop ~ herf + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                       clusters = name, data = table5_black_close)

tab5_reg11 <- lm_robust(hasgrad ~ herf + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                       clusters = name, data = table5_nonblack_close)

tab5_reg12 <- lm_robust(hsgrad ~ herf + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                       clusters = name, data = table5_black_close)

tab5_reg13 <- lm_robust(somecoll ~ herf + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                       clusters = name, data = table5_nonblack_close)

tab5_reg14 <- lm_robust(somecoll ~ herf + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                       clusters = name, data = table5_black_close)

tab5_reg15 <- lm_robust(collgrad ~ herf + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                       clusters = name, data = table5_nonblack_close)

tab5_reg16 <- lm_robust(colgrad ~ herf + lenper + agedum1 + agedum2 + agedum3 + agedum4 + agedum5 + agedum6 + agedum7 + agedum8 + agedum9,
                       clusters = name, data = table5_black_close)





