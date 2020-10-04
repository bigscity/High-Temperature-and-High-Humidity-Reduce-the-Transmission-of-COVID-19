clear
use data/US_data.dta

xtset code day

gen t6 = (temp_norm + temp_p1_norm + temp_p2_norm + temp_p3_norm + temp_p4_norm + temp_p5_norm )/6
gen h6 = (humi_norm + humi_p1_norm + humi_p2_norm + humi_p3_norm + humi_p4_norm + humi_p5_norm )/6
gen ah6 = (ah0_norm+ahm1_norm+ahm2_norm+ahm3_norm+ahm4_norm+ahm5_norm)/6
gen m50i6 = (m50_index_norm + m50_index_p1_norm + m50_index_p2_norm + m50_index_p3_norm + m50_index_p4_norm + m50_index_p5_norm)/6
gen home6 = (home_stay_norm + home_stay_p1_norm + home_stay_p2_norm + home_stay_p3_norm + home_stay_p4_norm + home_stay_p5_norm)/6

// build factor
factor p_gdp_norm pi_norm poverty_norm employment_norm rich_norm food_stamp_norm, pcf
rotate
predict se_factor

// Table 3 Panel B
xtfmb rt t6 p_dns_norm p_over65_norm gini_norm se_factor p_icubeds_norm m50i6 home6 lat_norm lon_norm if day < 23, lag(3)
xtfmb rt h6 p_dns_norm p_over65_norm gini_norm se_factor p_icubeds_norm m50i6 home6 lat_norm lon_norm if day < 23, lag(3)
xtfmb rt ah6 p_dns_norm p_over65_norm gini_norm se_factor p_icubeds_norm m50i6 home6 lat_norm lon_norm if day < 23, lag(3)
