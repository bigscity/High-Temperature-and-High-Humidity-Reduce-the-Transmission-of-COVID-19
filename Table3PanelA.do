clear
use data/CN_data.dta

xtset code day

gen t6 = (t0_norm+tm1_norm+tm2_norm+tm3_norm+tm4_norm+tm5_norm)/6
gen h6 = (h0_norm+hm1_norm+hm2_norm+hm3_norm+hm4_norm+hm5_norm)/6
gen ss_14 = day7day14_norm / 10000
gen ah6 = (ah0_norm+ahm1_norm+ahm2_norm+ahm3_norm+ahm4_norm+ahm5_norm)/6

gen bmidropr6 = (bmidropr_norm + bmidropr_p1_norm + bmidropr_p2_norm + bmidropr_p3_norm + bmidropr_p4_norm + bmidropr_p5_norm) / 6

// Table 3 Panel A
xtfmb rt t6  p_dns_norm old_norm p_gdp_norm doc_norm bmidropr6 ss_14 lat_norm lon_norm if day < 24, lag(3)
xtfmb rt h6 p_dns_norm old_norm p_gdp_norm doc_norm bmidropr6 ss_14 lat_norm lon_norm if day < 24, lag(3)
xtfmb rt ah6 p_dns_norm old_norm p_gdp_norm doc_norm bmidropr6 ss_14 lat_norm lon_norm if day < 24, lag(3)
