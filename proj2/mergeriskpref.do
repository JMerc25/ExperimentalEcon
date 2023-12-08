clear
*session 1
use "Contests with Revisions Replication files\Semi Cleaned Up Datasets\Session1_Lottery_75_25_Friday_04_08_11_30.dta"

drop if RiskLine1==. 
drop if LossLine1==. 
drop treatment Group Profit TotalProfit Participate Period

save "Contests with Revisions Replication files\RiskPrefdtas\Session1.dta", replace

*session2
use "Contests with Revisions Replication files\Semi Cleaned Up Datasets\Session2_AllPay_75_25_Friday_04_08_4_30.dta" 

drop if RiskLine1==. 
drop if LossLine1==. 
drop treatment Group Profit TotalProfit Participate Period

save "Contests with Revisions Replication files\RiskPrefdtas\Session2.dta", replace

*session3
use "Contests with Revisions Replication files\Semi Cleaned Up Datasets\Session3_AllPay_75_25_Saturday_04_09_11_00.dta" 

drop if RiskLine1==. 
drop if LossLine1==. 
drop treatment Group Profit TotalProfit Participate Period

save "Contests with Revisions Replication files\RiskPrefdtas\Session3.dta", replace

*session4
use "Contests with Revisions Replication files\Semi Cleaned Up Datasets\Session4_Lottery_25_75_Saturday_04_09_1_15.dta" 

drop if RiskLine1==. 
drop if LossLine1==. 
drop treatment Group Profit TotalProfit Participate Period

save "Contests with Revisions Replication files\RiskPrefdtas\Session4.dta", replace

*session5
use "Contests with Revisions Replication files\Semi Cleaned Up Datasets\Session5_Lottery_25_75_Saturday_04_09_3_30.dta" 


drop if RiskLine1==. 
drop if LossLine1==. 
drop treatment Group Profit TotalProfit Participate Period

save "Contests with Revisions Replication files\RiskPrefdtas\Session5.dta", replace

*session6
use "Contests with Revisions Replication files\Semi Cleaned Up Datasets\Session6_Lottery_75_25_Sunday_04_10_11_30.dta"  

drop if RiskLine1==. 
drop if LossLine1==. 
drop treatment Group Profit TotalProfit Participate Period

save "Contests with Revisions Replication files\RiskPrefdtas\Session6.dta", replace

*session7
use "Contests with Revisions Replication files\Semi Cleaned Up Datasets\Session7_AllPay_25_75_Sunday_04_10_1_45.dta" 

drop if RiskLine1==. 
drop if LossLine1==. 
drop treatment Group Profit TotalProfit Participate Period

save "Contests with Revisions Replication files\RiskPrefdtas\Session7.dta", replace

*session8
use "Contests with Revisions Replication files\Semi Cleaned Up Datasets\Session8_AllPay_25_75_Sunday_04_10_4_00.dta"  

drop if RiskLine1==. 
drop if LossLine1==. 
drop treatment Group Profit TotalProfit Participate Period

save "Contests with Revisions Replication files\RiskPrefdtas\Session8.dta", replace

**Merging with main Ds

*.do file that came with dataset
do "Contests with Revisions Replication files\Stata do files\var_definitions.do"

cd "C:\Users\joshu\OneDrive\Documents\GitHub\ExperimentalEcon\proj2\Contests with Revisions Replication files\RiskPrefdtas"

merge m:1 session Subject using Session1.dta
drop _merge
merge m:1 session Subject using Session2.dta
drop _merge
merge m:1 session Subject using Session3.dta
drop _merge
merge m:1 session Subject using Session4.dta
drop _merge
merge m:1 session Subject using Session5.dta
drop _merge
merge m:1 session Subject using Session6.dta
drop _merge
merge m:1 session Subject using Session7.dta
drop _merge
merge m:1 session Subject using Session8.dta
drop _merge



