clear all 
/*desktop directory
cd "C:\Users\joshu\OneDrive\Documents\GitHub\ExperimentalEcon\proj2"
*/
*laptop directory
cd "C:\Users\joshu\OneDrive\Documents\GitHub\ExperimentalEcon\proj2"

*school computer directory


*load dataset
use "Contests with Revisions Replication files\Stata dataset to use for analysis\revisionscontestdata.dta"

*.do file that came with dataset
do "Contests with Revisions Replication files\Stata do files\var_definitions.do"


*label changes
label var xjow "Joy of winning"
label var alpha "$\alpha$"
label var risk "Risk aversion"
label var loss "Loss aversion"
*label var LossLineSwitchToB "Loss aversion"
label var inv "$1/Period$"

*generate sd,median variables
gen x_sd=x
gen x_med=x

*alpha .75 dummy variable 
gen dalpha=0
replace dalpha=1 if alpha==0.75

gen first20=0
replace first20=1 if part==3

gen change=.
replace change=0 if Type==1 & x12==.
replace change=1 if Type==1 & x12~=.

gen reduce=0
replace reduce=1 if Type==1 & x12hat<x1hat

gen raise=0
replace raise=1 if Type==1 & x12hat>x1hat

gen rationalrev=0
replace rationalrev=1 if x1hat~=br1
*replace rationalrev=1 if x2hat & lottery==1

gen overbid=1
replace overbid=0 if x1hat<=br1  

gen underbid=0
replace underbid=1 if x1hat<br1 

gen ahead=0
replace ahead=1 if x1hat>x2hat

gen onedisttoother=x1hat-x2hat
gen twodisttoother=x12hat-x2hat

gen irr_nochange=0 
replace irr_nochange=1 if changehat==0 & rationalrev==1

gen irr_rev_dec=0
replace irr_rev_dec=1 if irr_nochange==1
replace irr_rev_dec=1 if changehat==1 & x1hat==br1

label var rationalrev "Revising is rational"
label var changehat "Subject revised"
label var irr_nochange "No change even though was rational"


****************************************
*************REPLICATION****************
****************************************








****************************************
*****************EXTENSION**************
****************************************

** Gender differences in risk preferences

