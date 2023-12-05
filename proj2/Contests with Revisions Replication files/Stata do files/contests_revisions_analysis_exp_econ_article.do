***************************************************
************CONTESTS WITH REVISIONS****************
**********E. DECHENAUX AND S.D. MAGO***************
***************************************************


*******************************
*****Subject charcteristics****
*******************************

*******************************
**********Footnote 12**********
*******************************
clear
use "C:\Users\joshu\OneDrive\Documents\GitHub\ExperimentalEcon\proj2\Contests with Revisions Replication files\Stata dataset to use for analysis\revisionscontestdata.dta"

do "C:\Users\joshu\OneDrive\Documents\GitHub\ExperimentalEcon\proj2\Contests with Revisions Replication files\Stata do files\var_definitions.do"
sort contest
gen ug=1-graduate
gen econmatheng=0
replace econmatheng=1 if major==2
replace econmatheng=1 if major==6
replace econmatheng=1 if major==7

by contest: sum i.gender i.major i.race i.class xjow if part==4  & Period==40
by contest: sum RiskLineSwitchToB LossLineSwitchToB if part==4  & Period==40 & validriskloss==1

prtest female if part==4  & Period==40, by(contest)
prtest joyful if part==4  & Period==40, by(contest)
prtest ug if part==4  & Period==40, by(contest)
prtest econmatheng if part==4  & Period==40, by(contest)
ttest RiskLineSwitchToB if part==4  & Period==40 & validriskloss==1, by(contest)
ttest LossLineSwitchToB if part==4  & Period==40 & validriskloss==1, by(contest)
ttest xjow if part==4  & Period==40, by(contest) 


************************************************
**NON-PARAMETRIC TESTS**************************
************************************************

*Period >10 & Period >30
keep if contestperiod==1
*drop if part==3 & Period<=10
*drop if part==4 & Period<=30

*Descriptive statistics
sort contest alpha
by contest alpha: sum x1hat x2hat x12hat x12eq_expost changehat if Type==1
by contest alpha: sum x12guess if Type==2
by contest alpha: sum x

*Rate of overbidding - Median compared to 1.72 (Sheremeta, 2013)
sort alpha
sum ratio_to_eq if Type==1 & lottery==1 & alpha==0.25, detail
sum ratio_to_eq if Type==2 & lottery==1 & alpha==0.25, detail
sum ratio_to_eq_12 if Type==1 & lottery==1 & alpha==0.25, detail

sum ratio_to_eq if Type==1 & lottery==1 & alpha==0.75, detail
sum ratio_to_eq if Type==2 & lottery==1 & alpha==0.75, detail
sum ratio_to_eq_12 if Type==1 & lottery==1 & alpha==0.75, detail

****************
*Section 5.1.2**
****************

****************
**Appendix C****
****************

****************
*INVESTING ZERO*
****************

*Period >10 & Period >30
keep if contestperiod==1
*drop if part==3 & Period<=10
*drop if part==4 & Period<=30

drop if Type==2
*Period >10 & Period >30
keep if contestperiod==1
*drop if part==3 & Period<=10
*drop if part==4 & Period<=30

gen invest01=1
replace invest01=0 if x1hat>5

gen invest02=1
replace invest02=0 if x2hat>5

gen eq_lthan5_1=0.07 if alpha==0.25
replace eq_lthan5_1=0.2 if alpha==0.75

gen eq_lthan5_2=0.3 if alpha==0.25
replace eq_lthan5_2=0.8 if alpha==0.75

sort contest alpha
by contest alpha: sum x10 invest01 x20 invest02

sort lottery alpha sessionid
collapse invest01 invest02 eq_lthan5_1 eq_lthan5_2, by(lottery alpha sessionid)

sort alpha
by alpha: signrank invest01=eq_lthan5_1 if lottery==0
by alpha: signrank invest02=eq_lthan5_2 if lottery==0

****Frequency of zero expenditure by subjectid

keep if contestperiod==1

gen r1_exp=0
replace r1_exp=1 if x<=5

sort lottery alpha subjectid
by lottery alpha subjectid: egen r1frac_zero=mean(r1_exp)

gen typeb=1-typea

gen zero=0
replace zero=1 if x>=0 & x<=5

gen type_lottery=typeb*lottery
gen type_inv=typeb*inv
gen type_alpha=typeb*alpha

xtprobit zero inv alpha lottery typeb i.sessionid RiskLineSwitchToB LossLineSwitchToB xjow female economics i.race, i(subjectid)
margins, dydx(*) post
est store A
xtprobit zero inv alpha lottery typeb type_lottery type_inv type_alpha i.sessionid RiskLineSwitchToB LossLineSwitchToB xjow female economics i.race, i(subjectid)
margins, dydx(*) post
est store B

esttab A B using table.txt, se r2 keep(inv alpha lottery typeb type_lottery type_inv type_alpha) replace f starlevels(* 0.1 ** 0.05 *** 0.01) 

sort lottery alpha subjectid
collapse r1_exp, by(lottery alpha subjectid)

gen obs=1
gen cat=4
replace cat=3 if r1_exp<=0.75
replace cat=2 if r1_exp<=0.5
replace cat=1 if r1_exp<=0.25

sort lottery alpha cat
egen count=count(obs), by(lottery alpha cat)
sort lottery cat alpha  
tabdisp lottery cat alpha, c(count)

sort alpha
hist r1_exp, width(0.1) fraction by(alpha, note(""))

******With lagged expenditure***********
clear

*Period >10 & Period >30
keep if contestperiod==1
*drop if part==3 & Period<=10
*drop if part==4 & Period<=30

gen typeb=1-typea

sort subjectid Period

gen xl1=x[_n-1]

drop if Period==1

tsset subjectid Period
xtreg x xl1 inv alpha typeb i.sessionid if lottery==1, i(subjectid)
xtreg x xl1 inv alpha typeb i.sessionid if lottery==0, i(subjectid)
xtregar x xl1 alpha typeb i.sessionid if lottery==1
xtregar x xl1 alpha typeb i.sessionid if lottery==0

****************
*INVESTING V***
****************

clear

drop if Type==2
*Period >10 & Period >30
keep if contestperiod==1
*drop if part==3 & Period<=10
*drop if part==4 & Period<=30

*Period >10 & Period >30
keep if contestperiod==1
*drop if part==3 & Period<=10
*drop if part==4 & Period<=30

gen invest1001=0
replace invest1001=1 if x1hat>=95

gen invest1002=0
replace invest1002=1 if x2hat>=95

sort contest alpha

drop if Type==2
by contest alpha: sum x1100 invest1001 x2100 invest1002

*Footnote 20*

gen response=3
replace response=2 if x12hat<=5
replace response=1 if x12hat>=95

by contest alpha: tab response if x2hat>=95


***********************************************************
******************RESULT 1*********************************
***********************************************************


*Period >10 & Period >30
keep if contestperiod==1
drop if Type==2
*drop if part==3 & Period<=10
*drop if part==4 & Period<=30

sort contest lottery alpha sessionid
collapse x1hat x2hat x12hat x1eq x2eq x12eq x12eq_expost changehat, by(contest lottery alpha sessionid)

***********************************************
****WITHIN SESSION EQUILIBRIUM COMPARISONS*****
***********************************************

sort alpha
*ALL PAY*
by alpha: signrank x1hat=x1eq if lottery==0
by alpha: signrank x2hat=x2eq if lottery==0
by alpha: signrank x12hat=x12eq if lottery==0
by alpha: signrank x12hat=x12eq_expost if lottery==0
by alpha: signrank changehat=1 if lottery==0
*LOTTERY*
by alpha: signrank x1hat=x1eq if lottery==1
by alpha: signrank x2hat=x2eq if lottery==1
by alpha: signrank x12hat=x12eq if lottery==1
by alpha: signrank x12hat=x12eq_expost if lottery==1
by alpha: signrank changehat=0 if lottery==1


***********************************************************
******************TABLES*********************************
***********************************************************

*******************
******TABLE 3******
*******************


*Period >10 & Period >30
keep if contestperiod==1
drop if Type==2
*drop if part==3 & Period<=10
*drop if part==4 & Period<=30

sort contest alpha
by contest alpha: sum x1hat x2hat x12hat changehat flexibility dissipation

***********************************************
****WITHIN SESSION EQUILIBRIUM COMPARISONS*****
***********************************************

sort contest lottery alpha sessionid
collapse x1hat x2hat x12hat flexibility dissipation x1eq x2eq x12eq x12eq_expost changehat eq_flexibility eq_dissipation, by(contest lottery alpha sessionid)

sort alpha
*ALL PAY*
by alpha: signrank x1hat=x1eq if lottery==0
by alpha: signrank x2hat=x2eq if lottery==0
by alpha: signrank x12hat=x12eq if lottery==0
by alpha: signrank x12hat=x12eq_expost if lottery==0
by alpha: signrank changehat=1 if lottery==0
by alpha: signrank flexibility=eq_flexibility if lottery==0
by alpha: signrank dissipation=eq_dissipation if lottery==0
*LOTTERY*
by alpha: signrank x1hat=x1eq if lottery==1
by alpha: signrank x2hat=x2eq if lottery==1
by alpha: signrank x12hat=x12eq if lottery==1
by alpha: signrank x12hat=x12eq_expost if lottery==1
by alpha: signrank changehat=0 if lottery==1
by alpha: signrank flexibility=eq_flexibility if lottery==1
by alpha: signrank dissipation=eq_dissipation if lottery==1

*******************
******TABLE 4******
*******************


*Period >10 & Period >30
keep if contestperiod==1
*drop if part==3 & Period<=10
*drop if part==4 & Period<=30

*Descriptive statistics
sort contest alpha
by contest alpha: sum x1hat x2hat x12hat x12eq_expost changehat if Type==1

********************************************************
****WITHIN SESSION TYPE AND ROUND 1 & 2 COMPARISONS*****
********************************************************
sort contest lottery alpha sessionid
collapse x1hat x2hat x12hat x1eq x2eq x12eq x12eq_expost changehat, by(contest lottery alpha sessionid)

sort alpha
*ALL PAY*
by alpha: signrank x1hat=x2hat if lottery==0
by alpha: signrank x2hat=x12hat if lottery==0
by alpha: signrank x1hat=x12hat if lottery==0
by alpha: signrank x2hat=x12hat if lottery==0
*LOTTERY*
by alpha: signrank x1hat=x2hat if lottery==1
by alpha: signrank x2hat=x12hat if lottery==1
by alpha: signrank x1hat=x12hat if lottery==1
by alpha: signrank x2hat=x12hat if lottery==1

********************************************************
****WITHIN SESSION TREATMENT (25 VS. 75) COMPARISONS****
********************************************************


*Period >10 & Period >30
keep if contestperiod==1
drop if part==3 & Period<=10
drop if part==4 & Period<=30

********************************************************
********FIRST CREATE PAIRED OBSERVATIONS DATASET********
********************************************************
sort contest sessionid alpha
collapse x1hat x2hat x12hat x1100 x2100 x12100 x10 x20 x120 changehat, ///
by(contest sessionid alpha)
save C:\data\bid25.dta, replace

clear
use C:\data\bid25.dta

keep if alpha==0.25
gen x125=x1hat
gen x225=x2hat
gen x1225=x12hat
gen x110025=x1100
gen x210025=x2100
gen x1210025=x12100
gen x1025=x10
gen x2025=x20
gen x12025=x120
gen change25=changehat
drop  x1hat x2hat x12hat x1100 x2100 x12100 x10 x20 x120 changehat
*label var sp5 "SP Low cost"
sort sessionid
save C:\data\bid25_1.dta, replace

clear
use C:\data\bid25.dta

keep if alpha==0.75
gen x175=x1hat
gen x275=x2hat
gen x1275=x12hat
gen x110075=x1100
gen x210075=x2100
gen x1210075=x12100
gen x1075=x10
gen x2075=x20
gen x12075=x120
gen change75=changehat
drop  x1hat x2hat x12hat x1100 x2100 x12100 x10 x20 x120 changehat
*label var sp15 "SP High cost"
sort sessionid
save C:\data\bid25_2.dta, replace

clear
use C:\data\bid25_1.dta
merge 1:1 sessionid using "C:\data\bid25_2.dta"
drop _merge
save C:\data\bid25.dta, replace

log using C:\purdue\nonpara, replace

********************************/
********SECOND RUN TESTS********
********************************/

sort contest sessionid
by contest: signrank x125=x175
by contest: signrank x225=x275
by contest: signrank x1225=x1275
by contest: signrank x110025=x110075
by contest: signrank x210025=x210075
by contest: signrank x1210025=x1210075

by contest: signrank x125=x225
by contest: signrank x175=x275

by contest: signrank change25=change75
*log close

*log using C:\purdue\nonpara, append

log close

*********************************************************


*******************
******TABLE 5******
*******************

do C:\purdue\var_definitions.do

keep if contestperiod==1

sort subjectid Period
by subjectid: gen totaltypea=typea[1]
by subjectid: replace totaltypea=typea[_n]+ totaltypea[_n-1] if _n>1
gen first20=0
replace first20=1 if part==3


*drop if part==3 & Period<=10
*drop if part==4 & Period<=30

label var xjow "Joy of winning"
label var alpha "$\alpha$"
label var risk "Risk aversion"
label var loss "Loss aversion"
*label var LossLineSwitchToB "Loss aversion"
label var inv "$1/Period$"

**********SUBJECT RE***********************************************************************
*ALL PAY AUCTION
xtreg x d75 inv first20 i.sessionid if Type==1 & contest=="All Pay Auction", i(subjectid) vce(cluster sessionid)
est store A
xtreg x d75 inv first20 i.sessionid if Type==2 & contest=="All Pay Auction", i(subjectid) vce(cluster sessionid)
est store B
*xtreg x12hat d75 inv f75 if Type==1 & validriskloss==1 & contest=="All Pay Auction", i(subjectid) vce(cluster sessionid)
*est store C
*LOTTERY
xtreg x d75 inv first20 i.sessionid if Type==1 &  contest=="Lottery", i(subjectid) vce(cluster sessionid)
est store D
xtreg x d75 inv first20 i.sessionid if Type==2 & contest=="Lottery", i(subjectid) vce(cluster sessionid)
est store E
*reg x12 d75 inv f75 if Type==1  &  contest=="Lottery", vce(cluster subjectid)
*est store F

esttab A B D E using table.txt, se r2 keep(_cons d75 inv first20 _cons) replace f starlevels(* 0.1 ** 0.05 *** 0.01) 
esttab A B D E using table.tex, se r2 keep(_cons d75 inv first20 _cons) replace f starlevels(* 0.1 ** 0.05 *** 0.01)

*******************
******TABLE 6******
*******************

**********DISPERSION MEASURES****************************
clear
do C:\purdue\var_definitions.do

keep if periodt>10

gen x_sd=x
gen x_med=x

sort lottery Type alpha sessionid subjectid
collapse (mean) x (sd) x_sd (p50) x_med, by(lottery Type alpha sessionid subjectid)
gen coeff_var=x_sd/x

gen dalpha=0
replace dalpha=1 if alpha==0.75

*H_s measure = SD of the median
by lottery Type alpha sessionid: egen x_sd_med=sd(x_med)
*D_s measure =  Median of the SD
by lottery Type alpha sessionid: egen x_med_sd=median(x_sd)

sort lottery Type alpha sessionid
collapse x_med_sd x_sd_med, by(lottery Type alpha sessionid)

*Compare alpha's*
save C:\purdue\cv_tests, replace

keep if alpha==0.25
rename x_sd_med x_med1
rename x_med_sd x_sd1
save C:\purdue\cv_type1, replace
clear
use C:\purdue\cv_tests
keep if alpha==0.75
rename x_med_sd x_sd2
rename x_sd_med x_med2
save C:\purdue\cv_type2, replace
merge 1:1 Type sessionid using "C:\purdue\cv_type1.dta"

sort lottery Type
*H_s measure = SD of the median
by lottery Type: signrank x_med1=x_med2
*D_s measure = Median of the SD
by lottery Type: signrank x_sd1=x_sd2

*Compare Types*
save C:\purdue\cv_tests, replace

keep if Type==1
rename x_med_sd x_sd1
rename x_sd_med x_med1
save C:\purdue\cv_type1, replace
clear
use C:\purdue\cv_tests
keep if Type==2
rename x_med_sd x_sd2
rename x_sd_med x_med2
save C:\purdue\cv_type2, replace
merge 1:1 alpha sessionid using "C:\purdue\cv_type1.dta"

sort lottery alpha
*H_s measure = SD of the median
by lottery alpha: signrank x_med1=x_med2
*D_s measure =  Median of the SD
by lottery alpha: signrank x_sd1=x_sd2

*Compare Contests*

sort alpha Type lottery
*H_s measure = SD of the median
by alpha Type: ranksum x_sd_med, by(lottery)
*D_s measure =  Median of the SD
by alpha Type: ranksum x_med_sd, by(lottery)

*******************
******TABLE 7******
*******************

do C:\purdue\var_definitions.do
keep if contestperiod==1
*drop if part==3 & Period<=10
*drop if part==4 & Period<=30

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

*******LOTTERY***************************************************
xtprobit change d75 inv first20 i.sessionid if Type==1 &  contest=="Lottery", ///
re i(subjectid) vce(cluster sessionid)
margins, dydx(*) post
est store A
******************************************
xtprobit change adist1 adist1_sq ahead d75 inv first20 i.sessionid if Type==1 &  contest=="Lottery", ///
re i(subjectid) vce(cluster sessionid)
margins, dydx(*) post
est store B
******************************************
xtprobit change adist1 adist1_sq ahead d75 inv first20 RiskLineSwitchToB LossLineSwitchToB //// 
xjow female if Type==1 & validriskloss==1 &  contest=="Lottery", ///
re i(subjectid) vce(cluster sessionid)
margins, dydx(*) post
est store C
*****************************************
esttab A B C using table.tex, se r2 keep(adist1 adist1_sq ahead d75 inv f75 ///
RiskLineSwitchToB LossLineSwitchToB xjow female) ///
replace f starlevels(* 0.1 ** 0.05 *** 0.01)
****************************************************************

*******************
******TABLE 8******
*******************

do C:\purdue\var_definitions.do

*keep if changehat==1

*Period >10 & <=20 & Period >30
*drop if part==3 & Period<=10
*drop if part==4 & Period<=30

keep if contestperiod==1
keep if lottery==1
keep if Type==1
sum match2

*gen direction=0
*replace direction=1 if diff_br_1>diff_br_2
*replace direction=1 if  diff_br_1==diff_br_2 & diff_br_1==0

gen outspend2=0
replace outspend2=1 if x12hat>=x2hat
sum outspend2
gen slightoutspend2=0
replace slightoutspend2=1 if x12hat-x2hat>=0.01 
replace slightoutspend2=0 if x12hat-x2hat>5
sum slightoutspend2

gen sqrtx2hat=sqrt(x2hat)
gen x2hatsq=x2hat*x2hat

egen xjowbar=mean(xjow)

*xtreg x12hat x2hat sqrtx2hat inv if alpha==0.25, fe i(subjectid)
*est store fixed
*xtreg x12hat x2hat sqrtx2hat inv if alpha==0.25, re i(subjectid)
*hausman fixed ., sigmamore

*IN THE PAPER - JUNE 2021
*alpha=0.25
xtreg x12hat x2hat sqrtx2hat inv if alpha==0.25, i(subjectid) vce(cluster sessionid)
*reg x12hat x2hat x2hatsq inv if alpha==0.25, vce(cluster subjectid)
est store A
*predict xbr25 if alpha==0.25
gen xbr25=_b[_cons]+_b[x2hat]*x2hat+_b[sqrtx2hat]*sqrtx2hat
*gen xbr25=_b[_cons]+_b[x2hat]*x2hat+_b[x2hatsq]*x2hatsq
*alpha=0.75
xtreg x12hat x2hat sqrtx2hat inv if alpha==0.75,  i(subjectid) vce(cluster sessionid)
*reg x12hat x2hat x2hatsq inv if alpha==0.75, vce(cluster subjectid)
est store B
*predict xbr75 if alpha==0.75
gen xbr75=_b[x2hat]*x2hat+_b[sqrtx2hat]*sqrtx2hat
*gen xbr75=_b[_cons]+_b[x2hat]*x2hat+_b[x2hatsq]*x2hatsq
xtreg x12hat x2hat sqrtx2hat inv if alpha==0.25 & changehat==1,  i(subjectid) vce(cluster sessionid)
*reg x12hat x2hat x2hatsq inv if alpha==0.75, vce(cluster subjectid)
est store C
*predict xbr75 if alpha==0.75
gen xbr25_rev=_b[x2hat]*x2hat+_b[sqrtx2hat]*sqrtx2hat
*gen xbr75=_b[_cons]+_b[x2hat]*x2hat+_b[x2hatsq]*x2hatsq
xtreg x12hat x2hat sqrtx2hat inv if alpha==0.75 & changehat==1,  i(subjectid) vce(cluster sessionid)
*reg x12hat x2hat x2hatsq inv if alpha==0.75, vce(cluster subjectid)
est store D
*predict xbr75 if alpha==0.75
gen xbr75_rev=_b[x2hat]*x2hat+_b[sqrtx2hat]*sqrtx2hat
*gen xbr75=_b[_cons]+_b[x2hat]*x2hat+_b[x2hatsq]*x2hatsq


esttab A B C D using table.tex, ///
se keep(x2hat sqrtx2hat inv _cons) replace f starlevels(* 0.1 ** 0.05 *** 0.01) stats(N r2)
esttab A B C D using table.txt, ///
se keep(x2hat sqrtx2hat inv _cons) replace f starlevels(* 0.1 ** 0.05 *** 0.01) stats(N r2)
*se keep(x2hat  x2hatsq inv _cons) replace f starlevels(* 0.1 ** 0.05 *** 0.01) stats(N r2)

*******************
******TABLE 9******
*******************

do C:\purdue\var_definitions.do
*Period >10 & Period >30
keep if contestperiod==1

sort contest alpha
by contest alpha: sum won_contest1 if Type==1 & Round==1
by contest alpha: sum won_contest2 if Type==1 & Round==2
by contest alpha: sum Profit if Type==1
by contest alpha: sum Profit if Type==2
by contest alpha: sum flexibility if Type==1
by contest alpha: sum dissipation if Type==1


********************************************************
********FIRST CREATE PAIRED OBSERVATIONS DATASET********
********************************************************

*******************
****alpha=0.25*****
*******************

do C:\purdue\var_definitions.do
*Period >10 & Period >30
keep if contestperiod==1

keep if alpha==0.25

sort contest alpha sessionid Type
collapse Profit payoff1 eq_flexibility dissipation eq_dissipation probwin1 probwin12, by(contest alpha sessionid Type)
save C:\data\profit.dta, replace

clear
use C:\data\profit.dta

keep if Type==1
gen profittype1=Profit
gen profittype1_25=Profit
gen profit1type1=payoff1
gen diss_25=dissipation
gen probwin1_25=probwin1
gen probwin12_25=probwin12
drop Profit payoff1
sort sessionid
save C:\data\profit1.dta, replace

clear
use C:\data\profit.dta

keep if Type==2
gen profittype2=Profit
gen profit1type2=payoff1
gen profittype2_25=Profit
gen diss_25=dissipation
drop Profit payoff1
sort sessionid
save C:\data\profit2.dta, replace

clear
use C:\data\profit1.dta
merge 1:1 sessionid using "C:\data\profit2.dta"
drop _merge
save C:\data\profit25.dta, replace

sort contest sessionid
by contest: signrank profittype1=profittype2
by contest: signrank profit1type1=profit1type2


*******************
****alpha=0.75*****
*******************

do C:\purdue\var_definitions.do
*Period >10 & Period >30
keep if contestperiod==1

keep if alpha==0.75

sort contest alpha sessionid Type
collapse Profit payoff1 eq_flexibility dissipation eq_dissipation probwin1 probwin12, by(contest alpha sessionid Type)
save C:\data\profit.dta, replace

clear
use C:\data\profit.dta

keep if Type==1
gen profittype1=Profit
gen profittype1_75=Profit
gen profit1type1=payoff1
gen probwin1_75=probwin1
gen probwin12_75=probwin12
gen diss_75=dissipation
drop Profit payoff1
sort sessionid
save C:\data\profit1.dta, replace

clear
use C:\data\profit.dta

keep if Type==2
gen profittype2=Profit
gen profittype2_75=Profit
gen profit1type2=payoff1
gen diss_75=dissipation
drop Profit payoff1
sort sessionid
save C:\data\profit2.dta, replace

clear
use C:\data\profit1.dta
merge 1:1 sessionid using "C:\data\profit2.dta"
drop _merge
save C:\data\profit75.dta, replace

****CREATE AND USE DATASET WITH BOTH ALPHA'S****

merge 1:1 sessionid using "C:\data\profit25.dta"
drop _merge
save C:\data\profit_alpha.dta, replace

clear
use C:\data\profit_alpha.dta

gen diff25=profittype1_25-profittype2_25
gen diff75=profittype1_75-profittype2_75

sort contest
by contest: signrank diff25=diff75
by contest: signrank diss_25=diss_75 if Type==1

by contest: signrank probwin1_25=probwin1_75 if Type==1
by contest: signrank probwin12_25=probwin12_75 if Type==1
by contest: signrank profittype1_25=profittype1_75 if Type==1
by contest: signrank profittype2_25=profittype2_75 if Type==1

***or***
append using C:\data\profit25.dta
save C:\data\profit_all.dta, replace

clear
use C:\data\profit_all.dta

sort contest alpha sessionid
by contest alpha: signrank profittype1=profittype2
by contest alpha: signrank profit1type1=profit1type2

gen obs_flexibility=profittype1-profittype2
by contest alpha: sum obs_flexibility eq_flexibility 
by contest alpha: signrank obs_flexibility=eq_flexibility
by contest alpha: signrank dissipation=eq_dissipation
sort contest
ranksum obs_flexibility if alpha==0.25, by(contest)
ranksum obs_flexibility if alpha==0.75, by(contest)
ranksum dissipation if alpha==0.25, by(contest)
ranksum dissipation if alpha==0.75, by(contest)



**********************************************************
*******************FIGURES********************************
**********************************************************

*******************
******FIGURE 1*****
*******************

*****TREATMENTS***

do C:\purdue\var_definitions.do

keep if contestperiod==1
*drop if part==3 & Period<=10
*drop if part==4 & Period<=30

drop if Type==2
sort contest alpha
collapse x1hat x1eq, by(contest alpha)

gen time=1
rename x1hat observed
rename x1eq equilibrium

save C:\purdue\d1, replace

**Step 2**
do C:\purdue\var_definitions.do

keep if contestperiod==1
*drop if part==3 & Period<=10
*drop if part==4 & Period<=30

drop if Type==2
sort contest alpha
collapse x2hat x2eq, by(contest alpha)

gen time=2
rename x2hat observed
rename x2eq equilibrium

save C:\purdue\d2, replace

**Step 3**
do C:\purdue\var_definitions.do
keep if contestperiod==1
*drop if part==3 & Period<=10
*drop if part==4 & Period<=30

drop if Type==2
sort contest alpha
collapse x12hat x12eq, by(contest alpha)
gen time=3
rename x12hat observed
rename x12eq equilibrium

save C:\purdue\d12, replace

append using C:\purdue\d1
append using C:\purdue\d2

gen a25=1 if alpha==0.25
replace a25=2 if alpha==0.75
gen alphag=1 if alpha==0.25
replace alphag=2 if alpha==0.75
gen timeg=1 if time==1
replace timeg=2 if time==2
replace timeg=3 if time==3

label var observed "Observed"
label var equilibrium "Equilibrium"

save C:\purdue\treatments, replace

sort time
*label define a25 0 " " 1 "alpha=0.25" 2 "alpha=0.75" 3 " "
label define a25 0 " " 1 "alpha=0.25" 2 "0.75" 3 " "
label define time 1 "Type A - Round 1" 2 "Type B - Round 1" 3 "Type A - Round 2"
label val timeg time
label val alphag a25
label var alphag "Treatment"
label val alphag a25
label var alphag "Treatment"
format observed %9.2f
format equilibrium %9.2f

*All Pay
twoway scatter observed equilibrium alphag if contest=="All Pay Auction", ///
by(timeg, cols(3) note("")) mlabel(observed equilibrium) mlabc(black black) mlabsize(medium medium) mlabposition(12 12) ///
xlabel(0 1 2 3, valuelabel labs(medium)) connect(l l) lp(solid dash) lc(black black) mc(black black) ///
legend(off) ylabel(0(25)50) ytitle("Expenditure") ///
saving(C:\purdue\figAPA, replace)
graph use C:\purdue\figAPA
graph export C:\purdue\figAPA.pdf, replace

*Lottery
twoway scatter observed equilibrium alphag if contest=="Lottery", ///
by(timeg , cols(3) note("")) mlabel(observed equilibrium) mlabc(black black) mlabsize(medium medium) mlabposition(12 12) ///
xlabel(0 1 2 3, valuelabel labs(medium)) connect(l l) lp(solid dash) lc(black black) mc(black black) ///
legend(off) ylabel(0(25)50) ytitle("Expenditure") ///
saving(C:\purdue\figLott, replace)
graph use C:\purdue\figLott
graph export C:\purdue\figLott.pdf, replace

*******************
******FIGURE 2*****
*******************

*****HISTOGRAMS****

do C:\purdue\var_definitions.do

keep if contestperiod==1
*drop if part==3 & Period<=10
*drop if part==4 & Period<=30

*Type A*
   hist x1hat if Type==1 & lottery==0 & alpha==0.25, ///
     xline(75) width(5) fraction ylabel(0(0.1)0.5) title("All-pay auction, alpha=0.25") col(blue) lc(black) saving(1APA25, replace) 
   hist x1hat if Type==1 & lottery==0 & alpha==0.75, ///
     xline(25) width(5) fraction ylabel(0(0.1)0.5) title("All-pay auction, alpha=0.75") col(blue) lc(black) saving(1APA75, replace)
   hist x1hat if Type==1 & lottery==1 & alpha==0.25, ///
     xline(25) width(5) fraction ylabel(0(0.1)0.5) title("Lottery, alpha=0.25") col(blue) lc(black) saving(1Lott25, replace)
   hist x1hat if Type==1 & lottery==1 & alpha==0.75, /// 
     xline(25) width(5) fraction ylabel(0(0.1)0.5) title("Lottery, alpha=0.75") col(blue) lc(black) saving(1Lott75, replace)

gr combine 1APA25.gph 1APA75.gph 1Lott25.gph 1Lott75.gph, saving(C:\purdue\1hist, replace)
	  graph use C:\purdue\1hist
      graph export C:\purdue\1hist.pdf, replace

*Type B*
   hist x2hat if Type==1 & lottery==0 & alpha==0.25, /// 
     xline(75) width(5) fraction ylabel(0(0.1)0.5) title("All-pay auction, alpha=0.25") col(blue) lc(black) saving(2APA25, replace) 
   hist x2hat if Type==1 & lottery==0 & alpha==0.75, /// 
     xline(25) width(5) fraction ylabel(0(0.1)0.5) title("All-pay auction, alpha=0.75") col(blue) lc(black) saving(2APA75, replace)
   hist x2hat if Type==1 & lottery==1 & alpha==0.25, /// 
     xline(25) width(5) fraction ylabel(0(0.1)0.5) title("Lottery, alpha=0.25") col(blue) lc(black) saving(2Lott25, replace)
   hist x2hat if Type==1 & lottery==1 & alpha==0.75, ///
     xline(25) width(5) fraction ylabel(0(0.1)0.5) title("Lottery, alpha=0.75") col(blue) lc(black) saving(2Lott75, replace)

gr combine 2APA25.gph 2APA75.gph 2Lott25.gph 2Lott75.gph, saving(C:\purdue\2hist, replace)
	  graph use C:\purdue\2hist
      graph export C:\purdue\2hist.pdf, replace

*******************
******FIGURE 3*****
*******************

*****BOX PLOTS*****
	  
do C:\purdue\var_definitions.do

drop if periodt<11

sort lottery Type alpha subjectid
by lottery Type alpha subjectid: egen med_x=median(x)
by lottery Type alpha subjectid: egen lqt_x = pctile(x), p(25)
by lottery Type alpha subjectid: egen uqt_x = pctile(x), p(75)
by lottery Type alpha subjectid: egen iqr_x = iqr(x)
by lottery Type alpha subjectid: egen mean_x = mean(x)
by lottery Type alpha subjectid: egen sd_x = sd(x)

by lottery Type alpha subjectid: egen ls_x = min(max(x, lqt_x-1.5*iqr_x))
by lottery Type alpha subjectid: egen us_x = max(min(x, uqt_x+1.5*iqr_x))
gen coeff_var=sd_x/mean_x

gen out_x = x if(x<=lqt_x-1.5*iqr_x | x>=uqt_x+1.5*iqr_x)

sort lottery Type alpha mean_x subjectid
egen mean_id=group(lottery Type alpha mean_x subjectid)

by lottery Type alpha: egen min_id=min(mean_id)
gen mean_id_scale=mean_id-min_id+1

label var mean_id_scale "Subject rank (low to high mean)"
label var x "Exp."

*LOTTERY*

twoway 	rbar lqt_x med_x mean_id_scale if lottery==1 & alpha==0.25 & Type==1, fcolor(gs12) lcolor(black)  || ///
		rbar med_x uqt_x mean_id_scale if lottery==1 & alpha==0.25 & Type==1, fcolor(gs12) lcolor(black)  || ///
		rspike uqt_x us_x mean_id_scale if lottery==1 & alpha==0.25 & Type==1, lcolor(gs12)  || ///
		rspike ls_x lqt_x mean_id_scale if lottery==1 & alpha==0.25 & Type==1, lcolor(gs12)   || ///
		rcap us_x us_x mean_id_scale if lottery==1 & alpha==0.25 & Type==1, lcolor(gs12)  || ///
		rcap ls_x ls_x mean_id_scale if lottery==1 & alpha==0.25 & Type==1, lcolor(gs12)  || ///
		scatter out_x mean_id_scale if  lottery==1 & alpha==0.25 & Type==1, ms(circle) mcolor(blue) mfc(none) msize(0.4) || ///
		scatter mean_x mean_id_scale if lottery==1 & alpha==0.25 & Type==1, legend(off) ms(diamond) mcolor(red) mfc(none) msize(0.4) title("Lottery - alpha = 0.25", si(medium)) yline(25, lc(blue)) ///
		ytitle("Expenditure") ylabel(0(25)100) name(typeA_025_subj, replace)
		
twoway 	rbar lqt_x med_x mean_id_scale if lottery==1 & alpha==0.25 & Type==2, fcolor(gs12) lcolor(black)  || ///
		rbar med_x uqt_x mean_id_scale if lottery==1 & alpha==0.25 & Type==2, fcolor(gs12) lcolor(black)  || ///
		rspike uqt_x us_x mean_id_scale if lottery==1 & alpha==0.25 & Type==2, lcolor(gs12)  || ///
		rspike ls_x lqt_x mean_id_scale if lottery==1 & alpha==0.25 & Type==2, lcolor(gs12)   || ///
		rcap us_x us_x mean_id_scale if lottery==1 & alpha==0.25 & Type==2, lcolor(gs12)  || ///
		rcap ls_x ls_x mean_id_scale if lottery==1 & alpha==0.25 & Type==2, lcolor(gs12)  || ///
		scatter out_x mean_id_scale if  lottery==1 & alpha==0.25 & Type==2, ms(circle) mcolor(blue) mfc(none) msize(0.4) || ///
		scatter mean_x mean_id_scale if lottery==1 & alpha==0.25 & Type==2, legend(off) ms(diamond) mcolor(red) mfc(none) msize(0.4) title("Lottery - alpha = 0.25", si(medium)) yline(25, lc(blue)) ///	
		ytitle("Expenditure")  ylabel(0(25)100) name(typeB_025_subj, replace)

twoway 	rbar lqt_x med_x mean_id_scale if lottery==1 & alpha==0.75 & Type==1, fcolor(gs12) lcolor(black)  || ///
		rbar med_x uqt_x mean_id_scale if lottery==1 & alpha==0.75 & Type==1, fcolor(gs12) lcolor(black)  || ///
		rspike uqt_x us_x mean_id_scale if lottery==1 & alpha==0.75 & Type==1, lcolor(gs12)  || ///
		rspike ls_x lqt_x mean_id_scale if lottery==1 & alpha==0.75 & Type==1, lcolor(gs12)   || ///
		rcap us_x us_x mean_id_scale if lottery==1 & alpha==0.75 & Type==1, lcolor(gs12)  || ///
		rcap ls_x ls_x mean_id_scale if lottery==1 & alpha==0.75 & Type==1, lcolor(gs12)  || ///
		scatter out_x mean_id_scale if  lottery==1 & alpha==0.75 & Type==1, ms(circle) mcolor(blue) mfc(none) msize(0.4) || ///
		scatter mean_x mean_id_scale if lottery==1 & alpha==0.75 & Type==1, legend(off) ms(diamond) mcolor(red) mfc(none) msize(0.4) title("Lottery - alpha = 0.75", si(medium)) yline(25, lc(blue)) ///
		ytitle("Expenditure")  ylabel(0(25)100) name(typeA_075_subj, replace)
		
twoway 	rbar lqt_x med_x mean_id_scale if lottery==1 & alpha==0.75 & Type==2, fcolor(gs12) lcolor(black)  || ///
		rbar med_x uqt_x mean_id_scale if lottery==1 & alpha==0.75 & Type==2, fcolor(gs12) lcolor(black)  || ///
		rspike uqt_x us_x mean_id_scale if lottery==1 & alpha==0.75 & Type==2, lcolor(gs12)  || ///
		rspike ls_x lqt_x mean_id_scale if lottery==1 & alpha==0.75 & Type==2, lcolor(gs12)   || ///
		rcap us_x us_x mean_id_scale if lottery==1 & alpha==0.75 & Type==2, lcolor(gs12)  || ///
		rcap ls_x ls_x mean_id_scale if lottery==1 & alpha==0.75 & Type==2, lcolor(gs12)  || ///
		scatter out_x mean_id_scale if  lottery==1 & alpha==0.75 & Type==2, ms(circle) mcolor(blue) mfc(none) msize(0.4) || ///
		scatter mean_x mean_id_scale if lottery==1 & alpha==0.75 & Type==2, legend(off) ms(diamond) mcolor(red) mfc(none) msize(0.4) title("Lottery - alpha = 0.75", si(medium)) yline(25, lc(blue)) ///
		ytitle("Expenditure")  ylabel(0(25)100) name(typeB_075_subj, replace)

graph combine typeA_025_subj typeA_075_subj typeB_025_subj typeB_075_subj, name(box_subj, replace)
graph export box_subj.jpg, replace

*ALL-PAY*

twoway 	rbar lqt_x med_x mean_id_scale if lottery==0 & alpha==0.25 & Type==1, fcolor(gs12) lcolor(black)  || ///
		rbar med_x uqt_x mean_id_scale if lottery==0 & alpha==0.25 & Type==1, fcolor(gs12) lcolor(black)  || ///
		rspike uqt_x us_x mean_id_scale if lottery==0 & alpha==0.25 & Type==1, lcolor(gs12)  || ///
		rspike ls_x lqt_x mean_id_scale if lottery==0 & alpha==0.25 & Type==1, lcolor(gs12)   || ///
		rcap us_x us_x mean_id_scale if lottery==0 & alpha==0.25 & Type==1, lcolor(gs12)  || ///
		rcap ls_x ls_x mean_id_scale if lottery==0 & alpha==0.25 & Type==1, lcolor(gs12)  || ///
		scatter out_x mean_id_scale if  lottery==0 & alpha==0.25 & Type==1, ms(circle) mcolor(blue) mfc(none) msize(0.4) || ///
		scatter mean_x mean_id_scale if lottery==0 & alpha==0.25 & Type==1, legend(off) ms(diamond) mcolor(red) mfc(none) msize(0.4) title("All-Pay - alpha = 0.25", si(medium)) yline(75, lc(blue)) ///
		ytitle("Expenditure") ylabel(0(25)100) name(typeA_025_subj_apa, replace)
		
twoway 	rbar lqt_x med_x mean_id_scale if lottery==0 & alpha==0.25 & Type==2, fcolor(gs12) lcolor(black)  || ///
		rbar med_x uqt_x mean_id_scale if lottery==0 & alpha==0.25 & Type==2, fcolor(gs12) lcolor(black)  || ///
		rspike uqt_x us_x mean_id_scale if lottery==0 & alpha==0.25 & Type==2, lcolor(gs12)  || ///
		rspike ls_x lqt_x mean_id_scale if lottery==0 & alpha==0.25 & Type==2, lcolor(gs12)   || ///
		rcap us_x us_x mean_id_scale if lottery==0 & alpha==0.25 & Type==2, lcolor(gs12)  || ///
		rcap ls_x ls_x mean_id_scale if lottery==0 & alpha==0.25 & Type==2, lcolor(gs12)  || ///
		scatter out_x mean_id_scale if  lottery==0 & alpha==0.25 & Type==2, ms(circle) mcolor(blue) mfc(none) msize(0.4) || ///
		scatter mean_x mean_id_scale if lottery==0 & alpha==0.25 & Type==2, legend(off) ms(diamond) mcolor(red) mfc(none) msize(0.4) title("All-Pay - alpha = 0.25", si(medium)) yline(75, lc(blue)) ///	
		ytitle("Expenditure")  ylabel(0(25)100) name(typeB_025_subj_apa, replace)

twoway 	rbar lqt_x med_x mean_id_scale if lottery==0 & alpha==0.75 & Type==1, fcolor(gs12) lcolor(black)  || ///
		rbar med_x uqt_x mean_id_scale if lottery==0 & alpha==0.75 & Type==1, fcolor(gs12) lcolor(black)  || ///
		rspike uqt_x us_x mean_id_scale if lottery==0 & alpha==0.75 & Type==1, lcolor(gs12)  || ///
		rspike ls_x lqt_x mean_id_scale if lottery==0 & alpha==0.75 & Type==1, lcolor(gs12)   || ///
		rcap us_x us_x mean_id_scale if lottery==0 & alpha==0.75 & Type==1, lcolor(gs12)  || ///
		rcap ls_x ls_x mean_id_scale if lottery==0 & alpha==0.75 & Type==1, lcolor(gs12)  || ///
		scatter out_x mean_id_scale if  lottery==0 & alpha==0.75 & Type==1, ms(circle) mcolor(blue) mfc(none) msize(0.4) || ///
		scatter mean_x mean_id_scale if lottery==0 & alpha==0.75 & Type==1, legend(off) ms(diamond) mcolor(red) mfc(none) msize(0.4) title("All-Pay - alpha = 0.75", si(medium)) yline(25, lc(blue)) ///
		ytitle("Expenditure")  ylabel(0(25)100) name(typeA_075_subj_apa, replace)
		
twoway 	rbar lqt_x med_x mean_id_scale if lottery==0 & alpha==0.75 & Type==2, fcolor(gs12) lcolor(black)  || ///
		rbar med_x uqt_x mean_id_scale if lottery==0 & alpha==0.75 & Type==2, fcolor(gs12) lcolor(black)  || ///
		rspike uqt_x us_x mean_id_scale if lottery==0 & alpha==0.75 & Type==2, lcolor(gs12)  || ///
		rspike ls_x lqt_x mean_id_scale if lottery==0 & alpha==0.75 & Type==2, lcolor(gs12)   || ///
		rcap us_x us_x mean_id_scale if lottery==0 & alpha==0.75 & Type==2, lcolor(gs12)  || ///
		rcap ls_x ls_x mean_id_scale if lottery==0 & alpha==0.75 & Type==2, lcolor(gs12)  || ///
		scatter out_x mean_id_scale if  lottery==0 & alpha==0.75 & Type==2, ms(circle) mcolor(blue) mfc(none) msize(0.4) || ///
		scatter mean_x mean_id_scale if lottery==0 & alpha==0.75 & Type==2, legend(off) ms(diamond) mcolor(red) mfc(none) msize(0.4) title("All-Pay - alpha = 0.75", si(medium)) yline(25, lc(blue)) ///
		ytitle("Expenditure")  ylabel(0(25)100) name(typeB_075_subj_apa, replace)

*graph combine typeA_025_subj typeA_075_subj typeB_025_subj typeB_075_subj, name(box_subj_apa, replace)
*graph export box_subj_apa.jpg, replace

graph combine typeA_025_subj_apa typeA_025_subj typeA_075_subj_apa typeA_075_subj, name(box_typeA, replace)
graph export box_typeA.pdf, replace

graph combine typeB_025_subj_apa typeB_025_subj typeB_075_subj_apa typeB_075_subj, name(box_typeB, replace)
graph export box_typeB.pdf, replace

*******************
******FIGURE 4*****
*******************
	 
do C:\purdue\var_definitions.do
keep if contestperiod==1
*drop if part==3 & Period<=10
*drop if part==4 & Period<=30

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

sort contest alpha
collapse changehat rationalrev, by(contest alpha)

sort contest
gen low=1 if alpha==0.25
replace low=4 if alpha==0.75
gen low1=2 if alpha==0.25
replace low1=5 if alpha==0.75

label var changehat "Actual revision"
label var rationalrev "Rational revision"

twoway  (bar changehat low, barw(1) bcolor(blue)) ///
(bar rationalrev low1, barw(1) bcolor(gray)), ///
ytitle("Fraction") ylabel(0(0.2)1) xlabel(1.5 "alpha=0.25" 4.5 "alpha=0.75") by(contest, note(""))  ///
saving(C:\purdue\revision, replace)
graph use C:\purdue\revision
graph export C:\purdue\revision.pdf, replace
	  
*******************
******FIGURE 5*****
*******************

do C:\purdue\var_definitions.do

*Period >10 & <=20 & Period >30

keep if contestperiod==1
*drop if part==3 & Period<=10
*drop if part==4 & Period<=30
keep if Type==1
keep if lottery==0

*keep if changehat==1

*keep if validriskloss==1

gen a25=1 if alpha==0.25
replace a25=2 if alpha==0.75
gen alphag=1 if alpha==0.25
replace alphag=2 if alpha==0.75

label define a25 0 " " 1 "alpha=0.25" 2 "alpha=0.75" 3 " "
label val alphag a25
label var alphag "Treatment"


**************************
***(a) All observations***
**************************
label var br1 "Best Response"
label var x12hat "Observations"
twoway (scatter br1 x2hat, legend(off) ms(X) mfc(none) mlc(red) msize(vlarge)) ///
(scatter x12hat x2hat, legend(off) ms(circle) mfc(none) mlc(black) mlw(medium) ytitle("Type A - Round 2")) if lottery==0, ///
by(alphag, note("")) saving("C:\purdue\bra_brall", replace) 
graph use C:\purdue\bra_brall
graph export C:\purdue\bra_brall.pdf, replace

*****************************
***(b) Revised Expenditure***
*****************************
label var br1 "Best Response"
label var x12hat "Observations"
twoway (scatter br1 x2hat if changehat==1, legend(off) ms(X) mfc(none) mlc(red) msize(vlarge)) ///
(scatter x12hat x2hat if changehat==1, legend(off) ms(circle) mfc(none) mlc(black) mlw(medium) ytitle("Type A - Round 2")), ///
by(alphag, note("")) saving("C:\purdue\bra_brch", replace) 
graph use C:\purdue\bra_brch
graph export C:\purdue\bra_brch.pdf, replace
  
*******************
******FIGURE 6*****
*******************

do C:\purdue\var_definitions.do

*keep if changehat==1

*Period >10 & <=20 & Period >30
*drop if part==3 & Period<=10
*drop if part==4 & Period<=30

keep if contestperiod==1
keep if lottery==1
keep if Type==1
sum match2

*gen direction=0
*replace direction=1 if diff_br_1>diff_br_2
*replace direction=1 if  diff_br_1==diff_br_2 & diff_br_1==0

gen outspend2=0
replace outspend2=1 if x12hat>=x2hat
sum outspend2
gen slightoutspend2=0
replace slightoutspend2=1 if x12hat-x2hat>=0.01 
replace slightoutspend2=0 if x12hat-x2hat>5
sum slightoutspend2

gen sqrtx2hat=sqrt(x2hat)
gen x2hatsq=x2hat*x2hat

egen xjowbar=mean(xjow)


*alpha=0.25
xtreg x12hat x2hat sqrtx2hat inv if alpha==0.25, i(subjectid) vce(cluster sessionid)
*reg x12hat x2hat x2hatsq inv if alpha==0.25, vce(cluster subjectid)
est store A
*predict xbr25 if alpha==0.25
gen xbr25=_b[_cons]+_b[x2hat]*x2hat+_b[sqrtx2hat]*sqrtx2hat
*gen xbr25=_b[_cons]+_b[x2hat]*x2hat+_b[x2hatsq]*x2hatsq
*alpha=0.75
xtreg x12hat x2hat sqrtx2hat inv if alpha==0.75,  i(subjectid) vce(cluster sessionid)
*reg x12hat x2hat x2hatsq inv if alpha==0.75, vce(cluster subjectid)
est store B
*predict xbr75 if alpha==0.75
gen xbr75=_b[x2hat]*x2hat+_b[sqrtx2hat]*sqrtx2hat
*gen xbr75=_b[_cons]+_b[x2hat]*x2hat+_b[x2hatsq]*x2hatsq
xtreg x12hat x2hat sqrtx2hat inv if alpha==0.25 & changehat==1,  i(subjectid) vce(cluster sessionid)
*reg x12hat x2hat x2hatsq inv if alpha==0.75, vce(cluster subjectid)
est store C
*predict xbr75 if alpha==0.75
gen xbr25_rev=_b[x2hat]*x2hat+_b[sqrtx2hat]*sqrtx2hat
*gen xbr75=_b[_cons]+_b[x2hat]*x2hat+_b[x2hatsq]*x2hatsq
xtreg x12hat x2hat sqrtx2hat inv if alpha==0.75 & changehat==1,  i(subjectid) vce(cluster sessionid)
*reg x12hat x2hat x2hatsq inv if alpha==0.75, vce(cluster subjectid)
est store D
*predict xbr75 if alpha==0.75
gen xbr75_rev=_b[x2hat]*x2hat+_b[sqrtx2hat]*sqrtx2hat
*gen xbr75=_b[_cons]+_b[x2hat]*x2hat+_b[x2hatsq]*x2hatsq


esttab A B C D using table.tex, ///
se keep(x2hat sqrtx2hat inv _cons) replace f starlevels(* 0.1 ** 0.05 *** 0.01) stats(N r2)
esttab A B C D using table.txt, ///
se keep(x2hat sqrtx2hat inv _cons) replace f starlevels(* 0.1 ** 0.05 *** 0.01) stats(N r2)
*se keep(x2hat  x2hatsq inv _cons) replace f starlevels(* 0.1 ** 0.05 *** 0.01) stats(N r2)

gen a25=1 if alpha==0.25
replace a25=2 if alpha==0.75
gen alphag=1 if alpha==0.25
replace alphag=2 if alpha==0.75

label define a25 0 " " 1 "alpha=0.25" 2 "alpha=0.75" 3 " "
label val alphag a25
label var alphag "Treatment"

label var br1 "Best Response"
label var x12hat "Observations"

sort x2hat
*twoway (scatter x12hat x2hat) (line br1 x2hat) 
*twoway (line xbr25 x2hat) (line xbr75 x2hat) (line br1 x2hat) 
gen x1225=x12hat if alpha==0.25
gen x1275=x12hat if alpha==0.75

**************************
***(a) All observations***
**************************
*****with the cloud of points and estimated BR****************
twoway (line xbr25 x2hat, lwidth(thick) lp(longdash_dot) lc(black)) ///
(line xbr75 x2hat, lwidth(thick) lp(dash) lc(black)) ///
(line br1 x2hat, lwidth(medthick) legend(off) lc(red)) ///
(scatter x12hat x2hat, ms(circle) mfc(none) mlc(black) mlw(medium)), /// *(line xbr25 x2hat, lwidth(thick) lp(longdash_dot)) /// *(line xbr75 x2hat, lwidth(thick) lp(dash)) ///
text(35 80 "{bf:alpha=0.75}", color(blue)) text(52 90 "{bf:alpha=0.25}", color(blue)) text(20 70 "{bf:Risk Neutral BR}", color(blue)) ///
ylabel(0(20)100) ytitle("Type A - Round 2") ///
saving("C:\purdue\brl_brall", replace)
graph use C:\purdue\brl_brall
graph export C:\purdue\brl_brall.pdf, replace

*****************************
***(b) Revised Expenditure***
*****************************
twoway (line xbr25_rev x2hat if changehat==1, lwidth(thick) lp(longdash_dot) lc(black)) ///
(line xbr75_rev x2hat if changehat==1, lwidth(thick) lp(dash) lc(black)) ///
(line br1 x2hat if changehat==1, lwidth(medthick) legend(off) lc(red)) ///
(scatter x12hat x2hat if changehat==1, ms(circle) mfc(none) mlc(black) mlw(medium)), /// *(line xbr25 x2hat, lwidth(thick) lp(longdash_dot)) /// *(line xbr75 x2hat, lwidth(thick) lp(dash)) ///
ylabel(0(20)100) ytitle("Type A - Round 2") ///
text(38 65 "{bf:alpha=0.75}", color(blue)) text(47 90 "{bf:alpha=0.25}", color(blue)) text(20 70 "{bf:Risk Neutral BR}", color(blue)) ///
saving("C:\purdue\brl_brch", replace)
graph use C:\purdue\brl_brch
graph export C:\purdue\brl_brch.pdf, replace
	  
	  
	  
	  
	  
	  
	  
	  
	  