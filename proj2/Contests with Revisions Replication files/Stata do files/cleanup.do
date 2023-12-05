* First copy STATA data sets (Session1-Session8) from folder "Semi Cleaned Up Datasets" into C:\data\

* Second create a folder C:\purdue\ 

* Third run the entire code below from top to bottom

* The complete dataset is C:\purdue\revisionscontestdata.dta

*Make a dataset of the questionnaire answers

clear
use C:\purdue\questionnaire
*session 1
gen sessionid=5 if session==1 & subject<=8
replace sessionid=6 if session==1 & subject>8
*session 2
replace sessionid=15 if session==2 & subject>=17
replace sessionid=14 if session==2 & subject<=16
replace sessionid=13 if session==2 & subject<=8
*session 3
replace sessionid=16 if session==3
*session 4
replace sessionid=1 if session==4 & subject<=8
replace sessionid=2 if session==4 & subject>8
*session 5
replace sessionid=3 if session==5 & subject<=8
replace sessionid=4 if session==5 & subject>8
*session 6
replace sessionid=7 if session==6 & subject<=8
replace sessionid=8 if session==6 & subject>8
*session 7
replace sessionid=9 if session==7 & subject<=8
replace sessionid=10 if session==7 & subject>8
*session 8
replace sessionid=11 if session==8 & subject<=8
replace sessionid=12 if session==8 & subject>8
egen xid=concat(sessionid subject)
gen subjectid=real(xid)
drop xid
save C:\purdue\questionnairev2, replace

keep if question==1
rename anger anger1
rename 	elation	elation1 
rename envy envy1
rename happiness happiness1
rename irritation irritation1
rename regret regret1
rename relief relief1
rename sadness sadness1
save C:\purdue\q1, replace

clear
use C:\purdue\questionnairev2

keep if question==2
rename anger anger2
rename 	elation	elation2
rename envy envy2
rename happiness happiness2
rename irritation irritation2
rename regret regret2
rename relief relief2
rename sadness sadness2
save C:\purdue\q2, replace

clear
use C:\purdue\questionnairev2

keep if question==3
rename anger anger3
rename 	elation	elation3
rename envy envy3
rename happiness happiness3
rename irritation irritation3
rename regret regret3
rename relief relief3
rename sadness sadness3
save C:\purdue\q3, replace


clear
use C:\purdue\questionnairev2

keep if question==4
rename anger anger4
rename 	elation	elation4
rename envy envy4
rename happiness happiness4
rename irritation irritation4
rename regret regret4
rename relief relief4
rename sadness sadness4
save C:\purdue\q4, replace

merge 1:1 subjectid using C:\purdue\q1
drop _merge
merge 1:1 subjectid using C:\purdue\q2
drop _merge
merge 1:1 subjectid using C:\purdue\q3
drop _merge
drop session
drop subject

save C:\purdue\questionnairev3, replace

clear
use C:\data\Session1_Lottery_75_25_Friday_04_08_11_30
*drop TotalProfit Participate RiskLine1 RiskLine2 RiskLine3 RiskLine4 RiskLine5 RiskLine6 RiskLine7 RiskLine8 RiskLine9 RiskLine10 RiskLine11 RiskLine12 RiskLine13 RiskLine14 RiskLine15 RiskLine16 RiskLine17 RiskLine18 RiskLine19 RiskLine20 TimeOKRiskAversionScreenOK LossLine1 LossLine2 LossLine3 LossLine4 LossLine5 LossLine6 LossLine7 LossLine8 LossLine9 LossLine10 LossLine11 LossLine12 LossLine13 LossLine14 LossLine15 LossLine16 LossLine17 LossLine18 LossLine19 LossLine20 TimeOKLossAversionScreenOK NumPeriod NumType alpha1 Rank Group1 Group2 oSub oNumType TimeCONTINUETreatmentChangeOK TimeSubmitRound1ChoicesOK numbersub TimeSubmitRounr1Round2ChoiceOK TimeKeepRound2r1Round2ChoiceOK TimeSubmitGuesr2Round2ChoiceOK TimeTheOtherPar2Round2ChoiceOK onumbersub number winsub Prob TimeOKProfitIfOnlyOneStOK groupchange ProbNoChange ProbChange TimeOKProfitIfTwoStsOK TimeSubmitChoicesOK TimeOKProfitOK XRate ShowUp RandRisk RandRiskLine RandLoss RandLossLine randperiod1 randperiod2 TimeSubmitRounr1Round2ChoiceOK TimeKeepRound2r1Round2ChoiceOK TimeSubmitGuesr2Round2ChoiceOK TimeTheOtherPar2Round2ChoiceOK onumbersub number winsub Prob TimeOKProfitIfOnlyOneStOK groupchange ProbNoChange ProbChange TimeOKProfitIfTwoStsOK TimeSubmitChoicesOK TimeOKProfitOK XRate ShowUp RandRisk RandRiskLine RandLoss RandLossLine randperiod1 randperiod2 PayingConfLine PayingConfToken PayingPeriod1 PayingPeriod2 EarningsPart1 EarningsPart2 EarningsPart3 EarningsPayingPeriod1 EarningsPayingPeriod2 TotalEarnings45 TotalEarnings45USD TotalEarnings1USDPlusEndowment TotalPayoutUSD ConfLine1 ConfLine2 ConfLine3 ConfLine4 ConfLine5 ConfLine6 ConfLine7 ConfLine8 ConfLine9 ConfLine10 ConfLine11 ConfLine12 ConfLine13 ConfLine14 ConfLine15 ConfLine16 ConfLine17 ConfLine18 ConfLine19 ConfLine20 TimeSubmitAnswersEmotions1OK TimeSubmitAnswersEmotions2OK TimeSubmitAnswersEmotions3OK TimeSubmitAnswersEmotions4OK TimeOKQuestionnaireOK PayoffF PayingRiskColor PayingRiskLine PayingLossColor PayingLossLine chosenperiod1 chosenperiod2 TimeOKDrawingsScreenOK TotalEarnings34 TotalEarningsUSD
keep anger session treatment tables Period Subject Group Profit Round stage1 segment Balance Type alpha Prize x otherx YourInvestment OtherInvestment x12 x12guess x12hat x2hat x1hat x1real secondst changehat win payoff1 payoff2 xJoy otherxJoy payoffJoy ProfitJoy  PayoffF
rename treatment part 
replace part=4 if part==3 & Period>=21
replace part=5 if xJoy~=.
drop if anger~=.
drop anger

replace x12=. if x12==-1
replace x12hat=. if x12hat==-1
gen sessionid=5 if Subject<=8
replace sessionid=6 if Subject>8
egen xid=concat(sessionid Subject)
gen subjectid=real(xid)
drop xid
save C:\purdue\Session1_Lottery_75_25_Friday_04_08_11_30, replace
use C:\purdue\Session1_Lottery_75_25_Friday_04_08_11_30

clear
use C:\data\Session2_AllPay_75_25_Friday_04_08_4_30
keep anger session treatment tables Period Subject Group Profit Round stage1 segment Balance Type alpha Prize x otherx YourInvestment OtherInvestment x12 x12guess x12hat x2hat x1hat x1real secondst changehat win payoff1 payoff2 xJoy otherxJoy payoffJoy ProfitJoy               PayoffF
rename treatment part 
replace part=4 if part==3 & Period>=21
replace part=5 if xJoy~=.
drop if anger~=.
drop anger

replace x12=. if x12==-1
replace x12hat=. if x12hat==-1
gen sessionid=15 if Subject>=17
replace sessionid=14 if Subject<=16
replace sessionid=13 if Subject<=8
egen xid=concat(sessionid Subject)
gen subjectid=real(xid)
drop xid
save C:\purdue\Session2_AllPay_75_25_Friday_04_08_4_30, replace
use C:\purdue\Session2_AllPay_75_25_Friday_04_08_4_30

clear
use C:\data\Session3_AllPay_75_25_Saturday_04_09_11_00
keep anger session treatment tables Period Subject Group Profit Round stage1 Balance Type alpha Prize x otherx YourInvestment OtherInvestment x12 x12guess x12hat x2hat x1hat x1real secondst changehat win payoff1 payoff2 xJoy otherxJoy payoffJoy ProfitJoy               PayoffF
rename treatment part 
replace part=4 if part==3 & Period>=21
replace part=5 if xJoy~=.
drop if anger~=.
drop anger

replace x12=. if x12==-1
replace x12hat=. if x12hat==-1
*Period 20 showed and used the wrong alpha - I don't think the subjects noticed at all
replace alpha=0.75 if Period==20
*********************************
gen sessionid=16 if Subject<=8
egen xid=concat(sessionid Subject)
gen subjectid=real(xid)
drop xid
save C:\purdue\Session3_AllPay_75_25_Saturday_04_09_11_00, replace
use C:\purdue\Session3_AllPay_75_25_Saturday_04_09_11_00

clear
use C:\data\Session4_Lottery_25_75_Saturday_04_09_1_15
keep anger session treatment tables Period Subject Group Profit Round stage1 segment Balance Type alpha Prize x otherx YourInvestment OtherInvestment x12 x12guess x12hat x2hat x1hat x1real secondst changehat win payoff1 payoff2 xJoy otherxJoy payoffJoy ProfitJoy               PayoffF
rename treatment part 
replace part=4 if part==3 & Period>=21
replace part=5 if xJoy~=.
drop if anger~=.
drop anger

replace x12=. if x12==-1
replace x12hat=. if x12hat==-1
gen sessionid=1 if Subject<=8
replace sessionid=2 if Subject>8
egen xid=concat(sessionid Subject)
gen subjectid=real(xid)
drop xid
save C:\purdue\Session4_Lottery_25_75_Saturday_04_09_1_15, replace
use C:\purdue\Session4_Lottery_25_75_Saturday_04_09_1_15

clear
use C:\data\Session5_Lottery_25_75_Saturday_04_09_3_30
keep anger session treatment tables Period Subject Group Profit Round stage1 segment Balance Type alpha Prize x otherx YourInvestment OtherInvestment x12 x12guess x12hat x2hat x1hat x1real secondst changehat win payoff1 payoff2 xJoy otherxJoy payoffJoy ProfitJoy               PayoffF
rename treatment part 
replace part=4 if part==3 & Period>=21
replace part=5 if xJoy~=.
drop if anger~=.
drop anger

replace x12=. if x12==-1
replace x12hat=. if x12hat==-1
gen sessionid=3 if Subject<=8
replace sessionid=4 if Subject>8
egen xid=concat(sessionid Subject)
gen subjectid=real(xid)
drop xid
save C:\purdue\Session5_Lottery_25_75_Saturday_04_09_3_30, replace
use C:\purdue\Session5_Lottery_25_75_Saturday_04_09_3_30

clear
use C:\data\Session6_Lottery_75_25_Sunday_04_10_11_30
keep anger session treatment tables Period Subject Group Profit Round stage1 segment Balance Type alpha Prize x otherx YourInvestment OtherInvestment x12 x12guess x12hat x2hat x1hat x1real secondst changehat win payoff1 payoff2 xJoy otherxJoy payoffJoy ProfitJoy               PayoffF
rename treatment part 
replace part=4 if part==3 & Period>=21
replace part=5 if xJoy~=.
drop if anger~=.
drop anger

replace x12=. if x12==-1
replace x12hat=. if x12hat==-1
gen sessionid=7 if Subject<=8
replace sessionid=8 if Subject>8
egen xid=concat(sessionid Subject)
gen subjectid=real(xid)
drop xid
save C:\purdue\Session6_Lottery_75_25_Sunday_04_10_11_30, replace
use C:\purdue\Session6_Lottery_75_25_Sunday_04_10_11_30

clear
use C:\data\Session7_AllPay_25_75_Sunday_04_10_1_45
keep anger session treatment tables Period Subject Group Profit Round stage1 segment Balance Type alpha Prize x otherx YourInvestment OtherInvestment x12 x12guess x12hat x2hat x1hat x1real secondst changehat win payoff1 payoff2 xJoy otherxJoy payoffJoy ProfitJoy               PayoffF
rename treatment part 
replace part=4 if part==3 & Period>=21
replace part=5 if xJoy~=.
drop if anger~=.
drop anger

replace x12=. if x12==-1
replace x12hat=. if x12hat==-1
gen sessionid=9 if Subject<=8
replace sessionid=10 if Subject>8
egen xid=concat(sessionid Subject)
gen subjectid=real(xid)
drop xid
save C:\purdue\Session7_AllPay_25_75_Sunday_04_10_1_45, replace
use C:\purdue\Session7_AllPay_25_75_Sunday_04_10_1_45

clear
use C:\data\Session8_AllPay_25_75_Sunday_04_10_4_00
keep anger session treatment tables Period Subject Group Profit Round stage1 segment Balance Type alpha Prize x otherx YourInvestment OtherInvestment x12 x12guess x12hat x2hat x1hat x1real secondst changehat win payoff1 payoff2 xJoy otherxJoy payoffJoy ProfitJoy               PayoffF
rename treatment part 
replace part=4 if part==3 & Period>=21
replace part=5 if xJoy~=.
drop if anger~=.
drop anger

replace x12=. if x12==-1
replace x12hat=. if x12hat==-1
gen sessionid=11 if Subject<=8
replace sessionid=12 if Subject>8
egen xid=concat(sessionid Subject)
gen subjectid=real(xid)
drop xid
save C:\purdue\Session8_AllPay_25_75_Sunday_04_10_4_00, replace
use C:\purdue\Session8_AllPay_25_75_Sunday_04_10_4_00

*Creates a dataset where rik, loss and joy of winning decisions are not conveniently set up
use C:\purdue\Session1_Lottery_75_25_Friday_04_08_11_30
append using C:\purdue\Session2_AllPay_75_25_Friday_04_08_4_30
save C:\purdue\merge1,replace
append using C:\purdue\Session3_AllPay_75_25_Saturday_04_09_11_00
save C:\purdue\merge2,replace
append using C:\purdue\Session4_Lottery_25_75_Saturday_04_09_1_15
save C:\purdue\merge3,replace
append using C:\purdue\Session5_Lottery_25_75_Saturday_04_09_3_30
save C:\purdue\merge4,replace
append using C:\purdue\Session6_Lottery_75_25_Sunday_04_10_11_30
save C:\purdue\merge5,replace
append using C:\purdue\Session7_AllPay_25_75_Sunday_04_10_1_45
save C:\purdue\merge6,replace
append using C:\purdue\Session8_AllPay_25_75_Sunday_04_10_4_00
save C:\purdue\revisionscontestdata, replace

*Makes a dataset of the joy of winning investments
clear
use C:\purdue\revisionscontestdata
keep if part==5
keep sessionid Subject subjectid xJoy
save C:\purdue\xjoy, replace

*Extracts the risk aversion and loss aversion switching lines
clear
ztree2stata session using C:\160408_1141.xls
keep if treatment==5
keep Subject RiskLineSwitchToB LossLineSwitchToB
gen sessionid=5 if Subject<=8
replace sessionid=6 if Subject>8
save C:\data\session1_session.dta, replace

clear
ztree2stata session using C:\160408_1605.xls
keep if treatment==5
keep if treatment==5
keep Subject RiskLineSwitchToB LossLineSwitchToB
gen sessionid=15 if Subject>=17
replace sessionid=14 if Subject<=16
replace sessionid=13 if Subject<=8
save C:\data\session2_session.dta, replace

clear
ztree2stata session using C:\160409_1020.xls
keep if treatment==5
keep if treatment==5
keep Subject RiskLineSwitchToB LossLineSwitchToB
gen sessionid=16 if Subject<=8
save C:\data\session3_session.dta, replace

clear
ztree2stata session using C:\160409_1253.xls
keep if treatment==5
keep Subject RiskLineSwitchToB LossLineSwitchToB
gen sessionid=1 if Subject<=8
replace sessionid=2 if Subject>8
save C:\data\session4_session.dta, replace

clear
ztree2stata session using C:\160409_1504.xls
keep if treatment==5
keep Subject RiskLineSwitchToB LossLineSwitchToB
gen sessionid=3 if Subject<=8
replace sessionid=4 if Subject>8
save C:\data\session5_session.dta, replace

clear
ztree2stata session using C:\160410_1034.xls
keep if treatment==5
keep Subject RiskLineSwitchToB LossLineSwitchToB
gen sessionid=7 if Subject<=8
replace sessionid=8 if Subject>8
save C:\data\session6_session.dta, replace

clear
ztree2stata session using C:\160410_1337.xls
keep if treatment==5
keep Subject RiskLineSwitchToB LossLineSwitchToB
gen sessionid=9 if Subject<=8
replace sessionid=10 if Subject>8
save C:\data\session7_session.dta, replace

clear
ztree2stata session using C:\160410_1549.xls
keep if treatment==5
keep Subject RiskLineSwitchToB LossLineSwitchToB
gen sessionid=11 if Subject<=8
replace sessionid=12 if Subject>8
save C:\data\session8_session.dta, replace

clear
use  C:\data\session1_session.dta
append using  C:\data\session2_session.dta
save C:\purdue\merge1,replace
append using C:\data\session3_session.dta
save C:\purdue\merge2,replace
append using C:\data\session4_session.dta
save C:\purdue\merge3,replace
append using C:\data\session5_session.dta
save C:\purdue\merge4,replace
append using C:\data\session6_session.dta
save C:\purdue\merge5,replace
append using  C:\data\session7_session.dta
save C:\purdue\merge6,replace
append using  C:\data\session8_session.dta
egen xid=concat(sessionid Subject)
gen subjectid=real(xid)
drop xid
save C:\purdue\risklossaversion, replace


*Makes the final dataset, which includes risk and loss aversion switching and joy of winning investment
merge 1:1 subjectid using C:\purdue\xjoy
drop _merge
rename xJoy xjow
save C:\purdue\risklossaversionjoy, replace

*Add the variables to the complete dataset
clear
use C:\purdue\revisionscontestdata
merge m:1 subjectid using C:\purdue\risklossaversionjoy
drop _merge
merge m:1 subjectid using C:\purdue\questionnairev3
drop _merge
save C:\purdue\revisionscontestdata, replace


