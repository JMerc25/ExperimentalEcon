import delimited "C:\Users\joshu\OneDrive\Documents\GitHub\ExperimentalEcon\Experiemental\experiment.csv", numericcols(6 13 14 15 16 17 18 19 20 21 22 37 38 61) clear

log using GroupReplication.dta, replace

* import delimited "C:\Users\joshu\OneDrive\Documents\Experiemental\experiment.csv", numericcols(6 13 14 15 16 17 18 19 20 21 22 37 38 61) clear 

/*--------------------------------------------------------
P1S1

binary variable:
	Whether or not Player 1 wants to share resources with 
	Player 2, takes values
--------------------------------------------------------
P3S1

binary variable:
	Whether or not Player 3 decides to punish Player 1 for 
	not sharing

--------------------------------------------------------
*/
*Make Variables
gen p2bot = 1 if scenario == "p2bot"
replace p2bot = 0 if p2bot ==.

gen p3bot = 1 if scenario =="p3bot"
replace p3bot = 0 if p3bot ==.

gen p1bot = 1 if scenario == "p1bot"
replace p1bot = 0 if p1bot ==.


*non-logistic regressions
reg p1s1 p2bot
reg p1s1 c.age##p2bot 

reg p3s1 p1bot p2bot
reg p3s1 c.age##p2bot c.age##p1bot 


*logistic Regressions

logit p1s1 p2bot , robust
outreg2 using myreg.doc, replace ctitle(Helper Give)
logit p1s1 c.age##p2bot, robust
outreg2 using myreg.doc, append ctitle(Helper Give)

logit p3s1 p1bot p2bot, robust
outreg2 using myreg.doc, append ctitle(Punisher Punish)
logit p3s1 c.age##p2bot c.age##p1bot, robust 
outreg2 using myreg.doc, append ctitle(Punisher Punish)

*Graphs
binscatter p1s1 age, by(p2bot) xtitle(Age) ytitle(Probability of Giving) line(qfit) savegraph(Figure1.png) replace

binscatter p3s1 age, by(p1bot) xtitle(Age) ytitle(Probability of Giving) line(qfit) savegraph(Figure2.png) replace

binscatter p3s1 age, by(p2bot) xtitle(Age) ytitle(Probability of Giving) line(qfit) savegraph(Figure3.png) replace

binscatter p3s1 age, by(p2bot) absorb(income) xtitle(Age) ytitle(Probability of Giving) line(qfit) savegraph(Figure3.png) replace
/* 

P4H_trust_gain

The amount the Helper would gain if they had shared with
the Beneficiary. This is calculated using P4HS1_Yes
and P4HS1_No

   */

* Not sure what to do with this section- seems like they asked the same people about two hypotheticals in the end, I can't find the results of the trust game


reg p4p_trust_gain c.age##p1bot##p2bot##p3bot
reg p4h_trust_gain c.age##p1bot##p2bot##p3bot

logit p4p_trust_gain c.age##p1bot##p2bot##p3bot
logit p4h_trust_gain c.age##p1bot##p2bot##p3bot

log close