import delimited "C:\Users\joshu\OneDrive\Documents\GitHub\ExperimentalEcon\Experiemental\experiment.csv", numericcols(6 13 14 15 16 17 18 19 20 21 22 37 38 61) clear

log using GroupReplication.log, replace


*Replication
** Helper
ttest p1s1, by(scenario) unequal
* p<0.001, identical to what was reported

** Pushiner
gen p3nobot = p3s1 if scenario=="no_bot"
gen p3p1bot = p3s1 if scenario=="p1bot"
gen p3p2bot = p3s1 if scenario=="p2bot"

ttest p3nobot == p3p1bot, unpaired
* not significant (as noted in the paper)*
ttest p3p2bot == p3p1bot, unpaired
*identical and p<0.001
ttest p3nobot == p3p2bot, unpaired
*identical and p<0.001


*Make Dummy Variables 
gen p2bot = 1 if scenario == "p2bot"
replace p2bot = 0 if p2bot ==.

gen p3bot = 1 if scenario =="p3bot"
replace p3bot = 0 if p3bot ==.

gen p1bot = 1 if scenario == "p1bot"
replace p1bot = 0 if p1bot ==.

gen agp1 = age*p1bot
gen agp2 = age*p2bot
*non-logistic regressions
quietly reg p1s1 p2bot
quietly reg p1s1 c.age##p2bot 

quietly reg p3s1 p1bot p2bot
quietly reg p3s1 c.age##p2bot c.age##p1bot 


*logistic Regressions  THESE WERE USED

logit p1s1 p2bot , robust
outreg2 using myreg.doc, replace ctitle(Helper Give)

logit p1s1 age p2bot agp2, robust
outreg2 using myreg.doc, append ctitle(Helper Give)

logit p3s1 p1bot p2bot, robust
outreg2 using myreg.doc, append ctitle(Punisher Punish)
logit p3s1 age p2bot agp2 p1bot agp1, robust 
outreg2 using myreg.doc, append ctitle(Punisher Punish)

*Graphs
binscatter p1s1 age, by(p2bot) xtitle(Age) ytitle(Probability of Giving) title(Figure 1) subtitle(Helper bots v. people) line(qfit) savegraph(Figure1.png) replace

binscatter p3s1 age, by(p1bot) xtitle(Age) ytitle(Probability of Punishing) title(Figure 2) subtitle(Punisher Punishing bots v. people) line(qfit) savegraph(Figure2.png) replace

binscatter p3s1 age, by(p2bot) xtitle(Age) ytitle(Probability of Punishing) title(Figure 3) subtitle(Punisher with Beneficiary bots v people)line(qfit) savegraph(Figure3.png) replace

binscatter p3s1 age, by(scenario) xtitle(Age) ytitle(Probability of Punishing) title(Figure 4) subtitle(Punisher)line(qfit) savegraph(Figure4.png) replace


* Player 4 regressions - not included in discussions
quietly reg p4p_trust_gain c.age##p1bot##p2bot##p3bot
quietly reg p4h_trust_gain c.age##p1bot##p2bot##p3bot

quietly logit p4p_trust_gain c.age##p1bot##p2bot##p3bot
quietly logit p4h_trust_gain c.age##p1bot##p2bot##p3bot



log close