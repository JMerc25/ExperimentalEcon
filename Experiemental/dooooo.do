import delimited "C:\Users\joshu\OneDrive\Documents\GitHub\ExperimentalEcon\Experiemental\experiment.csv", numericcols(6 13 14 15 16 17 18 19 20 21 22 37 38 61) clear

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

gen p2bot = 1 if scenario == "p2bot"
replace p2bot = 0 if p2bot ==.

gen p3bot = 1 if scenario =="p3bot"
replace p3bot = 0 if p3bot ==.

reg p1s1 p2bot
reg p1s1 c.age##p2bot##p3bot 
logit p1s1 c.age##p2bot##p3bot

gen p1bot = 1 if scenario == "p1bot"
replace p1bot = 0 if p1bot ==.

reg p3s1 p1bot
reg p3s1 c.age##p2bot c.age##p1bot 
logit p3s1 c.age##p2bot c.age##p1bot 


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