import delimited "C:\Users\joshu\OneDrive\Documents\Experiemental\experiment.csv", numericcols(6 13 14 15 16 17 18 19 20 21 22 37 38 61) clear 


gen p2bot = 1 if scenario == "p2bot"
replace p2bot = 0 if p2bot ==.

reg p1s1 age p2bot
logit p1s1 age p2bot

gen p1bot = 1 if scenario == "p1bot"
replace p1bot = 0 if p1bot ==.

reg p3s1 age p1bot

reg p4p_trust_gain age p1bot p2bot
reg p4h_trust_gain age p1bot p2bot