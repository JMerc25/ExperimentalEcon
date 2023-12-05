* Calculating expected payoffs based on actual majority signal and plurality vote

* Signals:
sort session_id period group
egen sum_signals = sum(state_info), by (session_id period group)
//remember: state_info = 1 if X, state_info_rep = 2 if Y 
// -> sum_signal = 3 if XXX, ..., sum_signal = 6 if YYY
gen signal_XXX = 0
replace signal_XXX = 1 if sum_signals == 3
gen signal_XXY = 0
replace signal_XXY = 1 if sum_signals == 4
gen signal_YYX = 0
replace signal_YYX = 1 if sum_signals == 5
gen signal_YYY = 0
replace signal_YYY = 1 if sum_signals == 6
drop sum_signals

*conditional probabilities
gen p_X_XXX = (.7*.7*.7)/((.7*.7*.7)+(.3*.3*.3))
gen p_Y_XXX = 1-((.7*.7*.7)/((.7*.7*.7)+(.3*.3*.3)))
gen p_X_XXY = .7 	// = (.7*.7*.3) / ((.7*.7*.3)+(.3*.3*.7))
gen p_Y_XXY = 1-.7	// = (.3*.3*.7) / ((.3*.3*.7)+(.7*.7*.3))
gen p_Y_YYY = (.7*.7*.7)/((.7*.7*.7)+(.3*.3*.3))
gen p_X_YYY = 1-((.7*.7*.7)/((.7*.7*.7)+(.3*.3*.3)))
gen p_Y_YYX = .7
gen p_X_YYX = 1-.7

** Per round
gen exp_payoff_round_sig = .
label var exp_payoff_round_sig "Actual Expected earnings per round"
***independent of color group
*if XXX & plurality vote is A
replace exp_payoff_round_sig = p_X_XXX*20 + p_Y_XXX*10 if signal_XXX & outc_pluralityA
*if XXX & plurality vote is B
replace exp_payoff_round_sig = p_X_XXX*0  + p_Y_XXX*20 if signal_XXX & outc_pluralityB
*if XXX & plurality vote is C
replace exp_payoff_round_sig = p_X_XXX*0  + p_Y_XXX*0  if signal_XXX & outc_pluralityC
*if XXX & tie between A & B
replace exp_payoff_round_sig = .5*(p_X_XXX*20 + p_Y_XXX*10) + .5*(p_X_XXX*0  + p_Y_XXX*20) if signal_XXX & outc_pluralityAB
*if XXX & tie between A & C
replace exp_payoff_round_sig = .5*(p_X_XXX*20 + p_Y_XXX*10) + .5*(p_X_XXX*0  + p_Y_XXX*0)  if signal_XXX & outc_pluralityAC
*if XXX & tie between B & C
replace exp_payoff_round_sig = .5*(p_X_XXX*0 + p_Y_XXX*20) + .5*(p_X_XXX*0  + p_Y_XXX*0)   if signal_XXX & outc_pluralityBC
*if XXX & tie between A, B & C
replace exp_payoff_round_sig = ((p_X_XXX*20 + p_Y_XXX*10) + (p_X_XXX*0  + p_Y_XXX*20) + (p_X_XXX*0  + p_Y_XXX*0))/3 if signal_XXX & outc_pluralityABC

*if XXY & plurality vote is A
replace exp_payoff_round_sig = p_X_XXY*20 + p_Y_XXY*10 if signal_XXY & outc_pluralityA
*if XXY & plurality vote is B
replace exp_payoff_round_sig = p_X_XXY*0  + p_Y_XXY*20 if signal_XXY & outc_pluralityB
*if XXY & plurality vote is C
replace exp_payoff_round_sig = p_X_XXY*0  + p_Y_XXY*0  if signal_XXY & outc_pluralityC
*if XXY & tie between A & B
replace exp_payoff_round_sig = .5*(p_X_XXY*20 + p_Y_XXY*10) + .5*(p_X_XXY*0  + p_Y_XXY*20) if signal_XXY & outc_pluralityAB
*if XXY & tie between A & C
replace exp_payoff_round_sig = .5*(p_X_XXY*20 + p_Y_XXY*10) + .5*(p_X_XXY*0  + p_Y_XXY*0)  if signal_XXY & outc_pluralityAC
*if XXY & tie between B & C
replace exp_payoff_round_sig = .5*(p_X_XXY*0 + p_Y_XXY*20) + .5*(p_X_XXY*0  + p_Y_XXY*0)   if signal_XXY & outc_pluralityBC
*if XXY & tie between A, B & C
replace exp_payoff_round_sig = ((p_X_XXY*20 + p_Y_XXY*10) + (p_X_XXY*0  + p_Y_XXY*20) + (p_X_XXY*0  + p_Y_XXY*0))/3 if signal_XXY & outc_pluralityABC

*if YYX & plurality vote is A
replace exp_payoff_round_sig = p_X_YYX*20 + p_Y_YYX*10 if signal_YYX & outc_pluralityA
*if YYX & plurality vote is B
replace exp_payoff_round_sig = p_X_YYX*0  + p_Y_YYX*20 if signal_YYX & outc_pluralityB
*if YYX & plurality vote is C
replace exp_payoff_round_sig = p_X_YYX*0  + p_Y_YYX*0  if signal_YYX & outc_pluralityC
*if YYX & tie between A & B
replace exp_payoff_round_sig = .5*(p_X_YYX*20 + p_Y_YYX*10) + .5*(p_X_YYX*0  + p_Y_YYX*20) if signal_YYX & outc_pluralityAB
*if YYX & tie between A & C
replace exp_payoff_round_sig = .5*(p_X_YYX*20 + p_Y_YYX*10) + .5*(p_X_YYX*0  + p_Y_YYX*0)  if signal_YYX & outc_pluralityAC
*if YYX & tie between B & C
replace exp_payoff_round_sig = .5*(p_X_YYX*0 + p_Y_YYX*20) + .5*(p_X_YYX*0  + p_Y_YYX*0)   if signal_YYX & outc_pluralityBC
*if YYX & tie between A, B & C
replace exp_payoff_round_sig = ((p_X_YYX*20 + p_Y_YYX*10) + (p_X_YYX*0  + p_Y_YYX*20) + (p_X_YYX*0  + p_Y_YYX*0))/3 if signal_YYX & outc_pluralityABC

*if YYY & plurality vote is A
replace exp_payoff_round_sig = p_X_YYY*20 + p_Y_YYY*10 if signal_YYY & outc_pluralityA
*if YYY & plurality vote is B
replace exp_payoff_round_sig = p_X_YYY*0  + p_Y_YYY*20 if signal_YYY & outc_pluralityB
*if YYY & plurality vote is C
replace exp_payoff_round_sig = p_X_YYY*0  + p_Y_YYY*0  if signal_YYY & outc_pluralityC
*if YYY & tie between A & B
replace exp_payoff_round_sig = .5*(p_X_YYY*20 + p_Y_YYY*10) + .5*(p_X_YYY*0  + p_Y_YYY*20) if signal_YYY & outc_pluralityAB
*if YYY & tie between A & C
replace exp_payoff_round_sig = .5*(p_X_YYY*20 + p_Y_YYY*10) + .5*(p_X_YYY*0  + p_Y_YYY*0)  if signal_YYY & outc_pluralityAC
*if YYY & tie between B & C
replace exp_payoff_round_sig = .5*(p_X_YYY*0 + p_Y_YYY*20) + .5*(p_X_YYY*0  + p_Y_YYY*0)   if signal_YYY & outc_pluralityBC
*if YYY & tie between A, B & C
replace exp_payoff_round_sig = ((p_X_YYY*20 + p_Y_YYY*10) + (p_X_YYY*0  + p_Y_YYY*20) + (p_X_YYY*0  + p_Y_YYY*0))/3 if signal_YYY & outc_pluralityABC


***for whites
*if XXX & plurality vote is A
replace exp_payoff_round_sig = p_X_XXX*20 + p_Y_XXX*10 if signal_XXX & outc_pluralityA & informed_type
*if XXX & plurality vote is B
replace exp_payoff_round_sig = p_X_XXX*0  + p_Y_XXX*20 if signal_XXX & outc_pluralityB & informed_type
*if XXX & plurality vote is C
replace exp_payoff_round_sig = p_X_XXX*0  + p_Y_XXX*0  if signal_XXX & outc_pluralityC & informed_type
*if XXX & tie between A & B
replace exp_payoff_round_sig = .5*(p_X_XXX*20 + p_Y_XXX*10) + .5*(p_X_XXX*0  + p_Y_XXX*20) if signal_XXX & outc_pluralityAB & informed_type
*if XXX & tie between A & C
replace exp_payoff_round_sig = .5*(p_X_XXX*20 + p_Y_XXX*10) + .5*(p_X_XXX*0  + p_Y_XXX*0)  if signal_XXX & outc_pluralityAC & informed_type
*if XXX & tie between B & C
replace exp_payoff_round_sig = .5*(p_X_XXX*0 + p_Y_XXX*20) + .5*(p_X_XXX*0  + p_Y_XXX*0)   if signal_XXX & outc_pluralityBC & informed_type
*if XXX & tie between A, B & C
replace exp_payoff_round_sig = ((p_X_XXX*20 + p_Y_XXX*10) + (p_X_XXX*0  + p_Y_XXX*20) + (p_X_XXX*0  + p_Y_XXX*0))/3 if signal_XXX & outc_pluralityABC & informed_type

*if XXY & plurality vote is A
replace exp_payoff_round_sig = p_X_XXY*20 + p_Y_XXY*10 if signal_XXY & outc_pluralityA & informed_type
*if XXY & plurality vote is B
replace exp_payoff_round_sig = p_X_XXY*0  + p_Y_XXY*20 if signal_XXY & outc_pluralityB & informed_type
*if XXY & plurality vote is C
replace exp_payoff_round_sig = p_X_XXY*0  + p_Y_XXY*0  if signal_XXY & outc_pluralityC & informed_type
*if XXY & tie between A & B
replace exp_payoff_round_sig = .5*(p_X_XXY*20 + p_Y_XXY*10) + .5*(p_X_XXY*0  + p_Y_XXY*20) if signal_XXY & outc_pluralityAB & informed_type
*if XXY & tie between A & C
replace exp_payoff_round_sig = .5*(p_X_XXY*20 + p_Y_XXY*10) + .5*(p_X_XXY*0  + p_Y_XXY*0)  if signal_XXY & outc_pluralityAC & informed_type
*if XXY & tie between B & C
replace exp_payoff_round_sig = .5*(p_X_XXY*0 + p_Y_XXY*20) + .5*(p_X_XXY*0  + p_Y_XXY*0)   if signal_XXY & outc_pluralityBC & informed_type
*if XXY & tie between A, B & C
replace exp_payoff_round_sig = ((p_X_XXY*20 + p_Y_XXY*10) + (p_X_XXY*0  + p_Y_XXY*20) + (p_X_XXY*0  + p_Y_XXY*0))/3 if signal_XXY & outc_pluralityABC & informed_type

*if YYX & plurality vote is A
replace exp_payoff_round_sig = p_X_YYX*20 + p_Y_YYX*10 if signal_YYX & outc_pluralityA & informed_type
*if YYX & plurality vote is B
replace exp_payoff_round_sig = p_X_YYX*0  + p_Y_YYX*20 if signal_YYX & outc_pluralityB & informed_type
*if YYX & plurality vote is C
replace exp_payoff_round_sig = p_X_YYX*0  + p_Y_YYX*0  if signal_YYX & outc_pluralityC & informed_type
*if YYX & tie between A & B
replace exp_payoff_round_sig = .5*(p_X_YYX*20 + p_Y_YYX*10) + .5*(p_X_YYX*0  + p_Y_YYX*20) if signal_YYX & outc_pluralityAB & informed_type
*if YYX & tie between A & C
replace exp_payoff_round_sig = .5*(p_X_YYX*20 + p_Y_YYX*10) + .5*(p_X_YYX*0  + p_Y_YYX*0)  if signal_YYX & outc_pluralityAC & informed_type
*if YYX & tie between B & C
replace exp_payoff_round_sig = .5*(p_X_YYX*0 + p_Y_YYX*20) + .5*(p_X_YYX*0  + p_Y_YYX*0)   if signal_YYX & outc_pluralityBC & informed_type
*if YYX & tie between A, B & C
replace exp_payoff_round_sig = ((p_X_YYX*20 + p_Y_YYX*10) + (p_X_YYX*0  + p_Y_YYX*20) + (p_X_YYX*0  + p_Y_YYX*0))/3 if signal_YYX & outc_pluralityABC & informed_type

*if YYY & plurality vote is A
replace exp_payoff_round_sig = p_X_YYY*20 + p_Y_YYY*10 if signal_YYY & outc_pluralityA & informed_type
*if YYY & plurality vote is B
replace exp_payoff_round_sig = p_X_YYY*0  + p_Y_YYY*20 if signal_YYY & outc_pluralityB & informed_type
*if YYY & plurality vote is C
replace exp_payoff_round_sig = p_X_YYY*0  + p_Y_YYY*0  if signal_YYY & outc_pluralityC & informed_type
*if YYY & tie between A & B
replace exp_payoff_round_sig = .5*(p_X_YYY*20 + p_Y_YYY*10) + .5*(p_X_YYY*0  + p_Y_YYY*20) if signal_YYY & outc_pluralityAB & informed_type
*if YYY & tie between A & C
replace exp_payoff_round_sig = .5*(p_X_YYY*20 + p_Y_YYY*10) + .5*(p_X_YYY*0  + p_Y_YYY*0)  if signal_YYY & outc_pluralityAC & informed_type
*if YYY & tie between B & C
replace exp_payoff_round_sig = .5*(p_X_YYY*0 + p_Y_YYY*20) + .5*(p_X_YYY*0  + p_Y_YYY*0)   if signal_YYY & outc_pluralityBC & informed_type
*if YYY & tie between A, B & C
replace exp_payoff_round_sig = ((p_X_YYY*20 + p_Y_YYY*10) + (p_X_YYY*0  + p_Y_YYY*20) + (p_X_YYY*0  + p_Y_YYY*0))/3 if signal_YYY & outc_pluralityABC & informed_type


***for blues
*if XXX & plurality vote is A
replace exp_payoff_round_sig = p_X_XXX*20 + p_Y_XXX*0   if signal_XXX & outc_pluralityA & !informed_type
*if XXX & plurality vote is B
replace exp_payoff_round_sig = p_X_XXX*0  + p_Y_XXX*10  if signal_XXX & outc_pluralityB & !informed_type
*if XXX & plurality vote is C
replace exp_payoff_round_sig = p_X_XXX*10  + p_Y_XXX*20 if signal_XXX & outc_pluralityC & !informed_type
*if XXX & tie between A & B
replace exp_payoff_round_sig = .5*(p_X_XXX*20 + p_Y_XXX*0) + .5*(p_X_XXX*0  + p_Y_XXX*10) if signal_XXX & outc_pluralityAB & !informed_type
*if XXX & tie between A & C
replace exp_payoff_round_sig = .5*(p_X_XXX*20 + p_Y_XXX*0) + .5*(p_X_XXX*10  + p_Y_XXX*20)  if signal_XXX & outc_pluralityAC & !informed_type
*if XXX & tie between B & C
replace exp_payoff_round_sig = .5*(p_X_XXX*0 + p_Y_XXX*10) + .5*(p_X_XXX*10  + p_Y_XXX*20)   if signal_XXX & outc_pluralityBC & !informed_type
*if XXX & tie between A, B & C
replace exp_payoff_round_sig = ((p_X_XXX*20 + p_Y_XXX*0) + (p_X_XXX*0  + p_Y_XXX*10) + (p_X_XXX*10  + p_Y_XXX*20))/3 if signal_XXX & outc_pluralityABC & !informed_type

*if XXY & plurality vote is A
replace exp_payoff_round_sig = p_X_XXY*20 + p_Y_XXY*0 if signal_XXY & outc_pluralityA & !informed_type
*if XXY & plurality vote is B
replace exp_payoff_round_sig = p_X_XXY*0  + p_Y_XXY*10 if signal_XXY & outc_pluralityB & !informed_type
*if XXY & plurality vote is C
replace exp_payoff_round_sig = p_X_XXY*10  + p_Y_XXY*20  if signal_XXY & outc_pluralityC & !informed_type
*if XXY & tie between A & B
replace exp_payoff_round_sig = .5*(p_X_XXY*20 + p_Y_XXY*0) + .5*(p_X_XXY*0  + p_Y_XXY*10) if signal_XXY & outc_pluralityAB & !informed_type
*if XXY & tie between A & C
replace exp_payoff_round_sig = .5*(p_X_XXY*20 + p_Y_XXY*0) + .5*(p_X_XXY*10  + p_Y_XXY*20)  if signal_XXY & outc_pluralityAC & !informed_type
*if XXY & tie between B & C
replace exp_payoff_round_sig = .5*(p_X_XXY*0 + p_Y_XXY*10) + .5*(p_X_XXY*10  + p_Y_XXY*20)   if signal_XXY & outc_pluralityBC & !informed_type
*if XXY & tie between A, B & C
replace exp_payoff_round_sig = ((p_X_XXY*20 + p_Y_XXY*0) + (p_X_XXY*0  + p_Y_XXY*10) + (p_X_XXY*10  + p_Y_XXY*20))/3 if signal_XXY & outc_pluralityABC & !informed_type

*if YYX & plurality vote is A
replace exp_payoff_round_sig = p_X_YYX*20 + p_Y_YYX*0 if signal_YYX & outc_pluralityA & !informed_type
*if YYX & plurality vote is B
replace exp_payoff_round_sig = p_X_YYX*0  + p_Y_YYX*10 if signal_YYX & outc_pluralityB & !informed_type
*if YYX & plurality vote is C
replace exp_payoff_round_sig = p_X_YYX*10  + p_Y_YYX*20  if signal_YYX & outc_pluralityC & !informed_type
*if YYX & tie between A & B
replace exp_payoff_round_sig = .5*(p_X_YYX*20 + p_Y_YYX*0) + .5*(p_X_YYX*0  + p_Y_YYX*10) if signal_YYX & outc_pluralityAB & !informed_type
*if YYX & tie between A & C
replace exp_payoff_round_sig = .5*(p_X_YYX*20 + p_Y_YYX*0) + .5*(p_X_YYX*10  + p_Y_YYX*20)  if signal_YYX & outc_pluralityAC & !informed_type
*if YYX & tie between B & C
replace exp_payoff_round_sig = .5*(p_X_YYX*0 + p_Y_YYX*10) + .5*(p_X_YYX*10  + p_Y_YYX*20)   if signal_YYX & outc_pluralityBC & !informed_type
*if YYX & tie between A, B & C
replace exp_payoff_round_sig = ((p_X_YYX*20 + p_Y_YYX*0) + (p_X_YYX*0  + p_Y_YYX*10) + (p_X_YYX*10  + p_Y_YYX*20))/3 if signal_YYX & outc_pluralityABC & !informed_type

*if YYY & plurality vote is A
replace exp_payoff_round_sig = p_X_YYY*20 + p_Y_YYY*0 if signal_YYY & outc_pluralityA & !informed_type
*if YYY & plurality vote is B
replace exp_payoff_round_sig = p_X_YYY*0  + p_Y_YYY*10 if signal_YYY & outc_pluralityB & !informed_type
*if YYY & plurality vote is C
replace exp_payoff_round_sig = p_X_YYY*10  + p_Y_YYY*20  if signal_YYY & outc_pluralityC & !informed_type
*if YYY & tie between A & B
replace exp_payoff_round_sig = .5*(p_X_YYY*20 + p_Y_YYY*0) + .5*(p_X_YYY*0  + p_Y_YYY*10) if signal_YYY & outc_pluralityAB & !informed_type
*if YYY & tie between A & C
replace exp_payoff_round_sig = .5*(p_X_YYY*20 + p_Y_YYY*0) + .5*(p_X_YYY*10  + p_Y_YYY*20)  if signal_YYY & outc_pluralityAC & !informed_type
*if YYY & tie between B & C
replace exp_payoff_round_sig = .5*(p_X_YYY*0 + p_Y_YYY*10) + .5*(p_X_YYY*10  + p_Y_YYY*20)   if signal_YYY & outc_pluralityBC & !informed_type
*if YYY & tie between A, B & C
replace exp_payoff_round_sig = ((p_X_YYY*20 + p_Y_YYY*0) + (p_X_YYY*0  + p_Y_YYY*10) + (p_X_YYY*10  + p_Y_YYY*20))/3 if signal_YYY & outc_pluralityABC & !informed_type


*************************************************************
*********** Expected earnings if A is implemented ***********
*************************************************************

* Calculating expected payoffs based on received majority signal 

** Per round
gen exp_payoff_a_impl = .
label var exp_payoff_a_impl "Exp. earnings if A  is implemented"
***independent of color group
*if XXX & all vote for A
replace exp_payoff_a_impl = p_X_XXX*20 + p_Y_XXX*10 if signal_XXX 
*if XXY & all vote for A
replace exp_payoff_a_impl = p_X_XXY*20 + p_Y_XXY*10 if signal_XXY 
*if YYX & all vote for A
replace exp_payoff_a_impl = p_X_YYX*20 + p_Y_YYX*10 if signal_YYX 
*if YYY & all vote for A
replace exp_payoff_a_impl = p_X_YYY*20 + p_Y_YYY*10 if signal_YYY 

***for whites
*if XXX & all vote for A
replace exp_payoff_a_impl = p_X_XXX*20 + p_Y_XXX*10 if signal_XXX & informed_type
*if XXY & all vote for A
replace exp_payoff_a_impl = p_X_XXY*20 + p_Y_XXY*10 if signal_XXY & informed_type
*if YYX & all vote for A
replace exp_payoff_a_impl = p_X_YYX*20 + p_Y_YYX*10 if signal_YYX & informed_type
*if YYY & all vote for A
replace exp_payoff_a_impl = p_X_YYY*20 + p_Y_YYY*10 if signal_YYY & informed_type

***for blues
*if XXX & all vote for A
replace exp_payoff_a_impl = p_X_XXX*20 + p_Y_XXX*0 if signal_XXX & !informed_type
*if XXY & all vote for A
replace exp_payoff_a_impl = p_X_XXY*20 + p_Y_XXY*0 if signal_XXY & !informed_type
*if YYX & all vote for A
replace exp_payoff_a_impl = p_X_YYX*20 + p_Y_YYX*0 if signal_YYX & !informed_type
*if YYY & all vote for A
replace exp_payoff_a_impl = p_X_YYY*20 + p_Y_YYY*0 if signal_YYY & !informed_type


*************************************************************
*********** Expected earnings if B is implemented ***********
*************************************************************

* Calculating expected payoffs based on received majority signal

** Per round
gen exp_payoff_b_impl = .
label var exp_payoff_b_impl "Exp. earnings if B is implemented"
***independent of color group
*if XXX & all vote for B
replace exp_payoff_b_impl = p_X_XXX*0 + p_Y_XXX*20 if signal_XXX
*if XXY & all vote for B
replace exp_payoff_b_impl = p_X_XXY*0 + p_Y_XXY*20 if signal_XXY
*if YYX & all vote for B
replace exp_payoff_b_impl = p_X_YYX*0 + p_Y_YYX*20 if signal_YYX
*if YYY & all vote for B
replace exp_payoff_b_impl = p_X_YYY*0 + p_Y_YYY*20 if signal_YYY

***for whites
*if XXX & all vote for B
replace exp_payoff_b_impl = p_X_XXX*0 + p_Y_XXX*20 if signal_XXX & informed_type
*if XXY & all vote for B
replace exp_payoff_b_impl = p_X_XXY*0 + p_Y_XXY*20 if signal_XXY & informed_type
*if YYX & all vote for B
replace exp_payoff_b_impl = p_X_YYX*0 + p_Y_YYX*20 if signal_YYX & informed_type
*if YYY & all vote for B
replace exp_payoff_b_impl = p_X_YYY*0 + p_Y_YYY*20 if signal_YYY & informed_type

***for blues
*if XXX & all vote for B
replace exp_payoff_b_impl = p_X_XXX*0 + p_Y_XXX*10 if signal_XXX & !informed_type
*if XXY & all vote for B
replace exp_payoff_b_impl = p_X_XXY*0 + p_Y_XXY*10 if signal_XXY & !informed_type
*if YYX & all vote for B
replace exp_payoff_b_impl = p_X_YYX*0 + p_Y_YYX*10 if signal_YYX & !informed_type
*if YYY & all vote for B
replace exp_payoff_b_impl = p_X_YYY*0 + p_Y_YYY*10 if signal_YYY & !informed_type


*************************************************************
*********** Expected earnings if C is impelmented ***********
*************************************************************

* Calculating expected payoffs based on received majority signal

** Per round
gen exp_payoff_c_impl = .
label var exp_payoff_c_impl "Exp. earnings if C is implemented"
***independent of color group
*if XXX & all vote for C
replace exp_payoff_c_impl = p_X_XXX*0 + p_Y_XXX*0 if signal_XXX
*if XXY & all vote for C
replace exp_payoff_c_impl = p_X_XXY*0 + p_Y_XXY*0 if signal_XXY
*if YYX & all vote for C
replace exp_payoff_c_impl = p_X_YYX*0 + p_Y_YYX*0 if signal_YYX
*if YYY & all vote for C
replace exp_payoff_c_impl = p_X_YYY*0 + p_Y_YYY*0 if signal_YYY

***for whites
*if XXX & all vote for C
replace exp_payoff_c_impl = p_X_XXX*0 + p_Y_XXX*0 if signal_XXX & informed_type
*if XXY & all vote for C
replace exp_payoff_c_impl = p_X_XXY*0 + p_Y_XXY*0 if signal_XXY & informed_type
*if YYX & all vote for C
replace exp_payoff_c_impl = p_X_YYX*0 + p_Y_YYX*0 if signal_YYX & informed_type
*if YYY & all vote for C
replace exp_payoff_c_impl = p_X_YYY*0 + p_Y_YYY*0 if signal_YYY & informed_type

***for blues
*if XXX & all vote for C
replace exp_payoff_c_impl = p_X_XXX*10 + p_Y_XXX*20 if signal_XXX & !informed_type
*if XXY & all vote for C
replace exp_payoff_c_impl = p_X_XXY*10 + p_Y_XXY*20 if signal_XXY & !informed_type
*if YYX & all vote for C
replace exp_payoff_c_impl = p_X_YYX*10 + p_Y_YYX*20 if signal_YYX & !informed_type
*if YYY & all vote for C
replace exp_payoff_c_impl = p_X_YYY*10 + p_Y_YYY*20 if signal_YYY & !informed_type


***************************************************************************
*********** Expected earnings if the AC outcome is implemented ***********
***************************************************************************

* Calculating expected payoffs based on received majority signal, 
* assuming that all white players voted for A, all blue players for C

** Per round
gen exp_payoff_AC_outc = .
label var exp_payoff_AC_outc "Exp. earnings in the AC outcome"
***independent of color group
*if XXX & tie between A & C
replace exp_payoff_AC_outc = .5*(p_X_XXX*20 + p_Y_XXX*10) + .5*(p_X_XXX*0  + p_Y_XXX*0)  if signal_XXX
*if XXY & tie between A & C
replace exp_payoff_AC_outc = .5*(p_X_XXY*20 + p_Y_XXY*10) + .5*(p_X_XXY*0  + p_Y_XXY*0)  if signal_XXY
*if YYX & tie between A & C
replace exp_payoff_AC_outc = .5*(p_X_YYX*20 + p_Y_YYX*10) + .5*(p_X_YYX*0  + p_Y_YYX*0)  if signal_YYX
*if YYY & tie between A & C
replace exp_payoff_AC_outc = .5*(p_X_YYY*20 + p_Y_YYY*10) + .5*(p_X_YYY*0  + p_Y_YYY*0)  if signal_YYY

***for whites
*if XXX & tie between A & C
replace exp_payoff_AC_outc = .5*(p_X_XXX*20 + p_Y_XXX*10) + .5*(p_X_XXX*0  + p_Y_XXX*0)  if signal_XXX & informed_type
*if XXY & tie between A & C
replace exp_payoff_AC_outc = .5*(p_X_XXY*20 + p_Y_XXY*10) + .5*(p_X_XXY*0  + p_Y_XXY*0)  if signal_XXY & informed_type
*if YYX & tie between A & C
replace exp_payoff_AC_outc = .5*(p_X_YYX*20 + p_Y_YYX*10) + .5*(p_X_YYX*0  + p_Y_YYX*0)  if signal_YYX & informed_type
*if YYY & tie between A & C
replace exp_payoff_AC_outc = .5*(p_X_YYY*20 + p_Y_YYY*10) + .5*(p_X_YYY*0  + p_Y_YYY*0)  if signal_YYY & informed_type

***for blues
*if XXX & tie between A & C
replace exp_payoff_AC_outc = .5*(p_X_XXX*20 + p_Y_XXX*0) + .5*(p_X_XXX*10  + p_Y_XXX*20)  if signal_XXX & !informed_type
*if XXY & tie between A & C
replace exp_payoff_AC_outc = .5*(p_X_XXY*20 + p_Y_XXY*0) + .5*(p_X_XXY*10  + p_Y_XXY*20)  if signal_XXY & !informed_type
*if YYX & tie between A & C
replace exp_payoff_AC_outc = .5*(p_X_YYX*20 + p_Y_YYX*0) + .5*(p_X_YYX*10  + p_Y_YYX*20)  if signal_YYX & !informed_type
*if YYY & tie between A & C
replace exp_payoff_AC_outc = .5*(p_X_YYY*20 + p_Y_YYY*0) + .5*(p_X_YYY*10  + p_Y_YYY*20)  if signal_YYY & !informed_type



*****************************************************************
****** Expected earnings if the BC outcome is implemented ******
*****************************************************************

* Calculating expected payoffs based on received majority signal, 
* assuming that all white players voted for B, all blue players for C

** Per round
gen exp_payoff_BC_outc = .
label var exp_payoff_BC_outc "Exp. earnings in the BC outcome"
***independent of color group
*if XXX & tie between B & C
replace exp_payoff_BC_outc = .5*(p_X_XXX*0 + p_Y_XXX*20) + .5*(p_X_XXX*0  + p_Y_XXX*0)  if signal_XXX
*if XXY & tie between B & C
replace exp_payoff_BC_outc = .5*(p_X_XXY*0 + p_Y_XXY*20) + .5*(p_X_XXY*0  + p_Y_XXY*0)  if signal_XXY
*if YYX & tie between B & C
replace exp_payoff_BC_outc = .5*(p_X_YYX*0 + p_Y_YYX*20) + .5*(p_X_YYX*0  + p_Y_YYX*0)  if signal_YYX
*if YYY & tie between B & C
replace exp_payoff_BC_outc = .5*(p_X_YYY*0 + p_Y_YYY*20) + .5*(p_X_YYY*0  + p_Y_YYY*0)  if signal_YYY

***for whites
*if XXX & tie between B & C
replace exp_payoff_BC_outc = .5*(p_X_XXX*0 + p_Y_XXX*20) + .5*(p_X_XXX*0  + p_Y_XXX*0)  if signal_XXX & informed_type
*if XXY & tie between B & C
replace exp_payoff_BC_outc = .5*(p_X_XXY*0 + p_Y_XXY*20) + .5*(p_X_XXY*0  + p_Y_XXY*0)  if signal_XXY & informed_type
*if YYX & tie between B & C
replace exp_payoff_BC_outc = .5*(p_X_YYX*0 + p_Y_YYX*20) + .5*(p_X_YYX*0  + p_Y_YYX*0)  if signal_YYX & informed_type
*if YYY & tie between B & C
replace exp_payoff_BC_outc = .5*(p_X_YYY*0 + p_Y_YYY*20) + .5*(p_X_YYY*0  + p_Y_YYY*0)  if signal_YYY & informed_type

***for blues
*if XXX & tie between B & C
replace exp_payoff_BC_outc = .5*(p_X_XXX*0 + p_Y_XXX*10) + .5*(p_X_XXX*10  + p_Y_XXX*20)  if signal_XXX & !informed_type
*if XXY & tie between B & C
replace exp_payoff_BC_outc = .5*(p_X_XXY*0 + p_Y_XXY*10) + .5*(p_X_XXY*10  + p_Y_XXY*20)  if signal_XXY & !informed_type
*if YYX & tie between B & C
replace exp_payoff_BC_outc = .5*(p_X_YYX*0 + p_Y_YYX*10) + .5*(p_X_YYX*10  + p_Y_YYX*20)  if signal_YYX & !informed_type
*if YYY & tie between B & C
replace exp_payoff_BC_outc = .5*(p_X_YYY*0 + p_Y_YYY*10) + .5*(p_X_YYY*10  + p_Y_YYY*20)  if signal_YYY & !informed_type



************************************************************************
*********** Expected earnings in the Split-Elite Equilibrium ***********
************************************************************************

* Calculating expected payoffs based on received majority signal, 
* assuming that all white players voted for A if they receive "X" and for B if they receive "Y", all blue players vote for C

** Per round
gen exp_payoff_Split_eq = .
label var exp_payoff_Split_eq "Exp. earnings in the split-whites eq."
***independent of color group
*if XXX -> tie between A & C
replace exp_payoff_Split_eq = exp_payoff_AC_outc 	if signal_XXX
*if XXY  -> C implemented
replace exp_payoff_Split_eq = exp_payoff_c_impl		if signal_XXY
*if YYX -> C implemented
replace exp_payoff_Split_eq = exp_payoff_c_impl		if signal_YYX
*if YYY -> tie between B and C
replace exp_payoff_Split_eq = exp_payoff_BC_out		if signal_YYY

***for whites
*if XXX -> tie between A & C
replace exp_payoff_Split_eq = exp_payoff_AC_outc 	if signal_XXX & informed_type
*if XXY  -> C implemented
replace exp_payoff_Split_eq = exp_payoff_c_impl		if signal_XXY & informed_type
*if YYX -> C implemented
replace exp_payoff_Split_eq = exp_payoff_c_impl		if signal_YYX & informed_type
*if YYY -> tie between B and C
replace exp_payoff_Split_eq = exp_payoff_BC_out		if signal_YYY & informed_type

***for blues
*if XXX -> tie between A & C
replace exp_payoff_Split_eq = exp_payoff_AC_outc	if signal_XXX & !informed_type
*if XXY  -> C implemented
replace exp_payoff_Split_eq = exp_payoff_c_impl		if signal_XXY & !informed_type
*if YYX -> C implemented
replace exp_payoff_Split_eq = exp_payoff_c_impl		if signal_YYX & !informed_type
*if YYY -> tie between B and C
replace exp_payoff_Split_eq = exp_payoff_BC_out		if signal_YYY & !informed_type



*****************************************************************
*********** Expected earnings in the LTED Equilibrium ***********
*****************************************************************

* Calculating expected payoffs based on received majority signal, 
* assuming that all white players voted for A if they receive "X" and for B if they receive "Y", 
*all blue players abstain

** Per round
gen exp_payoff_LTED_eq = .
label var exp_payoff_LTED_eq "Exp. earnings in the LTED eq."
***independent of color group
*if XXX -> A is implemented
replace exp_payoff_LTED_eq = exp_payoff_a_impl 	if signal_XXX 
*if XXY  > A is implemented
replace exp_payoff_LTED_eq = exp_payoff_a_impl		if signal_XXY
*if YYX -> B is implemented
replace exp_payoff_LTED_eq = exp_payoff_b_impl		if signal_YYX
*if YYY -> B is implemented
replace exp_payoff_LTED_eq = exp_payoff_b_impl		if signal_YYY

***for whites
*if XXX -> A is implemented
replace exp_payoff_LTED_eq = exp_payoff_a_impl 	if signal_XXX & informed_type
*if XXY  > A is implemented
replace exp_payoff_LTED_eq = exp_payoff_a_impl		if signal_XXY & informed_type
*if YYX -> B is implemented
replace exp_payoff_LTED_eq = exp_payoff_b_impl		if signal_YYX & informed_type
*if YYY -> B is implemented
replace exp_payoff_LTED_eq = exp_payoff_b_impl		if signal_YYY & informed_type

***for blues
*if XXX -> A is implemented
replace exp_payoff_LTED_eq = exp_payoff_a_impl		if signal_XXX & !informed_type
*if XXY -> A is implemented
replace exp_payoff_LTED_eq = exp_payoff_a_impl		if signal_XXY & !informed_type
*if YYX -> B implemented
replace exp_payoff_LTED_eq = exp_payoff_b_impl		if signal_YYX & !informed_type
*if YYY -> B implemented
replace exp_payoff_LTED_eq = exp_payoff_b_impl		if signal_YYY & !informed_type



