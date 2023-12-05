

clear
use BrandtsGerhardsMechtenberg_data.dta


***** ANALYSES (run with Stata/SE 15.1) *****

*Lying

preserve
keep if treatment>0 
collapse (max) 	maj_signal_infoY = maj_signal_infoY ///
				maj_message_lie = maj_message_lie ///
				maj_message_Y = maj_message_Y ///
				lie_successful = lie_successful ///
				sum_b_votes_a = sum_b_votes_a ///
				treatment = treatment ///
				treatment1 = treatment1 ///
				treatment2 = treatment2 ///
				treatment3 = treatment3 ///
				session_id = session_id ///
				, by(group_id period)

n bysort treatment: tab maj_message_lie if maj_signal_infoY == 1
restore


*Table 3
preserve
keep if maj_signal_infoY==1
collapse (max) 	maj_message_Y = maj_message_Y ///
				treatment1 = treatment1 ///
				treatment3 = treatment3 ///
				period = period ///
				session_id = session_id ///
				, by(group_id)
gen treat1period = treatment1 * period
gen treat3period = treatment3 * period
local rhs1 " treatment1 treatment3 period"
local rhs1a " treatment1 treatment3 period treat1period treat3period"
eststo clear
qui eststo: reg maj_message_Y 		 `rhs1', vce(cluster session_id)
n test _b[treatment1] = _b[treatment3] 
qui eststo: reg maj_message_Y 		 `rhs1a', vce(cluster session_id)
	n esttab, /*booktabs*/ label star(* 0.10 ** 0.05 *** 0.01) r2 b(3) p(3) ///
	scalars("N_clust Number of clusters") sfmt(0)  obslast eqlabels(none) ///
	mgroups("Majority message: Y", pattern (1 0)) ///
	note (" ") nomtitles order (treatment1 treatment3 period treat1period treat3period) ///	
	coeflabels(treatment1 "FullyPublic (FP)" treatment3 "TopDownClosed (TDC)" period "Period" ///
	treat1period "FullyPublic X Period" treat3period "TopDownClosed X Period")
eststo clear
restore


*Blues votes

*Table 4
preserve
keep if !informed_type 
bysort treatment maj_signal_infoY: sum group_id // explains the number of observations
eststo clear
gen treat1period = treatment1 * period
gen treat2period = treatment2 * period
gen treat3period = treatment3 * period
local rhs1 "treatment1 treatment2 treatment3  period if maj_signal_infoY==0"
local rhs1a "treatment1 treatment2 treatment3  period treat1period treat2period treat3period if maj_signal_infoY==0"
local rhs2 "treatment1 treatment3  period if treatment>0 & maj_message_Y==0"
local rhs2a "treatment1 treatment3  period treat1period treat3period if treatment>0 & maj_message_Y==0"
local rhs3 "treatment1 treatment2 treatment3  period if maj_signal_infoY==1"
local rhs3a "treatment1 treatment2 treatment3  period treat1period treat2period treat3period if maj_signal_infoY==1"
local rhs4 "treatment1 treatment3  period if treatment>0 & maj_message_Y==1"
local rhs4a "treatment1 treatment3  period treat1period treat3period if treatment>0 & maj_message_Y==1"
qui eststo: reg policy_a 		 `rhs1', vce(cluster session_id)
n test _b[treatment1] = _b[treatment3] 		
n test _b[treatment1] = _b[treatment2] 		
n test _b[treatment2] = _b[treatment3] 		
qui eststo: reg policy_a		 `rhs1a', vce(cluster session_id) 
qui eststo: reg policy_a 		 `rhs2', vce(cluster session_id)
n test _b[treatment1] = _b[treatment3] 		
qui eststo: reg policy_a		 `rhs2a', vce(cluster session_id) 
qui eststo: reg policy_b 		 `rhs3', vce(cluster session_id)
n test _b[treatment1] = _b[treatment3] 		
n test _b[treatment1] = _b[treatment2] 		
n test _b[treatment2] = _b[treatment3] 		
qui eststo: reg policy_b		 `rhs3a', vce(cluster session_id) 
qui eststo: reg policy_b 		 `rhs4', vce(cluster session_id)
n test _b[treatment1] = _b[treatment3] 		
qui eststo: reg policy_b		 `rhs4a', vce(cluster session_id) 
n esttab, /*booktabs*/ label star(* 0.10 ** 0.05 *** 0.01) r2 b(3) p(3) ///
	scalars("N_clust Number of clusters") sfmt(0)  obslast eqlabels(none)  ///
	mgroups("Majority signal: X" "Majority message: X" "Majority signal: Y" "Majority message: Y", pattern (1 0 1 0 1 0 1 0)) ///
	mtitles("A vote" "A vote" "A vote" "A vote" "B vote" "B vote" "B vote" "B vote")  ///
	note (" ") ///	
	coeflabels(treatment1 "FullyPublic (FP)" treatment2 "TopDown (TD)" treatment3 "TopDownClosed (TDC)" period "Period" ///
	treat1period "FullyPublic X Period" treat2period "TopDown X Period" treat3period "TopDownClosed X Period")
eststo clear
restore

** Figure 2
*** Analyzing voting decisions of Blues (=Uninformed) (at the individual level):
*** IF MAJORITY SIGNAL IS X
preserve
keep if informed_type==0 & maj_signal_infoY==0
collapse (mean) 	vote_a = policy_a ///
				vote_b = policy_b ///
				vote_c = policy_c ///
				vote_abstain = policy_abstain ///
				, by(period treatment)
line vote_a vote_b vote_c vote_abstain period if treatment==0,  ///
	legend(rows(3) order(1 "Votes for A" 2 "Votes for B" 3 "Votes for C" 4 "Abstentions"))	///
	lwidth(thick medthick thick medthick) lpattern(solid solid shortdash shortdash) ///
	name(line_ind_vote_blues_nochat, replace) ylabel(0(.2)1) ytitle("Fraction of votes") title(NoChat) nodraw
line vote_a vote_b vote_c vote_abstain period if treatment==1,  ///
	lwidth(thick medthick thick medthick) lpattern(solid solid shortdash shortdash) ///
	name(line_ind_vote_blues_delib, replace) ylabel(0(.2)1) ytitle(" ") title(FullyPublic) nodraw
line vote_a vote_b vote_c vote_abstain period if treatment==2,  ///
	lwidth(thick medthick thick medthick) lpattern(solid solid shortdash shortdash) ///
	name(line_ind_vote_blues_topdo, replace)  ylabel(0(.2)1) ytitle("Fraction of votes") title(TopDown) nodraw
line vote_a vote_b vote_c vote_abstain period if treatment==3,  ///
	lwidth(thick medthick thick medthick) lpattern(solid solid shortdash shortdash) ///
	name(line_ind_vote_blues_topdocl, replace)  ylabel(0(.2)1) ytitle(" ") title(TopDownClosed) nodraw
grc1leg line_ind_vote_blues_nochat line_ind_vote_blues_delib line_ind_vote_blues_topdo line_ind_vote_blues_topdocl, legendfrom(line_ind_vote_blues_nochat) title(Blues' votes if majority signal is X) imargin(medium) iscale(.7) row(2)
restore
*** IF MAJORITY SIGNAL IS Y
preserve
keep if informed_type==0 & maj_signal_infoY==1
collapse (mean) 	vote_a = policy_a ///
				vote_b = policy_b ///
				vote_c = policy_c ///
				vote_abstain = policy_abstain ///
				, by(period treatment)
line vote_a vote_b vote_c vote_abstain period if treatment==0,  ///
	legend(rows(3) order(1 "Votes for A" 2 "Votes for B" 3 "Votes for C" 4 "Abstentions"))	///
	lwidth(medthick thick thick medthick) lpattern(solid solid shortdash shortdash) ///
	name(line_ind_vote_blues_nochat, replace) ylabel(0(.2)1) ytitle("Fraction of votes") title(NoChat) nodraw
line vote_a vote_b vote_c vote_abstain period if treatment==1,  ///
	lwidth(medthick thick thick medthick) lpattern(solid solid shortdash shortdash) ///
	name(line_ind_vote_blues_delib, replace) ylabel(0(.2)1) ytitle(" ") title(FullyPublic) nodraw
line vote_a vote_b vote_c vote_abstain period if treatment==2,  ///
	lwidth(medthick thick thick medthick) lpattern(solid solid shortdash shortdash) ///
	name(line_ind_vote_blues_topdo, replace)  ylabel(0(.2)1) ytitle("Fraction of votes") title(TopDown) nodraw
line vote_a vote_b vote_c vote_abstain period if treatment==3,  ///
	lwidth(medthick thick thick medthick) lpattern(solid solid shortdash shortdash) ///
	name(line_ind_vote_blues_topdocl, replace)  ylabel(0(.2)1) ytitle(" ") title(TopDownClosed) nodraw
grc1leg line_ind_vote_blues_nochat line_ind_vote_blues_delib line_ind_vote_blues_topdo line_ind_vote_blues_topdocl, legendfrom(line_ind_vote_blues_nochat) title(Blues' votes if majority signal is Y) imargin(medium) iscale(.7) row(2)
restore


*Whites' votes

*Table 7 in Appendix
preserve
keep if informed_type 
bysort treatment maj_signal_infoY: sum group_id // explains the number of observations
eststo clear
local tablewidth "16cm" 
gen treat1period = treatment1 * period
gen treat2period = treatment2 * period
gen treat3period = treatment3 * period
local rhs1 "treatment1 treatment2 treatment3  period if informed_type & maj_signal_infoY==0"
local rhs1a "treatment1 treatment2 treatment3  period treat1period treat2period treat3period if informed_type & maj_signal_infoY==0"
local rhs2 "treatment1 treatment3  period if treatment>0 & informed_type & maj_message_Y==0"
local rhs2a "treatment1 treatment3  period treat1period treat3period if treatment>0 & informed_type & maj_message_Y==0"
local rhs3 "treatment1 treatment2 treatment3  period if informed_type & maj_signal_infoY==1"
local rhs3a "treatment1 treatment2 treatment3  period treat1period treat2period treat3period if informed_type & maj_signal_infoY==1"
local rhs4 "treatment1 treatment3  period if treatment>0 & informed_type & maj_message_Y==1"
local rhs4a "treatment1 treatment3  period treat1period treat3period if treatment>0 & informed_type & maj_message_Y==1"
qui eststo: reg policy_a 		 `rhs1', vce(cluster session_id)
n test _b[treatment1] = _b[treatment3] 		
n test _b[treatment1] = _b[treatment2] 		
n test _b[treatment2] = _b[treatment3] 		
qui eststo: reg policy_a		 `rhs1a', vce(cluster session_id) 
qui eststo: reg policy_a 		 `rhs2', vce(cluster session_id)
n test _b[treatment1] = _b[treatment3] 		
qui eststo: reg policy_a 		 `rhs2a', vce(cluster session_id)
qui eststo: reg policy_b 		 `rhs3', vce(cluster session_id)
n test _b[treatment1] = _b[treatment3] 		
n test _b[treatment1] = _b[treatment2] 		
n test _b[treatment2] = _b[treatment3] 		
qui eststo: reg policy_b 		 `rhs3a', vce(cluster session_id)
qui eststo: reg policy_b 		 `rhs4', vce(cluster session_id)
n test _b[treatment1] = _b[treatment3] 		
qui eststo: reg policy_b		 `rhs4a', vce(cluster session_id) 
n esttab, /*booktabs*/ label star(* 0.10 ** 0.05 *** 0.01) r2 b(3) p(3) ///
	scalars("N_clust Number of clusters") sfmt(0)  obslast eqlabels(none) ///
	mgroups("Majority signal: X" "Majority message: X" "Majority signal: Y" "Majority message: Y", pattern (1 0 1 0 1 0 1 0)) ///
	mtitles("A vote" "A vote" "A vote" "A vote" "B vote" "B vote" "B vote" "B vote")  ///
	note (" ") ///	
	coeflabels(treatment1 "FullyPublic (FP)" treatment2 "TopDown (TD)" treatment3 "TopDownClosed (TDC)" period "Period" ///
	treat1period "FullyPublic X Period" treat2period "TopDown X Period" treat3period "TopDownClosed X Period")
eststo clear
restore


** Figure 1 
*** Analyzing voting decisions of Whites (=Informed) (at the individual level):
*** IF MAJORITY SIGNAL IS X
preserve
keep if informed_type==1 & maj_signal_infoY==0
collapse (mean) 	vote_a = policy_a ///
				vote_b = policy_b ///
				vote_c = policy_c ///
				vote_abstain = policy_abstain ///
				, by(period treatment)
line vote_a vote_b vote_c vote_abstain period if treatment==0,  ///
	legend(rows(3) order(1 "Votes for A" 2 "Votes for B" 3 "Votes for C" 4 "Abstentions"))	///
	lwidth(thick thick medthick  medthick) lpattern(solid shortdash solid shortdash) ///
	name(line_ind_vote_whites_nochat, replace) ylabel(0(.2)1) ytitle("Fraction of votes") title(NoChat) nodraw
line vote_a vote_b vote_c vote_abstain period if treatment==1,  ///
	lwidth(thick thick medthick  medthick) lpattern(solid shortdash solid shortdash) ///
	name(line_ind_vote_whites_delib, replace) ylabel(0(.2)1) ytitle(" ") title(FullyPublic) nodraw
line vote_a vote_b vote_c vote_abstain period if treatment==2,  ///
	lwidth(thick thick medthick  medthick) lpattern(solid shortdash solid shortdash) ///
	name(line_ind_vote_whites_topdo, replace)  ylabel(0(.2)1) ytitle("Fraction of votes") title(TopDown) nodraw
line vote_a vote_b vote_c vote_abstain period if treatment==3,  ///
	lwidth(thick thick medthick  medthick) lpattern(solid shortdash solid shortdash) ///
	name(line_ind_vote_whites_topdocl, replace)  ylabel(0(.2)1) ytitle(" ") title(TopDownClosed) nodraw
grc1leg line_ind_vote_whites_nochat line_ind_vote_whites_delib line_ind_vote_whites_topdo line_ind_vote_whites_topdocl, legendfrom(line_ind_vote_whites_nochat) title(Whites' votes if majority signal is X) imargin(medium) iscale(.7) row(2)
restore
*** IF MAJORITY SIGNAL IS Y
preserve
keep if informed_type==1 & maj_signal_infoY==1
collapse (mean) 	vote_a = policy_a ///
				vote_b = policy_b ///
				vote_c = policy_c ///
				vote_abstain = policy_abstain ///
				, by(period treatment)
line vote_a vote_b vote_c vote_abstain period if treatment==0,  ///
	legend(rows(3) order(1 "Votes for A" 2 "Votes for B" 3 "Votes for C" 4 "Abstentions"))	///
	lwidth(thick thick medthick  medthick) lpattern(solid shortdash solid shortdash) ///
	name(line_ind_vote_whites_nochat, replace) ylabel(0(.2)1) ytitle("Fraction of votes") title(NoChat) nodraw
line vote_a vote_b vote_c vote_abstain period if treatment==1,  ///
	lwidth(thick thick medthick  medthick) lpattern(solid shortdash solid shortdash) ///
	name(line_ind_vote_whites_delib, replace) ylabel(0(.2)1) ytitle(" ") title(FullyPublic) nodraw
line vote_a vote_b vote_c vote_abstain period if treatment==2,  ///
	lwidth(thick thick medthick  medthick) lpattern(solid shortdash solid shortdash) ///
	name(line_ind_vote_whites_topdo, replace)  ylabel(0(.2)1) ytitle("Fraction of votes") title(TopDown) nodraw
line vote_a vote_b vote_c vote_abstain period if treatment==3,  ///
	lwidth(thick thick medthick  medthick) lpattern(solid shortdash solid shortdash) ///
	name(line_ind_vote_whites_topdocl, replace)  ylabel(0(.2)1) ytitle(" ") title(TopDownClosed) nodraw
grc1leg line_ind_vote_whites_nochat line_ind_vote_whites_delib line_ind_vote_whites_topdo line_ind_vote_whites_topdocl, legendfrom(line_ind_vote_whites_nochat) title(Whites' votes if majority signal is Y) imargin(medium) iscale(.7) row(2)
restore



*Deliberation treatmens: Blues' votes for the efficient policies

*Table 5 - Coder #1
preserve
keep if treatment>0 & !informed_type 
eststo clear
local tablewidth "11cm" 
local rhs2 "treatment1 treatment3 expected_lie resp_in_round_h disresp_in_round_h information_in_round_h justification_in_round_h welfare_mentioned_in_round_h joint_payoffs_mentioned_h period"
qui eststo: reg policy_a 		 `rhs2' if maj_message_Y==0, vce(cluster session_id)
n test _b[treatment1] = _b[treatment3] 		
qui eststo: reg policy_b 		 `rhs2' if maj_message_Y==1, vce(cluster session_id)
n test _b[treatment1] = _b[treatment3] 		
	n esttab, /*booktabs*/ label star(* 0.10 ** 0.05 *** 0.01) r2 b(3) p(3) ///
	scalars("N_clust Number of clusters") sfmt(0)  obslast  eqlabels(none) ///
	mgroups("Majority message: X" "Majority message: Y", pattern (1 1)) ///
	mtitles("A vote" "B vote")  ///
	note (" ") order(`rhs2') ///	
	coeflabels(treatment1 "FullyPublic" treatment3 "TopDownClosed" ///
	resp_in_round_h "Respectful whites" disresp_in_round_h "Disrespectful whites" ///
	information_in_round_h "Whites mention the experimental environment as information" ///
	justification_in_round_h "Whites mention the experimental environment to justify their behavior" ///
	welfare_mentioned_in_round_h "Whites mention the public spirit" joint_payoffs_mentioned_h "Whites mention whites' and blues' joint payoffs" ///
	period "Period")
eststo clear
restore


*Table 8 - Coder #2
preserve
keep if treatment>0 & !informed_type 
eststo clear
local tablewidth "11cm" 
local rhs2 "treatment1 treatment3 expected_lie resp_in_round_c disresp_in_round_c information_in_round_c justification_in_round_c welfare_mentioned_in_round_c joint_payoffs_mentioned_c period"
qui eststo: reg policy_a 		 `rhs2' if maj_message_Y==0, vce(cluster session_id)
n test _b[treatment1] = _b[treatment3] 	
qui eststo: reg policy_b 		 `rhs2' if maj_message_Y==1, vce(cluster session_id)
n test _b[treatment1] = _b[treatment3] 	
	n esttab, /*booktabs*/ label star(* 0.10 ** 0.05 *** 0.01) r2 b(3) p(3) ///
	scalars("N_clust Number of clusters") sfmt(0)  obslast eqlabels(none) ///
    mgroups("Majority message: X" "Majority message: Y", pattern (1 1)) ///
	mtitles("A vote" "B vote" )  ///
	note (" ") order(`rhs2') ///	
	coeflabels(treatment1 "FullyPublic" treatment3 "TopDownClosed" ///
	resp_in_round_c "Respectful whites" disresp_in_round_c "Disrespectful whites" ///
	information_in_round_c "Whites mention the experimental environment as information" ///
	justification_in_round_c "Whites mention the experimental environment to justify their behavior" ///
	welfare_mentioned_in_round_c "Whites mention the public spirit" joint_payoffs_mentioned_c "Whites mention whites' and blues' joint payoffs" ///
	period "Period")
eststo clear
restore


*Deliberation treatments: Lying decision of the whites

*Table 6 - Coder #1
preserve
keep if informed_type & own_signalY
eststo clear
local tablewidth "16cm" 
qui eststo: reg lie prev_suspicious_blue_h prev_recommA_blue_h prev_recommB_blue_h prev_recommC_blue_h prev_disresp_blue_h prev_voteC_blue prev_lie_successful period if treatment==1, vce(cluster session_id)
qui eststo: reg lie treatment1 treatment3 prev_voteC_blue prev_lie_successful period if treatment>0, vce(cluster session_id)
n test _b[treatment1] = _b[treatment3] 		// Prob > chi2 =    0.
	n esttab, /*booktabs*/ label star(* 0.10 ** 0.05 *** 0.01) r2 b(3) p(3) ///
	scalars("N_clust Number of clusters") sfmt(0)  obslast  eqlabels(none)  ///
 	mgroups("Only FullyPublic treatment" "All communication treatments", pattern (1 1)) ///
	nomtitles ///
	note (" ") order(treatment1 treatment3 prev_suspicious_blue_h prev_recommA_blue_h prev_recommB_blue_h prev_recommC_blue_h prev_disresp_blue_h prev_voteC_blue prev_lie_successful period) ///	
	coeflabels(treatment1 "FullyPublic" treatment3 "TopDownClosed" period "Period")
eststo clear
restore

*Table 9 - Coder #2
preserve
keep if informed_type & own_signalY
eststo clear
local tablewidth "16cm" 
qui eststo: reg lie prev_suspicious_blue_c prev_recommA_blue_c prev_recommB_blue_c prev_recommC_blue_c prev_disresp_blue_c prev_voteC_blue prev_lie_successful period if treatment==1, vce(cluster session_id)
	n esttab, /*booktabs*/ label star(* 0.10 ** 0.05 *** 0.01) r2 b(3) p(3) ///
	scalars("N_clust Number of clusters") sfmt(0)  obslast eqlabels(none)  ///
 	mgroups("Only FullyPublic treatment", pattern (1)) ///
	nomtitles ///
	note (" ") order(prev_suspicious_blue_c prev_recommA_blue_c prev_recommB_blue_c prev_recommC_blue_c prev_disresp_blue_c prev_voteC_blue prev_lie_successful period) ///	
	coeflabels(period "Period")
eststo clear
restore


********************************************************************************
* ELECTRONIC SUPPLEMENTARY MATERIAL

*Table B.1
*** Voting outcomes at the group level conditional on the majority signal
preserve
gen other = 0
replace other=1 if !ac_realization & !bc_realization & !outc_split_whites ///
	 &  !a_realization & !b_realization 
label var other "Other"
sort treatment	 	
*If majority signal is X
estpost tabstat outc_allA almost_outc_allA a_realization ///
	outc_AC outc_AC_split  almost_outc_AC ac_realization ///
	outc_allB almost_outc_allB b_realization other ///
	outc_BC outc_BC_split almost_outc_BC bc_realization ///
	outc_split_whites other ///
	if  maj_signal_infoY==0, by(treatment) ///
    statistics(mean) columns(statistics) listwise	nototal 
n esttab, /*booktabs*/ main(mean %9.3f)  label nostar unstack  noobs nonote nomtitle nonumber 
*If majority signal is Y
estpost tabstat outc_allA almost_outc_allA a_realization ///
	outc_AC outc_AC_split  almost_outc_AC ac_realization ///
	outc_allB almost_outc_allB b_realization other ///
	outc_BC outc_BC_split almost_outc_BC bc_realization ///
	outc_split_whites other ///
	if  maj_signal_infoY==1, by(treatment) ///
   statistics(mean) columns(statistics) listwise	nototal
n esttab, /*booktabs*/ main(mean %9.3f) label nostar unstack ///
  noobs nonote nomtitle nonumber 
restore


*Table B.2
preserve
collapse (max) 	outc_allA = outc_allA  ///
				outc_allB = outc_allB ///
				outc_AC = outc_AC ///
				outc_BC = outc_BC ///
				maj_signal_infoY = maj_signal_infoY ///
				maj_message_Y = maj_message_Y ///
				treatment = treatment ///
				treatment1 = treatment1 ///
				treatment2 = treatment2 ///
				treatment3 = treatment3 ///
				session_id = session_id ///
				, by(period group_id)
eststo clear
local tablewidth "13cm" 
local rhs1 "treatment1 treatment2 treatment3  period if maj_signal_infoY==0"
local rhs3 "treatment1 treatment2 treatment3  period if maj_signal_infoY==1"
qui eststo: reg outc_allA 		 `rhs1', vce(cluster session_id)
n test _b[treatment1] = _b[treatment2] 	
n test _b[treatment2] = _b[treatment3]	
n test _b[treatment1] = _b[treatment3] 	
qui eststo: reg outc_AC		 `rhs1', vce(cluster session_id) 
n test _b[treatment1] = _b[treatment2] 	
n test _b[treatment2] = _b[treatment3]	
n test _b[treatment1] = _b[treatment3] 	
qui eststo: reg outc_allB 		 `rhs3', vce(cluster session_id)
n test _b[treatment1] = _b[treatment2] 	
n test _b[treatment2] = _b[treatment3]	
n test _b[treatment1] = _b[treatment3] 	
qui eststo: reg outc_BC		 `rhs3', vce(cluster session_id) 
n test _b[treatment1] = _b[treatment2] 	
n test _b[treatment2] = _b[treatment3]	
n test _b[treatment1] = _b[treatment3] 	
n esttab, /*booktabs*/ label star(* 0.10 ** 0.05 *** 0.01) r2 b(3) p(3) ///
	scalars("N_clust Number of clusters") sfmt(0)  obslast eqlabels(none)  ///
    mgroups("Majority signal: X" "Majority signal: Y" , pattern (1 0 1 0 )) ///
	mtitles("All A" "A/C" "All B" "B/C")  ///
	note (" ") ///	
	coeflabels(treatment1 "FullyPublic" treatment2 "TopDown" treatment3 "TopDownClosed" period "Period")
eststo clear
restore



*Table B.3
preserve
quietly do generating_equil_payoffs
gen total_earnings = exp_payoff_round_sig*6
gen whites_earnings = exp_payoff_round_sig*3 if informed_type
gen blues_earnings = exp_payoff_round_sig*3 if !informed_type
sort treatment	 	
*all players
estpost tabstat total_earnings  ///
	, by(treatment) nototal ///
	statistics(mean p50 sd) columns(statistics) listwise	
n esttab, main(mean %9.2f) aux(sd) nostar unstack ///
  noobs nonote mtitle("all players") nonumber 
*white players
estpost tabstat whites_earnings ///
	if informed_type, by(treatment) nototal ///
	statistics(mean p50 sd) columns(statistics) listwise	
n esttab, main(mean %9.2f) aux(sd) nostar unstack ///
  noobs nonote mtitle("white players") nonumber   
*blue players 
estpost tabstat blues_earnings ///
	if !informed_type, by(treatment) nototal ///
	statistics(mean p50 sd) columns(statistics) listwise	
n esttab, main(mean %9.2f) aux(sd) nostar unstack ///
  noobs nonote mtitle("blue players") nonumber 
restore


*Table B.4
*
preserve
qui do generating_equil_payoffs
eststo clear
local tablewidth "14cm"
gen xx = informed_type * treatment1
gen yy = informed_type * treatment2
gen zz = informed_type * treatment3
local rhs1a "treatment1 treatment2 treatment3 period"
local rhs2 "treatment1 treatment2 treatment3 informed_type xx yy zz period "
qui eststo: reg exp_payoff_round_sig		`rhs1a', vce(cluster session_id)
n test _b[treatment1] = _b[treatment2] 	
n test _b[treatment2] = _b[treatment3]	
n test _b[treatment1] = _b[treatment3] 	
qui eststo: reg exp_payoff_round_sig		`rhs1a' if informed_type, vce(cluster session_id)
n test _b[treatment1] = _b[treatment2] 	
n test _b[treatment2] = _b[treatment3]	
n test _b[treatment1] = _b[treatment3] 	
qui eststo: reg exp_payoff_round_sig		`rhs1a' if !informed_type, vce(cluster session_id)
n test _b[treatment1] = _b[treatment2] 	
n test _b[treatment2] = _b[treatment3]	
n test _b[treatment1] = _b[treatment3] 	
qui eststo: reg exp_payoff_round_sig		`rhs2', vce(cluster session_id)
	n esttab, /*booktabs*/ label star(* 0.10 ** 0.05 *** 0.01) r2 b(3) p(3) ///
	scalars("N_clust Number of clusters") sfmt(0)  eqlabels(none) ///
	note (" ") mtitles ("All Players" "Whites" "Blues" "All Players" "All  Players") ///
	coeflabels(treatment1 "FullyPublic" treatment2 "TopDown" treatment3 "TopDownClosed" xx "FullyPublic X White player" ///
					yy "TopDown X White player" zz "TopDownClosed X White player" ///
					informed_type "White player" period "Period" )
eststo clear
restore

do "C:\Users\joshu\OneDrive\Documents\GitHub\ExperimentalEcon\PersonalProj\generating_equil_payoffs.do"


