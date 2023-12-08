
*expected payoff of 1 relative to the optimal payoff in a one round all payer auction.
gen flexibility1 = (Prize - x1hat)*(1-alpha) + (Prize-x12hat)*alpha if x1hat > x2hat
replace flexibility1 = -x1hat*(1-alpha) + (Prize-x12hat)*alpha if x1hat < x2hat 

* expected payoff of A - expected payoff of B given all payments
gen flexibility2 = (Prize - x1hat + x2hat)*(1-alpha) + (Prize-x12hat+x2hat)*alpha if x1hat > x2hat & lottery == 0
replace flexibility2 = (-x1hat-Prize+x2hat)*(1-alpha) + (Prize-x12hat+x2hat)*alpha if x1hat < x2hat & lottery == 0 
replace flexibility2 = (Prize - x1hat + x2hat)*(1-alpha)*(x1hat/(x1hat+x2hat)) + (-x1hat-Prize+x2hat)*(1-alpha)*(x2hat/(x1hat+x2hat)) + (Prize - x12hat + x2hat)*(alpha)*(x12hat/(x12hat+x2hat)) + (-x12hat-Prize+x2hat)*(alpha)*(x2hat/(x12hat+x2hat)) if lottery == 1


* expected payoff of A - expected payoff of B given only round 1 payments assuming that the efficient response is for x12hat == x2hat in APA and x1hat == x12hat in lottery
gen flexibility3 = (Prize - x1hat + x2hat)*(1-alpha) + (Prize)*alpha if x1hat > x2hat & lottery == 0 
replace flexibility3 = (-x1hat-Prize+x2hat)*(1-alpha) + (Prize)*alpha if x1hat < x2hat & lottery == 0 
replace flexibility3 = (Prize - x1hat + x2hat)*(x1hat/(x1hat+x2hat)) + (-x1hat-Prize+x2hat)*(x2hat/(x1hat+x2hat)) if lottery == 1