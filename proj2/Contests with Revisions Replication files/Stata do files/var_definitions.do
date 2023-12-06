clear

*load dataset
use "Contests with Revisions Replication files\Stata dataset to use for analysis\revisionscontestdata.dta"

gen typea=1 if Type==1
replace typea=0 if Type==2

drop if alpha==.


*format payoff1 payoff2 x x1hat x2hat x12hat %9.2f

*Create contest treatment variables
gen lottery=1
replace lottery=0 if sessionid>8
gen contest="Lottery" if lottery==1
replace contest="All Pay Auction" if lottery==0

*Periods 1-20 and 21-40
gen contestperiod=0
replace contestperiod=1 if part==3
replace contestperiod=1 if part==4

*Sessionid 1-4 = Lottery 25-75
*Sessionid 5-8 = Lottery 75-25
*Sessionid 9-12 =  All pay 25-75 
*Sessionid 13-16 =  All pay 75-25

label var x1hat "Type A - Round 1"
label var x2hat "Type B - Round 1"
label var x12hat "Type A - Round 2"
label var xjow "Investment for reward of $0"

*Code "guesses" as missing for Type A players?
replace x12guess=. if x12guess==-1


*****Variables related to best responses
gen br1=10*sqrt(x2hat)-x2hat if lottery==1 & x2hat>0
replace br1=0.01 if lottery==1 & x2hat==0
gen br2=10*sqrt(x1hat)-x1hat if lottery==1 & x1hat>0
replace br2=0.01 if lottery==1 & x1hat==0
gen notunique=runiform()
replace br1=x2hat+0.01 if lottery==0 & x2hat<=99.98 
replace br1=x2hat+0.01 if lottery==0 & x2hat>99.98 & x2hat<100 & notunique<=0.5
replace br1=0 if lottery==0 & x2hat>99.98 & x2hat<100 & notunique>0.5 
replace br1=0 if lottery==0 & x2hat==100
gen diff_br_1=abs(x1hat-br1)
gen diff_br_2=abs(x12hat-br1)

gen match2=1 if x12hat==x2hat
replace match2=0 if x12hat~=x2hat

gen x12guesshat=.
replace x12guesshat=x12guess if x12guess~=.
replace x12guesshat=x1hat if Type==2 & x12guess==.

gen absdev=abs(x12guesshat-x12hat)

*Generate equilibrium predictions
gen x1eq=Prize/4 if lottery==1
replace x1eq=(1-alpha)*(Prize/2) if lottery==0
gen x2eq=Prize/4 if lottery==1
replace x2eq=((1-alpha)^2)*(Prize/2) if lottery==0
gen x12eq=Prize/4 if lottery==1
replace x12eq=((1-alpha)^2)*(Prize/2) if lottery==0

gen x12eq_expost=br1

*Generate order dummies (f75=1 if the session was a 75-25 and 0 if a 25-75
gen f25=1
*replace f25=0 if sessionid>=5
*replace f25=1 if sessionid>=9
*replace f25=0 if sessionid>=13
replace f25=0 if Period>20 & alpha==0.25

gen f75=1 if f25==0
replace f75=0 if f25==1

gen order="75-25" if f75==1
replace order="25-75" if f75==0

*Rescale period number
gen periodt=Period
replace periodt=Period-20 if Period>20

gen inv=1/periodt

*Define trand variable

*Generate A's winning probability in round 1
gen probwin1=0
replace probwin1=1 if lottery==0 & x1hat>x2hat
replace probwin1=0.5 if lottery==0 & x1hat==x2hat
replace probwin1=x1hat/(x1hat+x2hat) if x1hat+x2hat>0 & lottery==1
replace probwin1=0.5 if x1hat+x2hat==0 & lottery==1


*Dummy for alpha
gen d75=0
replace d75=1 if alpha==0.75

*****Creates a dummy variable for subjects who switched only once on both RISK and LOSS 
gen validriskloss=1
replace validriskloss=0 if RiskLineSwitchToB==-1
replace validriskloss=0 if LossLineSwitchToB==-1
sum validriskloss if part==5

***Variable that measures the distance to the best response**
*In Round 1
gen dist1=x1hat-br1
gen adist1=abs(x1hat-br1)
gen adist1_sq=(x1hat-br1)^2
gen dist2=x2hat-br2
*In Round 2
gen dist12=x12hat-br1
*Interaction with treatment
gen distinter=dist12*lottery


*Investing the value of the prize
gen x1100=0
replace x1100=1 if x1hat==100
gen x2100=0
replace x2100=1 if x2hat==100
gen x12100=0
replace x12100=1 if x12hat==100


*Investing zero
gen x10=0
replace x10=1 if x1hat==0
gen x20=0
replace x20=1 if x2hat==0
gen x120=0
replace x120=1 if x12hat==0

*Upper bound
gen ub=(1-alpha)*100
replace ub=25 if contest=="Lottery"


*Dummy for >0 joy of winning
gen joyful=1
replace joyful=0 if xjow==0

*Create some alternative risk and loss aversion measures based on choices
gen risksafechoices=20-RiskLineSwitchToB
gen losssafechoices=20-LossLineSwitchToB 

gen riskaverse=0 
replace riskaverse=1 if RiskLineSwitchToB<10 & risksafechoices~=.
replace riskaverse=. if risksafechoices==-1

gen riskneutral=0 
replace riskneutral=1 if RiskLineSwitchToB>=10 & RiskLineSwitchToB<=11
replace riskneutral=. if risksafechoices==-1

gen riskloving=0 
replace riskloving=1 if RiskLineSwitchToB>11
replace riskloving=. if risksafechoices==-1

gen risk=. if validriskloss==.
replace risk=0.25-2.5 if RiskLineSwitchToB==1
replace risk=0.5-2.5 if RiskLineSwitchToB==2
replace risk=0.75-2.5 if RiskLineSwitchToB==3
replace risk=1-2.5 if RiskLineSwitchToB==4
replace risk=1.25-2.5 if RiskLineSwitchToB==5
replace risk=1.5-2.5 if RiskLineSwitchToB==6
replace risk=1.75-2.5 if RiskLineSwitchToB==7
replace risk=2-2.5 if RiskLineSwitchToB==8
replace risk=2.25-2.5 if RiskLineSwitchToB==9
replace risk=2.5-2.5 if RiskLineSwitchToB==10
replace risk=2.75-2.5 if RiskLineSwitchToB==11
replace risk=3-2.5 if RiskLineSwitchToB==12
replace risk=3.25-2.5 if RiskLineSwitchToB==13
replace risk=3.5-2.5 if RiskLineSwitchToB==14
replace risk=3.75-2.5 if RiskLineSwitchToB==15
replace risk=4-2.5 if RiskLineSwitchToB==16
replace risk=4.25-2.5 if RiskLineSwitchToB==17
replace risk=4.5-2.5 if RiskLineSwitchToB==18
replace risk=4.75-2.5 if RiskLineSwitchToB==19
replace risk=5-2.5 if RiskLineSwitchToB==20

*lambda =slope in the domain of losses
*Use 0.5(5)+0.5*lambda*(y)=0
*=> lambda=(2.5/(0.5*-y))
*loss=lambda
gen loss=.  if validriskloss==.
replace loss=5/0.5 if LossLineSwitchToB==1
replace loss=5/1 if LossLineSwitchToB==2
replace loss=5/1.5 if LossLineSwitchToB==3
replace loss=5/2 if LossLineSwitchToB==4
replace loss=5/2.5 if LossLineSwitchToB==5
replace loss=5/3 if LossLineSwitchToB==6
replace loss=5/3.5 if LossLineSwitchToB==7
replace loss=5/4 if LossLineSwitchToB==8
replace loss=5/4.5 if LossLineSwitchToB==9
replace loss=5/5 if LossLineSwitchToB==10
replace loss=5/5.5 if LossLineSwitchToB==11
replace loss=5/6 if LossLineSwitchToB==12
replace loss=5/6.5 if LossLineSwitchToB==13
replace loss=5/7 if LossLineSwitchToB==14
replace loss=5/7.5 if LossLineSwitchToB==15
replace loss=5/8 if LossLineSwitchToB==16
replace loss=5/8.5 if LossLineSwitchToB==17
replace loss=5/9 if LossLineSwitchToB==18
replace loss=5/9.5 if LossLineSwitchToB==19
replace loss=5/10 if LossLineSwitchToB==20

*Loss aversion
gen lossaverse=.
replace lossaverse=1 if LossLineSwitchToB<10
replace lossaverse=0 if LossLineSwitchToB>=10
replace lossaverse=. if LossLineSwitchToB==.


*gender dummy
gen female=1 if gender==2
replace female=0 if gender==1

*1 = "Business Administration"; 2 = "Economics"; 3 = "Social Sciences"; 
*4 = "Humanities"; 5 = "Natural Sciences"; *6 = "Math / Statistics"; 
*7 = "Engineering / Computer Science"; 8 = "Other";
gen engineer=1 if major==7
replace engineer=0 if major~=7
gen economics=0
replace economics=1 if major==2
*1 = "White"; 2 = "African-American"; 3 = "Hispanic"; 4 = "Asian";
*5 = "American Indian"; 6 = "Pacific Islander";
gen asian=1 if race==4
replace asian=0 if race~=4
*1  = "Freshman"; 2 = "Sophomore"; 3 = "Junior"; 
*4 = "Senior"; 5 = "Graduate Student";
gen graduate=1 if class==5
replace graduate=0 if class~=5

*REGRET variables*

sort contest
by contest: egen medreg1=median(regret1)
by contest: egen medreg2=median(regret2)
by contest: egen medreg3=median(regret3)
by contest: egen medreg4=median(regret4)

by contest: egen medhapp1=median(happiness1)
by contest: egen medhapp2=median(happiness2)
by contest: egen medhapp3=median(happiness3)
by contest: egen medhapp4=median(happiness4)

*Dummies
*3.5=mid point of the scale"
gen dreg1=0
replace dreg1=1 if regret1>3
gen dreg2=0
replace dreg2=1 if regret2>3
gen dreg3=0
replace dreg3=1 if regret3>3
gen dreg4=0
replace dreg4=1 if regret4>3

gen dhapp1=0
replace dhapp1=1 if happiness1>3
gen dhapp2=0
replace dhapp2=1 if happiness2>3
gen dhapp3=0
replace dhapp3=1 if happiness3>3
gen dhapp4=0
replace dhapp4=1 if happiness4>3

gen denvy1=0
replace denvy1=1 if envy1>3
gen denvy2=0
replace denvy2=1 if envy2>3
gen denvy3=0
replace denvy3=1 if envy3>3
gen denvy4=0
replace denvy4=1 if envy4>3

gen block_x2=5
replace block_x2=4 if x2hat<80
replace block_x2=3 if x2hat<60
replace block_x2=2 if x2hat<40
replace block_x2=1 if x2hat<20


gen won_contest1=0
replace won_contest1=1 if payoff1==200-x
replace won_contest1=1 if payoff1>100 & x>0
replace won_contest1=1 if x>otherx & Type==1 & lottery==0
replace won_contest1=1 if x<otherx & Type==2 & lottery==0

gen won_contest2=0
replace won_contest2=1 if payoff2==200-x12hat & Type==1
replace won_contest2=1 if payoff2==200-x2hat & Type==2
replace won_contest2=1 if payoff2>100 & x12hat>0 & Type==1
replace won_contest2=1 if payoff2>100 & x2hat>0 & Type==2
replace won_contest2=1 if x12>otherx & Type==1 & lottery==0
replace won_contest2=1 if x12<otherx & Type==2 & lottery==0


gen payoff_round1=0
replace payoff_round1=200-x1hat if x1hat>x2hat & Type==1 & lottery==0
replace payoff_round1=200-x1hat if x1hat==x2hat & win>=0.5 & Type==1 & lottery==0
replace payoff_round1=200-x2hat if x1hat<x2hat & Type==2 & lottery==0
replace payoff_round1=200-x2hat if x1hat==x2hat & win<0.5 &  Type==2 & lottery==0
replace payoff_round1=100-x1hat if x1hat<x2hat & Type==1 & lottery==0
replace payoff_round1=100-x1hat if x1hat==x2hat & win<0.5 & Type==1 & lottery==0
replace payoff_round1=100-x2hat if x1hat>x2hat & Type==2 & lottery==0
replace payoff_round1=100-x2hat if x1hat==x2hat & win>=0.5 &  Type==2 & lottery==0
replace payoff_round1=payoff1 if lottery==1


gen payoff_round2=0
replace payoff_round2=200-x12hat if x1hat>x2hat & Type==1 & lottery==0
replace payoff_round2=200-x12hat if x1hat==x2hat & win>=0.5 & Type==1 & lottery==0
replace payoff_round2=200-x2hat if x1hat<x2hat & Type==2 & lottery==0
replace payoff_round2=200-x2hat if x1hat==x2hat & win<0.5 &  Type==2 & lottery==0
replace payoff_round2=100-x12hat if x1hat<x2hat & Type==1 & lottery==0
replace payoff_round2=100-x12hat if x1hat==x2hat & win<0.5 & Type==1 & lottery==0
replace payoff_round2=100-x2hat if x1hat>x2hat & Type==2 & lottery==0
replace payoff_round2=100-x2hat if x1hat==x2hat & win>=0.5 &  Type==2 & lottery==0
replace payoff_round2=payoff2 if lottery==1

***Equilibrium flexibility***
gen eq_flexibility=0
replace eq_flexibility=36.72 if alpha==0.25 & lottery==0
replace eq_flexibility=91.41 if alpha==0.75 & lottery==0
