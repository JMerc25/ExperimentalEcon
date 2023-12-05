Documentation for replication code for "Contests with Revisions" by E. Dechenaux and S.D. Mago

This document explains how to produce the results, tables and figures in the article "Contests with Revisions" 

Files:

Stata data file =	"revisionscontestdata.dta"

Stata Do files =	"var_definitions.do"
			"contests_revisions_analysis_exp_econ_article.do"

The Stata do-file "contests_revisions_analysis_exp_econ_article.do" contains the code to replicate the analysis in the article. For each result, table and figure, the code for that result, table or figure first runs the do-file "var_definitions.do" on the data file "revisionscontestdata.dta". 

Alsmost all variables described in this document are in the original data file "revisionscontestdata.dta". The other variables used in the analysis are constructed either by "var_definitions.do" or by "contests_revisions_analysis_exp_econ_article.do" and their definitions are contained in one of those do files.


Variables =

SESSION
Experimental session names from zTree files. In the paper, a session refers to a matching group within an experimental session. See SESSIONID variable.

PART
part = 1 is the risk aversion elicitation task;
part = 2 is the loss eliciation task;
part = 3 has 20 contest periods with alpha = 0.25 or alpha = 0.75;
part = 4 has 20 contest periods with alpha = 0.75 or alpha = 0.75;
part = 5 has decisions in a contest for a prize of zero (one period; "joy of winning")
part = 6 has demographic and emotions intensity questionnaire

TABLES
zTree variable that is not used in the analysis.

PERIOD
Period number variable = 1, 2,...,40 over part = 3 and part = 4. Used to contruct CONTESTPERIOD variable.

SUBJECT
zTree assigned subject number within an experimental session.

GROUP
Match (i.e. pair of two subjects) number within a period of an experimental session.

PROFIT
Subject's realized contest payoff in a period. Relevant to part = 3 and part = 4.

ROUND
Round = 1 for round 1 decisions (x_A1 and x_B) and Round = 2 for round 2 decisions (x_A2).

STAGE1
Variable used within the zTree program, but not in the analysis.

SEGMENT
Variable used within the zTree program, but not in the analysis.

BALANCE
Cumulative profit variable from zTree. Irrelevant and not used anywhere in the analysis.

TYPE
Type = 1 if subject's type in period t is type A
Type = 2 if subject's type in period t is type B

ALPHA
alpha = 0.25 if alpha = 0.25 in this period.
alpha = 0.75 if alpha = 0.75 in this period.

PRIZE
Variable set to the prize value of 100.

X
Subject's round 1 expenditure choice.

OTHERX
Other subject's in group's round 1 expenditure choice.

YOURINVESTMENT
Variable used by the zTree program to display results, not used in the analysis.

OTHERINVESTMENT
Variable used by the zTree program to display results, not used in the analysis.

X12
Subject's round 2 expenditure if subject's type was A (Type = 1) in the period.

X12GUESS
Subject's guess about other subject's round 2 expenditure if subject's type was B (Type = B) in the period.

X1HAT
For a given pair, the type A player's expenditure in that pair. When using in the analysis, restrict data to one type ("if Type==1 or if Type==2"), otherwise data are duplicated.

X2HAT
For a given pair, the type B player's expenditure in that pair. When using in the analysis, restrict data to one type ("if Type==1 or if Type==2"), otherwise data are duplicated.

X12HAT
For a given pair, the type A player's round 2 expenditure in that pair. When using in the analysis, restrict data to one type ("if Type==1 or if Type==2"), otherwise data are duplicated.

X1REAL
is equal to x1hat if the contest was played in round 1 and equal to x12hat if the contest proceeded to round 2. When using in the analysis, restrict data to one type ("if Type==1 or if Type==2"), otherwise data are duplicated.

SECONDSTAGE
binary indicator euqal to 1 if the contest was played in round 1 and equal to x12hat if the contest proceeded to round 2.

CHANGEHAT
Binary indicator equal to 1 if the type A subject in the pair reevised their expenditure in round 2 and equal to zero if they kept expenditure the same as round 1. When using in the analysis, restrict data to one type ("if Type==1" or "if Type==2"), otherwise data are duplicated.

WIN
Variable used in the zTree program.

PAYOFF1
Subject's payoff in the period conditional on the contest being played in round 1.

PAYOFF2
Subject's payoff in the period conditional on the contest being played in round 2.

OTHERXJOY, PAYOFFJOY, PROFITJOY, PAYOFF
Variables related to investment for a prize of zero (joy of winning) used in the zTree program and not used in the analysis.

SESSIONID
A session is a matching group of 8 subjects within an experimental session. The SESSIONID is the matching group identifier, 1, 2,..,16.

SUBJECTID
Unique subject identifier. 128 different id numbers.

RISKLINESWITCHTOB
In the risk aversion task, line on which a subject switched from the risky option A to the safe option B. 
multiple switches = -1
otherwise = 0,...,20.

LOSSLINESWITCHTOB
In the loss aversion task, line on which a subject switched from the risky option A to the safe option B. -1 = multiple switches; otherwise = 0,...,20.
multiple switches = -1
otherwise = 0,...,20.

XJOW
In the joy of winning task, subject's investment in a contest for a prize of zero.

PARTICIPANTID
Subject id used by zTree in Part = 6. Not used in the analysis.

GENDER
= 1 if subject's response was Male
= 0 if subject's response was Female

CLASS
1  = "Freshman";
2 = "Sophomore";
3 = "Junior"; 
4 = "Senior";
5 = "Graduate Student";

MAJOR
1 = "Business Administration";
2 = "Economics";
3 = "Social Sciences"; 
4 = "Humanities";
5 = "Natural Sciences";
6 = "Math / Statistics"; 
7 = "Engineering / Computer Science";
8 = "Other";

RACE
1 = "White";
2 = "African-American";
3 = "Hispanic";
4 = "Asian";
5 = "American Indian";
6 = "Pacific Islander";











