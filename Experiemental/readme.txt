--------------------------------------------------------
uniqueId

alphanumeric string unique to each participant 
--------------------------------------------------------
study

Study number (1..5)
--------------------------------------------------------
duration

number of seconds spent in the experiment

--------------------------------------------------------
comprehension_num_correct

manually created using Comp_{1..8}

--------------------------------------------------------
comprehension_passed

binary variable, takes 1 if answered at least 3 questions
correctly in both stages, 0 otherwise
--------------------------------------------------------
scenario

indicates which condition the person
participated in, takes values:
	- no_bot: no bot was signalled
	- p1bot: Player 1 (Helper) is a bot 
	- p2bot: Player 2 (Beneficiary) is a bot 
	- p3bot: Player 3 (Punisher) is a bot 

--------------------------------------------------------
is_player

the Player number of the participant, takes values
	- 1: Player 1
	- 3: Player 3
	- 4(H): Player 4 paired with a Helper
	- 4(P): Player 4 paired with a Punisher

--------------------------------------------------------
age

age of the participant, numeric

--------------------------------------------------------
age_categories

age variable binned into:
	- 1: Below 30
	- 2: From 30 to 50
	- 3: Above 50

--------------------------------------------------------
gender

gender of the participant
can be male, female, other

--------------------------------------------------------
gender_categories

gender values recategorized into:
	- Male
	- Non-Male

--------------------------------------------------------
ethnics

race of the participants. multiple choice, 
choices separated by a comma
--------------------------------------------------------
race_categories

ethnics variable collapsed into:
	- Non-Hispanic White
	- Other

--------------------------------------------------------
educ

education level of participant

--------------------------------------------------------
educ_categories

educ variable recategorized into:
	- Less than college
	- College degree (2 or 4 years)
	- More than college 

--------------------------------------------------------
income

Income bracket of the participant for the previous year

--------------------------------------------------------
income_categories

income recategorized into:
	- Less than $30,000
	- $30,000 - $49,999
	- $50,000 - $69,999
	- $70,000 - $99,999
	- $100,000 and above
	- Prefer not to say

--------------------------------------------------------
location

the state the participant is from

--------------------------------------------------------
region

location categorized into regions:
	- Northeast
	- Midwest
	- South
	- West

--------------------------------------------------------
Comp_{1..8}

Stage 1 Comprehension question {1..4}
Stage 2 Comprehension question {5..8}

--------------------------------------------------------
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
P4HS1_No

The amount Player 4 would like to send to Player 1 if
they haven't shared with Player 2

--------------------------------------------------------
P4HS1_Yes

The amount Player 4 would like to send to Player 1 if
they have shared with Player 2

--------------------------------------------------------
P4PS1_No

The amount Player 4 would like to send to Player 3 if
they haven't punished Player 1 for not sharing

--------------------------------------------------------
P4PS1_Yes

The amount Player 4 would like to send to Player 3 if
they have punished Player 1 for not sharing

--------------------------------------------------------
P4H_trust_gain

The amount the Helper would gain if they had shared with
the Beneficiary. This is calculated using P4HS1_Yes
and P4HS1_No

--------------------------------------------------------
P4P_trust_gain

The amount the Punisher would gain if they had punished
the Helper for not sharing. This is calculated using 
P4PS1_Yes and P4PS1_No

--------------------------------------------------------
P1_followup_code{1..5}

Codes for the qualitative responses provided by Players 1
For detailed explanation of each value please refer to 
the SI of the paper

--------------------------------------------------------
P4H_followup_code{1..5}

Codes for the qualitative responses provided by Players 
4(H). For detailed explanation of each value please 
refer to the SI of the paper

--------------------------------------------------------
guess_norm_all

Participant's guess about what percentage of people 
think that they should share with bots

--------------------------------------------------------
guess_emp_all

Participant's guess about what percentage of people 
actually share with bots

--------------------------------------------------------
norm_self

Participant's guess about Player 1's or Player 3's decision
in the specific treatment.

--------------------------------------------------------
player_identity_{1..5}

Participant's recall as to who was a bot in the group.
Multiple choice question. 

--------------------------------------------------------
treatment_takeup

binary variable created based on player_identity_{1..5}
and scenario variables. 

--------------------------------------------------------
info_true

Whether or not they believed the norm manipulation

--------------------------------------------------------
with_norm

binary variable, indicating whether the participant was
presented with a norm manipulation

--------------------------------------------------------
p1_human_wants

Do you think that the person in the role of Player 1 
"wants" money in the sense that he/she has a need or 
a desire for it?

Values are integers from 0 to 100, with increments of 10
	0 - Definitely not
	50 - Neither yes or no
	100 - Definitely yes

--------------------------------------------------------
p1_bot_wants

Do you think that the bot in the role of Player 1 "wants" 
money in the sense that it has a need or a desire for it?

Values are integers from 0 to 100, with increments of 10
	0 - Definitely not
	50 - Neither yes or no
	100 - Definitely yes

--------------------------------------------------------
p1_bot_asif

Do you think the bot in the role of Player 1 behaves 
AS IF it "wants" money, in the sense that it is programed 
to behave that way?

Values are integers from 0 to 100, with increments of 10
	0 - Definitely not
	50 - Neither yes or no
	100 - Definitely yes

--------------------------------------------------------
twitter_human_wants

do you think they "want" their content to be Liked in 
the sense that they have a need or desire for it?

Values are integers from 0 to 100, with increments of 10
	0 - Definitely not
	50 - Neither yes or no
	100 - Definitely yes

--------------------------------------------------------
twitter_bot_wants

do you think they "want" their content to be Liked 
in the sense that they have a need or desire for it?

Values are integers from 0 to 100, with increments of 10
	0 - Definitely not
	50 - Neither yes or no
	100 - Definitely yes

--------------------------------------------------------
twitter_bot_asif

do you think they behave AS IF they "want" their content 
to be liked, in the sense that they are programed to 
behave this way?

Values are integers from 0 to 100, with increments of 10
	0 - Definitely not
	50 - Neither yes or no
	100 - Definitely yes

--------------------------------------------------------
wiki_human_wants

do you think they "want" to avoid being banned, in the 
sense that they feel a need or desire to not be banned?

Values are integers from 0 to 100, with increments of 10
	0 - Definitely not
	50 - Neither yes or no
	100 - Definitely yes

--------------------------------------------------------
wiki_bot_wants

do you think they "want" to avoid being banned, in the 
sense that they feel a need or desire to not be banned?

Values are integers from 0 to 100, with increments of 10
	0 - Definitely not
	50 - Neither yes or no
	100 - Definitely yes

--------------------------------------------------------
wiki_bots_asif

do you think they behave AS IF they "want" to avoid being 
banned, in the sense that they are programmed to behave 
that way?

Values are integers from 0 to 100, with increments of 10
	0 - Definitely not
	50 - Neither yes or no
	100 - Definitely yes

--------------------------------------------------------
earned

the amount of money earned during the experiment in 
dollars
--------------------------------------------------------