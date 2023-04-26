# library
library(tidyverse)
library(data.table)
library(ggplot2)

# Dice

## 1. 2 vectors representing the sample spaces of both dice throws.

### ANSWER TO Q1
d1 <- c(1, 2, 3, 4, 5, 6)
d2 <- copy(d1)


## 2. generate a data frame representing all 36 possible outcomes of throwing both dice
df <- expand_grid(d1 = d1, d2 = d2)

### ANSWER TO Q2
View(df)


## 3. Add a column that specifies the probability of each possible outcome (combination)
df <- df %>% mutate(probability = 1 / nrow(df))

### ANSWER TO Q3
View(df)


## 4. Add a column that specifies the sum across both dice for each possible outcome (combination).
df <- df %>% mutate(sum = d1 + d2)

### ANSWER TO Q4
View(df)


## 5. Compute the probability that the sum is ≥ 7 , given the first dice shows 3.
### sub-setting the scenarios where 1st die equal to 3
d1_3_all <- subset(df, d1 == 3)
### sub-setting the scenarios where 1st die equal to 3 and sum is >= 7
d1_3_sumAbove_7 <- subset(d1_3_all, sum >= 7)
### calculating the conditional probability
d1_3_sumAbove_7_prob <- nrow(d1_3_sumAbove_7) / nrow(d1_3_all)

### ANSWER TO Q5 (0.5)
d1_3_sumAbove_7_prob 


## 6. Compute the probability that the sum lies between 4 and 9.
### sub-setting the row where sum is between 4 and 9
sum_4_to_9 <- subset(df, sum >= 4 & sum <= 9)
### calculating the probability of such a case
sum_4_to_9_prob <- nrow(sum_4_to_9) / nrow(df)

### ANSWER TO Q6 (0.75)
sum_4_to_9_prob


## 7. What is the probability of the most probable sum?
### generate a table which has frequency of each sum value
sum_freq_table <- table(df$sum)
### find the sum value with the highest frequency, which is sum = 7
which.max(sum_freq_table)

### ANSWER TO Q7 (0.1666667)
nrow(df[df$sum == 7,]) / nrow(df)


# Probability of Delay

# self memo
# --------------------------------
# Understand dbinom() function:

# dbinom(4, 10, 0.3)
# Calculate the probability of 4 successes in 10 trials 
# where the probability of success is 0.3.
# Result: 0.2001209
# --------------------------------


## 8. compute the probability distribution over all possible numbers 
##    of delays using the dbinom() function.
## Bonus: Store all probability distributions in one data frame, 
##        where columns are the possible numbers of delay and rows 
##        are the possible probabilities of delay (or vice versa).


### define possible delay probabilities
probabilities <- seq(0, 1, by = 0.1)

### prepare the df to store the answer
### row represents number of delay out of 10 trains
### column represents each probability
q8_answer <- data.frame(replicate(11, rep(NA, 11)))
colnames(q8_answer) <- probabilities

### Generate the probability distribution
for (i in 1:length(probabilities)) {
  p <- probabilities[i]
  q8_answer[, i] <- dbinom(0:10, size = 10, prob = p)
}

### ANSWER TO Q8
View(q8_answer)


## 9. Use the code below to generate train ride data. 
##    Use the dbinom() function to compute the likelihood of the 
##    data (observed number of delays), given p=0, p=0.1, p=0.2, 
##    p=0.3, p=0.4, p=0.5, p=0.6, p=0.7, p=0.8, p=0.9, p=1.

sim_rides <- function(N, p){
  sample(c("L", "O"), 
         size=N, 
         replace=TRUE, 
         prob=c(p, 1-p))
}

set.seed(1237)
obs <- sim_rides(10, .3)
obs

### probability of 5 times delay in 10 train arrivals 
### given each prob from 0 to 1 w/ step of 0.1
likelihood <- dbinom(sum(obs == "L"), 
                     size = length(obs), 
                     prob = probabilities)

### ANSWER TO Q9
likelihood
###  [1] 0.000000000 0.001488035 0.026424115 
###  [4] 0.102919345 0.200658125 0.246093750 
###  [7] 0.200658125 0.102919345 0.026424115
###  [10] 0.001488035 0.000000000


### plot - for self understanding
likelihood_df <- data.frame(Probability = probabilities, Likelihood = likelihood)
ggplot(likelihood_df, 
       aes(x = Probability, y = Likelihood)) +
  geom_line() +
  geom_point() +
  theme_minimal()


## 10. Assume the below prior probabilities. Use the Bayes’ rule 
##     to calculate the posterior probability using the prior 
##     probabilities and the likelihoods that you computed. 

prior <- c(0.000, 0.004, 0.041, 0.123, 0.209, 0.246, 
           0.209, 0.123, 0.041, 0.004, 0.000)

### prepare the data frame to include the answer, or posterior prob
q10_df <- data.frame(theta = likelihood_df$Probability,
                     prior = prior,
                     likelihood = likelihood_df$Likelihood)

### Calculate the unnormalized posterior probabilities
posterior <- q10_df$prior * q10_df$likelihood

### Calculate the normalized posterior probabilities
posterior_norm <- posterior / sum(posterior)

### add posterior_norm to the df
q10_df <- q10_df %>% mutate(post = round(posterior_norm,3))

### ANSWER TO Q10
View(q10_df)
