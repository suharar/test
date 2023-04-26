#
# install and load packages
pacman::p_load(tidyverse)


# R 3.2 Probability of delay

# R 3.2.1 Generative simulation
sim_rides <- function(N, p){
  sample(c("L", "O"), 
         size=N, 
         replace=TRUE, 
         prob=c(p, 1-p)) 
}

## out of 100 trains, delay probability = 0.9
sim_rides(100, 0.9)

# R 3.2.2 Statistical model (estimator)
"""
The code essentially calculates the posterior probabilities based on 
observed data and a range of probability values, allowing you to analyze 
how different probabilities affect the likelihood of the observed data.
"""

## where poss = possible delay probabilities
compute_post <- function(obs, poss){ 
  
  L <- sum(obs=="L") # data
  O <- sum(obs=="O")
  ## This line calculates the likelihood or "ways" for each probability value 
  ## in the poss vector. It uses the sapply() function to iterate over each 
  ## probability value (q) in poss.  q = 0.00, 0.25, 0.50...etc.
  ## For each q, it calculates the likelihood 
  ## by raising q*4 to the power of L (number of "L" occurrences) 
  ## and ((1-q)*4) to the power of O (number of "O" occurrences). 
  ## The results are stored in the ways vector.
  
  #In this case, let's say we have a probability value q of 0.25. 
  #If we raise it to the power of L, which represents the number of 
  #occurrences of L, we are essentially multiplying the probability of L 
  #happening by itself L times. Similarly, raising q to the power of O, which 
  #represents the number of occurrences of O, is multiplying the 
  #probability of O happening by itself O times.

  #By multiplying these probabilities together, we are calculating the 
  #likelihood of the specific sequence of outcomes (L and O) happening together.
  
  ways <- sapply( poss , function(q) (q*4)^L * ((1-q)*4)^O ) 
  
  ## calculates the posterior probabilities by dividing each element in the ways 
  ## vector by the sum of all elements in the ways vector. 
  ## It normalizes the likelihoods to obtain probabilities that sum up to 1.
  post <- ways/sum(ways) # relative number
  data.frame(poss, ways, post=round(post,3)) # summary
  
}

## L = 2, O = 1
data <- c("L", "O", "L")
compute_post(obs = data, poss=seq(0,1,.25))


# R 3.2.3 Integrate prior knowledge
data <- c("L", "O", "L")
prior <- compute_post(obs = data, poss=seq(0,1,.25))
new <- compute_post(obs = "O", poss=seq(0,1,.25))
prior$ways * new$ways # absolute ways
round((prior$post * new$post)/sum(prior$post * new$post), 2) # relative 


# R 3.2.4 Bayesian updating with grid approximation

# define prior 
poss <- tibble(theta = seq(0,1,.05), 
               prior = rep(1/length(theta),length(theta)))



# statistical model
compute_post <- function(obs, poss){ 
  L <- sum(obs=="L")
  likelihood <- dbinom(L, N, prob = poss$theta)
  ## incorporate prior knowledge
  posterior <- likelihood*poss$prior
  posterior_norm <- posterior/sum(posterior)
  tibble(poss,lh=round(likelihood, 3), post=round(posterior_norm,3))
}

# estimation 
N <- 9
obs <- sim_rides(N, p = .5)
estimation <- compute_post(obs, poss)


# Check results
estimation
estimation %>% 
  pivot_longer(cols = c(prior,post), names_to = "type", values_to = "probability") %>% 
  ggplot(aes(x=theta, y = probability, color = type, linetype = type)) + 
  geom_line(size = 1) + 
  theme_minimal() + 
  labs(x = "Theta", 
       y = "Probability", 
       color = "Probability",
       linetype = "Probability")



# Step-wise updating and the value of more data 

compute_post <- function(obs, poss){ 
  L <- sum(obs=="L")
  likelihood <- dbinom(L, 1, prob = poss$theta)
  posterior <- likelihood*poss$prior
  posterior_norm <- posterior/sum(posterior)
  tibble(poss,lh=round(likelihood, 3), post=round(posterior_norm,3))
}

N <- 9
p <- .5
samples <- vector("numeric", N)
results <- vector("list", N)
poss <- tibble(theta = seq(0,1,.05), 
               prior = rep(1/length(theta),length(theta)))
for (i in seq_along(1:N)){
  
  # sample new data
  samples[i] <- sample(c("L", "O"), size=1, replace=TRUE, prob=c(p, 1-p))
  
  estimation <- compute_post(samples[i], poss)
  results[[i]] <- expand_grid(N = i, estimation)
  
  poss$prior <- estimation$post
  
}


# examine updating process
label <- tibble(N = 1:N,  samples)
plot <- results %>% 
  bind_rows() %>% 
  pivot_longer(cols = c(prior, post), names_to = "type", values_to = "probability") %>% 
  ggplot(aes(x=theta, y = probability)) + 
  facet_wrap(~N) +
  geom_line(aes(linetype = type, color = type), size = 1) + 
  theme_minimal() + 
  labs(x = "Theta", 
       y = "Probability", 
       color = "Probability",
       linetype = "Probability")

plot + geom_text(
  data    = label,
  mapping = aes(x = -Inf, y = -Inf, label = samples),
  hjust   = -1,
  vjust   = -11
)