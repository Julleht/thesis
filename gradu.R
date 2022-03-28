# R-code for the Master's thesis of Julius Lehtinen

# The main part of the code of the model is organised into three nested loops. 
# Innermost loop controls the individual legislative acts and their approval, and repeats the number of
# times that there are legislative acts in the parliamentary term. Second-innermost aggregates the acts
# and controls one simulation with many parliamentary terms, with a certain fraction of independent
# randomised legislators, and repeats as many times there are parliamentary terms in one simulation.
# The outermost loop aggregates the simulations with a given fraction of independents into a data frame,
# and runs ten times, for each 0.1 increment of independent legislators.

# Installing and loading libraries needed for the code
# install.packages("tidyverse")
# Sys.sleep(3)
library("tidyverse")

# Defining few functions needed in the code 
randp <- function(n = 1, r = 1) {
  if (n < 1 || r < 0) return(c())
  x <- rnorm(n)
  y <- rnorm(n)
  r <- r * sqrt(runif(n)/(x^2 + y^2))
  if (n == 1) U <- data.frame(x, y)
  else        U <- cbind(r*x, r*y)
  return(U)
}

rand2 <- function(n, r) {
  x <- rnorm(n)
  r <- r * sqrt(runif(n)/(x^2))
  U <- cbind(r*x)
  return(U)
}

sumfun <- function(x,start,end){
  return(sum(x[start:end]))
}

# Defining variables used in the model and setting seed for reproducibility
n_simulations <- 1000
n_acts <- 1000
rad <- 0.1 # Circle of tolerance of political parties
lower_bound_party_reps <- -0.9 # lower bound of values taken by the party-affiliated legislators in the Cipolla diagram

fraction_a <- 40/200
fraction_b <- 30/200
fraction_c <- 20/200
fraction_d <- 15/200
fraction_e <- 10/200
fraction_f <- 40/200
fraction_g <- 40/200
fraction_h <- 5/200
original_parliament <- 200

seed <- 197
set.seed(seed)

independents_df <- as.data.frame(c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1)) # fractions of independent legislators
names(independents_df) <- c("frac")

totaldf <- NULL
finaldist <- NULL

# Loops
for (i in 1:length(independents_df$frac)) {
  
  # The third-innermost loop controlling all the simulations of fractions of independent legislators
  
  fraction_ind <- independents_df$frac[i]
  n <- original_parliament - (fraction_ind*original_parliament)
  dist = NULL
  
  pb <- txtProgressBar(min = 0, max = n_simulations, initial = 0, style=3, width=50) # Progress bar for running the simulation
  stepi <- 0
  
  for (i in 1:n_simulations){
    
    # The second-innermost loop controlling individual simulation, aggregating the legislative acts
    
    ## Generating the parties and their legislators, loop for each
    party_a <- NULL
    if (fraction_a>0 & n > 0) {
      circle_a <- as.data.frame(randp(fraction_a*n,rad))
      names(circle_a) <- c("x", "y")
      x0 <-as.data.frame(rnorm(1))
      names(x0) <- c("x0")
      x0 <- x0 %>%
        mutate(x0 = case_when(
          x0 > 3 ~ 3,
          x0 < -3 ~ -3,
          TRUE ~ x0
        ))
      x0 <- as.numeric((1-lower_bound_party_reps)*((x0--3)/(3--3))+lower_bound_party_reps)
      y0 <- as.data.frame(rnorm(1))
      names(y0) <- c("y0")
      y0 <- y0 %>%
        mutate(y0 = case_when(
          y0 > 3 ~ 3,
          y0 < -3 ~ -3,
          TRUE ~ y0
        ))
      y0 <- as.numeric((1-lower_bound_party_reps)*((y0--3)/(3--3))+lower_bound_party_reps)
      r <- rad
      partycircle_a <- data.frame(x0, y0, r)
      partycircle_a$party <-  "a"
      x <- x0 + circle_a$x
      y <- y0 + circle_a$y
      party_a <- data.frame(x, y)
      names(party_a) <- c("x", "y")
      party_a
      for(i in 1:nrow(party_a)){
        while(party_a[i, 1] > 1 | party_a[i, 1] < -1 | (party_a[i,1] - partycircle_a$x0)^2 + (party_a[i,2] - partycircle_a$y0)^2 > rad^2){
          party_a[i, 1] <- (partycircle_a$x0 + rand2(1,rad))
        }
      }
      for(i in 1:nrow(party_a)){
        while(party_a[i, 2] > 1 | party_a[i, 2] < -1 | (party_a[i,1] - partycircle_a$x0)^2 + (party_a[i,2] - partycircle_a$y0)^2 > rad^2){
          party_a[i, 2] <- (partycircle_a$y0 + rand2(1,rad))
        }
      }
      party_a$party <- "a"
    }
    
    party_b <- NULL
    if (fraction_b>0 & n > 0) {
      circle_b <- as.data.frame(randp(fraction_b*n,rad))
      names(circle_b) <- c("x", "y")
      x0 <-as.data.frame(rnorm(1))
      names(x0) <- c("x0")
      x0 <- x0 %>%
        mutate(x0 = case_when(
          x0 > 3 ~ 3,
          x0 < -3 ~ -3,
          TRUE ~ x0
        ))
      x0 <- as.numeric((1-lower_bound_party_reps)*((x0--3)/(3--3))+lower_bound_party_reps)
      y0 <- as.data.frame(rnorm(1))
      names(y0) <- c("y0")
      y0 <- y0 %>%
        mutate(y0 = case_when(
          y0 > 3 ~ 3,
          y0 < -3 ~ -3,
          TRUE ~ y0
        ))
      y0 <- as.numeric((1-lower_bound_party_reps)*((y0--3)/(3--3))+lower_bound_party_reps)
      r <- rad
      partycircle_b <- data.frame(x0, y0, r)
      partycircle_b$party <-  "b"
      x <- x0 + circle_b$x
      y <- y0 + circle_b$y
      party_b <- data.frame(x, y)
      names(party_b) <- c("x", "y")
      for(i in 1:nrow(party_b)){
        while(party_b[i, 1] > 1 | party_b[i, 1] < -1 | (party_b[i,1] - partycircle_b$x0)^2 + (party_b[i,2] - partycircle_b$y0)^2 > rad^2){
          party_b[i, 1] <- (partycircle_b$x0 + rand2(1,rad))
        }
      }
      for(i in 1:nrow(party_b)){
        while(party_b[i, 2] > 1 | party_b[i, 2] < -1 | (party_b[i,1] - partycircle_b$x0)^2 + (party_b[i,2] - partycircle_b$y0)^2 > rad^2){
          party_b[i, 2] <- (partycircle_b$y0 + rand2(1,rad))
        }
      }
      party_b$party <- "b"
    }
    
    party_c <- NULL
    if (fraction_c>0 & n > 0) {
      circle_c <- as.data.frame(randp(fraction_c*n,rad))
      names(circle_c) <- c("x", "y")
      x0 <-as.data.frame(rnorm(1))
      names(x0) <- c("x0")
      x0 <- x0 %>%
        mutate(x0 = case_when(
          x0 > 3 ~ 3,
          x0 < -3 ~ -3,
          TRUE ~ x0
        ))
      x0 <- as.numeric((1-lower_bound_party_reps)*((x0--3)/(3--3))+lower_bound_party_reps)
      y0 <- as.data.frame(rnorm(1))
      names(y0) <- c("y0")
      y0 <- y0 %>%
        mutate(y0 = case_when(
          y0 > 3 ~ 3,
          y0 < -3 ~ -3,
          TRUE ~ y0
        ))
      y0 <- as.numeric((1-lower_bound_party_reps)*((y0--3)/(3--3))+lower_bound_party_reps)
      r <- rad
      partycircle_c <- data.frame(x0, y0, r)
      partycircle_c$party <-  "c"
      x <- x0 + circle_c$x
      y <- y0 + circle_c$y
      party_c <- data.frame(x, y)
      names(party_c) <- c("x", "y")
      for(i in 1:nrow(party_c)){
        while(party_c[i, 1] > 1 | party_c[i, 1] < -1 | (party_c[i,1] - partycircle_c$x0)^2 + (party_c[i,2] - partycircle_c$y0)^2 > rad^2){
          party_c[i, 1] <- (partycircle_c$x0 + rand2(1,rad))
        }
      }
      for(i in 1:nrow(party_c)){
        while(party_c[i, 2] > 1 | party_c[i, 2] < -1 | (party_c[i,1] - partycircle_c$x0)^2 + (party_c[i,2] - partycircle_c$y0)^2 > rad^2){
          party_c[i, 2] <- (partycircle_c$y0 + rand2(1,rad))
        }
      }
      party_c$party <- "c"
    }
    
    party_d <- NULL
    if (fraction_d>0 & n > 0) {
      circle_d <- as.data.frame(randp(fraction_d*n,rad))
      names(circle_d) <- c("x", "y")
      x0 <-as.data.frame(rnorm(1))
      names(x0) <- c("x0")
      x0 <- x0 %>%
        mutate(x0 = case_when(
          x0 > 3 ~ 3,
          x0 < -3 ~ -3,
          TRUE ~ x0
        ))
      x0 <- as.numeric((1-lower_bound_party_reps)*((x0--3)/(3--3))+lower_bound_party_reps)
      y0 <- as.data.frame(rnorm(1))
      names(y0) <- c("y0")
      y0 <- y0 %>%
        mutate(y0 = case_when(
          y0 > 3 ~ 3,
          y0 < -3 ~ -3,
          TRUE ~ y0
        ))
      y0 <- as.numeric((1-lower_bound_party_reps)*((y0--3)/(3--3))+lower_bound_party_reps)
      r <- rad
      partycircle_d <- data.frame(x0, y0, r)
      partycircle_d$party <-  "d"
      x <- x0 + circle_d$x
      y <- y0 + circle_d$y
      party_d <- data.frame(x, y)
      names(party_d) <- c("x", "y")
      for(i in 1:nrow(party_d)){
        while(party_d[i, 1] > 1 | party_d[i, 1] < -1 | (party_d[i,1] - partycircle_d$x0)^2 + (party_d[i,2] - partycircle_d$y0)^2 > rad^2){
          party_d[i, 1] <- (partycircle_d$x0 + rand2(1,rad))
        }
      }
      for(i in 1:nrow(party_d)){
        while(party_d[i, 2] > 1 | party_d[i, 2] < -1 | (party_d[i,1] - partycircle_d$x0)^2 + (party_d[i,2] - partycircle_d$y0)^2 > rad^2){
          party_d[i, 2] <- (partycircle_d$y0 + rand2(1,rad))
        }
      }
      party_d$party <- "d"
    }
    
    party_e <- NULL
    if (fraction_e>0 & n > 0) {
      circle_e <- as.data.frame(randp(fraction_e*n,rad))
      circle_e
      names(circle_e) <- c("x", "y")
      x0 <-as.data.frame(rnorm(1))
      names(x0) <- c("x0")
      x0 <- x0 %>%
        mutate(x0 = case_when(
          x0 > 3 ~ 3,
          x0 < -3 ~ -3,
          TRUE ~ x0
        ))
      x0 <- as.numeric((1-lower_bound_party_reps)*((x0--3)/(3--3))+lower_bound_party_reps)
      y0 <- as.data.frame(rnorm(1))
      names(y0) <- c("y0")
      y0 <- y0 %>%
        mutate(y0 = case_when(
          y0 > 3 ~ 3,
          y0 < -3 ~ -3,
          TRUE ~ y0
        ))
      y0 <- as.numeric((1-lower_bound_party_reps)*((y0--3)/(3--3))+lower_bound_party_reps)
      r <- rad
      partycircle_e <- data.frame(x0, y0, r)
      partycircle_e$party <-  "e"
      x <- x0 + circle_e$x
      y <- y0 + circle_e$y
      party_e <- data.frame(x, y)
      names(party_e) <- c("x", "y")
      for(i in 1:nrow(party_e)){
        while(party_e[i, 1] > 1 | party_e[i, 1] < -1 | (party_e[i,1] - partycircle_e$x0)^2 + (party_e[i,2] - partycircle_e$y0)^2 > rad^2){
          party_e[i, 1] <- (partycircle_e$x0 + rand2(1,rad))
        }
      }
      for(i in 1:nrow(party_e)){
        while(party_e[i, 2] > 1 | party_e[i, 2] < -1 | (party_e[i,1] - partycircle_e$x0)^2 + (party_e[i,2] - partycircle_e$y0)^2 > rad^2){
          party_e[i, 2] <- (partycircle_e$y0 + rand2(1,rad))
        }
      }
      party_e$party <- "e"
    }
    
    party_f <- NULL
    if (fraction_f>0 & n > 0) {
      circle_f <- as.data.frame(randp(fraction_f*n,rad))
      names(circle_f) <- c("x", "y")
      x0 <-as.data.frame(rnorm(1))
      names(x0) <- c("x0")
      x0 <- x0 %>%
        mutate(x0 = case_when(
          x0 > 3 ~ 3,
          x0 < -3 ~ -3,
          TRUE ~ x0
        ))
      x0 <- as.numeric((1-lower_bound_party_reps)*((x0--3)/(3--3))+lower_bound_party_reps)
      y0 <- as.data.frame(rnorm(1))
      names(y0) <- c("y0")
      y0 <- y0 %>%
        mutate(y0 = case_when(
          y0 > 3 ~ 3,
          y0 < -3 ~ -3,
          TRUE ~ y0
        ))
      y0 <- as.numeric((1-lower_bound_party_reps)*((y0--3)/(3--3))+lower_bound_party_reps)
      r <- rad
      partycircle_f <- data.frame(x0, y0, r)
      partycircle_f$party <-  "f"
      x <- x0 + circle_f$x
      y <- y0 + circle_f$y
      party_f <- data.frame(x, y)
      names(party_f) <- c("x", "y")
      for(i in 1:nrow(party_f)){
        while(party_f[i, 1] > 1 | party_f[i, 1] < -1 | (party_f[i,1] - partycircle_f$x0)^2 + (party_f[i,2] - partycircle_f$y0)^2 > rad^2){
          party_f[i, 1] <- (partycircle_f$x0 + rand2(1,rad))
        }
      }
      
      for(i in 1:nrow(party_f)){
        while(party_f[i, 2] > 1 | party_f[i, 2] < -1 | (party_f[i,1] - partycircle_f$x0)^2 + (party_f[i,2] - partycircle_f$y0)^2 > rad^2){
          party_f[i, 2] <- (partycircle_f$y0 + rand2(1,rad))
        }
      }
      party_f$party <- "f"
    }
    
    party_g <- NULL
    if (fraction_g>0 & n > 0) {
      circle_g <- as.data.frame(randp(fraction_g*n,rad))
      names(circle_g) <- c("x", "y")
      x0 <-as.data.frame(rnorm(1))
      names(x0) <- c("x0")
      x0 <- x0 %>%
        mutate(x0 = case_when(
          x0 > 3 ~ 3,
          x0 < -3 ~ -3,
          TRUE ~ x0
        ))
      x0 <- as.numeric((1-lower_bound_party_reps)*((x0--3)/(3--3))+lower_bound_party_reps)
      y0 <- as.data.frame(rnorm(1))
      names(y0) <- c("y0")
      y0 <- y0 %>%
        mutate(y0 = case_when(
          y0 > 3 ~ 3,
          y0 < -3 ~ -3,
          TRUE ~ y0
        ))
      y0 <- as.numeric((1-lower_bound_party_reps)*((y0--3)/(3--3))+lower_bound_party_reps)
      r <- rad
      partycircle_g <- data.frame(x0, y0, r)
      partycircle_g$party <-  "g"
      x <- x0 + circle_g$x
      y <- y0 + circle_g$y
      party_g <- data.frame(x, y)
      names(party_g) <- c("x", "y")
      for(i in 1:nrow(party_g)){
        while(party_g[i, 1] > 1 | party_g[i, 1] < -1 | (party_g[i,1] - partycircle_g$x0)^2 + (party_g[i,2] - partycircle_g$y0)^2 > rad^2){
          party_g[i, 1] <- (partycircle_g$x0 + rand2(1,rad))
        }
      }
      for(i in 1:nrow(party_g)){
        while(party_g[i, 2] > 1 | party_g[i, 2] < -1 | (party_g[i,1] - partycircle_g$x0)^2 + (party_g[i,2] - partycircle_g$y0)^2 > rad^2){
          party_g[i, 2] <- (partycircle_g$y0 + rand2(1,rad))
        }
      }
      party_g$party <- "g"
    }
    
    party_h <- NULL
    if (fraction_h>0 & n > 0) {
      circle_h <- as.data.frame(randp(fraction_h*n,rad))
      circle_h
      names(circle_h) <- c("x", "y")
      x0 <-as.data.frame(rnorm(1))
      names(x0) <- c("x0")
      x0 <- x0 %>%
        mutate(x0 = case_when(
          x0 > 3 ~ 3,
          x0 < -3 ~ -3,
          TRUE ~ x0
        ))
      x0 <- as.numeric((1-lower_bound_party_reps)*((x0--3)/(3--3))+lower_bound_party_reps)
      y0 <- as.data.frame(rnorm(1))
      names(y0) <- c("y0")
      y0 <- y0 %>%
        mutate(y0 = case_when(
          y0 > 3 ~ 3,
          y0 < -3 ~ -3,
          TRUE ~ y0
        ))
      y0 <- as.numeric((1-lower_bound_party_reps)*((y0--3)/(3--3))+lower_bound_party_reps)
      r <- rad
      partycircle_h <- data.frame(x0, y0, r)
      partycircle_h$party <-  "h"
      x <- x0 + circle_h$x
      y <- y0 + circle_h$y
      party_h <- data.frame(x, y)
      names(party_h) <- c("x", "y")
      if (length(party_h$x) > 1){
        for(i in 1:nrow(party_h)){
          while(party_h[i, 1] > 1 | party_h[i, 1] < -1 | (party_h[i,1] - partycircle_h$x0)^2 + (party_h[i,2] - partycircle_h$y0)^2 > rad^2){
            party_h[i, 1] <- (partycircle_h$x0 + rand2(1,rad))
          }
        }
        for(i in 1:nrow(party_h)){
          while(party_h[i, 2] > 1 | party_h[i, 2] < -1 | (party_h[i,1] - partycircle_h$x0)^2 + (party_h[i,2] - partycircle_h$y0)^2 > rad^2){
            party_h[i, 2] <- (partycircle_h$y0 + rand2(1,rad))
          }
        }
        party_h$party <- "h"
      }
    }
    
    ind <- NULL
    if (fraction_ind > 0) {
      x <- as.data.frame(rnorm(fraction_ind*original_parliament))
      names(x) <- c("x")
      x <- x %>%
        mutate(x = case_when(
          x > 3 ~ 3,
          x < -3 ~ -3,
          TRUE ~ x
        ))
      x <- (1--1)*((x--3)/(3--3))+-1
      y <- as.data.frame(rnorm(fraction_ind*original_parliament))
      names(y) <- c("y")
      y <- y %>%
        mutate(y = case_when(
          y > 3 ~ 3,
          y < -3 ~ -3,
          TRUE ~ y
        ))
      y <- as.numeric(1--1)*((y--3)/(3--3))+-1
      y
      ind <- data.frame(x,y)
      ind$party <- "ind"
    }
    
    partycircles <- bind_rows(partycircle_a, partycircle_b, partycircle_c, partycircle_d,
                              partycircle_e, partycircle_f, partycircle_g, partycircle_h)
    
    parliament <- bind_rows(party_a, party_b, party_c, party_d, party_e, party_f, party_g, party_h) # Binding the party-legislators to a data frame to have a parliament
    
    if (fraction_ind > 0) {
    # Binding the independents too if their fraction is above 0
        parliament <- bind_rows(parliament, ind)
    }
    
    acts <- parliament[sample(1:original_parliament, n_acts, replace = T),c(1:3)] # Extracting the positions of n legislative acts from the parliament

    votesdist <- NULL
    for(i in 1:n_acts) {
      
      # The innermost loop controlling individual legislative acts during a parliamentary term
      
      parliament$x2 <- runif(length(parliament$x), -1,1) # Extracting a voting point for each parliamentarian for each legislative act
      
      # Determining which legislators votes for the act
      parliament$supporting <- case_when(
        (acts$party[i] == parliament$party & parliament$party != "ind") ~ 1,
        (acts$party[i] == "a" & (parliament$party == "b" | parliament$party == "c"| parliament$party == "d"| parliament$party == "e") ~ 1),
        (acts$party[i] == "b" & (parliament$party == "a" | parliament$party == "c"| parliament$party == "d"| parliament$party == "e") ~ 1),
        (acts$party[i] == "c" & (parliament$party == "a" | parliament$party == "b"| parliament$party == "d"| parliament$party == "e") ~ 1),
        (acts$party[i] == "d" & (parliament$party == "a" | parliament$party == "b"| parliament$party == "c"| parliament$party == "e") ~ 1),
        (acts$party[i] == "e" & (parliament$party == "a" | parliament$party == "b"| parliament$party == "c"| parliament$party == "d") ~ 1),
        (parliament$party == "a" & parliament$x <= parliament$x2 & partycircle_a$y0 <= acts$y[i]) ~ 1,
        (parliament$party == "b" & parliament$x <= parliament$x2 & partycircle_b$y0 <= acts$y[i]) ~ 1,
        (parliament$party == "c" & parliament$x <= parliament$x2 & partycircle_c$y0 <= acts$y[i]) ~ 1,
        (parliament$party == "d" & parliament$x <= parliament$x2 & partycircle_d$y0 <= acts$y[i]) ~ 1,
        (parliament$party == "e" & parliament$x <= parliament$x2 & partycircle_e$y0 <= acts$y[i]) ~ 1,
        (parliament$party == "f" & parliament$x <= parliament$x2 & partycircle_f$y0 <= acts$y[i]) ~ 1,
        (parliament$party == "g" & parliament$x <= parliament$x2 & partycircle_g$y0 <= acts$y[i]) ~ 1,
        (parliament$party == "h" & parliament$x <= parliament$x2 & partycircle_h$y0 <= acts$y[i]) ~ 1,
        if (fraction_ind > 0) {
          (parliament$party == "ind" & parliament$x <= parliament$x2 & parliament$y <= acts$y[i]) ~ 1
        },
        TRUE ~ 0
      )
      
      votestemp <- sum(parliament$supporting)
      votesdist <- rbind(votesdist, votestemp)
    }
    
    acts$total <- votesdist
    acts$passed <- acts$total > original_parliament/2 # Act passes if over a half of the legislators votes for it
    
    # Social gain of the legislative act
    acts$socialgain <- case_when(
      acts$passed == TRUE ~ acts$y,
      acts$passed == FALSE ~ 0
    )
    
    final <- data.frame(sum(acts$passed)/n_acts, ifelse(sum(acts$passed) == 0, 0,(sum(acts$socialgain))/(sum(acts$passed))))
    names(final) <- c("acts passed", "average social gain")
    final$efficiency <- final$`acts passed` * final$`average social gain` * 100
    final$indies <- fraction_ind
    
    tmp <- final
    dist <- rbind(dist, tmp)
    stepi <- stepi + 1
    setTxtProgressBar(pb,stepi)
  }
  close(pb)
  Sys.sleep(1)
  
  dist
  dist <- dist %>%
    rowid_to_column("simulation")
  
  totaldf <- rbind(totaldf, dist)
  
  lowerbound <- mean(dist$efficiency) - (sd(dist$efficiency)/sqrt(length(dist$efficiency))) * 1.96
  upperbound <- mean(dist$efficiency) + (sd(dist$efficiency)/sqrt(length(dist$efficiency))) * 1.96
  grid <- data.frame(fraction_ind, mean(dist$`acts passed`), mean(dist$`average social gain`), mean(dist$efficiency), lowerbound, upperbound)
  names(grid) <- c("independents_fraction","passed_pct", "social_gain", "efficiency", "lower_bound", "upper_bound")
  finaldist <- rbind(finaldist, grid)
  print(finaldist)
}

# Graphics
## Quantity of the legislation passed during an individual parliamentary term
volume <- ggplot(dist, aes(x=simulation, y=`acts passed`, group=1))+
  geom_hline(yintercept = 0)+
  geom_point(alpha=0.5, colour="#d95f02", size=2)+
  geom_hline(yintercept = mean(dist$`acts passed`), colour="#d95f02")+
  geom_text(aes(x=5, y=0.30), label = paste0("Mean of acts passed during the parliamentary term: ", scales::percent(mean(dist$`acts passed`), accuracy=0.01)),
            colour="#d95f02", hjust = 0, size=6)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1))+
  scale_x_continuous(breaks=c(seq(0,1000,0.10*length(dist$simulation))))+
  theme_bw()+
  theme(
    text=element_text(size=20),
    legend.box.margin = margin(20,20,20,20),
    plot.margin = margin(10,10,10,10),
    legend.position = "none"
  )

## Quality of the legislation passed during an individual parliamentary term
socialgain <- ggplot(dist, aes(x=simulation, y=`average social gain`, group=1))+
  geom_hline(yintercept = 0)+
  geom_point(alpha=0.5, colour="#1b9e77", size=2)+
  geom_hline(yintercept = mean(dist$`average social gain`), colour="#1b9e77")+
  geom_text(aes(x=5, y=-0.65), colour="#1b9e77", hjust = 0, size=6,
            label = paste0("Mean of average social gain during the parliamentary term: ",scales::comma(mean(dist$`average social gain`),accuracy=0.01)))+
  scale_y_continuous(limits = c(-1,1))+
  scale_x_continuous(breaks=c(seq(0,1000,0.10*length(dist$simulation))))+
  theme_bw()+
  theme(
    text=element_text(size=20),
    legend.box.margin = margin(20,20,20,20),
    plot.margin = margin(10,10,10,10),
    legend.position = "none"
  )

## Efficiency of the legislation passed during an individual parliamentary term
efficiency <- ggplot(dist, aes(x=simulation, y=`efficiency`, group=1))+
  geom_hline(yintercept = 0)+
  geom_point(alpha=0.5, colour="#7570b3", size=2)+
  geom_hline(yintercept = mean(dist$efficiency), colour="#7570b3")+
  geom_text(aes(x=5, y=-65), label = paste0("Mean of efficiency during the parliamentary term: ", scales::comma(mean(dist$efficiency),accuracy=0.01)),
            colour="#7570b3", hjust = 0, size=6)+
  scale_y_continuous(limits = c(-100,100))+
  scale_x_continuous(breaks=c(seq(0,1000,0.10*length(dist$simulation))))+
  theme_bw()+
  theme(
    text=element_text(size=20),
    legend.box.margin = margin(20,20,20,20),
    plot.margin = margin(10,10,10,10),
    legend.position = "none"
  )

## Distribution of the efficiencies of the parliaments with a given fraction of independent legislators
distplot <- ggplot(finaldist, aes(x=(independents_fraction*original_parliament), y = efficiency))+
  geom_hline(yintercept = 0)+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width=2)+
  geom_label(aes(label = round(efficiency, 2)))+
  labs(x="Number of independents")+
  scale_y_continuous(limits=c(-10,20))+
  scale_x_continuous(breaks=c(seq(0,200,20)))+
  theme_bw()+
  theme(
    panel.grid.minor.x = element_blank(),
    text=element_text(size=20),
    legend.box.margin = margin(20,20,20,20),
    plot.margin = margin(10,10,10,10),
    legend.position = "none"
  )

distplot

finaldist
summary(aov(totaldf$efficiency ~ totaldf$indies)) # ANOVA of all parliamentary terms
subs <- subset(totaldf, indies == 0.0 | indies == 0.2) # Subset of only parliament without sortition and 20% sortition
summary(aov(subs$efficiency ~ subs$indies)) # ANOVA of the subset parliamentary terms
