####### Isaac Kamber 
####### 11/8/2026
####### ELC Mass Challenges


# Create table for "worst-case" challenge scenarios (95% CI)
library(samplingbook)


#Challenge sizes
challenge_sizes <- c(10, 25, 50, 100, 500, 1000, 5000, 10000)

#Sample sizes
sample_sizes <- c(10, 25, 50, 50, 50, 100, 500, 1000)

#Maximum incorrect allowed
max_incorrect <- c(3, 9, 15, 15, 15, 15, 15, 15)

#Create function to calculate upper bound
upper_bound <- function(N, n, m, level = 0.9) {
  if(n == N) {
    return (m / N)
  } else {
    Sprop(m = m, n = n, N = N, level = level)$ci$exact[2]
  }
}

Sprop(n = 100, N = 1000, m = 10)

#Apply function to the vectors
worst_rate <- mapply(upper_bound, challenge_sizes, sample_sizes, max_incorrect)

#Create output table
data_out <- data.frame(`Challenge Size` = challenge_sizes,
                       `Sample Size` = sample_sizes,
                       `Maximum Incorrect` = max_incorrect,
                       `Expected Rate` = max_incorrect / sample_sizes,
                       `Worst Case Rate` = worst_rate,
                       `Worst Case Voters Affected`= round(worst_rate * challenge_sizes),
                       check.names = F)

writexl::write_xlsx(data_out, "./Challenge_Audit_V1.xlsx")





# 75% Maximum with 95% CI -------------------------------------------------
# Identify sample thresholds to establish that challenge is 75% accurate with 95% confidence

max_wrong_pct <- 0.25
max_incorrect_dynamic <- c() #vector to fill with counter when threshold hit
save_upper_bound_dynamic <- c() #to fill in the worst-case percentage wrong

#Loop through the sample sizes == This is not pretty but I haven't written a line of code in 2 years
for(i in 1:length(challenge_sizes)) {
  this_challenge <- challenge_sizes[i]
  this_sample <- sample_sizes[i]
  
  #Loop through all possible CIs for number of wrong samples allowed
  for (j in 1:(this_sample-1)) {
    this_upper_bound <- upper_bound(this_challenge, this_sample, j, level = 0.9)
    if(this_upper_bound > max_wrong_pct){
      last_acceptable <- j-1 #because this one is greater than cutoff
      save_upper_bound <-  upper_bound(this_challenge, this_sample, last_acceptable, level = 0.9)
      break
    }
  }
  
  max_incorrect_dynamic <- c(max_incorrect_dynamic, last_acceptable)
  save_upper_bound_dynamic <- c(save_upper_bound_dynamic, save_upper_bound)
}

#Create output table
data_out_dynamic <- data.frame(`Challenge Size` = challenge_sizes,
                       `Sample Size` = sample_sizes,
                       `Maximum Incorrect` = max_incorrect_dynamic,
                       `Expected Rate` = max_incorrect_dynamic / sample_sizes,
                       `Worst Case Rate` = save_upper_bound_dynamic,
                       `Worst Case Voters Affected`= round(save_upper_bound_dynamic * challenge_sizes),
                       check.names = F)

writexl::write_xlsx(data_out_dynamic, "./Challenge_Audit_Threshold_Dynamic.xlsx")










