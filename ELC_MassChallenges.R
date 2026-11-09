####### Isaac Kamber 
####### 11/8/2026
####### ELC Mass Challenges


# Create table for "worst-case" challenge scenarios (95% CI)
library(samplingbook)


#Challenge sizes
challenge_sizes <- c(10, 25, 50, 100, 500, 1000, 5000, 1000)

#Sample sizes
sample_sizes <- c(10, 25, 50, 50, 50, 100, 500, 1000)

#Maximum incorrect allowed
max_incorrect <- c(4, 9, 17, 15, 15, 20, 20, 20)

#Create function to calculate upper bound
upper_bound <- function(N, n, m, level = 0.9) {
  if(n == N) {
    return (m / N)
  } else {
    Sprop(m = m, n = n, N = N, level = level)$ci$exact[2]
  }
}

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

