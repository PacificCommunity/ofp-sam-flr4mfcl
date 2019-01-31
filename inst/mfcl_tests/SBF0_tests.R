library(FLR4MFCL)
# Tests
library(testthat)
results_dir <- "~/Work/NZ_project/results_temp/"
rep_file <- "plot-em_pseudo_27_1.rep"
rep <- read.MFCLRep(paste(results_dir, rep_file, sep="/"))
sbf0 <- seasonMeans(areaSums(adultBiomass_nofish(rep)))
sb <- seasonMeans(areaSums(adultBiomass(rep)))

# SBF0

# Method 1, year range
years <- 2011:2015
# Not rolling
out <- SBF0Alt(rep=rep, years=years, rolling=FALSE)
expect_equal(dim(out), dim(sbf0))
expect_true(all(out == c(apply(sbf0[,as.character(years)], 6, mean))))
# Rolling
years <- 2028:2032
out <- SBF0Alt(rep=rep, years=years, rolling=TRUE)
expect_equal(dim(out), dim(sbf0))
sbf0_years <- dimnames(sbf0)$year
final_year <- as.numeric(sbf0_years[length(sbf0_years)])
for (lag in 0:5){
  expect_equal(c(out[,as.character(final_year-lag)]), c(apply(sbf0[,as.character(years-lag)], 6, mean)))
}

# Method 2, nyears, lag
nyears <- 5
lag <- 10
# Not rolling - relative to final year
out <- SBF0Alt(rep=rep, years=nyears, lag=lag, rolling=FALSE)
expect_equal(dim(out), dim(sbf0))
expect_true(all(out==c(apply(sbf0[,as.character((final_year-lag-nyears+1):(final_year-lag))],6,mean))))
# Rolling
out <- SBF0Alt(rep=rep, years=nyears, lag=lag, rolling=TRUE)
for (lag2 in 0:5){
  expect_equal(c(out[,as.character(final_year-lag2)]), c(apply(sbf0[,as.character(((final_year-lag-nyears+1):(final_year-lag))-lag2)], 6, mean)))
}

# SB/SBF0
# Assumes SBF0Alt is OK

# Method 1, year range
years <- 2011:2015
# Not rolling
out <- SBSBF0Alt(rep=rep, years=years, rolling=FALSE)
expect_equal(dim(out), dim(sbf0))
expect_equal(out, sb / SBF0Alt(rep=rep, years=years, rolling=FALSE))

# Rolling
years <- 2028:2032
out <- SBSBF0Alt(rep=rep, years=years, rolling=TRUE)
expect_equal(dim(out), dim(sbf0))
expect_equal(out, sb / SBF0Alt(rep=rep, years=years, rolling=TRUE))

# Method 2, nyears, lag
nyears <- 5
lag <- 10
# Not rolling - relative to final year
out <- SBSBF0Alt(rep=rep, years=nyears, lag=lag, rolling=FALSE)
expect_equal(out, sb / SBF0Alt(rep=rep, years=nyears, lag=lag, rolling=FALSE))
# Rolling
out <- SBSBF0Alt(rep=rep, years=nyears, lag=lag, rolling=TRUE)
expect_equal(out, sb / SBF0Alt(rep=rep, years=nyears, lag=lag, rolling=TRUE))



