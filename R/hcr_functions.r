


## write.simNumAge
## strips out the single iteration numbers at age and recruitments and writes simulated numbers at age file.

write.simNumAge <- function(simNumAge, filename, ctrl, nregions, iter){
  
  sna  <- simNumAge[c((((iter-1)*nregions+1)):(iter*nregions)+2)]
  srec <- simNumAge[c(((iter-1)*nregions+1):(iter*nregions)+(nsims(ctrl)*nregions)+3)]
  
  writeLines(c("# simulated_numbers_at_age", "1", sna, "# simulated_recruitments", srec), "tester.txt")
  
}