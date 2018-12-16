library(FLR4MFCL)
library(testthat)

# Testing the new generate method with various stocks and options
# To run these tests these yourself you will need to change the folder locations

#---------------------------------------------------
# Folders and general settings

init_seed <- 666
set.seed(init_seed)
mfcl_path <- "/home/mfcl/MFCL/Projects/ofp-sam-mfcl/src"
source("generate.R")

#---------------------------------------------------
# SKJ 2016
#---------------------------------------------------

# Get the initial OM files
initial_file_dir <- "/media/penguin/skj/2016/assessment/Model_Runs/Grid/"
# Names of the files
initial_ini <- 'skj.ini'
initial_frq <- 'skj.frq'
initial_tag <- 'skj.tag'
initial_par <- '10.par'

# Choose your OM
om_name <- "A1B1C1D1"

# Make the OM directory
om_dir <- paste("OMs/",om_name,sep="")
dir.create(om_dir)

# Copy over the ini files
system(paste("cp ", initial_file_dir, om_name, "/", initial_ini, " ", om_dir, "/", sep=""))
system(paste("cp ", initial_file_dir, om_name, "/", initial_frq, " ", om_dir, "/", sep=""))
system(paste("cp ", initial_file_dir, om_name, "/", initial_tag, " ", om_dir, "/", sep=""))
system(paste("cp ", initial_file_dir, om_name, "/", initial_par, " ", om_dir, "/", sep=""))
# Set a config file
writeLines(c("40000000", "320000000", "150000000"), con=paste(om_dir,"mfcl.cfg",sep="/"))

# Read in the assessment results
ini <- read.MFCLIni(paste(om_dir, initial_ini, sep="/"))
frq <- read.MFCLFrq(paste(om_dir, initial_frq, sep="/"))
tag <- read.MFCLTag(paste(om_dir, initial_tag, sep="/"))
first_yr <-  unname(range(frq)["minyear"])
par <- read.MFCLPar(paste(om_dir, initial_par, sep="/"), first.yr=first_yr)

nyears <- 30
# Years to average catch over to generate future catches
avyrs <- ac(2013:2015)
# First year of our projection
first_proj_yr <- as.numeric(range(frq)['maxyear']+1)

#---------------------------------------------------
# Test 1 : mix of effort and catch
proj_controls <- data.frame(name  =c('P-JPN-1','S-ALL-1','L-ALL-1','P-ALL-2','S-ASS-ALL-2','S-UNA-ALL-2','L-ALL-2','P-ALL-5','S-ASS-ALL-5','S-UNA-ALL-5','L-ALL-5','P-ALL-3','S-ASS-ALL-3','S-UNS-ALL-3','L-ALL-3','Z-PH-4','Z-ID-4','S-ID.PH-4','P-ALL-4','S-ASS.DW-4','S-UNA.DW-4','Z-VN-4','L-ALL-4'),
                             region=c(1,1,1,2,2,2,2,5,5,5,5,3,3,3,3,4,4,4,4,4,4,4,4),
                             caeff =c(1,2,1,1,2,2,1,2,2,2,1,2,2,2,1,1,1,1,2,2,2,1,1),
                             scaler=1, # status quo
                             ess   = NA) # don't set the ESS yet.
projCtrl <- MFCLprojControl(nyears=nyears, nsims=1, avyrs=avyrs, fprojyr=first_proj_yr, controls=proj_controls)
# Note that nsims is set to 1 as we are only running one simulation at a time - this gets adjusted later on
# Make a new frq file, based on the original file, that has the projection control information
projfrq  <- generate(frq, projCtrl)

# Old school method:
# Make a 00.par of the right size using a new frq file
# Call MFCL from inside the OM directory - ensures outputs are put in the right place
print(system.time({
  write(projfrq, paste(om_dir,"proj.frq",sep="/"))
  write(tag,     paste(om_dir,"proj.tag",sep="/"))
  # Create call and run MFCL 
  runcmd <- paste(mfcl_path, "/mfclo64 proj.frq ", initial_ini, " 00.par -makepar", sep="")
  writeLines(runcmd, con=paste(om_dir,'myrun',sep="/"))
  # We have to muck about with the permissions of the myrun file
  Sys.chmod(paste(om_dir,"myrun",sep="/"), "777")
  # Change into the OM dir, run MFCL, then move back
  system(paste("cd ",om_dir,"; ./myrun; cd ..",sep=""))
  # Read in the zero par we have just made
  zero.par <- read.MFCLPar(paste(om_dir,"00.par",sep="/"), first.yr=first_yr)
  # Generate the new par file which has the right size for everything and has all the old information
  old_school_projpar <- generate(par, zero.par, projfrq)
}))
# 8 s

# Using new generate() method with no zero.par
print(system.time({
  new_projpar <- generate(par, projfrq)
}))
# 0.8 s

# Expect equal
expect_equal(new_projpar, old_school_projpar)
# Nice

#---------------------------------------------------
# Test 2 : all catch
proj_controls <- data.frame(name  =c('P-JPN-1','S-ALL-1','L-ALL-1','P-ALL-2','S-ASS-ALL-2','S-UNA-ALL-2','L-ALL-2','P-ALL-5','S-ASS-ALL-5','S-UNA-ALL-5','L-ALL-5','P-ALL-3','S-ASS-ALL-3','S-UNS-ALL-3','L-ALL-3','Z-PH-4','Z-ID-4','S-ID.PH-4','P-ALL-4','S-ASS.DW-4','S-UNA.DW-4','Z-VN-4','L-ALL-4'),
                             region=c(1,1,1,2,2,2,2,5,5,5,5,3,3,3,3,4,4,4,4,4,4,4,4),
                             #caeff =c(1,2,1,1,2,2,1,2,2,2,1,2,2,2,1,1,1,1,2,2,2,1,1),
                             caeff = rep(1,23),
                             scaler=1, # status quo
                             ess   = NA) # don't set the ESS yet.
projCtrl <- MFCLprojControl(nyears=nyears, nsims=1, avyrs=avyrs, fprojyr=first_proj_yr, controls=proj_controls)
# Note that nsims is set to 1 as we are only running one simulation at a time - this gets adjusted later on
# Make a new frq file, based on the original file, that has the projection control information
projfrq  <- generate(frq, projCtrl)

# Old school method:
# Make a 00.par of the right size using a new frq file
# Call MFCL from inside the OM directory - ensures outputs are put in the right place
write(projfrq, paste(om_dir,"proj.frq",sep="/"))
write(tag,     paste(om_dir,"proj.tag",sep="/"))
# Create call and run MFCL 
runcmd <- paste(mfcl_path, "/mfclo64 proj.frq ", initial_ini, " 00.par -makepar", sep="")
writeLines(runcmd, con=paste(om_dir,'myrun',sep="/"))
# We have to muck about with the permissions of the myrun file
Sys.chmod(paste(om_dir,"myrun",sep="/"), "777")
# Change into the OM dir, run MFCL, then move back
system(paste("cd ",om_dir,"; ./myrun; cd ..",sep=""))
# Read in the zero par we have just made
zero.par <- read.MFCLPar(paste(om_dir,"00.par",sep="/"), first.yr=first_yr)
# Generate the new par file which has the right size for everything and has all the old information
old_school_projpar <- generate(par, zero.par, projfrq)

# Using new generate() method with no zero.par
print(system.time({
  new_projpar <- generate(par, projfrq)
}))

# Expect equal
expect_equal(new_projpar, old_school_projpar)
# Nice

#---------------------------------------------------
# Test 3 : all effort
proj_controls <- data.frame(name  =c('P-JPN-1','S-ALL-1','L-ALL-1','P-ALL-2','S-ASS-ALL-2','S-UNA-ALL-2','L-ALL-2','P-ALL-5','S-ASS-ALL-5','S-UNA-ALL-5','L-ALL-5','P-ALL-3','S-ASS-ALL-3','S-UNS-ALL-3','L-ALL-3','Z-PH-4','Z-ID-4','S-ID.PH-4','P-ALL-4','S-ASS.DW-4','S-UNA.DW-4','Z-VN-4','L-ALL-4'),
                             region=c(1,1,1,2,2,2,2,5,5,5,5,3,3,3,3,4,4,4,4,4,4,4,4),
                             #caeff =c(1,2,1,1,2,2,1,2,2,2,1,2,2,2,1,1,1,1,2,2,2,1,1),
                             caeff = rep(2,23),
                             scaler=1, # status quo
                             ess   = NA) # don't set the ESS yet.
projCtrl <- MFCLprojControl(nyears=nyears, nsims=1, avyrs=avyrs, fprojyr=first_proj_yr, controls=proj_controls)
# Note that nsims is set to 1 as we are only running one simulation at a time - this gets adjusted later on
# Make a new frq file, based on the original file, that has the projection control information
projfrq  <- generate(frq, projCtrl)

# Old school method:
# Make a 00.par of the right size using a new frq file
# Call MFCL from inside the OM directory - ensures outputs are put in the right place
write(projfrq, paste(om_dir,"proj.frq",sep="/"))
write(tag,     paste(om_dir,"proj.tag",sep="/"))
# Create call and run MFCL 
runcmd <- paste(mfcl_path, "/mfclo64 proj.frq ", initial_ini, " 00.par -makepar", sep="")
writeLines(runcmd, con=paste(om_dir,'myrun',sep="/"))
# We have to muck about with the permissions of the myrun file
Sys.chmod(paste(om_dir,"myrun",sep="/"), "777")
# Change into the OM dir, run MFCL, then move back
system(paste("cd ",om_dir,"; ./myrun; cd ..",sep=""))
# Read in the zero par we have just made
zero.par <- read.MFCLPar(paste(om_dir,"00.par",sep="/"), first.yr=first_yr)
# Generate the new par file which has the right size for everything and has all the old information
old_school_projpar <- generate(par, zero.par, projfrq)

# Using new generate() method with no zero.par
print(system.time({
  new_projpar <- generate(par, projfrq)
}))

# Expect equal
expect_equal(new_projpar, old_school_projpar)
# Nice



#---------------------------------------------------
# ALB 2018
#---------------------------------------------------
# Get the initial OM files
initial_file_dir <- "/media/penguin/alb/2018/Assessment/Model_Runs/"
# Choose your OM
om_name <- "GridML_GeoCPUE_GrowthChen-Wells_SizeWgt20_Steep0.8_M0.3"
# Names of the files
initial_ini <- 'alb.ini'
initial_frq <- 'alb.frq'
initial_tag <- 'alb.tag'
initial_par <- '13.par'

# Make the OM directory
om_dir <- paste("OMs/",om_name,sep="")
dir.create(om_dir)

# Copy over the ini files
system(paste("cp ", initial_file_dir, om_name, "/", initial_ini, " ", om_dir, "/", sep=""))
system(paste("cp ", initial_file_dir, om_name, "/", initial_frq, " ", om_dir, "/", sep=""))
system(paste("cp ", initial_file_dir, om_name, "/", initial_tag, " ", om_dir, "/", sep=""))
system(paste("cp ", initial_file_dir, om_name, "/", initial_par, " ", om_dir, "/", sep=""))
# Set a config file
writeLines(c("89478485", "320000000", "150000000"), con=paste(om_dir,"mfcl.cfg",sep="/"))

# Read in the assessment results
ini <- read.MFCLIni(paste(om_dir, initial_ini, sep="/"))
frq <- read.MFCLFrq(paste(om_dir, initial_frq, sep="/"))
tag <- read.MFCLTag(paste(om_dir, initial_tag, sep="/"))
first_yr <-  unname(range(frq)["minyear"])
par <- read.MFCLPar(paste(om_dir, initial_par, sep="/"), first.yr=first_yr)

# Copy this file
copyok <- file.copy(paste(initial_file_dir,om_name,"alb.age_length", sep="/"), paste(om_dir,"proj.age_length", sep="/"),overwrite=T)
# What is this? Fixes a message about tag group
recaptures(tag) <- recaptures(tag)[-70,]
recoveries(tag)[12] <- recoveries(tag)[12]-1

nyears <- 30
# Years to average catch over to generate future catches
avyrs <- ac(2013:2015)
# First year of our projection
first_proj_yr <- as.numeric(range(frq)['maxyear']+1)

#---------------------------------------------------
# Test 1 : all catch based
# Projection control

# Each OM has the same control object structure
# All fisheries are catch based so caeff is set to 1
proj_controls <- data.frame(name= c('DWFN-LL','PICT-LL','DWFN-LL','PICT-LL','AZ-LL','DWFN-LL','PICT-LL','AZ-LL','DWFN-LL','PICT-LL','DWFN-LL','PICT-LL','ALL-TR','ALL-TR','ALL-DN','ALL-DN','Index','Index','Index','Index','Index'),
                           region=c(1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 5, 5, 3, 5, 3, 5, 1, 2, 3, 4, 5),
                           caeff = rep(1,21), # All fleets are catch based
                           scaler= 1, # Set to 1 as status quo projection
                           ess   = NA) # don't set the ESS yet.

projCtrl <- MFCLprojControl(nyears=nyears, nsims=1, avyrs=avyrs, fprojyr=first_proj_yr, controls=proj_controls)
# Note that nsims is set to 1 as we are only running one simulation at a time - this gets adjusted later on
# Make a new frq file, based on the original file, that has the projection control information
projfrq  <- generate(frq, projCtrl)

# Old school method:
print(system.time({
  # Make a 00.par of the right size using a new frq file
  # Call MFCL from inside the OM directory - ensures outputs are put in the right place
  write(projfrq, paste(om_dir,"proj.frq",sep="/"))
  write(tag,     paste(om_dir,"proj.tag",sep="/"))
  # Create call and run MFCL 
  runcmd <- paste(mfcl_path, "/mfclo64 proj.frq ", initial_ini, " 00.par -makepar", sep="")
  writeLines(runcmd, con=paste(om_dir,'myrun',sep="/"))
  # We have to muck about with the permissions of the myrun file
  Sys.chmod(paste(om_dir,"myrun",sep="/"), "777")
  # Change into the OM dir, run MFCL, then move back
  system(paste("cd ",om_dir,"; ./myrun; cd ..",sep=""))
  # Read in the zero par we have just made
  zero.par <- read.MFCLPar(paste(om_dir,"00.par",sep="/"), first.yr=first_yr)
  # Generate the new par file which has the right size for everything and has all the old information
  old_school_projpar <- generate(par, zero.par, projfrq)
}))
# 9.4 s

# Using new generate() method with no zero.par
print(system.time({
  new_projpar <- generate(par, projfrq)
}))
# 0.7 s

# Expect equal
expect_equal(new_projpar, old_school_projpar)
# nice

#---------------------------------------------------
# Test 2 : all effort based
# Projection control

# Each OM has the same control object structure
# All fisheries are catch based so caeff is set to 1
proj_controls <- data.frame(name= c('DWFN-LL','PICT-LL','DWFN-LL','PICT-LL','AZ-LL','DWFN-LL','PICT-LL','AZ-LL','DWFN-LL','PICT-LL','DWFN-LL','PICT-LL','ALL-TR','ALL-TR','ALL-DN','ALL-DN','Index','Index','Index','Index','Index'),
                           region=c(1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 5, 5, 3, 5, 3, 5, 1, 2, 3, 4, 5),
                           caeff = rep(2,21), # All fleets are catch based
                           scaler= 1, # Set to 1 as status quo projection
                           ess   = NA) # don't set the ESS yet.

projCtrl <- MFCLprojControl(nyears=nyears, nsims=1, avyrs=avyrs, fprojyr=first_proj_yr, controls=proj_controls)
# Note that nsims is set to 1 as we are only running one simulation at a time - this gets adjusted later on
# Make a new frq file, based on the original file, that has the projection control information
projfrq  <- generate(frq, projCtrl)

# Old school method:
print(system.time({
  # Make a 00.par of the right size using a new frq file
  # Call MFCL from inside the OM directory - ensures outputs are put in the right place
  write(projfrq, paste(om_dir,"proj.frq",sep="/"))
  write(tag,     paste(om_dir,"proj.tag",sep="/"))
  # Create call and run MFCL 
  runcmd <- paste(mfcl_path, "/mfclo64 proj.frq ", initial_ini, " 00.par -makepar", sep="")
  writeLines(runcmd, con=paste(om_dir,'myrun',sep="/"))
  # We have to muck about with the permissions of the myrun file
  Sys.chmod(paste(om_dir,"myrun",sep="/"), "777")
  # Change into the OM dir, run MFCL, then move back
  system(paste("cd ",om_dir,"; ./myrun; cd ..",sep=""))
  # Read in the zero par we have just made
  zero.par <- read.MFCLPar(paste(om_dir,"00.par",sep="/"), first.yr=first_yr)
  # Generate the new par file which has the right size for everything and has all the old information
  old_school_projpar <- generate(par, zero.par, projfrq)
}))
# 9.7 s

# Using new generate() method with no zero.par
print(system.time({
  new_projpar <- generate(par, projfrq)
}))
# 0.7 s

# Expect equal
expect_equal(new_projpar, old_school_projpar)
# nice

#---------------------------------------------------
# YFT 2018
#---------------------------------------------------
# Get the initial OM files
initial_file_dir <- "/media/penguin/yft/2017/assessment/Model_Runs/"
# Choose your OM
om_name <- "Grid_Reg2014_OD1_CompWgt20_Mix1_Steep0.8"

# Names of the files
initial_ini <- 'yft.ini'
initial_frq <- 'yft.frq'
initial_tag <- 'yft.tag'
initial_par <- '13.par'

# Make the OM directory
om_dir <- paste("OMs/",om_name,sep="")
dir.create(om_dir)

# Copy over the ini files
system(paste("cp ", initial_file_dir, om_name, "/", initial_ini, " ", om_dir, "/", sep=""))
system(paste("cp ", initial_file_dir, om_name, "/", initial_frq, " ", om_dir, "/", sep=""))
system(paste("cp ", initial_file_dir, om_name, "/", initial_tag, " ", om_dir, "/", sep=""))
system(paste("cp ", initial_file_dir, om_name, "/", initial_par, " ", om_dir, "/", sep=""))
# Set a config file
#writeLines(c("89478485", "320000000", "150000000"), con=paste(om_dir,"mfcl.cfg",sep="/"))
writeLines(c("40000000", "320000000", "150000000"), con=paste(om_dir,"mfcl.cfg",sep="/"))

# Read in the assessment results
ini <- read.MFCLIni(paste(om_dir, initial_ini, sep="/"))
frq <- read.MFCLFrq(paste(om_dir, initial_frq, sep="/"))
tag <- read.MFCLTag(paste(om_dir, initial_tag, sep="/"))
first_yr <-  unname(range(frq)["minyear"])
par <- read.MFCLPar(paste(om_dir, initial_par, sep="/"), first.yr=first_yr)

nyears <- 30
# Years to average catch over to generate future catches
avyrs <- ac(2013:2015)
# First year of our projection
first_proj_yr <- as.numeric(range(frq)['maxyear']+1)


#---------------------------------------------------
# Test 1 : a mix of catch and effort based
# Projection control

# Set scalar in proj control based on the scalar loop
proj_controls <- data.frame(name  =c('L-ALL-1','L-ALL-2','L-US-2','L-ALL-3','L-OSE-3','L-OSW-7','L-ALL-7','L-ALL-8','L-ALL-4','L-US-4','L-AU-5','L-ALL-5','L-ALL-6','S-ASS-ALL-3','S-UNS-ALL-3','S-ASS-ALL-4','S-UNS-ALL-4','Misc-PH-7','HL-IDPH-7','S-JP-1','P-JP-1','P-ALL-3','P-ALL-8','Misc-ID-7','S-PHID-7','S-ASS-ALL-8','S-UNS-ALL-8','L-AU-9','P-ALL-7','L-ALL-9','S-ASS-ALL-7','S-UNS-ALL-7','Misc-VN-7'),
                             region=c(1, 2, 2, 3, 3, 7, 7, 8, 4, 4, 5, 5, 6, 3, 3, 4, 4, 7, 7, 1, 1, 3, 8, 7, 7,8,8,9,7,9,7,7,7),
                             #caeff =rep(c(1,2,1,2,2,2,2,2,1,2,1,2,2), c(5,2,6,4,2,4,2,2,1,1,1,2,1)),
                             #caeff =rep(c(1,1,1,2,2,2,2,2,1,2,1,2,2), c(5,2,6,4,2,4,2,2,1,1,1,2,1)), # Updated LL
                             caeff =rep(c(1,2,1,2,2,2,2,2,1,2,1,2,2), c(6,1,6,4,2,4,2,2,1,1,1,2,1)), # Updated LL again
                             #scaler=as.numeric(scalar_mults2[,scalar_count]),  # vector of the multipliers identified above for the 33 fisheries - one will get chopped out in the next lines below as needed
                             scaler=1,
                               #scaler=1,
                             ess   = NA) # don't set the ESS yet.

if (n_fisheries(frq)==32){
    proj_controls <- proj_controls[-9,] # to chop out the non-existent LL-ALL-4 for the new regional structure
}

projCtrl <- MFCLprojControl(nyears=nyears, nsims=1, avyrs=avyrs, fprojyr=first_proj_yr, controls=proj_controls)
# Note that nsims is set to 1 as we are only running one simulation at a time - this gets adjusted later on
# Make a new frq file, based on the original file, that has the projection control information
projfrq  <- generate(frq, projCtrl)

# Old school method:
print(system.time({
  # Make a 00.par of the right size using a new frq file
  # Call MFCL from inside the OM directory - ensures outputs are put in the right place
  write(projfrq, paste(om_dir,"proj.frq",sep="/"))
  write(tag,     paste(om_dir,"proj.tag",sep="/"))
  # Create call and run MFCL 
  runcmd <- paste(mfcl_path, "/mfclo64 proj.frq ", initial_ini, " 00.par -makepar", sep="")
  writeLines(runcmd, con=paste(om_dir,'myrun',sep="/"))
  # We have to muck about with the permissions of the myrun file
  Sys.chmod(paste(om_dir,"myrun",sep="/"), "777")
  # Change into the OM dir, run MFCL, then move back
  system(paste("cd ",om_dir,"; ./myrun; cd ..",sep=""))
  # Read in the zero par we have just made
  zero.par <- read.MFCLPar(paste(om_dir,"00.par",sep="/"), first.yr=first_yr)
  # Generate the new par file which has the right size for everything and has all the old information
  old_school_projpar <- generate(par, zero.par, projfrq)
}))
# 17 s

# Using new generate() method with no zero.par
print(system.time({
  new_projpar <- generate(par, projfrq)
}))
# 1.3 

# Expect equal
expect_equal(new_projpar, old_school_projpar)
# Nice



