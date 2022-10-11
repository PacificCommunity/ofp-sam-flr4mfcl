#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

################################################
##
## Regional contributions to spawning biomass plot
##
## rds 11/06/2017
##
##
##
################################################

# par <- read.MFCLPar("Q:\\skj\\2016\\assessment\\RefCase\\11.par")
# rep <- read.MFCLRep("Q:\\skj\\2016\\assessment\\RefCase\\plot-11.par.rep")
# contributions.plot(par, rep, Fmult=2, propn=F)
# par <- read.MFCLPar("/home/rob/MSE/skj/MSE_grid_2019/A0B0C0D0E0_fit/07.par")
# rep <- read.MFCLRep("/home/rob/MSE/skj/MSE_grid_2019/A0B0C0D0E0_fit/plot-07.par.rep")

contributions.plot <- function(par, rep, Fmult=0, propn=TRUE, cols=NULL){

  nages   <- dimensions(par)['agecls']
  ssns    <- dimensions(par)['seasons']
  regions <- dimensions(par)['regions']

  # m <- c(aperm(m_at_age(rep), c(4,1,2,3,5,6)))
  m <- m_at_age(rep)
  # fec <- c(aperm(mat(par), c(4,1,2,3,5,6)))
  fec <- mat(par)
  f <- fm(rep)[,as.character(range(rep)['maxyear'])]

  # Calculate average recruitment by region and season for an appropriate period
  recdist <- yearMeans(rec_region(rep))
  # recdist <- yearMeans(trim(rec_region(rep), year=1982:2014))
  # recdist <- region_pars(par)[1,]

  pop <- FLQuant(0, dimnames=list(age=1:nages, year="all", unit="unique", season=1:ssns, area=1:regions, iter=1:regions))

  # Seed the recruitment values
  for(rr in 1:regions)
    pop[1,,,,rr,rr] <- recdist[,,,,rr,]

  # Calculate popn numbers
  for(ii in 1:regions)
    for(qq in 1:ssns)
      for(aa in 2:nages)
        pop[aa,,,qq,ii] <- pop[aa-1,,,qq,ii] %*% diff_coffs_age_period(par)[,,aa,qq] * exp(-m[aa]-c(f[aa,,,qq,ii,]*Fmult))

  pop_ab <- seasonMeans(quantSums(sweep(pop, 1, waa(par)*fec, "*")))

  dat <- aperm(pop_ab, c(6,5,1,2,3,4))[,,1,1,1,1]

  # Plotting stuff - using legacy code where possible
  if(is.null(cols))
    cols <- gray(seq(0,1,length.out=regions+2))[-c(1,regions)]

  # X11(2000, 1400)
  layout(matrix(c(1,2),2,1,byrow=TRUE), c(35,20), c(20,5), TRUE)
  par(mar=c(2.5,3,2,2))

  if(propn)
    barplot(sweep(dat, 2, apply(dat, 2, sum), '/'), col=cols, names.arg= paste("Reg", 1:regions),las=1)
  if(!propn)
    barplot(dat, col=cols, names.arg= paste("Reg", 1:regions),las=1)

  mtext(side=2, "Proportion of biomass by source region", line=2.5)
  a <- barplot(dat, col=rainbow(6), plot=FALSE)

  # Dummy plot to plot legend
  par(mar=c(2,3,0,2))
  plot(0,0, ylim=c(0,1), xlim=c(0,(max(a)+a[1]/2)), type="n", ylab="", xlab="", yaxt="n", xaxt="n", xpd=T, bty="n")
  points(a-0.2, rep(1,length(a)), pch=15, cex=6, col=cols,xpd=T, bty="n")

}

# contributions.plot(par, rep, Fmult=0, propn=TRUE)
