



par <- read.MFCLPar('/home/rob/MSE/ofp-sam-skipjack_MSE/OM_2019/A0B0C0D0E0_fit/08.par')
rep <- read.MFCLRep('/home/rob/MSE/ofp-sam-skipjack_MSE/OM_2019/A0B0C0D0E0_fit/plot-08.par.rep')

proj8 <- read.MFCLPar('/home/rob/MSE/ofp-sam-skipjack_MSE/condor/OMs_2019/A0B0C0D0E0F0/proj8.par')


par <- read.MFCLPar('/home/rob/swo/DiagCase/12x.par')
rep <- read.MFCLRep('/home/rob/swo/DiagCase/plot-12.par.rep')


nages   <- dimensions(par)['agecls']
ssns    <- dimensions(par)['seasons']
regions <- dimensions(par)['regions']

frange  <- (range(rep)['maxyear']-flagval(proj8,2, 148)$value/ssns) : (range(rep)['maxyear']-flagval(proj8,2, 155)$value/ssns)

m   <- c(aperm(m_at_age(rep), c(4,1,2,3,5,6)))
fec <- c(aperm(mat(par), c(4,1,2,3,5,6)))
f   <- fm(rep)[,as.character(range(rep)['maxyear']-1),,,]

# swordfish F values from  ests.rep
#f[,,,1,1] <- c(0.0092, 0.0212, 0.1175, 0.3795, 0.3317, 0.2490, 0.1315, 0.0919, 0.0754, 0.0651, 0.0585, 0.0546, 0.0521, 0.0505, 0.0496, 0.0492, 0.0489, 0.0489, 0.0489, 0.0489)
#f[,,,1,2] <- c(0.0112, 0.0289, 0.1094, 0.3012, 0.2465, 0.1829, 0.1081, 0.0794, 0.0665, 0.0587, 0.0538, 0.0507, 0.0488, 0.0476, 0.0470, 0.0466, 0.0465, 0.0465, 0.0465, 0.0465)

#f   <- yearMeans(fm(rep)[,as.character(frange),,,])

n   <- popN(rep)[,as.character(range(rep)['maxyear']-1),,,]
n   <- propagate(n, regions)

n2   <- popN(rep)[,as.character(range(rep)['maxyear']),,,]

pop <- FLQuant(0, dimnames=list(age=1:nages, year="all", unit="unique", season=1:ssns, area=1:regions, iter=1:regions))
Fmult <- 1

# calculate starting N values
#for(ii in 1:regions)
  for(qq in 1:ssns){
    for(aa in 1:nages)
      pop[aa,,,qq,] <- drop(n[aa,,,qq,])[,1] %*% t(diff_coffs_age_period(par)[,,aa,qq])
    for(aa in 2:nages)    
      pop[aa,,,qq,] <- pop[aa-1,,,qq,] * exp(-m[aa-1]-c(f[aa-1,,,qq,,]*Fmult))
  }


qq <- 1
barchart(data~age|area, group=qname, data=as.data.frame(FLQuants(me=pop[,,,qq,,1],mfcl=n2[,,,qq,,1])), horiz=F)



