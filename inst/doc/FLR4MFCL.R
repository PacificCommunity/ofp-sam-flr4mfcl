## ---- echo=FALSE, include=FALSE-----------------------------------------------
library(FLR4MFCL)

## ---- echo=TRUE---------------------------------------------------------------
# create an instance of an MFCLFrq class using the MFCLFrq() constructor
myfrq <- MFCLFrq()

# get the slotnames from the class
slotNames(myfrq)
# use getSlots() for additional information 
getSlots('MFCLFrq')

## ---- echo=TRUE---------------------------------------------------------------
data(frq)
class(frq)

# use the slot accessor function to access the data in the 'n_fisheries' slot
n_fisheries(frq)

# use the slot accessor function to modify the data in the 'n_fisheries' slot
n_fisheries(frq) <- 5


## ---- echo=TRUE---------------------------------------------------------------
# the natural mortality at age data from the MFCLPar class is a good example of how slot accessors can improve access to data.
data(rep)
# using the slot accessor function
m_at_age(rep)

# using the slot method
slot(rep, 'm_at_age')

# using @
rep@m_at_age

## ---- echo=TRUE---------------------------------------------------------------
data(par)
class(par)
is(par)

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  myini <- read.MFCLIni('path_to/file.ini')
#  myfrq <- read.MFCLFrq('path_to/file.frq')
#  mytag <- read.MFCLTag('path_to/file.tag')
#  mypar <- read.MFCLPar('path_to/file.par', first.yr = range(myfrq)['minyear'])

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  write(frq, "path_to/my.frq")
#  write(tag, "path_to/my.tag")
#  # etc.

## ---- echo=TRUE, fig.width=7, fig.height=6, fig.fullwidth=TRUE----------------
data(rep)
xyplot(data~age|unit, data=sel(rep), type="l", xlab="Age", ylab="Selection")

