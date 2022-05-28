
###########################################
##
## Matt Vincent's spline functions
##
## Non-exported functions to support the par object slot accessors
## functions to calculate the value of natural mortality or selectivty from the values in the cubic spline. 
## this is useful if you are trying to line up which age selectivity should be maximized or or other stuff 
## with natural mortality
##
## 22/05/2022


## Internal function 1
spline <- function(x,y){
    yp1=0
    ypn=0
    n=length(x)
    y2=vector('numeric',n)
    u=vector('numeric',n-1)
    y2[1]=-.5
    u[1]=(3.0/(x[2]-x[1]))*((y[2]-y[1])/(x[2]-x[1])-yp1)
    for (i in 2:(n-1)) {
        y2[i]= (((x[i]-x[i-1])/(x[i+1]-x[i-1]))-1)/(((x[i]-x[i-1])/(x[i+1]-x[i-1]))*y2[i-1]+2)
        u[i]=( 6* ((y[i+1]-y[i])/(x[i+1]-x[i])-(y[i]-y[i-1])/(x[i]-x[i-1])) /(x[i+1]-x[i-1]) -((x[i]-x[i-1])/(x[i+1]-x[i-1])) *u[i-1])/(((x[i]-x[i-1])/(x[i+1]-x[i-1]))*y2[i-1]+2)
    }
    qn=0.5
    un = (3/(x[n]-x[n-1]))*(ypn-(y[n]-y[n-1])/(x[n]-x[n-1]))
    y2[n]=(un-qn*u[n-1])/(qn*y2[n-1]+1)
    for (k in (n-1):1) y2[k]=y2[k]*y2[k+1]+u[k]
    return(y2)
}
## Internal function 2
splint <- function(x,y,y2,u){
    n=length(x)
    klo=1
    khi=n
    while((khi-klo) >1) {
        k=floor((khi+klo)/2)
        if (x[k]>u) { khi=k
        }else { klo=k}
    }
    h=x[khi]-x[klo]
    a=(x[khi]-u)/h
    b=(u-x[klo])/h
    z = a*y[klo]+b*y[khi]+((a^3-a)*y2[klo]+(b^3-b)*y2[khi])*h^2/6
    return(z)
}

## Internal function 3
csf <- function(x,y,y2,u){
    z = numeric(length(u))
    for (i in 1:length(u)){
        z[i]=splint(x,y,y2,u[i])
    }
    return(z)
}

## function to calculate the natural mortality at age from the values in the par file
CalcMSpline <- function(MNodes,ages){
    nodes=length(MNodes)
    x = seq(0,1,length.out=nodes)
    xx = seq(0,1,length.out=ages)
    y2=spline(x,MNodes)
    MAge=csf(x,MNodes,y2,xx)
    return(exp(MAge))
}

## The functionality could probably be increased by automatically removing the zeros at the end or using parest flags 121
## Also if parest flag 122 is used to share M across some of the oldest ages then it will not match up
# Par=read.MFCLPar('/home/rob/MSE/skj/assessment/2019/Model_Runs/Length50_Growth20_5Region_Mix2_0.95/07.par')
# CalcMSpline(MNodes=as.vector(aperm(log_m(Par),c(4,1,2,3,5,6)))[1:5],ages=16)


## Function to calculate the selectivity of a fishery from the values in the par file or input from somewhere else

## nd = fish flag 3, if ==0 then # ages
## nodes = fish flags 61
## ff75 is fish flag 75
## k is from the von Bertalanfy growth curve
## tsel are the values stored in the fishery selectivity that are non-zero

CalcSelSpline <- function(nd,nodes,ff75,k,tsel){
    x = seq(0,1,length.out=nodes)
    xx = seq(0,1,length.out=nd)
    selmean = log(mean(exp(tsel)))
    tsel=tsel-selmean
    y=tsel
    tlength=vector('numeric',(nd-ff75))
    rho=exp(-k)          #exp(-k)
    div=1/(1-rho^(nd-1))
    tlength[1]=0
    for (j in 2:length(tlength)) tlength[j]=(1-rho^(j-1))*div
    y2=spline(x,tsel)
    tmp=csf(x,y,y2,tlength)
    sel=exp(csf(x,y,y2,tlength))
    sel=sel/max(sel)
    return(sel)
}

#tsel=c(-3.81918401667214e+00,  4.46434804757942e-01,  2.63720925992541e-01,  1.07560783456044e-01)
#nodes=4
#nd=15
#ff75=0
#k=2.14700000000000e-01
#CalcSelSpline(nd,nodes,ff75,k,tsel)
#plot(CalcSelSpline(nd,nodes,ff75,k,tsel))
#lines(c(0.0088,0.0919,0.8322,1.0000,0.6651,0.4870,0.4340,0.4233,0.4266,0.4334,0.4396,0.4442,0.4470,0.4484,0.4488,0.4488))

#Par <- read.MFCLPar('/home/rob/MSE/skj/assessment/2019/Model_Runs/Length50_Growth20_5Region_Mix2_0.95/07.par')

## Like the natural mortality this could probably become more functional if you read in the inputs directly from the par file for each fishery instead of inputting them manually as I did here.
## Additional functionality needed to match up with MFCL in all casses would be needed by including FF71 (time blocks) and FF74(seasonality)
