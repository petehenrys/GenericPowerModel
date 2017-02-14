
############################################################################
#                                                                          #
#   Simulating a set of data in accordance with Freeman and Newson (2008)  #
#                                                                          # 
############################################################################

rm(list=ls());ls()
require(MASS)


nosite<-round(runif(1,50,200)) # no of sites
treat<-runif(1,0.01,0.9)       # proportion treated
tslope<-runif(1,-0.1,0.1)      # slope without treatment effect
alpha<-runif(1,-0.02,0)        # alpha, strength of the effect
noyear<-round(runif(1,5,20))   # number of years
miss<-runif(1,0.2,0.5)         #  proportion of visits missed

site<-rep(1:nosite,rep(noyear,nosite))        # site and year variables to fit model
year<-rep(1:noyear,nosite)

od<-runif(1,1.00001,10)        # 'overdispersion'

P<-rep(0,length(year))
P[site<=(nosite*treat)]<-1      #  Identifies which sites are treated
cump<-P*(year-1)                # Years of treatment to date 

k<-runif(1,0,5)                 # initial mean (log scale)
Var<-runif(1,0.1,4)             # variance of initial values 
sd<-sqrt(Var)

###########################################################################


int1<-(rnorm(nosite,k,sd))                    # Site effects
int<-rep(int1,rep(noyear,nosite))             # Initial counts, log scale
mu<-exp(int+tslope*(year-1)+alpha*cump)       # Poisson expectaions
size<-mu/(od-1)                               # To overdisperse data

yes<-rbinom(nosite*noyear,1,miss)               # a 1 means a missing value
count<-rnbinom(nosite*noyear,mu=mu,size=size)   # negative binomial to overdisperse data

# Identify those counts actually taken:

count<-count[yes<1]
ysite<-site[yes<1]
yyear<-year[yes<1]
ycump<-cump[yes<1]

# Fit model of Freeman and Newson (2008)

model<-glm(count~factor(ysite)+(yyear-1)+ycump,family=quasipoisson)
summary(model)

##############################################################################









