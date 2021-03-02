#q5
set.seed(100)

x1<-runif(1000,1,3)
x2<-rgamma(1000,3,2)
x3<-rbinom(1000,1,0.3)
eplison<-rnorm(1000,2,1)
y<-0.5+1.2*x1-0.9*x2+0.1*x3+eplison
meany<-mean(y)
ydum<-matrix(0,1,1000)
ydum[y>meany]<-1

#q6 OLS
corr<-cor(y,x1)
x<-cbind(1,x1,x2,x3)
ix<-t(x)
#beta
beta<-solve(t(x)%*%x)%*%t(x)%*%y

#standard error
error=y-x%*%beta
var_e<-t(error)%*%error/(1000-3-1)
var_e<-as.numeric(var_e)
k<-solve(t(x)%*%x)
var_b<-k*var_e
param<-cbind(beta,diag(sqrt(var_b)))

ydum<-as.numeric(ydum)

#question 7
# Probit

### Likelihood function
like_func_probit = function(par,x1,x2,x3,yvar)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  pr              = pnorm(xbeta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(like))
}
### Run Probit
set.seed(100)
start = runif(4)
prob = optim(start,fn=like_func_probit,method="BFGS",control=list(trace=6,REPORT=1,maxit=2000)
             ,x1=x1,x2=x2,x3=x3,yvar=ydum,hessian=TRUE)
fisher= solve(prob$hessian)
sigma  = sqrt(diag(fisher))
prob
sigma

# Logit
like_func_logit = function(par,x1,x2,x3,yvar)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  pr              = exp(xbeta)/(1+exp(xbeta))
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(like))
}
logit_ = optim(start,fn=like_func_logit,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000)
              ,x1=x1,x2=x2,x3=x3,yvar=ydum,hessian=TRUE)
fisher2 = solve(logit_$hessian)
sigma2  = sqrt(diag(fisher2))
logit_
sigma2

# Linear Probability
linear<-lm(ydum~x)

#question 8
probit<-glm(ydum~x1+x2+x3,family=binomial(link=probit),x=TRUE)
logit<-glm(ydum~x1+x2+x3,family=binomial(link=logit),x=TRUE)

maBina(w=probit,x.mean = FALSE, rev.dum = TRUE,digits=3)
maBina(w=logit,x.mean = FALSE, rev.dum = TRUE,digits=3)
