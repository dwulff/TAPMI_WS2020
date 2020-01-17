

options(stringsAsFactors = F)
require(googlesheets)
require(choicepp)


data = readRDS('~/Dropbox (2.0)/Work/Software/Shiny/exploreGap/data/data.RDS')

# experience
p_test = data$test[,c('outA1','outA2','probA1','probA2','outB1','outB2','probB1','probB2')]
pa_test = as.matrix(do.call(rbind,lapply(1:50,function(x) p_test)))
pid_test = data$test$pid ; pid_test = rep(pid_test,50)
p_ns = table(pid_test) / length(pid_test)

pa_test = p_arrange(pa_test,2)

# experience
p_test = data$test[,c('outA1','outA2','probA1','probA2','outB1','outB2','probB1','probB2')]
#pa_test_s = as.matrix(do.call(rbind,lapply(1:1,function(x) p_test)))
pid_test_s = data$test$pid ; pid_test = rep(pid_test,1)
p_ns_s = table(pid_test_s) / length(pid_test_s)

pa_test_s = p_arrange(as.matrix(p_test),2)

tab = paste0('Exploring Decisions from Experience, January 2020 (Antworten)')

gs = gs_title(tab)
d = gs_read(gs)
d = as.data.frame(d)

d$Noise[d$Noise > 1] = d$Noise[d$Noise > 1] / 1000

# d$Alpha = as.numeric(gsub(',','.',d$Alpha))
# d$Recency = as.numeric(gsub(',','.',d$Recency))

# some corrections
# d$Noise[c(7,16,18,20)] = d$Noise[c(7,16,18,20)] / 1000

mses = c()
for(i in 1:nrow(d)){

  print(i)
    
  mean_n = d$`Per option sample size`[i]
  cpt_par = c(d$Alpha[i],d$Lambda[i],d$Gamma[i])
  noise = d$Recency[i]
  recency = d$Noise[i]
  
  # draw sample sizes
  lambda = 1/(mean_n-1)
  n = length(pid_test)
  sizes = sample(rexp(n,lambda))
  sizes = sizes[sample(n)]
  sizes = ceiling(sizes - .5) + 1
  smplz = sampl_n(pa_test,sizes)
  
  # edit experience
  exp_p = choicepp:::edit_exp_pos(smplz,pa_test,add_n = T,do_arrange = T)
  
  # choose
  choices = choicepp:::cpt_choice_pos(cpt_par,exp_p,type = 1,noise,recency)
  
  # eval fit
  crit = tapply(data$test$choice,data$test$pid,mean)
  pred = tapply(choices,pid_test,mean)
  
  mses[i] = sum((crit - pred)**2 * p_ns[names(crit)])
  
  }


###### Exercise results<

pdf('~/Dropbox (2.0)/Work/Teaching/Workshops/DfEWorkshop/xploreParticipantRes.pdf')
  plot_xplore(as.matrix(d[-c(1,2,3),-(1:2)]),mses[-c(1,2,3)],alpha=1,q=0)
  dev.off()

winners = d[which(mses == min(mses)),2]
winners


d[which(mses == min(mses)),]




########## ----------- OLD STUFF -------- #################


splot_xplore = function(pars, mses, alpha = .1, q = .001){

  par(mfrow=c(3,2),mar=c(3,2,1,1))
  
  sel = which(mses <= quantile(mses,q))
  
  plot.new();plot.window(xlim=c(1,40),c(0,.15))
  points(pars[,1],mses,pch=16,col=rgb(0,0,0,alpha=alpha))
  mtext(c('Sample size','MSE'),side=c(1,2),line=c(2,.5))
  mtext(c(1,seq(10,40,10)),at=c(1,seq(10,40,10)),side=1,cex=.8)
  points(pars[sel,1],mses[sel],pch=16,col='red',cex=1.5)
  
  plot.new();plot.window(xlim=c(0,2),c(0,.15))
  points(pars[,2],mses,pch=16,col=rgb(0,0,0,alpha=alpha))
  mtext(c('Alpha','MSE'),side=c(1,2),line=c(2,.5))
  mtext(seq(0,2,.5),at=seq(0,2,.5),side=1,cex=.8)
  points(pars[sel,2],mses[sel],pch=16,col='red',cex=1.5)
  
  plot.new();plot.window(xlim=c(1,3),c(0,.15))
  points(pars[,3],mses,pch=16,col=rgb(0,0,0,alpha=alpha))
  mtext(c('Lambda','MSE'),side=c(1,2),line=c(2,.5))
  mtext(seq(1,3,.5),at=seq(1,3,.5),side=1,cex=.8)
  points(pars[sel,3],mses[sel],pch=16,col='red',cex=1.5)
  
  plot.new();plot.window(xlim=c(0,2),c(0,.15))
  points(pars[,4],mses,pch=16,col=rgb(0,0,0,alpha=alpha))
  mtext(c('Gamma','MSE'),side=c(1,2),line=c(2,.5))
  mtext(seq(0,2,.5),at=seq(0,2,.5),side=1,cex=.8)
  points(pars[sel,4],mses[sel],pch=16,col='red',cex=1.5)
  
  plot.new();plot.window(xlim=c(-1,1),c(0,.15))
  points(pars[,5],mses,pch=16,col=rgb(0,0,0,alpha=alpha))
  mtext(c('Recency','MSE'),side=c(1,2),line=c(2,.5))
  mtext(seq(-1,1,.5),at=seq(-1,1,.5),side=1,cex=.8)
  points(pars[sel,5],mses[sel],pch=16,col='red',cex=1.5)
  
  plot.new();plot.window(xlim=c(0,1),c(0,.15))
  points(pars[,6],mses,pch=16,col=rgb(0,0,0,alpha=alpha))
  mtext(c('Noise','MSE'),side=c(1,2),line=c(2,.5))
  mtext(seq(0,1,.25),at=seq(0,1,.25),side=1,cex=.8)
  points(pars[sel,6],mses[sel],pch=16,col='red',cex=1.5)
  }



mean_n = 7
cpt_par = c(1,1,1)
noise = .1
recency = .5

# draw sample sizes
lambda = 1/(mean_n-1)
n = length(pid_test)
sizes = sample(rexp(n,lambda))
sizes = sizes[sample(n)]
sizes = ceiling(sizes - .5) + 1
smplz = sampl_n(pa_test,sizes)

# edit experience
exp_p = choicepp:::edit_exp_pos(smplz,pa_test,add_n = T,do_arrange = T)

# choose
choices = choicepp:::cpt_choice_pos(cpt_par,exp_p,type = 1,noise,recency)

# eval fit
crit = tapply(data$test$choice,data$test$pid,mean)
pred = tapply(choices,pid_test,mean)

mse_opt = sum((crit - pred)**2 * p_ns[names(crit)])

sum(mses<mse_opt)

dev.off()

pdf('~/Dropbox (2.0)/Work/Teaching/Workshops/DfEWorkshop/xploreSimMinimum.pdf',width=6,height=5)
hist(mses,las=1,col='grey75',border='white',main='',xlim=c(0,0.15),breaks=100,probability = T,xlab='MSE')
lines(c(mse_opt,mse_opt),c(0,48),lwd=2,col='red',lty=2)
text(mse_opt,50,label='Minimal model',col='red')
dev.off()



##### go through space

pars = cbind(round(rexp(10000,1/(7-1))+1),
             runif(10000,0,2),
             runif(10000,1,3),
             runif(10000,0,2),
             runif(10000,-1,1),
             runif(10000,0,1))


mses = c()
for(i in 1:nrow(pars)){
  
  print(i)
  
  mean_n = pars[i,1]
  cpt_par = c(pars[i,2],pars[i,3],pars[i,4])
  noise = pars[i,5]
  recency = pars[i,6]
  
  # draw sample sizes
  lambda = 1/(mean_n-1)
  n = length(pid_test_s)
  sizes = sample(rexp(n,lambda))
  sizes = sizes[sample(n)]
  sizes = ceiling(sizes - .5) + 1
  smplz = sampl_n(pa_test_s,sizes)
  
  # edit experience
  exp_p = choicepp:::edit_exp_pos(smplz,pa_test_s,add_n = T,do_arrange = T)
  
  # choose
  choices = choicepp:::cpt_choice_pos(cpt_par,exp_p,type = 1,noise,recency)
  
  # eval fit
  crit = tapply(data$test$choice,data$test$pid,mean)
  pred = tapply(choices,pid_test_s,mean)
  
  mses[i] = sum((crit - pred)**2 * p_ns[names(crit)])
  
}

res = list(pars,mses)
saveRDS(res,'~/Dropbox (2.0)/Work/Teaching/Workshops/DfEWorkshop/xploreSimRes.RDS')


res = readRDS('~/Dropbox (2.0)/Work/Teaching/Workshops/DfEWorkshop/xploreSimRes.RDS')
pars = res[[1]]
mses = res[[2]]


pdf('~/Dropbox (2.0)/Work/Teaching/Workshops/DfEWorkshop/xploreSimRes.pdf',width=6,height=6)
plot_xplore(pars,mses)
dev.off()
