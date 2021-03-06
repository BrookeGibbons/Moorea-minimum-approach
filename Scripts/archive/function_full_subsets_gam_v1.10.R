###### fuction to fit full subsets gam #########################################
full.subsets.gam=function(use.dat,test.fit,pred.vars.cont,pred.vars.fact=NA,
                         cyclic.vars=NA,linear.vars=NA,size=3,cov.cutoff=0.28,k=5,parallel=F,n.cores=4,
                         weight.vec=rep(1,nrow(use.dat)),null.terms="",bs.arg="'cr'",
                         smooth.interactions=pred.vars.fact,
                         factor.interactions=F, max.models=500){
         # set defaults:  cyclic.vars=NA;size=3;cov.cutoff=0.28;k=5;parallel=F;n.cores=4
         #                weight.vec=rep(1,nrow(use.dat));null.terms="";bs.arg="'cr'";
         #                factor.interactions=F; max.models=500; linear.vars=NA;
         #                smooth.interactions=pred.vars.fact

  ### requires: doParallel, MuMIn, gamm4, mgcv, nnet
  ### arguments ################################################################
  # use.dat - the datset to use. This must contain all columns that are included
  #         in pred.vars.cont and pred.vars.fact, as well as a column "response"
  #         the contains the response variable to test (after applying any
  #         transformations if relevant
  # test.fit - a gam model fitted via a call to uGamm. This can use any of the
  #          (preferably continuous) predictors in the call and will be used
  #          as a model to update in the fitting. This must contain the appropriate
  #          random effects and call to family (if not gaussian) and if gamm4
  #          should be used, or gamm
  #          (see ? uGamm)
  # pred.vars.cont - NA if there are not factor predictors, or if there are factors,
  #          a character vector indicating the continuous predictors to
  #          to use (all continuous predictors will be fitted with a smooth).
  #          These must match column names in use.dat exactly.
  # pred.vars.fact - a character vector indicating the factor predictors to use.
  #          These must match column names in use.dat exactly.
  #          All pred.vars.fact variables will be coerced to a factor, with
  #          unused levels simultaneously dropped via a call to factor. All
  #          factor variables will be included as interactions with the continuous
  #          smoothers via their specification as a by argument in the call to gam.
  #          Factor interactions are not included. This may be possible by manually
  #          creating interacting factors from two or more factor variables using paste.
  #          This may cause issues with over specified models, but such models are not
  #          expected to be successfully fit and will be removed as failed models.
  #          inspection of the failed models list.
  # cyclic.vars - NA if there are no cyclic predictors, or if there are cyclic predictors,
  #          a character vector containing the names of any cyclic variables.
  #          note that these must also be contained in the pred.vars.cont character
  #          vector as a continuous predictor. Please also note there are
  #          issues with bs='cc' and model selection as this uses by default
  #          shrinkagee. With shrinkage, variables
  #          are retained in models but with zero edf, which makes interpretation of
  #          AICc and BIC confusing. To account for this always select only the
  #          most parsinomious model (that with the fewest parameters), not
  #          just that with the lowest AICc. Reported estimated degrees of freedom
  #          in the model output table represent the sum of the edf of the smooth
  #          terms plus the number of paramteric coefficients. When cyclic variables
  #          are included and shrinkage is used, any estimated edf of the smooth terms
  #          that are less than 1 are reset to 1 before summing to ensure the the
  #          total number of predictors in the model is captured properly.
  # linear.vars - NA if there are no continuous predictors you which to treat as
  #          linear predictors (no smooth)
  #          only use this where variables are clearly continuous in nature, but you are
  #          confident a linear relationship is adequate. It may also be useful
  #          for continuous predictors are not well distributed along the x-axis
  #          (ie, sampling was conducted in clumped distances from a feature of
  #          interest). Where this is necessary, transformations should be considered
  #          where they can be used to theoretical "linearlize" response relationships
  # size - An integer indicating the maximum model size to fit (ie the maximum
  #          number of predictors to include in any one model. Default is 3.
  # cov.cutoff - A numeric value between 0 and 1 indicating the covariance cutoff
  #          value to use for excluding colinear models.
  #          Defaults is 0.28 (see Graham MH (2003) CONFRONTING MULTICOLLINEARITY
  #          IN ECOLOGICAL MULTIPLE REGRESSION. Ecology 84:2809-2815. It is highly
  #          recommended to keep this value low, as correlation among predictors
  #          can yield spurious results. Note that predictors with a correlation
  #          greater than the specified value will still appear in the model set
  #          but will never appear in the same model. Including highly correlated
  #          predictors can make interpreting variable importance values difficult.
  # k - An integer indicationg the dimension of the basis used to represent the
  #          smooth term(see ?s).
  #          The default value is 5. Higher values are not recommended unless
  #          a complex trend between the response variable and the
  #          continuous predictor variables is expected, and the data are sufficient
  #          to support this. k can be reduced to as low
  #          as 3 where there is trouble obtaining convergence, or sample size is low.
  #          Note that this must be set to overide the default value, regardless
  #          of what k is used in the test.fit
  # parallel - A logical value indicating if parallel processing should be used.
  #          The default is FALSE. Before changing to TRUE,
  #          please make sure parallel processing works for you by runing the code:
  #          cl=makePSOCKcluster(2)
  #          registerDoParallel(cl)
  # n.cores - The number of cores to use in the parallel processing. The default value
  #          is 4.
  # weight.vec - the weights vector to use for passing n trials in a binomial call
  # null.terms -  A character vector indicating the form of any re smooths to be
  #          included in gam [e.g. "s(site,bs='re')"] or any other fixed terms or
  #          smooths that the user wants to include in the null model. Use of
  #          bs="re" is an alternative way of fitting simple random structures
  #          that avoids use of PQL and allows a the greater range of families
  #          available in gam.mgcv to be used. see ?s and links therin.
  #          Note: make sure you use "gam" instead of uGamm to make sure PQL is
  #          not used. Other use of the null.terms argument are to pass
  #          smooths for factors that the user want to ensure are contained in
  #          all models, including the "null". For example, there may be known
  #          effects of length on swimming speed, or depth on species abundances
  #          that do not form part of the ecological question being addressed
  #          but must be accounted for in model fits.
  # bs.arg - Default is 'cr'. Can be used to choose a different smoother, see ?s for more
  #          information on smoother provided in gam (mgcv)
  # smooth.interactions - A character vector specify if/which factor variables
  #          should be included as "by" argument interaction terms with the
  #          continuous predictors. Default is the list of pred.vars.fact, meaning
  #          that all will be included by default, otherwise only those included
  #          in the character vector will be added. Note that specified factors must
  #          also be included in pred.vars.fact. If specified as NA o factor
  #          interactions will be include.
  #          Note interactions are only included  where the cov.cutoff value is not
  #          exceeded based on the calculated all predictor correlation matrix.
  # factor.interactions - A logical value indicating if interactions between
  #          factors should be included.
  #          Default is 'F'. Note that this can massively increase the number of
  #          models in the canidate set. Not recomemnded when
  #          there are factors with many levels.
  # max.models - The total number of models allowed to be fit. If the candidate set
  #          is bigger than this value, an error message will be returned, asking
  #          the user to reset this to a larger value if they are sure they want to
  #          fit that many models. Default is 500,

  ### known issues -------------------------------------------------------------
  # This function assumes you know what you are doing. Non-gaussian mixed model
  # gamm resorts to PQL meaning that AICc calls will not return the AIC of the
  # actual model. Please thoroughly read the help files contained within the gamm4
  # and mgcv packages, including information under details.
  ### --------------------------------------------------------------------------

  ##### function to check colinearity among predictors ###########################
  check.correlations=function(dat){#,parallel=parallel,n.cores=n.cores){
    classes.dat=sapply(dat,class)
    fact.vars=names(which(classes.dat=="factor" | classes.dat=="character"))
    cont.vars=names(which(classes.dat=="integer" | classes.dat=="numeric"))
    if(length(cont.vars)>1){
     cor.mat=cor(dat[,cont.vars],use="pairwise.complete.obs")}else{
     cor.mat=matrix(1,ncol=1,nrow=1)
     colnames(cor.mat)=cont.vars
     rownames(cor.mat)=cont.vars}
    if(length(fact.vars)>0){
     if(length(cont.vars)>0){
      lm.grid=expand.grid(list(fact.var=fact.vars,cont.var=cont.vars))
      r.estimates=cbind(lm.grid,apply(lm.grid,MARGIN=1,FUN=function(x){
          sqrt(summary(lm(dat[,x[2]]~factor(dat[,x[1]])))$r.sq)}))

      fact.cont.upper.right=matrix(NA,ncol=length(fact.vars),nrow=length(cont.vars))
      colnames(fact.cont.upper.right)=fact.vars;rownames(fact.cont.upper.right)=cont.vars

      fact.cont.lower.left=matrix(NA,ncol=length(cont.vars),nrow=length(fact.vars))
      colnames(fact.cont.lower.left)=cont.vars;rownames(fact.cont.lower.left)=fact.vars

      fact.fact.lower.right=matrix(NA,ncol=length(fact.vars),nrow=length(fact.vars))
      colnames(fact.fact.lower.right)=fact.vars;rownames(fact.fact.lower.right)=fact.vars

      out.cor.mat=rbind(cbind(cor.mat,fact.cont.upper.right),
                        cbind(fact.cont.lower.left,fact.fact.lower.right))

      # assign the estimated r values to the upper right and lower left corners
      for(r in 1:nrow(r.estimates)){
         # upper right
         col.index=which(colnames(out.cor.mat)==r.estimates$fact.var[r])
         row.index=which(rownames(out.cor.mat)==r.estimates$cont.var[r])
         out.cor.mat[row.index,col.index]=r.estimates[r,3]
         # lower left
         col.index=which(colnames(out.cor.mat)==r.estimates$cont.var[r])
         row.index=which(rownames(out.cor.mat)==r.estimates$fact.var[r])
         out.cor.mat[row.index,col.index]=r.estimates[r,3]
      }}else{
          fact.fact.lower.right=matrix(NA,ncol=length(fact.vars),nrow=length(fact.vars))
      colnames(fact.fact.lower.right)=fact.vars;rownames(fact.fact.lower.right)=fact.vars
      out.cor.mat=fact.fact.lower.right}

    # estimate r values for fact-fact combinations
    lm.grid=expand.grid(list(fact.var1=fact.vars,fact.var2=fact.vars))
    require(nnet)
    require(doParallel)
    if(parallel==T){
     cl=makePSOCKcluster(n.cores)
     registerDoParallel(cl)
     out.cor.dat<-foreach(r = 1:nrow(lm.grid),.packages=c('nnet'),.errorhandling='pass')%dopar%{
      var.1=as.character(lm.grid[r,1])
      var.2=as.character(lm.grid[r,2])
      dat.r=na.omit(dat[,c(var.1,var.2)])
      fit <- try(summary(multinom(dat.r[,var.1] ~ dat.r[,var.2],trace=F))$deviance,silent=T)
      null.fit=try(summary(multinom(dat[,var.1] ~ 1,trace=F))$deviance,silent=T)
      if(class(fit)!="try-error"){
        r.est=sqrt(1-(fit/null.fit))
        c(var.1,var.2,r.est)}}
     stopCluster(cl)
     registerDoSEQ()}else{
      out.cor.dat<-foreach(r = 1:nrow(lm.grid),.packages=c('nnet'),.errorhandling='pass')%do%{
      var.1=as.character(lm.grid[r,1])
      var.2=as.character(lm.grid[r,2])
      dat.r=na.omit(dat[,c(var.1,var.2)])
      fit <- try(summary(multinom(dat.r[,var.1] ~ dat.r[,var.2],trace=F))$deviance,silent=T)
      null.fit=try(summary(multinom(dat[,var.1] ~ 1,trace=F))$deviance,silent=T)
      if(class(fit)!="try-error"){
        r.est=sqrt(1-(fit/null.fit))
        out=c(var.1,var.2,r.est)}}}

      for(r in 1:length(out.cor.dat)){
         out.cor.mat[which(colnames(out.cor.mat)==out.cor.dat[[r]][1]),
                     which(rownames(out.cor.mat)==out.cor.dat[[r]][2])]=
                     as.numeric(out.cor.dat[[r]][3])}}else{out.cor.mat=cor.mat}
    return(out.cor.mat)

  } #------------------ end function to check colinearity among predictors ----#

  # make an "intercept" term for the null model
  use.dat$intercept=1
  interaction.terms=NA
  linear.interaction.terms=NA
  all.predictors=na.omit(c(pred.vars.cont,pred.vars.fact,linear.vars))

  # check the null model will fit
  if(length(null.terms)==0){
    null.formula=as.formula(paste("~ intercept-1",null.terms,sep="+"))}else{
    null.formula=as.formula("~ intercept-1")}
  null.fit=try(update(test.fit,formula=null.formula,
                data=use.dat),silent=T)
  if(class(null.fit)[1]=="try-error"){
        stop(paste("Null model not successfully fitted, please check your inputs.
                   If there are no random effects try using 'gam' instead of 'uGamm' 
                   in your test.fit model call.",
                   "  ",
                   "The following error message was provided:  ",
                   "  ",
                   null.fit, ""))}
  # check for missing predictor values
  if(max(is.na(use.dat[,all.predictors]))==1){
        stop("Predictor variables contain NA and AICc/BIC comparisons are invalid. 
        Remove rows with NA from the input data or interpolate missing predictors.")}

  # if there are factors
  if(length(na.omit(pred.vars.fact))>0){
  # if there are more than two factors
  if(length(na.omit(pred.vars.fact))>1){
    # make all the interactions between factors
    if(factor.interactions==T){
        if(length(pred.vars.fact)<2){
            stop("You have less than 2 factors. Please reset 'factor.interactions' to 'False'")}

      fact.combns=list()
      fact.cmbns.size=size
      if(size>length(pred.vars.fact)){fact.cmbns.size=length(pred.vars.fact)}
      for(i in 2:fact.cmbns.size){
        if(i<=length(pred.vars.fact)){
        fact.combns=c(fact.combns,
         combn(pred.vars.fact,i,simplify=F)) }}
        tt=data.frame(lapply(fact.combns,FUN=function(x){
                   do.call("paste",as.list(use.dat[,x]))}))
        factor.interaction.terms=unlist(lapply(fact.combns,FUN=paste,collapse=".I."))
        colnames(tt)=factor.interaction.terms
      use.dat=cbind(use.dat,tt)
      pred.vars.fact=c(pred.vars.fact,factor.interaction.terms)
    }
   }
   # make sure the factors are factors
   for(f in 1:length(pred.vars.fact)){
       use.dat[,pred.vars.fact[f]]=factor(use.dat[,pred.vars.fact[f]])}

   # check which ones should be included as interactions with the smoothers
   smooth.interactions=pred.vars.fact[which(unlist(lapply(strsplit(pred.vars.fact,
      split=".I."),function(x){
      max(is.na(match(x,smooth.interactions)))}))==0)]

   # make the interaction terms between the factors and continuous predictors
   if(length(na.omit(smooth.interactions))>0){
    all.interactions=expand.grid(setdiff(pred.vars.cont,linear.vars),smooth.interactions)
    interaction.terms=paste(all.interactions$Var1,all.interactions$Var2,sep=".by.")

    # now interactions between continous predictors and factors
    if(length(na.omit(linear.vars))>0){
     linear.interactions=expand.grid(linear.vars,smooth.interactions)
     linear.interaction.terms=paste(linear.interactions$Var1,linear.interactions$Var2,
                                sep=".t.")}
   }}

  all.predictors=na.omit(unique(c(all.predictors,pred.vars.fact)))
  # calculate a correlation matrix between all predictors
  cor.matrix=check.correlations(use.dat[,all.predictors])
  # replace NA's with zero.
  cor.matrix[which(cor.matrix=="NaN")]=0

  # make all possible combinations
  if(length(na.omit(c(pred.vars.cont,
                      pred.vars.fact)))<size){
        stop("Model size is greater than the number of predictors.")}
  all.mods=list()
  for(i in 1:size){
    all.mods=c(all.mods,
     combn(na.omit(c(pred.vars.cont,pred.vars.fact,
                     interaction.terms,linear.interaction.terms)),
                     i,simplify=F))
  }

  # remove redundant models
  use.mods=all.mods
  for(m in 1:length(all.mods)){
    mod.m=all.mods[[m]]
    mod.terms=unlist(strsplit(unlist(strsplit(mod.m,split=".by.",fixed=T)),
                               split=".t.",fixed=T))
    n.vars.m=unique(unlist(strsplit(unlist(strsplit(unlist(strsplit(mod.m,split=".by.",fixed=T)),
                                  split=".I.",fixed=T)), split=".t.",fixed=T)))
    cont.vars=na.omit(na.omit(c(pred.vars.cont,linear.vars))[match(mod.terms,na.omit(c(pred.vars.cont,linear.vars)))])
    fact.vars=unique(na.omit(pred.vars.fact[match(mod.terms,pred.vars.fact)]))

    # if there are factor vars
    if(length(fact.vars)>0){
      # check that any "by" factor vars are accompanied by a + term in its owns right
      if(max(is.na(match(fact.vars,mod.m)))==1){use.mods[[m]]=NA}}

    # remove the model if the predictors are correlated
    if(length(mod.terms)>1){
     row.index=which(match(rownames(cor.matrix),unique(mod.terms))>0)
     col.index=which(match(colnames(cor.matrix),unique(mod.terms))>0)
     cor.mat.m=cor.matrix[row.index,col.index]
     if(max(abs(cor.mat.m[upper.tri(cor.mat.m)]))>cov.cutoff){use.mods[[m]]=NA}
    }

    # remove the model if there are more than the number of terms specified in "size"
    if(length(n.vars.m)>size){use.mods[[m]]=NA}

    # remove the models if a continuous predictor occurs as a by and as a single term
    if(length(cont.vars)>length(unique(cont.vars))){use.mods[[m]]=NA}
  }

  use.mods[which(is.na(use.mods))]=NULL

  # now make the models into gamm formula
  if(nchar(null.terms)==0){# if there is no bs='re' random effect random effect
                             # or other null term in the null model
    mod.formula=list(as.formula("~ intercept-1"))}
  if(nchar(null.terms)>0){# to add a bs='re' random effect
    mod.formula=list(null.formula)}

  #use.mods=lapply(use.mods,FUN=function(x){gsub(".t.","*",x,fixed=T)})
  for(m in 1:length(use.mods)){
     mod.m=use.mods[[m]]
     cont.smooths=mod.m[which(match(mod.m,setdiff(pred.vars.cont,linear.vars))>0)]
     by.smooths=mod.m[grep(".by.",mod.m)]
     factor.terms=mod.m[which(match(mod.m,pred.vars.fact)>0)]
     linear.terms=mod.m[which(match(mod.m,linear.vars)>0)]
     linear.interaction.terms=mod.m[grep(paste(linear.vars,".t.",sep=""),mod.m)]
     all.terms.vec=character()

     if(length(cont.smooths>0)){all.terms.vec=c(all.terms.vec,
                  paste("s(",cont.smooths,",k=",k,",bs=",bs.arg,")",sep=""))}
     if(length(by.smooths>0)){all.terms.vec=c(all.terms.vec,
         paste("s(",gsub(".by.",",by=",by.smooths),",k=",k,",bs=",bs.arg,")",sep=""))}
     if(length(linear.interaction.terms>0)){all.terms.vec=c(all.terms.vec,
               gsub(".t.","*",linear.interaction.terms,fixed=T))}
     if(length(factor.terms>0)){all.terms.vec=c(all.terms.vec,factor.terms)}
     if(max(is.na(cyclic.vars))!=1){
       for(cc in 1:length(cyclic.vars)){
           for(v in 1:length(all.terms.vec)){
             if(length(grep(cyclic.vars[cc],all.terms.vec[v]))>0){
                  all.terms.vec[v]=gsub(paste("bs=",bs.arg,sep=""),"bs='cc'",all.terms.vec[v])
                  }}}}
     if(nchar(null.terms)==0){# if there is no bs='re' random effect
                                # or other null term in the null model
       formula.m=as.formula(paste("~",
               paste(all.terms.vec,collapse="+")))}
     if(nchar(null.terms)>0){# to add a bs='re' random effect
       formula.m=as.formula(paste("~",
               paste(c(all.terms.vec,null.terms),collapse="+")))}
     #out=list(formula.m=formula.m,test.fit=test.fit,use.dat=use.dat)
     #mod.formula=c(mod.formula,list(out))
     mod.formula=c(mod.formula,list(formula.m))
  }

  names(mod.formula)=c("null",lapply(use.mods,FUN=paste,collapse="+"))

  # Is this too many models?
  n.mods=length(mod.formula)
  time.to.run=round(system.time(try(update(test.fit,formula=mod.formula[[n.mods]],data=use.dat),silent=T))[3]*n.mods/60)
  test.mod=try(update(test.fit,formula=mod.formula[[n.mods]],data=use.dat),silent=T)
  mod.gbs=round(object.size(test.mod)/1073741824*n.mods,1)
  if(n.mods>max.models){
        stop(paste("You have ",n.mods," models. If you want to fit all of these you need to
        increase 'max.models' from ",max.models,". Otherwise, if the model set
        is larger than you can realistically fit, try reducing the number of predictors,
        setting the covariance 'cov.cutoff' argument to less than ", cov.cutoff,
        "
        or setting 'factor.interactions' to FALSE (if you have factors).

        To run the current model set will take approximately ",time.to.run," minutes
        and use ",mod.gbs," gigabytes of additional memory.",sep=""))
       }

  # now fit the models by updating the test fit (with or without parallel)
  require(doParallel)
  if(parallel==T){
   cl=makePSOCKcluster(n.cores)
   registerDoParallel(cl)
   out.dat<-foreach(l = 1:length(mod.formula),
                   .packages=c('mgcv','gamm4','MuMIn'),
                   .errorhandling='pass')%dopar%{
           out=update(test.fit,formula=mod.formula[[l]],data=use.dat)}
   stopCluster(cl)
   registerDoSEQ()
           }else{
   out.dat<-foreach(l = 1:length(mod.formula),
                   .packages=c('mgcv','gamm4','MuMIn'),
                   .errorhandling='pass')%do%{
           out=update(test.fit,formula=mod.formula[[l]],data=use.dat)}
           }

  names(out.dat)=names(mod.formula[1:n.mods])

  # find all the models that didn't fit and extract the error messages
  model.success=lapply(lapply(out.dat,FUN=class),FUN=function(x){
     x[1]=="gamm" | x[1]=="gamm4" | x[1]=="gam"})
  failed.models=mod.formula[which(model.success==F)]
  success.models=out.dat[which(model.success==T)]
  if(length(success.models)==0){
        stop("None of your models fitted successfully. Please check your input objects.")}

  # some functions for extracting model information
  require(MuMIn)
  wi<<-function(AIC.vals){# This function calculate the Aikaike weights:
   # wi=(exp(-1/2*AICc.vals.adj))/Sum.wi=1 to r (exp(-1/2*AICc.vals.adj))
   AICc.vals.adj=AIC.vals-min(na.omit(AIC.vals))
   wi.den=rep(NA,length(AICc.vals.adj))
   for(i in 1:length(AICc.vals.adj)){
    wi.den[i]=exp(-1/2*AICc.vals.adj[i])}
   wi.den.sum=sum(na.omit(wi.den))
   wi=wi.den/wi.den.sum
   return(wi)}

  # of the successful models, make a table indicating which variables are included
  var.inclusions=matrix(0,ncol=length(all.predictors),length(success.models))
  colnames(var.inclusions)=all.predictors
  for(m in 1:length(success.models)){
        pred.vars.m=unique(
          unlist(strsplit(unlist(strsplit(unlist(strsplit(unlist(strsplit(unlist(strsplit(names(success.models)[m],
          split="+",fixed=T)),split=".by.",fixed=T)),split=".I.",fixed=T)),
          split="*",fixed=T)),split=".t.",fixed=T)))
        if(pred.vars.m[1]!="null"){var.inclusions[m,pred.vars.m]=1}}

  # now make a table of all the model summary data
  mod.data.out=data.frame("modname"=names(success.models))
  mod.data.out$AICc=unlist(lapply(success.models,FUN=AICc))
  mod.data.out$BIC=unlist(lapply(success.models,FUN=BIC))
  mod.data.out$delta.AICc=round(mod.data.out$AICc-min(mod.data.out$AICc),3)
  mod.data.out$delta.BIC=round(mod.data.out$BIC-min(mod.data.out$BIC),3)
  mod.data.out$wi.AICc=round(wi(mod.data.out$AICc),3)
  mod.data.out$wi.BIC=round(wi(mod.data.out$BIC),3)
  mod.data.out$r2.vals=round(unlist(lapply(success.models,FUN=function(x){
        if(class(x)[1]=="gam"){out=summary(x)$dev.expl}
        #if(class(x)[1]=="gam"){summary(x$gam)$r.sq}
        if(class(x)[[1]]=="gamm4"){
           out=summary(lm(attributes(x$mer)$frame$y~predict(x,re.form=NA)))$r.sq}
        return(out)})),3)
        # if "gam" is used get the explained deviance "dev.expl"
        # if "gamm" or "gamm4" get the r.sq (dev.expl is empty).
        # note that these are adjusted r.sq values and may be negative.
  ## not implemented ##
  # substract the null model r2 value from each model r2 value
  #null.r2=mod.data.out$r2.vals[which(mod.data.out$modname=="null")]
  #mod.data.out$r2.vals.mod=mod.data.out$r2.vals-null.r2
     # note this only really effects models with the formulation "bs='re'"
     # for random effects - for the most part reported r square values
     # are zero for the null model anyway.
  # now calculate the summed edf
  mod.data.out$edf=round(unlist(lapply(success.models,FUN=function(x){
        if(class(x)[1]=="gam"){
          edf.m=summary(x)$edf
          p.coeff.m=summary(x)$p.coeff}else{
           edf.m=summary(x$gam)$edf
           p.coeff.m=summary(x$gam)$p.coeff}
        edf.m[which(edf.m<1)]=1 # any edf<0 are reset to 1 to ensure proper
                                # parameter count when there is shrinkage (bs='cc')
        return(sum(c(edf.m,length(p.coeff.m))))})),2)
  # count the edf values less than 0.25 to check for serious shrinkage
  mod.data.out$edf.less.1=unlist(lapply(success.models,FUN=function(x){
        if(class(x)[1]=="gam"){edf.m=summary(x)$edf}else{edf.m=summary(x$gam)$edf}
        return(length(which(edf.m<0.25)))}))
  # now add columns for the included predictors to the dataframe
  mod.data.out=cbind(mod.data.out,var.inclusions)

  # now calculate the variable imporants
  # first for AICc
  variable.weights.raw=colSums(mod.data.out[,all.predictors]*mod.data.out$wi.AICc)
  variable.weights.per.mod=variable.weights.raw/colSums(mod.data.out[,all.predictors])
  variable.weights.r2.scaled=variable.weights.per.mod/
                             max(variable.weights.per.mod)*
                             max(mod.data.out$r2.vals)
  aic.var.weights=list(variable.weights.raw=variable.weights.per.mod,
                       variable.weights.per.mod=variable.weights.per.mod,
                       variable.weights.r2.scaled=variable.weights.r2.scaled)
  # next for BIC
  variable.weights.raw=colSums(mod.data.out[,all.predictors]*mod.data.out$wi.BIC)
  variable.weights.per.mod=variable.weights.raw/colSums(mod.data.out[,all.predictors])
  variable.weights.r2.scaled=variable.weights.per.mod/
                             max(variable.weights.per.mod)*
                             max(mod.data.out$r2.vals)
  bic.var.weights=list(variable.weights.raw=variable.weights.per.mod,
                       variable.weights.per.mod=variable.weights.per.mod,
                       variable.weights.r2.scaled=variable.weights.r2.scaled)
  # now return the list of outputs
  return(list(mod.data.out=mod.data.out,
              used.data=use.dat,
              predictor.correlations=cor.matrix,
              failed.models=failed.models,
              success.models=success.models,
              variable.importance=
                 list(aic=aic.var.weights,bic=bic.var.weights)))
}

###- change log------------------------------------------------------------------
# v1.2 added error catching in the call to cor where predictor variables contain NA.
# to the call to cor in case there are missing
# predictors. Note that there should be no missing predictors or AIC comparisons
# will not be valid
# v1.4 added the capacity to cope with cyclic predictors.
# v1.4 - added the capacity to take models with no factor variables. Also removed
# any dependency on library plyr as there may be issues with version control
# all apply arguments are now based on the set shipped with base R.
# v1.5 - removed the conversion to shrinkage for non-cyclic variables when
# cyclic variables are present. There is some uncertiainty in the best approach
# here, but this seems to safest option if variable importance calculations
# are to remain valid. Note that for any cyclic variables, importance should
# only be interpreted if it is clear that the cyclic variables are not being reset
# to zero for any of the models.  see edf.less.1 count in the model table output to check
# how many terms were set to near zero.
# v1.8 - added error trapping to protect against people using uGamm when there are
# no random effects. In this instance factor only models will break.
# v1.9 - added the capacity to model linear continuous predictors if required and
# to specify which factors should be included in interactions with the continuous
# predictors.
# v1.10 - changes the s.re argument to null.terms, and used this to allow
# other factors and smoothers to be passed through to the null model.

# TO DO LIST:
# write code to check if cyclic variables are being "shrunk" and provide
# a warning against interpreting variable importance.


