# rm(list = ls())
# library(data.table)
# 
# set.seed(1)
# n = 1000
# df = data.table(groups = rbinom(n, size = 1, prob = .3))
# df$x = exp(rnorm(n, mean = 6 + df$groups, sd = exp(.2 + df$groups)))
# df$w = rpois(n, lambda = 2*df$groups) + 1
# 
#
# x      = df$x
# groups = df$groups
# w      = df$w

#' also check package wINEQ
#' https://cran.r-project.org/web/packages/wINEQ/wINEQ.pdf

theil_t = function(x, w){
        
        if(is.null(w)){
                w = rep(1, length(x))
        }
        
        complete = complete.cases(x, w) & (x > 0)
        x_c = x[complete]
        w_c = w[complete]
        
        X_bar = weighted.mean(x =  x_c, w = w_c)
        
        theil_t <- weighted.mean((x_c/X_bar) * log(x_c/X_bar), w = w_c)
        return(theil_t)
}



decomp_theil_t = function(x, groups, w = NULL){
  
  if(is.null(w)){
    w = rep(1, length(x))
  }
  
  complete = complete.cases(x, groups, w) & (x > 0)
  
  d = data.table(x_c      =      x[complete],
                 groups_c = groups[complete],
                 w_c      =      w[complete])
  
  mean_x  = weighted.mean(d$x_c, d$w_c)
  total_x = sum(d$x_c * d$w_c)
  N       = sum(d$w_c)
  
  d_summary = d[, .(s_i       = sum(x_c * w_c)/total_x,
                    mean_x_i  = weighted.mean(x_c, w_c),
                    theil_t_i = theil_t(x = x_c, w = w_c)),
                by = groups_c]
  
  within  = d_summary[, sum(s_i * theil_t_i)]
  between = d_summary[, sum(s_i * log(mean_x_i/mean_x))]
  
  theilt = theil_t(x, w)
  main_components = data.table(component    = c("within", "between", "total"),
                               value        = c(within, between, theil_t(x, w)),
                               contribution = c(within/theil_t(x, w), between/theil_t(x, w), 1))
  
  within_groups_details = data.table(group        = d_summary$groups_c,
                                     within_i     = d_summary$s_i *d_summary$theil_t_i,
                                     contribution_to_within = (d_summary$s_i *d_summary$theil_t_i)/within,
                                     contribution_to_total  = (d_summary$s_i *d_summary$theil_t_i)/theil_t(x, w))
  temp_list <- list( # 'theil_t'=theilt,
                    'main_components' = main_components,
                    'within_groups_details' = within_groups_details)
  return(temp_list)
}



theil_l = function(x, w = NULL){
        
        if(is.null(w)){
                w = rep(1, length(x))
        }
        
        complete = complete.cases(x, w) & (x > 0)
        x_c = x[complete]
        w_c = w[complete]
        
        X_bar = weighted.mean(x =  x_c, w = w_c)
        
        weighted.mean(log(X_bar/x_c), w = w_c)
}

gini = function(x, w = NULL){
        
        if(is.null(w)){
                w = rep(1, length(x))
        }
        
        complete = complete.cases(x, w) & (x > 0)
        x_c = x[complete]
        w_c = w[complete]
        
        o = order(x_c)
        x_o = x_c[o]
        w_o = w_c[o]
        
        x_cum = c(0, cumsum(x_o * w_o)/sum(x_o * w_o))
        w_cum = c(0, cumsum(w_o)/sum(w_o))
        
        b = x_cum[-length(x_cum)]
        B = x_cum[-1]
        h = diff(w_cum)
        
        area_under_lorenz = sum( ((B + b)*h)/2 )
        
        1 - 2*area_under_lorenz
        
}

concentr = function(x, y, w = NULL){
  
  if(is.null(w)){
    w = rep(1, length(x))
  }
  
  complete = complete.cases(x, y, w) & (x >= 0) & (y >= 0)
  x_c = x[complete]
  y_c = y[complete]
  w_c = w[complete]
  
  o = order(y_c)
  x_o = x_c[o]
  y_o = x_c[o]
  w_o = w_c[o]
  
  x_cum = c(0, cumsum(as.numeric(x_o * w_o)/sum(x_o * w_o)))
  w_cum = c(0, cumsum(as.numeric(w_o)/sum(w_o)))
  
  b = x_cum[-length(x_cum)]
  B = x_cum[-1]
  h = diff(w_cum)
  
  area_under_concentrationCurve = sum( ((B + b)*h)/2 )
  
  1 - 2*area_under_concentrationCurve
  
}


decomp_concentr = function(x, y, nonOverlaping_groups, w = NULL){
  
  groups = nonOverlaping_groups
  
  if(is.null(w)){
    w = rep(1, length(x))
  }
  
  complete = complete.cases(x, y, w) & (x >= 0) & (y >= 0)
  x_c = x[complete]
  y_c = y[complete]
  w_c = w[complete]
  groups_c = groups[complete]
  
  o = order(y_c)
  x_o = x_c[o]
  y_o = y_c[o]
  w_o = w_c[o]
  groups_o = groups_c[o]
  
  x_cum = c(0, cumsum(x_o * w_o)/sum(x_o * w_o))
  w_cum = c(0, cumsum(w_o)/sum(w_o))
  
  b = x_cum[-length(x_cum)]
  B = x_cum[-1]
  h = diff(w_cum)
  
  tmp = data.table(x_ = x_o,
                   y_ = y_o,
                   w_ = w_o,
                   b,
                   B,
                   h, 
                   groups_ = groups_o)
  
  #area_under_concentrationCurve = with(tmp, sum( ((B + b)*h)/2 ) )
  
  tmp2 = tmp[order(groups_c), 
             .(p_j        = sum(w_)/sum(tmp$w_),
               s_j        = sum(x_*w_)/sum(tmp$x_ * tmp$w_),
               concentr_j = concentr(x_, y_, w_)), 
             by = groups_]
  
  tmp2[ , p_cum := cumsum(p_j)]
  tmp2[ , s_cum := cumsum(s_j)]
  
  tmp2[ , within_component := (p_j * s_j * concentr_j)]
  
  concentrationCoefficient = concentr(x, y, w)
  between = tmp2[ , sum(p_j - p_j*(2*s_cum - s_j))]
  within  = tmp2[ , sum(within_component)]
  
  test = round(concentrationCoefficient, 3) == round(between + within, 3)
  if(!test){
    stop("Between and within components do not sum up to the total Concentration Coefficient")
  }
  
  list(concentrationCoefficient = concentrationCoefficient,
       
       main_components = data.table(component    = c("between", "within", "total"),
                                    value        = c(between, within, concentrationCoefficient),
                                    contribution = c(between, within, concentrationCoefficient)/concentrationCoefficient),
       
       within_components =  tmp2[ , .(groups_, 
                                      within_component, 
                                      contribution_to_within = within_component/within,
                                      contribution_to_total  = within_component/concentrationCoefficient)])
}


# df[, IC2::calcSGini(x, w)]$ineq$index
# df[, gini(x, w)]
# 
# 
# df[, IC2::calcGEI(x, w, alpha = 1)]$ineq$index
# df[, theil_t(x, w)]
# 
# df[, IC2::calcGEI(x, w, alpha = 0)]$ineq$index
# df[, theil_l(x, w)]
# 
# 
# df[, IC2::decompGEI(x = x, z = as.factor(groups), w = w, alpha = 1)]
# df[, decomp_theil_t(x, groups = groups, w)]




####################################################################################
#> Plat, D. (2012). IC2: Inequality and Concentration Indices and Curves. 
#> R package version 1.0-1. https://CRAN.R-project.org/package=IC2
#> https://github.com/cran/IC2/blob/master/R/InegConc.r



### Concentration index
calcSConc<-function(x, y, w=NULL, param=2)
{
  calcConc(x, y, w, param)
}

calcConc<-function(x, y, w, param)
{
  if (param<=0) return(NULL)
  if (!is.numeric(x)) return(NULL)
  xNA<-sum(as.numeric(is.na(x)))
  if (!is.null(y))
  {
    if(!is.numeric(y)) return(NULL)
    yNA<-sum(as.numeric(is.na(y)))
  }
  weighted<-FALSE
  wNA<-NULL
  if (is.null(w)) w<-rep(1, length(x))
  else 
  {
    if (!is.numeric(w)) return(NULL)
    weighted<-TRUE
    wNA<-sum(as.numeric(is.na(w)))
  }
  if (is.null(y)) df<-cbind("x"=x, "w"=w)
  else df<-cbind("x"=x, "y"=y, "w"=w)
  df<-df[complete.cases(df),, drop=FALSE]
  if (nrow(df)==0) return (NULL)
  if (any(df[,"x"]<0)) return(NULL)
  if (sum(df[,"x"])==0) return(NULL)
  index<-0
  names(param)<-"param"
  if (nrow(df)>1)
  {
    if (param != 1)
    {
      df[,"w"]<-df[,"w"]/sum(df[,"w"])
      xMean<-weighted.mean(df[,"x"],df[,"w"])
      if (is.null(y)) df<-df[order(df[,"x"]),]
      else df<-df[order(df[,"y"]),]
      sp<-cumsum(df[,"w"])
      sp[length(sp)]<-1
      sm<-c(0, sp[-length(sp)])
      wr<-(((1-sm)^param)-((1-sp)^param))/(param*df[,"w"])
      wvar<-sum(df[,"w"] * ((wr - weighted.mean(wr, df[,"w"]))^2))
      reg1<-coef(lm((-param*wvar*df[,"x"]/xMean)~wr, weights=df[,"w"]))
      names(reg1)<-NULL
      index<-reg1[2]
    }
  }
  if (is.null(y)) names(index)<-"SGini"
  else names(index)<-"SConc"
  SG<-list(ineq=   list(index=index,
                        parameter=param),
           nas=    NULL)
  if (is.null(y)) SG[["nas"]]<-list(xNA=xNA, wNA=wNA,
                                    totalNA=length(x)-nrow(df))
  else SG[["nas"]]<-list(xNA=xNA, yNA=yNA, wNA=wNA,
                         totalNA=length(x)-nrow(df))
  class(SG)<-"ICI"
  return(SG)
}



####################################################################################
### Curves
#   ordinary Lorenz curve, generalized LC, concentration curve 

curveLorenz<-function(x, w=NULL, gener=FALSE,
                      xlab=NA, ylab=NA, add=FALSE, grid=0, ...)
{
  curveLoCo(x, y=NULL, w, gener, xlab, ylab, add, grid, ...)
}


curveConcent<-function(x, y, w=NULL,
                       xlab=NA, ylab=NA, add=FALSE, grid=0, ...)
{
  curveLoCo(x, y, w, gener=FALSE, xlab, ylab, add, grid, ...)
}


curveLoCo<-function(x, y, w, gener, xlab, ylab, add, grid, ...)
{
  if (!is.numeric(x)) return(NULL)
  xNA<-sum(as.numeric(is.na(x)))
  if (!is.null(y))
  {
    if (!is.numeric(y)) return(NULL)
    yNA<-sum(as.numeric(is.na(y)))
  }
  weighted<-FALSE
  wNA<-NULL
  if (is.null(w)) w<-rep(1, length(x))
  else 
  {
    if (!is.numeric(w)) return(NULL)
    weighted<-TRUE
    wNA<-sum(as.numeric(is.na(w)))
  }
  if (is.null(y)) df<-cbind("x"=x,"w"=w)
  else df<-cbind("x"=x, "y"=y, "w"=w)
  df<-df[complete.cases(df),]
  if(any(df[,"x"]<0)) return(NULL)
  if(sum(df[,"x"])==0) return(NULL)
  if(nrow(df)==1) return(NULL)
  df[,"w"]<-df[,"w"]/sum(df[,"w"])
  if (is.null(y)) df<-df[order(df[,"x"]),]
  else df<-df[order(df[,"y"]),]
  if (gener) ymaxi<-weighted.mean(df[,"x"], df[,"w"])
  else ymaxi<-1
  if (!add)
  {
    plot(x=c(0,0), y=c(1,1), type = "l", lty = 1, las=1,
         xlim=c(0,1), ylim=c(0,ymaxi),
         xaxs = "i", yaxs = "i", cex.axis=0.75, cex.lab=1, xlab=xlab, ylab=ylab)
    if (grid>0)
    {
      abline(v=c((0:grid)/grid)[-c(1, grid+1)], col = "gray80")
      abline(h=c((0:grid*ymaxi)/grid)[-c(1, grid+1)], col = "gray80")
      
    }
    abline(0,ymaxi)
  }
  lines(cumsum(df[,"w"]),
        cumsum(ymaxi*(df[,"w"]*df[,"x"])/sum(df[,"w"]*df[,"x"])),
        type = "l", ...)
}


summary.ICI<-function(object, ...)
{
  x<-object
  if (is.null(x)) return(NULL)
  mc<-match.call()
  digits<-ifelse("digits" %in% names(mc),
                 mc[["digits"]], max(5, getOption("digits") - 5))
  names(x[["ineq"]])<-NULL
  xi<-unlist(x[["ineq"]])
  print.default(xi, print.gap=8, quote=FALSE, digits=digits)
  if ("decomp" %in% names(x))
  {
    typeDecomp<-""
    if (names(xi)[1]=="SGini" && length(x[["decomp"]])>2)
    {
      typeDecomp<- " (BM)"
      if (names(x[["decomp"]])[3]=="stratif") typeDecomp<- " (YL)"
    }
    if (names(xi)[1]=="Atk" && length(x[["decomp"]])>2)
    {
      typeDecomp<- " (DP)"
      if (names(x[["decomp"]])[3]=="cross") typeDecomp<- " (BDA)"
    }
    cat(paste("\nDecomposition", typeDecomp, ":\n", sep=""))
    decom<- unlist(x[["decomp"]])
    if (names(decom)[length(decom)]!="betweenELMO") print.default(decom,
                                                                  print.gap=2, quote=FALSE, digits=digits)
    else
    {
      print.default(decom[-length(decom)], print.gap=2,
                    quote=FALSE, digits=digits)
      print.default(decom[length(decom)], print.gap=2,
                    quote=FALSE, digits=digits)
    }
  }
}

