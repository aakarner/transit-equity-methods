options(scipen = 999)
rm(list = ls())
library(data.table)

set.seed(2)
n = 1000
df = data.table(groups = rbinom(n, size = 1, prob = .3))
df$x = exp(rnorm(n, mean = 6 + df$groups, sd = exp(.2 + df$groups)))
df$y = exp(log(df$x) + runif(n, 0, .1) + runif(n, 0, .3)*df$groups)
df$w = rpois(n, lambda = 2*df$groups) + 1
df$deciles = cut(df$x, breaks = Hmisc::wtd.quantile(df$x, weights = df$w, probs = seq(0, 1, .1)), include.lowest = T, ordered_result = T)

x      = df$x
y      = df$y
groups = df$groups
nonOverlaping_groups = df$deciles
w      = df$w

theil_t = function(x, w){
        
        if(is.null(w)){
                w = rep(1, length(x))
        }
        
        complete = complete.cases(x, w) & (x > 0)
        x_c = x[complete]
        w_c = w[complete]
        
        X_bar = weighted.mean(x =  x_c, w = w_c)
        
        weighted.mean((x_c/X_bar) * log(x_c/X_bar), w = w_c)
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
        
        list(theil_t = theil_t(x, w),
             main_components = data.table(component    = c("within", "between", "total"),
                                          value        = c(within, between, theil_t(x, w)),
                                          contribution = c(within/theil_t(x, w), between/theil_t(x, w), 1)),
                    
             within_groups_details = data.table(group        = d_summary$groups_c,
                                                within_i     = d_summary$s_i *d_summary$theil_t_i,
                                                contribution_to_within = (d_summary$s_i *d_summary$theil_t_i)/within,
                                                contribution_to_total  = (d_summary$s_i *d_summary$theil_t_i)/theil_t(x, w))
             )
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
        
        complete = complete.cases(x, w) & (x >= 0)
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
        
        x_cum = c(0, cumsum(x_o * w_o)/sum(x_o * w_o))
        w_cum = c(0, cumsum(w_o)/sum(w_o))
        
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





df[, IC2::calcSGini(x, w)]$ineq$index
df[, gini(x, w)]

df[, IC2::calcSConc(x = x, y = y, w = w)]$ineq$index
df[, concentr(x, y, w)]

df[, IC2::calcGEI(x, w, alpha = 1)]$ineq$index
df[, theil_t(x, w)]

df[, IC2::calcGEI(x, w, alpha = 0)]$ineq$index
df[, theil_l(x, w)]

decomp_concentr(x = x, y = y, nonOverlaping_groups = df$deciles, w = w) # NOVO!!!!!!!!!!!!!!

df[, IC2::decompGEI(x = x, z = as.factor(groups), w = w, alpha = 1)]
df[, decomp_theil_t(x, groups = groups, w)]

