# Setup of a Correlation Lower Panel in Scatterplot Matrix
myPanel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  # Para definir región de graficiación
  par(usr = c(usr[1:2], 0, 1.5) )
  # Para obtener una lista que guarde las marcas de clase y conteos en cada una:
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks;
  nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  # Para dibujar los histogramas
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

# Setup of a Boxplot Diagonal Panel in Scatterplot Matrix
myPanel.box <- function(x, ...){
  usr <- par("usr", bty = 'n')
  on.exit(par(usr))
  par(usr = c(-1, 1, min(x) - 0.5, max(x) + 0.5))
  b <- boxplot(x, plot = F)
  whisker.i <- b$stats[1,]
  whisker.s <- b$stats[5,]
  hinge.i <- b$stats[2,]
  mediana <- b$stats[3,]
  hinge.s <- b$stats[4,]
  rect(-0.5, hinge.i, 0.5, mediana, col = 'gray')
  segments(0, hinge.i, 0, whisker.i, lty = 2)
  segments(-0.1, whisker.i, 0.1, whisker.i)
  rect(-0.5, mediana, 0.5, hinge.s, col = 'gray')
  segments(0, hinge.s, 0, whisker.s, lty = 2)
  segments(-0.1, whisker.s, 0.1, whisker.s)
}

# Setup of a Correlation Lower Panel in Scatterplot Matrix
myPanel.cor <- function(x, y, digits = 2, prefix = "", cex.cor){
  usr <- par("usr"); on.exit(par(usr = usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if(missing(cex.cor))
    cex = 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1 + 1.5*abs(r))
}

# Ordinary or Studentized residuals QQ-plot with Shapiro-Wilk normal test results
myQQnorm <- function(modelo, student = F, ...){
  if(student){
    res <- rstandard(modelo)
    lab.plot <- "Normal Q-Q Plot of Studentized Residuals"
  } else {
    res <- residuals(modelo)
    lab.plot <- "Normal Q-Q Plot of Residuals"
  }
  shapiro <- shapiro.test(res)
  shapvalue <- ifelse(shapiro$p.value < 0.001, "P value < 0.001", paste("P value = ", round(shapiro$p.value, 4), sep = ""))
  shapstat <- paste("W = ", round(shapiro$statistic, 4), sep = "")
  q <- qqnorm(res, plot.it = FALSE)
  qqnorm(res, main = lab.plot, ...)
  qqline(res, lty = 2, col = 2)
  text(min(q$x, na.rm = TRUE), max(q$y, na.rm = TRUE)*0.95, pos = 4, 'Shapiro-Wilk Test', col = "blue", font = 2)
  text(min(q$x, na.rm = TRUE), max(q$y, na.rm = TRUE)*0.80, pos = 4, shapstat, col = "blue", font = 3)
  text(min(q$x, na.rm = TRUE), max(q$y, na.rm = TRUE)*0.65, pos = 4, shapvalue, col = "blue", font = 3)
}

# Table of Summary Statistics
mySumStats <- function(lm.model){
  stats <- summary(lm.model)
  RMSE <- stats$sigma
  R2 <- stats$r.squared
  adjR2 <- stats$adj.r.squared 
  result <- data.frame(Root_MSE = RMSE, R_square = R2, Adj_R_square = adjR2, row.names = "")
  format(result, digits = 6)
}

# Extract estimated and standardized coefficients, their 95% CI's and VIF's
myCoefficients <- function(lm.model, dataset){
  coeff <- coef(lm.model)
  scaled.data <- as.data.frame(scale(dataset))
  coef.std <- c(0, coef(lm(update(formula(lm.model), ~.+0), scaled.data)))
  limites <- confint(lm.model, level = 0.95)
  vifs <- c(0, vif(lm.model))
  result <- data.frame(Estimation = coeff, Coef.Std = coef.std, Limits = limites, Vif = vifs)
  names(result)[3:4] <- c("Limit_2.5%","Limit_97.5%")
  cat("Estimated and standardized coefficients, their 95% CI's and VIF's", "\n")
  result
}

# Analysis of Variance Table
myAnova <- function(lm.model){
  SSq <- unlist(anova(lm.model)["Sum Sq"])
  k <- length(SSq) - 1
  SSR <- sum(SSq[1:k])
  SSE <- SSq[(k + 1)]
  MSR <- SSR/k
  df.error <- unlist(anova(lm.model)["Df"])[k + 1]
  MSE <- SSE/df.error
  F0 <- MSR/MSE
  PV <- pf(F0, k, df.error, lower.tail = F)
  result<-data.frame(Sum_of_Squares = format(c(SSR, SSE), digits = 6), DF = format(c(k, df.error), digits = 6),
                     Mean_Square = format(c(MSR, MSE), digits = 6), F_Value = c(format(F0, digits = 6), ''),
                     P_value = c(format(PV, digits = 6), ''), row.names = c("Model", "Error"))
  result
}

# Diagnostics table for Leverage and Influence observations
myInfluence <- function(model, infl = influence(model), covr = F){
  is.influential <- function(infmat, n, covr = F){
    d <- dim(infmat)
    colrm <- if(covr) 4L else 3L
    k <- d[[length(d)]] - colrm
    if (n <= k) 
      stop("too few cases i with h_ii > 0), n < k")
    absmat <- abs(infmat)
    r <- if(!covr){
      if(is.matrix(infmat)){
        cbind(absmat[, 1L:k] > 2/sqrt(n), # > 1,
              absmat[, k + 1] > 2 * sqrt(k/n), # > 3 * sqrt(k/(n - k)),
              infmat[, k + 2] > 1, # pf(infmat[, k + 3], k, n - k) > 0.5,
              infmat[, k + 3] > 2 * p / n) # infmat[, k + 4] > (3 * k)/n)
      } else {
        c(absmat[, 1L:k] > 2/sqrt(n), # > 1, 
          absmat[, k + 1] > 2 * sqrt(k/n), # > 3 * sqrt(k/(n - k)),
          infmat[, k + 3] > 1, # pf(infmat[, , k + 3], k, n - k) > 0.5, 
          infmat[, k + 4] > 2 * p / n) # > (3 * k)/n)
      }
    } else {
      if(is.matrix(infmat)){
        cbind(absmat[, 1L:k] > 2/sqrt(n), # > 1,
              absmat[, k + 1] > 2 * sqrt(k/n), # > 3 * sqrt(k/(n - k)),
              abs(1 - infmat[, k + 2]) > 3 * p / n, # > (3 * k)/(n - k),
              infmat[, k + 3] > 1, # pf(infmat[, k + 3], k, n - k) > 0.5,
              infmat[, k + 4] > 2 * p / n) # infmat[, k + 4] > (3 * k)/n)
      } else {
        c(absmat[, 1L:k] > 2/sqrt(n), # > 1, 
          absmat[, k + 1] > 2 * sqrt(k/n), # > 3 * sqrt(k/(n - k)),
          abs(1 - infmat[, , k + 2]) > 3 * p / n, # > (3 * k)/(n - k), 
          infmat[, k + 3] > 1, # pf(infmat[, , k + 3], k, n - k) > 0.5, 
          infmat[, k + 4] > 2 * p / n) # > (3 * k)/n)
      }
    }
    attributes(r) <- attributes(infmat)
    r
  }
  p <- model$rank
  e <- weighted.residuals(model)
  s <- sqrt(sum(e^2, na.rm = TRUE)/df.residual(model))
  mqr <- stats:::qr.lm(model)
  xxi <- chol2inv(mqr$qr, mqr$rank)
  si <- infl$sigma
  h <- infl$hat
  is.mlm <- is.matrix(e)
  cf <- if (is.mlm){
    aperm(infl$coefficients, c(1L, 3:2))
  } else infl$coefficients
  dfbetas <- cf/outer(infl$sigma, sqrt(diag(xxi)))
  vn <- variable.names(model)
  vn[vn == "(Intercept)"] <- "1_"
  dimnames(dfbetas)[[length(dim(dfbetas))]] <- paste0("dfb.", abbreviate(vn))
  dffits <- e * sqrt(h)/(si * (1 - h))
  if(any(ii <- is.infinite(dffits))) dffits[ii] <- NaN
  if(covr) cov.ratio <- (si/s)^(2 * p)/(1 - h)
  cooks.d <- if (inherits(model, "glm")){ 
    (infl$pear.res/(1 - h))^2 * h/(summary(model)$dispersion * p)
  } else ((e/(s * (1 - h)))^2 * h)/p
  infmat <- if(is.mlm){
    dns <- dimnames(dfbetas)
    dns[[3]] <- c(dns[[3]], "dffit", "cov.r", 
                  "cook.d", "hat")
    a <- array(dfbetas, dim = dim(dfbetas) + c(0, 0, 3 + 1), dimnames = dns)
    a[, , "dffit"] <- dffits
    if(covr) a[, , "cov.r"] <- cov.ratio
    a[, , "cook.d"] <- cooks.d
    a[, , "hat"] <- h
    a
  } else {
    if(covr){
      cbind(dfbetas, dffit = dffits, cov.r = cov.ratio, cook.d = cooks.d, hat = h)
    } else cbind(dfbetas, dffit = dffits, cook.d = cooks.d, hat = h)
  }
  infmat[is.infinite(infmat)] <- NaN
  is.inf <- is.influential(infmat, sum(h > 0))
  ans <- list(infmat = infmat, is.inf = is.inf, call = model$call)
  class(ans) <- "infl"
  ans
}

# Extract Collinearity Diagnostics
myCollinDiag <- function(lm.model, center = F){
  if(center == F){
    X <- model.matrix(lm.model)
    eigen <- prcomp(X, center = FALSE, scale = TRUE)$sdev^2
    cond.idx <- colldiag(lm.model)
    cond.idx$pi <- round(cond.idx$pi, 6)
    result <- data.frame(Eigen_Value = format(eigen, digits = 5),
                         Condition_Index = cond.idx$condindx, 
                         cond.idx$pi)
    names(result)[2:3] <- c('Condition_Index','Intercept')
    cat("Collinearity Diagnostics", "\n", 
        paste0(rep("", 3+sum(nchar(names(result)[1:2])))), "Variance Decomposition Proportions", "\n")
  }
  else{
    X <- model.matrix(lm.model)[, -1]
    eigen <- prcomp(X, center = TRUE, scale = TRUE)$sdev^2
    cond.idx <- colldiag(lm.model, center = TRUE, scale = TRUE)
    cond.idx$pi <- round(cond.idx$pi, 6)
    result <- data.frame(Eigen_Value = format(eigen, digits = 5),
                         Condition_Index = cond.idx$condindx,
                         cond.idx$pi)
    names(result)[2] <- 'Condition_Index'
    cat("Collinearity Diagnostics (intercept adjusted)", "\n", 
        paste0(rep("", 3+sum(nchar(names(result)[1:2])))), "Variance Decomposition Proportions", "\n")
  }
  result
}

# All Posible Regressions Table
myAllRegTable <- function(lm.model, response = model.response(model.frame(lm.model)), MSE = F){
  regTable <- summary(regsubsets(model.matrix(lm.model)[, -1], response,
                                 nbest = 2^(lm.model$rank - 1) - 1, really.big = T))
  pvCount <- as.vector(apply(regTable$which[, -1], 1, sum))
  pvIDs <- apply(regTable$which[, -1], 1, function(x) as.character(paste(colnames(model.matrix(lm.model)[, -1])[x],
                                                                         collapse = " ")))
  result <- if(MSE){
    data.frame(k = pvCount, R_sq = round(regTable$rsq, 3), adj_R_sq = round(regTable$adjr2, 3),
               MSE = round(regTable$rss/(nrow(model.matrix(lm.model)[,-1]) - (pvCount + 1)), 3),
               Cp = round(regTable$cp, 3), Variables_in_model = pvIDs)
  } else {
    data.frame(k = pvCount, R_sq = round(regTable$rsq, 3), adj_R_sq = round(regTable$adjr2, 3),
               SSE = round(regTable$rss, 3),
               Cp = round(regTable$cp, 3), Variables_in_model = pvIDs)
  }
  format(result, digits = 6)
}

# Summary table and Plots of the Best of All Posible Models by Criterion
# Cp Criterion
myCp_criterion <- function(lm.model, response = model.response(model.frame(lm.model))){
  Cp <- leaps(model.matrix(lm.model)[, -1], response, method = "Cp", nbest = 1) # The Best model by number of parameters
  var_in_model <- apply(Cp$which, 1, 
                        function(x) as.character(paste(colnames(model.matrix(lm.model)[, -1])[x], collapse = " ")))
  Cp_result <- data.frame(k = Cp$size - 1, p = Cp$size, Cp = Cp$Cp, Variables.in.model = var_in_model)
  plot(Cp$size, Cp$Cp, type = "b", xlab = "p", ylab = '', xaxt = "n", cex = 2, ylim = c(0, max(Cp$Cp)), las = 1)
  axis(1, at = Cp$size, labels = Cp$size)
  mtext('Cp', 2, las = 1, adj = 3)
  abline(a = 0, b = 1, lty = 2, col = 2)
  cat("Models are Indexed in rows", "\n")
  print(Cp_result, row.names = F)
}

# R2 Criterion
myR2_criterion <- function(lm.model, response = model.response(model.frame(lm.model))){
  R2 <- leaps(model.matrix(lm.model)[, -1], response, method = "r2", nbest = 1) #Mejor modelo para cada p
  var_in_model <- apply(R2$which, 1,
                        function(x) as.character(paste(colnames(model.matrix(lm.model)[, -1])[x], collapse = " ")))
  R2_result <- data.frame(k = R2$size - 1, p = R2$size, R2 = R2$r2, Variables.in.model = var_in_model)
  plot(R2$size, R2$r2, type = "b", xlab = "p", ylab = "", xaxt = "n", cex = 2, las = 1)
  axis(1, at = R2$size, labels = R2$size)
  mtext("R2", 2, las = 1, adj = 4)
  cat("Models are Indexed in rows", "\n")
  print(R2_result, row.names = F)
}

# adjR2 Criterion
myAdj_R2_criterion <- function(lm.model, response = model.response(model.frame(lm.model))){
  adjR2 <- leaps(model.matrix(lm.model)[, -1], response, method = "adjr2", nbest = 1)
  var_in_model <- apply(adjR2$which, 1,
                        function(x) as.character(paste(colnames(model.matrix(lm.model)[, -1])[x], collapse = " ")))
  adjR2_result <- data.frame(k = adjR2$size - 1, p = adjR2$size, adjR2 = adjR2$adjr2, Variables.in.model = var_in_model)
  plot(adjR2$size, adjR2$adjr2, type = "b", xlab = "p", ylab = "", xaxt = "n", cex = 2, las = 1)
  axis(1, at = adjR2$size, labels = adjR2$size)
  mtext("adj_R2", 2, las = 1, adj = 2.2)
  cat("Models are Indexed in rows", "\n")
  print(adjR2_result, row.names = F)
}

myStepwise <- function(full.model, alpha.to.enter, alpha.to.leave, initial.model = lm(model.response(model.frame(full.model)) ~ 1)){
  ###################################################################################
  #                                                                                 #
  # Function to perform a stepwise linear regression using F tests of significance, #
  # based on the function developed by Paul A. Rubin (rubin@msu.edu)                #
  # URL = https://orinanobworld.blogspot.com/2011/02/stepwise-regression-in-r.html  #
  #                                                                                 #
  ###################################################################################
  #                                                                                 #
  # full.model    : model containing all possible terms                             #
  # alpha.to.enter: significance level above which a variable may enter             #
  # alpha.to.leave: significance level below which a variable may be deleted        #
  # initial.model : first model to consider. By default the first model is the one  #
  #                 without predictors                                              #
  ###################################################################################
  #
  # fit the full model
  full <- lm(full.model);
  # attach predictor variables in full model
  attach(as.data.frame(model.matrix(full.model)[, -1]), warn.conflicts = F);
  # MSE of full model
  msef <- (summary(full)$sigma)^2;
  # sample size
  n <- length(full$residuals);
  # this is the current model
  current <- lm(initial.model);
  # process each model until we break out of the loop
  while(TRUE){
    # summary output for the current model
    temp <- summary(current);
    # list of terms in the current model
    rnames <- rownames(temp$coefficients);
    # write the model description
    print(temp$coefficients);
    # current model's size
    p <- dim(temp$coefficients)[1];
    # MSE for current model
    mse <- (temp$sigma)^2;
    # Mallow's cp
    cp <- (n - p)*mse / msef - (n - 2 * p);
    # show the fit
    fit <- sprintf("\nS = %f, R-sq = %f, R-sq(adj) = %f, C-p = %f",
                   temp$sigma, temp$r.squared, temp$adj.r.squared, cp);
    write(fit, file = "");
    # print a separator
    write("=====", file = "");
    # don't try to drop a term if only one is left
    if(p > 1){
      # looks for significance of terms based on F tests
      d <- drop1(current, test = "F");
      # maximum p-value of any term (have to skip the intercept to avoid an NA)
      pmax <- max(d[-1, 6]);
      # we have a candidate for deletion
      if(pmax > alpha.to.leave){
        # name of variable to delete
        var <- rownames(d)[d[, 6] == pmax];
        # if an intercept is present, it will be the first name in the list
        if(length(var) > 1){
          # there also could be ties for worst p-value, a safe solution to 
          # both issues is taking the second entry if there is more than one 
          var <- var[2];
        }
        # print out the variable to be dropped
        write(paste("--- Dropping", var, "\n"), file="");
        # current formula
        f <- formula(current);
        # modify the formula to drop the chosen variable (by subtracting it)
        f <- as.formula(paste(f[2], "~", paste(f[3], var, sep=" - ")));
        # fit the modified model
        current <- lm(f);
        # return to the top of the loop
        next;
      }
      # if we get here, we failed to drop a term; try adding one
    }
    # note: add1 throws an error if nothing can be added (current == full), which
    # we trap with tryCatch
    # looks for significance of possible additions based on F tests
    a <- tryCatch(add1(current, scope = full.model, test = "F"), error = function(e) NULL);
    if(is.null(a)){
      # there are no unused variables (or something went splat), so we bail out
      break;
    }
    # minimum p-value of any term (skipping the intercept again)
    pmin <- min(a[-1, 6]);
    # we have a candidate for addition to the model
    if(pmin < alpha.to.enter){
      # name of variable to add
      var <- rownames(a)[a[,6] == pmin];
      # same issue with ties, intercept as above
      if(length(var) > 1){
        var <- var[2];
      }
      # print the variable being added
      write(paste("+++ Adding", var, "\n"), file="");
      # current formula
      f <- formula(current);
      # modify the formula to add the chosen variable
      f <- as.formula(paste(f[2], "~", paste(f[3], var, sep=" + ")));
      # fit the modified model
      current <- lm(f);
      # return to the top of the loop
      next;
    }
    # if we get here, we failed to make any changes to the model; time to punt
    break;
  }
  # detach predictor variables in full model
  detach(as.data.frame(model.matrix(full.model)[,-1]));
  current
}

myBackward <- function(base.full, alpha.to.leave = 0.05, verbose = T){
  ###################################################################################
  #                                                                                 #
  # Function to perform a backward linear regression using F tests of significance, #
  # based on the function developed by Joris Meys                                   #
  # URL = https://codeday.me/es/qa/20190117/101609.html                             #
  #                                                                                 #
  ###################################################################################
  #                                                                                 #
  # base.full     : dataset(Y, X1...)                                               #
  # alpha.to.leave: the significance level below which a variable may be deleted    #
  # verbose       : if TRUE, prints F-tests, dropped var and resulting model after  #
  #                                                                                 #
  ###################################################################################
  #
  has.interaction <- function(x, terms){
    ###############################################################################
    #                                                                             #
    # Function has.interaction developed by Joris Meys, checks whether x is part  #
    # of a term in terms, which is a vector with names of terms from a model      #
    #                                                                             #
    ###############################################################################
    #
    out <- sapply(terms, function(i){
      sum(1 - (strsplit(x, ":")[[1]] %in% strsplit(i, ":")[[1]])) == 0
    }
    )
    return(sum(out) > 0)
  }
  
  counter <- 1
  # check input
  #if(!is(model, "lm")) stop(paste(deparse(substitute(model)),"is not an lm object\n"))
  # calculate scope for drop1 function
  attach(base.full)
  model <- lm(base.full)
  terms <- attr(model$terms, "term.labels")
  # set scopevars to all terms
  scopevars <- terms
  # Backward model selection:
  while(TRUE){
    # extract the test statistics from drop.
    test <- drop1(model, scope = scopevars, test = "F")
    if(verbose){
      cat("-------------STEP ", counter, "-------------\n",
          "The drop statistics : \n")
      print(test)
    }
    pval <- test[, dim(test)[2]]
    names(pval) <- rownames(test)
    pval <- sort(pval, decreasing = T)
    if(sum(is.na(pval)) > 0){
      stop(paste("Model", deparse(substitute(model)), "is invalid. Check if all coefficients are estimated."))
    }
    # check if all significant
    if(pval[1] < alpha.to.leave){
      # stops the loop if all remaining vars are sign.
      break
    }
    # select var to drop
    i <- 1
    while(TRUE){
      dropvar <- names(pval)[i]
      check.terms <- terms[-match(dropvar, terms)]
      x <- has.interaction(dropvar, check.terms)
      if(x){
        i = i + 1
        next
      } else {
        break
      }
      # end while(T) drop var
    }
    # stops the loop if var to remove is significant
    if(pval[i] < alpha.to.leave){
      break
    }
    if(verbose){
      cat("\n--------\nTerm dropped in step", counter, ":", dropvar, "\n--------\n\n")
    }
    # update terms, scopevars and model
    scopevars <- scopevars[-match(dropvar, scopevars)]
    terms <- terms[-match(dropvar, terms)]
    formul <- as.formula(paste(".~.-", dropvar))
    model <- update(model, formul)
    if(length(scopevars) == 0){
      warning("All variables are thrown out of the model.\n", "No model could be specified.")
      return()
    }
    counter <- counter + 1
    # end while(T) main loop
  }
  return(model)
}