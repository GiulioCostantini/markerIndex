reliabAll <- function(data, pattern, simplify = TRUE, recode = FALSE, recString = "_r", omega = TRUE)
{
  if(recode)
  {
    data[str_detect(names(data), recString)] <-
      (-1) * data[str_detect(names(data), recString)]
  }
  
  slct <- lapply(pattern, function(x) str_detect_all(names(data), x))
  
  allstats <- list()
  warnAlpha <- list()
  errAlpha <- list()
  warnOmega <- list()
  errOmega <- list()
  
  
  for(i in 1:length(pattern))
  {
    warnAlpha[[i]] <- 0
    errAlpha[[i]]  <- 0
    
    alphaVal <- tryCatch(expr = {
      alpha(data[ slct[[i]] ])
    },
    warning = function(cond) {
      warnAlpha[[i]] <<- as.character(cond)
      alpha(data[ slct[[i]] ], warnings = FALSE)
    },
    error = function(cond) {
      errAlpha[[i]] <<- as.character(cond)
      alpha(data[ slct[[i]] ], warnings = FALSE)
      
    })
    
    if(omega)  
    {
      warnOmega[[i]] <- 0
      errOmega[[i]]  <- 0
      
      omegaVal <- tryCatch(expr = {
        ci.reliability(data[slct[[i]]], type = NULL, interval.type = "none")
      },
      warning = function(cond) {
        warnOmega[[i]] <<- as.character(cond)
        ci.reliability(data[slct[[i]]], type = NULL, interval.type = "none")
      },
      error = function(cond) {
        errOmega[[i]] <<- as.character(cond)
        ci.reliability(data[slct[[i]]], type = NULL, interval.type = "none")
      })
    }
    
    allstats[[i]] <- list()
    allstats[[i]]$alpha <- alphaVal
    if(omega)
    {
      allstats[[i]]$omega <- omegaVal
    } 
  }
  
  if(simplify)
  {
    out <- data.frame(
      "scale" = names(pattern),
      "nitem" = sapply(slct, sum, na.rm = TRUE))
    for(i in 1:length(pattern))
    {
      if(!is.null(allstats[[i]][1]))
      {
        out[i,"raw_alpha"] <- round(allstats[[i]]$alpha$total$raw_alpha, 2)
        out[i,"std_alpha"] <- round(allstats[[i]]$alpha$total$std.alpha, 2)
      } else {
        out[i,c("raw_alpha", "std_alpha")] <- NA
      }
      
      if(omega)
      {
        out[i,"omega"] <- round(allstats[[i]]$omega$est, 2)
        out[i,"omega"] <- round(allstats[[i]]$omega$est, 2)
      }
      
      out$errorsAlpha <- errAlpha
      out$warningsAlpha <- warnAlpha
      
      if(omega)
      {
        out$errorsOmega <- errOmega
        out$warningsOmega <- warnOmega
      }
      
      
    }
    
  } else
  {
    out <- allstats
  }
  
  out
}


reliabilityAll <- function(data, pattern, simplify = TRUE, recode = FALSE, recString = "_r", omega = TRUE)
{
  suppressMessages(suppressWarnings(reliabAll(data = data,
                                              pattern = pattern,
                                              simplify = simplify,
                                              recode = recode,
                                              recString = recString,
                                              omega = omega)))
}

