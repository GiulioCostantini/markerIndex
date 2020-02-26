scaleScores <- function(data, pattern, recode = FALSE, recString = "_r", min = 1, max = 5, na.rm = TRUE)
{
  if(recode)
  {
    data[str_detect(names(data), recString)] <-
      min + max - data[str_detect(names(data), recString)]
  }
  
  slct <- lapply(pattern, function(x) str_detect_all(names(data), x))
  allscales <- sapply(slct, function(x) rowMeans(data[x], na.rm = na.rm))
  allscales
}

