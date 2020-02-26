str_detect_all <- function(string, pattern, allorany = c("all", "any"))
{
  allorany <- match.arg(allorany)
  if(length(pattern) > 1) 
  {
    out <- sapply(pattern, function(x) str_detect(string, x))
    if(allorany == "all")
      out <- apply(out, 1, all)
    else if(allorany == "any")
      out <- apply(out, 1, any)
    
  } else {
    out <- str_detect(string, pattern)
  }
  out
}