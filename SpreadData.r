```{r}
SpreadData_Fun <- function(z){
  z[,'CaseInfoColNames'] <- as.character(z[,'CaseInfoColNames'])
  z[,'CaseInfoData'] <- as.character(z[,'CaseInfoData'])
  
  for(i in 1:nrow(z)){
    if(grepl("^Case Number:$", z[,'CaseInfoColNames'][i]) == TRUE ||
       grepl("^Warrant Case:$", z[,'CaseInfoColNames'][i]) == TRUE){
      z[,'CaseInfoColNames'][i] <- paste0("**", z[,'CaseInfoColNames'][i], sep = "")
    }
  }
  
  z <- z %>%
    mutate(caseID = cumsum(as.numeric(grepl("^\\*\\*",z[,'CaseInfoColNames']))))
  
  z <- ddply(z, .(caseID, CaseInfoColNames,UID,RunTime), summarise, CaseInfoData = toString(CaseInfoData))
  
  z <-z%>%
    spread(CaseInfoColNames, CaseInfoData)
  
  return(z)
}
```
