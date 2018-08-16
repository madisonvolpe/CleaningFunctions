```{r}

CaseInfo_fun <- function(a){
  for(i in a){

##Column Names##     
    
CaseInfoColNames <- i %>% 
          read_html()%>%
          html_nodes(xpath = "//*[@id='TABLE6']//b")%>%
          html_text()

CaseInfoColNames <- trimws(CaseInfoColNames)
CaseInfoColNames <- CaseInfoColNames[CaseInfoColNames!="Case(s) Information"]
CaseInfoColNames <- CaseInfoColNames[CaseInfoColNames!=""]

##Bringing in Data## 

RawInfo <- i %>% 
  read_html()%>%
  html_nodes(xpath = "//*[@id='TABLE6']")%>%
  html_text()

RawInfo <- RawInfo %>%
  str_replace_all("\n", "")%>%
  str_replace_all("\\s+", " ")%>%
  str_replace_all("\\s$", "") %>%
  str_replace_all("^\\s", "")

##Split Data at Column Names using CaseInfoColNames## 

CaseInfoColNames<- gsub("^\\s+", "", CaseInfoColNames)
CaseInfoColNamesSplit <- paste(CaseInfoColNames, collapse = '|')

CaseInfoData <- RawInfo %>%
  base::strsplit(split=CaseInfoColNamesSplit) %>%
  unlist()

CaseInfoData <- trimws(CaseInfoData, which = "right")
CaseInfoData <- CaseInfoData[CaseInfoData!="Case(s) Information"]

##Creating Dataframe##

CaseInfoDF <- data.frame(CaseInfoColNames, CaseInfoData)

CaseInfoDF2 <- CaseInfoDF %>%
  t() %>%
  as.data.frame(stringsAsFactors = F) %>%
  rownames_to_column("value") %>%
  `colnames<-`(.[1,]) %>%
  .[-1,] %>%
  `rownames<-`(NULL)
  }
  return(CaseInfoDF)
} 
```
