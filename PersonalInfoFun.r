## Libraries 

library(dplyr)
library(stringr)
library(qdap)

## Cleaning Function 

```{r}
PersonalInfo_fun <- function(a) {
  for(i in a){
RawInfo <- i %>%
  str_replace_all("\n", "")%>%
  str_replace_all("\\s+", " ")%>%
  str_replace_all("\\s$", "") %>%
  str_replace_all("Personal Information", "")%>%
  str_replace_all("^\\s", "")


PersonalInfoColNames <-  RawInfo %>% 
                        str_extract_all("\\w+:") %>%
                        unlist()

PersonalInfoColNames <- mgsub(":", "", PersonalInfoColNames)



PersonalInfoData <- RawInfo %>%
  str_replace_all(pattern = "\\w+:", replacement = "_")%>%
  str_split("_") %>%
  unlist()


PersonalInfoData <- trimws(PersonalInfoData, which = "both")
PersonalInfoData <- PersonalInfoData[-1]

PersonalInfoDF <- data.frame(PersonalInfoColNames, PersonalInfoData)

PersonalInfoDF <- data.frame(split(PersonalInfoDF[1:nrow(PersonalInfoDF), 2], 
                                  PersonalInfoDF[1:nrow(PersonalInfoDF),1]))

PersonalInfoDF <- PersonalInfoDF[,c(5,1,2,3,4,6,7,8)]
}
return(PersonalInfoDF)
}
```
