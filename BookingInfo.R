```{r}
BookingInfo_fun <- function(a) {
  for(i in a){
RawInfo<- i %>%
    str_replace_all("\n", "")%>%
    str_replace_all("\\s+", " ")%>%
    str_replace_all("\\s$", "") %>%
    str_replace_all("Booking Information", "")%>%
    str_replace_all("^\\s", "")

BookingInfoColNames <-  RawInfo %>% 
  str_extract_all("\\w+:") %>%
  unlist()

BookingInfoColNames <- BookingInfoColNames[-6]
BookingInfoColNames[1] <- "Jail Number"
BookingInfoColNames[4] <- "Date Booked"
BookingInfoColNames[5] <- "Time Booked"

BookingInfoColNames <- mgsub(":", "", BookingInfoColNames)

BookingInfoData <- RawInfo %>%
  base::strsplit(split="Jail Number:|IDS:|Loc:|Date Booked:|Time Booked:") %>%
  unlist()

BookingInfoData <- BookingInfoData[-1]

BookingInfoData <- trimws(BookingInfoData, which = "both")

BookingInfoDF <- data.frame(BookingInfoColNames, BookingInfoData)

BookingInfoDF <- data.frame(split(BookingInfoDF[1:nrow(BookingInfoDF), 2], 
                                  BookingInfoDF[1:nrow(BookingInfoDF),1]))
  }
  return(BookingInfoDF)
}
```
