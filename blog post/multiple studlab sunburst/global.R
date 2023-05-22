

library(sunburstR);library(data.table);library(htmltools);library(magrittr)
#> Warning: package 'htmltools' was built under R version 4.1.3
library(d3r)

a <- fread("420_13_pathways.csv", check.names = T)

DT::datatable(read.csv("420_13_pathways.csv"))

step2<-a[, names(a) := lapply(.SD, function(x) gsub("\\+", "-", x)), .SDcols = names(a)]%>%
  .[, .(size= personCount, Step.1=Step.1, Step.2=Step.2)]%>%
  .[, c("onelevel2", "onelevel1") := tstrsplit(Step.1, " - ", fixed = TRUE, type.convert = FALSE)]%>%
  .[is.na(onelevel1), onelevel1 := onelevel2]%>%
  .[, Step.1:=NULL]%>%
  .[, c("level1", "level2", "level3") := tstrsplit(Step.2, " - ", fixed = TRUE, type.convert = FALSE)]%>%
  .[is.na(level2), level2 := level1]%>%
  .[is.na(level3),level3:=level2] %>%
  .[,Step.2:=NULL]%>%
  setcolorder(., c("onelevel1", "onelevel2", "level1", "level2","level3", "size"))%>% .[]


library(d3r)
what <- d3_nest(step2, value_cols = "size")

#colors
colors<- c('#FFAA00', '#2D5F91','#91D4D2', '#E8655F')
labels <- c("KYR_Aspirin_20230414", "KYR_Clopidogrel_20230414", "KYR_cilostazol_20230414",  "KYR_Triflusal_20230414")


sunburst(what, legend = FALSE, width = "100%", colors= list(range = colors, domain = labels), height = 400, count= TRUE)



what <- d3_nest(step, value_cols = "personCount")


head(split)


what <- d3_nest(split[, c(1, 10)], value_cols = "personCount")

sunburst(data.table(letters[1:36], as.integer(split$personCount))[1:10,])
