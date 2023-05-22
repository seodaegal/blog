library(shiny);library(sunburstR);library(data.table);library(DT);library(plotly);library(d3r)

cat<- c("IS", "IS with new Carotid stenosis", "IS with new Coronary artery", "IS2", "IS before2013", "IS after2013")
days<- c("0days after", "30days after", "365days after")
hos<- c("AUMC", "GNUCH", "KDH", "KHMC", "KHNMC", "KWMC", "MJH", "SCHBC", "SCHCA", "SCHGM", "SCHSU")
setwd("~/kdh/doctorssi/TGAP")

# Define UI for application that draws a histogram
ui <- navbarPage("ATLAS",
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel( radioButtons("cohort", "Cohort", choices= cat, selected= cat[1], inline = T),
                                          radioButtons("days_aft", "Days After", choices = days, selected = days[1], inline = T),
                                          selectInput("hospital", "Data", hos, hos, multiple = T)),
                          mainPanel(
                            sunburstOutput("sunburst"),
                            textOutput("selection"),
                            DTOutput("data")
                          ))
                 )
)



server <- function(input, output) {
  
  output$sunburst <- renderSunburst({
    a<- ifelse(input$cohort %in% cat[1:3],"IS_carotid_coronary", "IS 20130704")
    b<- ifelse(input$days_aft=="0days after", "", ifelse(input$days_aft=="30days after", " after30d", " after365d"))
    
    fn <- file.path("IS_cohort_pathway", a, paste0(input$cohort,b))

    process_file <- function(file_path) {
      dt <- tryCatch(fread(file = file_path),error = function(e) e)
      if (any(class(dt) == "error")) {
        return(NULL)
      } else {
        return(dt[, c("sourceName", "targetId", "cohortCount", "pathwayCount", "pathwayPercent", "cohortPercent") := NULL])
      }
    }
    
    data.tb_list <- lapply(input$hospital, function(hospital) {
      file_path <- paste0(fn,"/",paste0(input$cohort, b), "_", hospital, ".csv")
      process_file(file_path)
    })
    
    data <- rbindlist(data.tb_list, fill = TRUE)%>%
      .[, .(personCount = sum(personCount)), by = setdiff(names(merged_data.tb), "personCount")]
    
    #step all NA인 step 칼럼 지우기기
    step <- data[, names(data) := lapply(.SD, function(x) gsub("\\+", "-", x)), .SDcols = names(data)]%>%
      subset(., select = c(colSums(is.na(.)) != nrow(.)))%>%
      .[, .SD, .SDcols = c( grep("Step", colnames(.), value = TRUE),"personCount")]%>%
      .[,personCount:=as.integer(data$"personCount")]
    
    #seperating steps into levels
    c<- c(1:length(step))
    
    for (i in c[-length(step)]){
      res<-step[, c(paste(colnames(step[,..i]), seq(1, length(tstrsplit(step[[i]], " - ", fixed = TRUE, type.convert = FALSE))), sep = " level ")) := tstrsplit(step[[i]], " - ", fixed = TRUE, type.convert = FALSE)]
    }
    
    split<- res[, .SD, .SDcols = c("personCount", grep("level", colnames(res), value = TRUE))]
    
    step_cols<- grep("^Step \\d+", names(split), value = TRUE) #all the Step level column names
    
    step_nums <- unique(sub("^Step (\\d+) level \\d+$", "\\1", step_cols)) #the number of steps
    
    # filling in NA when level 1 has a value (for sunburst)
    for ( i in step_nums){
      level<- grep(sprintf("Step %s level ",i), names(split), value = TRUE)
      level_nums <- unique(sub("^Step \\d+ level (\\d+)$", "\\1", level))
      
      for ( j in level_nums){
        
        name<- paste0(sprintf("Step %s level ",i), j)
        checking<- ifelse(j!=1 & is.na(split[[paste0(sprintf("Step %s level ",i), j)]]),split[[paste0(sprintf("Step %s level ",i), as.integer(j)-1)]],split[[name]])
        split[, sprintf("%s",name):= checking][]
      }
    }
    
    split[,"personCount":=as.integer(split$personCount)]
    
    setcolorder(split, c(setdiff(colnames(split), "personCount"), "personCount"))
    setnames(split, "personCount", "size")
    split$size<-as.character(split$size)
    
    # for (i in c[-length(a)]){ print(i)
    #   test<-a[, c(paste(colnames(a[,..i]), seq(1, length(tstrsplit(a[[i]], " - ", fixed = TRUE, type.convert = FALSE))), sep = "level")):= tstrsplit(a[[i]], " - ", fixed = TRUE, type.convert = FALSE)]
    
    what<-d3_nest(split, value_cols = "size")
    #invalidateLater(1000, session)
    add_shiny(sunburst(data=what, legend = TRUE, colors= c('#FFAA00', '#2D5F91','#91D4D2', '#E8655F'),  count= TRUE))
  })
  
  selection <- reactive({
    input$sunburst_mouseover
  })
  output$selection <- renderText(selection())
  output$data<- renderDT({
     a<- ifelse(input$cohort %in% cat[1:3],"IS_carotid_coronary", "IS 20130704")
    b<- ifelse(input$days_aft=="0days after", "", ifelse(input$days_aft=="30days after", " after30d", " after365d"))
    
    fn <- file.path("IS_cohort_pathway", a, paste0(input$cohort,b))

    process_file <- function(file_path) {
      dt <- tryCatch(fread(file = file_path),error = function(e) e)
      if (any(class(dt) == "error")) {
        return(NULL)
      } else {
        return(dt[, c("sourceName", "targetId", "cohortCount", "pathwayCount", "pathwayPercent", "cohortPercent") := NULL])
      }
    }
    
    data.tb_list <- lapply(input$hospital, function(hospital) {
      file_path <- paste0(fn,"/",paste0(input$cohort, b), "_", hospital, ".csv")
      process_file(file_path)
    })
    
    data <- rbindlist(data.tb_list, fill = TRUE)%>%
      .[, .(personCount = sum(personCount)), by = setdiff(names(merged_data.tb), "personCount")]
    DT::datatable(data,rownames=F)
  })
}


shinyApp(ui = ui, server = server)
