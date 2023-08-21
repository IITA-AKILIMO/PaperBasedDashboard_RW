#' Paper based Annex dashboard
#' Authors : Meklit Chernet, Turry Ouma, IITA
#' Last updated on : November 2021 (to include GH)
#' 
#setwd("C:/Users/User/Documents/ACAI/DASHBOARDS/paper based/PaperbasedDashboard_RW")

#C:\Users\User\Documents\ACAI\paper based\PaperbasedDashboard -v4 - Copy
#C:\Users\User\Documents\ACAI\paper based\PaperBasedAnnex - RW
library(tidyr)
require(plyr)
library(rgdal)
library(raster)
library(dismo)
library(maptools)
library(rgeos)
require(RColorBrewer)
require(graphics)
require(rasterVis)
library(sp)
library(shinyalert)
library(ggthemes)
require(ggplot2)
library(gridExtra) 
library(hexbin)
library(viridis)
library(sf)
library(ggspatial)

require(ggrepel)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)
library(leaflet)
library(cartogram)
library(grid)
library(formattable)
library(shinybusy)
library(DT)


### SHINY SERVER ###

server = function(input, output, session) {
  #.............................................................................  
  # Show a modal on initiation of tool
  #.............................................................................
  shinyalert("Paper Based Tools Annex", "This tool contains tables and maps with advice on application rates of urea,
                         NPK fertilizer for cassava, as well as the expected root yield response. Response to fertilizer depends on soil
                         conditions and the time of planting.
                     
                 
                 
                 ", type = "info", timer = 4000, size = 'm',
             closeOnClickOutside = FALSE,
             closeOnEsc = FALSE,
             animation = TRUE,
             html = TRUE,
             
             showConfirmButton = FALSE,
             showCancelButton = FALSE,
             confirmButtonText = "OK",
             confirmButtonCol = "#AEDEF4")
 
  #............................................................................. 
  #spinner before maps are displayed
  #.............................................................................
  observeEvent(input$btn_go, {
  
    shinybusy::show_modal_spinner(
      spin = "cube-grid",
      #spin = "fading-circle",
      #spin = "fading-circle",
      
      color = 	"#228B22",
      #00FF00
      text = "Please wait while the map loads..."
    )
    Sys.sleep(6)
    remove_modal_spinner()
  })
  
  #.............................................................................
  #render select input options
  #.............................................................................
  # output$country <- renderUI({
  # 
  #   pickerInput("country", "Select Country:",
  #               choices = c("Rwanda"),
  #               selected = "Rwanda",
  #               multiple = TRUE,
  #               options = pickerOptions(maxOptions = 1))
  #     })
#})

  # observeEvent(input$country, {
  #   if(input$country == "Rwanda")  {
  # 
  #     output$usecase <- renderUI({
  #      
  #       pickerInput("usecase", "Select use case",
  #                   choices = c("Fertilizer Recommendation", "Scheduled Planting"),
  #                   selected = NULL,
  #                   multiple = TRUE,
  #                   options = pickerOptions(maxOptions = 1),
  #                   )
  #           })
  #   }
  # })
    
 
  output$state <- renderUI({

    pickerInput("state", "Select Province",
                choices = c("Eastern", "Kigali City","Nothern", "Southern", "Western"),
                selected = NULL,
                multiple = TRUE,
                options = pickerOptions(maxOptions = 1))
  })
  
 
  observeEvent(input$state, {
    
    if( !is.null(input$state))  {
  output$plntmth <- renderUI({
        
      pickerInput("plntmth", "Select planting month",
                  choices = c("February", "March",  "September",
                              "October"),
                  selected = NULL,
                  multiple = TRUE,
                  options = pickerOptions(maxOptions = 1))
  })
    }
     
  })
  
  observeEvent(input$plntmth, {
    if(!is.null(input$plntmth)) {
      output$costs <- renderUI({
        
        pickerInput("costs", "Would you like to specify your prices for cassava and fertilizers?",
                    choices = c("Yes", "No"),
                    selected = NULL,
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
      })
  
    }
  })
  
  observeEvent(input$plntmth, {
    if(!is.null(input$plntmth))  {

  
  output$selection2 <- renderUI({
    
    pickerInput("selection2", "Select variable to view",
                                choices = c("NPK 17:17:17 rate", "DAP rate", "MOP rate", "Expected yield response", "Urea rate"),
                                selected = NULL,
                                multiple = TRUE,
                                options = pickerOptions(maxOptions = 1))
  })


    }
  })


  
  observeEvent(input$state, {
    if(!is.null(input$state))  {
      
      output$unit_loc_rw <- renderUI({
        
        selectInput("unit_loc_rw", "Select unit of land",
                    choices = c("are", "ha"))
        

      })
    }
  }) 
  

  observeEvent(input$unit_loc_rw, {
    if(!is.null(input$unit_loc_rw))  {
      output$FCY_ha_rw <- renderUI({
        
        selectInput("FCY_ha_rw", "Select Your Current Yield (Tonnes)",
                    choices = c("0-7.5 t/ha", "7.5-15 t/ha", "15-22.5 t/ha", "22.5-30 t/ha", ">30 t/ha"),
                    selected = " ")            
      })
 
      
        
      output$FCY_are_rw <- renderUI({
        
        selectInput("FCY_are_rw", "Select Your Current Yield (Kg)",
                    choices = c("0-75 kg/are", "75-150 kg/are", "150-225 kg/are", "225-300 kg/are", ">300 kg/are"),
                    selected = " ")
      })
    }
    
  })  
  


  observeEvent(input$costs, {
    if(input$costs == "Yes" ) {
   
  output$CassavaPrice <- renderUI({
    
    textInput("CassavaPrice", "Price of cassava per ton")
  })
    }
  })


  
  observeEvent(input$costs, {
    if(input$costs == "Yes")  {
      
      output$NPK171717Price <- renderUI({
        
        textInput("NPK171717Price", "Cost of NPK:17:17:17 per 50Kg bag")
      })
      
      
      output$DAPPrice <- renderUI({
        
        textInput("DAPPrice", "Cost of DAP per 50Kg bag")
      })
      
      output$MOPPrice <- renderUI({

        textInput("MOPPrice", "Cost of MOP per 50Kg bag")
      })

    }
  })
  

  #ADD GHANA FERT HERE
  
  
    observeEvent(input$costs, {
      if(input$costs == "Yes") {
      output$UreaPrice <- renderUI({
        
        textInput("UreaPrice", "Cost of Urea per 50Kg bag")
      })
    }
    })

  
  observeEvent(input$costs, {
    if(input$costs == "Yes") {  
      output$btn_go <- renderUI({
        actionButton("btn_go", "Get Maps & Tables", icon("map"),
                     style="color: #fff; background-color: green; border-color: #2e6da4")
        
      })
    }else if(input$costs == "No"){
      output$btn_go <- renderUI({
        actionButton("btn_go", "Get Maps & Tables", icon("map"),
                     style="color: #fff; background-color: green; border-color: #2e6da4")
        
      })
    }
      
    
  })
  
  
 #.................................................................................................................
  

  
#######################################################################################
      ## Read the GIS layers
#######################################################################################
 
      
  boundaryRW <- readOGR(dsn=getwd(), layer="gadm36_RWA_1")
  RWRegion <- readOGR(dsn=getwd(), layer="gadm36_RWA_2")
  
  ###########################################################################
  ## RW fertilizer recom for FCY 1:5
  ###########################################################################
  FR_RW_FCY <- readRDS("RW_CassavaPaperBased.RDS")
  #FCY_FRData <- readRDS("C:/Users/User/Documents/ACAI/paper based/PaperBasedAnnex_RW/Input/readGISlayers") 
  names(FR_RW_FCY)[18] <- "DISTRICT"
  FCY_FRData <- FR_RW_FCY[FR_RW_FCY$harvMonth == 12, ]
  
  
  FR_RW_FCY1 <- FCY_FRData[FCY_FRData$FCY == "level1", ]
  FR_RW_FCY2 <- FCY_FRData[FCY_FRData$FCY == "level2", ]
  FR_RW_FCY3 <- FCY_FRData[FCY_FRData$FCY == "level3", ]
  FR_RW_FCY4 <- FCY_FRData[FCY_FRData$FCY == "level4", ]
  FR_RW_FCY5 <- FCY_FRData[FCY_FRData$FCY == "level5", ]
  head(FCY_FRData)
  
  unique(FCY_FRData$STATE)
  unique(FCY_FRData$DISTRICT)
  
  ###########################################################################
  ##  adding planting month
  ###########################################################################
  country <- "RW"
  addplm <- function(ds, country){
    ds$respY <- ds$TargetY - ds$CurrentY
    ds$groRev <- ds$NR + ds$TC
    ds$plm <- as.factor(ds$plw)
    
    if(country == "RW"){
      ds$plm  <- ifelse(ds$plm %in% c(6,7,8,9), "February", 
                        ifelse(ds$plm %in% c(10,11,12,13,14), "March",
                               ifelse(ds$plm %in% c(37,38,39,40), "September", "October")))
      
    }else{
      levels(ds$plm)[levels(ds$plm) %in% 1:4]   <- "January"
      levels(ds$plm)[levels(ds$plm) %in% 5:8]   <- "February"
      levels(ds$plm)[levels(ds$plm) %in% 9:13]  <- "March"
      levels(ds$plm)[levels(ds$plm) %in% 14:17] <- "April"
      levels(ds$plm)[levels(ds$plm) %in% 18:22] <- "May"
      levels(ds$plm)[levels(ds$plm) %in% 23:26] <- "June"
      levels(ds$plm)[levels(ds$plm) %in% 27:30] <- "July"
      levels(ds$plm)[levels(ds$plm) %in% 31:35] <- "August"
      levels(ds$plm)[levels(ds$plm) %in% 36:39] <- "September"
      levels(ds$plm)[levels(ds$plm) %in% 40:43] <- "October"
      levels(ds$plm)[levels(ds$plm) %in% 44:48] <- "November"
      levels(ds$plm)[levels(ds$plm) %in% 49:53] <- "December"
    }
    
    
    if (country == "RW"){
      ds$rateUrea <- ds$Urea
      ds$rateNPK171717 <- ds$NPK17_17_17
      ds$rateMOP <- ds$MOP
      ds$rateNDAP <- ds$DAP
    }
    return(ds)
  }
  
  
  FR_RW_FCY1_plm <- addplm(ds=FR_RW_FCY1, country = "RW") ## RW if user current yield is level 1
  FR_RW_FCY2_plm <- addplm(ds=FR_RW_FCY2, country = "RW") ## RW if user current yield is level 2
  FR_RW_FCY3_plm <- addplm(ds=FR_RW_FCY3, country = "RW") ## RW if user current yield is level 3
  FR_RW_FCY4_plm <- addplm(ds=FR_RW_FCY4, country = "RW") ## RW if user current yield is level 4
  FR_RW_FCY5_plm <- addplm(ds=FR_RW_FCY5, country = "RW") ## RW if user current yield is level 5
  
  
      ###########################################################################
      ## select FCY and read the corresponding file 
      ## NG: Subsetting for the user defined Region and selecting a coordinate to put the state name in the map
      ###########################################################################
      
      #.................................................................................................................
      #Dashboard activity starts here
      #............................................................................. 
      ## Determine platform type and set working directory accordingly
   
      
      
    observeEvent(input$btn_go, {
    
    #define reactive values
   # country <- input$country
  
    FCY_are_rw <- input$FCY_are_rw
    FCY_ha_rw <- input$FCY_ha_rw
   

    Selection2 <- input$selection2

   # usecase <- input$usecase
    plantMonth <- input$plntmth
    state <- input$state
  
    lga_Groups <- input$state
  
    plantMonth <- input$plntmth
    cities <- input$city
    
    unit_rw <- input$unit_loc_rw
    UreaPrice <- as.numeric(input$UreaPrice)
    DAPPrice <- as.numeric(input$DAPPrice)
    MOPPrice <- as.numeric(input$MOPPrice)
    NPK171717Price <- as.numeric(input$NPK171717Price)
    
    CassavaPrice <- as.numeric(input$CassavaPrice)

    costs <- input$costs


    print(unit_rw)
    print(plantMonth)
    print(FCY_are_rw)
    
    #specify yield categories


    #Meklit here
    
    if(unit_rw == 'are'){
      yield_level <- ifelse(FCY_are_rw == "0-75 kg/are","a low yield level",
                             ifelse(FCY_are_rw == "75-150 kg/are","a normal yield level",
                                     ifelse(FCY_are_rw == "150-225 kg/are","a medium yield level",
                                             ifelse(FCY_are_rw == "225-300 kg/are","a high yield level",
                                                     ifelse(FCY_are_rw == ">300 kg/are","a very high yield level")
                                             ))))
    }else if(unit_rw == 'ha'){ 
      yield_level <- ifelse(FCY_ha_rw == "0-7.5 t/ha","a low yield level",
                             ifelse(FCY_ha_rw == "7.5-15 t/ha","a normal yield level",
                                     ifelse(FCY_ha_rw == "15-22.5 t/ha","a medium yield level",
                                             ifelse(FCY_ha_rw == "22.5-30 t/ha","a high yield level",
                                                     ifelse(FCY_ha_rw == ">30 t/ha","a very high yield level")
                                             ))))
    }
   

    
    print(yield_level)
    
    #define the state category
    if(state == "Southern"){
      lgaGroups <- "Amajyepfo"
    }else if(state == "Kigali City"){
      lgaGroups <- "Umujyi wa Kigali"
    }else if(state == "Western"){
      lgaGroups <- "Iburengerazuba"
    }else if (state == "Eastern"){
      lgaGroups <- "Iburasirazuba"
    }else if (state == "Nothern"){
      lgaGroups <- "Amajyaruguru"
    }

    
     #lgaGroups <- input$state
     lgaGroups2 <- input$state
  
      #define the yield category
      if (unit_rw == "ha"){
        FCY <- FCY_ha_rw
        if(FCY == "0-7.5 t/ha"){
          ds=FR_RW_FCY1_plm
        } else if(FCY == "7.5-15 t/ha"){
          ds=FR_RW_FCY2_plm
        }else if(FCY == "15-22.5 t/ha"){
          ds=FR_RW_FCY3_plm
        }else if(FCY == "22.5-30 t/ha"){
          ds <- FR_RW_FCY4_plm
        }else if(FCY == ">30 t/ha"){
          ds <- FR_RW_FCY5_plm
        }
      }else if(unit_rw == "are"){
        FCY <- FCY_are_rw
        if(FCY == "0-75 kg/are" ){
          ds=FR_RW_FCY2_plm
        } else if(FCY == "75-150 kg/are"){
          ds=FR_RW_FCY1_plm
        }else if(FCY == "150-225 kg/are" ){
          ds=FR_RW_FCY3_plm
        }else if(FCY == "225-300 kg/are" ){
          ds <- FR_RW_FCY4_plm
        }else if(FCY == ">300 kg/are" ){
          ds <- FR_RW_FCY5_plm
        }
      }
      
    #ds <- FR_RW_FCY2_plm ## ds will be defined based on the current yield. e.g. if user idicate that thier current yield is yield level 2 ds  = FR_NG_FCY2_plm
    #unique(ds$STATE)
    ds$REGION <- ds$STATE
    Amajyaruguru <- droplevels(ds[ds$STATE == "Amajyaruguru", ])
    Amajyarugurulabel <- data.frame(state= c("Amajyaruguru"), lat=c(-1.34), lon=c(30.05))
    
    Amajyepfo <- droplevels(ds[ds$STATE == "Amajyepfo", ])
    Amajyepfolabel <- data.frame(state= c("Amajyepfo"), lat=c(-1.8), lon=c(29.48))
    
    Iburasirazuba <- droplevels(ds[ds$STATE == "Iburasirazuba", ])
    Iburasirazubalabel <- data.frame(state= c("Iburasirazuba"), lat=c(-1.1), lon=c(30.4))
    
    Iburengerazuba <- droplevels(ds[ds$STATE == "Iburengerazuba", ])
    Iburengerazubalabel <- data.frame(state= c("Iburengerazuba"), lat=c(-1.5), lon=c(29.19))
    
    Kigali <- droplevels(ds[ds$STATE == "Umujyi wa Kigali", ])
    Kigalilabel <- data.frame(state= c("Umujyi wa Kigali"), lat=c(-1.79), lon=c(30.1))
    
    
    Amajyarugurucity <- data.frame(REGION = c("Amajyaruguru"),name=c("Byumba"), lat=c(-1.57), lon = c(30.06))
    Amajyepfocity <- data.frame(REGION = c("Amajyepfo"),name=c("Nyanza"), lat=c(-2.35), lon = c(29.75))
    Iburasirazubacity <- data.frame(REGION = c("Iburasirazuba"),name=c("Rwamagana"), lat=c(-1.95), lon = c(30.43))
    Iburengerazubacity <- data.frame(REGION = c("Iburengerazuba"),name=c("Kibuye"), lat=c(-2.06), lon = c(29.34))
    Kigalicity <- data.frame(REGION = c("Umujyi wa Kigali"),name=c("Kigali"), lat=c(-1.95), lon = c(30.07))
   
      
    if(lgaGroups =="Amajyaruguru"){
      LGApoints <- Amajyaruguru
      stateLabel <- Amajyarugurulabel
      textangle<-0
      cities = Amajyarugurucity
      engname = "Northern"
      

    }else if(lgaGroups =="Amajyepfo"){
      LGApoints <- Amajyepfo
      stateLabel <- Amajyepfolabel
      textangle<-0
      cities = Amajyepfocity
      couple <- "One"
      engname = "Southern"

    }else if(lgaGroups =="Iburengerazuba"){
      LGApoints <- Iburengerazuba
      stateLabel <- Iburengerazubalabel
      textangle<-0
      cities = Iburengerazubacity
      couple <- "One"
      engname = "Western"

    }else if(lgaGroups =="Iburasirazuba"){
      LGApoints <- Iburasirazuba
      stateLabel <- Iburasirazubalabel
      textangle<-0
      cities = Iburasirazubacity
      couple <- "One"
      engname = "Eastern"

    }else if(lgaGroups =="Umujyi wa Kigali"){
      LGApoints <- Kigali
      stateLabel <- Kigalilabel
      textangle<-0
      cities = Kigalicity
      couple <- "One"
      engname = "Kigali"

    }
    
   # plantMonth <- "September"
    
    plotData <- droplevels(LGApoints[LGApoints$plm == plantMonth & LGApoints$REGION %in% lgaGroups , ])
    
    
    AOI <- lgaGroups
    AOIMapS <- subset(boundaryRW, NAME_1 %in% AOI ) 
    
    AOIMap <- subset(RWRegion, NAME_1 %in% AOI )
    AOIMap <- AOIMap[,c("NAME_1", "NAME_2")]
    LGAnames <- as.data.frame(AOIMap)
    LGAnames <- cbind(LGAnames, coordinates(AOIMap))
    colnames(LGAnames) <- c("REGION","DISTRICT","long","lat")
    crop_ngstate <- subset(RWRegion, NAME_1 %in% AOI )
    
    
    ## take REGION average
    LGAaverage <- ddply(plotData, .(DISTRICT, REGION), summarize,
                        LGAUrea = round(mean(rateUrea), digits=0),
                        LGANPK171717 = round(mean(rateNPK171717 ), digits=0),
                        LGAMOP = round(mean(rateMOP), digits=0),
                        LGADAP = round(mean(rateNDAP), digits=0),
                        LGAdY = round(mean(respY), digits=0))
    
    
    LGAaverage$LGAUrea <- ifelse(LGAaverage$LGAUrea <25, 0, LGAaverage$LGAUrea)
    LGAaverage$LGANPK171717 <- ifelse(LGAaverage$LGANPK171717 <25, 0, LGAaverage$LGANPK171717)
    LGAaverage$LGAMOP <- ifelse(LGAaverage$LGAMOP <25, 0, LGAaverage$LGAMOP)
    LGAaverage$LGADAP <- ifelse(LGAaverage$LGADAP <25, 0, LGAaverage$LGADAP)
    
    
    plotData <- merge(plotData, LGAaverage, by=c("DISTRICT", "REGION"))
    
    
    plotData$Urea <- round(plotData$LGAUrea/10)*10
    plotData$NPK17_17_17 <- round(plotData$LGANPK171717/10)*10
    plotData$MOP <- round(plotData$LGAMOP /5)*5
    plotData$DAP <- round(plotData$LGADAP/5)*5
    plotData$dY <- round(plotData$LGAdY/1)*1
    
    
    fileNameCsv <- paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".csv", sep="")
    
    AOIMap2 <- merge(AOIMap, unique(plotData[, c("REGION","DISTRICT", "Urea", "NPK17_17_17","MOP", "DAP", "LGAdY")]),
                     by.x=c("NAME_1","NAME_2") ,by.y=c("REGION","DISTRICT"))
    AOIMap2$month <- plantMonth
    AOIMap2 <- AOIMap2[!is.na(AOIMap2$Urea), ]
    plotData$month <- plantMonth

     Currency <- "RWF"
     tt_rw <- unique(as.data.frame(plotData[, c("STATE","DISTRICT", "Urea", "NPK17_17_17","MOP", "DAP", "LGAdY", "month")]))
     
     
     tt_rw$LGAdY <- round(tt_rw$LGAdY, digits = 1)
     tt_rw2 <- dplyr::select(tt_rw, c(STATE, DISTRICT, Urea, NPK17_17_17, MOP,DAP, LGAdY))
     tt_rw2$STATE <- ifelse(tt_rw2$STATE == "Umujyi wa Kigali", "Kigali City",
                                  ifelse(tt_rw2$STATE == "Amajyepfo", "Southern",
                                         ifelse(tt_rw2$STATE == "Iburengerazuba", "Western",
                                                ifelse(tt_rw2$STATE == "Amajyaruguru", "Nothern",
                                                       ifelse(tt_rw2$STATE == "Iburasirazuba", "Eastern")))))
     
     tt_rw3 <- tt_rw2
     colnames(tt_rw3) <- c("Province","District", "Recommended urea", "NPK17_17_17", "MOP", "DAP", "Expected yield response")
     
     
     #table output based on cost inputs
     
     
     if(costs == "No"){
      #  output$mytable = DT::renderDataTable({
      #    data_output(tt_rw2)
      #  }, caption=paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep=""),
      # )
       
       output$tabletext_rwf<- renderText({
         
         
         paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is ", FCY, ".", sep="")
         
       })
       
       output$mytable <- renderDT({tt_rw3},
                                   rownames = FALSE, 
                                   extensions = c('Buttons','FixedColumns'), 
                                   options = list(dom = 'Bfrtip',
                                                  pageLength = nrow(tt_rw3),
                                                  initComplete = DT::JS(
                                                    "function(settings, json) {",
                                                    "$(this.api().table().header()).css({'background-color': 'black', 'color': '#fff'});",
                                                    "}"),
                                                  
                                                  buttons = list(
                                                    list(extend = 'excel', 
                                                         filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
                                                         title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY, ".", sep="")),
                                                    list(extend = 'pdf',
                                                         filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
                                                         title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  ".", sep=""),
                                                         header = TRUE)
                                                  )
                                                  
                                   )
       )
       
     }else if (costs == "Yes"){
       #colnames(tt) <- c("State","LGA", "Recommended urea rate (kg/ha)", "NPK15_15_15 rate", "Expected yield response", "Planting month")
       tt_dataframe2 <- reactive({
         
         # df_rw2 <- data.frame(UreaPrice=1000,NPK171717Price=5000,CassavaPrice=6000,DAPPrice=5000,MOPPrice=9000,
         #                      STATE="Southern")
         # 
         df_rw2 <- data.frame(UreaPrice=as.numeric(input$UreaPrice),NPK171717Price=as.numeric(input$NPK171717Price),
                              CassavaPrice=as.numeric(input$CassavaPrice),DAPPrice=as.numeric(input$DAPPrice),MOPPrice=as.numeric(input$MOPPrice),
                              STATE=input$state)
         
         return(df_rw2)
       })
       
       #df_tt <- data.frame(UreaPrice=UreaPrice,NPK171717Price=NPK171717Price,CassavaPrice=CassavaPrice,REGION=lga_Groups,DAPPrice=DAPPrice)
       #  
       print(CassavaPrice)
       tt_merge_rw <- merge(tt_rw2, tt_dataframe2(),by="STATE")
       #tt_merge_rw <- merge(tt_rw2, df_rw2,by="STATE")
       
       tt_merge_rw$totalSalePrice = tt_merge_rw$LGAdY  * tt_merge_rw$CassavaPrice
       tt_merge_rw$totalCost = (tt_merge_rw$UreaPrice/50 * tt_merge_rw$Urea) + 
         (tt_merge_rw$NPK171717Price/50 * tt_merge_rw$NPK17_17_17)+
         (tt_merge_rw$MOPPrice/50 * tt_merge_rw$MOP)+
        (tt_merge_rw$DAPPrice/50 * tt_merge_rw$DAP)
       #tt_merge$NetRevenue = tt_merge$totalSalePrice - tt_merge$totalCost
       #totalCost = (as.numeric(UreaPrice)/50 * 15) + (as.numeric(NPK151515Price)/50 * 300)
       
       tt_merge_rw$NetRevenue = tt_merge_rw$totalSalePrice - tt_merge_rw$totalCost
       
       tt_merge_rw2 <- dplyr::select(tt_merge_rw, c(STATE, DISTRICT, Urea, NPK17_17_17, DAP, LGAdY, CassavaPrice, totalSalePrice, totalCost, NetRevenue))
       # tt_merge_rw2$STATE <- ifelse(tt_merge_rw2$STATE == "Umujyi wa Kigali", "Kigali City",
       #                           ifelse(tt_merge_rw2$STATE == "Amajyepfo", "Southern",
       #                              ifelse(tt_merge_rw2$STATE == "Iburengerazuba", "Western",
       #                                ifelse(tt_merge_rw2$STATE == "Amajyaruguru", "Nothern",
       #                                  ifelse(tt_merge_rw2$STATE == "Iburasirazuba", "Eastern")))))
        colnames(tt_merge_rw2) <- c("Province","District", "Urea (kg/ha)", "NPK 17:17:17 (kg/ha)", "DAP (kg/ha)","Expected yield increase (t)",
                                   "Cassava Price",  "Total sale (RWF)", "Fertilizer cost (RWF)", "Profit (RWF)")
       
       
       write.csv(tt_merge_rw2, fileNameCsv, row.names = FALSE)
       
       AOIMap3 <- st_as_sf(AOIMap2)
       
       output$tabletext_rwf<- renderText({
         
         
         paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep="")
         
       })
       
       output$mytable2 <- renderDT({tt_merge_rw2},
                                   rownames = FALSE, 
                                   extensions = c('Buttons','FixedColumns'), 
                                   options = list(dom = 'Bfrtip',
                                                  pageLength = nrow(tt_merge_rw2),
                                                  initComplete = DT::JS(
                                                    "function(settings, json) {",
                                                    "$(this.api().table().header()).css({'background-color': 'black', 'color': '#fff'});",
                                                    "}"),
                                                  
                                                  buttons = list(
                                                    list(extend = 'excel', 
                                                         filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
                                                         title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep="")),
                                                    list(extend = 'pdf',
                                                         filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
                                                         title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep=""),
                                                         header = TRUE)
                                                  )
                                                  
                                   )
       )
       
     }
      # --------------------------------------------------------------------------
      #side by side maps
      # --------------------------------------------------------------------------
      AOIMap3 <- st_as_sf(AOIMap2)
      #urea plot
      ################################################
      
      #reactive  title based on unit of land
      tturea <- reactive({
        
        if(unit_rw == "ha"){
          
          tturea <- paste("Recommended urea rate(kg/ha)")
        }else {
          
          tturea <- paste("Recommended urea rate(kg/acre)")
        }
      })
      
      
      
      ureasclae <- unique(AOIMap3$Urea)
      keU <- as.character(ureasclae[order(ureasclae)])
      AOIMap3$Urea <- factor(AOIMap3$Urea)
      levels(AOIMap3$Urea) <- keU
      
      require(ggrepel)
      library(tmap)
      
      #tmap output using reactive values  
      #urea
      observeEvent(tturea(),
                   {
                     
                     output$ureaplot2 <- renderTmap({
                       
                       
                       sm1 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "Urea",
                           title = tturea(),
                           #breaks = c(200, 175, 150, 125,100),
                           # labels = c("Low", "Medium", "High"),
                           palette = "Greens")+
                         tm_text(text = "NAME_2")
                       sm1
                       
                       
                       
                     })
                   })
      ############################################################################   
      #npk plot
      #############################################################################
      #reactive title based on unit of land
      
      ttnpk <- reactive({
        
        if(unit_rw == "ha"){
          
          ttnpk <- paste("Recommended NPK 17:17:17 rate (kg/ha)")
        }else {
          
          ttnpk <- paste("Recommended NPK 17:17:17 rate(kg/acre)")
        }
      })
      
      
      
      mopsclae <- unique(AOIMap3$NPK17_17_17)
      kev <- as.character(mopsclae[order(mopsclae)])
      AOIMap3$NPK17_17_17 <- factor(AOIMap3$NPK17_17_17)
      levels(AOIMap3$NPK17_17_17) <- kev
      
      #npk plot
      observeEvent(ttnpk(),
                   {
                     
                     output$npkplot_tr <- renderTmap({
                       
                       
                       
                       sm2 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "NPK17_17_17",
                           title = ttnpk(),
                           #tm_borders(lwd = 1, col = "black", alpha = .5) +
                           # breaks = c(100, 110, 120, 130),
                           # labels = c("Low", "Medium", "High"),
                           palette = "Oranges")+
                         tm_text(text = "NAME_2")
                            
                       sm2
                       
                     }) 
                   })
      
      
      ############################################################################   
      #dap plot
      #############################################################################
      #reactive title based on unit of land  
      
      ttdap <- reactive({
        
        if(unit_rw == "ha"){
          
          ttdap <- paste("Recommended DAP (kg/ha)")
        }else {
          
          ttdap <- paste("Recommended DAP (kg/acre)")
        }
      })
      
      
      
      dapsclae <- unique(AOIMap3$DAP)
      kedap <- as.factor(dapsclae[order(dapsclae)])
      AOIMap3$DAP <- factor(AOIMap3$DAP)
      levels(AOIMap3$DAP) <- kedap
      
      #dap plot
      observeEvent(ttdap(),
                   {
                     
                     output$dapplot <- renderTmap({
                       
                       
                       
                       sm4 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "DAP",
                           title = ttdap(),
                           #tm_borders(lwd = 1, col = "black", alpha = .5) +
                           # breaks = c(100, 110, 120, 130),
                           # labels = c("Low", "Medium", "High"),
                           palette = "Purples", direction = -1)+
                         tm_text(text = "NAME_2")
                       sm4
                       
                     }) 
                   })
      
      ############################################################################   
      #mop plot
      #############################################################################
      #reactive title based on unit of land  
      
      ttmop <- reactive({
        
        if(unit_rw == "ha"){
          
          ttmop <- paste("Recommended MOP (kg/ha)")
        }else {
          
          ttmop <- paste("Recommended MOP (kg/acre)")
        }
      })
      
      
      
      mopsclae <- unique(AOIMap3$MOP)
      kemop <- as.factor(mopsclae[order(mopsclae)])
      AOIMap3$MOP <- factor(AOIMap3$MOP)
      levels(AOIMap3$MOP) <- kemop
      
      #dap plot
      observeEvent(ttmop(),
                   {
                     
                     output$mopplot <- renderTmap({
                       
                       
                       
                       sm4 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "MOP",
                           title = ttmop(),
                           #tm_borders(lwd = 1, col = "black", alpha = .5) +
                           # breaks = c(100, 110, 120, 130),
                           # labels = c("Low", "Medium", "High"),
                           palette = "GnBu", direction = -1)+
                         tm_text(text = "NAME_2")
                       sm4
                       
                     }) 
                   })
      
      ############################################################################   
      #yield plot
      #############################################################################
      #reactive title based on unit of land
      
      ttha <- reactive({
        
        if(unit_rw == "ha"){
          
          ttha <- paste("Recommended Yield response (t/ha)")
        }else {
          
          ttha <- paste("Recommended Yield response (t/acre)")
          
        }
      })
      
      
      # Ydcols <- unique(AOIMap3$dY)
      # keY <- as.factor(Ydcols[order(Ydcols)])
      # AOIMap3$dY <- factor(AOIMap3$dY)
      # levels(AOIMap3$dY) <- keY
      
      #yield plot
      observeEvent(ttha(),
                   {
                     
                     
                     output$yieldplot <- renderTmap({
                       
                       
                       sm3 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "dY",
                           title = ttha(),
                           #breaks = c(3, 4, 5, 6),
                           #labels = c("Low", "Medium", "High"),
                           palette = "YlGnBu")+
                         tm_text(text = "NAME_2")
                       
                       sm3
                       
                     })
                   })
      
      #-------------------------------
      #generate downloadable maps
      #-------------------------------
      
      #generate color pallette
      ggUrea <- NULL
      #if(any(AOIMap3$Urea>0)){
      if(unit_rw == "ha"){
        ureacols <- c("0" = "#FFFFFF", "90"= "#E5F5E0", "100"= "#C7E9C0", "110"= "#A1D99B", "120"= "#74C476",
                      "130"= "#41AB5D", "135"= "#238B45", "140"="#006D2C", "150"= "#00441B")
        
        trw <- "Urea (kg/ha)"
      }else {
        ureacols <- c("0" = "#FFFFFF", "10"= "#E5F5E0", "20"= "#C7E9C0", "30"= "#A1D99B", "40"= "#74C476",
                      "50"= "#41AB5D", "60"= "#238B45", "70"="#006D2C", "80"= "#00441B")
        trw <- "Urea (kg/acre)"
      }
      
      ureacols <- ureacols[names(ureacols) %in% AOIMap3$Urea]
 
      ureasclae <- unique(AOIMap3$Urea)
      keU <- as.character(ureasclae[order(ureasclae)])
      AOIMap3$Urea <- factor(AOIMap3$Urea)
      levels(AOIMap3$Urea) <- keU

      require(ggrepel)

      #ggplot urea

      ggUrea <- ggplot(AOIMap3) +
        geom_sf(aes(fill=Urea), col="darkgrey") +
        scale_fill_manual(values = ureacols, guide = guide_legend(reverse=TRUE))+
        geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
        geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
        geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
        geom_text(data=stateLabel, aes(lon, lat, label=engname, fontface=2), col='black', size=6)+
        geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        annotation_scale(location = "bl", width_hint = 0.3, line_width = 0.4) +
        annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                               style = north_arrow_fancy_orienteering) +
        xlab("") + ylab("") +
        ggtitle(trw) +
        theme_bw() +
        theme(legend.position="right", legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
              axis.text = element_text(size=8)) 
      #}
      

      ggNPK171717 <- NULL
      ### NPK 151515 palette <- brewer.pal(9,"YlOrBr")
      #if(any(AOIMap3$NPK17_17_17>0)){
      if(unit_rw == "ha"){
        NPKcols <- c("0"="#FFFFFF","35"= "#FFF7BC", "40"= "#FEE391", "45"= "#FEC44F", "50"= "#FE9929", 
                     "55"= "#EC7014", "60"= "#CC4C02","65" = "#993404", "70"= "#662506")
        
        trw <- "NPK 17:17:17 (kg/ha)"
      }else{
        NPKcols <- c("40"="#FFFFFF","50"= "#FFF7BC", "60"= "#FEE391", "70"= "#FEC44F", "80"= "#FE9929", 
                     "90"= "#EC7014", "100"= "#CC4C02", "110" ="#993404","120" = "#662506")
        trw <- "NPK 17:17:17 (kg/acre)"
      }

        NPKcols <- NPKcols[names(NPKcols) %in% AOIMap3$NPK17_17_17]
        
        NPK17_17_17sclae <- unique(AOIMap3$NPK17_17_17)
        kev <- as.character(NPK17_17_17sclae[order(NPK17_17_17sclae)])
        AOIMap3$NPK17_17_17 <- factor(AOIMap3$NPK17_17_17)
        levels(AOIMap3$NPK17_17_17) <- kev
        

      #ggplot NPK
        ggNPK171717 <- ggplot(AOIMap3) +
          geom_sf(aes(fill=NPK17_17_17), col="darkgrey") +
          
          scale_fill_manual(values = NPKcols, guide = guide_legend(reverse=TRUE))+
          geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
          geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
          geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
          geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
          xlab("") + ylab("") +
          ggtitle(trw) +
          theme_bw() +
          theme(legend.position="right", legend.title=element_blank(),
                plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
                axis.text = element_text(size=8))
        
     # }

      
      ggMOP <- NULL
     # if(any(AOIMap3$MOP>0)){
        DAPPpalette <- brewer.pal(9,"YlGnBu")
        if(unit_rw == "ha"){
          MOPcols <- c("0"="#FFFFFF", "40" = "#FFFFD9" ,"45" ="#EDF8B1" ,"50"= "#C7E9B4", "55"= "#7FCDBB", "60"= "#41B6C4",
                       "65"= "#1D91C0","70"="#9999FF", "75"="#0000FF", "80"= "#225EA8", "85"= "#253494", "90"= "#081D58")
          trw <- "MOP (kg/ha)"
        }else{
          MOPcols <- c("0"="#FFFFFF","10"= "#C7E9B4", "20"= "#7FCDBB", "30"= "#41B6C4",
                       "40"= "#1D91C0", "50"= "#225EA8", "60"= "#253494", "70"= "#081D58")
          trw <- "NPK 12:30:17 (kg/acre)"
        }
        
        MOPcols <- MOPcols[names(MOPcols) %in% AOIMap3$MOP]
        
        MOPsclae <- unique(AOIMap3$MOP)
        kev <- as.character(MOPsclae[order(MOPsclae)])
        AOIMap3$MOP <- factor(AOIMap3$MOP)
        levels(AOIMap3$MOP) <- kev
        
        ggMOP <- ggplot(AOIMap3) +
          geom_sf(aes(fill=MOP), col="darkgrey") +
          
          scale_fill_manual(values = DAPPpalette, guide = guide_legend(reverse=TRUE))+
          geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
          geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
          geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) +
          geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
          xlab("") + ylab("") +
          ggtitle(trw) +
          theme_bw() +
          theme(legend.position="right", legend.title=element_blank(),
                plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
                axis.text = element_text(size=8))
    #  }
      

      ggDAP <- NULL
      # DAP color pallette
      #if(any(AOIMap3$DAP>0)){
        DAPpalette <- brewer.pal(9,"Purples")
      if(unit_rw == "ha"){
        DAPcols <- c("0"="#FCFBFD","25"= "#EFEDF5", "50"= "#DADAEB", "75"= "#BCBDDC",
                     "80"= "#9E9AC8", "100"= "#807DBA", "125"= "#6A51A3", "150"= "#54278F", "175" = "#3F007D")
        trw <- "DAP (kg/ha)"
      }else{
        DAPcols <- c("0"="#FCFBFD","10"= "#EFEDF5", "20"= "#DADAEB", "30"= "#BCBDDC",
                     "40"= "#9E9AC8", "50"= "#807DBA", "60"= "#6A51A3", "70"= "#54278F", "80" = "#3F007D")
        trw <- "DAP (kg/acre)"
      }

        DAPcols <- DAPcols[names(DAPcols) %in% AOIMap3$DAP]
        
        
        DAPsclae <- unique(AOIMap3$DAP)
        keDAP <- as.factor(DAPsclae[order(DAPsclae)])
        AOIMap3$DAP <- factor(AOIMap3$DAP)
        levels(AOIMap3$DAP) <- keDAP
        
        ggDAP <- ggplot(AOIMap3) +
          geom_sf(aes(fill=DAP), col="darkgrey") +
          scale_fill_manual(values = DAPpalette, guide = guide_legend(reverse=TRUE))+
          geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
          geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
          geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) +
          geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
          xlab("") + ylab("") +
          ggtitle(trw) +
          theme_bw() +
          theme(legend.position="right", legend.title=element_blank(),
                plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
                axis.text = element_text(size=8))
      #}
      

      if(unit_rw == "ha"){
        Ydcols <- c( "17"= "#E8FF00FF", "16"="#A2FF00FF", "15"= "#5DFF00FF", "14"= "#17FF00FF", "13"= "#00FF2EFF", "12"= "#00FF74FF",
                     "11"="#00FFB9FF", "10"= "#00FFFFFF", "9"= "#00B9FFFF", "8"= "#0074FFFF", "7"= "#002EFFFF",
                     "6"="#1700FFFF", "5"= "#5D00FFFF", "4"= "#A200FFFF", "3"= "#E800FFFF", "2"= "#FF00D1FF",
                     "1"= "#FF008BFF", "0"= "#FFFFFF")
        tt <- "Yield increase (t/ha)"
      }else{
        Ydcols <- c("14"= "#17FF00FF", "13"= "#00FF2EFF", "12"= "#00FF74FF",
                    "11"="#00FFB9FF", "10"= "#00FFFFFF", "9"= "#00B9FFFF", "8"= "#0074FFFF", "7"= "#002EFFFF",
                    "6"="#1700FFFF", "5"= "#5D00FFFF", "4"= "#A200FFFF", "3"= "#E800FFFF", "2"= "#FF00D1FF",
                    "1"= "#FF008BFF", "0"= "#FFFFFF")
        tt <- "Yield increase (t/acre)"
      }

      
      if(unit_rw == "ha"){
        trw <- "Yield increase (t/ha)"
      }else{
        trw <- "Yield increase (t/are)"
      }

      Ysclae <- unique(AOIMap3$LGAdY)
      keY <- as.factor(Ysclae[order(Ysclae)])
      AOIMap3$dY <- factor(AOIMap3$LGAdY)
      levels(AOIMap3$dY) <- keY
      
      Ydcols <- Ydcols[names(Ydcols) %in% AOIMap3$dY]
      
      ggYield <- ggplot(AOIMap3) +
        geom_sf(aes(fill=dY), col="darkgrey") +
        
        scale_fill_manual(values = Ydcols, guide = guide_legend(reverse=TRUE))+
        geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
        geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
        geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
        #geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
        #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
        geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        xlab("") + ylab("") +
        ggtitle(trw) +
        theme_bw() +
        theme(legend.position="right", legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
              axis.text = element_text(size=8))

      
      #Combine plots together in pdf
      fileName <- paste("maps", ".pdf", sep="")
      pdf(fileName, onefile = TRUE, height = 14, width=12)
  
      #pdf.options(paper = "a4")
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(5, 2, heights = unit(c(0.6, 4, 4, 4, 0.4), "null"))))
      grid.text(paste("Planting in", plantMonth, sep=" "), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))

        
      print(ggUrea, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))         
      print(ggNPK171717, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
      print(ggMOP, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
      print(ggDAP, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
      print(ggYield, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
      dev.off()

      # Ureapalette <- brewer.pal(9,"Greens")
      # colpallets <- c(mypalette[c((9-length(unique(plotData$Urea))): length(mypalette))])

      # #-------------------------------------------------------------------------
      # #front page dynamic tmap
      # #-------------------------------------------------------------------------
      # 
      #reactive selection of variable to view
      filt_select <- reactive({
        print(Selection2)
        if (Selection2 == "Urea rate"){
          filt_select <- "Urea rate"
        }else if (Selection2 == "Expected yield response"){
          filt_select <- "Expected yield response"
        }else if (Selection2 == "NPK 17:17:17 rate"){
          filt_select <- "NPK 17:17:17 rate"
        }else if (Selection2 == "NPK 15:15:15 rate"){
          filt_select <- "NPK 15:15:15 rate"
        }else if (Selection2 == "MOP rate"){
          filt_select <- "MOP rate"
        }else{
          filt_select <- "DAP rate"

        }

      })

      # choices = c("NPK 15:15:15 rate", "Expected yield response", "Urea rate"),
      # choices = c("NPK 17:17:17 rate", "DAP rate", "Expected yield response", "Urea rate"),

      #show map based on selection of variable but retaining single name

      #filter by variable selected and unit for color pallette
      observeEvent(filt_select(), {
        if (filt_select() == "Urea rate"){

          ureacols <- reactive({

            if(unit_rw == "ha"){
              ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                            "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
              tturea <- "Urea (kg/ha)"
            }else {
              ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                            "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
              tturea <- "Urea (kg/are)"
            }
          })

          
          
          #reactive legend title
          tturea <- reactive({

            if(unit_rw == "ha"){

              tturea <- paste("Recommended urea rate(kg/ha)")
            }else {

              tturea <- paste("Recommended urea rate (kg/are)")
            }
          })



          ureasclae <- unique(AOIMap3$Urea)
          keU <- as.character(ureasclae[order(ureasclae)])
          AOIMap3$Urea <- factor(AOIMap3$Urea)
          levels(AOIMap3$Urea) <- keU

          require(ggrepel)
          library(tmap)


          #urea
          observeEvent(tturea(),
                       {

                         output$tmapplot <- renderTmap({


                           sm1 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "Urea",
                               title = tturea(),
                               #breaks = c(200, 175, 150, 125,100),
                               # labels = c("Low", "Medium", "High"),
                               palette = "Greens")+
                             tm_text(text = "NAME_2")
                           sm1



                         })
                       })
        }else if(filt_select() == "NPK 17:17:17 rate"){

          ttnpk <- reactive({

            if(unit_rw == "ha"){

              ttnpk <- paste("Recommended NPK 17:17:17 rate (kg/ha)")
            }else {

              ttnpk <- paste("Recommended NPK 17:17:17 rate (kg/are)")
            }
          })



          mopsclae <- unique(AOIMap3$NPK17_17_17)
          kev <- as.character(mopsclae[order(mopsclae)])
          AOIMap3$NPK17_17_17 <- factor(AOIMap3$NPK17_17_17)
          levels(AOIMap3$NPK17_17_17) <- kev

          observeEvent(ttnpk(),
                       {

                         output$tmapplot <- renderTmap({



                           sm2 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "NPK17_17_17",
                               title = ttnpk(),
                               palette = "Oranges")+
                             tm_text(text = "NAME_2")

                           sm2

                         })
                       })

        }else if(filt_select() == "DAP rate"){
          ttdap <- reactive({

            if(unit_rw == "ha"){

              ttdap <- paste("Recommended DAP rate (kg/ha)")
            }else {

              ttdap <- paste("Recommended DAP rate (kg/are)")
            }
          })



          dapsclae <- unique(AOIMap3$DAP)
          kedap <- as.factor(dapsclae[order(dapsclae)])
          AOIMap3$DAP <- factor(AOIMap3$DAP)
          levels(AOIMap3$DAP) <- kedap

          observeEvent(ttdap(),
                       {

                         output$tmapplot <- renderTmap({



                           sm4 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "DAP",
                               title = ttdap(),
                               palette = "Purples", direction = -1)+
                             tm_text(text = "NAME_2")
                           sm4

                         })
                       })

        }else if(filt_select() == "MOP rate"){
          ttmop <- reactive({
            
            if(unit_rw == "ha"){
              
              ttmop <- paste("Recommended MOP rate (kg/ha)")
            }else {
              
              ttmop <- paste("Recommended MOP rate (kg/are)")
            }
          })
          
 
          MOPsclae <- unique(AOIMap3$MOP)
          kev <- as.character(MOPsclae[order(MOPsclae)])
          AOIMap3$MOP <- factor(AOIMap3$MOP)
          levels(AOIMap3$MOP) <- kev
          
          observeEvent(ttmop(),
                       {
                         
                         output$tmapplot <- renderTmap({
                           
                           
                           
                           sm4 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "MOP",
                               title = ttmop(),
                               palette = "GnBu", direction = -1)+
                             tm_text(text = "NAME_2")
                           sm4
                           
                         })
                       })
          
        }else if(filt_select() == "Expected yield response"){
          ttha <- reactive({

            if(unit_rw == "ha"){

              ttha <- paste("Recommended yield increase (t/ha)")
            }else {

              ttha <- paste("Recommended yield increase (t/are)")

            }
          })


          # Ydcols <- unique(AOIMap3$dY)
          # keY <- as.factor(Ydcols[order(Ydcols)])
          # AOIMap3$dY <- factor(AOIMap3$dY)
          # levels(AOIMap3$dY) <- keY

          observeEvent(ttha(),
                       {


                         output$tmapplot <- renderTmap({


                           sm3 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "dY",
                               title = ttha(),
                               palette = "YlGnBu")+
                             tm_text(text = "NAME_2")

                           sm3

                         })
                       })



        }


      })
      
    
      
      
      if ( unit_rw == "are"){
        
        #download acre printable guides
        output$downloadDatafr <- 
          
          downloadHandler(
            filename <- function() {
              paste("FR Printable guides (acre)",  ".pdf", sep="")
            },
            
            content <- function(file) {
              file.copy("data/Tailored fertilizer application recommendations for cassava - Tanzania Acre latest.pdf", file)
            },
            contentType = "application/pdf"
          )
        
      }else if(unit_rw == "ha"){
        #download hectare printable guides
        output$downloadDatafr <- 
          downloadHandler(
            filename <- function() {
              paste("FR Printable guides (ha)",  ".pdf", sep="")
            },
            
            content <- function(file) {
              file.copy("data/Tailored fertilizer application recommendations for cassava - Tanzania Hectare latest.pdf", file)
            },
            contentType = "application/pdf"
          ) 
        
        
      }  
 
    
    
    output$sidetext <- renderText({

 
      # paste0('<span style=\"background-color:', "color", '\ ">',text,' #<span style=\"font-size:8px;font-weight:bold;background-color:white;">',"ent_type",'</span></span>')
      paste("Maps and tables below present fertilizer recommendations for cassava planted in", plantMonth, "in", lgaGroups2, "province, in a field with", yield_level,
            ". Recommendations are optimized to obtain a maximal return on investment, assuming cassava will be harvested after 12 months.
              ")
    
    })
    

    
  
    #download maps
    output$downloadData <- downloadHandler(
      filename <- function() {
        paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".pdf", sep="")
      },
      
      content <- function(file) {
        file.copy("maps.pdf", file)
      },
      contentType = "application/pdf"
    )
    
    #download tables
    output$downloadcsv <- downloadHandler(
      filename <- function() {
        paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".csv", sep="")
      },
      
      content <- function(file) {
        file.copy("tables.csv", file)
      },
      contentType = "application/csv"
    )
    
 
     })
  #})
  
}


#runApp(shinyApp(ui, server), launch.browser = TRUE)
#shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")
