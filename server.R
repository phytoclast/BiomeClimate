#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
#calculate percentiles
library(plyr)


######
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

 rv <- reactiveValues(my_text = "") 
    #parameters
    lat <- c(0, 80)
    lon <- c(-150, -50) 
    elev <- c(-432, 8848) 
    temp <- c(-30, 35) 
    prec <- c(0, 9000) 
    biomnm <- ''
    econame <- ''
    
    output$Biome = renderUI({
      
      listBiome1 <- subset(Biomeclimate,
                           Latitude >= input$lat[1] &
                             Latitude <= input$lat[2] &
                             Longitude >= input$lon[1] &
                             Longitude <= input$lon[2] &
                             Elevation >= input$elev[1] &
                             Elevation <= input$elev[2] &
                             TT >= input$temp[1] &
                             TT <= input$temp[2]&
                             PP >= input$prec[1] &
                             PP <= input$prec[2]
      )
      listBiome <- unique(listBiome1[, c('biomname')])
      
      
      selectInput(inputId = "Biome",
                  label = "Select Biome:",
                  choices = listBiome,
                  selected = "")
    })
    
    
    output$ECO_NAME = renderUI({

      listEco1 <- subset(Biomeclimate,
                           Latitude >= input$lat[1] &
                           Latitude <= input$lat[2] &
                           Longitude >= input$lon[1] &
                           Longitude <= input$lon[2] &
                           Elevation >= input$elev[1] &
                           Elevation <= input$elev[2] &
                           TT >= input$temp[1] &
                           TT <= input$temp[2] &
                           PP >= input$prec[1] &
                           PP <= input$prec[2] &
                           grepl(input$Biome, biomname))
      listEco <- unique(listEco1[, c('ECO_NAME')])
      
      selectInput(inputId = "ECO_NAME", #name of input
                  label = "Select WWF Ecoregion:", #label displayed in ui
                  choices = unique(listEco), #calls list of available counties
                  selected = unique(listEco)[1])
    })
    output$elev = renderUI({
      
      sliderInput(inputId = 'elev',
                  label = 'Elevation range',
                  min= -500, max= 10000,
                  value= c(-432, 8848), step = 50,
                  dragRange = TRUE)
      
    })
    output$lat = renderUI({
      
      sliderInput(inputId = 'lat',
                  label = 'Latitude range',
                  min= -90, max= 90,
                  value= c(25, 50), step = 5,
                  dragRange = TRUE)
      
    })
    output$lon = renderUI({
      
      sliderInput(inputId = 'lon',
                  label = 'Longitude range',
                  min= -180, max= 180,
                  value= c(-150, -50), step = 5,
                  dragRange = TRUE)
      
    })

    output$temp = renderUI({
      
      sliderInput(inputId = 'temp',
                  label = 'Mean Annual Air Temperature range',
                  min= -30, max= 35,
                  value= c(-30, 35), step = 1,
                  dragRange = TRUE)
      
    })

    output$prec = renderUI({

      sliderInput(inputId = 'prec',
                  label = 'Mean Annual Precipitation range',
                  min= 0, max= 9000,
                  value= c(0, 9000), step = 50,
                  dragRange = TRUE)
      
    })
    
    
       output$climplot <- renderPlot({ 

    #parameters
    lat <- c(0, 80)
    lon <- c(-150, -50) 
    elev <- c(-432, 8848) 
    temp <- c(-30, 35) 
    prec <- c(0, 9000) 
    biomnm <- input$Biome
    econame <- input$ECO_NAME
    

    
    
    selectClim <- subset(Biomeclimate,
                         Latitude >= input$lat[1] &
                           Latitude <= input$lat[2] &
                           Longitude >= input$lon[1] &
                           Longitude <= input$lon[2] &
                           Elevation >= input$elev[1] &
                           Elevation <= input$elev[2] &
                           TT >= input$temp[1] &
                           TT <= input$temp[2] &
                           PP >= input$prec[1] &
                           PP <= input$prec[2] &
                           grepl(biomnm, biomname) &
                           grepl(econame, ECO_NAME))
    
    #Make Monthly Rows
    
    #Jan
    selectMonthly <- selectClim[,c("ECO_ID","ECO_NAME","BIOME","Latitude","Longitude","Elevation","t01","tl01","th01","p01","e01")]
    colnames(selectMonthly) <- c("ECO_ID","ECO_NAME","BIOME","Latitude","Longitude","Elevation","t","tl","th","p","e")
    selectMonthly$Month<- 1
    
    #Feb-Dec
    for (i in 1:11){
      
      selectMonthlyA <- selectClim[,c("ECO_ID","ECO_NAME","BIOME","Latitude","Longitude", "Elevation",
                                      colnames(selectClim)[which(colnames(selectClim)=='t01')+i],
                                      colnames(selectClim)[which(colnames(selectClim)=='tl01')+i],
                                      colnames(selectClim)[which(colnames(selectClim)=='th01')+i],
                                      colnames(selectClim)[which(colnames(selectClim)=='p01')+i],
                                      colnames(selectClim)[which(colnames(selectClim)=='e01')+i])]
      colnames(selectMonthlyA)<- c("ECO_ID","ECO_NAME","BIOME","Latitude","Longitude","Elevation","t","tl","th","p","e")
      selectMonthlyA$Month<- i+1
      selectMonthly <- rbind(selectMonthly, selectMonthlyA)
    }
    rm(selectMonthlyA)
    sumMonthly <- ddply(selectMonthly, "Month", summarise,
                        t25 = quantile(t, 0.25), t75 = quantile(t, 0.75), t = mean(t),
                        p25 = quantile(p, 0.25), p75 = quantile(p, 0.75), p = mean(p), 
                        e = mean(e), tl = mean(tl), th = mean(th))
    
    
    
    T <- apply(selectClim[,c('t01', 't02', 't03', 't04', 't05', 't06', 't07', 't08', 't09', 't10', 't11', 't12')], MARGIN=c(2), na.rm=TRUE, FUN='mean')
    E <- apply(selectClim[,c('e01', 'e02', 'e03', 'e04', 'e05', 'e06', 'e07', 'e08', 'e09', 'e10', 'e11', 'e12')], MARGIN=c(2), na.rm=TRUE, FUN='mean')
    P <- apply(selectClim[,c('p01', 'p02', 'p03', 'p04', 'p05', 'p06', 'p07', 'p08', 'p09', 'p10', 'p11', 'p12')], MARGIN=c(2), na.rm=TRUE, FUN='mean')
    MAAT <- mean(T)
    
    Elev<-mean(selectClim$Elevation, na.rm=TRUE)
    
    Lat<-mean(selectClim$Latitude, na.rm=TRUE)
    Lon<-mean(selectClim$Longitude, na.rm=TRUE)
    PET<-sum(E, na.rm=TRUE)
    MAP<-sum(P, na.rm=TRUE)
    
    
    PPETRatio<-MAP/(PET+0.0001)
    Deficit <- mean(selectClim$Deficit, na.rm=TRUE)
    Surplus <- mean(selectClim$Surplus, na.rm=TRUE)
    SummerBioT<-mean(selectClim$Tg, na.rm=TRUE)
    peakAET <- mean(selectClim$pAET, na.rm=TRUE)
    
    Tw<-mean(selectClim$Tw, na.rm=TRUE)
    Twh<-mean(selectClim$Twh, na.rm=TRUE)
    Tc<-mean(selectClim$Tc, na.rm=TRUE)
    Tcl<-mean(selectClim$Tcl, na.rm=TRUE)
    Tclx <- mean(selectClim$Tclx, na.rm=TRUE)
    
    #Key to climate type_____________________________________________________
    
    
    Seasonalilty <- ifelse(Deficit < 150 & PPETRatio>=1, "Isopluvial",
                           ifelse(Surplus < 25 & PPETRatio < 0.5 & peakAET < 75, "Isoxeric",
                                  ifelse(peakAET < 75,"Xerothermic","Pluviothermic")))
    
    
    
    
    
    
    
    MRegime <- ifelse(PPETRatio>=2,"Perhumid",
                      ifelse(PPETRatio>=1.414,"Moist-Humid",
                             ifelse(PPETRatio>=1,"Dry-Humid",
                                    ifelse(PPETRatio>=0.707,"Moist-Subhumid",
                                           ifelse(PPETRatio>=0.5,"Dry-Subhumid",
                                                  ifelse(PPETRatio>=0.25,"Semiarid",
                                                         ifelse(PPETRatio>=0.125,"Arid","Perarid"
                                                         )))))))
    
    
    BioTemperatureC <- 
      ifelse(Tc >= 20 & Tclx >=5,"Meso-Tropical",
             ifelse(Tc >= 15 & Tclx >=0,"Cryo-Tropical",
                    ifelse(Tc >= 10 & Tclx >=-5,"Thermo-Sutropical",
                           ifelse(Tc >= 5 & Tclx >=-10,"Meso-Subtropical",
                                  ifelse(Tc >= 0 & Tclx >=-15,"Cryo-Subtropical",
                                         ifelse(Tc >= -5 & Tclx >=-20,"Thermo-Temperate",
                                                ifelse(Tc >= -10 & Tclx >=-25,"Meso-Temperate",
                                                       ifelse(Tc >= -25 & Tclx >=-40,"Cryo-Temperate","Polar"
                                                       ))))))))
    
    BioTemperatureW <- ifelse(SummerBioT >= 24,"Hot (Lowland)",
                              ifelse(SummerBioT >= 18,"Warm (Premontane)",
                                     ifelse(SummerBioT >= 15,"Warm-Mild (Lower-Montane)",
                                            ifelse(SummerBioT >= 12,"Cool-Mild (Upper-Montane)",
                                                   ifelse(SummerBioT >= 6,"Cool (Subalpine)","Cold (Alpine)"
                                                   )))))
    Climatetext<-paste(BioTemperatureW," ",BioTemperatureC,", ",MRegime," ",Seasonalilty, sep="" )
    
    #assemble supplemental summary
    
    my_text1 <- paste("Lat: ",round(Lat,digits=2),"  Lon:", round(Lon,digits=2),"  Elev: ",round(Elev,digits=0)," m")
    my_text2 <- paste("Lat: ",round(Lat,digits=2),"°;  Lon: ", round(Lon,digits=2),"°;  Elev: ",round(Elev,digits=0)," m","\n",
                      "MAAT: ",round(MAAT,digits=1),"°C;  ","MAP: ", round(MAP,0)," mm  ","\n",
                     "Warm Month: ", round(Tw,1),"°C; High: ",round(Twh,1),"°C; ", "Cold Month: ", round(Tc,1),"°C; Low: ",round(Tcl,1),"°C","\n",
                     "Growing Season Temperature: ",round(SummerBioT,digits=1),"°C; Annual Extreme Low: ", round(Tclx,1),"°C","\n",
                     "P/PET: ", round(PPETRatio,2),"; Surplus: ", round(Surplus,0)," mm; Deficit: ", round(Deficit,0)," mm; Peak AET: ", round(peakAET,0), " mm","\n", Climatetext,sep="")
    
    rv$my_text2 <- my_text2
    #aggregate graph
    climplot <- ggplot(sumMonthly, aes(x=Month)) + 
      geom_bar(stat="identity",aes(fill="Precipitation", y=p/5), alpha = 0.85,  color="blue") +
      geom_bar(stat="identity", aes(fill='PET', y=e/5), alpha = 0.60,  color="red" ) +
      geom_line(stat="identity",  aes(color= "Temperature", y=t), alpha = 1) +
      geom_point(aes(shape='Mean', y=t), color="red") +
      geom_point(aes(shape='Low', y=tl), color="red") +
      geom_point(aes(shape='High', y=th), color="red") +
      geom_errorbar(aes(ymin=p25/5, ymax=p75/5), width=.2,position=position_dodge(-0.9), color="blue") +
      geom_errorbar(aes(ymin=t25, ymax=t75), width=.2,position=position_dodge(0.9), color="red") +
      
      scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12), labels=c('01','02','03','04','05','06','07','08','09','10','11','12'))+
      scale_y_continuous(name= "Temperature",
                         breaks=c(-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45), labels=c('-20 (-4)', '-15 (  5)', '-10 (14)', '-5 (23)', '0 (32)', '5 (41)', '10 (50)', '15 (59)', '20 (68)', '25 (77)', '30 (86)', '35 (95)', '40 (104)', '°C (°F)'),
                         sec.axis = sec_axis(trans = ~.*1,
                                             name = "Precipitation",
                                             breaks=c(0,5,10,15,20,25,30,35,40,45),
                                             labels = c('0', '25   (1)', '50   (2)', '75   (3)', '100 (4)', '125 (5)', '150 (6)', '175 (7)', '200 (8)', 'mm (in)')))+
      
      
      #coord_cartesian(xlim = c(1,12), ylim = c(-20, 43))+
      
      theme(legend.position="bottom") +
      scale_fill_manual("Legend", values = c("Precipitation" = "cyan", "PET" = "yellow"))+
      scale_color_manual("",values = c("Temperature" = "red", "Mean" = "red", "Low" = "red", "High"="red","Growth"="darkgreen"))+
      scale_shape_manual("",values = c("Mean" = 19, "Low" = 6, "High"=2))+
      coord_fixed(ratio = 1/9,xlim = c(1,12), ylim = c(-20, 43))+
      labs(title = paste("Climate of ",selectClim[1,]$ECO_NAME, sep=""))# ,  subtitle = my_text1)
    
    climplot
    

    

    })

    
  output$Climtext = renderText({ 
    rv$my_text2
    })

})
