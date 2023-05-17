
'TRENDS OF HUMAN TRAFFICKING IN THE WESTERN BALKAN'


# I. Import libraries ----------------------------------------

      library(readxl)
      library(plotly)
      library(data.table)
      library(DT)
      library(rpivotTable)
      library(openintro)
      library(countrycode)
      library(ggplot2)
      library(gridExtra)
      library(stargazer)
      library(dplyr)
      library(tidyverse)
      library(htmltools)
      library(ggplot2)
      library(plotly)
      library(notly)


    # Define fonts
        t <- list(
          family = "Arial",
          size = 12
        )
        
        
        t_7 <- list(
          family = "Arial",
          size = 7
        )
        
        t_8 <- list(
          family = "Arial",
          size = 10#8
        )

        # Import data
        data_tbl <- read_excel("C:/WB_MIDEX/Git/Module 6/DATA/DataAll.xlsx")
        
        data_tbl[is.na(data_tbl)] <- 0
        
        data_tbl<-data_tbl%>%arrange((Year))
        
        # Create dataframe with MARRI Participants
        MARRI_Partipants_codes<-data.frame(MarriParticipant=c("Albania","North Macedonia","Bosnia and Herzegovina","Serbia","Montenegro","Kosovo*"),
                                   Codes=c("AL","MK","BA","RS","ME","XK"))

       '* This designation is without prejudice to positions on status, and is in line with UNSCR 1244(1999) and the ICJ Opinion on the Kosovo declaration of independence.'


       
# II. Analysis of data by MARRI Participant ------------------------------
      # 1.Albania -----------------------------------------
               # 1.Log-lin models ----------------------------------------------
                          # 1.1 Presumed Victims ----------------------------------------------------------------
                           
                           df<-data_tbl%>%
                             dplyr::select(Year,Age,Codes,PresumedVictims)%>%
                             dplyr::filter(Age=="Total")%>%
                             dplyr::select(-c(Age))%>%
                             data.table()
                           
                           
                           df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                           df$MarriParticipant<-as.factor(df$MarriParticipant)
                           df$Codes<-NULL
                           
                 
                           df_f_AL<-df %>%
                             dplyr:: filter(MarriParticipant=="Albania")
                           
                           # Regression 
                           OLS_AL1<-lm(log(PresumedVictims)~(Year), data=df_f_AL)
                           
                           
                          # 1.2 Identified Victims-------------------------------------------------------------------------
                           
                           df<-data_tbl%>%
                             dplyr::select(Year,Age,Codes,IdentifiedVictims)%>%
                             dplyr::filter(Age=="Total")%>%
                             dplyr::select(-c(Age))%>%
                             data.table()
                           
                           
                           df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                           df$MarriParticipant<-as.factor(df$MarriParticipant)
                           df$Codes<-NULL
                           
         
                           
                      
                           df_f_AL<-df %>%
                             dplyr:: filter(MarriParticipant=="Albania")
                           
                           # Regression 
                           OLS_AL2<-lm(log(IdentifiedVictims)~(Year), data=df_f_AL)
                           
                           
                          # 1.3 Children ----------------------------------------------------------------
                           
                           df<-data_tbl%>%
                             dplyr::select(Year,Age,Codes,TotalTHB)%>%
                             dplyr::filter(Age=="Children(Under18)")%>%
                             dplyr::select(-c(Age))%>%
                             data.table()
                           
                           
                           df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                           df$MarriParticipant<-as.factor(df$MarriParticipant)
                           df$Codes<-NULL
                           
                           
                           
                           ## AL
                           df_f_AL<-df %>%
                             dplyr:: filter(MarriParticipant=="Albania")%>%
                             dplyr::rename("Children"="TotalTHB")
                           
                           # Regression 
                           OLS_AL3<-lm(log(Children)~(Year), data=df_f_AL)
                           
                          # 1.4 Adults ------------------------------------------------------------------
                           
                           
                           df<-data_tbl%>%
                             dplyr::select(Year,Age,Codes,TotalTHB)%>%
                             dplyr::filter(Age=="Adults(18+)")%>%
                             dplyr::select(-c(Age))%>%
                             data.table()
                           
                           
                           df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                           df$MarriParticipant<-as.factor(df$MarriParticipant)
                           df$Codes<-NULL
                           
                         
                           
                           ## AL
                           df_f_AL<-df %>%
                             dplyr:: filter(MarriParticipant=="Albania")%>%
                             dplyr::rename("Adult"="TotalTHB")
                           
                           # Regression 
                           OLS_AL4<-lm(log(Adult)~(Year), data=df_f_AL)
                           
                           
                          # 1.5 Output --------------------------------------------------------
             stargazer(OLS_AL1,OLS_AL2,OLS_AL3,OLS_AL4,
                       type = "text", title="Comparison of log-lin models", digits=4, out="ComparisionTotalTHB_AL.txt")
             
             
               
             
              # 2. Visualizations -----------------------------------------------------------------------
                     # 2.1 Facet  -----------------------------------------------------------------
                          # 2.1.1 Total number of identified and presumed victims of trafficking (VT/PVT) in the period 2018-2022 -----------------------------------------------------------------
                          
                                df<-data_tbl%>%
                                    dplyr::select(Year,Age,Codes,TotalTHB)%>%
                                    dplyr::filter(Age=="Total"&Codes=='AL')%>%
                                    dplyr::select(-c(Age))
                                
                                
                                df$Year<-as.character(df$Year)
                                
                                df<-df%>%
                                      dplyr::group_by(Year)%>%
                                      dplyr::summarize(TotalTHB=sum(TotalTHB))%>%
                                      data.table()
                                
                          
                                Chart1 <- plot_ly(df,x = ~Year, y = ~TotalTHB, type = 'scatter', mode = 'lines', fill = 'tozeroy',
                                            text = ~paste0(" ",round(TotalTHB,0)),
                                            textposition = "outside")
            
                    
                         
                          # 2.1.2 Presumed Victims-------------------------------------------------
                          
                         # color = c('#2ca02c', '#ff7f0e','#d62728', '#1f77b4','#9370DB','#8c564b'
                          
                          df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,PresumedVictims)%>%
                                dplyr::filter(Age=="Total"&Codes=='AL')%>%
                                dplyr::select(-c(Age))%>%
                                data.table()
                          
                         
                          
                          df$Year<-as.character(df$Year)
                          
                         
                          
                          Chart2 <- plot_ly(df,x = ~Year, y = ~PresumedVictims, type = 'scatter',  fill = 'tonexty',
                                            # mode = 'lines',
                                            #text = ~paste0(" ",round(PresumedVictims,0)),
                                            #textposition = "outside"
                                            mode = "text+lines", 
                                            text = ~ paste0(" ", round(PresumedVictims, 0)),
                                            textposition="outside"
                                            
                                            
                          )
                         
                         
                          # 2.1.2 Identified Victims------------------------------------
                          
                          
                          df<-data_tbl%>%
                                  dplyr::select(Year,Age,Codes,IdentifiedVictims)%>%
                                  dplyr::filter(Age=="Total"&Codes=='AL')%>%
                                  dplyr::select(-c(Age))%>%
                                  data.table()
                          
                          
                          df$Year<-as.character(df$Year)
                          
                          
                          ## NEW TEST
                          
                          Chart3 <- plot_ly(df,x = ~Year, y = ~IdentifiedVictims, type = 'scatter', fill = 'tonexty',
                                            # text = ~paste0(" ",round(IdentifiedVictims,0)),
                                            # textposition = "outside"
                                            mode = "text+lines", 
                                            text = ~ paste0(" ", round(IdentifiedVictims, 0)),
                                            textposition="outside"
                          )
                         
                    
                          # 2.1.3 Children(Under18)---------------------------------------------
                    
                    df<-data_tbl%>%
                          dplyr::select(Year,Age,Codes,TotalTHB)%>%
                          dplyr::filter(Age=="Children(Under18)"&Codes=='AL')%>%
                          dplyr::select(-c(Codes,Age))
                    
                    
                    df$Year<-as.character(df$Year)
                    
                    
                    ## NEW TEST
                    
                    Chart4a <- plot_ly(df,x = ~Year, y = ~TotalTHB, type = 'scatter', fill = 'tonexty',
                                      # text = ~paste0(" ",round(TotalTHB,0)),
                                      # textposition = "outside"
                                      mode = "text+lines", 
                                      text = ~ paste0(" ", round(TotalTHB, 0)),
                                      textposition="outside"
                                      
                                      
                                      
                                      )%>%
                      layout(
                             annotations =
                               list(x = 0, y = -0.18,
                                    title = " ",
                                    text = "Source: MARRI-RC",
                                    showarrow = F,
                                    xref='paper',
                                    yref='paper'),font = t_8)
                    
               
                    
                    
                          # 2.1.4 Adult -------------------------------------------------------------------
                    
                    df<-data_tbl%>%
                      dplyr::select(Year,Age,Codes,TotalTHB)%>%
                      dplyr::filter(Age=="Adults(18+)"&Codes=='AL')%>%
                      dplyr::select(-c(Codes,Age))
                    
                    
                    df$Year<-as.character(df$Year)
                    
                    
                 
                    
                    Chart4ab <- plot_ly(df,x = ~Year, y = ~TotalTHB, type = 'scatter', fill = 'tonexty',
                                       # text = ~paste0(" ",round(TotalTHB,0)),
                                       # textposition = "outside"
                                       
                                       mode = "text+lines",
                                       
                                       text = ~ paste0(" ", round(TotalTHB, 0)),
                                       textposition="outside"
                    )
                   
                    
                          # 2.1.5 Output -------------------------------------------------------------------
                    
                   
                    
                    
                    fig <- subplot(
                                  style(Chart2, showlegend = FALSE),
                                  style(Chart3, showlegend = FALSE),
                                  style(Chart4a, showlegend = FALSE),
                                  style(Chart4ab, showlegend = FALSE),
                                               # titleX  = FALSE,
                      # style(Chart1,showlegend = FALSE),
                      # Chart2, #style(Chart3, showlegend = FALSE),
                      nrows = 2, margin = 0.05
                    )
                    
                    annotations = list( 
                      list( 
                        x = 0.2,  
                        y = 1.0,  
                        text = "<b>Total number of presumed victims of trafficking (PVT)<b>",  
                        xref = "paper",  
                        yref = "paper",  
                        xanchor = "center",  
                        yanchor = "bottom",  
                        showarrow = FALSE 
                      ),  
                      list( 
                        x = 0.8,  
                        y = 1,  
                        text = "<b>Total number of identify victims of trafficking (VT)<b>",  
                        xref = "paper",  
                        yref = "paper",  
                        xanchor = "center",  
                        yanchor = "bottom",  
                        showarrow = FALSE 
                      ),  
                      list( 
                        x = 0.2,  
                        y = 0.45,  
                        text = "<b>Total number of children (VT/PVT)<b>",  
                        xref = "paper",  
                        yref = "paper",  
                        xanchor = "center",  
                        yanchor = "bottom",  
                        showarrow = FALSE 
                      ),
                      list( 
                        x = 0.8,  
                        y = 0.45,  
                        text = "<b>Total number of adults (VT/PVT)<b>",  
                        xref = "paper",  
                        yref = "paper",  
                        xanchor = "center",  
                        yanchor = "bottom",  
                        showarrow = FALSE 
                      ))
                    
                    
                    # t_8 <- list(
                    #   family = "Arial",
                    #   size = 16#8
                    # )
                    # 
                    
                    fig <- fig %>%layout(annotations = annotations,font = t) 
                    
                    fig
                    
                    
                 
                    
            
        
                            
                      # 2.2 Facet   -------------------------------------------------
      
      legend <- list(orientation = "v", y = 1, x = 0.9)
      
                          # 2.2.1 Female --------------------------------------------------------------
                          
                          
                          df<-data_tbl%>%
                            dplyr::select(Year,Age,Codes,Male,Female,Transgender)%>%
                            #dplyr::filter(!Age %in% c('Total'))%>%
                            dplyr::select(-c(Age))%>%
                            dplyr::group_by(Year,Codes)%>%
                            dplyr:: summarize(Male=sum(Male),
                                              Female=sum(Female),
                                              Transgender=sum(Transgender))%>%
                            dplyr::mutate(Total=Male+Female+Transgender)%>%
                            dplyr::mutate(female=(Female/Total)*100,
                                          male=(Male/Total)*100,
                                          transgender=(Transgender/Total)*100)%>%
                            dplyr::filter(Codes=='AL')%>%
                            dplyr::select(-c(Male,Female,Transgender,Total))%>%
                            data.table()
                          
                          
                          df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                          df$Year<-as.character(df$Year)
                          df<-melt(df)
                          df$MarriParticipant<-as.factor(df$MarriParticipant)
                          df$color <- factor(df$variable, labels = c("goldenrod","#2ca02c","#9870AB"))
                          
                          
                          
                          Chart1<-plot_ly(df, x = ~Year, y = ~value,
                                          color = ~variable, colors = ~as.character(color), type = "bar",
                                          legendgroup = "1"
                          )|>
                            layout(barmode = "stack",showlegend = TRUE, legend = legend, 
                                   title = '<b>Percentage of VT/PVT disaggregated by gender<b>',font = t_8,
                                   xaxis = list(title = ' '),
                                   yaxis = list(title = 'Percentage')
                            )
                          
            
                          
                          # 2.2.2 Children ----------------------------------------------------------------
                          
                          
                          df<-data_tbl%>%
                            dplyr::select(Year,Age,Codes,TotalTHB)%>%
                            dplyr::filter(!Age %in% c('Unknown'))
                          
                          df_total<-df%>%
                            dplyr::select(Age,Codes,Year,TotalTHB)%>%
                            dplyr::filter(Age=='Total'&Codes=='AL')%>%
                            dplyr::select(-c(Age))%>%
                            dplyr::rename('total'='TotalTHB')
                          
                          df_total1<-left_join(df,df_total,by = c('Codes','Year'))
                          
                          df_total2<-df_total1%>%
                            dplyr::filter(!Age %in% c('Total')&Codes=='AL')%>%
                            dplyr::mutate(share=(TotalTHB/total)*100)
                          df_total2$TotalTHB<-NULL
                          df_total2$total<-NULL
                          
                          
                          df_total2<-left_join(df_total2,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                          df_total2$Year<-as.character(df_total2$Year)
                          df_total3<-melt(df_total2)
                          df_total3$MarriParticipant<-as.factor(df_total3$MarriParticipant)
                          
                          df_total3$color <- factor(df_total3$Age, labels = c("orange","forestgreen"))
                          df_total3[is.na(df_total3)] <- 0
                          
                          df_total3$variable<-NULL
                          df_total3$codes<-NULL
                          
                          #'#bcbd22', "#66CC99",
                          
                          df_total4 <- df_total3 %>%
                            dplyr::select(-color)  %>%
                            tidyr::pivot_wider(names_from = Age, values_from = value)
                          
                          df_total4<-df_total4%>%arrange((MarriParticipant))
                          
                          
                          Chart2<-plot_ly(df_total4,
                                          x = ~ Year, y = ~`Children(Under18)`,
                                          type = "bar", marker = list(
                                            color = "#1f77b4"
                                          ), name = "Children",legendgroup = "2") %>%
                            add_bars(y = ~`Adults(18+)`, marker = list(
                              color = "#66CC99"
                            ), name = "Adults")  %>%
                            layout(showlegend = TRUE, legend = legend,
                                   barmode = "stack",
                                   title = "<b>Percentage of children identified as VT/PVT<b>",font = t_8,
                                   xaxis = list(title = ""),
                                   yaxis = list(title = "Percentage")) #,
                          
            
                          
                          # 2.2.3 Data on the cases of trafficking separated with forms of trafficking -------------------------
                          
                          df<-data_tbl%>%
                            dplyr::select(Year,Age,Codes,Type_SexualExploitation,Type_ForcedBegging,TypeForced_marriage,Type_LabourExploitation,
                                          Type_ForcedCriminality,Type_TraffickingWithBabies,Type_TraffickingWithOrgans,Type_Pornography,Type_MixedForms,Type_OtherForms)%>%
                            dplyr::filter(Age=="Total")%>%
                            dplyr::select(-c(Age))%>%
                            dplyr::group_by(Year,Codes)%>%
                            dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>%
                            data.table()%>%
                            rename('Forced marriage'='TypeForced_marriage',
                                   'Sexual Exploitation'='Type_SexualExploitation',
                                   'Forced Begging'='Type_ForcedBegging',
                                   'Labour Exploitation'='Type_LabourExploitation',
                                   'Forced Criminality'='Type_ForcedCriminality',
                                   'Trafficking with Babies'='Type_TraffickingWithBabies',
                                   'Trafficking with Organs'='Type_TraffickingWithOrgans',
                                   'Pornography'='Type_Pornography',
                                   'Mixed Forms'='Type_MixedForms',
                                   'Other Forms'='Type_OtherForms',
                            )
                          
                      
                          df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))#%>%
                          #dplyr::select(MarriParticipant,Year,IdentifiedVictims)
                          
                          df$Year<-as.character(df$Year)
                          
                          df$MarriParticipant<-as.factor(df$MarriParticipant)
                          
                          df$Codes<-NULL
                          
                          df_t<-melt(df)%>%
                            dplyr::filter(MarriParticipant=="Albania")
                          
                          df_t$color <- factor(df_t$variable, labels = c('#d62728', '#ff7f0e','#2ca02c','#bcbd22', "#66CC99",'#9370DB','#8c564b',"#CC6666","#9870AB",'#1f77b4'))
                          
                          Chart3<-plot_ly(df_t, x = ~Year,y = ~value,
                                          
                                          color = ~variable, colors = ~as.character(color), type = "bar")%>%
                            layout(showlegend = TRUE, legend = legend,
                                   barmode = "stack",
                                   title = '<b>Data on forms of human trafficking</b>',font = t_8,
                                   xaxis = list(title = ''),
                                   yaxis = list(title = ' '))
                          
            
                          
                          
                          # 2.2.4 Domestic / Foreign  of the VT/PVT for the period 2018-2022, by MARRI Participant------------------

                          df<-data_tbl%>%
                            dplyr::select(Year,Age,Codes,
                                          NationalityandCountryOfExplotation_DomesticCitizensExploitedAbroad,
                                          NationalityandCountryOfExplotation_ForeignCitizensInternallyExploited,
                                          NationalityandCountryOfExplotation_InternalTrafficking)%>%
                            dplyr::filter(Age=="Total"&Codes=='AL')%>%
                            dplyr::select(-c(Age))%>%
                            dplyr::group_by(Year,Codes)%>%
                            dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>%
                            data.table()%>%
                          
                            rename('Domestic victums exploited abroad'='NationalityandCountryOfExplotation_DomesticCitizensExploitedAbroad',
                                   'Foreign victums internally exploited'='NationalityandCountryOfExplotation_ForeignCitizensInternallyExploited',
                                   'Domestic victums exploited internally'='NationalityandCountryOfExplotation_InternalTrafficking')
                          
                          df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))#%>%
                          #dplyr::select(MarriParticipant,Year,IdentifiedVictims)
                          
                          df$Year<-as.character(df$Year)
                          
                          df$MarriParticipant<-as.factor(df$MarriParticipant)
                          
                          
                          df$Codes<-NULL
                          
                          df_t<-melt(df)
                          
                          df_t$color <- factor(df_t$variable, labels = c('#d62728',"#1f77b4", '#ff7f0e'))
                          
                          
                          Chart4<-plot_ly(df_t, x = ~Year, y = ~value,
                                          color = ~variable, colors = ~as.character(color), type = "bar")%>%
                            layout(showlegend = TRUE, legend = legend,
                                   barmode = "stack",
                                   title = '<b>Domestic and foreign victums of the VT/PVT<b>',font = t_8,
                                   xaxis = list(title = ''),
                                   yaxis = list(title = ' ')) #,
                

                          # 2.2.5 Output ------------------------------------------------------------------

                         test<- htmltools::div(
                            crosstalk::bscols(
                              Chart1, Chart2
                            ),
                            crosstalk::bscols(
                              Chart3, Chart4
                            )
                          ) |>
                            htmltools::browsable()
                          
             
     # 2. North Macedonia ---------------------------------------------
                    # 1. Log-lin models-------------------------------------
                          # 1.1 Presumed Victims ----------------------------------------------------------------
                          
                          df<-data_tbl%>%
                            dplyr::select(Year,Age,Codes,PresumedVictims)%>%
                            dplyr::filter(Age=="Total")%>%
                            dplyr::select(-c(Age))%>%
                            data.table()
                          
                          
                          df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                          df$MarriParticipant<-as.factor(df$MarriParticipant)
                          df$Codes<-NULL
                          
                     
                          
                          ## MK
                          df_f_MK<-df %>%
                            dplyr:: filter(MarriParticipant=="North Macedonia")
                          
                          # Regression 
                          OLS_MK1<-lm(log(PresumedVictims)~(Year), data=df_f_MK)
                          
                          
                          # 1.2 Identified Victims-------------------------------------------------------------------------
                          
                          df<-data_tbl%>%
                            dplyr::select(Year,Age,Codes,IdentifiedVictims)%>%
                            dplyr::filter(Age=="Total")%>%
                            dplyr::select(-c(Age))%>%
                            data.table()
                          
                          
                          df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                          df$MarriParticipant<-as.factor(df$MarriParticipant)
                          df$Codes<-NULL
                          
                   
                          
                          ## MK
                          df_f_MK<-df %>%
                            dplyr:: filter(MarriParticipant=="North Macedonia")
                          
                          # Regression 
                          OLS_MK2<-lm(log(IdentifiedVictims)~(Year), data=df_f_MK)
                          
                          
                          
                          
                          # 1.3 Children ----------------------------------------------------------------
                          
                          df<-data_tbl%>%
                            dplyr::select(Year,Age,Codes,TotalTHB)%>%
                            dplyr::filter(Age=="Children(Under18)")%>%
                            dplyr::select(-c(Age))%>%
                            data.table()
                          
                          
                          df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                          df$MarriParticipant<-as.factor(df$MarriParticipant)
                          df$Codes<-NULL
                          
                  
                          
                          ## MK
                          df_f_MK<-df %>%
                            dplyr:: filter(MarriParticipant=="North Macedonia")%>%
                            dplyr::rename("Children"="TotalTHB")
                          
                          # Regression 
                          OLS_MK3<-lm(log(Children)~(Year), data=df_f_MK)
                          
                          # 1.4 Adults ------------------------------------------------------------------
                          
                          
                          df<-data_tbl%>%
                            dplyr::select(Year,Age,Codes,TotalTHB)%>%
                            dplyr::filter(Age=="Adults(18+)")%>%
                            dplyr::select(-c(Age))%>%
                            data.table()
                          
                          
                          df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                          df$MarriParticipant<-as.factor(df$MarriParticipant)
                          df$Codes<-NULL
                          
                         
                          
                          ## MK
                          df_f_AL<-df %>%
                            dplyr:: filter(MarriParticipant=="North Macedonia")%>%
                            dplyr::rename("Adult"="TotalTHB")
                          
                          # Regression 
                          OLS_MK4<-lm(log(Adult)~(Year), data=df_f_AL)
                          
                          
                          
                          
                          
                          
                          # 1.5 Output ------------------------------------------------------------------
                          
                          stargazer(OLS_MK1,OLS_MK2,OLS_MK3,OLS_MK4,
                                    type = "text", title="Comparison of log-lin models", digits=4, out="ComparisionTotalTHB_MK.txt")
                          
                          
                          
                    # 2. Visualizations--------------------------------------------
                      #  2.1 Facet  -----------------------------------------------------------------------
                          # 2.1.1 Total number of identified and presumed victims of trafficking (VT/PVT) in the period 2018-2022 -----------------------------------------------------------------
              
              df<-data_tbl%>%
                dplyr::select(Year,Age,Codes,TotalTHB)%>%
                dplyr::filter(Age=="Total"&Codes=='MK')%>%
                dplyr::select(-c(Age))
              
              
              df$Year<-as.character(df$Year)
              
              df<-df%>%
                dplyr::group_by(Year)%>%
                dplyr::summarize(TotalTHB=sum(TotalTHB))%>%
                data.table()
              
              
              Chart1 <- plot_ly(df,x = ~Year, y = ~TotalTHB, type = 'scatter', mode = 'lines', fill = 'tozeroy',
                                text = ~paste0(" ",round(TotalTHB,0)),
                                textposition = "outside"
              )
             
              
                          # 2.1.2 Presumed Victims -------------------------------------------------

              df<-data_tbl%>%
                dplyr::select(Year,Age,Codes,PresumedVictims)%>%
                dplyr::filter(Age=="Total"&Codes=='MK')%>%
                dplyr::select(-c(Age))%>%
                data.table()

              
              df$Year<-as.character(df$Year)

              
              Chart2 <- plot_ly(df,x = ~Year, y = ~PresumedVictims, type = 'scatter',  fill = 'tonexty',
                                # mode = 'lines',
                                #text = ~paste0(" ",round(PresumedVictims,0)),
                                #textposition = "outside"
                                mode = "text+lines", 
                                text = ~ paste0(" ", round(PresumedVictims, 0)),
                                textposition="outside"
                                
                                
              )
            
              
                          # 2.1.3 Identified Victims------------------------------------
              
              
              df<-data_tbl%>%
                dplyr::select(Year,Age,Codes,IdentifiedVictims)%>%
                dplyr::filter(Age=="Total"&Codes=='MK')%>%
                dplyr::select(-c(Age))%>%
                data.table()
              
              
              df$Year<-as.character(df$Year)
              
              
              ## NEW TEST
              
              Chart3 <- plot_ly(df,x = ~Year, y = ~IdentifiedVictims, type = 'scatter', fill = 'tonexty',
                                # text = ~paste0(" ",round(IdentifiedVictims,0)),
                                # textposition = "outside"
                                mode = "text+lines", 
                                text = ~ paste0(" ", round(IdentifiedVictims, 0)),
                                textposition="outside"
              )
             
                          # 2.1.4 Children(Under18)---------------------------------------------
              
              df<-data_tbl%>%
                dplyr::select(Year,Age,Codes,TotalTHB)%>%
                dplyr::filter(Age=="Children(Under18)"&Codes=='MK')%>%
                dplyr::select(-c(Codes,Age))
              
              
              df$Year<-as.character(df$Year)
              
              
              ## NEW TEST
              
              Chart4a <- plot_ly(df,x = ~Year, y = ~TotalTHB, type = 'scatter', fill = 'tonexty',
                                 # text = ~paste0(" ",round(TotalTHB,0)),
                                 # textposition = "outside"
                                 mode = "text+lines", 
                                 text = ~ paste0(" ", round(TotalTHB, 0)),
                                 textposition="outside"
                                 
                                 
                                 
              )%>%
                layout(
                  annotations =
                    list(x = 0, y = -0.18,
                         title = " ",
                         text = "Source: MARRI-RC",
                         showarrow = F,
                         xref='paper',
                         yref='paper'),font = t_8)

              
                          # 2.1.5 Adult -------------------------------------------------------------------
              
              df<-data_tbl%>%
                dplyr::select(Year,Age,Codes,TotalTHB)%>%
                dplyr::filter(Age=="Adults(18+)"&Codes=='MK')%>%
                dplyr::select(-c(Codes,Age))
              
              
              df$Year<-as.character(df$Year)
              
              
              ## NEW TEST
              
              Chart4ab <- plot_ly(df,x = ~Year, y = ~TotalTHB, type = 'scatter', fill = 'tonexty',
                                  # text = ~paste0(" ",round(TotalTHB,0)),
                                  # textposition = "outside"
                                  
                                  mode = "text+lines", 
                                  text = ~ paste0(" ", round(TotalTHB, 0)),
                                  textposition="outside"
              )
       
              
              
                          # 2.1.6 Output -------------------------------------------------------------------
              
                          fig <- subplot(
                            style(Chart2, showlegend = FALSE),
                            style(Chart3, showlegend = FALSE),
                            style(Chart4a, showlegend = FALSE),
                            style(Chart4ab, showlegend = FALSE),
                            nrows = 2, margin = 0.05
                          )
                          
                          annotations = list( 
                            list( 
                              x = 0.2,  
                              y = 1.0,  
                              text = "<b>Total number of presumed victims of trafficking (PVT)<b>",  
                              xref = "paper",  
                              yref = "paper",  
                              xanchor = "center",  
                              yanchor = "bottom",  
                              showarrow = FALSE 
                            ),  
                            list( 
                              x = 0.8,  
                              y = 1,  
                              text = "<b>Total number of identify victims of trafficking (VT)<b>",  
                              xref = "paper",  
                              yref = "paper",  
                              xanchor = "center",  
                              yanchor = "bottom",  
                              showarrow = FALSE 
                            ),  
                            list( 
                              x = 0.2,  
                              y = 0.45,  
                              text = "<b>Total number of children (VT/PVT)<b>",  
                              xref = "paper",  
                              yref = "paper",  
                              xanchor = "center",  
                              yanchor = "bottom",  
                              showarrow = FALSE 
                            ),
                            list( 
                              x = 0.8,  
                              y = 0.45,  
                              text = "<b>Total number of adults (VT/PVT)<b>",  
                              xref = "paper",  
                              yref = "paper",  
                              xanchor = "center",  
                              yanchor = "bottom",  
                              showarrow = FALSE 
                            ))
                          
                          
                          # t_8 <- list(
                          #   family = "Arial",
                          #   size = 14#8
                          # )
                          
                          
                          fig <- fig %>%layout(annotations = annotations,font = t) 
                          
                          fig
                          
                  

                        # 2.2 Facet  -------------------------------------------------
      

      legend <- list(orientation = "v", y = 1, x = 1)
      
                          # 2.1 Female --------------------------------------------------------------
                            
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,Male,Female,Transgender)%>%
                              #dplyr::filter(!Age %in% c('Total'))%>%
                              dplyr::select(-c(Age))%>%
                              dplyr::group_by(Year,Codes)%>%
                              dplyr:: summarize(Male=sum(Male),
                                                Female=sum(Female),
                                                Transgender=sum(Transgender))%>%
                              dplyr::mutate(Total=Male+Female+Transgender)%>%
                              dplyr::mutate(female=(Female/Total)*100,
                                            male=(Male/Total)*100,
                                            transgender=(Transgender/Total)*100)%>%
                              dplyr::filter(Codes=='MK')%>%
                              dplyr::select(-c(Male,Female,Transgender,Total))%>%
                              data.table()
                            
                            
                            df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                            df$Year<-as.character(df$Year)
                            df<-melt(df)
                            df$MarriParticipant<-as.factor(df$MarriParticipant)
                            df$color <- factor(df$variable, labels = c("goldenrod","#2ca02c","#9870AB"))
                            
                            
                            
                            Chart1<-plot_ly(df, x = ~Year, y = ~value,
                                            color = ~variable, colors = ~as.character(color), type = "bar",
                                            legendgroup = "1"
                            )|>
                              layout(barmode = "stack",showlegend = TRUE, legend = legend, 
                                     title = '<b>Percentage of VT/PVT disaggregated by gender<b>',font = 10,
                                     xaxis = list(title = ' '),
                                     yaxis = list(title = 'Percentage')
                              )
                            
                            
                        
                            
                          # 2.2 Children ----------------------------------------------------------------
                            
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,TotalTHB)%>%
                              dplyr::filter(!Age %in% c('Unknown'))
                            
                            df_total<-df%>%
                              dplyr::select(Age,Codes,Year,TotalTHB)%>%
                              dplyr::filter(Age=='Total'&Codes=='MK')%>%
                              dplyr::select(-c(Age))%>%
                              dplyr::rename('total'='TotalTHB')
                            
                            df_total1<-left_join(df,df_total,by = c('Codes','Year'))
                            
                            df_total2<-df_total1%>%
                              dplyr::filter(!Age %in% c('Total')&Codes=='MK')%>%
                              dplyr::mutate(share=(TotalTHB/total)*100)
                            df_total2$TotalTHB<-NULL
                            df_total2$total<-NULL
                            
                            
                            df_total2<-left_join(df_total2,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                            df_total2$Year<-as.character(df_total2$Year)
                            df_total3<-melt(df_total2)
                            df_total3$MarriParticipant<-as.factor(df_total3$MarriParticipant)
                            
                            df_total3$color <- factor(df_total3$Age, labels = c("orange","forestgreen"))
                            df_total3[is.na(df_total3)] <- 0
                            
                            df_total3$variable<-NULL
                            df_total3$codes<-NULL
                            
                            #'#bcbd22', "#66CC99",
                            
                            df_total4 <- df_total3 %>%
                              dplyr::select(-color)  %>%
                              tidyr::pivot_wider(names_from = Age, values_from = value)
                            
                            df_total4<-df_total4%>%arrange((MarriParticipant))
                            
                            
                            Chart2<-plot_ly(df_total4,
                                            x = ~ Year, y = ~`Children(Under18)`,
                                            type = "bar", marker = list(
                                              color = "#1f77b4"
                                            ), name = "Children",legendgroup = "2") %>%
                              add_bars(y = ~`Adults(18+)`, marker = list(
                                color = "#66CC99"
                              ), name = "Adults")  %>%
                              layout(showlegend = TRUE, legend = legend,
                                     barmode = "stack",
                                     title = "<b>Percentage of children identified as VT/PVT<b>",font = 10 ,
                                     xaxis = list(title = ""),
                                     yaxis = list(title = "Percentage")) #,
                            
                     
                            
                          # 2.3 Data on the cases of trafficking separated with forms of trafficking -------------------------
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,Type_SexualExploitation,Type_ForcedBegging,TypeForced_marriage,Type_LabourExploitation,
                                            Type_ForcedCriminality,Type_TraffickingWithBabies,Type_TraffickingWithOrgans,Type_Pornography,Type_MixedForms,Type_OtherForms)%>%
                              dplyr::filter(Age=="Total")%>%
                              dplyr::select(-c(Age))%>%
                              dplyr::group_by(Year,Codes)%>%
                              dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>%
                              data.table()%>%
                              rename('Forced marriage'='TypeForced_marriage',
                                     'Sexual Exploitation'='Type_SexualExploitation',
                                     'Forced Begging'='Type_ForcedBegging',
                                     'Labour Exploitation'='Type_LabourExploitation',
                                     'Forced Criminality'='Type_ForcedCriminality',
                                     'Trafficking with Babies'='Type_TraffickingWithBabies',
                                     'Trafficking with Organs'='Type_TraffickingWithOrgans',
                                     'Pornography'='Type_Pornography',
                                     'Mixed Forms'='Type_MixedForms',
                                     'Other Forms'='Type_OtherForms',
                              )
                            
                            
                            df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))#%>%
                            #dplyr::select(MarriParticipant,Year,IdentifiedVictims)
                            
                            df$Year<-as.character(df$Year)
                            
                            df$MarriParticipant<-as.factor(df$MarriParticipant)
                            
                            df$Codes<-NULL
                            
                            df_t<-melt(df)%>%
                              dplyr::filter(MarriParticipant=="Albania")
                            
                            df_t$color <- factor(df_t$variable, labels = c('#d62728', '#ff7f0e','#2ca02c','#bcbd22', "#66CC99",'#9370DB','#8c564b',"#CC6666","#9870AB",'#1f77b4'))
                            
                            Chart3<-plot_ly(df_t, x = ~Year,y = ~value,
                                            
                                            color = ~variable, colors = ~as.character(color), type = "bar")%>%
                              layout(showlegend = TRUE, legend = legend,
                                     barmode = "stack",
                                     title = '<b>Data on forms of human trafficking</b>',font = 10,
                                     xaxis = list(title = ''),
                                     yaxis = list(title = ' '))
                            
                            
                          # 2.4 Domestic and foreign victims of the VT/PVT for the period 2018-2022, by MARRI Participant ------------------
                            
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,
                                            NationalityandCountryOfExplotation_DomesticCitizensExploitedAbroad,
                                            NationalityandCountryOfExplotation_ForeignCitizensInternallyExploited,
                                            NationalityandCountryOfExplotation_InternalTrafficking)%>%
                              dplyr::filter(Age=="Total"&Codes=='MK')%>%
                              dplyr::select(-c(Age))%>%
                              dplyr::group_by(Year,Codes)%>%
                              dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>%
                              data.table()%>%
                              rename('Domestic victums exploited abroad'='NationalityandCountryOfExplotation_DomesticCitizensExploitedAbroad',
                                     'Foreign victums internally exploited'='NationalityandCountryOfExplotation_ForeignCitizensInternallyExploited',
                                     'Domestic victums exploited internally'='NationalityandCountryOfExplotation_InternalTrafficking')
                            
                            
                            df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))#%>%
                            #dplyr::select(MarriParticipant,Year,IdentifiedVictims)
                            
                            df$Year<-as.character(df$Year)
                            
                            df$MarriParticipant<-as.factor(df$MarriParticipant)
                            
                            
                            df$Codes<-NULL
                            
                            df_t<-melt(df)
                            
                            df_t$color <- factor(df_t$variable, labels = c('#d62728',"#1f77b4", '#ff7f0e'))
                            
                            
                            Chart4<-plot_ly(df_t, x = ~Year, y = ~value,
                                            color = ~variable, colors = ~as.character(color), type = "bar")%>%
                              layout(showlegend = TRUE, legend = legend,
                                     barmode = "stack",
                                     title = '<b>Domestic and foreign victums of the VT/PVT<b>',font = 10 ,
                                     xaxis = list(title = ''),
                                     yaxis = list(title = ' ')) #,
                            

                          # 2.5 Output ------------------------------------------------

                            
                            
                            # Facet 
                            
                            htmltools::div(
                              crosstalk::bscols(
                                Chart1, Chart2
                              ),
                              crosstalk::bscols(
                                Chart3, Chart4
                              )
                            ) |>
                              htmltools::browsable()
                            
                            
                           
                            test <- subplot(Chart1, Chart2, Chart3, Chart4, nrows = 2, margin = 0.05)
              
                       
                            

# 3.Bosnia and Herzegovina ------------------------------------------------
                        # 1. Log-lin models--------------------------------------------------------------
                            # 1,1 Presumed Victims  ----------------------------------------------------------------
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,PresumedVictims)%>%
                                dplyr::filter(Age=="Total")%>%
                                dplyr::select(-c(Age))%>%
                                data.table()
                              
                              
                              df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                              df$MarriParticipant<-as.factor(df$MarriParticipant)
                              df$Codes<-NULL
                              
                              
                           
                              df_f_BA<-df %>%
                                dplyr:: filter(MarriParticipant=="Bosnia and Herzegovina")
                              
                              # Regression 
                              OLS_BA1<-lm(log(PresumedVictims)~(Year), data=df_f_BA)
                              
                              
                              
                            # 1.2 Identified Victims -------------------------------------------------------------------------
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,IdentifiedVictims)%>%
                                dplyr::filter(Age=="Total")%>%
                                dplyr::select(-c(Age))%>%
                                data.table()
                              
                              
                              df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                              df$MarriParticipant<-as.factor(df$MarriParticipant)
                              df$Codes<-NULL
                              
                          
                              
                              ## BA
                              df_f_BA<-df %>%
                                dplyr:: filter(MarriParticipant=="Bosnia and Herzegovina")
                              
                              # Regression 
                              OLS_BA2<-lm(log(IdentifiedVictims)~(Year), data=df_f_BA)
                              
                              
                              
                              
                            # 1.3 Children ----------------------------------------------------------------
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,TotalTHB)%>%
                                dplyr::filter(Age=="Children(Under18)")%>%
                                dplyr::select(-c(Age))%>%
                                data.table()
                              
                              
                              df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                              df$MarriParticipant<-as.factor(df$MarriParticipant)
                              df$Codes<-NULL
                              
                         
                              
                              ## BA
                              df_f_BA<-df %>%
                                dplyr:: filter(MarriParticipant=="Bosnia and Herzegovina")%>%
                                dplyr::rename("Children"="TotalTHB")
                              
                              # Regression 
                              OLS_BA3<-lm(log(Children)~(Year), data=df_f_BA)
                              
                            # 1.4 Adults ------------------------------------------------------------------
                              
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,TotalTHB)%>%
                                dplyr::filter(Age=="Adults(18+)")%>%
                                dplyr::select(-c(Age))%>%
                                data.table()
                              
                              
                              df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                              df$MarriParticipant<-as.factor(df$MarriParticipant)
                              df$Codes<-NULL
                              
                       
                              
                              ## BA
                              df_f_BA<-df %>%
                                dplyr:: filter(MarriParticipant=="Bosnia and Herzegovina")%>%
                                dplyr::rename("Adult"="TotalTHB")
                              
                              
                              # Regression 
                              OLS_BA4<-lm(log(Adult)~(Year), data=df_f_BA)
                              
                              
                            # 1.5 Output ------------------------------------------------------------------
                              
                              stargazer(OLS_BA1,OLS_BA3,OLS_BA4,
                                        type = "text", title="Comparison of log-lin models", digits=4, out="ComparisionTotalTHB_BA.txt")              
                              
                     # 2. Visualizations--------------------------------------------
                          # 2.1 Facet  -----------------------------------------------------------------------
                            # 2.1.1Total number of identified and presumed victims of trafficking (VT/PVT) in the period 2018-2022 -----------------------------------------------------------------
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,TotalTHB)%>%
                              dplyr::filter(Age=="Total"&Codes=='BA')%>%
                              dplyr::select(-c(Age))
                            
                            
                            df$Year<-as.character(df$Year)
                            
                            df<-df%>%
                              dplyr::group_by(Year)%>%
                              dplyr::summarize(TotalTHB=sum(TotalTHB))%>%
                              data.table()
                            
                            
                            Chart1 <- plot_ly(df,x = ~Year, y = ~TotalTHB, type = 'scatter', mode = 'lines', fill = 'tozeroy',
                                              text = ~paste0(" ",round(TotalTHB,0)),
                                              textposition = "outside"
                            )
                            
                            
                            # 2.1.2 Presumed Victims-------------------------------------------------
                            
                            # color = c('#2ca02c', '#ff7f0e','#d62728', '#1f77b4','#9370DB','#8c564b'
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,PresumedVictims)%>%
                              dplyr::filter(Age=="Total"&Codes=='BA')%>%
                              dplyr::select(-c(Age))%>%
                              data.table()
                            
                            
                            df$Year<-as.character(df$Year)
                            
                            
                            
                            Chart2 <- plot_ly(df,x = ~Year, y = ~PresumedVictims, type = 'scatter',  fill = 'tonexty',
                                              # mode = 'lines',
                                              #text = ~paste0(" ",round(PresumedVictims,0)),
                                              #textposition = "outside"
                                              mode = "text+lines", 
                                              text = ~ paste0(" ", round(PresumedVictims, 0)),
                                              textposition="outside"
                                              
                                              
                            )
              
                            # 2.1.2 Identified Victims------------------------------------
                            
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,IdentifiedVictims)%>%
                              dplyr::filter(Age=="Total"&Codes=='BA')%>%
                              dplyr::select(-c(Age))%>%
                              data.table()
                            
                            
                            df$Year<-as.character(df$Year)
                            
                            
                            ## NEW TEST
                            
                            Chart3 <- plot_ly(df,x = ~Year, y = ~IdentifiedVictims, type = 'scatter', fill = 'tonexty',
                                              # text = ~paste0(" ",round(IdentifiedVictims,0)),
                                              # textposition = "outside"
                                              mode = "text+lines", 
                                              text = ~ paste0(" ", round(IdentifiedVictims, 0)),
                                              textposition="outside"
                            )
              
                            
                            
                            # 2.1.3 Children(Under18)---------------------------------------------
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,TotalTHB)%>%
                              dplyr::filter(Age=="Children(Under18)"&Codes=='BA')%>%
                              dplyr::select(-c(Codes,Age))
                            
                            
                            df$Year<-as.character(df$Year)
                            
                            
                            ## NEW TEST
                            
                            Chart4a <- plot_ly(df,x = ~Year, y = ~TotalTHB, type = 'scatter', fill = 'tonexty',
                                               # text = ~paste0(" ",round(TotalTHB,0)),
                                               # textposition = "outside"
                                               mode = "text+lines", 
                                               text = ~ paste0(" ", round(TotalTHB, 0)),
                                               textposition="outside"
                                               
                                               
                                               
                            )%>%
                              layout(
                                annotations =
                                  list(x = 0, y = -0.18,
                                       title = " ",
                                       text = "Source: MARRI-RC",
                                       showarrow = F,
                                       xref='paper',
                                       yref='paper'),font = t_8)
              
                            
                            # 2.1.4 Adult -------------------------------------------------------------------
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,TotalTHB)%>%
                              dplyr::filter(Age=="Adults(18+)"&Codes=='BA')%>%
                              dplyr::select(-c(Codes,Age))
                            
                            
                            df$Year<-as.character(df$Year)
                            
                            
                            ## NEW TEST
                            
                            Chart4ab <- plot_ly(df,x = ~Year, y = ~TotalTHB, type = 'scatter', fill = 'tonexty',
                                                # text = ~paste0(" ",round(TotalTHB,0)),
                                                # textposition = "outside"
                                                
                                                mode = "text+lines", 
                                                text = ~ paste0(" ", round(TotalTHB, 0)),
                                                textposition="outside"
                            )
                            #Chart4ab
                            
                            
                            # 2.1.5 Facet -------------------------------------------------------------------
                            
                            
                            fig <- subplot(
                              style(Chart2, showlegend = FALSE),
                              style(Chart3, showlegend = FALSE),
                              style(Chart4a, showlegend = FALSE),
                              style(Chart4ab, showlegend = FALSE),
                              # titleX  = FALSE,
                              # style(Chart1,showlegend = FALSE),
                              # Chart2, #style(Chart3, showlegend = FALSE),
                              nrows = 2, margin = 0.05
                            )
                            
                            annotations = list( 
                              list( 
                                x = 0.2,  
                                y = 1.0,  
                                text = "<b>Total number of presumed victims of trafficking (PVT)<b>",  
                                xref = "paper",  
                                yref = "paper",  
                                xanchor = "center",  
                                yanchor = "bottom",  
                                showarrow = FALSE 
                              ),  
                              list(
                                x = 0.8,
                                y = 1,
                                text = "<b>Total number of identify victims of trafficking (VT)<b>",
                                xref = "paper",
                                yref = "paper",
                                xanchor = "center",
                                yanchor = "bottom",
                                showarrow = FALSE
                              ),
                              list( 
                                x = 0.2,  
                                y = 0.45,  
                                text = "<b>Total number of children (VT/PVT)<b>",  
                                xref = "paper",  
                                yref = "paper",  
                                xanchor = "center",  
                                yanchor = "bottom",  
                                showarrow = FALSE 
                              ),
                              list( 
                                x = 0.8,  
                                y = 0.45,  
                                text = "<b>Total number of adults (VT/PVT)<b>",  
                                xref = "paper",  
                                yref = "paper",  
                                xanchor = "center",  
                                yanchor = "bottom",  
                                showarrow = FALSE 
                              ))
                            
                            
                            # t_8 <- list(
                            #   family = "Arial",
                            #   size = 14#8
                            # )
                            # 
                            
                            fig <- fig %>%layout(annotations = annotations,font = t) 
                            
                            fig
                            
                    
                          #  2.2. Facet -------------------------------------------------

              legend <- list(orientation = "v", y = 1, x = 1)
              
                            # 2.1 Female --------------------------------------------------------------
                            
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,Male,Female,Transgender)%>%
                              #dplyr::filter(!Age %in% c('Total'))%>%
                              dplyr::select(-c(Age))%>%
                              dplyr::group_by(Year,Codes)%>%
                              dplyr:: summarize(Male=sum(Male),
                                                Female=sum(Female),
                                                Transgender=sum(Transgender))%>%
                              dplyr::mutate(Total=Male+Female+Transgender)%>%
                              dplyr::mutate(female=(Female/Total)*100,
                                            male=(Male/Total)*100,
                                            transgender=(Transgender/Total)*100)%>%
                              dplyr::filter(Codes=='BA')%>%
                              dplyr::select(-c(Male,Female,Transgender,Total))%>%
                              data.table()
                            
                            
                            df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                            df$Year<-as.character(df$Year)
                            df<-melt(df)
                            df$MarriParticipant<-as.factor(df$MarriParticipant)
                            df$color <- factor(df$variable, labels = c("goldenrod","#2ca02c","#9870AB"))
                            
                            
                            
                            Chart1<-plot_ly(df, x = ~Year, y = ~value,
                                            color = ~variable, colors = ~as.character(color), type = "bar",
                                            legendgroup = "1"
                            )|>
                              layout(barmode = "stack",showlegend = TRUE, legend = legend, 
                                     title = '<b>Percentage of VT/PVT disaggregated by gender<b>',font = t_8,
                                     xaxis = list(title = ' '),
                                     yaxis = list(title = 'Percentage')
                              )
              
                            # 2.2 Children ----------------------------------------------------------------
                            
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,TotalTHB)%>%
                              dplyr::filter(!Age %in% c('Unknown'))
                            
                            df_total<-df%>%
                              dplyr::select(Age,Codes,Year,TotalTHB)%>%
                              dplyr::filter(Age=='Total'&Codes=='BA')%>%
                              dplyr::select(-c(Age))%>%
                              dplyr::rename('total'='TotalTHB')
                            
                            df_total1<-left_join(df,df_total,by = c('Codes','Year'))
                            
                            df_total2<-df_total1%>%
                              dplyr::filter(!Age %in% c('Total')&Codes=='BA')%>%
                              dplyr::mutate(share=(TotalTHB/total)*100)
                            df_total2$TotalTHB<-NULL
                            df_total2$total<-NULL
                            
                            
                            df_total2<-left_join(df_total2,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                            df_total2$Year<-as.character(df_total2$Year)
                            df_total3<-melt(df_total2)
                            df_total3$MarriParticipant<-as.factor(df_total3$MarriParticipant)
                            
                            df_total3$color <- factor(df_total3$Age, labels = c("orange","forestgreen"))
                            df_total3[is.na(df_total3)] <- 0
                            
                            df_total3$variable<-NULL
                            df_total3$codes<-NULL
                            
                            #'#bcbd22', "#66CC99",
                            
                            df_total4 <- df_total3 %>%
                              dplyr::select(-color)  %>%
                              tidyr::pivot_wider(names_from = Age, values_from = value)
                            
                            df_total4<-df_total4%>%arrange((MarriParticipant))
                            
                            
                            Chart2<-plot_ly(df_total4,
                                            x = ~ Year, y = ~`Children(Under18)`,
                                            type = "bar", marker = list(
                                              color = "#1f77b4"
                                            ), name = "Children",legendgroup = "2") %>%
                              add_bars(y = ~`Adults(18+)`, marker = list(
                                color = "#66CC99"
                              ), name = "Adults")  %>%
                              layout(showlegend = TRUE, legend = legend,
                                     barmode = "stack",
                                     title = "<b>Percentage of children identified as VT/PVT<b>",font = t_8,
                                     xaxis = list(title = ""),
                                     yaxis = list(title = "Percentage")) #,
                            
                            
                            # 2.3 Data on the cases of trafficking separated with forms of trafficking -------------------------
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,Type_SexualExploitation,Type_ForcedBegging,TypeForced_marriage,Type_LabourExploitation,
                                            Type_ForcedCriminality,Type_TraffickingWithBabies,Type_TraffickingWithOrgans,Type_Pornography,Type_MixedForms,Type_OtherForms)%>%
                              dplyr::filter(Age=="Total")%>%
                              dplyr::select(-c(Age))%>%
                              dplyr::group_by(Year,Codes)%>%
                              dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>%
                              data.table()%>%
                              rename('Forced marriage'='TypeForced_marriage',
                                     'Sexual Exploitation'='Type_SexualExploitation',
                                     'Forced Begging'='Type_ForcedBegging',
                                     'Labour Exploitation'='Type_LabourExploitation',
                                     'Forced Criminality'='Type_ForcedCriminality',
                                     'Trafficking with Babies'='Type_TraffickingWithBabies',
                                     'Trafficking with Organs'='Type_TraffickingWithOrgans',
                                     'Pornography'='Type_Pornography',
                                     'Mixed Forms'='Type_MixedForms',
                                     'Other Forms'='Type_OtherForms',
                              )
                            
                            
                            df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))#%>%
                            #dplyr::select(MarriParticipant,Year,IdentifiedVictims)
                            
                            df$Year<-as.character(df$Year)
                            
                            df$MarriParticipant<-as.factor(df$MarriParticipant)
                            
                            df$Codes<-NULL
                            
                            df_t<-melt(df)%>%
                              dplyr::filter(MarriParticipant=="Albania")
                            
                            df_t$color <- factor(df_t$variable, labels = c('#d62728', '#ff7f0e','#2ca02c','#bcbd22', "#66CC99",'#9370DB','#8c564b',"#CC6666","#9870AB",'#1f77b4'))
                            
                            Chart3<-plot_ly(df_t, x = ~Year,y = ~value,
                                            
                                            color = ~variable, colors = ~as.character(color), type = "bar")%>%
                              layout(showlegend = TRUE, legend = legend,
                                     barmode = "stack",
                                     title = '<b>Data on forms of human trafficking</b>',font = t_8,
                                     xaxis = list(title = ''),
                                     yaxis = list(title = ' '))
              
                            
                            
                            # 2.4 Domestic and foreign victims of the VT/PVT for the period 2018-2022, by MARRI Participant------------------
                            
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,
                                            NationalityandCountryOfExplotation_DomesticCitizensExploitedAbroad,
                                            NationalityandCountryOfExplotation_ForeignCitizensInternallyExploited,
                                            NationalityandCountryOfExplotation_InternalTrafficking)%>%
                              dplyr::filter(Age=="Total"&Codes=='BA')%>%
                              dplyr::select(-c(Age))%>%
                              dplyr::group_by(Year,Codes)%>%
                              dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>%
                              data.table()%>%
                              rename('Domestic victums exploited abroad'='NationalityandCountryOfExplotation_DomesticCitizensExploitedAbroad',
                                     'Foreign victums internally exploited'='NationalityandCountryOfExplotation_ForeignCitizensInternallyExploited',
                                     'Domestic victums exploited internally'='NationalityandCountryOfExplotation_InternalTrafficking')
                            
                            
                            df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))#%>%
                            #dplyr::select(MarriParticipant,Year,IdentifiedVictims)
                            
                            df$Year<-as.character(df$Year)
                            
                            df$MarriParticipant<-as.factor(df$MarriParticipant)
                            
                            
                            df$Codes<-NULL
                            
                            df_t<-melt(df)
                            
                            df_t$color <- factor(df_t$variable, labels = c('#d62728',"#1f77b4", '#ff7f0e'))
                            
                            
                            Chart4<-plot_ly(df_t, x = ~Year, y = ~value,
                                            color = ~variable, colors = ~as.character(color), type = "bar")%>%
                              layout(showlegend = TRUE, legend = legend,
                                     barmode = "stack",
                                     title = '<b>Domestic and foreign victums of the VT/PVT<b>',font = t_8,
                                     xaxis = list(title = ''),
                                     yaxis = list(title = ' ')) #,
                     
                            # Facet 
                            
                            htmltools::div(
                              crosstalk::bscols(
                                Chart1, Chart2
                              ),
                              crosstalk::bscols(
                                Chart3, Chart4
                              )
                            ) |>
                              htmltools::browsable()
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            
              
                            
                            
              
      # 4. Serbia --------------------------------------------------------------
                    # 1.Log-lin models --------------------------------------------------------
                            # 1.1 Presumed Victims (Regression) ----------------------------------------------------------------
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,PresumedVictims)%>%
                              dplyr::filter(Age=="Total")%>%
                              dplyr::select(-c(Age))%>%
                              data.table()
                            
                            
                            df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                            df$MarriParticipant<-as.factor(df$MarriParticipant)
                            df$Codes<-NULL
                            
                            
                            
                            ## RS
                            df_f_RS<-df %>%
                              dplyr:: filter(MarriParticipant=="Serbia")
                            
                            # Regression 
                            OLS_RS1<-lm(log(PresumedVictims)~(Year), data=df_f_RS)
                            
                            
                            # 1.2 Identified Victims-------------------------------------------------------------------------
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,IdentifiedVictims)%>%
                              dplyr::filter(Age=="Total")%>%
                              dplyr::select(-c(Age))%>%
                              data.table()
                            
                            
                            df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                            df$MarriParticipant<-as.factor(df$MarriParticipant)
                            df$Codes<-NULL
                            
                            
                            
                            ## RS
                            df_f_RS<-df %>%
                              dplyr:: filter(MarriParticipant=="Serbia")
                            
                            # Regression 
                            OLS_RS2<-lm(log(IdentifiedVictims)~(Year), data=df_f_RS)
                            
                            
                            
                            
                            # 1.3 Children ----------------------------------------------------------------
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,TotalTHB)%>%
                              dplyr::filter(Age=="Children(Under18)")%>%
                              dplyr::select(-c(Age))%>%
                              data.table()
                            
                            
                            df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                            df$MarriParticipant<-as.factor(df$MarriParticipant)
                            df$Codes<-NULL
                            
                            
                            
                            ## RS
                            df_f_RS<-df %>%
                              dplyr:: filter(MarriParticipant=="Serbia")%>%
                              dplyr::rename("Children"="TotalTHB")
                            
                            # Regression 
                            OLS_RS3<-lm(log(Children)~(Year), data=df_f_RS)
                            
                            # 1.4 Adults ------------------------------------------------------------------
                            
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,TotalTHB)%>%
                              dplyr::filter(Age=="Adults(18+)")%>%
                              dplyr::select(-c(Age))%>%
                              data.table()
                            
                            
                            df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                            df$MarriParticipant<-as.factor(df$MarriParticipant)
                            df$Codes<-NULL
                            
                            
                            
                            ## RS
                            df_f_RS<-df %>%
                              dplyr:: filter(MarriParticipant=="Serbia")%>%
                              dplyr::rename("Adult"="TotalTHB")
                            
                            # Regression 
                            OLS_RS4<-lm(log(Adult)~(Year), data=df_f_RS)
                            
                            
                            # 1.5 Output ------------------------------------------------------------------
                            
                            stargazer(OLS_RS1,OLS_RS2,OLS_RS3,OLS_RS4,
                                      type = "text", title="Comparison of log-lin models", digits=4, out="ComparisionTotalTHB_RS.txt")
                            
                            
                            
                      # 2. Visualizations--------------------------------------------
                            #  2.1 Facet  -----------------------------------------------------------------------
                            # 2.1.1 Total number of identified and presumed victims of trafficking (VT/PVT) in the period 2018-2022 -----------------------------------------------------------------
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,TotalTHB)%>%
                              dplyr::filter(Age=="Total"&Codes=='RS')%>%
                              dplyr::select(-c(Age))
                            
                            
                            df$Year<-as.character(df$Year)
                            
                            df<-df%>%
                              dplyr::group_by(Year)%>%
                              dplyr::summarize(TotalTHB=sum(TotalTHB))%>%
                              data.table()
                            
                            
                            Chart1 <- plot_ly(df,x = ~Year, y = ~TotalTHB, type = 'scatter', mode = 'lines', fill = 'tozeroy',
                                              text = ~paste0(" ",round(TotalTHB,0)),
                                              textposition = "outside"
                            )
                            
                            
                            # 2.1.2 Presumed Victims-------------------------------------------------
                            
                            # color = c('#2ca02c', '#ff7f0e','#d62728', '#1f77b4','#9370DB','#8c564b'
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,PresumedVictims)%>%
                              dplyr::filter(Age=="Total"&Codes=='RS')%>%
                              dplyr::select(-c(Age))%>%
                              data.table()
                            
                            
                            df$Year<-as.character(df$Year)
                            
                            
                            
                            Chart2 <- plot_ly(df,x = ~Year, y = ~PresumedVictims, type = 'scatter',  fill = 'tonexty',
                                              # mode = 'lines',
                                              #text = ~paste0(" ",round(PresumedVictims,0)),
                                              #textposition = "outside"
                                              mode = "text+lines", 
                                              text = ~ paste0(" ", round(PresumedVictims, 0)),
                                              textposition="outside"
                                              
                                              
                            )
                            
                            
                            # 2.1.3 Identified Victims------------------------------------
                            
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,IdentifiedVictims)%>%
                              dplyr::filter(Age=="Total"&Codes=='RS')%>%
                              dplyr::select(-c(Age))%>%
                              data.table()
                            
                            
                            df$Year<-as.character(df$Year)
                            
                            
                            
                            Chart3 <- plot_ly(df,x = ~Year, y = ~IdentifiedVictims, type = 'scatter', fill = 'tonexty',
                                              # text = ~paste0(" ",round(IdentifiedVictims,0)),
                                              # textposition = "outside"
                                              mode = "text+lines", 
                                              text = ~ paste0(" ", round(IdentifiedVictims, 0)),
                                              textposition="outside"
                            )
                            
                            
                            
                            # 2.1.4 Children(Under18)---------------------------------------------
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,TotalTHB)%>%
                              dplyr::filter(Age=="Children(Under18)"&Codes=='RS')%>%
                              dplyr::select(-c(Codes,Age))
                            
                            
                            df$Year<-as.character(df$Year)
                            
                            
                            
                            Chart4a <- plot_ly(df,x = ~Year, y = ~TotalTHB, type = 'scatter', fill = 'tonexty',
                                               # text = ~paste0(" ",round(TotalTHB,0)),
                                               # textposition = "outside"
                                               mode = "text+lines", 
                                               text = ~ paste0(" ", round(TotalTHB, 0)),
                                               textposition="outside"
                                               
                                               
                                               
                            )%>%
                              layout(
                                annotations =
                                  list(x = 0, y = -0.18,
                                       title = " ",
                                       text = "Source: MARRI-RC",
                                       showarrow = F,
                                       xref='paper',
                                       yref='paper'),font = t_8)
                            
                            
                            # 2.1.5 Adult -------------------------------------------------------------------
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,TotalTHB)%>%
                              dplyr::filter(Age=="Adults(18+)"&Codes=='RS')%>%
                              dplyr::select(-c(Codes,Age))
                            
                            
                            df$Year<-as.character(df$Year)
                            
                            
                            Chart4ab <- plot_ly(df,x = ~Year, y = ~TotalTHB, type = 'scatter', fill = 'tonexty',
                                                # text = ~paste0(" ",round(TotalTHB,0)),
                                                # textposition = "outside"
                                                
                                                mode = "text+lines", 
                                                text = ~ paste0(" ", round(TotalTHB, 0)),
                                                textposition="outside"
                            )
                            
                            
                            
                            # 2.1.6 Output -------------------------------------------------------------------
                            
                            
                            fig <- subplot(
                              style(Chart2, showlegend = FALSE),
                              style(Chart3, showlegend = FALSE),
                              style(Chart4a, showlegend = FALSE),
                              style(Chart4ab, showlegend = FALSE),
                              # titleX  = FALSE,
                              # style(Chart1,showlegend = FALSE),
                              # Chart2, #style(Chart3, showlegend = FALSE),
                              nrows = 2, margin = 0.05
                            )
                            
                            annotations = list( 
                              list( 
                                x = 0.2,  
                                y = 1.0,  
                                text = "<b>Total number of presumed victims of trafficking (PVT)<b>",  
                                xref = "paper",  
                                yref = "paper",  
                                xanchor = "center",  
                                yanchor = "bottom",  
                                showarrow = FALSE 
                              ),  
                              list( 
                                x = 0.8,  
                                y = 1,  
                                text = "<b>Total number of identify victims of trafficking (VT)<b>",  
                                xref = "paper",  
                                yref = "paper",  
                                xanchor = "center",  
                                yanchor = "bottom",  
                                showarrow = FALSE 
                              ),  
                              list( 
                                x = 0.2,  
                                y = 0.45,  
                                text = "<b>Total number of children (VT/PVT)<b>",  
                                xref = "paper",  
                                yref = "paper",  
                                xanchor = "center",  
                                yanchor = "bottom",  
                                showarrow = FALSE 
                              ),
                              list( 
                                x = 0.8,  
                                y = 0.45,  
                                text = "<b>Total number of adults (VT/PVT)<b>",  
                                xref = "paper",  
                                yref = "paper",  
                                xanchor = "center",  
                                yanchor = "bottom",  
                                showarrow = FALSE 
                              ))
                            
                            
                            # t_8 <- list(
                            #   family = "Arial",
                            #   size = 14#8
                            # )
                            # 
                            
                            
                            
                            fig <- fig %>%layout(annotations = annotations,font = t) 
                            
                            fig
                            
                            
                            
                            
                            #  2.2 Facet  -------------------------------------------------
                            
                            library(plotly)
                            library(htmltools)
                            
                            legend <- list(orientation = "v", y = 1, x = 1)
                            
                            # 2.1 Female --------------------------------------------------------------
                            
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,Male,Female,Transgender)%>%
                              #dplyr::filter(!Age %in% c('Total'))%>%
                              dplyr::select(-c(Age))%>%
                              dplyr::group_by(Year,Codes)%>%
                              dplyr:: summarize(Male=sum(Male),
                                                Female=sum(Female),
                                                Transgender=sum(Transgender))%>%
                              dplyr::mutate(Total=Male+Female+Transgender)%>%
                              dplyr::mutate(female=(Female/Total)*100,
                                            male=(Male/Total)*100,
                                            transgender=(Transgender/Total)*100)%>%
                              dplyr::filter(Codes=='RS')%>%
                              dplyr::select(-c(Male,Female,Transgender,Total))%>%
                              data.table()
                            
                            
                            df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                            df$Year<-as.character(df$Year)
                            df<-melt(df)
                            df$MarriParticipant<-as.factor(df$MarriParticipant)
                            df$color <- factor(df$variable, labels = c("goldenrod","#2ca02c","#9870AB"))
                            
                            
                            
                            Chart1<-plot_ly(df, x = ~Year, y = ~value,
                                            color = ~variable, colors = ~as.character(color), type = "bar",
                                            legendgroup = "1"
                            )|>
                              layout(barmode = "stack",showlegend = TRUE, legend = legend, 
                                     title = '<b>Percentage of VT/PVT disaggregated by gender<b>',font = t,
                                     xaxis = list(title = ' '),
                                     yaxis = list(title = 'Percentage')
                              )
                            
                            
                            # 2.2 Children ----------------------------------------------------------------
                            
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,TotalTHB)%>%
                              dplyr::filter(!Age %in% c('Unknown'))
                            
                            df_total<-df%>%
                              dplyr::select(Age,Codes,Year,TotalTHB)%>%
                              dplyr::filter(Age=='Total'&Codes=='RS')%>%
                              dplyr::select(-c(Age))%>%
                              dplyr::rename('total'='TotalTHB')
                            
                            df_total1<-left_join(df,df_total,by = c('Codes','Year'))
                            
                            df_total2<-df_total1%>%
                              dplyr::filter(!Age %in% c('Total')&Codes=='RS')%>%
                              dplyr::mutate(share=(TotalTHB/total)*100)
                            df_total2$TotalTHB<-NULL
                            df_total2$total<-NULL
                            
                            
                            df_total2<-left_join(df_total2,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                            df_total2$Year<-as.character(df_total2$Year)
                            df_total3<-melt(df_total2)
                            df_total3$MarriParticipant<-as.factor(df_total3$MarriParticipant)
                            
                            df_total3$color <- factor(df_total3$Age, labels = c("orange","forestgreen"))
                            df_total3[is.na(df_total3)] <- 0
                            
                            df_total3$variable<-NULL
                            df_total3$codes<-NULL
                            
                            #'#bcbd22', "#66CC99",
                            
                            df_total4 <- df_total3 %>%
                              dplyr::select(-color)  %>%
                              tidyr::pivot_wider(names_from = Age, values_from = value)
                            
                            df_total4<-df_total4%>%arrange((MarriParticipant))
                            
                            
                            Chart2<-plot_ly(df_total4,
                                            x = ~ Year, y = ~`Children(Under18)`,
                                            type = "bar", marker = list(
                                              color = "#1f77b4"
                                            ), name = "Children",legendgroup = "2") %>%
                              add_bars(y = ~`Adults(18+)`, marker = list(
                                color = "#66CC99"
                              ), name = "Adults")  %>%
                              layout(showlegend = TRUE, legend = legend,
                                     barmode = "stack",
                                     title = "<b>Percentage of children identified as VT/PVT<b>",font = t,
                                     xaxis = list(title = ""),
                                     yaxis = list(title = "Percentage")) #,
                            
                            
                            # 2.3 Data on the cases of trafficking separated with forms of trafficking-------------------------
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,Type_SexualExploitation,Type_ForcedBegging,TypeForced_marriage,Type_LabourExploitation,
                                            Type_ForcedCriminality,Type_TraffickingWithBabies,Type_TraffickingWithOrgans,Type_Pornography,Type_MixedForms,Type_OtherForms)%>%
                              dplyr::filter(Age=="Total")%>%
                              dplyr::select(-c(Age))%>%
                              dplyr::group_by(Year,Codes)%>%
                              dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>%
                              data.table()%>%
                              rename('Forced marriage'='TypeForced_marriage',
                                     'Sexual Exploitation'='Type_SexualExploitation',
                                     'Forced Begging'='Type_ForcedBegging',
                                     'Labour Exploitation'='Type_LabourExploitation',
                                     'Forced Criminality'='Type_ForcedCriminality',
                                     'Trafficking with Babies'='Type_TraffickingWithBabies',
                                     'Trafficking with Organs'='Type_TraffickingWithOrgans',
                                     'Pornography'='Type_Pornography',
                                     'Mixed Forms'='Type_MixedForms',
                                     'Other Forms'='Type_OtherForms',
                              )
                            
                            
                            df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))#%>%
                            #dplyr::select(MarriParticipant,Year,IdentifiedVictims)
                            
                            df$Year<-as.character(df$Year)
                            
                            df$MarriParticipant<-as.factor(df$MarriParticipant)
                            
                            df$Codes<-NULL
                            
                            df_t<-melt(df)%>%
                              dplyr::filter(MarriParticipant=="Albania")
                            
                            df_t$color <- factor(df_t$variable, labels = c('#d62728', '#ff7f0e','#2ca02c','#bcbd22', "#66CC99",'#9370DB','#8c564b',"#CC6666","#9870AB",'#1f77b4'))
                            
                            Chart3<-plot_ly(df_t, x = ~Year,y = ~value,
                                            
                                            color = ~variable, colors = ~as.character(color), type = "bar")%>%
                              layout(showlegend = TRUE, legend = legend,
                                     barmode = "stack",
                                     title = '<b>Data on forms of human trafficking</b>',font = t,
                                     xaxis = list(title = ''),
                                     yaxis = list(title = ' '))
                            
                            
                            # 2.4 Domestic and foreign victims of the VT/PVT for the period 2018-2022, by MARRI Participant------------------
                            
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,
                                            NationalityandCountryOfExplotation_DomesticCitizensExploitedAbroad,
                                            NationalityandCountryOfExplotation_ForeignCitizensInternallyExploited,
                                            NationalityandCountryOfExplotation_InternalTrafficking)%>%
                              dplyr::filter(Age=="Total"&Codes=='RS')%>%
                              dplyr::select(-c(Age))%>%
                              dplyr::group_by(Year,Codes)%>%
                              dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>%
                              data.table()%>%
                              rename('Domestic victums exploited abroad'='NationalityandCountryOfExplotation_DomesticCitizensExploitedAbroad',
                                     'Foreign victums internally exploited'='NationalityandCountryOfExplotation_ForeignCitizensInternallyExploited',
                                     'Domestic victums exploited internally'='NationalityandCountryOfExplotation_InternalTrafficking')
                            
                            
                            df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))#%>%
                            #dplyr::select(MarriParticipant,Year,IdentifiedVictims)
                            
                            df$Year<-as.character(df$Year)
                            
                            df$MarriParticipant<-as.factor(df$MarriParticipant)
                            
                            
                            df$Codes<-NULL
                            
                            df_t<-melt(df)
                            
                            df_t$color <- factor(df_t$variable, labels = c('#d62728',"#1f77b4", '#ff7f0e'))
                            
                            
                            Chart4<-plot_ly(df_t, x = ~Year, y = ~value,
                                            color = ~variable, colors = ~as.character(color), type = "bar")%>%
                              layout(showlegend = TRUE, legend = legend,
                                     barmode = "stack",
                                     title = '<b>Domestic and foreign victums of the VT/PVT<b>',font = t,
                                     xaxis = list(title = ''),
                                     yaxis = list(title = ' ')) #,
                            Chart4
                            
                            # Facet 
                            
                            htmltools::div(
                              crosstalk::bscols(
                                Chart1, Chart2
                              ),
                              crosstalk::bscols(
                                Chart3, Chart4
                              )
                            ) |>
                              htmltools::browsable()
                            
                            
                            
                            # IV. KOSOVO             
                            
                            
                            
                            
                            
                            
                            
  # 5.Montenegro -----------------------------------------------------------

                    # 1.Log-lin models -------------------------------------------------------------
                            # 1.1 Presumed Victims (Regression) ----------------------------------------------------------------
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,PresumedVictims)%>%
                              dplyr::filter(Age=="Total")%>%
                              dplyr::select(-c(Age))%>%
                              data.table()
                            
                            
                            df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                            df$MarriParticipant<-as.factor(df$MarriParticipant)
                            df$Codes<-NULL
                            
                     
                            ## ME
                            df_f_ME<-df %>%
                              dplyr:: filter(MarriParticipant=="Montenegro")
                            
                            # Regression 
                            OLS_ME1<-lm(log(PresumedVictims)~(Year), data=df_f_ME)
                            
                            
                            
                            # 1.2 Identified Victims-------------------------------------------------------------------------
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,IdentifiedVictims)%>%
                              dplyr::filter(Age=="Total")%>%
                              dplyr::select(-c(Age))%>%
                              data.table()
                            
                            
                            df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                            df$MarriParticipant<-as.factor(df$MarriParticipant)
                            df$Codes<-NULL
                            
                        
                            
                            ## ME
                            df_f_ME<-df %>%
                              dplyr:: filter(MarriParticipant=="Montenegro")
                            
                            # Regression 
                            OLS_ME2<-lm(log(IdentifiedVictims)~(Year), data=df_f_ME)
                            
                            
                            
                            
                            # 1.3 Children ----------------------------------------------------------------
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,TotalTHB)%>%
                              dplyr::filter(Age=="Children(Under18)")%>%
                              dplyr::select(-c(Age))%>%
                              data.table()
                            
                            
                            df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                            df$MarriParticipant<-as.factor(df$MarriParticipant)
                            df$Codes<-NULL
                            
                     
                            ## ME
                            df_f_ME<-df %>%
                              dplyr:: filter(MarriParticipant=="Montenegro")%>%
                              dplyr::rename("Children"="TotalTHB")
                            
                            # Regression 
                            OLS_ME3<-lm(log(Children)~(Year), data=df_f_ME)
                            
                            # 1.4 Adults ------------------------------------------------------------------
                            
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,TotalTHB)%>%
                              dplyr::filter(Age=="Adults(18+)")%>%
                              dplyr::select(-c(Age))%>%
                              data.table()
                            
                            
                            df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                            df$MarriParticipant<-as.factor(df$MarriParticipant)
                            df$Codes<-NULL
                            
                          
                            
                            ## ME
                            df_f_ME<-df %>%
                              dplyr:: filter(MarriParticipant=="Montenegro")%>%
                              dplyr::rename("Adult"="TotalTHB")
                            
                            # Regression 
                            OLS_ME4<-lm(log(Adult)~(Year), data=df_f_ME)
                            

                            # 1.5 Output ------------------------------------------------------------------

                            
                            stargazer(OLS_ME2,OLS_ME3,
                                      type = "text", title="Comparison of log-lin models", digits=4, out="ComparisionTotalTHB_ME.txt")
                            
                            
                            
                            
                            
                            
                            
                            Visualizations
                  # 2. Visualizations ---------------------------------------------------------------------

                            
                            
                            
                            # 2.1 Facet 1 -----------------------------------------------------------------------
                            # 2.1.1.Total number of identified and presumed victims of trafficking (VT/PVT) in the period 2018-2022 -----------------------------------------------------------------
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,TotalTHB)%>%
                              dplyr::filter(Age=="Total"&Codes=='ME')%>%
                              dplyr::select(-c(Age))
                            
                            
                            df$Year<-as.character(df$Year)
                            
                            df<-df%>%
                              dplyr::group_by(Year)%>%
                              dplyr::summarize(TotalTHB=sum(TotalTHB))%>%
                              data.table()
                            
                            
                            Chart1 <- plot_ly(df,x = ~Year, y = ~TotalTHB, type = 'scatter', mode = 'lines', fill = 'tozeroy',
                                              text = ~paste0(" ",round(TotalTHB,0)),
                                              textposition = "outside"
                            )
                            
                            
                            # 2.1.1 Presumed Victims-------------------------------------------------
                            
                            # color = c('#2ca02c', '#ff7f0e','#d62728', '#1f77b4','#9370DB','#8c564b'
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,PresumedVictims)%>%
                              dplyr::filter(Age=="Total"&Codes=='ME')%>%
                              dplyr::select(-c(Age))%>%
                              data.table()
                            
                            
                            df$Year<-as.character(df$Year)
                            
                            
                            
                            Chart2 <- plot_ly(df,x = ~Year, y = ~PresumedVictims, type = 'scatter',  fill = 'tonexty',
                                              # mode = 'lines',
                                              #text = ~paste0(" ",round(PresumedVictims,0)),
                                              #textposition = "outside"
                                              mode = "text+lines", 
                                              text = ~ paste0(" ", round(PresumedVictims, 0)),
                                              textposition="outside"
                                              
                                              
                            )
                            
                            # 2.1.2 Identified Victims------------------------------------
                            
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,IdentifiedVictims)%>%
                              dplyr::filter(Age=="Total"&Codes=='ME')%>%
                              dplyr::select(-c(Age))%>%
                              data.table()
                            
                            
                            df$Year<-as.character(df$Year)
                            
                            
                            ## NEW TEST
                            
                            Chart3 <- plot_ly(df,x = ~Year, y = ~IdentifiedVictims, type = 'scatter', fill = 'tonexty',
                                              # text = ~paste0(" ",round(IdentifiedVictims,0)),
                                              # textposition = "outside"
                                              mode = "text+lines", 
                                              text = ~ paste0(" ", round(IdentifiedVictims, 0)),
                                              textposition="outside"
                            )
                            
                            
                            
                            # 2.1.3 Children(Under18)---------------------------------------------
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,TotalTHB)%>%
                              dplyr::filter(Age=="Children(Under18)"&Codes=='ME')%>%
                              dplyr::select(-c(Codes,Age))
                            
                            
                            df$Year<-as.character(df$Year)
                            
                            
                            ## NEW TEST
                            
                            Chart4a <- plot_ly(df,x = ~Year, y = ~TotalTHB, type = 'scatter', fill = 'tonexty',
                                               # text = ~paste0(" ",round(TotalTHB,0)),
                                               # textposition = "outside"
                                               mode = "text+lines", 
                                               text = ~ paste0(" ", round(TotalTHB, 0)),
                                               textposition="outside"
                                               
                                               
                                               
                            )%>%
                              layout(
                                annotations =
                                  list(x = 0, y = -0.18,
                                       title = " ",
                                       text = "Source: MARRI-RC",
                                       showarrow = F,
                                       xref='paper',
                                       yref='paper'),font = t_8)
                            
                            
                            # 2.1.4 Adult -------------------------------------------------------------------
                            
                            df<-data_tbl%>%
                              dplyr::select(Year,Age,Codes,TotalTHB)%>%
                              dplyr::filter(Age=="Adults(18+)"&Codes=='ME')%>%
                              dplyr::select(-c(Codes,Age))
                            
                            
                            df$Year<-as.character(df$Year)
                            
                            
                            ## NEW TEST
                            
                            Chart4ab <- plot_ly(df,x = ~Year, y = ~TotalTHB, type = 'scatter', fill = 'tonexty',
                                                # text = ~paste0(" ",round(TotalTHB,0)),
                                                # textposition = "outside"
                                                
                                                mode = "text+lines", 
                                                text = ~ paste0(" ", round(TotalTHB, 0)),
                                                textposition="outside"
                            )
                            #Chart4ab
                            
                            
                            # 2.1.5 Facet -------------------------------------------------------------------
                            
                            
                            fig <- subplot(
                              style(Chart2, showlegend = FALSE),
                              style(Chart3, showlegend = FALSE),
                              style(Chart4a, showlegend = FALSE),
                              style(Chart4ab, showlegend = FALSE),
                              # titleX  = FALSE,
                              # style(Chart1,showlegend = FALSE),
                              # Chart2, #style(Chart3, showlegend = FALSE),
                              nrows = 2, margin = 0.05
                            )
                            
                            annotations = list( 
                              list( 
                                x = 0.2,  
                                y = 1.0,  
                                text = "<b>Total number of presumed victims of trafficking (PVT)<b>",  
                                xref = "paper",  
                                yref = "paper",  
                                xanchor = "center",  
                                yanchor = "bottom",  
                                showarrow = FALSE 
                              ),  
                              list(
                                x = 0.8,
                                y = 1,
                                text = "<b>Total number of identify victims of trafficking (VT)<b>",
                                xref = "paper",
                                yref = "paper",
                                xanchor = "center",
                                yanchor = "bottom",
                                showarrow = FALSE
                              ),
                              list( 
                                x = 0.2,  
                                y = 0.45,  
                                text = "<b>Total number of children (VT/PVT)<b>",  
                                xref = "paper",  
                                yref = "paper",  
                                xanchor = "center",  
                                yanchor = "bottom",  
                                showarrow = FALSE 
                              ),
                              list( 
                                x = 0.8,  
                                y = 0.45,  
                                text = "<b>Total number of adults (VT/PVT)<b>",  
                                xref = "paper",  
                                yref = "paper",  
                                xanchor = "center",  
                                yanchor = "bottom",  
                                showarrow = FALSE 
                              ))
                            
                            
                            # t_8 <- list(
                            #   family = "Arial",
                            #   size = 14#8
                            # )
                            # 
                            
                            #fig <- fig %>%layout(annotations = annotations,font = t) 
                            
                            
                            
                            fig <- fig %>%layout(annotations = annotations,font = t) 
                            
                            fig
                            
                            
                       
                          #  2.2 Facet 2 -------------------------------------------------
                          
                          legend <- list(orientation = "v", y = 1, x = 1)
                          
                              # 2.1 Female --------------------------------------------------------------
                              
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,Male,Female,Transgender)%>%
                                #dplyr::filter(!Age %in% c('Total'))%>%
                                dplyr::select(-c(Age))%>%
                                dplyr::group_by(Year,Codes)%>%
                                dplyr:: summarize(Male=sum(Male),
                                                  Female=sum(Female),
                                                  Transgender=sum(Transgender))%>%
                                dplyr::mutate(Total=Male+Female+Transgender)%>%
                                dplyr::mutate(female=(Female/Total)*100,
                                              male=(Male/Total)*100,
                                              transgender=(Transgender/Total)*100)%>%
                                dplyr::filter(Codes=='ME')%>%
                                dplyr::select(-c(Male,Female,Transgender,Total))%>%
                                data.table()
                              
                              
                              df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                              df$Year<-as.character(df$Year)
                              df<-melt(df)
                              df$MarriParticipant<-as.factor(df$MarriParticipant)
                              df$color <- factor(df$variable, labels = c("goldenrod","#2ca02c","#9870AB"))
                              
                              
                              
                              Chart1<-plot_ly(df, x = ~Year, y = ~value,
                                              color = ~variable, colors = ~as.character(color), type = "bar",
                                              legendgroup = "1"
                              )|>
                                layout(barmode = "stack",showlegend = TRUE, legend = legend, 
                                       title = '<b>Percentage of VT/PVT disaggregated by gender<b>',font = t_8,
                                       xaxis = list(title = ' '),
                                       yaxis = list(title = 'Percentage')
                                )
                              
                              # 2.2 Children ----------------------------------------------------------------
                              
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,TotalTHB)%>%
                                dplyr::filter(!Age %in% c('Unknown'))
                              
                              df_total<-df%>%
                                dplyr::select(Age,Codes,Year,TotalTHB)%>%
                                dplyr::filter(Age=='Total'&Codes=='ME')%>%
                                dplyr::select(-c(Age))%>%
                                dplyr::rename('total'='TotalTHB')
                              
                              df_total1<-left_join(df,df_total,by = c('Codes','Year'))
                              
                              df_total2<-df_total1%>%
                                dplyr::filter(!Age %in% c('Total')&Codes=='ME')%>%
                                dplyr::mutate(share=(TotalTHB/total)*100)
                              df_total2$TotalTHB<-NULL
                              df_total2$total<-NULL
                              
                              
                              df_total2<-left_join(df_total2,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                              df_total2$Year<-as.character(df_total2$Year)
                              df_total3<-melt(df_total2)
                              df_total3$MarriParticipant<-as.factor(df_total3$MarriParticipant)
                              
                              df_total3$color <- factor(df_total3$Age, labels = c("orange","forestgreen"))
                              df_total3[is.na(df_total3)] <- 0
                              
                              df_total3$variable<-NULL
                              df_total3$codes<-NULL
                              
                              #'#bcbd22', "#66CC99",
                              
                              df_total4 <- df_total3 %>%
                                dplyr::select(-color)  %>%
                                tidyr::pivot_wider(names_from = Age, values_from = value)
                              
                              df_total4<-df_total4%>%arrange((MarriParticipant))
                              
                              
                              Chart2<-plot_ly(df_total4,
                                              x = ~ Year, y = ~`Children(Under18)`,
                                              type = "bar", marker = list(
                                                color = "#1f77b4"
                                              ), name = "Children",legendgroup = "2") %>%
                                add_bars(y = ~`Adults(18+)`, marker = list(
                                  color = "#66CC99"
                                ), name = "Adults")  %>%
                                layout(showlegend = TRUE, legend = legend,
                                       barmode = "stack",
                                       title = "<b>Percentage of children identified as VT/PVT<b>",font = t_8,
                                       xaxis = list(title = ""),
                                       yaxis = list(title = "Percentage")) #,
                              
                              
                              # 2.3 Data on the cases of trafficking separated with forms of trafficking-------------------------
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,Type_SexualExploitation,Type_ForcedBegging,TypeForced_marriage,Type_LabourExploitation,
                                              Type_ForcedCriminality,Type_TraffickingWithBabies,Type_TraffickingWithOrgans,Type_Pornography,Type_MixedForms,Type_OtherForms)%>%
                                dplyr::filter(Age=="Total")%>%
                                dplyr::select(-c(Age))%>%
                                dplyr::group_by(Year,Codes)%>%
                                dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>%
                                data.table()%>%
                                rename('Forced marriage'='TypeForced_marriage',
                                       'Sexual Exploitation'='Type_SexualExploitation',
                                       'Forced Begging'='Type_ForcedBegging',
                                       'Labour Exploitation'='Type_LabourExploitation',
                                       'Forced Criminality'='Type_ForcedCriminality',
                                       'Trafficking with Babies'='Type_TraffickingWithBabies',
                                       'Trafficking with Organs'='Type_TraffickingWithOrgans',
                                       'Pornography'='Type_Pornography',
                                       'Mixed Forms'='Type_MixedForms',
                                       'Other Forms'='Type_OtherForms',
                                )
                              
                              
                              df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))#%>%
                              #dplyr::select(MarriParticipant,Year,IdentifiedVictims)
                              
                              df$Year<-as.character(df$Year)
                              
                              df$MarriParticipant<-as.factor(df$MarriParticipant)
                              
                              df$Codes<-NULL
                              
                              df_t<-melt(df)%>%
                                dplyr::filter(MarriParticipant=="Albania")
                              
                              df_t$color <- factor(df_t$variable, labels = c('#d62728', '#ff7f0e','#2ca02c','#bcbd22', "#66CC99",'#9370DB','#8c564b',"#CC6666","#9870AB",'#1f77b4'))
                              
                              Chart3<-plot_ly(df_t, x = ~Year,y = ~value,
                                              
                                              color = ~variable, colors = ~as.character(color), type = "bar")%>%
                                layout(showlegend = TRUE, legend = legend,
                                       barmode = "stack",
                                       title = '<b>Data on forms of human trafficking</b>',font = t_8,
                                       xaxis = list(title = ''),
                                       yaxis = list(title = ' '))
                              
                              
                              
                              # 2.4 Domestic and foreign victims of the VT/PVT for the period 2018-2022, by MARRI Participant------------------
                              
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,
                                              NationalityandCountryOfExplotation_DomesticCitizensExploitedAbroad,
                                              NationalityandCountryOfExplotation_ForeignCitizensInternallyExploited,
                                              NationalityandCountryOfExplotation_InternalTrafficking)%>%
                                dplyr::filter(Age=="Total"&Codes=='ME')%>%
                                dplyr::select(-c(Age))%>%
                                dplyr::group_by(Year,Codes)%>%
                                dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>%
                                data.table()%>%
                                rename('Domestic victums exploited abroad'='NationalityandCountryOfExplotation_DomesticCitizensExploitedAbroad',
                                       'Foreign victums internally exploited'='NationalityandCountryOfExplotation_ForeignCitizensInternallyExploited',
                                       'Domestic victums exploited internally'='NationalityandCountryOfExplotation_InternalTrafficking')
                              
                              
                              df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))#%>%
                              #dplyr::select(MarriParticipant,Year,IdentifiedVictims)
                              
                              df$Year<-as.character(df$Year)
                              
                              df$MarriParticipant<-as.factor(df$MarriParticipant)
                              
                              
                              df$Codes<-NULL
                              
                              df_t<-melt(df)
                              
                              df_t$color <- factor(df_t$variable, labels = c('#d62728',"#1f77b4", '#ff7f0e'))
                              
                              
                              Chart4<-plot_ly(df_t, x = ~Year, y = ~value,
                                              color = ~variable, colors = ~as.character(color), type = "bar")%>%
                                layout(showlegend = TRUE, legend = legend,
                                       barmode = "stack",
                                       title = '<b>Domestic and foreign victums of the VT/PVT<b>',font = t_8,
                                       xaxis = list(title = ''),
                                       yaxis = list(title = ' ')) #,
                              
                              # Facet 
                              
                              htmltools::div(
                                crosstalk::bscols(
                                  Chart1, Chart2
                                ),
                                crosstalk::bscols(
                                  Chart3, Chart4
                                )
                              ) |>
                                htmltools::browsable()
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
      # 6.Kosovo* ------------------------------------------------------------------
                    # 1.Log-lin models -------------------------------------------------------------
                              # 1.1 Presumed Victims (Regression) ----------------------------------------------------------------
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,PresumedVictims)%>%
                                dplyr::filter(Age=="Total")%>%
                                dplyr::select(-c(Age))%>%
                                data.table()
                              
                              
                              df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                              df$MarriParticipant<-as.factor(df$MarriParticipant)
                              df$Codes<-NULL
                              
                              
                              
                              ## XK
                              df_f_XK<-df %>%
                                dplyr:: filter(MarriParticipant=="Kosovo*")
                              
                              # Regression 
                              OLS_XK1<-lm(log(PresumedVictims)~(Year), data=df_f_XK)
                              
                              
                              
                              # 1.2 Identified Victims-------------------------------------------------------------------------
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,IdentifiedVictims)%>%
                                dplyr::filter(Age=="Total")%>%
                                dplyr::select(-c(Age))%>%
                                data.table()
                              
                              
                              df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                              df$MarriParticipant<-as.factor(df$MarriParticipant)
                              df$Codes<-NULL
                              
                              
                              
                              ## XK
                              df_f_XK<-df %>%
                                dplyr:: filter(MarriParticipant=="Kosovo*")
                              
                              # Regression 
                              OLS_XK2<-lm(log(IdentifiedVictims)~(Year), data=df_f_XK)
                              
                              
                              
                              
                              # 1.3 Children ----------------------------------------------------------------
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,TotalTHB)%>%
                                dplyr::filter(Age=="Children(Under18)")%>%
                                dplyr::select(-c(Age))%>%
                                data.table()
                              
                              
                              df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                              df$MarriParticipant<-as.factor(df$MarriParticipant)
                              df$Codes<-NULL
                              
                              
                              
                              ## XK
                              df_f_XK<-df %>%
                                dplyr:: filter(MarriParticipant=="Kosovo*")%>%
                                dplyr::rename("Children"="TotalTHB")
                              
                              # Regression 
                              OLS_XK3<-lm(log(Children)~(Year), data=df_f_XK)
                              
                              # 1.4 Adults ------------------------------------------------------------------
                              
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,TotalTHB)%>%
                                dplyr::filter(Age=="Adults(18+)")%>%
                                dplyr::select(-c(Age))%>%
                                data.table()
                              
                              
                              df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                              df$MarriParticipant<-as.factor(df$MarriParticipant)
                              df$Codes<-NULL
                              
                              
                              
                              ## XK
                              df_f_XK<-df %>%
                                dplyr:: filter(MarriParticipant=="Kosovo*")%>%
                                dplyr::rename("Adult"="TotalTHB")
                              
                              # Regression 
                              OLS_XK4<-lm(log(Adult)~(Year), data=df_f_XK)
                              
                              
                              # 1.5 Output ------------------------------------------------------------------
                              
                              stargazer(OLS_XK1,OLS_XK2,OLS_XK3,OLS_XK4,
                                        type = "text", title="Comparison of log-lin models", digits=4, out="ComparisionTotalTHB_XK.txt")
                              
                              
                              
                              
                              
                      # 2. Visualizations--------------------------------------------
                              # 2.1 Facet  -----------------------------------------------------------------------
                              # 1.Total number of identified and presumed victims of trafficking (VT/PVT) in the period 2018-2022 -----------------------------------------------------------------
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,TotalTHB)%>%
                                dplyr::filter(Age=="Total"&Codes=='XK')%>%
                                dplyr::select(-c(Age))
                              
                              
                              df$Year<-as.character(df$Year)
                              
                              df<-df%>%
                                dplyr::group_by(Year)%>%
                                dplyr::summarize(TotalTHB=sum(TotalTHB))%>%
                                data.table()
                              
                              
                              Chart1 <- plot_ly(df,x = ~Year, y = ~TotalTHB, type = 'scatter', mode = 'lines', fill = 'tozeroy',
                                                text = ~paste0(" ",round(TotalTHB,0)),
                                                textposition = "outside"
                              )
                              
                              
                              # 1.1 Presumed Victims-------------------------------------------------
                              
                              # color = c('#2ca02c', '#ff7f0e','#d62728', '#1f77b4','#9370DB','#8c564b'
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,PresumedVictims)%>%
                                dplyr::filter(Age=="Total"&Codes=='XK')%>%
                                dplyr::select(-c(Age))%>%
                                data.table()
                              
                              
                              df$Year<-as.character(df$Year)
                              
                              
                              
                              Chart2 <- plot_ly(df,x = ~Year, y = ~PresumedVictims, type = 'scatter',  fill = 'tonexty',
                                                # mode = 'lines',
                                                #text = ~paste0(" ",round(PresumedVictims,0)),
                                                #textposition = "outside"
                                                mode = "text+lines", 
                                                text = ~ paste0(" ", round(PresumedVictims, 0)),
                                                textposition="outside"
                                                
                                                
                              )
                              
                              
                              # 1.2 Identified Victims------------------------------------
                              
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,IdentifiedVictims)%>%
                                dplyr::filter(Age=="Total"&Codes=='XK')%>%
                                dplyr::select(-c(Age))%>%
                                data.table()
                              
                              
                              df$Year<-as.character(df$Year)
                              
                              
                              
                              Chart3 <- plot_ly(df,x = ~Year, y = ~IdentifiedVictims, type = 'scatter', fill = 'tonexty',
                                                # text = ~paste0(" ",round(IdentifiedVictims,0)),
                                                # textposition = "outside"
                                                mode = "text+lines", 
                                                text = ~ paste0(" ", round(IdentifiedVictims, 0)),
                                                textposition="outside"
                              )
                              
                              
                              
                              # 1.3 Children(Under18)---------------------------------------------
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,TotalTHB)%>%
                                dplyr::filter(Age=="Children(Under18)"&Codes=='XK')%>%
                                dplyr::select(-c(Codes,Age))
                              
                              
                              df$Year<-as.character(df$Year)
                              
                              
                              ## NEW TEST
                              
                              Chart4a <- plot_ly(df,x = ~Year, y = ~TotalTHB, type = 'scatter', fill = 'tonexty',
                                                 # text = ~paste0(" ",round(TotalTHB,0)),
                                                 # textposition = "outside"
                                                 mode = "text+lines", 
                                                 text = ~ paste0(" ", round(TotalTHB, 0)),
                                                 textposition="outside"
                                                 
                                                 
                                                 
                              )%>%
                                layout(
                                  annotations =
                                    list(x = 0, y = -0.18,
                                         title = " ",
                                         text = "Source: MARRI-RC",
                                         showarrow = F,
                                         xref='paper',
                                         yref='paper'),font = t_8)
                              
                              
                              # 1.4 Adult -------------------------------------------------------------------
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,TotalTHB)%>%
                                dplyr::filter(Age=="Adults(18+)"&Codes=='XK')%>%
                                dplyr::select(-c(Codes,Age))
                              
                              
                              df$Year<-as.character(df$Year)
                              
                              
                              ## NEW TEST
                              
                              Chart4ab <- plot_ly(df,x = ~Year, y = ~TotalTHB, type = 'scatter', fill = 'tonexty',
                                                  # text = ~paste0(" ",round(TotalTHB,0)),
                                                  # textposition = "outside"
                                                  
                                                  mode = "text+lines", 
                                                  text = ~ paste0(" ", round(TotalTHB, 0)),
                                                  textposition="outside"
                              )
                              
                              
                              # 1.5 Output -------------------------------------------------------------------
                              
                              
                              fig <- subplot(
                                style(Chart2, showlegend = FALSE),
                                style(Chart3, showlegend = FALSE),
                                style(Chart4a, showlegend = FALSE),
                                style(Chart4ab, showlegend = FALSE),
                                # titleX  = FALSE,
                                # style(Chart1,showlegend = FALSE),
                                # Chart2, #style(Chart3, showlegend = FALSE),
                                nrows = 2, margin = 0.05
                              )
                              
                              annotations = list( 
                                list( 
                                  x = 0.2,  
                                  y = 1.0,  
                                  text = "<b>Total number of presumed victims of trafficking (PVT)<b>",  
                                  xref = "paper",  
                                  yref = "paper",  
                                  xanchor = "center",  
                                  yanchor = "bottom",  
                                  showarrow = FALSE 
                                ),  
                                list( 
                                  x = 0.8,  
                                  y = 1,  
                                  text = "<b>Total number of identify victims of trafficking (VT)<b>",  
                                  xref = "paper",  
                                  yref = "paper",  
                                  xanchor = "center",  
                                  yanchor = "bottom",  
                                  showarrow = FALSE 
                                ),  
                                list( 
                                  x = 0.2,  
                                  y = 0.45,  
                                  text = "<b>Total number of children (VT/PVT)<b>",  
                                  xref = "paper",  
                                  yref = "paper",  
                                  xanchor = "center",  
                                  yanchor = "bottom",  
                                  showarrow = FALSE 
                                ),
                                list( 
                                  x = 0.8,  
                                  y = 0.45,  
                                  text = "<b>Total number of adults (VT/PVT)<b>",  
                                  xref = "paper",  
                                  yref = "paper",  
                                  xanchor = "center",  
                                  yanchor = "bottom",  
                                  showarrow = FALSE 
                                ))
                              
                              # 
                              # t_8 <- list(
                              #   family = "Arial",
                              #   size = 14#8
                              # )
                              
                              
                              
                              fig <- fig %>%layout(annotations = annotations,font = t) 
                              
                              fig
                              
                              
                              
                              
                              #  2.2 Facet -------------------------------------------------
                              
                              legend <- list(orientation = "v", y = 1, x = 1)
                              
                              # 2.1 Female --------------------------------------------------------------
                              
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,Male,Female,Transgender)%>%
                                #dplyr::filter(!Age %in% c('Total'))%>%
                                dplyr::select(-c(Age))%>%
                                dplyr::group_by(Year,Codes)%>%
                                dplyr:: summarize(Male=sum(Male),
                                                  Female=sum(Female),
                                                  Transgender=sum(Transgender))%>%
                                dplyr::mutate(Total=Male+Female+Transgender)%>%
                                dplyr::mutate(female=(Female/Total)*100,
                                              male=(Male/Total)*100,
                                              transgender=(Transgender/Total)*100)%>%
                                dplyr::filter(Codes=='XK')%>%
                                dplyr::select(-c(Male,Female,Transgender,Total))%>%
                                data.table()
                              
                              
                              df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                              df$Year<-as.character(df$Year)
                              df<-melt(df)
                              df$MarriParticipant<-as.factor(df$MarriParticipant)
                              df$color <- factor(df$variable, labels = c("goldenrod","#2ca02c","#9870AB"))
                              
                              
                              
                              Chart1<-plot_ly(df, x = ~Year, y = ~value,
                                              color = ~variable, colors = ~as.character(color), type = "bar",
                                              legendgroup = "1"
                              )|>
                                layout(barmode = "stack",showlegend = TRUE, legend = legend, 
                                       title = '<b>Percentage of VT/PVT disaggregated by gender<b>',font = t,
                                       xaxis = list(title = ' '),
                                       yaxis = list(title = 'Percentage')
                                )
                              
                              
                              # 2.2 Children ----------------------------------------------------------------
                              
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,TotalTHB)%>%
                                dplyr::filter(!Age %in% c('Unknown'))
                              
                              df_total<-df%>%
                                dplyr::select(Age,Codes,Year,TotalTHB)%>%
                                dplyr::filter(Age=='Total'&Codes=='XK')%>%
                                dplyr::select(-c(Age))%>%
                                dplyr::rename('total'='TotalTHB')
                              
                              df_total1<-left_join(df,df_total,by = c('Codes','Year'))
                              
                              df_total2<-df_total1%>%
                                dplyr::filter(!Age %in% c('Total')&Codes=='XK')%>%
                                dplyr::mutate(share=(TotalTHB/total)*100)
                              df_total2$TotalTHB<-NULL
                              df_total2$total<-NULL
                              
                              
                              df_total2<-left_join(df_total2,MARRI_Partipants_codes,by = c("Codes"="Codes"))
                              df_total2$Year<-as.character(df_total2$Year)
                              df_total3<-melt(df_total2)
                              df_total3$MarriParticipant<-as.factor(df_total3$MarriParticipant)
                              
                              df_total3$color <- factor(df_total3$Age, labels = c("orange","forestgreen"))
                              df_total3[is.na(df_total3)] <- 0
                              
                              df_total3$variable<-NULL
                              df_total3$codes<-NULL
                              
                              #'#bcbd22', "#66CC99",
                              
                              df_total4 <- df_total3 %>%
                                dplyr::select(-color)  %>%
                                tidyr::pivot_wider(names_from = Age, values_from = value)
                              
                              df_total4<-df_total4%>%arrange((MarriParticipant))
                              
                              
                              Chart2<-plot_ly(df_total4,
                                              x = ~ Year, y = ~`Children(Under18)`,
                                              type = "bar", marker = list(
                                                color = "#1f77b4"
                                              ), name = "Children",legendgroup = "2") %>%
                                add_bars(y = ~`Adults(18+)`, marker = list(
                                  color = "#66CC99"
                                ), name = "Adults")  %>%
                                layout(showlegend = TRUE, legend = legend,
                                       barmode = "stack",
                                       title = "<b>Percentage of children identified as VT/PVT<b>",font = t,
                                       xaxis = list(title = ""),
                                       yaxis = list(title = "Percentage")) #,
                              
                              # 2.3 Data on the cases of trafficking separated with forms of trafficking-------------------------
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,Type_SexualExploitation,Type_ForcedBegging,TypeForced_marriage,Type_LabourExploitation,
                                              Type_ForcedCriminality,Type_TraffickingWithBabies,Type_TraffickingWithOrgans,Type_Pornography,Type_MixedForms,Type_OtherForms)%>%
                                dplyr::filter(Age=="Total")%>%
                                dplyr::select(-c(Age))%>%
                                dplyr::group_by(Year,Codes)%>%
                                dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>%
                                data.table()%>%
                                rename('Forced marriage'='TypeForced_marriage',
                                       'Sexual Exploitation'='Type_SexualExploitation',
                                       'Forced Begging'='Type_ForcedBegging',
                                       'Labour Exploitation'='Type_LabourExploitation',
                                       'Forced Criminality'='Type_ForcedCriminality',
                                       'Trafficking with Babies'='Type_TraffickingWithBabies',
                                       'Trafficking with Organs'='Type_TraffickingWithOrgans',
                                       'Pornography'='Type_Pornography',
                                       'Mixed Forms'='Type_MixedForms',
                                       'Other Forms'='Type_OtherForms',
                                )
                              
                              
                              df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))#%>%
                              #dplyr::select(MarriParticipant,Year,IdentifiedVictims)
                              
                              df$Year<-as.character(df$Year)
                              
                              df$MarriParticipant<-as.factor(df$MarriParticipant)
                              
                              df$Codes<-NULL
                              
                              df_t<-melt(df)%>%
                                dplyr::filter(MarriParticipant=="Albania")
                              
                              df_t$color <- factor(df_t$variable, labels = c('#d62728', '#ff7f0e','#2ca02c','#bcbd22', "#66CC99",'#9370DB','#8c564b',"#CC6666","#9870AB",'#1f77b4'))
                              
                              Chart3<-plot_ly(df_t, x = ~Year,y = ~value,
                                              
                                              color = ~variable, colors = ~as.character(color), type = "bar")%>%
                                layout(showlegend = TRUE, legend = legend,
                                       barmode = "stack",
                                       title = '<b>Data on forms of human trafficking</b>',font = t,
                                       xaxis = list(title = ''),
                                       yaxis = list(title = ' '))
                              
                              # 2.4 Domestic and foreign victims of the VT/PVT for the period 2018-2022, by MARRI Participant------------------
                              
                              
                              df<-data_tbl%>%
                                dplyr::select(Year,Age,Codes,
                                              NationalityandCountryOfExplotation_DomesticCitizensExploitedAbroad,
                                              NationalityandCountryOfExplotation_ForeignCitizensInternallyExploited,
                                              NationalityandCountryOfExplotation_InternalTrafficking)%>%
                                dplyr::filter(Age=="Total"&Codes=='XK')%>%
                                dplyr::select(-c(Age))%>%
                                dplyr::group_by(Year,Codes)%>%
                                dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>%
                                data.table()%>%
                                rename('Domestic victums exploited abroad'='NationalityandCountryOfExplotation_DomesticCitizensExploitedAbroad',
                                       'Foreign victums internally exploited'='NationalityandCountryOfExplotation_ForeignCitizensInternallyExploited',
                                       'Domestic victums exploited internally'='NationalityandCountryOfExplotation_InternalTrafficking')
                              
                              
                              df<-left_join(df,MARRI_Partipants_codes,by = c("Codes"="Codes"))#%>%
                              #dplyr::select(MarriParticipant,Year,IdentifiedVictims)
                              
                              df$Year<-as.character(df$Year)
                              
                              df$MarriParticipant<-as.factor(df$MarriParticipant)
                              
                              
                              df$Codes<-NULL
                              
                              df_t<-melt(df)
                              
                              df_t$color <- factor(df_t$variable, labels = c('#d62728',"#1f77b4", '#ff7f0e'))
                              
                              
                              Chart4<-plot_ly(df_t, x = ~Year, y = ~value,
                                              color = ~variable, colors = ~as.character(color), type = "bar")%>%
                                layout(showlegend = TRUE, legend = legend,
                                       barmode = "stack",
                                       title = '<b>Domestic and foreign victums of the VT/PVT<b>',font = t,
                                       xaxis = list(title = ''),
                                       yaxis = list(title = ' ')) #,
                              
                              # Facet 
                              
                              htmltools::div(
                                crosstalk::bscols(
                                  Chart1, Chart2
                                ),
                                crosstalk::bscols(
                                  Chart3, Chart4
                                )
                              ) |>
                                htmltools::browsable()
                              
                              
                              
                             
                              
  #'* This designation is without prejudice to positions on status, and is in line with UNSCR 1244(1999) and the ICJ Opinion on the Kosovo declaration of independence.'
                              
                              
                              