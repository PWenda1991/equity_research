library(shiny)
library(readr)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(readxl)
library(plotly)
library(tidyr)
library(ggrepel)
library(scales)
library(DT)
library(prophet)
library(lubridate)
library(forecast)
library(zoo)
library(dygraphs)
library(purrr)
library(mice)
library(reactable)
library(kableExtra)
library(mgcv)
library(tidyverse)
library(lmvar)
library(psych) 
library(car)
library(randomForest)
library(shinyWidgets)
library(mailtoR)
library(reshape2)
interval_value_formatter <- "function(num, opts, seriesName, g, row, col) {
  value = g.getValue(row, col);
  if(value[0] != value[2]) {
    lower = Dygraph.numberValueFormatter(value[0], opts);
    upper = Dygraph.numberValueFormatter(value[2], opts);
    return '[' + lower + ', ' + upper + ']';
  } else {
    return Dygraph.numberValueFormatter(num, opts);
  }
}"


set.seed(123)


Market_data_frame <- read_csv("Market_data_frame.csv")
Date = rep(c("30-12-2015", "30-12-2016", "30-12-2017", "30-12-2018", "30-12-2019", "30-12-2020", "30-12-2021"),9)
Market_data_frame$Date <- Date
Market_data_frame$Date <- as.Date(Market_data_frame$Date,format = "%d-%m-%Y")
Market_data_frame$Company_name <- as.factor(Market_data_frame$Company_name)
names(Market_data_frame)[names(Market_data_frame) == 'EBIDTA'] <- 'EBIT'
# Market_data_frame <- Market_data_frame %>%
# mutate(Market_cap_class = case_when(
#         Market_cap <= 250000000 ~ "Micro",
#         Market_cap <= 500000000 & Market_cap > 250000000 ~ "Small",
#         Market_cap > 500000000 & Market_cap <=1000000000 ~ "Med",
#         Market_cap > 1000000000  ~ "Large"
#     ))
Market_data_frame <- Market_data_frame[8:63,]
Market_data_frame_cap <- Market_data_frame
# drop <- c("Company_name")
# Market_data_frame_cap = Market_data_frame_cap[,!(names(Market_data_frame_cap) %in% drop)]
Market_data_frame_cap <- Market_data_frame_cap[, -c(2,3,7,9,10,13)]
Market_data_frame_total <- Market_data_frame_cap %>%
    group_by(Date) %>%
    summarise_if(is.numeric, funs(sum, mean))
Market_data_frame_total[,2:27] <- round(Market_data_frame_total[,2:27], 0)

Market_data_frame_total <- Market_data_frame_total %>% gather("Variable", "Value", 2:27)
Market_data_frame_total$Variable <- as.factor(Market_data_frame_total$Variable)
# Breakdown by company
Market_data_frame_break <- Market_data_frame_cap %>%
    group_by(Date, Company_name)
Market_data_frame_break <- Market_data_frame_break %>% gather("Variable", "Value", c(-Date,-Company_name))
Market_data_frame_break$Variable <- as.factor(Market_data_frame_break$Variable)

Market_data_frame_capik <- Market_data_frame_cap[, -15]
Market_data_frame_capik <- aggregate(Market_data_frame_capik[,-1], by=Market_data_frame_capik["Date"], sum)
# Calculation of metrics 

Market_data_frame_Market_Metrics = Market_data_frame %>%
    mutate(Asset_Turnover = round(Revenue / Total_Assets, 2), Return_on_Sales = round(((Net_Income / Revenue) * 100), 2),
           Return_on_Assets = round(Asset_Turnover * Return_on_Sales,2), Financial_Leverage = round((Total_Assets / Equity),2),
           Return_on_Equity = round(Asset_Turnover * Return_on_Sales * Financial_Leverage, 2), Sales_to_market_cap = round(Revenue / Market_cap, 2),
           Market_cap_to_book_value = round(Market_cap / BookValue, 2), Total_Assets_to_Total_liabilities = round(Total_Assets / Total_Liabilities, 2),
           Working_capital_to_current_assets = round(Working_capital / Current_Asset_Value, 2),
           Total_Debt_to_EBIT = round(Total_Liabilities / EBIT, 2), Interest_Coverage = round(EBIT / Interest,2),
           Assets_to_Equity = round(Total_Assets / Equity, 2), Dividend_yield = round(Dividends / Market_cap,2))
Market_data_frame_Market_Metrics <- Market_data_frame_Market_Metrics[,c(1,21:34)]
Market_data_frame_Market_Metrics <- Market_data_frame_Market_Metrics %>% gather("Variable", "Value", c(-Date,-Company_name))
Market_data_frame_Market_Metrics$Variable <- as.factor(Market_data_frame_Market_Metrics$Variable)
Market_data_frame_Market_Metrics$Company_name <- as.factor(Market_data_frame_Market_Metrics$Company_name)

# List of companies market cap (share price analysis) 
ALUMETAL_COMPLETE <- read_csv("ALUMETAL_COMPLETE.csv")
Alumetal <- ALUMETAL_COMPLETE[,1:4]
RevkaAlu <- ALUMETAL_COMPLETE[,1:6]
BOWIM_BOWIM_COMPLETE <- read_csv("BOWIM\\BOWIM_COMPLETE.csv")
Bowim <- BOWIM_BOWIM_COMPLETE[,1:4]
RevkaBow <- BOWIM_BOWIM_COMPLETE[,1:6]
COGNOR_COMPLETE <- read_csv("COGNOR_COMPLETE.csv")
Cognor <- COGNOR_COMPLETE[,1:4]
RevkaCognor<- COGNOR_COMPLETE[,1:6]
BOWIM_DROZAPOL_COMPLETE <- read_csv("BOWIM\\DROZAPOL_COMPLETE.csv")
Drozapol <- BOWIM_DROZAPOL_COMPLETE[,1:4]
RevkaDrozapol<- BOWIM_DROZAPOL_COMPLETE[,1:6]
KETY_COMPLETE <- read_csv("KETY_COMPLETE.csv")
Kety <- KETY_COMPLETE[,1:4]
RevkaKety<- KETY_COMPLETE[,1:6]
MANGATA_COMPLETE <- read_csv("MANGATA_COMPLETE.csv")
Mangata <- MANGATA_COMPLETE[,1:4]
RevkaMangata<- MANGATA_COMPLETE[,1:6]
STALPRODUKT_COMPLETE <- read_csv("STALPRODUKT_COMPLETE.csv")
Stal_produkt <- STALPRODUKT_COMPLETE[,1:4]
RevkaStal_produkt<- STALPRODUKT_COMPLETE[,1:6]
STALPROFILSA_COMPLETE <- read_csv("STALPROFILSA_COMPLETE.csv")
Stal_profil <- STALPROFILSA_COMPLETE[,1:4]
RevkaStal_profil<- STALPROFILSA_COMPLETE[,1:6]
Share_price_tot <- list(Alumetal, Bowim, Cognor, Drozapol, Kety, Mangata, Stal_produkt, Stal_profil)
Share_price_totenhamon <- list(RevkaAlu, RevkaBow, RevkaCognor, RevkaDrozapol, RevkaKety, RevkaMangata, RevkaStal_produkt, RevkaStal_profil)
names(Share_price_tot) <- c("Alumetal", "Bowim", "Cognor", "Drozapol", "Kety", "Mangata", "Stal_produkt", "Stal_profil")
names(Share_price_totenhamon) <- c("Alumetal", "Bowim", "Cognor", "Drozapol", "Kety", "Mangata", "Stal_produkt", "Stal_profil")
Market_data_frame_total_market_cap <- Market_data_frame_total[1:7,c(1,3)]



Ratio <- lapply(Share_price_tot, function(x) x[,4] / Market_data_frame_total_market_cap$Value)



Share_price_tot2 <- mapply(cbind, Share_price_tot, Ratio, SIMPLIFY=F)

ChangeInRatio <- sapply(Share_price_tot2, function(x) diff(x[,5]))
Share_price_tot1 <- sapply(Share_price_tot2, "[", 7, 5)
Share_price_tot1 <- data.frame(Share_price_tot1)
Share_price_tot1 <- t(Share_price_tot1)
ChangeInRatio <- as.data.frame(ChangeInRatio)
average <- colMeans(ChangeInRatio)
ChangeInRatio[nrow(ChangeInRatio) + 1,] <- average

Cap_ratio22 <- Share_price_tot1 + average
Cap_ratio23 <- Cap_ratio22 + average
Cap_ratio24 <- Cap_ratio23 + average
Cap_ratio25 <- Cap_ratio24 + average
Cap_ratio26 <- Cap_ratio25 + average
Cap_ratio27 <- Cap_ratio26 + average
Cap_ratio28 <- Cap_ratio27 + average

Companies_cap_ratio <- rbind(Cap_ratio22, Cap_ratio23, Cap_ratio24, Cap_ratio25, Cap_ratio26, Cap_ratio27, Cap_ratio28) 

Companies_cap_ratio <- as.data.frame(Companies_cap_ratio)

rownames(ChangeInRatio) <- c("2016", "2017", "2018", "2019", "2020", "2021", "Average")

Shares_total_2 <-  sapply(Share_price_tot2, "[", 7, 3)
Shares_total_2 <- t(Shares_total_2)
Shares_total_2 <- as.data.frame(Shares_total_2)

Shares_pricing_past<-  sapply(Share_price_tot2, "[", 1:7, 2)
Shares_pricing_past <- as.data.frame(Shares_pricing_past)
# Table 

Market_data_frame_financial_strength = Market_data_frame %>%
    mutate(Asset_Turnover = round(Revenue / Total_Assets, 2), Return_on_Sales = round(((Net_Income / Revenue) * 100), 2),
           Return_on_Assets = round(Asset_Turnover * Return_on_Sales,2), Financial_Leverage = round((Total_Assets / Equity),2),
           Return_on_Equity = round(Asset_Turnover * Return_on_Sales * Financial_Leverage, 2), Sales_to_market_cap = round(Revenue / Market_cap, 2),
           Market_cap_to_book_value = round(Market_cap / BookValue, 2), Total_Assets_to_Total_liabilities = round(Total_Assets / Total_Liabilities, 2),
           Working_capital_to_current_assets = round(Working_capital / Current_Asset_Value, 2),
           Total_Debt_to_EBIT = round(Total_Liabilities / EBIT, 2), Interest_Coverage = round(EBIT / Interest,2),
           Assets_to_Equity = round(Total_Assets / Equity, 2), Dividend_yield = round(Dividends / Market_cap,2))

Strength20 <- Market_data_frame_financial_strength[,c(1,21:34)] 
Strength20 = Strength20 %>% filter(Strength20$Date > '2020-12-30')
Strength20 <- Strength20[,-c(1, 14)]



Average_Results_Fin <- aggregate(Market_data_frame_financial_strength[,22:34],list(Market_data_frame_financial_strength$Company_name), FUN=mean)
Average_Results_Fin <- as.data.frame(Average_Results_Fin)
Average_Results_Fin[,2:14] <- round(Average_Results_Fin[,2:14], 2)
rownames(Average_Results_Fin) <- Average_Results_Fin$Group.1
Average_Results_Fin <- Average_Results_Fin[,-1]
Average_Results_Fin <- as.data.frame(Average_Results_Fin)
Average_Results_Fin <- t(Average_Results_Fin)
Average_Results_Fin <- as.data.frame(Average_Results_Fin)
Average <- apply(Average_Results_Fin, 1, mean)
Average_Results_Fin$Median = apply(Average_Results_Fin,1,median)
Average_Results_Fin$Median <- round(Average_Results_Fin$Median, 2)
Average_Results_Fin$Average <- round(Average, 2)
Average_Results_Fin <- as.data.frame(Average_Results_Fin)
Average_Results_Fin <- Average_Results_Fin[-c(12),]

Dividendpay <- Market_data_frame %>% mutate(Dividend_payout_ratio = Market_data_frame$Dividends / Market_data_frame$Net_Income)
AverageDividendPayout <- aggregate(Dividendpay$Dividend_payout_ratio,list(Dividendpay$Company_name), FUN=mean)
Dividendpay1 <- Market_data_frame %>% mutate(Dividend_per_share = Market_data_frame$Dividends / Market_data_frame$Shares_outstanding)


regr <- randomForest(Share_Price ~ Dividend_per_share + Earned_per_share , ntree=100, data = Dividendpay1)
fit.sum <- summary(regr)
test <- cbind(Dividendpay1$Dividend_per_share,Dividendpay1$Earned_per_share)
colnames(test) <- c("Dividend_per_share", "Earned_per_share")
predictions = predict(regr, test)

R2 <- caret::postResample(predictions , Dividendpay1$Share_Price)['Rsquared']
MAPE <- mean(abs((Dividendpay1$Share_Price-predictions)/Dividendpay1$Share_Price)) * 100
MSE <- mean((Dividendpay1$Share_Price-predictions)^2)
RMSE <- sqrt(mean((Dividendpay1$Share_Price-predictions)^2))
Validation_metrics <- rbind.data.frame(R2, MAPE, MSE, RMSE)
Dividendpay1$Company_name <- as.character(Dividendpay1$Company_name)
Validation_metrics1 <- data_frame(Dividendpay1$Date,Dividendpay1$Share_Price, predictions, Dividendpay1$Company_name)
colnames(Validation_metrics) <- c("Validation")
rownames(Validation_metrics) <- c("R2", "MAPE", "MSE", "RMSE")
colnames(Validation_metrics1) <- c("Date", "Share Price", "Predicted", "Entity")
# Market_data_frame_Market_Metrics_Table <- Market_data_frame_Market_Metrics[,-(1)]
# Market_data_frame_Market_Metrics_Table <- t(Market_data_frame_Market_Metrics_Table)
Average_Results_Fin88 <- as.data.frame(Average_Results_Fin)
ROS <- Average_Results_Fin88[2,]

Ratio <- lapply(Share_price_totenhamon, function(x) x[,5] / Market_data_frame_capik$Revenue)
Ratio <- sapply(Ratio, colMeans)

Company_name<- c("Alumetal", "Bowim", "Cognor", "Drozapol", "Kety", "Mangata", 
                 "Stal_produkt", "Stal_profil")
Ratio <- cbind.data.frame(Ratio, Company_name)
Ratio <- as.data.frame(Ratio)
rownames(Ratio) <- NULL

Market_data_frame_capik11 <- Market_data_frame_capik$Revenue
Market_data_frame_capik22 <- ts(Market_data_frame_capik11, start = 2015, end = 2021)
Market_data_frame_capik22 <- window(Market_data_frame_capik22, start = 2015, end = 2021)
fit.Market_data_frame_capik22 <- tslm(Market_data_frame_capik22 ~ trend)
Rka10 <- forecast::forecast(fit.Market_data_frame_capik22, h = 7)
Rka10 <- as.data.frame(Rka10)

ROS <- t(ROS)

ROS <- ROS[1:8,]
Company_name <- c("Alumetal","Bowim","Cognor","Drozapol","Kety","Mangata","Stal_produkt","Stal_profil")
ROS <- data.frame(ROS)
ROS$Company_name <- Company_name
ROS$ROS = ROS$ROS / 100



# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("flatly"),
                 "Metal Market Poland - (GPW)  Warsaw Stock Exchange Listed Companies (8 out of 16)",
                 tabPanel("Intro & Disclaimer", 
                          h3(strong("Author:"), "Piotr Wenda", align = "center"),
                          h3(strong("Steel Market Research - Warsaw Stock Exchange Listed Companies"),align = "center"),
                          h3(strong("Published:"), "19 September 2022", align = "center"), br(), 
                          br(), 
                          br(), 
                          h3(strong("Disclaimer:"), align = "left"),
                          tags$div(tags$p("The information in this document has been created and is provided for educational and informational purposes only. It does not constitute any offer, recommendation or advice to any person to enter into any transaction or adopt any hedging, trading or investment strategy. Readers of this document should seek advice regarding the appropriateness of investing in any securities, financial instruments or investment strategies referred to on this document and should understand that statements regarding future prospects may not be realised. Opinions, analyses and estimates are subject to change and to readers’ own interpretation. I accept no liability and will not be liable for any loss or damage arising directly or indirectly from your use of this document." 
                          ))),
                 tabPanel("Analyses",
                          sidebarLayout(position = "left",
                                        sidebarPanel(
                                            conditionalPanel(condition = "input.tabs1==1",
                                                             h5("Market Data (sum & mean per year for pre-selected variable)"), br(),
                                                             selectizeInput("select",
                                                                            "Select market variables to be compared",
                                                                            choices = unique(Market_data_frame_total$Variable),
                                                                            multiple = TRUE,
                                                                            selected = "Market_cap_sum"), br(), 
                                                             selectizeInput("select8",
                                                                            "Select market value to be forecasted",
                                                                            choices = colnames(Market_data_frame_capik),
                                                                            multiple = F,
                                                                            selected = "Market_cap"), 
                                                             h5("Market Data - companies' breakdown for pre-selected variable"), br(),
                                                             selectizeInput("select2",
                                                                            "Select variable",
                                                                            choices = unique(Market_data_frame_break$Variable),
                                                                            multiple = FALSE,
                                                                            selected = "Revenue"),
                                                             
                                                             br(),
                                                             selectizeInput("select3",
                                                                            "Select variable",
                                                                            choices = unique(Market_data_frame_break$Variable),
                                                                            multiple = FALSE,
                                                                            selected = "Market_cap")),
                                            conditionalPanel(condition = "input.tabs1==2", 
                                                             h3("Pick company to see share price forecast"), br(),
                                                             selectizeInput("select15",
                                                                            "Company's name",
                                                                            choices = colnames(Companies_cap_ratio),
                                                                            multiple = F,
                                                                            selected = "Alumetal"), br()), 
                                            conditionalPanel(condition = "input.tabs1==3", 
                                                             h3("Select metrics to be compared"), br(),
                                                             selectizeInput("select20",
                                                                            "Indicator1",
                                                                            choices = unique(Market_data_frame_Market_Metrics$Variable),
                                                                            multiple = F,
                                                                            selected = "Asset_Turnover"), br(),
                                                             selectizeInput("select21",
                                                                            "Indicator2",
                                                                            choices = unique(Market_data_frame_Market_Metrics$Variable),
                                                                            multiple = F,
                                                                            selected = "Financial_Leverage"), br()),
                                            conditionalPanel(condition = "input.tabs1==5", 
                                                             h3("Random Forest Classifier", align = "center"), 
                                                             h5(strong("Relationship between share price, dividends and earnings per share"), align = "center"), 
                                                             tags$div(tags$p(
                                                                 "Strong correlation was found between Dividends, and Earnings per share, both, being statistically significant in relation to Share Price (p < 0.05), with explained variability of 85% via linear regression model.
                                                                  However, the linear and polynomial classifiers lent themselves inconsiderable deterministic powers, as exhibited. Essentially, they did not capture all data points, and in such, the model was flawed, notwithstanding the confirmed high correlation.
Thus, a random forest classification regression was adopted to enhance the accuracy, given its future use of forecasting  share prices. The results proven to be higher and more accurate, shown via exhibit 2, which showcases how closely the predicted values followed the actuals, thereby improving Adjusted R-squared to 87% and R-squared to 97%. 
                                                                  Root-mean-square deviation of 32.5 corroborates the findings, despite relatively high Mean-square-error of 1059.54 (MSE). The relatively high MSE results from dividends being not paid at all throughout certain periods, and/or high variability in the dividends' payout ratio. 
                                                                  Another way of looking at the model features and validity, is to compare the actuals and predicted values to understand how the classifier rectifies the over/undervaluations dominant in the stock market. Notably, it normalised the values in those enterprises, whose share price was less stable than those of their peers, viz., Bowim, Alumetal and Kety."
                                                                
                                            ))),
                                            conditionalPanel(condition = "input.tabs1==6", 
                                                             h3("Select share price to be evaluated", align="center"), 
                                                             selectizeInput("select50",
                                                                            "Forecasted company",
                                                                            choices = unique(Ratio$Company_name),
                                                                            multiple = F,
                                                                            selected = "Bowim"), 
                                                             shinyWidgets::autonumericInput(
                                                                 inputId = "markcap", 
                                                                 label = "Specify selected company's revenue share to total revenue cap ratio", 
                                                                 value = 9, 
                                                                 currencySymbol = "%",
                                                                 currencySymbolPlacement = "p",
                                                                 decimalPlaces = 2,
                                                                 digitGroupSeparator = ",",
                                                                 decimalCharacter = "."
                                                             ),
                                                             shinyWidgets::autonumericInput(
                                                                 inputId = "Retonsales", 
                                                                 label = "Specify selected company's Return on Sales ratio", 
                                                                 value = 1.9, 
                                                                 currencySymbol = "%",
                                                                 currencySymbolPlacement = "p",
                                                                 decimalPlaces = 2,
                                                                 digitGroupSeparator = ",",
                                                                 decimalCharacter = "."
                                                             ),
                                                             shinyWidgets::autonumericInput(
                                                                 inputId = "dividendpay", 
                                                                 label = "Specify selected company's Dividend Payout ratio", 
                                                                 value = 6.2, 
                                                                 currencySymbol = "%",
                                                                 currencySymbolPlacement = "p",
                                                                 decimalPlaces = 2,
                                                                 digitGroupSeparator = ",",
                                                                 decimalCharacter = "."
                                                             ),
                                                             h4("Explanatory notes", align = "center"), br(),
                                                             h5("Revenue, Earnings & Dividend Forecasts", align = "center"),
                                                             tags$div(tags$p("The model encompasses multiple key components, one of which, is the previous time-series regression model and revenue market forecast, carried out under the tab", strong( 'Market Metrics.'), "The forecasted revenue streams determines the company's sales by applying the enterprise's revenue to the total market revenue ratio. Interactivity and dynamicity of the shiny's dashboard streamlines the sensitivity analyses, allowing for a change of underlying conditions contingent upon selection. The table", strong('Average ratios per company'), "provides historical records that are used to administer an average and/or close to average of given ratios. In this way,  normalizing the predicted results and keeping them realistic and less prone to manipulation. Similarly, the earnings and dividends per shares, are obtained via historical profit margin and dividend payout ratios, which ingeniously combines both statistical and financial assumptions, bolstering the model's fundamentals."
                                                             )),
                                                             h5("Share Price Forecast", align = "center"),
                                                             tags$div(tags$p("When all the underlying variables are selected, the random forest regressor is applied to predict share price via dividends and earnings per share factors, as portrayed previously. 
                                                                             Undoubtedly, one of the best qualities of the tested model is that it not only captures the market movement, in general but more importantly, it consists of lower and upper confidence levels, 
                                                                             corresponding to those of the formerly computed time-series revenue regression estimates, whereby containing hypothetical recession and thus, probable contractions directly applicable to share prices' fluctuations. 
                                                                             "
                                                             )),
                                                             width = 3)),
                                        
                                        mainPanel(
                                            tabsetPanel(id="tabs1", type = c("pills"),
                                                        tabPanel("Market Metrics", value=1,
                                                                 fluidRow(splitLayout(cellWidths = c("55%", "45%"),
                                                                                      plotOutput("plot"), dygraphOutput("plot4")), br(),
                                                                          splitLayout(cellWidths = c("55%", "45%"),
                                                                                      plotOutput("plot2"), 
                                                                                      plotOutput("plot3")),
                                                                          h5(strong("Forecast accuracy metrics"), align  = "center"),
                                                                          DTOutput("text4")),
                                                                 
                                                                 h3("Abstract", align = "center"), 
                                                                 h5(strong("Market capitalisation vs Revenue, Net Income, Total Liabilities and Book Value"), align = "left"), 
                                                                 tags$div(tags$p(
                                                                     "Comparison between market capitalisation and sales permits to understand the sentiment investors bear 
                                      when valuing equities and reveals a positive link between increase in sales, earnings and market cap.
                                      However, it also showcases a prevailing pessimism upon the COVID-19 outbreak and how uncertainty thereof resulted in 
                                      a significant decline in shares' prices, notwithstanding the revenue's growth. Further market correction confirms this view. 
                                      Not all the companies, nevertheless, have enjoyed such ascription, which creates an opportunity to purchase certain securities
                                      before the investors' sentiment shifts otherwise. 
                                      For instance,", tags$b("Bowim"), 
                                                                     "over the studied period, managed to increase its sales/net income share  (revenue/net income to total market revenue/net income ratio) by
                                      4.1% and 7.4%, respectively. Whilst its market cap ratio only advanced by 1.1%.
                                      Similarly,", tags$b("Stal Profil"), "expanded its revenue/net income to total market ratio over
                                      2% and 8.7%, accordingly. Yet, Its market cap ratio declined by nearly 1%.
                                      Investors seem to unreasonably oversubscribe to", tags$b("Kety"), "which like other entities in the market tripled/quadrupled its revenue,
                                      with net revenue/net income ratio decrease of 3.6% and 11%, yet, having its market cap ratio advancing by 15.6%.", "High dividends of Kety, are widely recognisable and are a major driver behind company's phenomena.",
                                                                     tags$b("Stal Produkt,"), "although, with the steady revenue growth, had lost its sales/income market share to competitors, reflected
                                      via ratios decline of 7.2% and 17.6%, accordingly, which markedly affected its market cap ratio that depreciated by 15%.
                                      Interestingly, upon the Covid-19 breakdown, the liquidation value of Stal Produkt was about PLN150, whereas, the company was traded at PLN105 (as of 13 March 2020).
                                      Equally, companies with book values relatively high compared to price had been subjected to unfavorable sentiment, 
                                      pinpointing investors paid little to no attention in regard to debt that can be seen via total liabilities' breakdown.
                                      "), 
                                                                     h5(strong("Financial Metrics and Forecast"), align = "left"), 
                                                                     tags$div(tags$p("Market forecast of the underlying variables, as well as other plots, represent the consolidated (summed factors per year) for the 9 out of 15 listed Steel Market's companies.
                                      The foregoing model applies multiple linear time-series regression to predict future values. 
                                      The accuracy metrics are far from desirable, however, can be considered optimal, with the Mean Absolute Percentage Error (MAPE) of 16%, indicating average variability between predicted and actual values,
                                      which in data with large numbers represents considerable variations that are exhibited via medium of confidence intervals. Mean Absolute Scaled Error (MASE)
                                      implies model's superiority over naive forecast, with 77.4% score, while, Mean Percentage Error (MPE) remains consistently low (-3.1%), suggesting insignificant differences between forecast and actual values.",
                                                                                     br(), 
                                                                                     "Essential to interpreting the market behaviour correctly and estimating future fluctuations, is to account for current energy crisis that leans towards
                                                      temporary market decline, both in revenues and market cap, with the first one stemming from soaring energy costs that would lead to reduced production capacity, culminating higher steel prices due to supply being lower than demand. 
                                                      We shall, therefore, expect these market conditions to prevail, at least, for the next 2-3 years, before it stabilizes and adapts to a new reality and/or until the crisis fades away. 
                                                      Therefore, 80% and 95% confidence interval predictions shall constitute a rough approximation/fluctuations that will occur in the market and impact
                                                      shares' valuation. As mentioned earlier, the market is particularly susceptible to uncertainties as shown throughout COVID-19, thus, in the short run, market prices of securities will be exposed to relatively high fluctuations,
                                                      handsomely rewarding those equipped in patience and investing at the right time."
                                                                                     
                                                                     )),
                                                                     
                                                                     
                                                                 )),
                                                        tabPanel("Companies' Financial Metrics", value=2, 
                                                                 dygraphOutput("plot15"), br(), 
                                                                 h5(strong("Change in enterprise market cap to total market cap ratio"), align = "center"),
                                                                 DTOutput("marketcapratio"),     
                                                                 h5(strong("Predicted ratios"), align = "center"),
                                                                 DTOutput("marketcapratio2"), br(),
                                                                 h5(strong("Predicted stocks' yearly returns"), align = "center"),
                                                                 reactableOutput("sharepr"), br(), 
                                                                 h5(strong("Predicted market's yearly returns"), align = "center"),
                                                                 DTOutput("sharepr2"),
                                                                 h3(strong("Change in Investors' perception"), align = "center"),
                                                                 tags$div(tags$p(
                                                                     "The above prediction of stocks' prices is based on the market forecast set out under the tab 'Market Metrics' with share prices deriving from individual securities market cap to total market cap ratios, compounded by their average change over the scrutinised period. 
I carried out this exercise to explain and substantiate the thesis of a shift in investors' sentiment in forthcoming years that would 
                                               turn the odds, thereby appreciating the value of the securities undervalued in the past. 
                                               Let's assume that investors do not change their behavior, and the securities continue to be valued according to their trend, i.e., market cap ratio compounded by the average change thereof. 
                                             In such, a striking example is evidenced via", strong("Stal Produkt"), "whose value consistently with the sentiment would have depreciated below 0, which is clearly irrational. 
                                               Shrewd investors would rather exercise the liquidation value that ranges between PLN150-170 in the event of price deteriorating to ridiculuously low levels.",
                                                                     "Once the company starts disbursing higher dividends, as per the case of Kety, its price is likely to go back around its book-value thresholds of PLN500 upon the market condition improvement/restoration. 
                                               Hence, the premise that securities' fluctuations will largely depend on dividend payouts, whose retention, although, justified from a conservative business standpoint, would lower their value throughout the general market decline that seems to be imminent, considering 
                                               the energy crisis that infested Europe. Stocks' yearly returns with green colour indicate forecasted values above the market mean return prognosis, and in red, below."
                                                                     
                                                                     
                                                                 ))
                                                        ), 
                                                        tabPanel("Financial Strength", value=3, 
                                                                 fluidRow(splitLayout(cellWidths = c("55%", "45%"),
                                                                                      plotlyOutput("MetricsFin"), 
                                                                                      plotlyOutput("MetricsFin2")
                                                                 )), 
                                                                 fluidRow( 
                                                                     h5(strong("Financial Metrics 2015-2021 Average Ratios"), align = "center"),
                                                                     DTOutput("finmetrics10"), 
                                                                     h5(strong("Financial Metrics 2021"), align = "center"),
                                                                     DTOutput("finmetrics11"),
                                                                     h3(strong("Debt as a key driver to growth"), align = "center"),
                                                                     tags$div(tags$p(
                                                                         "Special attention is paid to debt to distinguish how companies differently use it to drive their growth/profitability. 
                                                   Bowim and Cognor are primary examples of what we should call conservative management in utilising borrowings to augment market share and maximise returns on equity while reducing financial leverage through the steel prices' upturns. 
                                                   Distinct discrepancies in debt-related ratios between averaged metrics and those of 2021 help to elucidate this phenomenon. 
                                                   Drozapol, although less efficiently, has also leveraged these dynamics in its favor, however, to a much lesser extent (mean ROA, -0.11). 
                                                   Relatively higher average ROA compared to more established businesses such as Stal Produkt, and Kety implies that the smaller cap companies tend to rely more on external financing to support their expansion and profitability. 
                                                   When the products' demand hikes, debt is utilised to achieve economies of scale throughout early inventory build-up, so that the profit margin increases proportionately with the material costs, whereas provisioning tax shield, respectively.
Long-standing companies, that had already passed the stage of growth are more frequently balancing debt with internal financing to replenish stocks, while not being exposed to higher interest on the underlying loans. "), br(), 
                                                                         h3(strong("Classification"), align = "center"), 
                                                                         h5(strong("Dividend Stocks"), align = "center"),
                                                                         h6(strong("Kety"), align = "left")),
                                                                     tags$div(tags$p("Showcased exceptional financial prudence, leading in almost each  individual criteria which has been duly recognised by the investors via marked share price advance. 
                                                   For those stakeholders particularly susceptible to market swings and focused on safety, it comprises a solid dividend stock investment with little to no risk. 
                                                   Strong liquidity and profit margin permit to endure during market decline and high inflation, as the superior dividend policy protects against the slump in value. 
                                                   However, one of its key facets, can turn into weakness, in the event of payout cessation/shrinkage. Furthermore, currently is traded at more than 3 times their books value. 
                                                                   Thus, it does not constitute a bargain, which would lead to extraordinary and/or certain returns. On the other hand, high dividend can compensate for the reduced stock value.")),
                                                                     h6(strong("Stal Produkt"), align = "left"),
                                                                     tags$div(tags$p(
                                                                         "One of the best examples of what might happen to a stock price when the dividend payouts halt or shrink is the Stal Produkt. 
                                                   In the past, it was valued more closely to Kety, while currently being discounted at more than half its book value whereas, enjoying an analogously high average profit margin of 7.74 as against Kety's 10.7. 
                                                   Lower returns on assets alongside financial leverage pinpoint overreliance on internal financing, which, unlike debt, does not lessen paid income taxes (tax shield). 
                                                   Despite noticeable deterioration, Stal Produkt consists of a bargain, with fundamentally strong metrics and adequate asset-back-up. Subject to soaring dividend payout, the price will shift upwards."
                                                                     )), 
                                                                     h6(strong("Alumetal"), align = "left"),
                                                                     tags$div(tags$p(
                                                                         "Alumetal compared to Stal Produkt and Kety, on average used somewhat less debt to finance its operations, yet, its ROE was the highest (14.97), 
                                                       which is indicative of good prospects ahead, that could unfold similarly to those of their rivalry. Akin to their peers, it has comfortable financial situation, consistent dividend disbursements,
                                                      and is valued only at around 1.5x of its book value, which translates into reasonable buy recommendation. It encompasses characteristics of both the conservative dividend and growth stock."
                                                                     )), 
                                                                     h5(strong("Growth Stocks"), align = "center"),
                                                                     h6(strong("Mangata"), align = "left"),
                                                                     tags$div(tags$p(
                                                                         "After Stal Profil, Mangata is on the second place when it comes to Total Debt to EBIT ratio, with total liabilities estimated to be repaid in approximately 4 years. 
                                                       What differentiates it from other growth stocks with similar market cap, such as Bowim, Cognor, and/or Drozapol, is the fact that the average Total Debt to Liabilities ratio is nearly the same 
                                                       as in 2021, which might be indicative of continuously revolving debt via which, the company gains a tax cushion, whilst paying constant dividends at competitively high levels.
                                                       On the other hand, during the general market decline and/or steel prices dowturn, the up-to-date seemingly solid profit margin (7.74, average) might curtail due to soaring COGS, 
                                                       leaving the company vulnerable to equally growing liabilities. Unlike other counterparts, instead of strengthening its financials through debt repayment and gradually moving towards
                                                       a mix of internal and external financing, it had deteriorated its liquid assets position (Total Assets to Total Liabilities = 1.32, 2021). As such, Mangata is one of the riskier instruments, considering
                                                        other more attractive choices with a greater margin of safety thereon.")), 
                                                                     h6(strong("Bowim & Cognor"), align = "left"),
                                                                     tags$div(tags$p(
                                                                         "Both experienced marked expansion and used the debt to consolidate their standing in the market.
                                                       Nonetheless, Cognor seems to diminutively surpass Bowim, illustrated via the following ratios:", 
                                                                         tags$ol(
                                                                             tags$li("Average Return on Sales (Profit margin): 3.07 > 1.97; 2021: 12.93 > 6.79"), 
                                                                             tags$li("Average Interest Coverage: 2.96 > 2.03; 2021: 11.77 > 6.15"), 
                                                                             tags$li("Total Assets to Total Liabilities (2021): 1.75 > 1.66"),
                                                                             tags$li("Average Dividend Yield: 6% > 1%")
                                                                         ), 
                                                                         
                                                                         "When it comes to the margin of profit and safety, Cognor is undoubtedly leading, at least, from the investment standpoint.
                                                       I would have been more inclined towards purchasing Bowim, as the Market cap to book value suggests the latter one is undervalued (0.61 < 1.13). However,
                                                       at the time of a revision(12/09/22), the price of Cognor declined to PLN3.42, making it nearly equally attractive, while having discernibly greater sales and earnings market cap (14.7 > 11.81 & 18.11 > 7.64).
                                                       On the contrary, Bowim had generated higher average sales (average Sales to market cap: 20.52 > 11.55), which also proves its undervaluation. 
                                                      In my view, both companies qualify to be components within the investment growth fund/portfolio."
                                                                         
                                                                     )),
                                                                     
                                                                     h6(strong("Drozapol"), align = "left"),
                                                                     tags$div(tags$p(
                                                                         "Despite sufficient debt coverage, Drozapol struggles to achieve a stable profit margin, which can be seen via low average ROS (0.25), and ROE (-0.35) ratios. 
                                                       Increasing market competitiveness stems the stable growth, subjecting the company to higher volatility both in dividends and ensuing share pricing. 
                                                       Mere reliance on adequate debt coverage does not provision greater returns than those of peers, and presently sufficient liquidity can diametrically change, 
                                                       given unexpected market contractions. An attractive price-to-book value ratio shall not constitute sole merit based on which, the investment decision should be made. 
                                                       Thus, Drozapol should be recognised as a riskier high-yield asset, with intrinsically written risk."
                                                                     )), 
                                                                     h6(strong("Stal Profil"), align = "left"),
                                                                     tags$div(tags$p(
                                                                         "While Stal Profil enjoys higher profit margin stability than Drozapol (average: 1.82 > 0.25), it succumbs to Cognor (3.07) and Bowim (1.97). 
                                                       Moreover, it will take company c. 4.5 years to repay its total debt with a depreciating total asset to total debt ratio, signifying rising debt. Yet, it can more than adequately cover the average interest payments (9.55) compared to the above three enterprises.
                                                       Noteworthy, unlike Drozapol whose revenue market share cannot be measured against Bowim or Cognor (too small), Stal Profil successfully competes against its peers (see revenue & net income market breakdown), notwithstanding concurrently lower ROE than those of equal-sized competitors.  To demonstrate this point, it is crucial to match their sales and earnings market shares. Either, Cognor and Bowim, albeit superior, share a comparably close percentage.
Having the lowest market cap to book value ratio does not necessarily mean that the stock is the most undervalued equity, as in the case of Stal Profil.  Yet, it should be clear that it is undervalued and alike could be ascribed as a growth stock with promising returns."
                                                                         
                                                                     )), 
                                                                     h6(strong("Final note"), align = "left"),
                                                                     tags$div(tags$p(
                                                                         "The foregoing assessment enabled us to find a variety of weaknesses and strengths of the Steel Market components, yet, it does not
                                                       form a full recommendation, as essentially, it only scopes out the contour of the full valuation. 
                                                       Under tab 'Random Forest - stock prediction classifier', whose analyses based on the Random Forest algorithm to predict the 
                                                       future share prices, given market sentiment linked to underlying dividend disbursements."
                                                                         
                                                                     )),
                                                                 )
                                                        ), 
                                                        
                                                        tabPanel("Random Forest - stock prediction classifier", value=5, 
                                                                 fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                                                                      plotlyOutput("DDM1"), 
                                                                                      plotlyOutput("DDM2"))),
                                                                 DTOutput("DDM3"), 
                                                                 DTOutput("DDM4")),
                                                                 
                                                        tabPanel("Stock Price Forecast", value=6, br(),
                                                                 h6(strong("Random Forest Stock Price Prediction"), align = "center"),
                                                                 plotOutput("stockpred"),
                                                                 h6(strong("Sales Forecast"), align = "center"),
                                                                                      DTOutput("DDM5"), 
                                                                 h6(strong("Earnings per share Forecast"), align = "center"),
                                                                                      DTOutput("DDM6"), 
                                                                                      h6(strong("Dividends per share Forecast"), align = "center"),
                                                                                      DTOutput("DDM7"), 
                                                                                      h6(strong("Average ratios per company"), align = "center"),
                                                                 DTOutput("DDM8")), 
                                                        tabPanel("Summary", value=6, br(),
                                                                 h6(strong("Random Forest Stock Price Prediction"), align = "center"),
                                                                 plotOutput("stockpred2"), br(),
                                                                 h6(strong("Recommendations (Sensitivity Analyses)"), align = "center"), br(), 
                                                                 h6(strong("Alumetal - Buy (PLN 71.60, current share price as of 15 September 2022)", style = "color:darkgreen")), 
                                                                 tags$div(tags$p(
                                                                 "
                                                                 Given the applied average rates of the past, Alumetal's pricing can deteriorate below its present value - PLN 71.6 as of 15 September 2022.
                                                                 Yet, the investors would have been rewarded with the dividend payments ranging from PLN 3.72-5.23, which averaged, yields c. 6% annual returns, if stock purchased at the current price level. 
                                                                 The upper bound bracket closes its price at around PLN 97-103, with markedly greater dividend payments, oscillating between PLN 4.85-7.12. 
                                                                 This scenario, nonetheless, is quite pessimistic, and does not account for the improvement with respect to market position and revenue growth. The company with evidenced solid financial results is more likely to improve its profit margin, hence, achieve more satisfactory outcomes.
                                                                 Let's assume that the determined ratios, have equally gone up by 3%, simultaneously. The ceiling price skyrockets to PLN 350, with equitably high dividends varying between PLN 10-15. The actual forecast, in this instance, appreciates the price above the present value, ranging from 83 to 275 at the end.
                                                                 On the other hand, if we deduct 3% from the averaged performance metrics, concurrently. The decline, in the worst case scenario, would result in the price deterioration down to PLN 20, and ultimately, somewhat above PLN 30, cushioned by continuous dividends distributions, equivalent to only c. 50-60% loss, as against much greater returns illustrated via the positive variant."
                                                                 
                                                                 
                                                            
                                                                 )), 
                                                                 h6(strong("Bowim - Buy (PLN 9.3, current share price as of 15 September 2022)", style = "color:darkgreen")), 
                                                                 tags$div(tags$p(
                                                                     "The application of Bowim's past average metrics does not yield satisfactory returns at the outset. The share price is bound to abate somewhere between PLN 5.9-7.12 in 2023 to recoup its capital losses in 2028, as pinpointed by higher confidence intervals and actual forecast values of PLN 17.1 and 9.89, respectively. 
                                                                     Interestingly, if we adjust the dividend payout ratio to higher levels, let's say 20%, as compared to a mean of 6.2%, while withholding other variables constant, the outlooks are changed diametrically, illustrating how the stock price valuation heavily relies on the forces of market sales' movement and dividend allotment. 
                                                                     The forecast point and lower confidence level  exemplify how the share might behave throughout years of depression, heavily suppressing its value to regain from 2026 (see higher confidence level and actual forecast point) during years of plenty, reaching a notch between PLN 23.94 to 33.58 at its highest. 
                                                                     Knowing that Bowim quite conservatively manages its debt to increase market share and gain a competitive edge, let us assume its profit margin goes up on average by 1%. As such, the stock instantly appreciates, reaching values that have never been accomplished before, nota bene, PLN 21.8 (lower) - 45.73 at the highest. 
                                                                     Evenly, the lower confidence interval price goes up to levels way higher than the current market price, making it an exceptionally an attractive growth stock.  
                                                                     However, as mentioned earlier, those more susceptible to losses due to overall market decline might find it distressing to see the price deteriorating throughout the years of recession. Thus, not every stock is suitable for investor's risk appetite and Bowim, could be perceived as a slightly riskier asset due to a greater volatility.  
                                                                     "
                                                                 )), 
                                                                 h6(strong("Cognor - Strong Buy (PLN 3.53, current share price as of 15 September 2022)", style = "color:lightgreen")),
                                                                 tags$div(tags$p(
                                                                     "The current share price is deemed to be undervalued, given averaged financial metrics employed during the initial investigation. With the share price appreciation of c. PLN 2, investors can count on c. 50% return on capital gains plus 4% average annual dividend yield. 
                                                                     Under the assumption of an improved profit margin, of let's say 2%, the share price is more likely to experience a considerable advance, in the worst-case scenario, nearly tripling the current value to PLN 10 throughout 7 year-period. Despite, the possible initial decline, given the lower intervals, the potential price drop, would be compensated via high dividends in its entirety, 
                                                                     while plausible scenario indicating price advancing to the thresholds of PLN 21.74, consequently, making Cognor a good bargain. 
                                                                     "
                                                                 )), 
                                                                 h6(strong("For further recommendations, please", style = "color:red"), mailtoR(email = "piotr.wenda@yahoo.co.uk", text = "click here to contact me via email.", 
                                                                                                                                                                  subject = "Equity Research Recommendations", 
                                                                                                                                                                  body = "Dear Piotr, 
                 
                 I am contacting you to discuss ..."),
                                                                 
                                                                 use_mailtoR()
                                                                 
                                                                 ))
                                            )
                                        )
                          )))

server <- function(input, output, session) {
    output$plot <- renderPlot({
        if (is.null(input$select)) {
            return(NULL)
        }
        req(Market_data_frame_total)
        Market_data_frame_total %>%
            filter(Variable %in% input$select) %>%
            ggplot() +
            geom_line(aes(x = Date, Value, color = Variable))+
            geom_point(aes(x = Date, Value, color = Variable)) +
            scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                           scientific = FALSE)) +
            theme(legend.position = "bottom") +
            geom_text_repel(aes(x = Date, y = Value, label = label_number_si()(Value),size=6),show_guide = F) + 
            theme(text = element_text(size = 12)) + theme(axis.text.y = element_text(size = 10)) + 
            theme(axis.text.x = element_text(size = 10))  + theme(legend.text = element_text(size = 10))  +
            ggtitle("Market Data (per selected variables)") +
            theme(plot.title = element_text(hjust = 0.5))
    })
    
    val1 <- reactive({
        if (is.null(input$select8)) {
            return(NULL)
        }
        Market_data_frame_capik1 <- Market_data_frame_capik %>% select(input$select8)
        Market_data_frame_capik <- ts(Market_data_frame_capik1, start = 2015, end = 2021)
        Market_data_frame_capik <- window(Market_data_frame_capik, start = 2015, end = 2021)
        fit.Market_data_frame_capik <- tslm(Market_data_frame_capik ~ trend)
        Rka <- forecast::forecast(fit.Market_data_frame_capik, h = 7)
        list(Rka)
    })
    
    
    output$plot4 <- renderDygraph({
        if (is.null(input$select8)) {
            return(NULL)
        }
        req(val1())
        options(scipen = 999)
        
        Market_data_frame_capik1 <- Market_data_frame_capik %>% select(input$select8)
        Market_data_frame_capik <- ts(Market_data_frame_capik1, start = 2015, end = 2021)
        Market_data_frame_capik <- window(Market_data_frame_capik, start = 2015, end = 2021)
        fit.Market_data_frame_capik <- tslm(Market_data_frame_capik ~ trend)
        Rka <- forecast::forecast(fit.Market_data_frame_capik, h = 7)
        accuracy(fit.Market_data_frame_capik)
        Rka1 <- Rka %>%
            {ts(cbind(actuals = .$x, forecast_mean = .$mean,
                      lower_95=.$lower[,2], upper_95=.$upper[,2],
                      lower_80=.$lower[,1], upper_80=.$upper[,1]), start = c(2015), end = c(2028),  freq = 1)} %>%  
            dygraph(main = "Forecast") %>%
            dyAxis("y", valueFormatter = interval_value_formatter) %>%
            dySeries("actuals", color = "black") %>%
            dySeries("forecast_mean", color = "blue", label = "forecast") %>%
            dySeries(c("lower_80", "forecast_mean", "upper_80"),
                     label = "80%", color = "blue") %>%
            dySeries(c("lower_95", "forecast_mean", "upper_95"),
                     label = "95%", color = "blue") %>%
            dyLegend(labelsSeparateLines=TRUE) %>%
            dyRangeSelector() %>%
            dyOptions(digitsAfterDecimal = 4) %>%
            dyOptions(labelsKMB = TRUE) %>% 
            dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }"))
        
        
        
        
    })
    
    
    
    output$plot2 <- renderPlot({
        if (is.null(input$select2)) {
            return(NULL)
        }
        req(Market_data_frame_break)
        Market_data_frame_break %>%
            filter(Variable %in% input$select2) %>%
            group_by(Date, Variable) %>% 
            mutate(Percentage = Value/sum(Value)) %>%
            ggplot(aes(fill=Company_name, y=Value, x=Date)) +
            geom_bar(stat="identity") +
            geom_col() +
            geom_text_repel(max.overlaps = 2000,
                            aes(label = paste0(round(Percentage*100, 2), "%"), x = Date, y = Value), position = position_stack(vjust = 0.5)) +
            scale_y_continuous(labels = scales::number_format()) +
            facet_wrap(~Variable) +
            theme(legend.position = "right") + scale_fill_brewer(palette="Set3") + 
            theme(text = element_text(size = 12)) + theme(axis.text.y = element_text(size = 10)) + 
            theme(axis.text.x = element_text(size = 10)) +
            ggtitle("Market data breakdown") +
            theme(plot.title = element_text(hjust = 0.5))
    })
    
    output$plot3 <- renderPlot({
        if (is.null(input$select3)) {
            return(NULL)
        }
        req(Market_data_frame_break)
        Market_data_frame_break %>%
            filter(Variable %in% input$select3) %>%
            group_by(Date, Variable) %>% 
            mutate(Percentage = Value/sum(Value)) %>%
            ggplot(aes(fill=Company_name, y=Value, x=Date)) +
            geom_bar(stat="identity") +
            geom_col() +
            geom_text_repel(
                aes(label = paste0(round(Percentage*100, 2), "%"), x = Date, y = Value), position = position_stack(vjust = 0.5)) +
            scale_y_continuous(labels = scales::number_format()) +
            facet_wrap(~Variable) + 
            coord_flip() + theme(legend.position = "none") + scale_fill_brewer(palette="Set3") + 
            theme(text = element_text(size = 12)) + theme(axis.text.y = element_text(size = 10)) + 
            theme(axis.text.x = element_text(size = 10)) +
            ggtitle("Market data breakdown") +
            theme(plot.title = element_text(hjust = 0.5))
    })    
    
    output$text4 <-  DT::renderDT({
        req(Market_data_frame_capik)
        Market_data_frame_capik1 <- Market_data_frame_capik %>% select(input$select8)
        Market_data_frame_capik <- ts(Market_data_frame_capik1, start = 2015, end = 2021)
        Market_data_frame_capik <- window(Market_data_frame_capik, start = 2015, end = 2021)
        fit.Market_data_frame_capik <- tslm(Market_data_frame_capik ~ trend)
        Rka <- forecast::forecast(fit.Market_data_frame_capik, h = 7)
        Rka0 <- accuracy(Rka)
        Rka0 <- round(Rka0, 4)
        DT::datatable(Rka0, options = list(searching = FALSE, pageLength = 1, paging = FALSE))
    })
    
    val3 <- reactive({
        if (is.null(input$select8)) {
            return(NULL)
        }
        Market_data_frame_capik1 <- Market_data_frame_capik$Market_cap
        Market_data_frame_capik <- ts(Market_data_frame_capik1, start = 2015, end = 2021)
        Market_data_frame_capik <- window(Market_data_frame_capik, start = 2015, end = 2021)
        fit.Market_data_frame_capik <- tslm(Market_data_frame_capik ~ trend)
        Rka3 <- forecast::forecast(fit.Market_data_frame_capik, h = 7)
        list(Rka3)
    })         
    
    
    observeEvent(input$select15, {
        
        output$plot15 <- renderDygraph({
            if (is.null(input$select15)) {
                return(NULL)
            }
            options(scipen = 999)
            req(val3())
            Rka <- val3()
            Rka <- as.data.frame(Rka)
            Companies_cap_ratio <- Companies_cap_ratio %>% select(input$select15)
            Companies_shares_outstanding <- Shares_total_2  %>% select(input$select15)
            Share_pricing <- Companies_cap_ratio[,1] * Rka
            Share_pricing <- round(Share_pricing / Companies_shares_outstanding[,1], 2)
            Shares_pricing_past <- Shares_pricing_past %>% select(input$select15)
            colnames(Shares_pricing_past) <- c("Point.Forecast")
            Final_Pricing <- dplyr::bind_rows(Shares_pricing_past, Share_pricing)
            Final_Pricing$Actuals = rbind(Shares_pricing_past, NA,NA,NA,NA,NA,NA,NA)
            Final_Pricing$Point.Forecast <- c(NA,NA,NA,NA,NA,NA,NA, Final_Pricing[8:14,1])
            Final_Pricing <- do.call(cbind.data.frame, Final_Pricing)
            colnames(Final_Pricing) <- c("Point.Forecast", "Lo.80", "Hi.80", "Lo.95", "Hi.95", "Actuals")
            Final_Pricing <- ts(Final_Pricing, start = c(2015), end = c(2028), freq = 1)
            Final_Pricing <- ts(Final_Pricing, start = c(2015), end = c(2028), freq = 1) 
            dygraph(Final_Pricing, main = "Forecast") %>%
                dyAxis("y", valueFormatter = interval_value_formatter) %>%
                dySeries("Point.Forecast", color = "blue", label = "forecast") %>%
                dySeries(c("Lo.80", "Point.Forecast", "Hi.80"),
                         label = "80%", color = "blue") %>%
                dySeries(c("Lo.95", "Point.Forecast", "Hi.95"),
                         label = "95%", color = "blue") %>%
                dySeries("Actuals", color = "black", label = "Actuals") %>% 
                dyLegend(labelsSeparateLines=TRUE) %>%
                dyRangeSelector() %>%
                dyOptions(digitsAfterDecimal = 4, connectSeparatedPoints = TRUE, drawYAxis = TRUE) %>%
                dyOptions(labelsKMB = TRUE) %>% 
                dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }")) 
            
            
            
        })
    })
    
    output$marketcapratio <- DT::renderDT({
        req(ChangeInRatio)
        ChangeInRatio <- data.frame(ChangeInRatio)
        datatable(ChangeInRatio, options = list(searching = FALSE, pageLength = 1, paging = FALSE)) %>% 
            formatPercentage(colnames(ChangeInRatio),2)
        
        
    })
    
    output$marketcapratio2 <- DT::renderDT({
        req(Companies_cap_ratio) 
        rownames(Companies_cap_ratio) <- c("2022", "2023", "2024", "2025", "2026", "2027", "2028")
        datatable(Companies_cap_ratio, options = list(searching = FALSE, pageLength = 1, paging = FALSE)) %>% 
            formatPercentage(colnames(Companies_cap_ratio),2)
    })
    
    
    val3 <- reactive({
        if (is.null(input$select8)) {
            return(NULL)
        }
        Market_data_frame_capik1 <- Market_data_frame_capik$Market_cap
        Market_data_frame_capik <- ts(Market_data_frame_capik1, start = 2015, end = 2021)
        Market_data_frame_capik <- window(Market_data_frame_capik, start = 2015, end = 2021)
        fit.Market_data_frame_capik <- tslm(Market_data_frame_capik ~ trend)
        Rka3 <- forecast::forecast(fit.Market_data_frame_capik, h = 7)
        list(Rka3)
    })
    
    
    
    
    output$sharepr <- renderReactable({
        if (is.null(input$select15)) {
            return(NULL)
        }
        options(scipen = 999)
        req(val3())
        Rka3 <- val3()
        Rka3 <- as.data.frame(Rka3)
        Companies_cap_ratio <- Companies_cap_ratio %>% select(input$select15)
        Companies_shares_outstanding <- Shares_total_2  %>% select(input$select15)
        Share_pricing <- Companies_cap_ratio[,1] * Rka3
        Share_pricing <- round(Share_pricing / Companies_shares_outstanding[,1], 2)
        Share_pricing <- ((lead(Share_pricing) - Share_pricing)) / Share_pricing
        Share_pricing <- na.omit(Share_pricing)
        Share_pricing <- round(Share_pricing, 2)
        if (input$select15 == "Stal_produkt") {
            Share_pricing[6,] <- Share_pricing[6,] * -1 
        } else Share_pricing[6,] = Share_pricing[6,]
        reactable(Share_pricing, columns = list(
            Point.Forecast = colDef(style = function(value) {
                if (value > 0.02) {
                    color <- "#008000"
                } else if (value < 0.02) {
                    color <- "#e00000" }
                list(color = color, fontWeight = "bold")
            }, format = colFormat(percent = TRUE, digits = 2),), 
            
            
            Lo.80 =   colDef(format = colFormat(percent = TRUE, digits = 2)), 
            Hi.80 = colDef(format = colFormat(percent = TRUE, digits = 2)),
            Lo.95 = colDef(format = colFormat(percent = TRUE, digits = 2)),
            Hi.95 = colDef(format = colFormat(percent = TRUE, digits = 2))))
    })
    
    output$sharepr2 <- renderDT({
        
        options(scipen = 999)
        req(val3())
        Rka3 <- val3()
        Rka3 <- as.data.frame(Rka3)
        
        Rka3 <- data.frame(Rka3)
        Rka3 <- ((lead(Rka3) - Rka3)) / Rka3
        Share_pricing2 <- na.omit(Rka3)
        Share_pricing2 <- round(Share_pricing2, 2)
        
        datatable(Share_pricing2, options = list(searching = FALSE, pageLength = 1, paging = FALSE)) %>% 
            formatPercentage(colnames(Share_pricing2),2)
    })
    
    output$MetricsFin <- renderPlotly({
        req(Market_data_frame_Market_Metrics)
        K1 <-  Market_data_frame_Market_Metrics %>%
            filter(Variable %in% input$select20) %>%
            ggplot() +
            geom_line(aes(x = Date, Value, color = Company_name))+
            geom_point(aes(x = Date, Value)) +
            geom_line(aes(x = Date, y = mean(Value)), color = "black", linetype = "dotted") +
            scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                           scientific = FALSE)) +
            theme(text = element_text(size = 12)) + theme(axis.text.y = element_text(size = 10)) + 
            theme(axis.text.x = element_text(size = 10))  + theme(legend.text = element_text(size = 10))  +
            ggtitle("Financial Metric") +
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_point(aes(x = Date, Value, color = Variable, label = Company_name)) +
            theme(text = element_text(size = 12)) + theme(axis.text.y = element_text(size = 10)) + 
            theme(axis.text.x = element_text(size = 10))  + theme(legend.text = element_text(size = 10)) +
            theme(plot.title = element_text(hjust = 0.5)) 
        
        ggplotly(K1)
    })
    
    output$MetricsFin2 <- renderPlotly({
        req(Market_data_frame_Market_Metrics)
        K <-  Market_data_frame_Market_Metrics %>%
            filter(Variable %in% input$select21) %>%
            ggplot() +
            geom_line(aes(x = Date, Value, color = Company_name))+
            geom_point(aes(x = Date, Value)) +
            geom_line(aes(x = Date, y = mean(Value)), color = "black", linetype = "dotted") +
            scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                           scientific = FALSE)) +
            theme(text = element_text(size = 12)) + theme(axis.text.y = element_text(size = 10)) + 
            theme(axis.text.x = element_text(size = 10))  + theme(legend.position = "none")  +
            ggtitle("Financial Metric") +
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_point(aes(x = Date, Value, color = Variable, label = Company_name)) +
            theme(text = element_text(size = 12)) + theme(axis.text.y = element_text(size = 10)) + 
            theme(axis.text.x = element_text(size = 10))  + theme(legend.text = element_text(size = 10)) +
            theme(plot.title = element_text(hjust = 0.5))  
        
        ggplotly(K) 
    })
    
    
    
    output$finmetrics10 <- renderDT({
        Average_Results_Fin <- t(Average_Results_Fin)
        
        datatable(Average_Results_Fin, options = list(scrollX = TRUE, fixedColumns = list(leftColumns = 1)), extensions = "FixedColumns") %>% 
            formatStyle(
                'Asset_Turnover',
                backgroundColor = styleInterval(1.46, c('red', 'green'))
            ) %>% 
            formatStyle(
                'Return_on_Sales',
                backgroundColor = styleInterval(4.17, c('red', 'green'))
            ) %>% 
            formatStyle(
                'Return_on_Assets',
                backgroundColor = styleInterval(6.38, c('red', 'green'))
            ) %>% 
            formatStyle(
                'Financial_Leverage',
                backgroundColor = styleInterval(1.71, c('green', 'red'))
            ) %>% 
            formatStyle(
                'Return_on_Equity',
                backgroundColor = styleInterval(13.16, c('red', 'green'))) %>% 
            formatStyle(
                'Market_cap_to_book_value',
                backgroundColor = styleInterval(0.78, c('green', 'red'))
            ) %>% 
            formatStyle(
                'Total_Assets_to_Total_liabilities',
                backgroundColor = styleInterval(2.07, c('red', 'green'))
            ) %>% 
            formatStyle(
                'Working_capital_to_current_assets',
                backgroundColor = styleInterval(1.26, c('red', 'green'))
            ) %>% 
            formatStyle(
                'Total_Debt_to_EBIT',
                backgroundColor = styleInterval(4.59, c('green', 'red'))
            ) %>% 
            formatStyle(
                'Interest_Coverage',
                backgroundColor = styleInterval(11.61, c('red', 'green'))
            ) %>% 
            formatStyle(
                'Dividend_yield',
                backgroundColor = styleInterval(0.03, c('red', 'green'))
            ) %>% 
            formatStyle(
                'Sales_to_market_cap',
                backgroundColor = styleInterval(4.83, c('red', 'green'))
            )
        
        
        
        
        
        
    })
    
    output$finmetrics11 <- renderDT({
        Strength20 <- as.data.frame(Strength20)
        colfunc <- colorRampPalette(c("blue", "deepskyblue"))
        datatable(Strength20, options = list(scrollX = TRUE, fixedColumns = c(leftColumns = 2)), extensions = "FixedColumns") %>% 
            formatStyle("Asset_Turnover", backgroundColor = styleEqual(sort(unique(Strength20$Asset_Turnover), 
                                                                            decreasing = TRUE),colfunc(length(unique(Strength20$Asset_Turnover))))) %>% 
            formatStyle("Return_on_Sales", backgroundColor = styleEqual(sort(unique(Strength20$Return_on_Sales), 
                                                                             decreasing = TRUE),colfunc(length(unique(Strength20$Return_on_Sales))))) %>% 
            formatStyle("Return_on_Assets", backgroundColor = styleEqual(sort(unique(Strength20$Return_on_Assets), 
                                                                              decreasing = TRUE),colfunc(length(unique(Strength20$Return_on_Assets))))) %>% 
            formatStyle("Financial_Leverage", backgroundColor = styleEqual(sort(unique(Strength20$Financial_Leverage), 
                                                                                decreasing = F),colfunc(length(unique(Strength20$Financial_Leverage))))) %>% 
            formatStyle("Return_on_Equity", backgroundColor = styleEqual(sort(unique(Strength20$Return_on_Equity), 
                                                                              decreasing = TRUE),colfunc(length(unique(Strength20$Return_on_Equity))))) %>% 
            formatStyle("Sales_to_market_cap", backgroundColor = styleEqual(sort(unique(Strength20$Sales_to_market_cap), 
                                                                                 decreasing = TRUE),colfunc(length(unique(Strength20$Sales_to_market_cap))))) %>% 
            formatStyle("Market_cap_to_book_value", backgroundColor = styleEqual(sort(unique(Strength20$Market_cap_to_book_value), 
                                                                                      decreasing = TRUE),colfunc(length(unique(Strength20$Market_cap_to_book_value))))) %>% 
            formatStyle("Total_Assets_to_Total_liabilities", backgroundColor = styleEqual(sort(unique(Strength20$Total_Assets_to_Total_liabilities), 
                                                                                               decreasing = TRUE),colfunc(length(unique(Strength20$Total_Assets_to_Total_liabilities))))) %>% 
            formatStyle("Working_capital_to_current_assets", backgroundColor = styleEqual(sort(unique(Strength20$Working_capital_to_current_assets), 
                                                                                               decreasing = TRUE),colfunc(length(unique(Strength20$Working_capital_to_current_assets))))) %>% 
            formatStyle("Total_Debt_to_EBIT", backgroundColor = styleEqual(sort(unique(Strength20$Total_Debt_to_EBIT), 
                                                                                decreasing = TRUE),colfunc(length(unique(Strength20$Total_Debt_to_EBIT))))) %>% 
            formatStyle("Interest_Coverage", backgroundColor = styleEqual(sort(unique(Strength20$Interest_Coverage), 
                                                                               decreasing = TRUE),colfunc(length(unique(Strength20$Interest_Coverage))))) %>% 
            formatStyle("Dividend_yield", backgroundColor = styleEqual(sort(unique(Strength20$Dividend_yield), 
                                                                            decreasing = TRUE),colfunc(length(unique(Strength20$Dividend_yield)))))           
        
        
    })
    
    output$DDM1 <- renderPlotly({
        
        ggplot(Dividendpay1, aes(y=Share_Price,x=Dividend_per_share, color=Earned_per_share)) + stat_smooth(method="lm",se=T, fill = "lightblue", formula = y ~ poly(x,2), colour = "red") +
            geom_point() + theme(text = element_text(size = 12)) + theme(axis.text.y = element_text(size = 10)) + 
            ylab("Share Price") + xlab("Dividend") +
            theme(axis.text.x = element_text(size = 10))  + theme(legend.text = element_text(size = 10))  +
            ggtitle("LM: Share Price vs Dividend & Earned per share") +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(text = element_text(size = 8)) + theme(axis.text.y = element_text(size = 10)) + 
            theme(axis.text.x = element_text(size = 10))  + theme(legend.text = element_text(size = 10)) +
            theme(plot.title = element_text(hjust = 0.5))
        
    })
    
    output$DDM2 <- renderPlotly({
        
        ggplot() +
            geom_point( aes(x = Dividendpay1$Dividend_per_share, y = Dividendpay1$Share_Price, colour = "Actual", alpha = 0.5)) + 
            geom_point( aes(x = Dividendpay1$Dividend_per_share , y = predictions, colour ="Predicted",  alpha = 0.5)) + 
            labs(x = "Dividends", y = "Price", color = "", alpha = 'Transperency') +
            theme(axis.text.x = element_text(size = 10))  + theme(legend.text = element_text(size = 10))  +
            ggtitle("Random forest prediction, ntree = 100") +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(text = element_text(size = 8)) + theme(axis.text.y = element_text(size = 10)) + 
            theme(axis.text.x = element_text(size = 10))  + theme(legend.text = element_text(size = 10)) 
        
    })
    
    output$DDM3 <- DT::renderDT({
        req(Validation_metrics)
        Validation_metrics <- round(Validation_metrics, 2)
        
        datatable(Validation_metrics, options = list(searching = FALSE, pageLength = 1, paging = FALSE)) 
        
        
    })
    
    
    
    output$DDM4 <- DT::renderDT({
        req(Validation_metrics1)
        Validation_metrics1$Predicted <- round(Validation_metrics1$Predicted, 2)
        datatable(Validation_metrics1, options = list(searching = T, pageLength = 10, paging = T))
        
    })
    

    Ratio_Market_Cap_Company_Forecast <- reactive({
      markcap <- input$markcap / 100
        })
    
    Retonsales <- reactive({
        Retonsales <- input$Retonsales / 100
    })
    
    Market_data_frame99 <- Market_data_frame
    Market_data_frame99$Company_name <- as.character(Market_data_frame99$Company_name)
    Market_data_frame99 <- Market_data_frame99[,c(1,3,21)]
    
    Shares_Out_per_Com <- as.data.frame(Market_data_frame99 %>% subset(Date == "2021-12-30"))
    
    Shares_Out_per_Com$Company_name <- c("Stal_produkt","Alumetal","Kety","Bowim","Stal_profil","Drozapol","Mangata","Cognor")
        
    Predicted_Sales <- reactive({
        Company_Market_cap_ratio <- as.vector(Ratio_Market_Cap_Company_Forecast())
    Predicted_Sales <- round(Company_Market_cap_ratio * Rka10, 0)
    return(Predicted_Sales)
    })
    
    Predicted_Earnings_Per_Share <- reactive({
    Return_on_Sales <- as.vector(Retonsales())
    Shares_Out_per_Com <- as.vector(Shares_Out_per_Com %>%
                  filter(Company_name %in% input$select50) %>% select(Shares_outstanding))
    Predicted_Earnings <- round((Predicted_Sales()*Return_on_Sales) / Shares_Out_per_Com,2)
    return(Predicted_Earnings)
    })
    
    Dividend_Per_Share <- reactive({ 
        if (is.null(input$dividendpay)) {
            return(NULL)
        }
        Predicted_Earnings <- Predicted_Earnings_Per_Share()
        Dividend_payout <- (as.vector(input$dividendpay) / 100)
        Dividend_per_share <- Predicted_Earnings * Dividend_payout
    return(Dividend_per_share)
        
    })
output$stockpred <- renderPlot({

    # Point Forecast 
    Dividend_Per_Share <- Dividend_Per_Share()
    Dividend_Forecast <- Dividend_Per_Share$`Point Forecast`
    Predicted_Earnings_Per_Share <- Predicted_Earnings_Per_Share()
    Predicted_Earnings_Forecast <- Predicted_Earnings_Per_Share$`Point Forecast`

    train_forecast <- cbind(Dividend_Forecast, Predicted_Earnings_Forecast)
    colnames(train_forecast) <- c("Dividend_per_share", "Earned_per_share")
    Point_forecast <- predict(regr, train_forecast)
    
    # Lo 80
    
    Dividend_Forecast_Low80  <- Dividend_Per_Share$`Lo 80`

    Predicted_Earnings_Low80 <- Predicted_Earnings_Per_Share$`Lo 80`
    
    train_forecast_Low80 <- cbind(Dividend_Forecast_Low80, Predicted_Earnings_Low80)
    colnames(train_forecast_Low80) <- c("Dividend_per_share", "Earned_per_share")
    Low80_forecast <- predict(regr, train_forecast_Low80)
    
    # Hi 80
    
    Dividend_Forecast_High80  <- Dividend_Per_Share$`Hi 80`
    
    Predicted_Earnings_High80 <- Predicted_Earnings_Per_Share$`Hi 80`
    
    train_forecast_High80 <- cbind(Dividend_Forecast_High80, Predicted_Earnings_High80)
    colnames(train_forecast_High80) <- c("Dividend_per_share", "Earned_per_share")
    High80_forecast <- predict(regr, train_forecast_High80)
    
    # High 95 
    
    Dividend_Forecast_High95  <- Dividend_Per_Share$`Hi 95`
    
    Predicted_Earnings_High95 <- Predicted_Earnings_Per_Share$`Hi 95`
    
    train_forecast_High95 <- cbind(Dividend_Forecast_High95, Predicted_Earnings_High95)
    colnames(train_forecast_High95) <- c("Dividend_per_share", "Earned_per_share")
    High95_forecast <- predict(regr, train_forecast_High95)
    
    # Low95
    
    Dividend_Forecast_Low95  <- Dividend_Per_Share$`Lo 95`
    
    Predicted_Earnings_Low95  <- Predicted_Earnings_Per_Share$`Lo 95`
    
    train_forecast_Low95 <- cbind(Dividend_Forecast_Low95, Predicted_Earnings_Low95)
    colnames(train_forecast_Low95) <- c("Dividend_per_share", "Earned_per_share")
    Low95_forecast <- predict(regr, train_forecast_Low95)
    
    Forecasted_Data_Total <- data_frame(Point_forecast, Low80_forecast, High80_forecast, Low95_forecast, High95_forecast)
    colnames(Forecasted_Data_Total) <- c("Point.Forecast", "Lo.80", "Hi.80", "Lo.95", "Hi.95")
    Forecasted_Data_Total <- as.data.frame(Forecasted_Data_Total)
    
    # 2022 
    Pred22 <- Forecasted_Data_Total[1,]
    Pred22 <- as.numeric(Pred22)
    sample.mean22 <- mean(Pred22)
    sample.n22 <- length(Pred22)
    sample.sd22 <- sd(Pred22)
    sample.se22 <- sample.sd22/sqrt(sample.n22)
    
    alpha = 0.05
    degrees.freedom = sample.n22 - 1
    t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
    
    
    margin.error22 <- t.score * sample.se22
    lower.bound22 <- sample.mean22 - margin.error22
    upper.bound22 <- sample.mean22 + margin.error22
    
    # 2023
    
    Pred23 <- Forecasted_Data_Total[2,]
    Pred23 <- as.numeric(Pred23)
    sample.mean23 <- mean(Pred23)
    sample.n23 <- length(Pred23)
    sample.sd23 <- sd(Pred23)
    sample.se23 <- sample.sd23/sqrt(sample.n23)
    
    margin.error23 <- t.score * sample.se23
    lower.bound23 <- sample.mean23 - margin.error23
    upper.bound23 <- sample.mean23 + margin.error23
    
    # 2024
    
    Pred24 <- Forecasted_Data_Total[3,]
    Pred24 <- as.numeric(Pred24)
    sample.mean24 <- mean(Pred24)
    sample.n24 <- length(Pred24)
    sample.sd24 <- sd(Pred24)
    sample.se24 <- sample.sd24/sqrt(sample.n24)
    
    margin.error24 <- t.score * sample.se24
    lower.bound24 <- sample.mean24 - margin.error24
    upper.bound24 <- sample.mean24 + margin.error24
    
    # 2025
    
    Pred25 <- Forecasted_Data_Total[4,]
    Pred25 <- as.numeric(Pred25)
    sample.mean25 <- mean(Pred25)
    sample.n25 <- length(Pred25)
    sample.sd25 <- sd(Pred25)
    sample.se25 <- sample.sd25/sqrt(sample.n25)
    
    margin.error25 <- t.score * sample.se25
    lower.bound25 <- sample.mean25 - margin.error25
    upper.bound25 <- sample.mean25 + margin.error25
    
    # 2026
    
    Pred26 <- Forecasted_Data_Total[5,]
    sample.n26 <- length(Pred26)
    Pred26 <- as.numeric(Pred26)
    sample.mean26 <- mean(Pred26)
    sample.sd26 <- sd(Pred26)
    sample.se26 <- sample.sd26/sqrt(sample.n26)
    
    margin.error26 <- t.score * sample.se26
    lower.bound26 <- sample.mean26 - margin.error26
    upper.bound26 <- sample.mean26 + margin.error26
    
    # 2027
    
    Pred27 <- Forecasted_Data_Total[6,]
    Pred27 <- as.numeric(Pred27)
    sample.n27 <- length(Pred27)
    sample.mean27 <- mean(Pred27)
    sample.sd27 <- sd(Pred27)
    sample.se27 <- sample.sd27/sqrt(sample.n27)
    
    margin.error27 <- t.score * sample.se27
    lower.bound27 <- sample.mean27 - margin.error27
    upper.bound27 <- sample.mean27 + margin.error27
    
    # 2028
    
    Pred28 <- Forecasted_Data_Total[7,]
    Pred28 <- as.numeric(Pred28)
    sample.n28 <- length(Pred28)
    sample.mean28 <- mean(Pred28)
    sample.sd28 <- sd(Pred28)
    sample.se28 <- sample.sd28/sqrt(sample.n28)
    
    margin.error28 <- t.score * sample.se28
    lower.bound28 <- sample.mean28 - margin.error28
    upper.bound28 <- sample.mean28 + margin.error28
    
    # Combine all 
    Hi.95 <- rbind(upper.bound22, upper.bound23,upper.bound24,upper.bound25,upper.bound26,upper.bound27,upper.bound28)
    Lo.95 <- rbind(lower.bound22, lower.bound23, lower.bound24, lower.bound25, lower.bound26, lower.bound27, lower.bound28)
    
    Date <- c("30/12/22", "30/12/23", "30/12/24", "30/12/25", "30/12/26", "30/12/27", "30/12/28")
    Date <- as.Date(Date, "%d/%m/%y")

    
    Forecasted_Data_Total <- data.frame(Forecasted_Data_Total$Point.Forecast, Hi.95, Lo.95, Date)
    
    colnames(Forecasted_Data_Total) <- c("Point.Forecast", "Hi.95", "Lo.95", "Date")
    rownames(Forecasted_Data_Total) <- NULL
    
    
    
    
    Forecasted_Data_Total <- Forecasted_Data_Total %>%
        gather(Type, Value, -Date)
    
    Forecasted_Data_Total$Value <- round(Forecasted_Data_Total$Value, 2)
    
    ggplot(Forecasted_Data_Total, aes(x=Date, y = Value, col = factor(Type))) +
        geom_line()+
        labs(x = "Date",
             y = "Share Price",
             color = "Legend") +
            geom_point() + geom_label_repel(aes(label = Value,
                                                fill = Value), color = 'white',
                                            size = 5, show.legend = F) +
        scale_color_manual(values=c('green','red', "black")) +
        theme(axis.text.x = element_text(size = 10))  + theme(legend.text = element_text(size = 10)) +
        scale_x_date(date_breaks = "1 year")
    })
    
    
    output$DDM5 <- DT::renderDT({
        if (is.null(input$Retonsales)) {
            return(NULL)
        }
Predicted_Sales <- Predicted_Sales()
        datatable(Predicted_Sales, options = list(searching = T, pageLength = 10, paging = T)) %>%
            formatCurrency(c("Point Forecast","Lo 80", "Hi 80", "Lo 95", "Hi 95"),currency = "PLN", interval = 3, mark = ",")
    })
    
    output$DDM6 <- DT::renderDT({
        Predicted_Earnings <- Predicted_Earnings_Per_Share()
         datatable(Predicted_Earnings, options = list(searching = T, pageLength = 10, paging = T)) %>%
             formatCurrency(c("Point Forecast","Lo 80", "Hi 80", "Lo 95", "Hi 95"),currency = "PLN", interval = 3, mark = ",")
    })
    
     output$DDM7 <- DT::renderDT({
         Dividend_Per_Share <- Dividend_Per_Share()
              datatable(Dividend_Per_Share, options = list(searching = T, pageLength = 10, paging = T)) %>%
             formatCurrency(c("Point Forecast","Lo 80", "Hi 80", "Lo 95", "Hi 95"),currency = "PLN", interval = 3, mark = ",")
             

        })


    
     output$DDM8 <- DT::renderDT({

         Dividend_Caps <- cbind.data.frame(Market_data_frame$Dividends, Market_data_frame$Net_Income, Market_data_frame$Company_name)
         Dividend_Caps$Div_Ratio <- Dividend_Caps$`Market_data_frame$Dividends` / Dividend_Caps$`Market_data_frame$Net_Income`
         Dividend_Caps <- Dividend_Caps[,-c(1,2)]
         Dividend_Caps <- aggregate(Dividend_Caps, list(Dividend_Caps[,1]), FUN="mean")
         Dividend_Caps <- Dividend_Caps[,-c(2)]
         colnames(Dividend_Caps) <- c("Company_name", "Ratio")
         Dividend_Caps$Company_name <- c("Alumetal", "Bowim", "Cognor", "Drozapol", "Kety", "Mangata", "Stal_produkt", "Stal_profil")
         
     ROS$Ratio2 <- Ratio$Ratio
     ROS$AverageDividend <- Dividend_Caps$Ratio
     colnames(ROS) <- c("Average Return on Sales", "Company Name", "Average Revenue share per company to Total Revenue Cap", "Average Dividend Payout")
     ROS <- ROS[,c(1,3,4,2)]

     ROS[,1:3] <- round(ROS[,1:3],4)
     ROS <- ROS[,-c(4)]
     datatable(ROS, options= list(searching = T, pageLength = 10, paging = T)) %>%  formatPercentage(c("Average Return on Sales", "Average Revenue share per company to Total Revenue Cap", "Average Dividend Payout"), 2) 
     })
     
     output$stockpred <- renderPlot({
         
         # Point Forecast 
         Dividend_Per_Share <- Dividend_Per_Share()
         Dividend_Forecast <- Dividend_Per_Share$`Point Forecast`
         Predicted_Earnings_Per_Share <- Predicted_Earnings_Per_Share()
         Predicted_Earnings_Forecast <- Predicted_Earnings_Per_Share$`Point Forecast`
         
         train_forecast <- cbind(Dividend_Forecast, Predicted_Earnings_Forecast)
         colnames(train_forecast) <- c("Dividend_per_share", "Earned_per_share")
         Point_forecast <- predict(regr, train_forecast)
         
         # Lo 80
         
         Dividend_Forecast_Low80  <- Dividend_Per_Share$`Lo 80`
         
         Predicted_Earnings_Low80 <- Predicted_Earnings_Per_Share$`Lo 80`
         
         train_forecast_Low80 <- cbind(Dividend_Forecast_Low80, Predicted_Earnings_Low80)
         colnames(train_forecast_Low80) <- c("Dividend_per_share", "Earned_per_share")
         Low80_forecast <- predict(regr, train_forecast_Low80)
         
         # Hi 80
         
         Dividend_Forecast_High80  <- Dividend_Per_Share$`Hi 80`
         
         Predicted_Earnings_High80 <- Predicted_Earnings_Per_Share$`Hi 80`
         
         train_forecast_High80 <- cbind(Dividend_Forecast_High80, Predicted_Earnings_High80)
         colnames(train_forecast_High80) <- c("Dividend_per_share", "Earned_per_share")
         High80_forecast <- predict(regr, train_forecast_High80)
         
         # High 95 
         
         Dividend_Forecast_High95  <- Dividend_Per_Share$`Hi 95`
         
         Predicted_Earnings_High95 <- Predicted_Earnings_Per_Share$`Hi 95`
         
         train_forecast_High95 <- cbind(Dividend_Forecast_High95, Predicted_Earnings_High95)
         colnames(train_forecast_High95) <- c("Dividend_per_share", "Earned_per_share")
         High95_forecast <- predict(regr, train_forecast_High95)
         
         # Low95
         
         Dividend_Forecast_Low95  <- Dividend_Per_Share$`Lo 95`
         
         Predicted_Earnings_Low95  <- Predicted_Earnings_Per_Share$`Lo 95`
         
         train_forecast_Low95 <- cbind(Dividend_Forecast_Low95, Predicted_Earnings_Low95)
         colnames(train_forecast_Low95) <- c("Dividend_per_share", "Earned_per_share")
         Low95_forecast <- predict(regr, train_forecast_Low95)
         
         Forecasted_Data_Total <- data_frame(Point_forecast, Low80_forecast, High80_forecast, Low95_forecast, High95_forecast)
         colnames(Forecasted_Data_Total) <- c("Point.Forecast", "Lo.80", "Hi.80", "Lo.95", "Hi.95")
         Forecasted_Data_Total <- as.data.frame(Forecasted_Data_Total)
         
         # 2022 
         Pred22 <- Forecasted_Data_Total[1,]
         Pred22 <- as.numeric(Pred22)
         sample.mean22 <- mean(Pred22)
         sample.n22 <- length(Pred22)
         sample.sd22 <- sd(Pred22)
         sample.se22 <- sample.sd22/sqrt(sample.n22)
         
         alpha = 0.05
         degrees.freedom = sample.n22 - 1
         t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
         
         
         margin.error22 <- t.score * sample.se22
         lower.bound22 <- sample.mean22 - margin.error22
         upper.bound22 <- sample.mean22 + margin.error22
         
         # 2023
         
         Pred23 <- Forecasted_Data_Total[2,]
         Pred23 <- as.numeric(Pred23)
         sample.mean23 <- mean(Pred23)
         sample.n23 <- length(Pred23)
         sample.sd23 <- sd(Pred23)
         sample.se23 <- sample.sd23/sqrt(sample.n23)
         
         margin.error23 <- t.score * sample.se23
         lower.bound23 <- sample.mean23 - margin.error23
         upper.bound23 <- sample.mean23 + margin.error23
         
         # 2024
         
         Pred24 <- Forecasted_Data_Total[3,]
         Pred24 <- as.numeric(Pred24)
         sample.mean24 <- mean(Pred24)
         sample.n24 <- length(Pred24)
         sample.sd24 <- sd(Pred24)
         sample.se24 <- sample.sd24/sqrt(sample.n24)
         
         margin.error24 <- t.score * sample.se24
         lower.bound24 <- sample.mean24 - margin.error24
         upper.bound24 <- sample.mean24 + margin.error24
         
         # 2025
         
         Pred25 <- Forecasted_Data_Total[4,]
         Pred25 <- as.numeric(Pred25)
         sample.mean25 <- mean(Pred25)
         sample.n25 <- length(Pred25)
         sample.sd25 <- sd(Pred25)
         sample.se25 <- sample.sd25/sqrt(sample.n25)
         
         margin.error25 <- t.score * sample.se25
         lower.bound25 <- sample.mean25 - margin.error25
         upper.bound25 <- sample.mean25 + margin.error25
         
         # 2026
         
         Pred26 <- Forecasted_Data_Total[5,]
         sample.n26 <- length(Pred26)
         Pred26 <- as.numeric(Pred26)
         sample.mean26 <- mean(Pred26)
         sample.sd26 <- sd(Pred26)
         sample.se26 <- sample.sd26/sqrt(sample.n26)
         
         margin.error26 <- t.score * sample.se26
         lower.bound26 <- sample.mean26 - margin.error26
         upper.bound26 <- sample.mean26 + margin.error26
         
         # 2027
         
         Pred27 <- Forecasted_Data_Total[6,]
         Pred27 <- as.numeric(Pred27)
         sample.n27 <- length(Pred27)
         sample.mean27 <- mean(Pred27)
         sample.sd27 <- sd(Pred27)
         sample.se27 <- sample.sd27/sqrt(sample.n27)
         
         margin.error27 <- t.score * sample.se27
         lower.bound27 <- sample.mean27 - margin.error27
         upper.bound27 <- sample.mean27 + margin.error27
         
         # 2028
         
         Pred28 <- Forecasted_Data_Total[7,]
         Pred28 <- as.numeric(Pred28)
         sample.n28 <- length(Pred28)
         sample.mean28 <- mean(Pred28)
         sample.sd28 <- sd(Pred28)
         sample.se28 <- sample.sd28/sqrt(sample.n28)
         
         margin.error28 <- t.score * sample.se28
         lower.bound28 <- sample.mean28 - margin.error28
         upper.bound28 <- sample.mean28 + margin.error28
         
         # Combine all 
         Hi.95 <- rbind(upper.bound22, upper.bound23,upper.bound24,upper.bound25,upper.bound26,upper.bound27,upper.bound28)
         Lo.95 <- rbind(lower.bound22, lower.bound23, lower.bound24, lower.bound25, lower.bound26, lower.bound27, lower.bound28)
         
         Date <- c("30/12/22", "30/12/23", "30/12/24", "30/12/25", "30/12/26", "30/12/27", "30/12/28")
         Date <- as.Date(Date, "%d/%m/%y")
         
         
         Forecasted_Data_Total <- data.frame(Forecasted_Data_Total$Point.Forecast, Hi.95, Lo.95, Date)
         
         colnames(Forecasted_Data_Total) <- c("Point.Forecast", "Hi.95", "Lo.95", "Date")
         rownames(Forecasted_Data_Total) <- NULL
         
         
         
         
         Forecasted_Data_Total <- Forecasted_Data_Total %>%
             gather(Type, Value, -Date)
         
         Forecasted_Data_Total$Value <- round(Forecasted_Data_Total$Value, 2)
         Forecasted_Data_Total$Value <- ifelse(Forecasted_Data_Total$Value < 0, abs(Forecasted_Data_Total$Value), Forecasted_Data_Total$Value)
         ggplot(Forecasted_Data_Total, aes(x=Date, y = Value, col = factor(Type))) +
             geom_line()+
             labs(x = "Date",
                  y = "Share Price",
                  color = "Legend") +
             geom_point() + geom_label_repel(aes(label = Value,
                                                 fill = Value), color = 'white',
                                             size = 5, show.legend = F) +
             scale_color_manual(values=c('green','red', "black")) +
             theme(axis.text.x = element_text(size = 10))  + theme(legend.text = element_text(size = 10)) +
             scale_x_date(date_breaks = "1 year")
     })
     
     output$stockpred2 <- renderPlot({
         
         # Point Forecast 
         Dividend_Per_Share <- Dividend_Per_Share()
         Dividend_Forecast <- Dividend_Per_Share$`Point Forecast`
         Predicted_Earnings_Per_Share <- Predicted_Earnings_Per_Share()
         Predicted_Earnings_Forecast <- Predicted_Earnings_Per_Share$`Point Forecast`
         
         train_forecast <- cbind(Dividend_Forecast, Predicted_Earnings_Forecast)
         colnames(train_forecast) <- c("Dividend_per_share", "Earned_per_share")
         Point_forecast <- predict(regr, train_forecast)
         
         # Lo 80
         
         Dividend_Forecast_Low80  <- Dividend_Per_Share$`Lo 80`
         
         Predicted_Earnings_Low80 <- Predicted_Earnings_Per_Share$`Lo 80`
         
         train_forecast_Low80 <- cbind(Dividend_Forecast_Low80, Predicted_Earnings_Low80)
         colnames(train_forecast_Low80) <- c("Dividend_per_share", "Earned_per_share")
         Low80_forecast <- predict(regr, train_forecast_Low80)
         
         # Hi 80
         
         Dividend_Forecast_High80  <- Dividend_Per_Share$`Hi 80`
         
         Predicted_Earnings_High80 <- Predicted_Earnings_Per_Share$`Hi 80`
         
         train_forecast_High80 <- cbind(Dividend_Forecast_High80, Predicted_Earnings_High80)
         colnames(train_forecast_High80) <- c("Dividend_per_share", "Earned_per_share")
         High80_forecast <- predict(regr, train_forecast_High80)
         
         # High 95 
         
         Dividend_Forecast_High95  <- Dividend_Per_Share$`Hi 95`
         
         Predicted_Earnings_High95 <- Predicted_Earnings_Per_Share$`Hi 95`
         
         train_forecast_High95 <- cbind(Dividend_Forecast_High95, Predicted_Earnings_High95)
         colnames(train_forecast_High95) <- c("Dividend_per_share", "Earned_per_share")
         High95_forecast <- predict(regr, train_forecast_High95)
         
         # Low95
         
         Dividend_Forecast_Low95  <- Dividend_Per_Share$`Lo 95`
         
         Predicted_Earnings_Low95  <- Predicted_Earnings_Per_Share$`Lo 95`
         
         train_forecast_Low95 <- cbind(Dividend_Forecast_Low95, Predicted_Earnings_Low95)
         colnames(train_forecast_Low95) <- c("Dividend_per_share", "Earned_per_share")
         Low95_forecast <- predict(regr, train_forecast_Low95)
         
         Forecasted_Data_Total <- data_frame(Point_forecast, Low80_forecast, High80_forecast, Low95_forecast, High95_forecast)
         colnames(Forecasted_Data_Total) <- c("Point.Forecast", "Lo.80", "Hi.80", "Lo.95", "Hi.95")
         Forecasted_Data_Total <- as.data.frame(Forecasted_Data_Total)
         
         # 2022 
         Pred22 <- Forecasted_Data_Total[1,]
         Pred22 <- as.numeric(Pred22)
         sample.mean22 <- mean(Pred22)
         sample.n22 <- length(Pred22)
         sample.sd22 <- sd(Pred22)
         sample.se22 <- sample.sd22/sqrt(sample.n22)
         
         alpha = 0.05
         degrees.freedom = sample.n22 - 1
         t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
         
         
         margin.error22 <- t.score * sample.se22
         lower.bound22 <- sample.mean22 - margin.error22
         upper.bound22 <- sample.mean22 + margin.error22
         
         # 2023
         
         Pred23 <- Forecasted_Data_Total[2,]
         Pred23 <- as.numeric(Pred23)
         sample.mean23 <- mean(Pred23)
         sample.n23 <- length(Pred23)
         sample.sd23 <- sd(Pred23)
         sample.se23 <- sample.sd23/sqrt(sample.n23)
         
         margin.error23 <- t.score * sample.se23
         lower.bound23 <- sample.mean23 - margin.error23
         upper.bound23 <- sample.mean23 + margin.error23
         
         # 2024
         
         Pred24 <- Forecasted_Data_Total[3,]
         Pred24 <- as.numeric(Pred24)
         sample.mean24 <- mean(Pred24)
         sample.n24 <- length(Pred24)
         sample.sd24 <- sd(Pred24)
         sample.se24 <- sample.sd24/sqrt(sample.n24)
         
         margin.error24 <- t.score * sample.se24
         lower.bound24 <- sample.mean24 - margin.error24
         upper.bound24 <- sample.mean24 + margin.error24
         
         # 2025
         
         Pred25 <- Forecasted_Data_Total[4,]
         Pred25 <- as.numeric(Pred25)
         sample.mean25 <- mean(Pred25)
         sample.n25 <- length(Pred25)
         sample.sd25 <- sd(Pred25)
         sample.se25 <- sample.sd25/sqrt(sample.n25)
         
         margin.error25 <- t.score * sample.se25
         lower.bound25 <- sample.mean25 - margin.error25
         upper.bound25 <- sample.mean25 + margin.error25
         
         # 2026
         
         Pred26 <- Forecasted_Data_Total[5,]
         sample.n26 <- length(Pred26)
         Pred26 <- as.numeric(Pred26)
         sample.mean26 <- mean(Pred26)
         sample.sd26 <- sd(Pred26)
         sample.se26 <- sample.sd26/sqrt(sample.n26)
         
         margin.error26 <- t.score * sample.se26
         lower.bound26 <- sample.mean26 - margin.error26
         upper.bound26 <- sample.mean26 + margin.error26
         
         # 2027
         
         Pred27 <- Forecasted_Data_Total[6,]
         Pred27 <- as.numeric(Pred27)
         sample.n27 <- length(Pred27)
         sample.mean27 <- mean(Pred27)
         sample.sd27 <- sd(Pred27)
         sample.se27 <- sample.sd27/sqrt(sample.n27)
         
         margin.error27 <- t.score * sample.se27
         lower.bound27 <- sample.mean27 - margin.error27
         upper.bound27 <- sample.mean27 + margin.error27
         
         # 2028
         
         Pred28 <- Forecasted_Data_Total[7,]
         Pred28 <- as.numeric(Pred28)
         sample.n28 <- length(Pred28)
         sample.mean28 <- mean(Pred28)
         sample.sd28 <- sd(Pred28)
         sample.se28 <- sample.sd28/sqrt(sample.n28)
         
         margin.error28 <- t.score * sample.se28
         lower.bound28 <- sample.mean28 - margin.error28
         upper.bound28 <- sample.mean28 + margin.error28
         
         # Combine all 
         Hi.95 <- rbind(upper.bound22, upper.bound23,upper.bound24,upper.bound25,upper.bound26,upper.bound27,upper.bound28)
         Lo.95 <- rbind(lower.bound22, lower.bound23, lower.bound24, lower.bound25, lower.bound26, lower.bound27, lower.bound28)
         
         Date <- c("30/12/22", "30/12/23", "30/12/24", "30/12/25", "30/12/26", "30/12/27", "30/12/28")
         Date <- as.Date(Date, "%d/%m/%y")
         
         
         Forecasted_Data_Total <- data.frame(Forecasted_Data_Total$Point.Forecast, Hi.95, Lo.95, Date)
         
         colnames(Forecasted_Data_Total) <- c("Point.Forecast", "Hi.95", "Lo.95", "Date")
         rownames(Forecasted_Data_Total) <- NULL

         Forecasted_Data_Total <- Forecasted_Data_Total %>%
             gather(Type, Value, -Date)
         
         Forecasted_Data_Total$Value <- round(Forecasted_Data_Total$Value, 2)
         Forecasted_Data_Total$Value <- ifelse(Forecasted_Data_Total$Value < 0, abs(Forecasted_Data_Total$Value), Forecasted_Data_Total$Value)
         ggplot(Forecasted_Data_Total, aes(x=Date, y = Value, col = factor(Type))) +
             geom_line()+
             labs(x = "Date",
                  y = "Share Price",
                  color = "Legend") +
             geom_point() + geom_label_repel(aes(label = Value,
                                                 fill = Value), color = 'white',
                                             size = 5, show.legend = F) +
             scale_color_manual(values=c('green','red', "black")) +
             theme(axis.text.x = element_text(size = 10))  + theme(legend.text = element_text(size = 10)) +
             scale_x_date(date_breaks = "1 year")
     })
     
}
# Run the application
shinyApp(ui, server)