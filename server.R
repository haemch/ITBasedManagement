library(shiny)
library(shinydashboard)
library(ggplot2)
library(RSQLite)
library(shinyjs)
library(xts)
library(dygraphs)
library(V8)

#necessary for remote box-collapsing
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"
sqlite <- dbConnect(SQLite(), "db.sqlite")


server <- function(input, output, session) {
  
  ##################################################################
  ## FW Pricing
  ##################################################################
  
  observeEvent(input$ab_Initial_Pricing, {
    js$collapse("box_Do")
    hide(id = "box_Initial_Pricing", anim = FALSE)
    
    temp_db_Stock_Derivative_Static <-
      cbind.data.frame(
        input$ti_Type_Of_Stock_Derivative,
        input$ti_Stock_ISIN,
        input$ti_Exercise_Or_Forward_Price,
        as.character(input$ti_Contracting_Date),
        as.character(input$ti_Expiration_Date),
        input$ti_Contract_Size,
        input$ti_Number_Of_Contracts,
        input$ti_Stock_Volatility,
        input$ti_Interest_Rate,
        input$ti_Mark_To_Model
      )
    names(temp_db_Stock_Derivative_Static) <-
      c(
        "Type_Of_Stock_Derivative",
        "Stock_ISIN",
        "Exercise_Or_Forward_Price",
        "Contracting_Date",
        "Expiration_Date",
        "Contract_Size",
        "Number_Of_Contracts",
        "Stock_Volatility",
        "Interest_Rate",
        "Mark_To_Model"
      )
    dbWriteTable(sqlite,
                 "Stock_Derivative_Static",
                 temp_db_Stock_Derivative_Static,
                 append = TRUE)
  })
  observeEvent(input$button_Do, {
    temp_db_Stock_Pricing_Dynamic <-
      cbind.data.frame(
        input$ti_Stock_ISIN,
        input$ti_Do_Stock_Price,
        as.character(input$ti_Do_timestamp)
      )
    names(temp_db_Stock_Pricing_Dynamic) <-
      c("Stock_ISIN",
        "Stock_Price",
        "timestamp")
    dbWriteTable(sqlite,
                 "Stock_Pricing_Dynamic",
                 temp_db_Stock_Pricing_Dynamic,
                 append = TRUE)
    
    
    js$collapse("box_Plan")
  })
  
  observeEvent(input$button_Plan, {
    
    output$to_Plan <- renderText("N(d1) = 1")
    js$collapse("box_Check")
  })
  
  #https://stackoverflow.com/questions/19611254/r-shiny-disable-able-shinyui-elements
  
  
  observeEvent(input$button_Check, {
    output$to_Check <- renderText("Delta N(d1) = 0")
    js$collapse("box_Act")
  })
  
  observeEvent(input$button_Act, {
    output$to_Act <- renderText("Forward: No action possible")
    v$doCalcAndPlot <- input$button_Act #CalcAndPlot
  })
  
  observeEvent(input$button_Act_Continue, {
    js$collapse("box_Act")
    js$collapse("box_Plan")
    js$collapse("box_Check")
    
    output$to_Plan <- renderText("")
    output$to_Check <- renderText("")
    output$to_Act <- renderText("")
    
  })
  
  v <- reactiveValues(doCalcAndPlot = FALSE) #recalc and redraw
  
  output$timeline <- renderDygraph({
    if (v$doCalcAndPlot == FALSE)
      return()
    isolate({
      temp_db_draw <- dbReadTable(sqlite, "Stock_Pricing_Dynamic")
      temp_db_draw$Pricing_Date <-
        as.Date(as.POSIXct(temp_db_draw$timestamp))
      
      #legacy calc
      temp_db_draw$TtM <-
        as.numeric(difftime(
          as.Date(isolate(input$ti_Expiration_Date)),
          as.Date(temp_db_draw$Pricing_Date),
          unit = "weeks"
        )) / 52.1775
      temp_db_draw$Interest_Rate <-
        as.numeric(input$ti_Interest_Rate) / 100
      temp_db_draw$Interest_Rate_Cont <-
        log(1 + temp_db_draw$Interest_Rate)
      temp_db_draw$F_Price <-
        temp_db_draw[1, 3] * (1 + as.numeric(input$ti_Interest_Rate) / 100) ^ (as.numeric(difftime(
          as.Date(input$ti_Expiration_Date),
          as.Date(input$ti_Contracting_Date),
          unit = "weeks"
        )) / 52.1775)
      
      ##################################################################
      ## Calculation
      ##################################################################
      
      temp_db_draw$Liability <-
        -temp_db_draw$F_Price * exp(-temp_db_draw$Interest_Rate_Cont * temp_db_draw$TtM)
      temp_db_draw$Asset <- temp_db_draw$Stock_Price
      temp_db_draw$'Forward Value' <-
        round(temp_db_draw$Liability + temp_db_draw$Stock_Price, 1)

      #Composing XTS
      temp_xts_draw <-
        xts(x = temp_db_draw[, c("Asset", "Liability", "Forward Value")], order.by =
              temp_db_draw[, 5])
      
      #Derivative_Instrument_Dynamic entry
      temp_Stock_Derivative_Static <-
        dbReadTable(sqlite, "Stock_Derivative_Static")
      temp_db_Derivative_Instrument_Dynamic <-
        cbind.data.frame(
          tail(temp_Stock_Derivative_Static$Stock_Derivative_Static_ID, 1),
          as.character(input$ti_Do_timestamp),
          tail(temp_db_draw$'Forward Value', 1)
        )
      names(temp_db_Derivative_Instrument_Dynamic) <-
        c("Stock_Derivative_Static_ID",
          "timestamp",
          "Fair_Value")
      dbWriteTable(
        sqlite,
        "Derivative_Instrument_Dynamic",
        temp_db_Derivative_Instrument_Dynamic,
        append = TRUE
      )
      
      #Economic_Resource_Risky_Income entry
      temp_Derivative_Instrument_Dynamic <-
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      temp_db_Economic_Resource_Risky_Income <-
        cbind.data.frame(
          tail(
            temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
            1
          ),
          as.character(input$ti_Do_timestamp),
          1,
          tail(temp_db_draw$'Asset', 1),
          1
        )
      names(temp_db_Economic_Resource_Risky_Income) <-
        c(
          "Derivative_Instrument_Dynamic_ID",
          "timestamp",
          "Nd1t",
          "Value",
          "Asset_Or_Liability"
        )
      dbWriteTable(
        sqlite,
        "Economic_Resource_Risky_Income",
        temp_db_Economic_Resource_Risky_Income,
        append = TRUE
      )
      
      #Economic_Resource_Fixed_Income entry
      temp_Derivative_Instrument_Dynamic <-
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      temp_db_Economic_Resource_Fixed_Income <-
        cbind.data.frame(
          tail(
            temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
            1
          ),
          as.character(input$ti_Do_timestamp),
          tail(temp_db_draw$'Liability', 1),
          1
        )
      names(temp_db_Economic_Resource_Fixed_Income) <-
        c(
          "Derivative_Instrument_Dynamic_ID",
          "timestamp",
          "Present_Value",
          "Asset_Or_Liability"
        )
      dbWriteTable(
        sqlite,
        "Economic_Resource_Fixed_Income",
        temp_db_Economic_Resource_Fixed_Income,
        append = TRUE
      )
      
      #Asset, Liability of Off Balance
      if (tail(temp_db_draw$'Forward Value', 1) > 0) {
        #Asset
        temp_Derivative_Instrument_Dynamic <-
          dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
        temp_db_asset <-
          cbind.data.frame(
            tail(
              temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
              1
            ),
            as.character(input$ti_Do_timestamp),
            tail(temp_Derivative_Instrument_Dynamic$Fair_Value, 1)
          )
        names(temp_db_asset) <-
          c("Derivative_Instrument_Dynamic_ID",
            "timestamp",
            "Fair_Value")
        dbWriteTable(sqlite, "Asset", temp_db_asset, append = TRUE)
      } else if (tail(temp_db_draw$'Forward Value', 1) < 0) {
        #Liability
        temp_Derivative_Instrument_Dynamic <-
          dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
        temp_db_liability <-
          cbind.data.frame(
            tail(
              temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
              1
            ),
            as.character(input$ti_Do_timestamp),
            tail(temp_Derivative_Instrument_Dynamic$Fair_Value, 1)
          )
        names(temp_db_liability) <-
          c("Derivative_Instrument_Dynamic_ID",
            "timestamp",
            "Fair_Value")
        dbWriteTable(sqlite, "Liability", temp_db_liability, append = TRUE)
      }
      else {
        # Off_Balance
        temp_Derivative_Instrument_Dynamic <-
          dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
        temp_db_off_balance <-
          cbind.data.frame(
            tail(
              temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
              1
            ),
            as.character(input$ti_Do_timestamp)
          )
        names(temp_db_off_balance) <-
          c("Derivative_Instrument_Dynamic_ID",
            "timestamp")
        dbWriteTable(sqlite, "Off_Balance", temp_db_off_balance, append = TRUE)
      }
      
      #Plotting XTS
      dygraph(temp_xts_draw) %>%
        dyRangeSelector()
    })
  })
  
  ##################################################################
  ## Calculate d1 and d2
  ## P...Stock price
  ## X...Exercise price
  ## r... interest rate
  ## v... volatility
  ## t... time to maturity
  ## d1ord2... if 1 -> d1 , if 2 -> d2 is calculated
  ##################################################################
  
  calculate_d <- function(P,X,r,v,t, d1ord2){
    if(d1ord2 == 1){
      return((log(P/X) + (r+v^2/2)*t)/(v*sqrt(t)))
    } 
    if(d1ord2==2){
      return((log(P/X)+(r-v^2/2)*t)/(v*sqrt(t)))
    }
    return(NULL)
  }
  
  ##################################################################
  ## Option Pricing
  ##################################################################
  observeEvent(input$ab_Initial_Pricing_OP, {
    js$collapse("box_Do_OP")
    hide(id = "box_Initial_Pricing_OP", anim = FALSE)
    
    ## Create table input for Stock_Derivative_Static
    temp_db_Stock_Derivative_Static_OP <-
      cbind.data.frame(
        input$ti_Type_Of_Stock_Derivative_OP,
        input$ti_Stock_ISIN_OP,
        input$ti_Exercise_Or_Forward_Price_OP,
        as.character(input$ti_Contracting_Date_OP),
        as.character(input$ti_Expiration_Date_OP),
        input$ti_Contract_Size_OP,
        input$ti_Number_Of_Contracts_OP,
        input$ti_Stock_Volatility_OP,
        input$ti_Interest_Rate_OP,
        input$ti_Mark_To_Model_OP
      )
    names(temp_db_Stock_Derivative_Static_OP) <-
      c(
        "Type_Of_Stock_Derivative",
        "Stock_ISIN",
        "Exercise_Or_Forward_Price",
        "Contracting_Date",
        "Expiration_Date",
        "Contract_Size",
        "Number_Of_Contracts",
        "Stock_Volatility",
        "Interest_Rate",
        "Mark_To_Model"
      )
    dbWriteTable(sqlite,
                 "Stock_Derivative_Static",
                 temp_db_Stock_Derivative_Static_OP,
                 append = TRUE)
    
    ## Calculate asset and liability part of FV
    p <- as.double(as.character(temp_db_Stock_Derivative_Static_OP$Exercise_Or_Forward_Price))
    r <- as.double(as.character(temp_db_Stock_Derivative_Static_OP$Interest_Rate))/100
    v <- as.double(as.character(temp_db_Stock_Derivative_Static_OP$Stock_Volatility))/100
    asset <-  p * pnorm(calculate_d(p,p,r,v,1,1))
    liability <- p * exp(-r) * pnorm(calculate_d(p,p,r,v,1,2))
    
    
    ## Get key for Derivative_Instrument_Dynamic foreign key
    temp_Stock_Derivative_Static <-
      dbReadTable(sqlite, "Stock_Derivative_Static")
    
    ## Create table input for table Derivative_Instrument_Dynamic
    temp_db_Derivative_Instrument_Dynamic <- 
      cbind.data.frame(
        tail(temp_Stock_Derivative_Static,1)[,1],
        as.character(temp_db_Stock_Derivative_Static_OP$Contracting_Date),
        asset - liability
      )
    names(temp_db_Derivative_Instrument_Dynamic) <- 
      c(
        "Stock_Derivative_Static_ID",
        "timestamp",
        "Fair_Value"
      )
    dbWriteTable(sqlite,
                 "Derivative_Instrument_Dynamic",
                 temp_db_Derivative_Instrument_Dynamic,
                 append=TRUE
                 )
    
    ## Private foreign key Derivative_Instrument_Dynamic_ID
    temp_db_Derivative_Instrument_Dynamic <-
      dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
    pfk <- tail(temp_db_Derivative_Instrument_Dynamic,1)[,1]
  
    ## Write asset to Economic_Resource_Risky_Income
    temp_db_Economic_Resource_Risky_Income <-
      cbind.data.frame(
          pfk,
          as.character(temp_db_Stock_Derivative_Static_OP$Contracting_Date),
          pnorm(calculate_d(p,p,r,v,1,1)),
          asset,
          0
      )
    names(temp_db_Economic_Resource_Risky_Income) <-
      c(
        "Derivative_Instrument_Dynamic_ID",
        "timestamp",
        "Nd1t",
        "Value",
        "Asset_Or_Liability"
      )
    dbWriteTable(sqlite,
                 "Economic_Resource_Risky_Income",
                 temp_db_Economic_Resource_Risky_Income,
                 append= TRUE
                 )
    
    ## Write liability to Economic_Resource_Fixed_Income
    temp_db_Economic_Resource_Fixed_Income <-
      cbind.data.frame(
        pfk,
        as.character(temp_db_Stock_Derivative_Static_OP$Contracting_Date),
        liability,
        1
      )
    names(temp_db_Economic_Resource_Fixed_Income) <-
      c(
        "Derivative_Instrument_Dynamic_ID",
        "timestamp",
        "Present_Value",
        "Asset_Or_Liability"
      )
    dbWriteTable(sqlite,
                 "Economic_Resource_Fixed_Income",
                 temp_db_Economic_Resource_Fixed_Income,
                 append= TRUE
    )
    
  })
  
  ## Do Button
  observeEvent(input$button_Do_OP, {
    temp_db_Stock_Pricing_Dynamic_OP <-
      cbind.data.frame(
        input$ti_Stock_ISIN_OP,
        input$ti_Do_Stock_Price_OP,
        as.character(input$ti_Do_timestamp_OP)
      )
    names(temp_db_Stock_Pricing_Dynamic_OP) <-
      c("Stock_ISIN",
        "Stock_Price",
        "timestamp")
    dbWriteTable(sqlite,
                 "Stock_Pricing_Dynamic",
                 temp_db_Stock_Pricing_Dynamic_OP,
                 append = TRUE)
    
    
    js$collapse("box_Plan_OP")
  })
  
  
  ## PLan Button
  observeEvent(input$button_Plan_OP, {
    db_Stock_Pricing_Dynamic <- dbReadTable(sqlite, "Stock_Pricing_Dynamic")
    stock_Price <- tail(db_Stock_Pricing_Dynamic,1)[,3]
    stock_Date <- tail(db_Stock_Pricing_Dynamic,1)[,4]
    db_Stock_Derivative_Static <- dbReadTable(sqlite, "Stock_Derivative_Static")
    last_Entry <- tail(db_Stock_Derivative_Static,1)
    
    p <- as.double(as.character(last_Entry$Exercise_Or_Forward_Price))
    r <- as.double(as.character(last_Entry$Interest_Rate))/100
    v <- as.double(as.character(last_Entry$Stock_Volatility))/100
    time_to_maturity <- 
        as.numeric(difftime(
    as.Date(last_Entry$Expiration_Date),
    as.Date(stock_Date),
    unit = "weeks"
  )) / 52.1775
    nd1 <- pnorm(calculate_d(stock_Price,p,r,v,time_to_maturity,1))
    asset <-  p * nd1
    liability <- p * exp(-r) * pnorm(calculate_d(stock_Price,p,r,v,time_to_maturity,2))
    
    output$to_Plan_OP <- renderText(paste("N(d1) =", round(nd1*100,2), "%"))
    output$to_Plan_OP_Risky_Income <- renderText(paste("Asset =", round(asset,2)))
    output$to_Plan_OP_Fixed_Income <- renderText(paste("Liability =", round(liability,2)))
    js$collapse("box_Check_OP")
  })
  
  
  observeEvent(input$button_Check, {
    output$to_Check <- renderText("Delta N(d1) = 0")
    js$collapse("box_Act_OP")
  })
  
  
  ##################################################################
  ## Table Explorer
  ##################################################################
  
  observeEvent(
    input$load_table_Stock_Pricing_Dynamic,
    output$table_Stock_Pricing_Dynamic <- renderDataTable({
      dbReadTable(sqlite, "Stock_Pricing_Dynamic")
    })
  )
  
  observeEvent(
    input$load_table_Stock_Information_Static,
    output$table_Stock_Information_Static <- renderDataTable({
      dbReadTable(sqlite, "Stock_Information_Static")
    })
  )
  
  observeEvent(
    input$load_table_Stock_Derivative_Static,
    output$table_Stock_Derivative_Static <-
      renderDataTable({
        dbReadTable(sqlite, "Stock_Derivative_Static")
      })
  )
  
  observeEvent(
    input$load_table_Derivative_Instrument_Dynamic,
    output$table_Derivative_Instrument_Dynamic <-
      renderDataTable({
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      })
  )
  
  observeEvent(
    input$load_table_Economic_Resource_Risky_Income,
    output$table_Economic_Resource_Risky_Income <-
      renderDataTable({
        dbReadTable(sqlite, "Economic_Resource_Risky_Income")
      })
  )
  
  observeEvent(
    input$load_table_Economic_Resource_Fixed_Income,
    output$table_Economic_Resource_Fixed_Income <-
      renderDataTable({
        dbReadTable(sqlite, "Economic_Resource_Fixed_Income")
      })
  )
  
  observeEvent(input$load_table_Asset,
               output$table_Asset <- renderDataTable({
                 dbReadTable(sqlite, "Asset")
               }))
  
  observeEvent(input$load_table_Liability,
               output$table_Liability <- renderDataTable({
                 dbReadTable(sqlite, "Liability")
               }))
  
  observeEvent(input$load_table_Off_Balance,
               output$table_Off_Balance <- renderDataTable({
                 dbReadTable(sqlite, "Off_Balance")
               }))
  
 ##################################################################
 ## Reset DB
 ##################################################################
  
  observeEvent(input$reset_db, {
    dbSendStatement(sqlite, "DELETE from Stock_Derivative_Static")
    dbSendStatement(sqlite, "DELETE from Stock_Pricing_Dynamic")
    dbSendStatement(sqlite, "DELETE from Derivative_Instrument_Dynamic")
    dbSendStatement(sqlite, "DELETE from Economic_Resource_Risky_Income")
    dbSendStatement(sqlite, "DELETE from Economic_Resource_Fixed_Income")
    dbSendStatement(sqlite, "DELETE from Asset")
    dbSendStatement(sqlite, "DELETE from Liability")
    dbSendStatement(sqlite, "DELETE from Off_Balance")
  })
  
  
  
}