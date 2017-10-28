library(shiny)
library(shinydashboard)
library(ggplot2)
library(RSQLite)
library(shinyjs)
library(xts)
library(dygraphs)
library(V8)

jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"
ui <- dashboardPage(
  dashboardHeader(title = "OntoREA© Prototype"),
  
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Forward Pricing",
      tabName = "forwardpricing",
      icon = icon("balance-scale")
    ),
    menuItem(
      "Option Pricing",
      tabName = "optionpricing",
      badgeColor = "red",
      badgeLabel = "help!",
      icon = icon("road")
    ),
    menuItem("Table Explorer",
             tabName = "tableexplorer",
             icon = icon("gear"))
    
    
  ),
  actionButton("reset_db", "Reset Database")),
  
  dashboardBody(
    #necessary for remote box-collapsing
    useShinyjs(),
    extendShinyjs(text = jscode),
    
    tabItems(
      tabItem(
        tabName = "forwardpricing",
        h2("Forward Contracting"),
        fluidRow(
          box(
            id = "box_Initial_Pricing",
            title = "Initial Pricing",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            #actionButton("initialReadFile", "Load initial values"),
            column(
              3,
              textInput("ti_Type_Of_Stock_Derivative", "Type Of Stock Derivative", "0"),
              textInput("ti_Stock_ISIN", "Stock ISIN", "AT0001"),
              textInput("ti_Contract_Size", "Contract Size", "1")
            ),
            
            column(
              3,
              textInput("ti_Number_Of_Contracts", "Number Of Contracts", "1"),
              textInput("ti_Exercise_Or_Forward_Price", "Exercise Or Forward Price", "100"),
              dateInput(
                "ti_Contracting_Date",
                "Contracting Date",
                value = "2020-01-01",
                min = "2020-01-01"
              )
            ),
            
            column(
              3,
              dateInput(
                "ti_Expiration_Date",
                "Expiration Date",
                value = "2020-12-31",
                min = "2020-01-01"
              ),
              textInput("ti_Interest_Rate", "Interest Rate in %", "4"),
              textInput("ti_Stock_Volatility", "Stock Volatility in %", "0")
                   ),
            
            column(
              3,
              textInput("ti_Mark_To_Model", "Mark To Model", "1"),
              actionButton("ab_Initial_Pricing", "Finish Initial Pricing")
            )
            ),
          
          box(
            id = "box_Do",
            title = "First Step (Do)",
            width = 3,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            #  title = "Subsequent Pricings",width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE,
            
            dateInput(
              "ti_Do_timestamp",
              "Date",
              value = "2020-01-01",
              min = "2020-01-01"
            ),
            textInput(
              "ti_Do_Stock_Price",
              "Stock Price",
              value = 100
            ),
            actionButton("button_Do", "Do")
            
            
          ),
          
          box(
            id = "box_Plan",
            title = "Second Step (Plan)",
            width = 3,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            p("N(d1) = 1"),
            actionButton("button_Plan", "Plan")
          ),
          
          box(
            id = "box_Check",
            title = "Third Step (Check)",
            width = 3,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            p("ΔN(d1) = 0"),
            actionButton("button_Check", "Check")
          ),
          
          box(
            id = "box_Act",
            title = "Fourth Step (Act)",
            width = 3,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            actionButton("button_Act", "Act"),
            p("Forward: No action possible"),
            actionButton("button_Act_Continue", "Continue")
          ),
          
          box(
            title = "Timeline",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            dygraphOutput("timeline", height = 250)
          )
        )
      ),
      
      tabItem(tabName = "optionpricing",
              h2("Option Pricing")),
      
      tabItem(
        tabName = "tableexplorer",
        h2("Table Explorer"),
        fluidRow(

          box(
            title = "Table: Stock_Pricing_Dynamic",
            width = 12,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            actionButton("load_table_Stock_Pricing_Dynamic", "Load from database"),
            dataTableOutput('table_Stock_Pricing_Dynamic')
          ),
          
          box(
            title = "Table: Stock_Information_Static",
            width = 12,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            actionButton("load_table_Stock_Information_Static", "Load from database"),
            dataTableOutput('table_Stock_Information_Static')
          )
          ,
          
          box(
            title = "Table: Stock_Derivative_Static",
            width = 12,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            actionButton("load_table_Stock_Derivative_Static", "Load from database"),
            dataTableOutput('table_Stock_Derivative_Static')
          ),
          
          box(
            title = "Table: Derivative_Instrument_Dynamic",
            width = 12,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            actionButton("load_table_Derivative_Instrument_Dynamic", "Load from database"),
            dataTableOutput('table_Derivative_Instrument_Dynamic')
          ),
          
          box(
            title = "Table: Economic_Resource_Risky_Income",
            width = 12,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            actionButton("load_table_Economic_Resource_Risky_Income", "Load from database"),
            dataTableOutput('table_Economic_Resource_Risky_Income')
          ),
          
          box(
            title = "Table: Economic_Resource_Fixed_Income",
            width = 12,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            actionButton("load_table_Economic_Resource_Fixed_Income", "Load from database"),
            dataTableOutput('table_Economic_Resource_Fixed_Income')
          ),
          
          
          box(
            title = "Table: Asset",
            width = 12,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            actionButton("load_table_Asset", "Load from database"),
            dataTableOutput('table_Asset')
          ),
          
          box(
            title = "Table: Liability",
            width = 12,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            actionButton("load_table_Liability", "Load from database"),
            dataTableOutput('table_Liability')
          ),
          
          box(
            title = "Table: Off_Balance",
            width = 12,
            align = "center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            actionButton("load_table_Off_Balance", "Load from database"),
            dataTableOutput('table_Off_Balance')
          )
        )
      )
    )
  )
)
