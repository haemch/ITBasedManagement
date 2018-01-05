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
      icon = icon("road")
    ),
    menuItem("Table Explorer",
             tabName = "tableexplorer",
             icon = icon("gear")),
    menuItem("User Guide and Documentation",
             tabName = "userinfo",
             icon = icon("info"))
    
    
  ),
  actionButton("reset_db", "Reset Database")),
  
  dashboardBody(
    #necessary for remote box-collapsing
    useShinyjs(),
    extendShinyjs(text = jscode),
    
    ##################################################################
    ## FW Pricing
    ##################################################################
    
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
            actionButton("button_Plan", "Plan"),
            verbatimTextOutput("to_Plan", placeholder = TRUE)
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
            actionButton("button_Check", "Check"),
            verbatimTextOutput("to_Check", placeholder = TRUE)
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
            verbatimTextOutput("to_Act", placeholder = TRUE),
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
      
      ##################################################################
      ## Option Pricing
      ##################################################################
      
      tabItem(tabName = "optionpricing",
              h2("Option Pricing"),
              fluidRow(
                box(
                  id = "box_Initial_Pricing_OP",
                  title = "Initial Pricing",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  column(
                    3,
                    textInput("ti_Type_Of_Stock_Derivative_OP", "Type Of Stock Derivative", "1"),
                    textInput("ti_Stock_ISIN_OP", "Stock ISIN", "AT0001"),
                    textInput("ti_Contract_Size_OP", "Contract Size", "1")
                  ),
                  
                  column(
                    3,
                    textInput("ti_Number_Of_Contracts_OP", "Number Of Contracts", "1"),
                    textInput("ti_Exercise_Or_Forward_Price_OP", "Exercise Or Forward Price", "100"),
                    dateInput(
                      "ti_Contracting_Date_OP",
                      "Contracting Date",
                      value = "2020-01-01",
                      min = "2020-01-01"
                    )
                  ),
                  
                  column(
                    3,
                    dateInput(
                      "ti_Expiration_Date_OP",
                      "Expiration Date",
                      value = "2020-12-31",
                      min = "2020-01-01"
                    ),
                    textInput("ti_Interest_Rate_OP", "Interest Rate in %", "5"),
                    textInput("ti_Stock_Volatility_OP", "Stock Volatility in %", "20")
                  ),
                  
                  column(
                    3,
                    textInput("ti_Mark_To_Model_OP", "Mark To Model", "1"),
                    actionButton("ab_Initial_Pricing_OP", "Finish Initial Pricing")
                  )
                ),
                
                box(
                  id = "box_Do_OP",
                  title = "First Step (Do)",
                  width = 3,
                  align = "center",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  
                  dateInput(
                    "ti_Do_timestamp_OP",
                    "Date",
                    value = "2020-01-01",
                    min = "2020-01-01"
                  ),
                  textInput(
                    "ti_Do_Stock_Price_OP",
                    "Stock Price",
                    value = 100
                  ),
                  actionButton("button_Do_OP", "Do")
                  
                  
                ),
                
                box(
                  id = "box_Plan_OP",
                  title = "Second Step (Plan)",
                  width = 3,
                  align = "center",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  actionButton("button_Plan_OP", "Plan"),
                  verbatimTextOutput("to_Plan_OP", placeholder = TRUE),
                  verbatimTextOutput("to_Plan_OP_Risky_Income", placeholder = TRUE),
                  verbatimTextOutput("to_Plan_OP_Fixed_Income", placeholder = TRUE)
                ),
                
                box(
                  id = "box_Check_OP",
                  title = "Third Step (Check)",
                  width = 3,
                  align = "center",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  actionButton("button_Check_OP", "Check"),
                  verbatimTextOutput("to_Check_OP", placeholder = TRUE)
                ),
                
                box(
                  id = "box_Act_OP",
                  title = "Fourth Step (Act)",
                  width = 3,
                  align = "center",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  actionButton("button_Act_OP", "Act"),
                  verbatimTextOutput("to_Act_OP", placeholder = TRUE),
                  actionButton("button_Act_Continue_OP", "Continue")
                ),
                
                box(
                  title = "Timeline",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  dygraphOutput("timeline_OP", height = 250)
                )
              )
              ),
      
      ##################################################################
      ## Table Explorer
      ##################################################################
      
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
      ),
      
      ##################################################################
      ## User Guide and Documentation
      ##################################################################
      
      tabItem(
        tabName = "userinfo",
        h2("User Guide and Documentation"),
        fluidRow(
          
          p("Initial Pricing: Enter initial pricing data"),
          p("Do: Enter date and stock price"),
          p("Plan: "),
          p("Check: Proposal for rebalancing portfolio"),
          p("Act: Perform the proposed rebalancing of portfolio"),
          
          box(
            title = "Forward Pricing",
            width = 12,
            align = "left",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE
          ),
          box(
            title = "Option Pricing",
            width = 12,
            align = "left",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            img(src="forward-formular.png", align="center", width=700, height=400),
            p("Time to maturity (in y) T"),
            p("Price of underlying P"),
            p("Exercise price X"),
            p("Interest rate (cont.) r"),
            p("Volatility (p.a.): sigma"),
            p("Aux. Variable: d1"),
            p("Asset weight N(d1)"),
            p("Aux. Variable: d2"),
            p("Loan weight N(d2)")
          ),
          box(
            title = "Table Explorer",
            width = 12,
            align = "left",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            strong("With the table explorer the user is enabled to explore the tables of the underlying database.
               The names of the boxes relate to the database table."),
            br(),
            br(),
            em("The following image displays the structure of the database:"),
            hr(),
            div(img(src="db.png", align="center", width=700, height=400), align="center"),
            hr(),
            img(src="reset_db_button.png", width=90, height=25),
            br(),
            br(),
            strong("The 'Reset Database' button deletes all the entries in the database except the tables which are considered as Masterdata.")
            
            
          )
        )
      )
    )
  )
)
