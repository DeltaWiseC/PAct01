library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)


# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                     tags$img(src="images/Delta.png",width="180px",height="120px"),
                     tags$h2("ACCESO", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                     textInput("userName", placeholder="Usuario", label = tagList(icon("user"), "Usuario")),
                     passwordInput("passwd", placeholder="Contraseña", label = tagList(icon("unlock-alt"), "Contraseña")),
                     br(),
                     div(
                         style = "text-align: center;",
                         actionButton("login", "INGRESO", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                         shinyjs::hidden(
                             div(id = "nomatch",
                                 tags$p("Uups! Usuario o contraseña incorrectos!",
                                        style = "color: red; font-weight: 600;
                                            padding-top: 5px;font-size:16px;",
                                        class = "text-center"))),
                         br(),
                         br(),
                         tags$code("Username: myuser  Password: mypass"),
                         br(),
                         tags$code("Username: myuser1  Password: mypass1")
                     ))
)
Usuarios<-read.csv("www/Usuarios01.csv",header = TRUE,stringsAsFactors = FALSE)

credentials = data.frame(
    username_id = Usuarios$Usuario,
    passod   = sapply(Usuarios$Password,password_store),
    permission  = Usuarios$Permiso,
    stringsAsFactors = F
)


header <- dashboardHeader( title = "Practicas Actuariales", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "green")

server <- function(input, output, session) {

    login = FALSE
    USER <- reactiveValues(login = login)

    observe({
        if (USER$login == FALSE) {
            if (!is.null(input$login)) {
                if (input$login > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    if(length(which(credentials$username_id==Username))==1) {
                        pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
                        pasverify <- password_verify(pasmatch, Password)
                        if(pasverify) {
                            USER$login <- TRUE
                        } else {
                            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                        }
                    } else {
                        shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                        shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                    }
                }
            }
        }
    })

    output$logoutbtn <- renderUI({
        req(USER$login)
        tags$li(a(icon("fa fa-sign-out"), "Logout",
                  href="javascript:window.location.reload(true)"),
                class = "dropdown",
                style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
    })

    output$sidebarpanel <- renderUI({
        if (USER$login == TRUE ){
            sidebarMenu(
                menuItem("Practicas Actuariales", tabName = "dashboard", icon = icon("dashboard"))
            )
        }
    })

    output$body <- renderUI({
        if (USER$login == TRUE ) {
            tabItem(tabName ="dashboard", class = "active",

                    fluidRow(
                        tags$img(src="images/Delta.png",width="180px",height="120px"),
                        box(
                            p("Esta e una monserga")
                        )
                    ))
        }
        else {
            loginpage
        }
    })

    output$results <-  DT::renderDataTable({
        datatable(iris, options = list(autoWidth = TRUE,
                                       searching = FALSE))
    })

}

runApp(list(ui = ui, server = server), launch.browser = TRUE)
