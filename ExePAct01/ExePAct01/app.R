library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(learnr)
library(devtools)


# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
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
                                 tags$p("Uups! Usuario o contraseña incorrectos! o excedio la fecha de realización",
                                        style = "color: red; font-weight: 600;
                                            padding-top: 5px;font-size:16px;",
                                        class = "text-center"))),
                         br()

                     ))
)
Usuarios<-read.csv("PAct01/www/Usuarios01.csv",header = TRUE,stringsAsFactors = FALSE)
Usuarios$Ultima <- as.Date(Usuarios$Ultima, "%d/%m/%Y")
Usuarios$Inicia <- as.Date(Usuarios$Inicia, "%d/%m/%Y")
Usuarios$Termina <- as.Date(Usuarios$Termina, "%d/%m/%Y")

Hoy<-Sys.Date()

credentials = data.frame(
    username_id = Usuarios$Usuario,
    passod   = sapply(Usuarios$Password,password_store),
    permission  = Usuarios$Permiso,
    Inicia=Usuarios$Inicia,
    Termina=Usuarios$Termina,
    Ultima=Usuarios$Ultima,
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
                        if (credentials["Termina"][which(credentials$username_id==Username),]>=Hoy &
                            credentials["Ultima"][which(credentials$username_id==Username),]<=Hoy) {
                            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]} else{
                            pasmatch  <- credentials["passod"][which(credentials$username_id=="Martin"),]}
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
            if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="participante") {
                sidebarMenu(
                    menuItem("Instalar", tabName = "Instala", icon = icon("dashboard")),
                    menuItem("Ejecutar", tabName = "Ejecuta", icon = icon("th")),
                    menuItem("Enviar Ejercicios", tabName = "Envia", icon = icon("th"))
                )
            }
            else{
                sidebarMenu(
                    menuItem("Revisar Usuarios", tabName = "dashboard", icon = icon("dashboard")),
                    uiOutput('Otro')
                )

            }
        }
    })

    output$body <- renderUI({
        if (USER$login == TRUE ) {
            if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="participante") {
                tabItems(
                    if (sidebarMenu$input=="Instala") {
                    tabItem(tabName ="Ejecuta", class = "active",
                            fluidRow(
                          h3("Espacio para enviar"))
                    )} else if (sidebarMenu$input=="Ejecuta") {
                    tabItem(tabName ="Instala", class = "active",
                        fluidRow(
 #                           library(learnr),
                            h3("Instalando .. "))
                        )} else if (sidebarMenu$input=="Envia") {
                    tabItem(tabName ="Envia",
                          install_github("DeltaWiseC/PAct01",quiet=TRUE,echo=FALSE),
                            h3("This is second tab"))}
                    )
            }
            else {
                tabItem(
                    tabName ="dashboard", class = "active",
                    fluidRow(
                        )
                    )
            }
        }
        else {
            loginpage
        }
    })


}

runApp(list(ui = ui, server = server), launch.browser = TRUE)
