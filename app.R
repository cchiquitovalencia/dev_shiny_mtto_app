library(shiny)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(ggplot2)
library(dplyr)

# Definir UI
ui <- fluidPage(
        
        # Título de app
        titlePanel("Planeación de Mantenimiento"),
        
        # Opciones para el modelo
        fluidPage(
                sidebarPanel(
                        numericInput("m", "Flota", value = 2, min = 1, max = 5, step = 1),
                        numericInput("H", "Periodos", value = 8, min = 4, max = 12, step = 1),
                        numericInput("D", "Demanda", value = 480, min = 200, max = 1000, step = 20),
                        numericInput("Ui", "Periodos entre MTTOs", value = 3, min = 1, max = 7, step = 1),
                        numericInput("Ti", "Horas entre MTTOs", value = 90, min = 50, max = 200, step = 10),
                        numericInput("xmax", "Máximo horas operación", value = 744, min = 500, max = 1000, step = 66),
                        actionButton("solucionar", "Obtener solución!"),
                        br(),
                        actionButton("graficar", "Graficar solución!")
                        , width = 3)
                ,
                
                mainPanel(
                        h4('Descripción del modelo'),
                        #verbatimTextOutput("periodosMtto"),
                        #verbatimTextOutput("horasMtto"),
                        verbatimTextOutput("imprimeModelo"),
                        verbatimTextOutput("imprimeResultado"),
                        plotOutput("grafica")
                )
                
        )
        
)

# Definir lógica del servidor para resolver el modelo
server <- function(input, output) {
        
        # Usado para mostrar variables internas
        output$periodosMtto <- renderPrint({
                rep(input$Ui, input$m)
        })
        
        output$horasMtto <- renderPrint({
                rep(input$Ti, input$m)
        })
        
        # Función para crear modelo planeación
        crearModelo <- reactive({
                
                # Repetir valores ingresados por usuario para Ui y Ti
                realUi <- rep(input$Ui, input$m)
                realTi <- rep(input$Ti, input$m)
                
                # Definir el modelo con ompr
                model <- MIPModel() |>
                        
                        # Variables
                        add_variable(y[i, t], i=1:input$m, t=1:input$H, type="binary") |>
                        add_variable(x[i, t], i=1:input$m, t=1:input$H, type="continuous", lb=0) |>
                        
                        # Función objetivo
                        set_objective(sum_expr(y[i, t], i=1:input$m, t=1:input$H), "min") |>
                        
                        # Restricciones
                        add_constraint(sum_expr(x[i, t], i=1:input$m, t=1:input$H) >= input$D) |>
                        add_constraint(sum_expr(y[i, j], j=tao:(tao+realUi[i])) >= 1, i=1:input$m, tao=1:(input$H-realUi[i])) |>
                        add_constraint(sum_expr(x[i, j], j=t:(t+u)) <= realTi[i] + realTi[i]*sum_expr(y[i, j], j=t:(t+u)), i=1:input$m, t=1:input$H, u=0:(input$H-t)) |>
                        add_constraint(x[i, t] + input$xmax*y[i, t] <= input$xmax, i=1:input$m, t=1:input$H)
                
                # Devolver el modelo         
                model
        })
        
        # Función para entregar características del modelo
        output$imprimeModelo <- renderPrint(crearModelo())
        
        # Ejecutar solución del modelo
        resultado <- eventReactive(
                input$solucionar, {
                        
                        # Activar solver
                        result <- solve_model(crearModelo(), with_ROI(solver = "glpk", verbose = TRUE))
                        result
                        
                })
        
        # Función para entregar características de la solución
        output$imprimeResultado <- renderPrint(resultado())
        
        # función para crear gráficas de la solución
        plotear <- eventReactive(
                input$graficar, {
                        
                        # Graficar solución
                        gridExtra::grid.arrange(
                                get_solution(resultado(), y[i,t]) |>
                                        mutate(Mtto = "Mant") |>
                                        rename(unidad = i, periodo = t) |>
                                        ggplot()+
                                        geom_point(aes(periodo, value, color = as.factor(value)), size = 5)+
                                        facet_grid(unidad~.)+
                                        hrbrthemes::theme_ipsum()+
                                        labs(x = "PERIODO", y = "VALOR", col = "MTTO")+
                                        theme(legend.position = "top"),
                                
                                
                                get_solution(resultado(), x[i,t]) |>
                                        mutate(Horas = "Opera") |>
                                        rename(unidad = i, periodo = t) |>
                                        ggplot()+
                                        geom_point(aes(periodo, value, color = as.factor(value)), size = 5)+
                                        facet_grid(unidad~.)+
                                        hrbrthemes::theme_ipsum()+
                                        labs(x = "PERIODO", y = "VALOR", col = "OPERA")+
                                        theme(legend.position = "top"),
                                
                                ncol = 2
                        )
                        
                })
        
        output$grafica <- renderPlot(plotear())
        
}

# Run the application 
shinyApp(ui = ui, server = server)
