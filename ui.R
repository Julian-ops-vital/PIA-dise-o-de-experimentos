# interfaz.R
library(shiny)
library(plotly)    # <-- agregar esta línea
library(DT)

ui <- navbarPage(
  title = "ANOVA Interactivo (Diseño Uni y Bifactorial)",
  
  # ---------------------- ANOVA UNIFACTORIAL ------------------------
  tabPanel(
    "ANOVA unifactorial",
    sidebarLayout(
      sidebarPanel(
        h3("Configuración del diseño unifactorial"),
        
        radioButtons(
          "fuente_datos_1",
          "¿Cómo quieres los datos?",
          choices = c("Simulados" = "sim",
                      "Cargar archivo (.csv)" = "file")
        ),
        
        conditionalPanel(
          condition = "input.fuente_datos_1 == 'sim'",
          numericInput("n_grupos_1", "Número de grupos:", value = 3, min = 2, max = 6),
          numericInput("n_por_grupo_1", "Observaciones por grupo:", value = 10, min = 3, max = 300),
          textInput("medias_1", "Medias separadas por comas:", value = "10, 12, 15"),
          numericInput("sd_1", "Desviación estándar:", value = 2, min = 0.1),
          numericInput("semilla_1", "Semilla:", value = 123)
        ),
        
        conditionalPanel(
          condition = "input.fuente_datos_1 == 'file'",
          fileInput("archivo_1", "Selecciona archivo CSV:", accept = ".csv"),
          textInput("nombre_respuesta_1", "Columna respuesta:", value = "y"),
          textInput("nombre_factor_1", "Columna factor:", value = "grupo")
        ),
        
        actionButton("btn_correr_1", "Correr ANOVA unifactorial", class = "btn-primary"),
        
        hr(),
        h4("Opciones de gráfica"),
        checkboxInput("mostrar_boxplot_1", "Boxplot por grupo", TRUE),
        checkboxInput("mostrar_diagnostico_1", "Gráfico de residuos", TRUE)
      ),
      
      mainPanel(
        h3("Resultados — ANOVA unifactorial"),
        
        tabsetPanel(
          tabPanel("Tabla ANOVA", DT::dataTableOutput("resumen_anova_1")),
          tabPanel("Resumen descriptivo", tableOutput("tabla_resumen_1")),
          tabPanel(
            "Gráficas",
            conditionalPanel(
              condition = "input.mostrar_boxplot_1 == true",
              plotlyOutput("plot_boxplot_1", height = "400px")
            ),
            conditionalPanel(
              condition = "input.mostrar_diagnostico_1 == true",
              plotlyOutput("plot_residuos_1", height = "350px")
            )
          ),
          tabPanel("Interpretación", verbatimTextOutput("interpretacion_1"))
        )
      )
    )
  ),
  
  # ---------------------- ANOVA BIFACTORIAL ------------------------
  tabPanel(
    "ANOVA bifactorial",
    sidebarLayout(
      sidebarPanel(
        h3("Configuración del diseño bifactorial"),
        
        radioButtons(
          "fuente_datos_2",
          "¿Cómo quieres los datos?",
          choices = c("Simulados" = "sim", "Cargar CSV" = "file")
        ),
        
        conditionalPanel(
          condition = "input.fuente_datos_2 == 'sim'",
          numericInput("niveles_A", "Niveles factor A:", value = 2, min = 2, max = 5),
          numericInput("niveles_B", "Niveles factor B:", value = 3, min = 2, max = 5),
          numericInput("n_por_celda", "Observaciones por celda:", value = 8),
          numericInput("media_global_2", "Media global:", value = 20),
          numericInput("sd_2", "Desviación estándar:", value = 3),
          numericInput("semilla_2", "Semilla:", value = 456),
          checkboxInput("incluir_interaccion", "Incluir interacción A:B", TRUE)
        ),
        
        conditionalPanel(
          condition = "input.fuente_datos_2 == 'file'",
          fileInput("archivo_2", "Selecciona archivo CSV:", accept = ".csv"),
          textInput("nombre_respuesta_2", "Columna respuesta:", value = "y"),
          textInput("nombre_factorA_2", "Columna factor A:", value = "A"),
          textInput("nombre_factorB_2", "Columna factor B:", value = "B")
        ),
        
        actionButton("btn_correr_2", "Correr ANOVA bifactorial", class = "btn-primary"),
        
        hr(),
        checkboxInput("mostrar_interaccion_2", "Gráfica de interacción", TRUE),
        checkboxInput("mostrar_boxplot_2", "Boxplot A:B", TRUE)
      ),
      
      mainPanel(
        h3("Resultados — ANOVA bifactorial"),
        
        tabsetPanel(
          tabPanel("Tabla ANOVA", DT::dataTableOutput("resumen_anova_2")),
          tabPanel("Resumen descriptivo", tableOutput("tabla_resumen_2")),
          tabPanel(
            "Gráficas",
            conditionalPanel(condition = "input.mostrar_interaccion_2 == true", plotlyOutput("plot_interaccion_2", height = "420px")),
            conditionalPanel(condition = "input.mostrar_boxplot_2 == true", plotlyOutput("plot_boxplot_2", height = "420px"))
          ),
          tabPanel("Interpretación", verbatimTextOutput("interpretacion_2"))
        )
      )
    )
  ),
  
  # ---------------------- ACERCA DE ------------------------
  tabPanel(
    "Acerca de",
    fluidPage(
      h2("Aplicación Shiny para ANOVA"),
      p("Esta app permite realizar ANOVA unifactorial y bifactorial con datos simulados o CSV."),
      p("Incluye tablas, gráficos, interpretación automática y opciones interactivas.")
    )
  )
)
