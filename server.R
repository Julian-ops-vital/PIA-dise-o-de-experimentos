# server.R
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)    # <-- agregado
library(DT)        # <-- agregado

# ---------------- FUNCIONES -----------------------------

# 1) Genera datos unifactoriales
generar_datos_unifactorial <- function(n_grupos, n_por_grupo, medias_texto, sd, semilla=NULL) {
  if (!is.null(semilla)) set.seed(semilla)
  medias <- as.numeric(trimws(strsplit(medias_texto, ",")[[1]]))
  if (length(medias) != n_grupos) stop("Las medias no coinciden con los grupos.")
  
  grupo <- rep(paste0("G", 1:n_grupos), each = n_por_grupo)
  y <- unlist(lapply(1:n_grupos, function(i) rnorm(n_por_grupo, medias[i], sd)))
  data.frame(y = y, grupo = factor(grupo))
}

# 2) Lee CSV unifactorial
leer_datos_unifactorial <- function(archivo, nombre_respuesta, nombre_factor) {
  df <- read.csv(archivo$datapath)
  data.frame(
    y = df[[nombre_respuesta]],
    grupo = factor(df[[nombre_factor]])
  )
}

# 3) Corre ANOVA unifactorial
correr_anova_unifactorial <- function(df) {
  modelo <- aov(y ~ grupo, data = df)
  list(
    modelo = modelo,
    tabla_anova = summary(modelo)[[1]],
    resumen = df %>% group_by(grupo) %>% summarise(n=n(), media=mean(y), sd=sd(y))
  )
}

# 4) Gráficas unifactorial
graficas_unifactorial <- function(res) {
  df <- res$modelo$model
  p1 <- ggplot(df, aes(grupo, y)) + geom_boxplot() + theme_minimal()
  p2 <- ggplot(data.frame(aj=fitted(res$modelo), res=resid(res$modelo)),
               aes(aj, res)) +
    geom_point() + geom_hline(yintercept=0, linetype=2) + theme_minimal()
  list(boxplot=p1, residuos=p2)
}

# 5) Datos bifactoriales
generar_datos_bifactorial <- function(nA, nB, n, mg, sd, semilla=NULL, inter=TRUE) {
  if (!is.null(semilla)) set.seed(semilla)
  A <- paste0("A", 1:nA)
  B <- paste0("B", 1:nB)
  grid <- expand.grid(A=A, B=B)
  efectoA <- seq(-1,1,length=nA); names(efectoA)=A
  efectoB <- seq(-1,1,length=nB); names(efectoB)=B
  interAB <- if (inter) rnorm(nA*nB,0,0.5) else rep(0,nA*nB)
  grid$mu <- mg + efectoA[grid$A] + efectoB[grid$B] + interAB
  do.call(rbind, lapply(1:nrow(grid), function(i){
    data.frame(y = rnorm(n, grid$mu[i], sd),
               A = grid$A[i], B = grid$B[i])
  }))
}

# 6) Lee CSV bifactorial
leer_datos_bifactorial <- function(archivo, ycol, Acol, Bcol) {
  df <- read.csv(archivo$datapath)
  data.frame(y = df[[ycol]], A = factor(df[[Acol]]), B = factor(df[[Bcol]]))
}

# 7) Corre ANOVA bifactorial
correr_anova_bifactorial <- function(df) {
  modelo <- aov(y ~ A * B, data = df)
  list(
    modelo = modelo,
    tabla_anova = summary(modelo)[[1]],
    resumen = df %>% group_by(A,B) %>% summarise(n=n(), media=mean(y), sd=sd(y))
  )
}

# 8) Gráficas bifactorial
graficas_bifactorial <- function(df, res) {
  p1 <- ggplot(df, aes(x = A, y = y, group=B, color=B)) +
    stat_summary(fun=mean, geom="line") +
    stat_summary(fun=mean, geom="point") +
    theme_minimal()
  
  p2 <- ggplot(df, aes(x = interaction(A,B), y = y)) +
    geom_boxplot() + theme_minimal()
  
  list(interaccion=p1, boxplot=p2)
}

# 9) Interpretación automática
interpretar_anova <- function(tabla) {
  p <- tabla[1,"Pr(>F)"]
  if (p < 0.05)
    return(paste("El factor es significativo (p =", round(p,4), ")."))
  else
    return(paste("El factor NO es significativo (p =", round(p,4), ")."))
}

# ------------------ SERVER -------------------------------

server <- function(input, output) {
  
  # ----------- UNIFACTORIAL -----------
  datos_uni <- eventReactive(input$btn_correr_1, {
    if (input$fuente_datos_1 == "sim") {
      generar_datos_unifactorial(input$n_grupos_1, input$n_por_grupo_1,
                                 input$medias_1, input$sd_1, input$semilla_1)
    } else {
      leer_datos_unifactorial(input$archivo_1, input$nombre_respuesta_1, input$nombre_factor_1)
    }
  })
  
  res_uni <- reactive({ correr_anova_unifactorial(datos_uni()) })
  
  output$resumen_anova_1 <- DT::renderDT({
    tabla <- as.data.frame(res_uni()$tabla_anova)
    tabla$Term <- rownames(tabla)
    tabla <- tabla[, c(ncol(tabla), 1:(ncol(tabla)-1))]
    # crear columna de significancia basada en p-value (si existe)
    pcol <- grep("Pr", names(tabla))[1]
    if (!is.na(pcol)) {
      pvals <- tabla[[pcol]]
      sig <- rep("", length(pvals))
      sig[!is.na(pvals) & pvals < 0.001] <- "***"
      sig[!is.na(pvals) & pvals >= 0.001 & pvals < 0.01] <- "**"
      sig[!is.na(pvals) & pvals >= 0.01 & pvals < 0.05] <- "*"
      sig[!is.na(pvals) & pvals >= 0.05 & pvals < 0.1] <- "."
      tabla$Signif <- sig
    }
    num_cols <- names(tabla)[sapply(tabla, is.numeric)]
    dt <- datatable(tabla, rownames = FALSE, options = list(dom = 't', paging = FALSE),
                    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left; font-weight: bold;',
                                                      'Tabla ANOVA'))
    if (length(num_cols) > 0) dt <- formatRound(dt, columns = num_cols, digits = 4)
    dt
  })
  output$tabla_resumen_1 <- renderTable(res_uni()$resumen)
  
  output$plot_boxplot_1 <- renderPlotly({
    p <- graficas_unifactorial(res_uni())$boxplot +
      labs(title = "Boxplot por grupo") +
      theme_minimal(base_size = 14)
    ggplotly(p, tooltip = "y") %>% config(displayModeBar = TRUE, responsive = TRUE)
  })
  
  output$plot_residuos_1 <- renderPlotly({
    # agregar texto para tooltip en residuos
    df_res <- res_uni()$modelo$model
    p2 <- ggplot(df_res, aes(x = fitted(res_uni()$modelo), y = resid(res_uni()$modelo),
                             text = paste0("Fitted: ", round(fitted(res_uni()$modelo),3),
                                           "<br>Residuo: ", round(resid(res_uni()$modelo),3),
                                           "<br>Grupo: ", grupo))) +
      geom_point(color = "#2c7fb8", size = 2, alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = 2) +
      labs(x = "Fitted", y = "Residuo", title = "Residuales vs Ajustados") +
      theme_minimal(base_size = 14)
    ggplotly(p2, tooltip = "text") %>% config(displayModeBar = TRUE, responsive = TRUE)
  })
  
  output$interpretacion_1 <- renderText(interpretar_anova(res_uni()$tabla_anova))
  
  # ----------- BIFACTORIAL -----------
  datos_bi <- eventReactive(input$btn_correr_2, {
    if (input$fuente_datos_2 == "sim") {
      generar_datos_bifactorial(input$niveles_A, input$niveles_B,
                                input$n_por_celda, input$media_global_2,
                                input$sd_2, input$semilla_2, input$incluir_interaccion)
    } else {
      leer_datos_bifactorial(input$archivo_2,
                             input$nombre_respuesta_2,
                             input$nombre_factorA_2,
                             input$nombre_factorB_2)
    }
  })
  
  res_bi <- reactive({ correr_anova_bifactorial(datos_bi()) })
  
  output$resumen_anova_2 <- DT::renderDT({
    tabla <- as.data.frame(res_bi()$tabla_anova)
    tabla$Term <- rownames(tabla)
    tabla <- tabla[, c(ncol(tabla), 1:(ncol(tabla)-1))]
    pcol <- grep("Pr", names(tabla))[1]
    if (!is.na(pcol)) {
      pvals <- tabla[[pcol]]
      sig <- rep("", length(pvals))
      sig[!is.na(pvals) & pvals < 0.001] <- "***"
      sig[!is.na(pvals) & pvals >= 0.001 & pvals < 0.01] <- "**"
      sig[!is.na(pvals) & pvals >= 0.01 & pvals < 0.05] <- "*"
      sig[!is.na(pvals) & pvals >= 0.05 & pvals < 0.1] <- "."
      tabla$Signif <- sig
    }
    num_cols <- names(tabla)[sapply(tabla, is.numeric)]
    dt <- datatable(tabla, rownames = FALSE, options = list(dom = 't', paging = FALSE),
                    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left; font-weight: bold;',
                                                      'Tabla ANOVA'))
    if (length(num_cols) > 0) dt <- formatRound(dt, columns = num_cols, digits = 4)
    dt
  })
  output$tabla_resumen_2 <- renderTable(res_bi()$resumen)
  
  output$plot_interaccion_2 <- renderPlotly({
    p <- graficas_bifactorial(datos_bi(), res_bi())$interaccion +
      labs(title = "Interacción (medias por nivel)") +
      theme_minimal(base_size = 14)
    ggplotly(p, tooltip = c("y","A","B")) %>% config(displayModeBar = TRUE, responsive = TRUE)
  })
  
  output$plot_boxplot_2 <- renderPlotly({
    p2 <- graficas_bifactorial(datos_bi(), res_bi())$boxplot +
      labs(title = "Boxplots por combinación A:B") +
      theme_minimal(base_size = 14)
    ggplotly(p2, tooltip = "y") %>% config(displayModeBar = TRUE, responsive = TRUE)
  })
  
  output$interpretacion_2 <- renderText(interpretar_anova(res_bi()$tabla_anova))
}
