\name{easy_ml}
\alias{easy_ml}
\title{easyML: Machine Learning Automatizado para Clasificacion y Regresion}
\description{
Funcion principal que ejecuta un pipeline completo de Machine Learning.
Automatiza EDA, preprocesamiento, entrenamiento de multiples modelos con
validacion cruzada, tuning de hiperparametros, evaluacion e interpretabilidad.
}
\usage{
easy_ml(
  data,
  target,
  task = c("auto", "classification", "regression"),
  models = c("rf", "xgboost", "glm"),
  test_split = 0.20,
  cv_folds = 10,
  select_metric = NULL,
  tune_best = TRUE,
  tune_method = c("random", "grid", "bayes", "racing"),
  tune_grid = 20,
  tune_iter = 30,
  feature_selection = FALSE,
  balance_classes = FALSE,
  impute = TRUE,
  normalize = TRUE,
  run_eda = TRUE,
  run_shap = TRUE,
  n_shap = 100,
  optimize_threshold = TRUE,
  threshold_method = "youden",
  calibrate_probs = FALSE,
  calibration_method = "platt",
  check_leakage = TRUE,
  nested_cv = FALSE,
  seed = 2024,
  verbose = TRUE
)
}
\arguments{
\item{data}{Data frame con los datos.}
\item{target}{Nombre de la variable objetivo (como string).}
\item{task}{Tipo de tarea: "auto", "classification", "regression".}
\item{models}{Vector con modelos a entrenar. Opciones: "rf", "xgboost", "glm", "svm", "nnet", "rpart".}
\item{test_split}{Proporcion de datos para test (default: 0.20).}
\item{cv_folds}{Numero de folds para validacion cruzada (default: 10).}
\item{select_metric}{Metrica para seleccionar el mejor modelo. Para clasificacion: "roc_auc" (default), "f_meas", "accuracy", "sensitivity", "specificity", "bal_accuracy", "pr_auc". Para regresion: "rmse" (default), "rsq", "mae".}
\item{tune_best}{Realizar tuning del mejor modelo (default: TRUE).}
\item{tune_method}{Metodo de busqueda de hiperparametros: "random", "grid", "bayes", "racing".}
\item{tune_grid}{Numero de combinaciones para tuning (default: 20).}
\item{tune_iter}{Numero de iteraciones para Bayesian Optimization (default: 30).}
\item{feature_selection}{Usar seleccion de variables con CV (default: FALSE).}
\item{balance_classes}{Para clasificacion, balancear clases con SMOTE (default: FALSE).}
\item{impute}{Imputar valores faltantes (default: TRUE).}
\item{normalize}{Normalizar variables numericas (default: TRUE).}
\item{run_eda}{Ejecutar analisis exploratorio (default: TRUE).}
\item{run_shap}{Calcular valores SHAP (default: TRUE).}
\item{n_shap}{Numero de observaciones para SHAP (default: 100).}
\item{optimize_threshold}{Optimizar threshold para clasificacion (default: TRUE).}
\item{threshold_method}{Metodo de optimizacion: "youden", "f1", "balanced" (default: "youden").}
\item{calibrate_probs}{Calibrar probabilidades (default: FALSE).}
\item{calibration_method}{Metodo: "platt" o "isotonic" (default: "platt").}
\item{check_leakage}{Verificar posible data leakage (default: TRUE).}
\item{nested_cv}{Usar nested CV para estimacion menos sesgada (default: FALSE).}
\item{seed}{Semilla para reproducibilidad (default: 2024).}
\item{verbose}{Mostrar progreso en consola (default: TRUE).}
}
\value{
Objeto de clase \code{easyml} con resultados completos del analisis ML, incluyendo:
\itemize{
  \item \code{task}: Tipo de tarea (classification/regression)
  \item \code{target}: Variable objetivo
  \item \code{best_model}: Nombre del mejor modelo
  \item \code{best_params}: Hiperparametros tuneados
  \item \code{final_fit}: Modelo final ajustado
  \item \code{test_metrics}: Metricas en conjunto de prueba
  \item \code{predictions}: Predicciones en test
  \item \code{importance}: Importancia de variables
  \item \code{plots}: Graficos generados
}
}
\examples{
\dontrun{
# Ejemplo basico
resultado <- easy_ml(
  data = mis_datos,
  target = "mi_variable",
  task = "classification"
)

# Ver resultados
print(resultado)
plot(resultado)

# Hacer predicciones
predict(resultado, nuevos_datos)

# Exportar para generar reportes
export_verbose_json(resultado, "mi_analisis.json")

# Generar reporte con IA
generate_report_with_ai(
  json_path = "mi_analisis.json",
  api_key = "tu-api-key",
  output_path = "Reporte.docx"
)
}
}
\seealso{
\code{\link{easy_ml_capture}}, \code{\link{export_verbose_json}},
\code{\link{generate_report_with_ai}}, \code{\link{ml_sample_size}}
}
\author{
Jose Ventura-Leon
}
