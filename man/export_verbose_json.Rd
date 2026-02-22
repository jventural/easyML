\name{export_verbose_json}
\alias{export_verbose_json}
\title{Exportar Resultado como JSON}
\description{
Exporta el resultado de easyML como archivo JSON estructurado.
Funciona tanto con resultados de \code{easy_ml_capture()} como de \code{easy_ml()}.
El JSON generado puede usarse para crear reportes cientificos con IA.
}
\usage{
export_verbose_json(result, file_path = "easyml_report.json")
}
\arguments{
\item{result}{Resultado de \code{easy_ml_capture()} o \code{easy_ml()}.}
\item{file_path}{Ruta del archivo de salida (default: "easyml_report.json").}
}
\value{
Invisiblemente, la ruta del archivo generado.
}
\details{
El archivo JSON contiene:
\itemize{
  \item \code{metadata}: Informacion del analisis (tarea, target, n observaciones)
  \item \code{metrics}: Metricas de rendimiento del modelo
  \item \code{model_info}: Informacion del modelo (nombre, hiperparametros)
  \item \code{variable_importance}: Top 10 variables mas importantes
  \item \code{threshold}: Threshold optimizado (si aplica)
  \item \code{references}: Referencias bibliograficas APA
}
}
\examples{
\dontrun{
# Ejecutar analisis
resultado <- easy_ml(data, target = "mi_variable", task = "classification")

# Exportar como JSON
export_verbose_json(resultado, "mi_analisis.json")

# O usar easy_ml_capture para capturar verbose completo
resultado <- easy_ml_capture(data, target = "mi_variable", task = "classification")
export_verbose_json(resultado, "mi_analisis.json")
}
}
\seealso{
\code{\link{easy_ml}}, \code{\link{easy_ml_capture}},
\code{\link{export_verbose_txt}}, \code{\link{generate_report_with_ai}}
}
