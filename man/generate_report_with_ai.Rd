\name{generate_report_with_ai}
\alias{generate_report_with_ai}
\title{Generar Reporte Cientifico con IA}
\description{
Genera un reporte cientifico en formato Word utilizando ChatGPT directamente desde R,
sin necesidad de abrir la aplicacion Shiny. El reporte sigue el estilo de articulo
cientifico con parrafos extensos, citas APA y placeholders para tablas y figuras.
}
\usage{
generate_report_with_ai(
  json_path,
  api_key,
  output_path = "easyML_Report.docx",
  title = "Analisis de Machine Learning",
  author = NULL,
  language = c("es", "en"),
  model = "gpt-4.1-mini"
)
}
\arguments{
\item{json_path}{Ruta al archivo JSON exportado con \code{export_verbose_json()}.}
\item{api_key}{Tu API Key de OpenAI.}
\item{output_path}{Ruta del archivo de salida (default: "easyML_Report.docx").}
\item{title}{Titulo del reporte.}
\item{author}{Nombre del autor (opcional).}
\item{language}{Idioma del reporte: "es" (espanol) o "en" (ingles).}
\item{model}{Modelo de OpenAI a usar (default: "gpt-4.1-mini").}
}
\value{
Invisiblemente, la ruta del archivo generado.
}
\details{
El reporte generado incluye:
\itemize{
  \item Seccion de Metodo con Participantes y Analisis de Datos

  \item Seccion de Resultados con Rendimiento del Modelo e Importancia de Variables
  \item Placeholders para tablas y figuras: [Insertar Tabla 1 aqui], etc.
  \item Referencias en formato APA 7
}

Modelos disponibles:
\itemize{
  \item gpt-4.1-mini: Rapido y economico (recomendado)
  \item gpt-4o-mini: Alternativa economica
  \item gpt-4o: Mayor calidad, mas costoso
  \item gpt-4.1: Maxima calidad
}
}
\examples{
\dontrun{
# Primero ejecutar analisis y exportar JSON
resultado <- easy_ml(data, target = "mi_variable", task = "classification")
export_verbose_json(resultado, "mi_analisis.json")

# Generar reporte con IA
generate_report_with_ai(
  json_path = "mi_analisis.json",
  api_key = "sk-proj-...",
  output_path = "Reporte_Cientifico.docx",
  title = "Analisis Predictivo de Disolucion de Relaciones",
  author = "Dr. Juan Perez",
  language = "es",
  model = "gpt-4.1-mini"
)
}
}
\seealso{
\code{\link{export_verbose_json}}, \code{\link{launch_report_generator}},
\code{\link{easy_ml}}
}
