\name{launch_report_generator}
\alias{launch_report_generator}
\title{Lanzar Generador de Reportes Shiny}
\description{
Abre la aplicacion Shiny interactiva para generar reportes cientificos
a partir de archivos JSON exportados de easyML. La aplicacion permite
configurar el titulo, autor, idioma y modelo de IA.
}
\usage{
launch_report_generator(launch.browser = TRUE)
}
\arguments{
\item{launch.browser}{Abrir en navegador (default: TRUE).}
}
\details{
La aplicacion Shiny permite:
\itemize{
  \item Cargar archivos JSON generados con \code{export_verbose_json()}
  \item Configurar API Key de OpenAI
  \item Seleccionar modelo de IA (GPT-4.1-mini, GPT-4o, etc.)
  \item Generar reportes en formato Word o Markdown
  \item Vista previa del reporte antes de descargar
}
}
\examples{
\dontrun{
# Primero exportar el JSON
resultado <- easy_ml(data, target = "mi_variable", task = "classification")
export_verbose_json(resultado, "mi_analisis.json")

# Abrir la aplicacion Shiny
launch_report_generator()
}
}
\seealso{
\code{\link{generate_report_with_ai}}, \code{\link{export_verbose_json}}
}
