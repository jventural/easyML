\name{easy_ml_capture}
\alias{easy_ml_capture}
\title{Capturar Verbose Output de easyML}
\description{
Ejecuta \code{easy_ml()} capturando todo el output verbose para exportacion.
El resultado incluye el texto completo del verbose que puede exportarse
como TXT o JSON para generar reportes.
}
\usage{
easy_ml_capture(...)
}
\arguments{
\item{...}{Todos los argumentos se pasan directamente a \code{easy_ml()}.}
}
\value{
Objeto de clase \code{easyml} con campos adicionales:
\itemize{
  \item \code{verbose_text}: Texto completo del verbose
  \item \code{verbose_lines}: Vector de lineas del verbose
  \item \code{verbose_json}: Estructura JSON parseada
}
}
\examples{
\dontrun{
# Ejecutar analisis capturando verbose
resultado <- easy_ml_capture(
  data = mis_datos,
  target = "mi_variable",
  task = "classification"
)

# Exportar como TXT
export_verbose_txt(resultado, "analisis.txt")

# Exportar como JSON
export_verbose_json(resultado, "analisis.json")
}
}
\seealso{
\code{\link{easy_ml}}, \code{\link{export_verbose_txt}},
\code{\link{export_verbose_json}}, \code{\link{easy_ml_export}}
}
