\name{easy_ml_export}
\alias{easy_ml_export}
\title{Exportar Analisis Completo}
\description{
Funcion de conveniencia que ejecuta \code{easy_ml()}, captura el verbose,
y exporta tanto TXT como JSON en un solo paso.
}
\usage{
easy_ml_export(..., output_dir = ".", prefix = "easyml")
}
\arguments{
\item{...}{Argumentos para \code{easy_ml()}.}
\item{output_dir}{Directorio de salida (default: directorio actual).}
\item{prefix}{Prefijo para los archivos (default: "easyml").}
}
\value{
El resultado de \code{easy_ml()} con verbose capturado (invisible).
}
\details{
Esta funcion genera dos archivos:
\itemize{
  \item \code{prefix_FECHA_HORA.txt}: Verbose en formato texto
  \item \code{prefix_FECHA_HORA.json}: Datos estructurados para reportes IA
}
}
\examples{
\dontrun{
# Ejecutar y exportar todo en un paso
resultado <- easy_ml_export(
  data = mis_datos,
  target = "mi_variable",
  task = "classification",
  output_dir = "resultados",
  prefix = "estudio1"
)
# Genera: resultados/estudio1_20241228_153000.txt
#         resultados/estudio1_20241228_153000.json
}
}
\seealso{
\code{\link{easy_ml}}, \code{\link{easy_ml_capture}},
\code{\link{export_verbose_txt}}, \code{\link{export_verbose_json}}
}
