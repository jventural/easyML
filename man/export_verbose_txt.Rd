\name{export_verbose_txt}
\alias{export_verbose_txt}
\title{Exportar Verbose como Texto}
\description{
Exporta el verbose output como archivo de texto plano.
Requiere que el resultado haya sido generado con \code{easy_ml_capture()}.
}
\usage{
export_verbose_txt(result, file_path = "easyml_report.txt")
}
\arguments{
\item{result}{Resultado de \code{easy_ml_capture()}.}
\item{file_path}{Ruta del archivo de salida (default: "easyml_report.txt").}
}
\value{
Invisiblemente, la ruta del archivo generado.
}
\examples{
\dontrun{
# Ejecutar analisis capturando verbose
resultado <- easy_ml_capture(data, target = "mi_variable", task = "classification")

# Exportar como TXT
export_verbose_txt(resultado, "mi_analisis.txt")
}
}
\seealso{
\code{\link{easy_ml_capture}}, \code{\link{export_verbose_json}}
}
