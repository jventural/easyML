# easyML 2.4.1 (2026-09-22)

Correcciones encontradas al correr `supervised_ml()` sobre datos bibliométricos
con clases muy desbalanceadas (4 % de positivos) y predictores muy colineales
(VIF de cientos). Pruebas en `tests/test_shap_fixes.R`.

* `compare_models()`: `supervised_ml()` con **un solo modelo** fallaba al
  calcular el ranking promedio (`Can't recycle input of size 10 to size 1`).
* `calculate_shap()`: las variables que el preprocesamiento elimina (alta
  correlación, VIF, varianza cero) aparecían en la importancia y en los
  gráficos SHAP con valor **0**, como si no importaran, cuando el modelo
  nunca las vio. Ahora se excluyen y se informan en `$removed_vars`.
* `calculate_shap()`: nuevo `$collinear_vars` con los predictores usados de
  VIF > 5 y un aviso en la salida: el reparto del SHAP entre variables casi
  redundantes no se debe interpretar variable por variable.
* `calculate_shap()`: en clasificación la muestra SHAP se estratifica con el
  mismo número de casos por clase (`$sample_design`). Con 100 casos al azar y
  una prevalencia del 4 %, la clase positiva quedaba con unos 4 casos.
* Umbral óptimo: se **elige** con predicciones fuera de pliegue del conjunto
  de entrenamiento (el workflow final reajustado con los mismos pliegues) y se
  **evalúa** en el conjunto de prueba (`$threshold_optimization$source`,
  `$threshold_optimization$test_metrics`). Antes se elegía y evaluaba sobre
  las mismas predicciones de prueba, lo que daba una sensibilidad optimista.
