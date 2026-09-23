# easyML 2.4.2 (2026-09-23)

* `fit_models_cv()`: en clasificación binaria, el **PR-AUC de la validación
  cruzada se calculaba para la clase negativa**. `tune::fit_resamples()` pasa a
  las métricas de probabilidad la primera columna (`.pred_<nivel 1>`), y
  `pr_auc` no se ajustaba al `event_level`. Con clases desbalanceadas daba
  valores inverosímiles (p. ej., .999 con un 4 % de positivos, frente a .786
  real). Ahora se recalcula por pliegue con la probabilidad de la clase
  positiva. El ROC-AUC no estaba afectado, porque es simétrico.
* Nota: `.calculate_test_metrics()` (utils_easyml.R) toma la segunda clase
  como positiva pero llama a yardstick sin `event_level`. No la usa ninguna
  función del paquete; conviene corregirla o eliminarla.

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
