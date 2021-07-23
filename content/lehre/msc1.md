+++
# A Recent Blog Posts section created with the Pages widget.
# This section displays recent blog posts from `content/post/`.

widget = "blank"  # See https://sourcethemes.com/academic/docs/page-builder/
headless = true  # This file represents a page section.
active = true  # Activate this widget? true/false
weight = 40  # Order that this section will appear.

title = "PsyMSc 1"
subtitle = "Forschungsmethoden und Evaluation I & II"

[content]
  # Page type to display. E.g. post, talk, or publication.
  page_type = "post"

  # Choose how much pages you would like to display (0 = all pages)
  count = 0

  # Choose how many pages you would like to offset by
  offset = 0

  # Page order. Descending (desc) or ascending (asc) date.
  order = "desc"

  # Filter posts by a taxonomy term.
  [content.filters]
    tag = ""
    category = "MSc1"
    publication_type = ""
    exclude_featured = false

[design]
  # Toggle between the various page layout types.
  #   1 = List
  #   2 = Compact
  #   3 = Card
  #   4 = Citation (publication only)
  view = 1

[design.background]
  # Apply a background color, gradient, or image.
  #   Uncomment (by removing `#`) an option to apply it.
  #   Choose a light or dark text color by setting `text_color_light`.
  #   Any HTML color name or Hex value is valid.

  # Background color.
  # color = "navy"

  # Background gradient.
  # gradient_start = "DeepSkyBlue"
  # gradient_end = "SkyBlue"

  # Background image.
  # image = "google_post.jpg"  # Name of image in `static/img/`.
  # image_darken = 0.6  # Darken the image? Range 0-1 where 0 is transparent and 1 is opaque.

  # Text color (true=light or false=dark).
  # text_color_light = true  

[advanced]
 # Custom CSS.
 css_style = ""

 # CSS class.
 css_class = ""
+++

<a id="PsyMSc1"></a>
Das Modul PsyMSc1 ist in zwei Teile untergliedert: Forschungsmethoden und Evaluation I und II. In F&E I geht es um multivariate Vorhersagemodelle, die als (multivariate) Erweiterung des Allgemeinen Linearen Modells angesehen werden können. Beispielsweise wird die Regressionsanalyse erweitert, um auch bestimmte Abhängigkeiten in den Daten modellieren zu können, sowie um auch dichotome abhängige Variablen vorhersagen zu können. Neben der multivariaten Erweiterung der Varianzanalyse (ANOVA) werden auch Datenvorbereitungsmaßnahmen vorgestellt, welche den/die Anwender/in beim Verstehen der Struktur in den Daten unterstützen sollen. Die inhaltlichen Sitzungen werden hierbei durch die Umsetzung in `R` unterstützt:

|  |  |  |  | | |
| --- | --- | --- | --- | --- | --- |
| 0 | **Einleitung und Wiederholung** | [Inhalte](/post/einleitung-und-wiederholung)  | [Übungsdaten](/post/MSc1-Daten) |
| 1 | **Regressionsanalyse und Ausreißerdiagnostik** | [Inhalte](/post/regression-und-ausreisserdiagnostik)  | [Übungsdaten](/post/MSc1-Daten) |
| 2 | **Hierarchische Regressionsanalyse** | [Inhalte](/post/multi-level-modeling)  | [Übungsdaten](/post/MSc1-Daten) |
| 3 | **Hauptkomponentenanalyse** | [Inhalte](/post/PCA)  | [Übungsdaten](/post/MSc1-Daten) |
| 4 | **Logistische Regressionsanalyse** | [Inhalte](/post/logistische-regression)  | [Übungsdaten](/post/MSc1-Daten) |
| 5 | **Multivariate Varianzanalyse** | [Inhalte](/post/manova)  | [Übungsdaten](/post/MSc1-Daten) |
| *Zusatz* | **Diskriminanzanalyse** | [Inhalte](/post/diskriminanzanalyse)  | --- |
|  |  |  |  | | |

Die zweite Hälfte des Moduls, F&E II, befasst sich vor allem mit Ansätzen zur Modellierung latenter Variablen und deren Beziehungen zueinander. Darunter fallen z.B. explorative und konfirmatorische Faktorenanalysen, die die Beziehung zwischen manifesten Variablen und den ihnen zugrundeliegenden latenten Variablen modellieren. Aber auch die Modellierung der Beziehung zwischen latenten psychologischen Konstrukten (Strukturgleichungsmodelle) und die Vergleiche von Modellen zwischen verschiedenen Gruppen, z.B. für interkulturelle Studien, ist Bestandteil dieses Semesters.

|  |  |  |  | | | |
| --- | --- | --- | --- | --- | --- | --- |
| 1 | **Einführung in lavaan** | [Inhalte](/post/lavaan-intro)  | [Übungsdaten](/post/fairplayer.rda) | [R-Skript](/post/FEII_intro.R) |
| 2 | **Explorative Faktorenanalyse** | [Inhalte](/post/efa)  | [Übungsdaten](/post/Big5_EFA.rda) | [R-Skript](/post/FEII_efa.R) |
| 3 | **Konfirmatorische Faktorenanalyse** | [Inhalte](/post/cfa)  | [Übungsdaten](/post/conspiracy.rda) | [R-Skript](/post/FEII_cfa.R) |
| 4 | **Strukturgleichungsmodelle** | [Inhalte](/post/sem)  | [Übungsdaten](/post/StressAtWork.rda) | [R-Skript](/post/FEII_sem.R) |
| *Zusatz* |  **Modelfit** | [Inhalte](/post/exkurs-modellfit) |  | | | |
| 5 | **Multigruppen Modelle** | [Inhalte](/post/msa)  | [Übungsdaten](/post/StressAtWork.rda) | [R-Skript](/post/FEII_msa.R) |
| *Zusatz* |  **Invarianztestung** | [Inhalte](/post/exkurs-invarianzstufen) |  | | | |
|  |   |  |  | | | |

* *Zusatz*-Abschnitte sollen als Ergänzung für Interessierte dienen und einige angesprochene Aspekte vertiefen. Hier werden keine neuen `R`-Inhalte vermittelt.
