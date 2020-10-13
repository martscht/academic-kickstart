+++
# A Recent Blog Posts section created with the Pages widget.
# This section displays recent blog posts from `content/post/`.

widget = "blank"  # See https://sourcethemes.com/academic/docs/page-builder/
headless = true  # This file represents a page section.
active = true  # Activate this widget? true/false
weight = 30  # Order that this section will appear.

title = "PsyMSc 1 - F&E I"
subtitle = "Forschungsmethoden und Evaluation I"

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
    category = "MSc 1"
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

Das Modul PsyMSc1 ist in zwei Teile untergliedert: Forschungsmethoden und Evaluation I und II. In F&E I geht es um multivariate Vorhersagemodelle, die als (multivariate) Erweiterung des allgemeinen linearen Modells angesehen werden können. Beispielsweise wird die Regressionsanalyse erweitert um auch bestimmte Abhängigkeiten in den Daten modellieren zu können, sowie um auch dichotome abhängige Variablen vorhersagen zu können. Neben der multivariaten Erweiterung der Varianzanalyse (ANOVA) werden auch Datenvorbereitungsmaßnahmen vorgestellt, welche den/die Anwender/in beim Verstehen der Struktur in den Daten unterstützen soll. Die inhaltlichen Sitzungen werden hierbei durch die Umsetzung in `R` unterstützt:


|  |  |  |  | | |
| --- | --- | --- | --- | --- | --- |
| 0 | **Einleitung und Wiederholung** | [Inhalte](/post/einleitung-und-wiederholung)  | [`R` Skript](https://raw.githubusercontent.com/jpirmer/MSc1_FEI/master/R-Scripts/0_Intro_RCode.R) | [Daten](/post/Schulleistungen.rda) | [Übungsdaten](/post/Schulleistungen.rda) |
| 1 | **Regressionsanalyse und Ausreißerdiagnostik** | [Inhalte](/post/regression-und-ausreisserdiagnostik)  | [`R` Skript](https://raw.githubusercontent.com/jpirmer/MSc1_FEI/master/R-Scripts/1_Regression_RCode.R) | [Daten](/post/Schulleistungen.rda) | Übungsdaten |
| 2 | **Hierarchische Regressionsanalyse** | [Inhalte](/post/multi-level-modeling)  | [`R` Skript](https://raw.githubusercontent.com/jpirmer/MSc1_FEI/master/R-Scripts/2_Multi-Level-Modeling_RCode.R) | [Daten](/post/StudentsInClasses.rda) | [Übungsdaten](/post/StudentsInClasses.rda) |
| 3 | **Hauptkomponentenanalyse** | Inhalte  | [`R` Skript](https://raw.githubusercontent.com/jpirmer/MSc1_FEI/master/R-Scripts/3_PCA_RCode.R) | Daten | Übungsdaten |
| 4 | **Logistische Regressionsanalyse** | Inhalte  | `R` Skript | Daten | Übungsdaten |
| 5 | **Multivariate Varianzanalyse** | Inhalte  | `R` Skript | Daten | Übungsdaten |
|  |  |  |  | | |
