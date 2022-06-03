+++
# A Recent Blog Posts section created with the Pages widget.
# This section displays recent blog posts from `content/post/`.

widget = "blank"  # See https://sourcethemes.com/academic/docs/page-builder/
headless = true  # This file represents a page section.
active = true  # Activate this widget? true/false
weight = 30  # Order that this section will appear.

title = "PsyBSc 7"
subtitle = "Statistik II"

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
    category = "BSc7"
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

<a id="PsyBSc7"></a>
Das Modul PsyBSc7 vertieft die im Modul PsyBSc2 vermittelten Grundlagen. Behandelt werden u.a. Matrixalgebra, spezielle Typen von Korrelationskoeffizienten, multiple Regression, deren Voraussetzungen und Erweiterungen, Varianzanalysen, das Testen komplexer Hypothesen, Messwiederholungsdesigns, Tests für kategoriale Variablen sowie forschungslogische Aspekte bei der Anwendung statistischer Modelle. Aufbauend auf den theoretischen Grundlagen der Regressions- und Varianzanalyse lernen Studierende, empirische Fragestellungen in die Form statistischer Modelle zu übertragen, komplexe Hypothesen zu testen und multiple Variablenzusammenhänge differenziert zu beurteilen. Einige dieser Inhalte werden hier in `R` umgesetzt und vertieft.

| |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- |
| I | 1 | **Wiederholung von Grundlagen in R** | [Inhalte](/post/wiederholung-grundlagen) | -- |
| I | 2 | **Grafiken mit ggplot2** | [Inhalte](/post/grafiken-mit-ggplot2) | [Daten Quiz 1](/post/bsc7-daten/#Quiz1) |
| II | 3 | **Partial- & Semipartialkorrelation** | [Inhalte](/post/partial) | -- |
| II | 4 | **Regressionsanalyse I: multiple Regression** | [Inhalte](/post/reg1) | [Daten Quiz 2](/post/bsc7-daten/#Quiz2) |
| III | 5 | **Regressionsanalyse II: Modelloptimierung** | [Inhalte](/post/reg2) | -- |
| III | 6 | **Regressionanalyse III: Voraussetzungsprüfung** | [Inhalte](/post/reg3) | [Daten Quiz 3](/post/bsc7-daten/#Quiz3) |
| IV | 7 | **Regressionanalyse IV: quadratische und Interaktionseffekte** | [Inhalte](/post/quadratische-und-moderierte-regression)  | -- |
| IV | 8 | **Loops und Funktionen** | [Inhalte](/post/loops-und-funktionen) | [Daten Quiz 4](/post/bsc7-daten/#Quiz4) |
| V | 9 | **ANOVA I: einfaktorielle Varianzanalyse** | [Inhalte](/post/anova1) | -- |
| V | 10 | **ANOVA II: zweifaktorielle Varianzanalyse** | [Inhalte](/post/anova2) | [Daten Quiz 5](/post/bsc7-daten/#Quiz5) |
| VI | 11 | **ANOVA III: Varianzanalyse mit Messwiederholung** |  [Inhalte](/post/anova3) | [Daten Quiz 6](/post/bsc7-daten/#Quiz6) |
| | Zusatz | **Regressionanalyse V: nichtlineare Regression** | [Inhalte](/post/nichtlineare-regression) | [Daten Zusatzquiz](/post/bsc7-daten/#QuizZusatz) |
| | Zusatz | **Regressionanalyse VI: Nominalskalierte Prädiktoren** | [Inhalte](/post/nominalskalierte-praediktoren) | -- |
| | |
