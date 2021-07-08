+++
# A Recent Blog Posts section created with the Pages widget.
# This section displays recent blog posts from `content/post/`.

widget = "blank"  # See https://sourcethemes.com/academic/docs/page-builder/
headless = true  # This file represents a page section.
active = true  # Activate this widget? true/false
weight = 10  # Order that this section will appear.

title = "ggplotting"
subtitle = "Teil der [digiGEBF21](https://www.digigebf21.de/frontend/index.php)"

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
    category = "ggplotting"
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

<a id="ggplotting"></a>
Ein Bild sagt mehr als tausend Zahlen! Mit einer guten Grafik lassen sich viele Informationen übersichtlich darstellen, Sachverhalten schnell erkennen und auch an Laien komplizierte Datenlagen verständlich kommunizieren. Dabei ist eine gute Visualisierung nicht einfach: komplexe Inhalte müssen klar, präzise und effizient dargestellt werden und sollen idealerweise auch noch hübsch aussehen. `ggplot` ist die R-Antwort auf diese Probleme und kann um diverse Aspekte - z.B. Animationen mit `gganimate` - erweitert werden!

|  |  |  |  |  |
| --- | --- | --- | --- | --- |
| 0 | **Datenvorbereitung** | [Inhalte](/post/ggplotting-daten) | [R-Skript](/post/ggplotting-daten.R) |
| 1 | **Intro** | -- | -- |
| 2 | **ggplot2 Intro** | [Inhalte](/post/ggplotting-intro) | [R-Skript](/post/ggplotting-intro.R) |
| 3 | **Hübsche Grafiken: Theorie** | -- | -- |
| 4 | **Hübsche Grafiken: Praxis** | [Inhalte](/post/ggplotting-themes) | [R-Skript](/post/ggplotting-themes.R) |
| 5 | **Übung 1** | -- | R-Skript |
| 6 | **ggplotpourri** | [Inhalte](/post/ggplotting-ggplotpourri) | -- |
| 7 | **gganimate** | [Inhalte](/post/ggplotting-gganimate) | [R-Skript](/post/ggplotting-gganimate.R) |
| 8 | **plotly** | [Inhalte](/post/ggplotting-plotly) | [R-Skript](/post/ggplotting-plotly.R) |
| 9 | **Explorative Grafiken** | -- | [R-Skript](/post/ggplotting-exploration.R) |
| 10 | **Übung 2** | -- | R-Skript |
| 11 | **Outro** | -- | [R-Skript](/post/ggplotting-outro.R) |
| | | | |

Den [<i class='fas fa-download'></i> Foliensatz zum Workshop gibt es hier](/post/ggplotting-folien.pdf).
