+++
# A Recent Blog Posts section created with the Pages widget.
# This section displays recent blog posts from `content/post/`.

widget = "blank"  # See https://sourcethemes.com/academic/docs/page-builder/
headless = true  # This file represents a page section.
active = true  # Activate this widget? true/false
weight = 20  # Order that this section will appear.

title = "PsyBSc 2"
subtitle = "Statistik I"

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
    category = "BSc2"
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

In PsyBSc 2 geht es um die erste Einführung in die Statistik im Psychologiestudium. Dafür betrachten wir die Grundstruktur von R, Datenimport, einfache Grafiken, Deskriptivstatistiken, Verteilungsfunktionen und einige Tests. Das dazugehörige Material taucht im Verlauf des Semesters hier auf.

**Hinweis**: Aufgrund der Lehrumstellung zum Wintersemester 2021/2022 ändert sich auch die Struktur dieses Abschnitts. Wir arbeiten daran, die Inhalte so schnell wie möglich wieder öffentlich verfügbar zu machen.

|  |  |  |  |  |
| --- | --- | --- | --- | --- |
| 1 | **R-Intro** | [Inhalte](/post/r-crash-kurs) | [Aufgaben](/post/r-crash-kurs-aufgaben) | [Lösungen](/post/r-crash-kurs-loesungen)
| 2 | **Deskriptivstatistik für Nominal- und Ordinalskalen** | [Inhalte](/post/deskriptiv-nominal) | [Aufgaben](/post/deskriptiv-nominal-aufgaben) | [Lösungen](/post/deskriptiv-nominal-loesungen) |
| 3 | **Deskriptivstatistik für Intervallskalen** | [Inhalte](/post/deskriptiv-intervall) | [Aufgaben](/post/deskriptiv-intervall-aufgaben) | [Lösungen](/post/deskriptiv-intervall-loesungen) |
| 4 | **Verteilungen** | [Inhalte](/post/verteilungen) | [Aufgaben](/post/verteilungen-aufgaben) | [Lösungen](/post/verteilungen-loesungen) |
| *Zusatz* | **Übungsaufgaben** | -- | [Aufgaben](/post/zusatz-aufgaben) | [Lösungen](/post/zusatz-loesungen) |
| 5 | **Tests und Konfidenzintervalle** | [Inhalte](/post/tests-und-konfidenzintervalle) | [Aufgaben](/post/tests-und-konfidenzintervalle-aufgaben) | [Lösungen](/post/tests-und-konfidenzintervalle-loesungen) |
| 6 | **Tests für unabhängige Stichproben** | [Inhalte](/post/gruppenvergleiche-unabhaengig) | [Aufgaben](/post/gruppenvergleiche-unabhaengig-aufgaben) | [Lösungen](/post/gruppenvergleiche-unabhaengig-loesungen) |
| 7 | **Tests für abhängige Stichproben** | [Inhalte](/post/gruppenvergleiche-abhaengig) | [Aufgaben](/post/gruppenvergleiche-abhaengig-aufgaben) | [Lösungen](/post/gruppenvergleiche-abhaengig-loesungen) |
| 8 | **Korrelation** | [Inhalte](/post/korrelation) | [Aufgaben](/post/korrelation-aufgaben) | [Lösungen](/post/korrelation-loesungen) |
| 9 | **Regression** | [Inhalte](/post/regression) | [Aufgaben](/post/regression-aufgaben) | [Lösungen](/post/regression-loesungen) |
| 10 | **Simulation und Poweranalyse** | Inhalte | Aufgaben | Lösungen |
| *Zusatz* | **Gruppenprojekt** | [Inhalte](/post/gruppenprojekt) | [Hinweise Ergebnisbericht](/post/hinweise-zum-ergebnisbericht) | -- |
| | | | |

Die Umfrage aus der ersten Woche gibt es [hier](https://psybsc2.formr.org/). Die Daten, die dabei in der ersten Sitzung entstanden sind, können Sie [<i class="fas fa-download"></i>   hier im RDA Format](/post/fb21.rda) und [<i class="fas fa-download"></i>   hier im CSV Format](/post/fb21.csv) herunterladen. Was welche Variablen in diesem Datensatz bedeutet, wird in der [<i class="fas fa-download"></i>   Variablenübersicht erläutert](/post/variablen.pdf).

<!-- Falls Sie ein wenig Orientierungshilfe für das Gruppenprojekt benötigen, finden Sie [<i class="fas fa-download"></i>   hier ein Beispiel](/post/aspredicted_example.pdf) für eine Präregistrierung mit [AsPredicted](https://aspredicted.org/) und [<i class="fas fa-download"></i>   hier den fertigen Bericht](/post/bericht_example.pdf) von der gleichen Gruppe aus dem Wintersemester 2019/2020. -->
