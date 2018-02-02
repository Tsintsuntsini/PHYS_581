(TeX-add-style-hook
 "Lab1"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "twocolumn")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "graphicx"
    "amsmath"
    "amssymb"
    "hyperref"
    "caption")
   (LaTeX-add-labels
    "fig:fig1"
    "fig:fig2"
    "fig:fig3"
    "fig:fig4"
    "eq:tau_sample"
    "eq:theta_sample"
    "fig:fig6"
    "fig:fig7"
    "fig:fig8"
    "fig:fig9"
    "fig:fig10"
    "fig:fig13"
    "fig:fig14"
    "eq:acceptance"
    "eq:example")
   (LaTeX-add-bibitems
    "ouyed"
    "NR"))
 :latex)

