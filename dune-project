(lang dune 1.11)
(generate_opam_files true)

(name humanitas)
(source (github carni00/humanitas))
(homepage "https://github.com/carni00/humanitas/")
(bug_reports "https://github.com/carni00/humanitas/issues")
(license GPLv3)
(authors "Carnifex Iulius" "Philippe Veber")
(maintainers "carnifex.iulius@gmail.com")

(package
  (name humanitas)
  (synopsis "Humanitas")
  (description "Humanitas")
  (depends
    (dune (> 2.0))))
