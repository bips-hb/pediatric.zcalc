## R CMD check results

0 errors | 0 warnings | 1 note

Test environments:

* Windows 11 x64, R 4.6.1, local
* Windows, R-release, win-builder
* Windows, R-devel, win-builder

The CRAN incoming feasibility check reports a NOTE because this is a new submission. On win-builder, the same check also reports the following possibly misspelled words in `DESCRIPTION`:

* IDEFICS
* MetS

Both terms are intentional. IDEFICS is the name/acronym of the reference study used by the package, and MetS is the abbreviation for metabolic syndrome.

This is a new submission.
