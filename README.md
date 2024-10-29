# dajmcdon latex files

## `paper/dajmcdon.bib`

*Easiest is to use this and another `.bib`, specific to your paper.*

If you do need to add a reference to _this_ `.bib`, then

1. The file is sorted by cite key.
2. The cite key is McDonaldBien2021 (last names of the first 2 authors in
camel case followed by the 4 digit year).
3. If the cite key exists, add a lower case letter to disambiguate.
4. You should be able to add a reference anywhere in the file, then run
``` bash
bibtool -s dajmcdon.bib -o dajmcdon.bib
```
to sort and correctly generate the cite key. (If you need to install `bibtool`,
you can use Homebrew on a Mac `brew install bib-tool`.)

5. Try to avoid duplicates.
6. Please feel free to PR into this repo.

## `paper/dajmcdon.tex`

* Uses [ShorTeX](https://github.com/trevorcampbell/shortex) for math macros,
  comments, and loading standard packages.
* Provides some descriptions of best practices.
* `notes/*.tex` is similar
* All figures for the paper should go in `paper/fig`

## `refs/`

* This is intended to contain files for papers you may cite.
* Often best not to commit these publicly.

## `code/`

* Self explanatory. It should be any scripts needed to generate the figures and
  produce analysis. 
* I usually use a single `.qmd`/`.Rmd` file that may rely on some scripts for
  long running code. Otherwise, number the scripts in order.
* You may need a directory for `data/` as well

## `tools/`

These are used to update ShorTeX and `dajmcdon.bib` if you haven't in a while.

* `tools/cp-bib` will copy the remote bib file here, overwriting
* `tools/cp-shortex` will update ShorTeX, overwriting
* `tools/make-arXiv` will copy the paper, figures, and tex files, then zip them
  up for submission to arXiv.

## Cleaning up junk

- `git clean -xf`
