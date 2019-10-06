# scss-format

An opinionated scss formatter

## Style

* Indent four spaces
* a single newline after selector
* a single newline inbetween at properties and at-rules
* sort properties

## Install

Asumes `haskell-stack` is installed

1. clone repository
2. cd into directory
3. stack install
4. scss-format 'filename.scss'

## Develop


Compile (and run in case of ghcid) on save

Asumes `ghcid` is installed

`ghcid --command 'stack ghci' --test 'dev' --warnings`

or

`stack build --file-watch --fast`
