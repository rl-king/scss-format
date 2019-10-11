[![Build Status](https://travis-ci.org/rl-king/scss-format.svg?branch=master)](https://travis-ci.org/rl-king/scss-format)
# scss-format

An opinionated scss formatter

## Style

* Sort properties
* Indent four spaces
* A single newline inbetween selectors
* Add newlines around top level comments
* Add closing `;` to last property if missing


<table><thead><tr><th>Before</th><th>After</th></tr></thead><tbody><tr><td>

```scss
// PAGE
.page {
    margin: 1rem;

    @include breakpoint($small) {
        padding: 1rem;


         .body-text {padding-left: 1.5rem;}
    }


    padding: 1rem;

    h4 {
        margin: .5em 0 .25em;
        font-size: 1em
    }
}
```

</td><td>

```scss
// PAGE

.page {
    padding: 1rem;
    margin: 1rem;

    h4 {
        margin: .5em 0 .25em;
        font-size: 1em;
    }

    @include breakpoint($small) {
        padding: 1rem;

        .body-text {
            padding-left: 1.5rem;
        }
    }
}
```

</td></tr> </tbody></table>

## Install executable

Asumes `haskell-stack` is installed

1. clone repository
2. cd into directory
3. stack install
4. scss-format --path './style.scss'


## Install (spac)emacs

Install [reformatter](https://github.com/purcell/reformatter.el) and add the following to your config:
``` elisp
(reformatter-define scss-format
  :program "scss-format"
  :args '("--stdin")
  :lighter " SCSSF")
```

Enable format on save by adding `(add-hook 'scss-mode-hook 'scss-format-on-save-mode)` to your config as well.

## Develop

Compile (and run in case of ghcid) on save

Asumes `ghcid` is installed

`ghcid --command 'stack ghci' --test 'dev' --warnings`

or

`stack build --file-watch --fast`
