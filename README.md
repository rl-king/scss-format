[![Build Status](https://travis-ci.org/rl-king/scss-format.svg?branch=master)](https://travis-ci.org/rl-king/scss-format)

# scss-format

A non-configurable scss formatter

* Sort properties
* Indent four spaces
* A single newline inbetween selectors
* Add newlines around top level comments
* Add closing `;` to last property if missing

## Usage
```shell
Usage: scss-format [--verbose]
                   [(-p|--path PATH) [-v|--verify] [-o|--overwrite] | --stdin]
  Format scss files and directories, prints the result to stdout by default, use
  '-o' to replace the original file.

Available options:
  --verbose                Log a bit
  -p,--path PATH           Path or dir to a scss file(s)
  -v,--verify              Test if file is correctly formatted
  -o,--overwrite           Replace the orginal file
  --stdin                  Read from stdin
  -h,--help                Show this help text
```

### Unformatted

```scss
// PAGE
.page {
    margin: 1rem;

    @include breakpoint($small) {
        padding: 1rem;


         .body-text {padding-left: 1.5rem;}
    }


    padding: 2rem;

    h4 {
        margin: .5em 0 .25em;
        font-size: 1em
    }
}
```

### Formatted

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
        padding: 2rem;

        .body-text {
            padding-left: 1.5rem;
        }
    }
}
```

## Install executable

Asumes `haskell-stack` is installed

1. clone repository
2. cd into directory
3. stack install
4. scss-format --path './style.scss'


### Install emacs

Install [reformatter](https://github.com/purcell/reformatter.el) and add the following to your config:
``` elisp
(reformatter-define scss-format
  :program "scss-format"
  :args '("--stdin")
  :lighter " SCSSF")
```

Enable format on save by adding `(add-hook 'scss-mode-hook 'scss-format-on-save-mode)` to your config as well.

### VSCode

Install `steefh.external-formatters`, search in extensions tab. Configure with:

``` json
"externalFormatters.languages": {
    "scss": {
        "command": "scss-format",
        "arguments": [
            "--stdin"
        ]
    }
}
```

## Develop

Compile (and run in case of ghcid) on save

Asumes `ghcid` is installed

`ghcid --command 'stack ghci' --test 'dev' --warnings`

or

`stack build --file-watch --fast`
