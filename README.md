# Awebitious markdown

Based on commonmark but with HTML support removed. In addition we support question tags such as:

```
?? What contry has he most forrest?

?? Which nation has been nuked the most?
?= Japan
```

The library is based on https://github.com/zudov/haskell-comark

# Old readme

The functionality is split across several libraries which are contained in this
repo:

- **comark**           -- reexports other libraries
- **comark-syntax**    -- definition of Commonmark's AST;
- **comark-parser**    -- parsing commonmark text and producing AST
- **comark-html**      -- rendering commonmark's AST into HTML
- **comark-testutils** -- utilities for testing these packages.

# Usage

The easiest way to get it running is to use [stack](https://haskellstack.org). For instructions see [Downloading and Installation][stack-installation].

[stack]: https://haskellstack.org "The Haskell Tool Stack"
[stack-installation]: https://docs.haskellstack.org/en/stable/GUIDE/#downloading-and-installation "Stack: Downloading and Instalation"

## Executable

Executable version lets you play with and test comark easily.
To get the executable try doing `stack install comark`.

```shell
$ comark-hs --help
Usage:    comark-hs [OPTIONS*] [FILE*]
Options:
  --to, -t FORMAT  Specify output format (html, native)
  --normalize      Consolidate adjacent text nodes
  --help, -h       Print usage information

$ echo "# Hello, Markdown" | comark-hs
<h1>Hello, Markdown</h1>
$ comark-hs --normalize README.md > README.html
$ comark-hs --to native README.md > README.hs
```
## Library

Initially adding `comark` dependency should be enough.
For more granular imports you might want to add some of the libraries listed above.

To learn how the dependency can be added see [Adding dependencies][stack-dependencies].

[stack-dependencies]: https://docs.haskellstack.org/en/stable/GUIDE/#adding-dependencies "Stack: Adding dependencies"

```haskell
import           Data.Text (Text)
import qualified Comark

markdownToHtml :: Text -> Text
markdownToHtml =
  Comark.render . Comark.parse [ Comark.Normalize ]
```

You can find examples of more complex transformations in [comark-examples](./comark-examples).
And make sure to checkout the docs.

# TODO

- [X] Working parser implementation, pass all tests
- [X] Fast and accurate renderer, renders things exactly as in spec.txt.
- [X] Make full use of spec examples.
	  Both renderer and parser are separately tested against the spec relying only on it
	  and the reference implementation (libcmark).
- [X] Handle pathological inputs. Benchmarks are run against `markdown-it`'s samples
	  and over notoriously known nested parenthesis/brackets (`"[" * 50000 + "foo" + "]" * 50000`)
- [X] Revise module structure, names, and (re-)exports.
- [ ] Add to hackage and stackage.
- [X] Document things up.
- [ ] Extend ecosystem with additional libraries that ease the integration
	  (comark-blaze, comark-lucid, comark-json)
- [ ] Add helpers for processing/walking the AST and examples of typical manipulations.
- [ ] Work on extensibility.
- [ ] Compile with ghcjs. If performance/size isn't great, consider making bindings to
	  `commonmark.js` but provide `comark-syntax` based interface.

# License

This library is released under BSD-3-Clause license. See LICENSE for terms and copyright notice.

Custom parser combinators and block structure parser are largely based on the ones found
in [Cheapskate](https://github.com/jgm/cheapskate) (Copyright © 2012, 2013, 2014 John MacFarlane).
