# agda-lint

This is a naive style linter for Agda, which simply parses the code and checks
for common style issues. It reads `.agda`, `.lagda`, `.lagda.tex`, `.lagda.md`,
fixes many findings in place, and takes project-specific rules.

```text
$ agda-lint
src/Syntax.agda:12:9: error [asciiArrow] use → instead of ->
src/Syntax.agda:40:1: warning [importOrder] Data.Bool should come before Data.Nat
1 error(s), 1 warning(s)
```

## Install

Binaries are available on
[releases](https://github.com/vikraman/agda-lint/releases) (Linux x86_64 and
arm64 with glibc 2.35 or later, macOS arm64). Alternatively, you can build it
from source with GHC 9.12 or 9.14: `cabal install exe:agda-lint`.

## Usage

Run `agda-lint` in the project root.

| flag                              |                                           |
| --------------------------------- | ----------------------------------------- |
| `--fix`                           | rewrite fixable findings in place         |
| `--no-warn`                       | only report errors                        |
| `--list-rules`                    | list rules with their configured level    |
| `--config FILE`                   | default `agda-lint.toml`                  |
| `--format text\|github`           | `github` prints workflow annotations      |
| `--fail-on error\|warning\|never` | what exits with 1, default `error`        |
| `PATH...`                         | only report on these files or directories |
| `--version`                       | print the version                         |

## Configuration

`agda-lint.toml`, all optional.

```toml
sources = ["src/**/*.agda"]    # default: every agda file
exclude = ["_build/**"]
preset = "core"                # or "all": every rule at its level below

[rules]                        # error, warning or off
letBinding = "warning"
tab = "error"

[allow]                        # intended findings: path glob, or glob:line
importOrder = ["src/Everything.agda"]
asciiArrow = ["src/Syntax.agda:12"]
```

### Rules

*core* rules are on by default; the rest need `[rules]` or `preset = "all"`.

| rule                  | level   |           | checks                                          |
| --------------------- | ------- | --------- | ----------------------------------------------- |
| `asciiArrow`          | error   | core, fix | `→`, not `->`                                   |
| `asciiLambda`         | error   | core, fix | `λ`, not `\`                                    |
| `trailingWhitespace`  | warning | core, fix | trailing whitespace                             |
| `tab`                 | warning | core      | tab characters                                  |
| `importOrder`         | warning | core      | import blocks are alphabetical                  |
| `bannedName`          | error   | core, fix | names in `[bannedNames]`                        |
| `sortLetter`          | warning | core      | binders use the letters in `[[sorts]]`          |
| `reservedLetter`      | error   | core      | reserved letters are not used for other types   |
| `pairSubscript`       | warning | core      | `M N` from `pairs`, not `M M₁`                  |
| `patternLambda`       | error   |           | pattern lambdas are `λ { (…) → … }`             |
| `asciiSubscript`      | error   | fix       | `V₁`, not `V1`                                  |
| `prime`               | error   |           | no primes on single-letter variables            |
| `prefixType`          | error   | fix       | types with a `syntax` declaration use it        |
| `unneededSubscript`   | warning |           | a lone `M₁` should be `M`                       |
| `unusedVariable`      | warning |           | unused generalizable variables                  |
| `lineLength`          | warning |           | lines over `lineLength`                         |
| `leadingArrow`        | warning |           | arrows end a line, not start one                |
| `moduleBlankLine`     | warning | fix       | one blank line after the module header          |
| `importPublic`        | warning |           | no `public` in the import list                  |
| `importModifierOrder` | warning |           | `public`, `using`, `renaming` in that order     |
| `mutualBlock`         | warning |           | no `mutual` blocks                              |
| `letBinding`          | warning |           | `where`, not `let`                              |
| `trailingComment`     | warning |           | comments go above the term                      |
| `sectionHeader`       | warning |           | divider width and title case                    |
| `withAlignment`       | warning |           | `... \|` is not aligned                         |
| `unicodeInstance`     | warning | fix       | `{{ }}`, not `⦃ ⦄`                              |
| `asciiPrime`          | warning | fix       | `′`, not `'`                                    |
| `missingFixity`       | warning |           | operators have a fixity                         |
| `beginSameLine`       | warning |           | `begin` is on the line of its proof             |
| `whereLayout`         | warning |           | `where` is on its own line                      |
| `stackedPrivate`      | warning |           | `private` and `variable` on separate lines      |
| `botElim`             | warning |           | `contradiction`, not `⊥-elim`                   |
| `rewriteName`         | warning |           | `REWRITE` names end in one of `rewriteSuffixes` |

### Rule options

```toml
# top-level keys go before the first table
fixBannedNames = ["`Unit"]             # bannedName: replaced by --fix
pairs = ["MN", "ΓΔ"]                   # pairSubscript
subscriptExempt = ["≈"]                # skip clauses with these names
asciiSubscriptIgnore = ["ex"]          # prefixes that keep ascii digits
rewriteSuffixes = ["-β", "-η"]

[bannedNames]                          # name = replacement, or ""
"`Unit" = "`𝟙"

[[sorts]]
heads = ["⊢ᵛ", "Val"]                  # type mentions one of these
letters = "VW"

[[sorts]]
exact = ["Ty"]                         # type is exactly this
letters = "XYZ"
reserved = true                        # nothing else may use the letters

[[pairSkip]]                           # pairs not checked below a path
path = "src/L/"
pairs = ["ΓΔ"]

[style]
lineLength = 72
sectionWidth = 72
sectionTitle = "sentence"              # sentence, lower or any
```

## Project rules

### In the config

```toml
[[pattern]]
name = "doubleStruckNat"
level = "error"            # default: warning
token = "Nat"              # a whole token
replace = "ℕ"              # optional, makes it fixable

[[pattern]]
name = "noTodo"
regex = "TODO|FIXME"       # posix extended, inside a token
scope = "line"             # or inside a line, comments included
message = "resolve before merging"
```

One of `token` or `regex` is required. A pattern is a rule like any other in
`[rules]`, `[allow]` and `--list-rules`.

### In Haskell

Add a cabal project in the repository, say `lint/`; see
[examples/custom](examples/custom).

`lint/cabal.project`:

```text
packages: .

source-repository-package
  type: git
  location: https://github.com/vikraman/agda-lint
  tag: v0.1.0
```

`lint/Lint.hs`:

```haskell
import AgdaLint

main :: IO ()
main = defaultMain (builtinRules ++ [noPostulate])

noPostulate :: Rule
noPostulate = rule "noPostulate" Warning "no postulates" $ \_ f ->
  [ finding (tokPos t) "prove it or move it to an assumptions module"
  | l <- codeLines f
  , t <- codeTokens l
  , tokText t == "postulate"
  ]
```

Run it from the project root, with the same flags as `agda-lint`:

```text
cabal run -v0 --project-dir=lint lint -- --fix
```

- A check is `Context -> SourceFile -> [Finding]`: one file's lines, tokens,
  clauses, binders and imports, plus every file of the project.
- `ruleWith` adds options, decoded from the config keys the rule names.
- Custom rules are off until enabled; `core` turns one on by default.
- Fixes are `ReplaceSpan`, `ReplaceLine`, `InsertLineAfter` and `DeleteLine`;
  a finding may carry several fixes.

`AgdaLint` exports what a rule usually needs. The token parsers and binder
analysis the built-in rules use are in `AgdaLint.Source`, the lexer in
`AgdaLint.Lexer`; see [src/AgdaLint/Rules](src/AgdaLint/Rules).

## GitHub Action

```yaml
- uses: actions/checkout@v7
- uses: vikraman/agda-lint@v0.1.0
```

Findings will be annotated on the pull request diff. The action runs the release
it is tagged with; on a branch or commit it runs the latest.

| input               | default          |                                                                |
| ------------------- | ---------------- | -------------------------------------------------------------- |
| `version`           | the action's tag | release to download, or `latest`                               |
| `config`            | `agda-lint.toml` | config file                                                    |
| `args`              |                  | extra arguments, such as paths                                 |
| `fail-on`           | `error`          | `error`, `warning` or `never`                                  |
| `working-directory` | `.`              | project root                                                   |
| `project`           |                  | cabal project with project rules, built instead of downloading |
| `executable`        | `lint`           | executable of that project                                     |
| `ghc-version`       | `9.14.1`         | GHC to build it with                                           |

## Development

`cabal test`. Each directory in [test/fixtures](test/fixtures) is a project
with its report in `expected.txt` and its `--fix` output in `x.fixed`;
`AGDA_LINT_ACCEPT=1 cabal test` regenerates them.
