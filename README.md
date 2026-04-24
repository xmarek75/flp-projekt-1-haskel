### Testovací nástroj pro projekty z IPP
# Projekt z funkcionální části FLP 2025/26
## Poznámky k řešení:



### Bonusový bod: Kontejnerizace

Projekt obsahuje `Containerfile` pro bonusovou kontejnerizaci. 
Obsahuje stage `build`, `test` a `runtime`. 
Stage `build` překládá nástroj, `test` spouští `cabal test` a `runtime` obsahuje pouze přeložený program a potřebný nástroj `diff`.

### Změny mimo funkce
Přidání importů 
- TestCaseFile do `Generetors.hs`
- parseHeader a parseTestFile do `ParserSpec.hs`

## GitHub
https://github.com/xmarek75/flp-projekt-1-haskel



