# Šablona projektu pro funkcionální část FLP 2025/26

Do této šablony budete doplňovat svá řešení implementace několika funkcí, ze kterých tak poskládáte funkční nástroj pro integrační testování projektů z předmětu IPP. Všechny důležité pokyny najdete v zadání, implementační detaily jsou vysvětleny v komentářích v kódu. V případě nejasností kontaktujte cvičicího.

Tento soubor v odevzdaném projektu nahraďte svým `README.md` napsaným dle zadání (pokud žádný nepíšete, vůbec jej do odevzdaného archivu nedávejte).

## Struktura projektu

Je jistým zvykem, že se haskellové programy rozdělují (alespoň) na dvě části:
- samotný spustitelný program, ve kterém žije především hlavní struktura programu a funkce `main`, která vrací `IO` akci, která bude po spuštění provedena;
- knihovnu, ve které žije většina samotné logiky programu.

Má to dva hlavní důvody (neberte je však úplně dogmaticky):
- Rozlišení mezi *pure* světem, který funguje předvídatelně – funkce má pro stejný vstup vždy stejný výstup, a světem s *vedlejšími efekty* – `IO` akce po vyhodnocení vždycky mohou dopadnout jinak. Typicky chceme psát funkcionální programy tak, že většinu jejich komplexní logiky vyjadřujeme pomocí *pure* funkcí, které jsou definovány v knihovně. Ve spustitelné části pak máme jen (relativně) jednoduchý `IO` kód, který obstarává potřebnou interakci s vnějším světem, k čemuž využívá právě ty složité *pure* funkce z knihovny.
- Testovatelnost – kód k testování unit testy nebo property testy žije v knihovně, zatímco aplikační kód už vyžaduje spíše integrační testování.  

**Struktura repozitáře:**
- `app/` obsahuje kód spustitelné aplikace – je v něm pouze jediný modul `Main.hs`.
  - **Zde začněte**, abyste se seznámili s celkovým postupem programu. Nemělo by ale být nutné zde cokoliv měnit.

- `src/SOLTest/` obsahuje řadu modulů knihovní části:
  - `CLI.hs`: Definuje parser pro parametry příkazové řádky pomocí knihovny [optparse-applicative](https://github.com/pcapriotti/optparse-applicative).
    - Zde **doplňte** funkci `buildFilterSpec`.

  - `Discovery.hs`: Definuje funkci pro (potenciálně rekurzivní) prohledání cílového adresáře s testy, která vrací základní „deskriptory” nalezených `.test` souborů.
    - Zde **doplňte** funkci `discoverTests`.

  - `Executor.hs`: Definuje funkce (`IO` akce) pro spouštění parseru, interpretu a nástroje *diff*.
    - Zde **doplňte** funkce `executeCombined`, `checkInterpreterResult`, `runDiffOnOutput`, `checkExecutable`.

  - `Filter.hs`: Definuje funkci pro filtrování testů na základě dodaných pravidel (získaných z CLI argumentů).
    - Zde **doplňte** funkce `filterTests` a `matchesCriterion`.

  - `JSON.hs`: Definuje pro jednotlivé vlastní datové typy způsob, jakým se serializují do výstupního formátu JSON. Nemělo by být nutné zde cokoliv měnit.

  - `Parser.hs`: Definuje funkce pro načtení souboru s testem ve formátu SOLtest.
    - Zde máte dvě možnosti.
    - Možnost 1: Jen **doplňte** funkce `splitHeaderBody`, `parseHeaderLine`, `buildExitCodes`.
    - Možnost 2 (za bonusový bod ✨): Přidejte do projektu knihovnu [megaparsec](https://hackage.haskell.org/package/megaparsec), která slouží pro vytváření „produkčních” parserů, a správným způsobem ji zde využijte (je to trochu kanón na vrabce, ale proč by ne). Dále upravte property testy v `ParserSpec`, aby tuto novou implementaci řádně testovaly.

  - `Report.hs`: Definuje funkce pro sestavení výsledného reportu o testování.
    - Zde **doplňte** funkce `groupByCategory`, `computeStats`, `computeHistogram`.

- `test/SOLTest/` obsahuje předchystané property-based testy pro čistě funkcionální (*pure*) části projektu. Zkuste se s nimi podrobně seznámit, abyste alespoň obecně pochopili, jak se QuickTest používá. Z definovaných vlastností je možné vyčíst také očekávané chování vašich funkcí.

- `cabal.project` je soubor, který ovlivňuje nastavení *projektu* – lze v něm nastavit například verzi překladače.
- `flp-fun.cabal` je soubor, který konfiguruje *balíčky* – základní „samostatně přeložitelné” programové jednotky (knihovna, spustitelný program, spustitelný program pro testy).

- `dummy-parser.py` a `dummy-interpreter.py` jsou jednoduché pythonové skripty, které je možné použít pro testování vašeho nástroje jakožto implementace překladače a interpretu. Jejich chování je vysvětleno v záhlaví těchto souborů.
- `example_sol_tests/` obsahuje několik ukázkových definic testů (pro použití s dummy překladačem/interpretem). Soubor `expected_output.json` pak ukazuje, jaký výstupní JSON by měl váš nástroj pro tyto testy vytvořit.

## Překlad, spouštění, testování

Při vývoji je nejpohodlnější pro spuštění projektu používat příkaz `cabal run`:

```sh
cabal run flp-fun -- (argumenty předané spuštěnému programu)
```

Pokud chcete své řešení vyzkoušet např. na dodané sadě ukázek, můžete tedy spustit:

```sh
# skripty použité pro -p, -t vyžadují, aby v systému byl nainstalovaný Python 3
cabal run flp-fun -- -p ./dummy-parser.py -t ./dummy-interpreter.py example_sol_tests

# pro "pretty print" výstupního JSON lze použít utilitu `jq` (nutno nainstalovat do systému)
cabal run flp-fun -- -p ./dummy-parser.py -t ./dummy-interpreter.py example_sol_tests | jq
```

Tento příkaz by měl automaticky zajistit překlad, pokud je nutný. Vynutit překlad je možné také pomocí příkazu `cabal build`. Vytvořená binárka bude k dispozici jako soubor `./dist-newstyle/build/[platforma]/ghc-[překladač]/flp-fun-[verze]/x/flp-fun/build/flp-fun/flp-fun`.

Pokud by Cabal házel nějaké divné chyby, zkuste nejprve `cabal clean`, pak `cabal build` a až pak `cabal run`.

Přiložené property-based testy spustíte pomocí `cabal test`.

Při vývoji se také může hodit příkaz `cabal repl`, který spustí interaktivní prostředí GHCi, do kterého automaticky načte všechny moduly z knihovny (které se povede přeložit). Metapříkazem `:r` je pak možné změněné moduly přeložit a znovu načíst.
