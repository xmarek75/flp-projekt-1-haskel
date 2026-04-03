-- | Command-line argument parsing for the SOLtest tool.
--
-- All arguments are parsed with @optparse-applicative@, which also
-- generates the @--help@ text automatically.
module SOLTest.CLI
  ( parseOptions,
    optionsParserInfo,
  )
where

import Options.Applicative
import SOLTest.Types

-- | Parse command-line arguments into an 'Options' record.
-- Exits with a help message on @--help@ or on invalid arguments.
parseOptions :: IO Options
parseOptions = execParser optionsParserInfo

-- | The full 'ParserInfo' for the tool, including the description shown
-- in @--help@ output.
optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Discover, filter, and execute SOLtest test cases."
        <> header "flp-fun - SOL26 interpreter testing tool"
    )

-- | Create the @optparse-applicative@ parser for the @Options@ data type.
--
-- The library uses an "applicative" approach to building parsers. To understand this, imagine you already have
-- the values for the @Options@. To create a concrete instance, you'd write a normal function *application*, e.g.:
--
-- > myOptions   = Options     "testdir"  (Just "parserpath") (Just "interprpath") ...
--
-- (Remember that data constructors are just functions). This means: take the @Options@ function; apply it on
-- @"testdir"@; then take the resulting function; apply it on @Just "parserpath"@... and so on, ultimately building
-- a fully applied @Options@ data instance. Now have a look at this:
--
-- > myOptParser = Options <$> parseCliStr <*> parseCliStr <*> parseCliStr <*> ...
--
-- Even without fully understanding, you may see the similarity with the normal application and deduce that this
-- is probably somehow building a @Parser@ using some simpler String parsers, by somehow *applying* the @Options@
-- function on them, step by step, ultimately building a fully applied @Parser Options@.
--
-- In deed, the <$> and <*> operators are somewhat of a generalization of this normal application. The @argument@, 
-- @optional@, @switch@ functions below return a *monadic* @Parser String@ - a type that "when evaluated (somehow),
-- it will provide us with a parsed CLI argument as a String" (this should ring a bell - think Prep and Lab 3).
--
-- @Options \<$> argument (...)@ hence means: take the @Options@ function and smash it INSIDE of this @Parser@,
-- so after it does its thing, it will also map the resulting @String@ using the @Options@ function. This way, you
-- convert the @Parser String@ to a @Parser (sth1 -> sth2 -> ... -> Options)@ - so that's a function enclosed in
-- some monadic context. The <*> operator reads as "apply" and does exactly that: combines the two monads so that
-- the function in the first is applied on the value in the second. So after the first <*>, you now have
-- @Parser (sth2 -> ... -> Options)@. 
--
-- Do this as many times as needed to construct the @Options@ data instance and you suddenly get a @Parser Options@.
-- The nice thing is that you don't care at all about how the library actually achieves this. This monadic heavy
-- lifting is implemented in the library. You basically just say "I wanna make a new complex CLI argument parser
-- by combining the values produced by these simple parsers as an @Options@ object".
--
-- (You may also try to go and find a similarity between this and the @combStep@ function from Lab 3.)
optionsParser :: Parser Options
optionsParser =
  Options
    <$> argument str (metavar "TEST_DIR" <> help "Directory containing .test files")
    <*> optional
      ( strOption
          ( long "parser"
              <> short 'p'
              <> metavar "FILE"
              <> help "Path to the SOL26 parser executable"
          )
      )
    <*> optional
      ( strOption
          ( long "interpreter"
              <> short 't'
              <> metavar "FILE"
              <> help "Path to the interpreter executable"
          )
      )
    <*> switch
      ( long "recursive"
          <> short 'r'
          <> help "Search TEST_DIR recursively for .test files"
      )
    <*> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "FILE"
              <> help "Write the JSON report to FILE instead of stdout"
          )
      )
    <*> switch
      ( long "dry-run"
          <> help "Discover and filter tests but do not execute them"
      )
    <*> filterSpecParser

-- | Parser for all filtering-related options, assembled into a 'FilterSpec'.
filterSpecParser :: Parser FilterSpec
filterSpecParser =
  buildFilterSpec
    <$> many
      ( strOption
          ( long "include"
              <> short 'i'
              <> metavar "ID"
              <> help "Include tests matching ID (name, tag, or category)"
          )
      )
    <*> many
      ( strOption
          ( long "exclude"
              <> short 'e'
              <> metavar "ID"
              <> help "Exclude tests matching ID (name, tag, or category)"
          )
      )
    <*> many
      ( strOption
          ( long "ic"
              <> metavar "ID"
              <> help "Include tests with category ID"
          )
      )
    <*> many
      ( strOption
          ( long "it"
              <> metavar "ID"
              <> help "Include tests with tag ID"
          )
      )
    <*> many
      ( strOption
          ( long "ec"
              <> metavar "ID"
              <> help "Exclude tests with category ID"
          )
      )
    <*> many
      ( strOption
          ( long "et"
              <> metavar "ID"
              <> help "Exclude tests with tag ID"
          )
      )

-- | Assemble raw filter string lists into a 'FilterSpec'.
--
-- FLP: Implement this function (read the long comment above first).

-- buildFilterSpec :: ??? -> FilterSpec
-- buildFilterSpec ???
