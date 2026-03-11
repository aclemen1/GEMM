-- |
-- Module      : GEMM.Schema
-- Description : Machine-readable schema for AI agent discovery.
--
-- Implements the @schema@ subcommand, enabling AI agents to introspect
-- the GEMM CLI at runtime: available commands, parameter types and
-- constraints, output formats, and response schemas.
module GEMM.Schema
  ( runSchema
  ) where

import Data.List (intercalate)

-- | Version matching gemm.cabal.
version :: String
version = "1.0.0"

-- | Dispatch the schema subcommand.
runSchema :: [String] -> IO ()
runSchema []               = putStrLn toolSchema
runSchema ["homology-p"]   = putStrLn homologyPSchema
runSchema ["homology-z"]   = putStrLn homologyZSchema
runSchema ["certificate"]  = putStrLn certificateSchema
runSchema [unknown]        = do
  putStrLn $ "{ \"error\": \"Unknown command: " ++ jsonEsc unknown ++ "\" }"
runSchema _                = do
  putStrLn "{ \"error\": \"Usage: gemm schema [command]\" }"

-- ---------------------------------------------------------------------------
-- Tool-level schema (gemm schema)
-- ---------------------------------------------------------------------------

toolSchema :: String
toolSchema = obj
  [ "name"        .= str "gemm"
  , "version"     .= str version
  , "description" .= str "The Generalized Eilenberg-MacLane Machine. Computes integral homology and cohomology groups of Eilenberg-MacLane spaces K(Z/p^f, n) for any prime p, and K(Z, n)."
  , "commands"    .= arr
      [ obj
          [ "name"        .= str "homology-p"
          , "description" .= str "Compute H_*(K(Z/p^f, n); Z) and H^*(K(Z/p^f, n); Z)"
          , "usage"       .= str "gemm [--json] [--time] p f n range"
          ]
      , obj
          [ "name"        .= str "homology-z"
          , "description" .= str "Compute H_*(K(Z, n); Z) and H^*(K(Z, n); Z)"
          , "usage"       .= str "gemm [--json] [--time] Z n range"
          ]
      , obj
          [ "name"        .= str "certificate"
          , "description" .= str "Generate a Lean 4 proof certificate for K(Z/p^f, n)"
          , "usage"       .= str "gemm --cert [--time] [--name NAME] p f n range"
          ]
      ]
  , "output_formats" .= arr
      [ obj [ "name" .= str "latex", "description" .= str "LaTeX document (amsart), default output" ]
      , obj [ "name" .= str "json",  "description" .= str "Structured JSON (--json flag)" ]
      , obj [ "name" .= str "lean4", "description" .= str "Lean 4 proof certificate (--cert flag)" ]
      ]
  , "global_flags" .= arr
      [ obj [ "flag" .= str "--json",      "description" .= str "Output structured JSON instead of LaTeX" ]
      , obj [ "flag" .= str "--time",      "description" .= str "Print CPU timing to stderr" ]
      , obj [ "flag" .= str "--cert",      "description" .= str "Generate Lean 4 proof certificate" ]
      , obj [ "flag" .= str "--name NAME", "description" .= str "Set definition name in generated Lean code (with --cert)" ]
      ]
  , "introspection" .= str "gemm schema [command]"
  ]

-- ---------------------------------------------------------------------------
-- Command-level schemas
-- ---------------------------------------------------------------------------

homologyPSchema :: String
homologyPSchema = obj
  [ "command"     .= str "homology-p"
  , "description" .= str "Compute integral homology and cohomology of the Eilenberg-MacLane space K(Z/p^f, n) for a prime p."
  , "usage"       .= arr [ str "gemm [--json] [--time] p f n range", str "gemm [--json] [--time] s n range" ]
  , "note"        .= str "With 3 positional arguments (s n range), p defaults to 2 and f = s."
  , "parameters"  .= arr
      [ param "p"     "integer" True  "Prime number (must be prime: 2, 3, 5, 7, ...)"
      , param "f"     "integer" True  "Exponent of the cyclic group Z/p^f (f >= 1)"
      , param "n"     "integer" True  "Dimension parameter, connectivity + 1 (n >= 1)"
      , param "range" "integer" True  "Upper bound for degree computation (range >= 0)"
      ]
  , "flags" .= arr
      [ obj [ "flag" .= str "--json", "description" .= str "Output JSON instead of LaTeX" ]
      , obj [ "flag" .= str "--time", "description" .= str "Print CPU timing to stderr" ]
      ]
  , "output" .= obj
      [ "default_format" .= str "latex"
      , "json_schema" .= obj
          [ "space"      .= str "string — LaTeX name of the space, e.g. K(\\\\Z/3,4)"
          , "parameters" .= str "object — { p: int, f: int, n: int, range: int }"
          , "homology"   .= str "object — degree (string) -> group (LaTeX string), e.g. { \"0\": \"\\\\Z\", \"4\": \"\\\\Z/3\" }"
          , "cohomology" .= str "object — same structure as homology"
          , "generators" .= str "array — [{ degree: int, genus: int, type: string, sequence: int[], pair: [string, string] }]"
          ]
      ]
  , "examples" .= arr
      [ obj [ "description" .= str "H_*(K(Z/2, 2); Z) up to degree 10, JSON"
            , "command"     .= str "gemm --json 2 1 2 10"
            ]
      , obj [ "description" .= str "H_*(K(Z/3, 4); Z) up to degree 20, LaTeX"
            , "command"     .= str "gemm 3 1 4 20"
            ]
      , obj [ "description" .= str "H_*(K(Z/4, 2); Z) up to degree 15, JSON with timing"
            , "command"     .= str "gemm --json --time 2 2 2 15"
            ]
      ]
  ]

homologyZSchema :: String
homologyZSchema = obj
  [ "command"     .= str "homology-z"
  , "description" .= str "Compute integral homology and cohomology of the Eilenberg-MacLane space K(Z, n)."
  , "usage"       .= str "gemm [--json] [--time] Z n range"
  , "parameters"  .= arr
      [ param "n"     "integer" True "Dimension parameter, connectivity + 1 (n >= 1)"
      , param "range" "integer" True "Upper bound for degree computation (range >= 0)"
      ]
  , "flags" .= arr
      [ obj [ "flag" .= str "--json", "description" .= str "Output JSON instead of LaTeX" ]
      , obj [ "flag" .= str "--time", "description" .= str "Print CPU timing to stderr" ]
      ]
  , "output" .= obj
      [ "default_format" .= str "latex"
      , "json_schema" .= obj
          [ "space"      .= str "string — e.g. K(\\\\Z,4)"
          , "parameters" .= str "object — { n: int, range: int }"
          , "homology"   .= str "object — degree (string) -> group (LaTeX string)"
          , "cohomology" .= str "object — same structure as homology"
          , "generators" .= str "array — [{ prime: int|null, degree: int, genus: int, type: string, sequence?: int[], pair?: [string, string], generator?: string }]"
          ]
      ]
  , "examples" .= arr
      [ obj [ "description" .= str "H_*(K(Z, 2); Z) up to degree 10, JSON"
            , "command"     .= str "gemm --json Z 2 10"
            ]
      , obj [ "description" .= str "H_*(K(Z, 4); Z) up to degree 20, LaTeX"
            , "command"     .= str "gemm Z 4 20"
            ]
      ]
  ]

certificateSchema :: String
certificateSchema = obj
  [ "command"     .= str "certificate"
  , "description" .= str "Generate a Lean 4 proof certificate verifying the computed homology of K(Z/p^f, n). Not yet supported for K(Z, n)."
  , "usage"       .= str "gemm --cert [--time] [--name NAME] p f n range"
  , "parameters"  .= arr
      [ param "p"     "integer" True  "Prime number (must be prime)"
      , param "f"     "integer" True  "Exponent of the cyclic group Z/p^f (f >= 1)"
      , param "n"     "integer" True  "Dimension parameter, connectivity + 1 (n >= 1)"
      , param "range" "integer" True  "Upper bound for degree computation (range >= 0)"
      ]
  , "flags" .= arr
      [ obj [ "flag" .= str "--cert", "description" .= str "Required flag to activate certificate mode" ]
      , obj [ "flag" .= str "--time", "description" .= str "Print CPU timing to stderr" ]
      , obj [ "flag" .= str "--name NAME", "description" .= str "Set the Lean definition name (default: cert)" ]
      ]
  , "output" .= obj
      [ "format"      .= str "lean4"
      , "description" .= str "Lean 4 source code with a def and a #eval verifyFull call"
      ]
  , "examples" .= arr
      [ obj [ "description" .= str "Certificate for K(Z/2, 2) up to degree 10"
            , "command"     .= str "gemm --cert 2 1 2 10"
            ]
      , obj [ "description" .= str "Certificate with custom name"
            , "command"     .= str "gemm --cert --name myProof 3 1 4 20"
            ]
      ]
  ]

-- ---------------------------------------------------------------------------
-- JSON builder helpers (no external dependencies)
-- ---------------------------------------------------------------------------

-- | Build a parameter descriptor.
param :: String -> String -> Bool -> String -> String
param name typ required desc = obj
  [ "name"        .= str name
  , "type"        .= str typ
  , "required"    .= bare (if required then "true" else "false")
  , "description" .= str desc
  ]

-- | Key-value pair for JSON objects.
(.=) :: String -> String -> (String, String)
key .= val = (key, val)

-- | Render a JSON object from key-value pairs.
obj :: [(String, String)] -> String
obj pairs = "{\n" ++ intercalate ",\n" (map renderPair pairs) ++ "\n}"
  where
    renderPair (k, v) = "  " ++ show k ++ ": " ++ v

-- | Render a JSON array.
arr :: [String] -> String
arr items = "[\n" ++ intercalate ",\n" (map ("  " ++) items) ++ "\n]"

-- | Render a JSON string.
str :: String -> String
str s = "\"" ++ jsonEsc s ++ "\""

-- | Render a bare JSON value (true, false, null, number).
bare :: String -> String
bare = id

-- | Escape a string for JSON.
jsonEsc :: String -> String
jsonEsc = concatMap esc
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc c    = [c]
