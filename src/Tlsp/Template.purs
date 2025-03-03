module Tlsp.Template where

import Prelude

import Data.Array as Array
import Data.Traversable (sequence)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, writeTextFile)
import Tlsp.Common (dist_dirpath, public_dirpath)
import Tlsp.Frontend.Example.Frontend1.Template as Frontend1.Template
import Tlsp.Node (copyDir, rmDir)
import Utility (replaceFormatVars')

main :: Effect Unit
main = do
  whenM (exists dist_dirpath) do
    -- rmdir' dist_dirpath
    rmDir dist_dirpath
  -- void $ execSync $ replaceFormatVars' { public_dirpath, dist_dirpath } "cp -r {{public_dirpath}} {{dist_dirpath}}"
  copyDir { source: public_dirpath, target: dist_dirpath }

  let example_url = "example/"
  names_baseurls <- sequence
    [ do
        let base_url = example_url <> "frontend1/"
        { name } <- Frontend1.Template.main { base_url }
        pure { name, href: base_url }
    ]
  writeTextFile UTF8
    (dist_dirpath <> "index.html")
    ( replaceFormatVars'
        { names_baseurls_listitems:
            names_baseurls
              # map (flip replaceFormatVars' """<li> <a href="{{href}}">{{name}}</a> </li>""")
              # Array.intercalate "\\n"
        }
        index_html
    )

  pure unit

index_html =
  """
<!DOCTYPE html>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>tlsp</title>
</head>

<body>
<h1>Tlsp</h1>

<h2>Examples</h2>
<ul>
{{names_baseurls_listitems}}
<ul>

</body>

</html>
"""
