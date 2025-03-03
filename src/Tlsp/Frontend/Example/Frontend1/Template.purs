module Tlsp.Frontend.Example.Frontend1.Template where

import Prelude

import Node.ChildProcess (execSync)
import Node.Encoding (Encoding(..))
import Node.FS.Perms (permsAll)
import Node.FS.Sync (mkdir', writeTextFile)
import Tlsp.Common (dist_dirpath)
import Tlsp.Frontend.Example.Frontend1 (frontend)
import Tlsp.Template.Common (TemplateMain)
import Utility (replaceFormatVars')

app_modulename = "Tlsp.Frontend.Example.Frontend1.App"

main :: TemplateMain
main { base_url } = do
  mkdir' (dist_dirpath <> base_url) { mode: permsAll, recursive: true }
  void $ execSync $ replaceFormatVars' { app_modulename, main_js_filepath }
    "bun spago bundle --bundle-type app --module {{app_modulename}} --outfile {{main_js_filepath}}"
  writeTextFile UTF8 (dist_dirpath <> base_url <> "index.html") index_html
  pure { name: frontend.name }

  where

  main_js_url = "/" <> base_url <> "main.js"
  main_js_filepath = dist_dirpath <> main_js_url

  index_html =
    replaceFormatVars' { title: "Frontend: " <> frontend.name, main_js_url }
      """
  <!DOCTYPE html>
  <html lang="en">

  <body></body>

  <head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{{title}}</title>
  <script src="{{main_js_url}}"></script>
  <style>
  html, body {
    margin: 0;
    padding: 0;
  }
  </style>
  </head>

  </html>
  """
