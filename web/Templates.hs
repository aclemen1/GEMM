-- |
-- Module      : Templates
-- Description : HTML templates for the GEMM web interface.
module Templates
  ( indexPage
  , resultFragment
  , errorFragment
  ) where

import GEMM.Types (Group, groupInDegree)
import GEMM.LaTeX (renderGroup)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy as TL
import Cache (ComputedResult(..), SpaceType(..))

-- | The full index HTML page.
indexPage :: String
indexPage = unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "<meta charset=\"utf-8\">"
  , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "<title>GEMM — Generalized Eilenberg-MacLane Machine</title>"
  , "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.16.21/dist/katex.min.css\">"
  , "<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.21/dist/katex.min.js\"></script>"
  , "<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.21/dist/contrib/auto-render.min.js\"></script>"
  , "<script src=\"https://unpkg.com/htmx.org@2.0.4\"></script>"
  , "<style>"
  , ":root { --bg: #0d1117; --surface: #161b22; --border: #30363d;"
  , "  --text: #e6edf3; --muted: #8b949e; --accent: #58a6ff;"
  , "  --accent2: #3fb950; --error: #f85149; }"
  , "*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }"
  , "body { font-family: 'SF Mono', 'Fira Code', 'JetBrains Mono', monospace;"
  , "  background: var(--bg); color: var(--text); line-height: 1.6;"
  , "  max-width: 900px; margin: 0 auto; padding: 2rem 1rem; }"
  , "h1 { font-size: 1.5rem; font-weight: 600; margin-bottom: 0.25rem; }"
  , ".subtitle { color: var(--muted); font-size: 0.875rem; margin-bottom: 2rem; }"
  , ".card { background: var(--surface); border: 1px solid var(--border);"
  , "  border-radius: 8px; padding: 1.5rem; margin-bottom: 1.5rem; }"
  , ".card h2 { font-size: 1rem; margin-bottom: 1rem; color: var(--accent); }"
  , "label { display: block; color: var(--muted); font-size: 0.8rem;"
  , "  margin-bottom: 0.25rem; }"
  , "input[type=number], select { background: var(--bg); color: var(--text);"
  , "  border: 1px solid var(--border); border-radius: 4px;"
  , "  padding: 0.5rem 0.75rem; font-family: inherit; font-size: 0.9rem;"
  , "  width: 100%; }"
  , "input:focus, select:focus { outline: none; border-color: var(--accent); }"
  , ".params { display: grid; grid-template-columns: repeat(auto-fit, minmax(120px, 1fr));"
  , "  gap: 1rem; margin-bottom: 1rem; }"
  , ".pf-only { transition: opacity 0.2s; }"
  , "button { background: var(--accent); color: var(--bg); border: none;"
  , "  border-radius: 4px; padding: 0.6rem 1.5rem; font-family: inherit;"
  , "  font-size: 0.9rem; font-weight: 600; cursor: pointer; }"
  , "button:hover { opacity: 0.9; }"
  , "button:disabled { opacity: 0.5; cursor: not-allowed; }"
  , ".htmx-request button { opacity: 0.5; }"
  , ".spinner { display: none; }"
  , ".htmx-request .spinner { display: inline; }"
  , "table { width: 100%; border-collapse: collapse; font-size: 0.85rem; }"
  , "th { text-align: left; color: var(--muted); font-weight: 500;"
  , "  border-bottom: 1px solid var(--border); padding: 0.5rem 0.75rem; }"
  , "td { padding: 0.5rem 0.75rem; border-bottom: 1px solid var(--border); }"
  , "tr:hover { background: rgba(88, 166, 255, 0.04); }"
  , ".exports { display: flex; gap: 0.75rem; margin-top: 1rem; }"
  , ".exports a { color: var(--accent); text-decoration: none; font-size: 0.85rem;"
  , "  border: 1px solid var(--border); border-radius: 4px; padding: 0.4rem 0.8rem; }"
  , ".exports a:hover { background: rgba(88, 166, 255, 0.1); }"
  , ".error { color: var(--error); padding: 1rem; }"
  , ".tabs { display: flex; gap: 0; margin-bottom: 1rem; }"
  , ".tab { padding: 0.4rem 1rem; cursor: pointer; color: var(--muted);"
  , "  border-bottom: 2px solid transparent; font-size: 0.85rem; }"
  , ".tab.active { color: var(--accent); border-bottom-color: var(--accent); }"
  , "footer { text-align: center; color: var(--muted); font-size: 0.75rem;"
  , "  margin-top: 3rem; }"
  , "footer a { color: var(--accent); text-decoration: none; }"
  , "</style>"
  , "</head>"
  , "<body>"
  , ""
  , "<h1>GEMM</h1>"
  , "<div class=\"subtitle\">Generalized Eilenberg-MacLane Machine &mdash; "
  , "  computes the integral (co)homology of Eilenberg-MacLane spaces</div>"
  , ""
  , "<form class=\"card\" hx-post=\"/compute\" hx-target=\"#result\""
  , "  hx-indicator=\"this\">"
  , "  <h2>Parameters</h2>"
  , "  <div style=\"margin-bottom:1rem\">"
  , "    <label>Space type</label>"
  , "    <select name=\"space\" id=\"space-select\" onchange=\"togglePF()\">"
  , "      <option value=\"P\">K(&#x2124;/p<sup>f</sup>, n)</option>"
  , "      <option value=\"Z\">K(&#x2124;, n)</option>"
  , "    </select>"
  , "  </div>"
  , "  <div class=\"params\">"
  , "    <div class=\"pf-only\" id=\"field-p\">"
  , "      <label>p (prime)</label>"
  , "      <input type=\"number\" name=\"p\" value=\"2\" min=\"2\">"
  , "    </div>"
  , "    <div class=\"pf-only\" id=\"field-f\">"
  , "      <label>f (exponent)</label>"
  , "      <input type=\"number\" name=\"f\" value=\"1\" min=\"1\">"
  , "    </div>"
  , "    <div>"
  , "      <label>n (dimension)</label>"
  , "      <input type=\"number\" name=\"n\" value=\"2\" min=\"1\">"
  , "    </div>"
  , "    <div>"
  , "      <label>range</label>"
  , "      <input type=\"number\" name=\"range\" value=\"20\" min=\"1\">"
  , "    </div>"
  , "  </div>"
  , "  <button type=\"submit\">Compute <span class=\"spinner\">...</span></button>"
  , "</form>"
  , ""
  , "<div id=\"result\"></div>"
  , ""
  , "<footer>"
  , "  <p>GEMM &copy; Alain Cl&eacute;ment &mdash; "
  , "  <a href=\"https://github.com/aclemen1/GEMM\">Source on GitHub</a></p>"
  , "</footer>"
  , ""
  , "<script>"
  , "function togglePF() {"
  , "  const v = document.getElementById('space-select').value;"
  , "  const show = v === 'P';"
  , "  document.getElementById('field-p').style.opacity = show ? '1' : '0.3';"
  , "  document.getElementById('field-f').style.opacity = show ? '1' : '0.3';"
  , "  document.querySelectorAll('.pf-only input').forEach(i => i.disabled = !show);"
  , "}"
  , "document.addEventListener('htmx:afterSwap', function(evt) {"
  , "  if (typeof renderMathInElement !== 'undefined') {"
  , "    renderMathInElement(document.getElementById('result'), {"
  , "      delimiters: ["
  , "        { left: '\\\\(', right: '\\\\)', display: false },"
  , "        { left: '\\\\[', right: '\\\\]', display: true }"
  , "      ]"
  , "    });"
  , "  }"
  , "});"
  , "</script>"
  , "</body>"
  , "</html>"
  ]

-- | Render the result as an HTML fragment for htmx.
resultFragment :: SpaceType -> ComputedResult -> String
resultFragment sp cr = unlines $
  [ "<div class=\"card\">"
  , "  <h2>" ++ spaceName ++ "</h2>"
  , "  <div class=\"tabs\">"
  , "    <div class=\"tab active\" onclick=\"showTab(this,'hom')\">Homology</div>"
  , "    <div class=\"tab\" onclick=\"showTab(this,'cohom')\">Cohomology</div>"
  , "  </div>"
  , "  <div id=\"hom\">"
  , "  <table><tr><th>Degree</th><th>Group</th></tr>"
  ] ++
  [ "  <tr><td>" ++ show deg ++ "</td><td>\\("
    ++ groupToLatex (groupInDegree deg (crHomology cr))
    ++ "\\)</td></tr>"
  | deg <- [0 .. crRange cr]
  ] ++
  [ "  </table>"
  , "  </div>"
  , "  <div id=\"cohom\" style=\"display:none\">"
  , "  <table><tr><th>Degree</th><th>Group</th></tr>"
  ] ++
  [ "  <tr><td>" ++ show deg ++ "</td><td>\\("
    ++ groupToLatex (groupInDegree deg (crCohomology cr))
    ++ "\\)</td></tr>"
  | deg <- [0 .. crRange cr]
  ] ++
  [ "  </table>"
  , "  </div>"
  , "  <div class=\"exports\">"
  , "    <a href=\"/export/latex?" ++ qparams ++ "\">LaTeX</a>"
  , "    <a href=\"/export/json?" ++ qparams ++ "\">JSON</a>"
  ] ++
  (if sp == SpacePF
   then ["    <a href=\"/export/cert?" ++ qparams ++ "\">Lean certificate</a>"]
   else []) ++
  [ "  </div>"
  , "</div>"
  , "<script>"
  , "function showTab(el, id) {"
  , "  el.parentNode.querySelectorAll('.tab').forEach(t => t.classList.remove('active'));"
  , "  el.classList.add('active');"
  , "  document.getElementById('hom').style.display = id==='hom' ? '' : 'none';"
  , "  document.getElementById('cohom').style.display = id==='cohom' ? '' : 'none';"
  , "  if (typeof renderMathInElement !== 'undefined') {"
  , "    renderMathInElement(document.getElementById(id), {"
  , "      delimiters: [{ left: '\\\\(', right: '\\\\)', display: false }]"
  , "    });"
  , "  }"
  , "}"
  , "</script>"
  ]
  where
    spaceName = case sp of
      SpacePF -> "H<sub>*</sub>(K(&#x2124;/"
        ++ show (crP cr) ++ "<sup>" ++ show (crF cr) ++ "</sup>, "
        ++ show (crN cr) ++ "); &#x2124;) &mdash; range "
        ++ show (crRange cr)
      SpaceZ -> "H<sub>*</sub>(K(&#x2124;, "
        ++ show (crN cr) ++ "); &#x2124;) &mdash; range "
        ++ show (crRange cr)
    qparams = case sp of
      SpacePF -> "space=P&p=" ++ show (crP cr) ++ "&f=" ++ show (crF cr)
                  ++ "&n=" ++ show (crN cr) ++ "&range=" ++ show (crRange cr)
      SpaceZ  -> "space=Z&n=" ++ show (crN cr) ++ "&range=" ++ show (crRange cr)

-- | Render an error message as an HTML fragment.
errorFragment :: String -> String
errorFragment msg =
  "<div class=\"card\"><div class=\"error\">" ++ escapeHtml msg ++ "</div></div>"

-- | Convert a Group to a LaTeX string for KaTeX rendering.
groupToLatex :: Group -> String
groupToLatex = TL.unpack . toLazyText . renderGroup

-- | Basic HTML escaping.
escapeHtml :: String -> String
escapeHtml = concatMap esc
  where
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc '&' = "&amp;"
    esc '"' = "&quot;"
    esc c   = [c]
