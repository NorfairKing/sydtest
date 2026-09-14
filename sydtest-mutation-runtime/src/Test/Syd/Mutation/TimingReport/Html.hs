{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The timing report as a standalone HTML page.
--
-- The terminal rendering in "Test.Syd.Mutation.TimingReport" has to fit in a
-- build log, so it shows only the worst few rows of each section.  A run with
-- thousands of mutations needs the whole listing, sorted by whichever column
-- the reader is chasing and filtered to the module they care about, which is
-- what this page is for.
--
-- The page is self-contained: styles and behaviour are inline, there are no
-- external assets, and it opens straight from a Nix store path with no server.
module Test.Syd.Mutation.TimingReport.Html
  ( renderPhaseTimingSummaryHtml,
  )
where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Test.Syd.Mutation.TimingReport
  ( CostBreakdown (..),
    PhaseTimingSummary (..),
    TimingBucket (..),
    TimingDimension (..),
    TimingEntry (..),
    costTotalNanos,
    entryCell,
    entryColumnNames,
    percentText,
    renderDurationNanos,
  )
import Text.Printf (printf)

renderPhaseTimingSummaryHtml :: PhaseTimingSummary -> Text
renderPhaseTimingSummaryHtml summary@PhaseTimingSummary {..} =
  T.pack $
    unlines $
      concat
        [ [ "<!doctype html>",
            "<html lang=\"en\">",
            "<head>",
            "<meta charset=\"utf-8\">",
            "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
            concat ["<title>Timing report: ", escapeHtml phaseTimingSummaryLabel, "</title>"],
            "<style>",
            pageStyle,
            "</style>",
            "</head>",
            "<body>",
            "<main>"
          ],
          headerSection summary,
          costSection summary,
          concatMap (dimensionSection summary) phaseTimingSummaryDimensions,
          entrySection summary,
          [ "</main>",
            "<script>",
            pageScript,
            "</script>",
            "</body>",
            "</html>"
          ]
        ]

headerSection :: PhaseTimingSummary -> [String]
headerSection PhaseTimingSummary {..} =
  concat
    [ [ "<header>",
        concat ["<h1>Timing report<span class=\"phase\">", escapeHtml phaseTimingSummaryLabel, "</span></h1>"],
        "<p class=\"lede\">Where this phase's child processes spent their time. Every share and bar below is a share of the summed child wall time, so a bar in one section is comparable to a bar in another.</p>",
        "<div class=\"stats\">"
      ],
      concatMap
        (uncurry statCard)
        ( [ ("Wall clock", renderDurationNanos phaseTimingSummaryWallNanos),
            ("Children", T.pack (show phaseTimingSummaryChildren)),
            ("Sum of child wall time", renderDurationNanos phaseTimingSummaryChildWallNanos),
            ( "Achieved parallelism",
              T.concat
                [ T.pack (printf "%.2f\215" (parallelismOf phaseTimingSummaryWallNanos phaseTimingSummaryChildWallNanos)),
                  " of ",
                  T.pack (show phaseTimingSummaryJobs)
                ]
            )
          ]
            ++ phaseTimingSummaryNotes
        ),
      ["</div>", "</header>"]
    ]

statCard :: Text -> Text -> [String]
statCard label value =
  [ "<div class=\"stat\">",
    concat ["<div class=\"stat-value\">", escapeHtml value, "</div>"],
    concat ["<div class=\"stat-label\">", escapeHtml label, "</div>"],
    "</div>"
  ]

parallelismOf :: Word64 -> Word64 -> Double
parallelismOf wallNanos childWallNanos =
  if wallNanos == 0
    then 0
    else fromIntegral childWallNanos / fromIntegral wallNanos

-- | The headline: one full-width stacked bar for the whole phase, then a row
-- per cost.  This is the number that decides what would actually make the run
-- faster, so it gets the largest graph on the page.
costSection :: PhaseTimingSummary -> [String]
costSection PhaseTimingSummary {..} =
  let total = phaseTimingSummaryChildWallNanos
      CostBreakdown {..} = phaseTimingSummaryCosts
      costs =
        -- Phrased so each hint holds for both phases: the mutation phase runs
        -- a child per mutation and the coverage phase one per test, but the
        -- lever each cost points at is the same either way.
        [ ("start", "process startup", costProcessNanos, "Spawning the process, RTS init, reading the manifest, building the spec forest, printing output. Dominated by this, the run gets faster by doing more per child process, not by making the tests faster."),
          ("setup", "suite setup", costSetupNanos, "around/aroundAll resources, paid once per child instead of once per suite. Dominated by this, the run gets faster by sharing those resources across children."),
          ("tests", "test execution", costTestNanos, "The leaf tests themselves. Dominated by this, the tests are genuinely slow, or a mutation's covering-test set is too large."),
          ("unattr", "unattributed", costUnattributedNanos, "Children that reported no breakdown, such as one killed on its timeout.")
        ]
      shown = [c | c@(_, _, n, _) <- costs, n > 0]
   in concat
        [ [ "<section>",
            "<h2>Where the child time goes</h2>",
            "<div class=\"hero-bar\">"
          ],
          [ concat
              [ "<span class=\"seg s-",
                cls,
                "\" style=\"width:",
                percentStyle total n,
                "\" title=\"",
                escapeHtml name,
                ": ",
                escapeHtml (renderDurationNanos n),
                "\"></span>"
              ]
          | (cls, name, n, _) <- shown
          ],
          ["</div>", "<div class=\"table-wrap\">", "<table class=\"costs\">", "<tbody>"],
          concat
            [ [ "<tr>",
                concat ["<td class=\"swatch\"><span class=\"key s-", cls, "\"></span></td>"],
                concat ["<th scope=\"row\">", escapeHtml name, "<div class=\"hint\">", escapeHtml hint, "</div></th>"],
                concat ["<td class=\"num\">", escapeHtml (renderDurationNanos n), "</td>"],
                concat ["<td class=\"num\">", escapeHtml (percentText (shareOf total n)), "</td>"],
                "</tr>"
              ]
            | (cls, name, n, hint) <- shown
            ],
          [ "<tr class=\"total\">",
            "<td></td>",
            "<th scope=\"row\">total</th>",
            concat ["<td class=\"num\">", escapeHtml (renderDurationNanos (costTotalNanos phaseTimingSummaryCosts)), "</td>"],
            "<td class=\"num\">100.0%</td>",
            "</tr>",
            "</tbody>",
            "</table>",
            "</div>",
            "</section>"
          ]
        ]

dimensionSection :: PhaseTimingSummary -> TimingDimension -> [String]
dimensionSection PhaseTimingSummary {phaseTimingSummaryChildWallNanos} TimingDimension {..} =
  let total = phaseTimingSummaryChildWallNanos
   in concat
        [ [ "<section>",
            concat ["<h2>By ", escapeHtml timingDimensionName, "</h2>"],
            "<div class=\"table-wrap\">",
            "<table class=\"sortable\">",
            "<thead><tr>",
            concat ["<th>", escapeHtml timingDimensionName, "</th>"],
            "<th class=\"num\">children</th>",
            "<th class=\"num\">time</th>",
            "<th class=\"num\">share</th>",
            "<th class=\"bar-col\">startup / setup / tests</th>",
            "</tr></thead>",
            "<tbody>"
          ],
          concat
            [ [ "<tr>",
                concat ["<td class=\"label\">", escapeHtml timingBucketLabel, "</td>"],
                concat ["<td class=\"num\" data-v=\"", show timingBucketChildren, "\">", show timingBucketChildren, "</td>"],
                concat ["<td class=\"num\" data-v=\"", show timingBucketWallNanos, "\">", escapeHtml (renderDurationNanos timingBucketWallNanos), "</td>"],
                concat ["<td class=\"num\" data-v=\"", show timingBucketWallNanos, "\">", escapeHtml (percentText (shareOf total timingBucketWallNanos)), "</td>"],
                concat ["<td>", stackedBar total timingBucketCosts, "</td>"],
                "</tr>"
              ]
            | TimingBucket {..} <- timingDimensionBuckets
            ],
          ["</tbody>", "</table>", "</div>", "</section>"]
        ]

-- | The per-child listing: every child, with its identity split into its own
-- sortable columns and a filter box over the lot.  This is the detail the
-- terminal report can only gesture at.
entrySection :: PhaseTimingSummary -> [String]
entrySection PhaseTimingSummary {..} =
  let total = phaseTimingSummaryChildWallNanos
      columns = entryColumnNames phaseTimingSummaryEntries
   in concat
        [ [ "<section>",
            "<h2>Per child</h2>",
            "<div class=\"toolbar\">",
            "<input id=\"filter\" type=\"search\" placeholder=\"Filter rows...\" aria-label=\"Filter rows\">",
            concat ["<span class=\"count\" id=\"count\">", show (length phaseTimingSummaryEntries), " rows</span>"],
            "</div>",
            "<div class=\"table-wrap tall\">",
            "<table class=\"sortable\" id=\"entries\">",
            "<thead><tr>"
          ],
          [concat ["<th>", escapeHtml c, "</th>"] | c <- columns],
          [ "<th class=\"num\">wall</th>",
            "<th class=\"num\">startup</th>",
            "<th class=\"num\">setup</th>",
            "<th class=\"num\">tests</th>",
            "<th class=\"bar-col\">startup / setup / tests</th>",
            "</tr></thead>",
            "<tbody>"
          ],
          concatMap (entryRow total columns) phaseTimingSummaryEntries,
          ["</tbody>", "</table>", "</div>", "</section>"]
        ]

entryRow :: Word64 -> [Text] -> TimingEntry -> [String]
entryRow total columns entry@TimingEntry {..} =
  let CostBreakdown {..} = timingEntryCosts
      numCell n =
        concat
          [ "<td class=\"num\" data-v=\"",
            show n,
            "\">",
            escapeHtml (renderDurationNanos n),
            "</td>"
          ]
      -- A child that reported no breakdown has no split to show, and three
      -- zeroes would read as three genuine measurements.  It still sorts to
      -- the bottom on those columns, which is where an unknown belongs.
      splitCell n
        | costUnattributedNanos > 0 = "<td class=\"num unknown\" data-v=\"-1\">&mdash;</td>"
        | otherwise = numCell n
   in concat
        [ ["<tr>"],
          [concat ["<td class=\"label\">", escapeHtml (entryCell c entry), "</td>"] | c <- columns],
          [ numCell timingEntryWallNanos,
            splitCell costProcessNanos,
            splitCell costSetupNanos,
            splitCell costTestNanos,
            concat ["<td>", stackedBar total timingEntryCosts, "</td>"],
            "</tr>"
          ]
        ]

-- | A bar whose length is the breakdown's share of the phase, split into its
-- costs.  Widths are percentages of the track, so the browser does the
-- scaling and the bar stays honest at any window size.
stackedBar :: Word64 -> CostBreakdown -> String
stackedBar total cb@CostBreakdown {..} =
  concat
    [ "<span class=\"track\" title=\"",
      escapeHtml (renderDurationNanos (costTotalNanos cb)),
      "\">",
      concat
        [ concat ["<span class=\"seg s-", cls, "\" style=\"width:", percentStyle total n, "\"></span>"]
        | (cls, n) <-
            [ ("start", costProcessNanos),
              ("setup", costSetupNanos),
              ("tests", costTestNanos),
              ("unattr", costUnattributedNanos)
            ],
          n > 0
        ],
      "</span>"
    ]

shareOf :: Word64 -> Word64 -> Double
shareOf total n = if total == 0 then 0 else fromIntegral n / fromIntegral total

percentStyle :: Word64 -> Word64 -> String
percentStyle total n = printf "%.4f%%" (100 * shareOf total n)

-- | Escape the five characters that can end a text node or an attribute
-- value early.  Every value on the page comes from a module name, a test
-- description or a source path, none of which is under this code's control.
escapeHtml :: Text -> String
escapeHtml = concatMap escapeChar . T.unpack
  where
    escapeChar :: Char -> String
    escapeChar = \case
      '&' -> "&amp;"
      '<' -> "&lt;"
      '>' -> "&gt;"
      '"' -> "&quot;"
      '\'' -> "&#39;"
      c -> [c]

pageStyle :: String
pageStyle =
  intercalate
    "\n"
    [ ":root{--bg:#fbfbfa;--panel:#fff;--ink:#1a1a19;--muted:#6b6b66;--line:#e4e4e0;",
      "--start:#c9432f;--setup:#c98a12;--tests:#2f8f4e;--unattr:#4a63c9;--accent:#3b5bdb}",
      "@media (prefers-color-scheme:dark){:root{--bg:#16161a;--panel:#1d1d22;--ink:#e9e9e6;--muted:#9a9a94;--line:#2e2e35;",
      "--start:#e8705c;--setup:#e5ad3d;--tests:#5cbf7c;--unattr:#7d92e8;--accent:#8ea2f0}}",
      "*{box-sizing:border-box}",
      "body{margin:0;background:var(--bg);color:var(--ink);",
      "font:14px/1.5 system-ui,-apple-system,'Segoe UI',Roboto,sans-serif;font-variant-numeric:tabular-nums}",
      "main{max-width:1400px;margin:0 auto;padding:32px 24px 96px}",
      ".table-wrap{overflow:auto;border:1px solid var(--line);border-radius:8px}",
      "/* A scroll box of its own, so the sticky header sticks to the table rather",
      "   than scrolling away with the page on a listing thousands of rows long. */",
      ".table-wrap.tall{max-height:70vh}",
      ".table-wrap table{border:0;border-radius:0}",
      "header{margin-bottom:36px}",
      "h1{font-size:22px;font-weight:650;margin:0 0 6px;letter-spacing:-.01em}",
      "h1 .phase{color:var(--muted);font-weight:450;margin-left:10px}",
      ".lede{color:var(--muted);margin:0 0 22px;max-width:70ch}",
      "h2{font-size:13px;font-weight:650;text-transform:uppercase;letter-spacing:.07em;",
      "color:var(--muted);margin:0 0 12px}",
      "section{margin-bottom:40px}",
      ".stats{display:flex;flex-wrap:wrap;gap:10px}",
      ".stat{background:var(--panel);border:1px solid var(--line);border-radius:8px;padding:11px 18px;min-width:145px}",
      ".stat-value{font-size:19px;font-weight:600;letter-spacing:-.01em}",
      ".stat-label{color:var(--muted);font-size:12px;margin-top:2px}",
      ".hero-bar{display:flex;height:30px;border-radius:6px;overflow:hidden;background:var(--line);margin-bottom:16px}",
      ".hero-bar .seg{height:100%}",
      ".seg.s-start{background:var(--start)}.seg.s-setup{background:var(--setup)}",
      ".seg.s-tests{background:var(--tests)}.seg.s-unattr{background:var(--unattr)}",
      "table{width:100%;border-collapse:collapse;background:var(--panel)}",
      "th,td{text-align:left;padding:7px 12px;border-bottom:1px solid var(--line);vertical-align:top}",
      "tbody tr:last-child td,tbody tr:last-child th{border-bottom:0}",
      "thead th{position:sticky;top:0;background:var(--panel);z-index:1;font-size:12px;",
      "font-weight:600;color:var(--muted);border-bottom:1px solid var(--line)}",
      "table.sortable thead th{cursor:pointer;user-select:none}",
      "table.sortable thead th:hover{color:var(--ink)}",
      "table.sortable thead th[aria-sort]:after{content:'\\2191';margin-left:5px;opacity:.75}",
      "table.sortable thead th[aria-sort=descending]:after{content:'\\2193'}",
      "tbody tr:hover{background:color-mix(in srgb,var(--accent) 7%,transparent)}",
      ".num{text-align:right;white-space:nowrap;font-variant-numeric:tabular-nums}",
      ".label{word-break:break-word}",
      ".costs th{font-weight:600}",
      ".costs .hint{font-weight:400;color:var(--muted);font-size:12px;margin-top:2px;max-width:62ch}",
      ".costs .swatch{width:1px;padding-right:0}",
      ".key{display:inline-block;width:11px;height:11px;border-radius:3px;margin-top:4px}",
      ".key.s-start{background:var(--start)}.key.s-setup{background:var(--setup)}",
      ".key.s-tests{background:var(--tests)}.key.s-unattr{background:var(--unattr)}",
      ".total th,.total td{font-weight:600;border-top:1px solid var(--line)}",
      ".bar-col{width:220px}",
      ".track{display:flex;width:200px;height:9px;border-radius:5px;overflow:hidden;",
      "background:color-mix(in srgb,var(--ink) 8%,transparent);margin-top:4px}",
      ".track .seg{height:100%}",
      ".toolbar{display:flex;align-items:center;gap:12px;margin-bottom:10px}",
      "#filter{flex:0 1 320px;padding:7px 11px;border:1px solid var(--line);border-radius:7px;",
      "background:var(--panel);color:var(--ink);font:inherit}",
      "#filter:focus{outline:2px solid var(--accent);outline-offset:-1px}",
      ".count{color:var(--muted);font-size:12px}",
      ".num.unknown{color:var(--muted)}"
    ]

-- | Sorting and filtering, in plain DOM calls.
--
-- A numeric column carries the real value in @data-v@ so the sort orders by
-- nanoseconds rather than by the rendered string, in which \"9.0ms\" would
-- come after \"1m02s\".
pageScript :: String
pageScript =
  intercalate
    "\n"
    [ "document.querySelectorAll('table.sortable').forEach(function (table) {",
      "  var heads = table.tHead.rows[0].cells;",
      "  Array.prototype.forEach.call(heads, function (th, i) {",
      "    th.addEventListener('click', function () {",
      "      var desc = th.getAttribute('aria-sort') !== 'descending';",
      "      Array.prototype.forEach.call(heads, function (o) { o.removeAttribute('aria-sort'); });",
      "      th.setAttribute('aria-sort', desc ? 'descending' : 'ascending');",
      "      var body = table.tBodies[0];",
      "      var rows = Array.prototype.slice.call(body.rows);",
      "      var key = function (row) {",
      "        var cell = row.cells[i];",
      "        if (!cell) { return { n: null, s: '' }; }",
      "        var v = cell.getAttribute('data-v');",
      "        return v === null ? { n: null, s: cell.textContent } : { n: Number(v), s: '' };",
      "      };",
      "      rows.sort(function (a, b) {",
      "        var ka = key(a), kb = key(b);",
      "        var c = ka.n !== null && kb.n !== null ? ka.n - kb.n : ka.s.localeCompare(kb.s);",
      "        return desc ? -c : c;",
      "      });",
      "      rows.forEach(function (row) { body.appendChild(row); });",
      "    });",
      "  });",
      "});",
      "var filter = document.getElementById('filter');",
      "if (filter) {",
      "  var entries = document.getElementById('entries');",
      "  var count = document.getElementById('count');",
      "  var rows = Array.prototype.slice.call(entries.tBodies[0].rows);",
      "  filter.addEventListener('input', function () {",
      "    var needle = filter.value.toLowerCase();",
      "    var shown = 0;",
      "    rows.forEach(function (row) {",
      "      var hit = row.textContent.toLowerCase().indexOf(needle) !== -1;",
      "      row.hidden = !hit;",
      "      if (hit) { shown++; }",
      "    });",
      "    count.textContent = shown + ' of ' + rows.length + ' rows';",
      "  });",
      "}"
    ]
