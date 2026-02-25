{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isAlphaNum, isSpace)
import Data.Colour.SRGB (sRGB24)
import Data.Default.Class (def)
import qualified Data.Text as T
import Graphics.Rendering.Chart.Backend.Diagrams (FileOptions (..), toFile)
import Graphics.Rendering.Chart.Easy
import Hakyll
import System.Directory (createDirectoryIfMissing)
import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc)
import Text.Pandoc.Walk (walk)

-- Colors matching the original Mermaid charts
clRed, clGreen, clGray, clBlue, clAmber, clLtGray :: Colour Double
clRed = sRGB24 0xef 0x44 0x44 -- #ef4444
clGreen = sRGB24 0x05 0x96 0x69 -- #059669
clGray = sRGB24 0x64 0x74 0x8b -- #64748b
clBlue = sRGB24 0x25 0x63 0xeb -- #2563eb
clAmber = sRGB24 0xd9 0x77 0x06 -- #d97706
clLtGray = sRGB24 0x94 0xa3 0xb8 -- #94a3b8

chartSize :: (Double, Double)
chartSize = (700, 400)

main :: IO ()
main = do
  buildCharts
  hakyll $ do
    match "style.css" $ do
      route idRoute
      compile compressCssCompiler

    match "templates/*" $
      compile templateBodyCompiler

    match "charts/*.svg" $ do
      route idRoute
      compile copyFileCompiler

    match "index.md" $ do
      route $ setExtension "html"
      compile $
        pandocCompilerWithTransform
          defaultHakyllReaderOptions
          defaultHakyllWriterOptions
          fixHeadingIds
          >>= loadAndApplyTemplate "templates/default.html" defaultContext

-- ---------------------------------------------------------------------------
-- Chart generation
-- ---------------------------------------------------------------------------

buildCharts :: IO ()
buildCharts = do
  createDirectoryIfMissing True "charts"
  chartArchitecture
  chartOptimizationJourney
  chartDgemmComparison
  chartCrossover
  chartFfiOverhead
  chartBatchedCalls

-- Architecture diagram — minimal metro-map style
chartArchitecture :: IO ()
chartArchitecture = writeFile "charts/architecture.svg" $ unlines
  [ "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 640 380\""
  , "     width=\"640\" height=\"380\""
  , "     font-family=\"'Inter',system-ui,-apple-system,sans-serif\">"
  , ""
  , "  <!-- Entry points -->"
  , "  <text x=\"90\" y=\"28\" font-size=\"12\" fill=\"#475569\""
  , "        font-family=\"'SF Mono',Consolas,monospace\">ccall safe</text>"
  , "  <text x=\"90\" y=\"46\" font-size=\"11\" fill=\"#94a3b8\">Haskell program</text>"
  , "  <line x1=\"90\" y1=\"55\" x2=\"90\" y2=\"82\" stroke=\"#cbd5e1\" stroke-width=\"1.5\"/>"
  , "  <polygon points=\"85,80 90,90 95,80\" fill=\"#cbd5e1\"/>"
  , ""
  , "  <text x=\"630\" y=\"28\" font-size=\"12\" fill=\"#475569\" text-anchor=\"end\""
  , "        font-family=\"'SF Mono',Consolas,monospace\">GOMP_parallel()</text>"
  , "  <text x=\"630\" y=\"46\" font-size=\"11\" fill=\"#94a3b8\" text-anchor=\"end\">C program</text>"
  , "  <line x1=\"540\" y1=\"55\" x2=\"540\" y2=\"82\" stroke=\"#cbd5e1\" stroke-width=\"1.5\"/>"
  , "  <polygon points=\"535,80 540,90 545,80\" fill=\"#cbd5e1\"/>"
  , ""
  , "  <!-- Runtime container -->"
  , "  <rect x=\"55\" y=\"64\" width=\"585\" height=\"216\" rx=\"10\""
  , "        fill=\"#f8fafc\" stroke=\"#e2e8f0\" stroke-width=\"1.5\"/>"
  , "  <text x=\"348\" y=\"82\" text-anchor=\"middle\" font-size=\"11\""
  , "        font-weight=\"600\" fill=\"#059669\""
  , "        font-family=\"'SF Mono',Consolas,monospace\">ghc_omp_runtime_rts.c</text>"
  , ""
  , "  <!-- Thick U-shaped track -->"
  , "  <path d=\"M 90 110 L 540 110 L 540 230 L 90 230\""
  , "        fill=\"none\" stroke=\"#2563eb\" stroke-width=\"3\""
  , "        stroke-linejoin=\"round\" stroke-linecap=\"round\""
  , "        stroke-opacity=\"0.15\"/>"
  , ""
  , "  <!-- Station 1 -->"
  , "  <circle cx=\"90\" cy=\"110\" r=\"14\" fill=\"#2563eb\"/>"
  , "  <text x=\"90\" y=\"115\" text-anchor=\"middle\" font-size=\"12\""
  , "        font-weight=\"700\" fill=\"white\">1</text>"
  , "  <text x=\"118\" y=\"106\" font-size=\"12\" font-weight=\"600\""
  , "        fill=\"#1e293b\">ensure_rts()</text>"
  , "  <text x=\"118\" y=\"121\" font-size=\"10\" fill=\"#94a3b8\">Boot RTS if needed</text>"
  , ""
  , "  <!-- Arrow 1\x2192\x0032 -->"
  , "  <polygon points=\"248,105 256,110 248,115\" fill=\"#93c5fd\"/>"
  , ""
  , "  <!-- Station 2 -->"
  , "  <circle cx=\"315\" cy=\"110\" r=\"14\" fill=\"#2563eb\"/>"
  , "  <text x=\"315\" y=\"115\" text-anchor=\"middle\" font-size=\"12\""
  , "        font-weight=\"700\" fill=\"white\">2</text>"
  , "  <text x=\"340\" y=\"106\" font-size=\"12\" font-weight=\"600\""
  , "        fill=\"#1e293b\">Store fn/data</text>"
  , "  <text x=\"340\" y=\"121\" font-size=\"10\" fill=\"#94a3b8\">Set outlined function ptr</text>"
  , ""
  , "  <!-- Arrow 2\x2192\x0033 -->"
  , "  <polygon points=\"468,105 476,110 468,115\" fill=\"#93c5fd\"/>"
  , ""
  , "  <!-- Station 3 -->"
  , "  <circle cx=\"540\" cy=\"110\" r=\"14\" fill=\"#2563eb\"/>"
  , "  <text x=\"540\" y=\"115\" text-anchor=\"middle\" font-size=\"12\""
  , "        font-weight=\"700\" fill=\"white\">3</text>"
  , "  <text x=\"418\" y=\"88\" font-size=\"12\" font-weight=\"600\""
  , "        fill=\"#1e293b\">Wake workers</text>"
  , "  <text x=\"418\" y=\"76\" font-size=\"10\" fill=\"#94a3b8\">Bump generation counter</text>"
  , ""
  , "  <!-- Arrow 3\x2192\x0034 (down) -->"
  , "  <polygon points=\"535,168 540,176 545,168\" fill=\"#93c5fd\"/>"
  , ""
  , "  <!-- Station 4 -->"
  , "  <circle cx=\"540\" cy=\"230\" r=\"14\" fill=\"#2563eb\"/>"
  , "  <text x=\"540\" y=\"235\" text-anchor=\"middle\" font-size=\"12\""
  , "        font-weight=\"700\" fill=\"white\">4</text>"
  , "  <text x=\"418\" y=\"252\" font-size=\"12\" font-weight=\"600\""
  , "        fill=\"#1e293b\">Start barrier</text>"
  , "  <text x=\"418\" y=\"265\" font-size=\"10\" fill=\"#94a3b8\">Sense-reversing barrier</text>"
  , ""
  , "  <!-- Arrow 4\x2192\x0035 -->"
  , "  <polygon points=\"392,225 384,230 392,235\" fill=\"#93c5fd\"/>"
  , ""
  , "  <!-- Station 5 -->"
  , "  <circle cx=\"315\" cy=\"230\" r=\"14\" fill=\"#2563eb\"/>"
  , "  <text x=\"315\" y=\"235\" text-anchor=\"middle\" font-size=\"12\""
  , "        font-weight=\"700\" fill=\"white\">5</text>"
  , "  <text x=\"220\" y=\"225\" font-size=\"12\" font-weight=\"600\""
  , "        fill=\"#1e293b\">fn(data)</text>"
  , "  <text x=\"220\" y=\"240\" font-size=\"10\" fill=\"#94a3b8\">Execute user code</text>"
  , ""
  , "  <!-- Arrow 5\x2192\x0036 -->"
  , "  <polygon points=\"172,225 164,230 172,235\" fill=\"#93c5fd\"/>"
  , ""
  , "  <!-- Station 6 -->"
  , "  <circle cx=\"90\" cy=\"230\" r=\"14\" fill=\"#2563eb\"/>"
  , "  <text x=\"90\" y=\"235\" text-anchor=\"middle\" font-size=\"12\""
  , "        font-weight=\"700\" fill=\"white\">6</text>"
  , "  <text x=\"118\" y=\"225\" font-size=\"12\" font-weight=\"600\""
  , "        fill=\"#1e293b\">End barrier</text>"
  , "  <text x=\"118\" y=\"240\" font-size=\"10\" fill=\"#94a3b8\">All threads synchronize</text>"
  , ""
  , "  <!-- Down to RTS -->"
  , "  <line x1=\"315\" y1=\"260\" x2=\"315\" y2=\"290\" stroke=\"#cbd5e1\" stroke-width=\"1.5\"/>"
  , "  <polygon points=\"310,288 315,298 320,288\" fill=\"#cbd5e1\"/>"
  , ""
  , "  <!-- GHC RTS — simple inline row -->"
  , "  <text x=\"315\" y=\"320\" text-anchor=\"middle\" font-size=\"12\""
  , "        font-weight=\"600\" fill=\"#475569\">GHC Runtime System</text>"
  , "  <text x=\"315\" y=\"340\" text-anchor=\"middle\" font-size=\"11\""
  , "        fill=\"#94a3b8\""
  , "        font-family=\"'SF Mono',Consolas,monospace\">"
  , "    Cap 0 (master) &#xb7; Cap 1 &#xb7; Cap 2 &#xb7; Cap 3</text>"
  , "  <text x=\"315\" y=\"358\" text-anchor=\"middle\" font-size=\"10\""
  , "        fill=\"#cbd5e1\">OS threads pinned to Capabilities via rts_setInCallCapability()</text>"
  , "</svg>"
  ]

-- Chart 1: Optimization Journey — grouped bar, 3 series × 2 categories
chartOptimizationJourney :: IO ()
chartOptimizationJourney =
  toFile (def {_fo_size = chartSize}) "charts/optimization-journey.svg" $ do
    layout_title .= "Optimization Journey: Phase 2 \x2192 Phase 3 (4 threads, \x03bcs)"
    layout_x_axis . laxis_generate .= autoIndexAxis ["Fork/join", "Barrier"]
    layout_y_axis . laxis_title .= "Latency (\x03bcs)"
    setColors [opaque clRed, opaque clGreen, opaque clGray]
    plot $
      plotBars
        <$> bars
          ["Phase 2 (mutex+condvar)", "Phase 3 (lock-free)", "Native libgomp"]
          (addIndexes ([[24.35, 0.81, 0.97], [7.01, 0.25, 0.51]] :: [[Double]]))

-- Chart 2: DGEMM Head-to-Head — grouped bar, 2 series × 4 categories
chartDgemmComparison :: IO ()
chartDgemmComparison =
  toFile (def {_fo_size = chartSize}) "charts/dgemm-comparison.svg" $ do
    layout_title .= "DGEMM Head-to-Head: Native libgomp vs RTS (4 threads, ms)"
    layout_x_axis . laxis_generate .= autoIndexAxis ["N=128", "N=256", "N=512", "N=1024"]
    layout_y_axis . laxis_title .= "Time (ms)"
    setColors [opaque clGray, opaque clBlue]
    plot $
      plotBars
        <$> bars
          ["Native libgomp", "RTS-backed"]
          (addIndexes ([[0.86, 0.94], [12.62, 12.28], [77.51, 76.96], [748.83, 663.37]] :: [[Double]]))

-- Chart 3: Parallelism Crossover — line chart, 2 series
chartCrossover :: IO ()
chartCrossover =
  toFile (def {_fo_size = chartSize}) "charts/crossover.svg" $ do
    layout_title .= "Parallelism Crossover: Sequential vs Parallel (4 threads)"
    layout_x_axis . laxis_generate .= autoIndexAxis ["100", "200", "500", "1K", "5K", "100K"]
    layout_y_axis . laxis_title .= "Time (\x03bcs)"
    setColors [opaque clAmber, opaque clBlue]
    let idx = map PlotIndex [0 ..]
    plot $ line "Sequential C (unsafe FFI)" [zip idx ([0.5, 1.3, 3.6, 7.5, 49.0, 1132] :: [Double])]
    plot $ line "Parallel OpenMP (safe FFI)" [zip idx ([2.1, 2.2, 2.9, 3.9, 16.6, 279] :: [Double])]

-- Chart 4: FFI Calling Convention Overhead — simple bar, 1 series
chartFfiOverhead :: IO ()
chartFfiOverhead =
  toFile (def {_fo_size = chartSize}) "charts/ffi-overhead.svg" $ do
    layout_title .= "FFI Calling Convention Overhead (ns/call)"
    layout_x_axis . laxis_generate .= autoIndexAxis ["prim (Cmm)", "ccall unsafe", "ccall safe"]
    layout_y_axis . laxis_title .= "Latency (ns)"
    layout_legend .= Nothing
    setColors [opaque clBlue]
    plot $
      plotBars
        <$> bars
          [""]
          (addIndexes ([[0.1], [2.3], [67.5]] :: [[Double]]))

-- Chart 5: Batched Safe Calls — line chart, 2 series
chartBatchedCalls :: IO ()
chartBatchedCalls =
  toFile (def {_fo_size = chartSize}) "charts/batched-calls.svg" $ do
    layout_title .= "Batched Safe Calls: Per-Call Overhead (ns)"
    layout_x_axis . laxis_generate .= autoIndexAxis ["1", "2", "5", "10", "20", "50", "100"]
    layout_y_axis . laxis_title .= "ns/call"
    setColors [opaque clGreen, opaque clLtGray]
    let idx = map PlotIndex [0 ..]
    plot $ line "Cmm batched" [zip idx ([69.1, 36.1, 15.2, 8.7, 5.3, 3.4, 2.7] :: [Double])]
    plot $ line "Standard safe" [zip idx ([72.4, 71.3, 73.0, 71.0, 71.1, 71.7, 71.4] :: [Double])]

-- ---------------------------------------------------------------------------
-- Pandoc transform: Jekyll-compatible heading IDs
-- ---------------------------------------------------------------------------

-- Generate heading IDs matching Jekyll/kramdown convention.
-- Jekyll keeps leading numbers ("1-abstract"); pandoc strips them ("abstract").
fixHeadingIds :: Pandoc -> Pandoc
fixHeadingIds = walk go
  where
    go :: Block -> Block
    go (Header lvl (_, cls, kvs) ils) =
      Header lvl (jekyllId (extractText ils), cls, kvs) ils
    go b = b

    extractText :: [Inline] -> T.Text
    extractText = foldMap inlineText

    inlineText :: Inline -> T.Text
    inlineText (Str t) = t
    inlineText Space = " "
    inlineText SoftBreak = " "
    inlineText (Code _ t) = t
    inlineText (Emph is) = extractText is
    inlineText (Strong is) = extractText is
    inlineText (Strikeout is) = extractText is
    inlineText (Link _ is _) = extractText is
    inlineText _ = ""

    jekyllId :: T.Text -> T.Text
    jekyllId =
      T.intercalate "-"
        . T.words
        . T.filter (\c -> isAlphaNum c || isSpace c || c == '-')
        . T.toLower
