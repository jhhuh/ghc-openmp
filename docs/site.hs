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

-- Architecture diagram — boxed components with U-shaped flow
chartArchitecture :: IO ()
chartArchitecture = writeFile "charts/architecture.svg" $ unlines
  [ "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 700 470\""
  , "     width=\"700\" height=\"470\""
  , "     font-family=\"'Inter',system-ui,-apple-system,sans-serif\">"
  , "  <defs>"
  , "    <marker id=\"arr\" viewBox=\"0 0 10 7\" refX=\"10\" refY=\"3.5\""
  , "            markerWidth=\"8\" markerHeight=\"6\" orient=\"auto\">"
  , "      <polygon points=\"0,0 10,3.5 0,7\" fill=\"#94a3b8\"/>"
  , "    </marker>"
  , "  </defs>"
  , ""
  , "  <!-- Host: Haskell program -->"
  , "  <rect x=\"50\" y=\"12\" width=\"230\" height=\"58\" rx=\"8\""
  , "        fill=\"#f8fafc\" stroke=\"#e2e8f0\" stroke-width=\"1.5\"/>"
  , "  <text x=\"165\" y=\"37\" text-anchor=\"middle\" font-size=\"13\""
  , "        font-weight=\"600\" fill=\"#1e293b\">Haskell program</text>"
  , "  <text x=\"165\" y=\"56\" text-anchor=\"middle\" font-size=\"11\""
  , "        fill=\"#94a3b8\""
  , "        font-family=\"'SF Mono',Consolas,monospace\">ghc -threaded</text>"
  , ""
  , "  <!-- Host: C program -->"
  , "  <rect x=\"420\" y=\"12\" width=\"230\" height=\"58\" rx=\"8\""
  , "        fill=\"#f8fafc\" stroke=\"#e2e8f0\" stroke-width=\"1.5\"/>"
  , "  <text x=\"535\" y=\"37\" text-anchor=\"middle\" font-size=\"13\""
  , "        font-weight=\"600\" fill=\"#1e293b\">C program</text>"
  , "  <text x=\"535\" y=\"56\" text-anchor=\"middle\" font-size=\"11\""
  , "        fill=\"#94a3b8\""
  , "        font-family=\"'SF Mono',Consolas,monospace\">gcc -fopenmp</text>"
  , ""
  , "  <!-- Arrows: Host \x2192 Runtime -->"
  , "  <line x1=\"165\" y1=\"70\" x2=\"165\" y2=\"105\""
  , "        stroke=\"#94a3b8\" stroke-width=\"1.5\" marker-end=\"url(#arr)\"/>"
  , "  <text x=\"180\" y=\"94\" font-size=\"9.5\" fill=\"#64748b\""
  , "        font-family=\"'SF Mono',Consolas,monospace\">ccall safe</text>"
  , "  <line x1=\"535\" y1=\"70\" x2=\"535\" y2=\"105\""
  , "        stroke=\"#94a3b8\" stroke-width=\"1.5\" marker-end=\"url(#arr)\"/>"
  , "  <text x=\"420\" y=\"94\" font-size=\"9.5\" fill=\"#64748b\""
  , "        font-family=\"'SF Mono',Consolas,monospace\">GOMP_parallel()</text>"
  , ""
  , "  <!-- Runtime container -->"
  , "  <rect x=\"30\" y=\"108\" width=\"640\" height=\"210\" rx=\"10\""
  , "        fill=\"#f0fdf4\" stroke=\"#bbf7d0\" stroke-width=\"1.5\"/>"
  , "  <text x=\"350\" y=\"128\" text-anchor=\"middle\" font-size=\"12\""
  , "        font-weight=\"600\" fill=\"#059669\""
  , "        font-family=\"'SF Mono',Consolas,monospace\">ghc_omp_runtime_rts.c</text>"
  , ""
  , "  <!-- U-shaped flow track -->"
  , "  <path d=\"M 110 160 H 590 V 270 H 110\""
  , "        fill=\"none\" stroke=\"#059669\" stroke-width=\"3\""
  , "        stroke-linejoin=\"round\" stroke-linecap=\"round\""
  , "        stroke-opacity=\"0.15\"/>"
  , ""
  , "  <!-- Direction chevrons -->"
  , "  <polygon points=\"258,155 266,160 258,165\" fill=\"#86efac\"/>"
  , "  <polygon points=\"488,155 496,160 488,165\" fill=\"#86efac\"/>"
  , "  <polygon points=\"595,218 590,226 585,218\" fill=\"#86efac\"/>"
  , "  <polygon points=\"432,265 424,270 432,275\" fill=\"#86efac\"/>"
  , "  <polygon points=\"222,265 214,270 222,275\" fill=\"#86efac\"/>"
  , ""
  , "  <!-- Station 1 -->"
  , "  <circle cx=\"110\" cy=\"160\" r=\"14\" fill=\"#059669\"/>"
  , "  <text x=\"110\" y=\"165\" text-anchor=\"middle\" font-size=\"12\""
  , "        font-weight=\"700\" fill=\"white\">1</text>"
  , "  <text x=\"110\" y=\"195\" text-anchor=\"middle\" font-size=\"11\""
  , "        font-weight=\"500\" fill=\"#1e293b\""
  , "        font-family=\"'SF Mono',Consolas,monospace\">ensure_rts()</text>"
  , ""
  , "  <!-- Station 2 -->"
  , "  <circle cx=\"350\" cy=\"160\" r=\"14\" fill=\"#059669\"/>"
  , "  <text x=\"350\" y=\"165\" text-anchor=\"middle\" font-size=\"12\""
  , "        font-weight=\"700\" fill=\"white\">2</text>"
  , "  <text x=\"350\" y=\"195\" text-anchor=\"middle\" font-size=\"11\""
  , "        font-weight=\"500\" fill=\"#1e293b\">Store fn/data</text>"
  , ""
  , "  <!-- Station 3 -->"
  , "  <circle cx=\"590\" cy=\"160\" r=\"14\" fill=\"#059669\"/>"
  , "  <text x=\"590\" y=\"165\" text-anchor=\"middle\" font-size=\"12\""
  , "        font-weight=\"700\" fill=\"white\">3</text>"
  , "  <text x=\"590\" y=\"195\" text-anchor=\"middle\" font-size=\"11\""
  , "        font-weight=\"500\" fill=\"#1e293b\">Wake workers</text>"
  , ""
  , "  <!-- Station 4 -->"
  , "  <circle cx=\"590\" cy=\"270\" r=\"14\" fill=\"#059669\"/>"
  , "  <text x=\"590\" y=\"275\" text-anchor=\"middle\" font-size=\"12\""
  , "        font-weight=\"700\" fill=\"white\">4</text>"
  , "  <text x=\"590\" y=\"302\" text-anchor=\"middle\" font-size=\"11\""
  , "        font-weight=\"500\" fill=\"#1e293b\">Start barrier</text>"
  , ""
  , "  <!-- Station 5 -->"
  , "  <circle cx=\"350\" cy=\"270\" r=\"14\" fill=\"#059669\"/>"
  , "  <text x=\"350\" y=\"275\" text-anchor=\"middle\" font-size=\"12\""
  , "        font-weight=\"700\" fill=\"white\">5</text>"
  , "  <text x=\"350\" y=\"302\" text-anchor=\"middle\" font-size=\"11\""
  , "        font-weight=\"500\" fill=\"#1e293b\""
  , "        font-family=\"'SF Mono',Consolas,monospace\">fn(data)</text>"
  , ""
  , "  <!-- Station 6 -->"
  , "  <circle cx=\"110\" cy=\"270\" r=\"14\" fill=\"#059669\"/>"
  , "  <text x=\"110\" y=\"275\" text-anchor=\"middle\" font-size=\"12\""
  , "        font-weight=\"700\" fill=\"white\">6</text>"
  , "  <text x=\"110\" y=\"302\" text-anchor=\"middle\" font-size=\"11\""
  , "        font-weight=\"500\" fill=\"#1e293b\">End barrier</text>"
  , ""
  , "  <!-- Arrow: Runtime \x2192 RTS -->"
  , "  <line x1=\"350\" y1=\"318\" x2=\"350\" y2=\"348\""
  , "        stroke=\"#94a3b8\" stroke-width=\"1.5\" marker-end=\"url(#arr)\"/>"
  , ""
  , "  <!-- GHC RTS container -->"
  , "  <rect x=\"30\" y=\"350\" width=\"640\" height=\"110\" rx=\"10\""
  , "        fill=\"#eff6ff\" stroke=\"#bfdbfe\" stroke-width=\"1.5\"/>"
  , "  <text x=\"350\" y=\"372\" text-anchor=\"middle\" font-size=\"12\""
  , "        font-weight=\"600\" fill=\"#2563eb\">GHC Runtime System</text>"
  , ""
  , "  <!-- Capability boxes -->"
  , "  <rect x=\"55\" y=\"385\" width=\"135\" height=\"55\" rx=\"6\""
  , "        fill=\"white\" stroke=\"#93c5fd\"/>"
  , "  <text x=\"122\" y=\"410\" text-anchor=\"middle\" font-size=\"13\""
  , "        font-weight=\"600\" fill=\"#1e293b\">Cap 0</text>"
  , "  <text x=\"122\" y=\"428\" text-anchor=\"middle\" font-size=\"10\""
  , "        fill=\"#64748b\">master</text>"
  , ""
  , "  <rect x=\"205\" y=\"385\" width=\"135\" height=\"55\" rx=\"6\""
  , "        fill=\"white\" stroke=\"#93c5fd\"/>"
  , "  <text x=\"272\" y=\"410\" text-anchor=\"middle\" font-size=\"13\""
  , "        font-weight=\"600\" fill=\"#1e293b\">Cap 1</text>"
  , "  <text x=\"272\" y=\"428\" text-anchor=\"middle\" font-size=\"10\""
  , "        fill=\"#64748b\">worker</text>"
  , ""
  , "  <rect x=\"355\" y=\"385\" width=\"135\" height=\"55\" rx=\"6\""
  , "        fill=\"white\" stroke=\"#93c5fd\"/>"
  , "  <text x=\"422\" y=\"410\" text-anchor=\"middle\" font-size=\"13\""
  , "        font-weight=\"600\" fill=\"#1e293b\">Cap 2</text>"
  , "  <text x=\"422\" y=\"428\" text-anchor=\"middle\" font-size=\"10\""
  , "        fill=\"#64748b\">worker</text>"
  , ""
  , "  <rect x=\"505\" y=\"385\" width=\"135\" height=\"55\" rx=\"6\""
  , "        fill=\"white\" stroke=\"#93c5fd\"/>"
  , "  <text x=\"572\" y=\"410\" text-anchor=\"middle\" font-size=\"13\""
  , "        font-weight=\"600\" fill=\"#1e293b\">Cap 3</text>"
  , "  <text x=\"572\" y=\"428\" text-anchor=\"middle\" font-size=\"10\""
  , "        fill=\"#64748b\">worker</text>"
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
