import Data.List
import Test.DocTest

sources =
    [ "src/ExRender.hs"
    , "src/ExRender/Base.hs"
    , "src/ExRender/Dependency.hs"
    , "src/ExRender/Haddock.hs"
    , "src/ExRender/License.hs"
    , "src/Main.hs"
    ]

optarg _ [] = []
optarg x ys = x : intersperse x ys

main = doctest $ optarg "-isrc" sources
