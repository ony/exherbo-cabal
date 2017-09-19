import Data.List
import Test.DocTest

sources :: [String]
sources =
    [ "src/ExRender.hs"
    , "src/ExRender/Base.hs"
    , "src/ExRender/Dependency.hs"
    , "src/ExRender/Haddock.hs"
    , "src/ExRender/License.hs"
    , "src/Main.hs"
    ]

optarg :: t -> [t] -> [t]
optarg _ [] = []
optarg x ys = x : intersperse x ys

main :: IO ()
main = doctest $ optarg "-isrc" sources
