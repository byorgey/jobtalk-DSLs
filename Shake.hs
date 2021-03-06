import           Development.Shake
import           Development.Shake.FilePath

lhs2TeX, pdflatex, mklatex :: String
lhs2TeX  = "lhs2TeX"
pdflatex = "pdflatex"
mklatex  = "mklatex"

main :: IO ()
main = shake shakeOptions $ do

    want ["2015-jobtalk-DSLs.pdf"]

    "*.tex" *> \output -> do
        let input = replaceExtension output "lhs"
        hsFiles <- getDirectoryFiles "" ["*.hs"]
        need (input : hsFiles)
        cmd lhs2TeX $ ["--poly", "-o", output] ++ [input]

    "*.pdf" *> \output -> do
        let input = replaceExtension output "tex"
        need [input, "GUI.hs", "Icons.hs", "Hypothetical.hs"]
        cmd pdflatex $ ["--enable-write18", input]
--        cmd "scp" [output, "byorgey@rath:"]
--        system' pdflatex $ ["--enable-write18", input]
