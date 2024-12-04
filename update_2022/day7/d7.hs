{-# LANGUAGE ViewPatterns #-}
import Data.List ( stripPrefix )
import Data.Tree ( Tree(..), flatten )
import Data.Tuple ( swap )
import Data.Bifunctor (bimap, first, second)

data File = F { fName :: String, fSize :: Int } deriving (Show)
data Dir = D { dName :: String, dSize :: Int, files :: [File] } deriving (Show)

addFileToDir :: Dir -> File -> Dir
addFileToDir d f = D (dName d) (fSize f + dSize d) (f:files d)

addFileToTree :: Tree Dir -> File -> Tree Dir
addFileToTree t f = Node (addFileToDir (rootLabel t) f) (subForest t)

addDirToTree :: Tree Dir -> Tree Dir -> Tree Dir
addDirToTree t st = Node (rootLabel t) (st:subForest t)

newTree :: String -> Tree Dir
newTree name = Node (D name 0 []) []

updateDirSize :: Dir -> Int -> Dir
updateDirSize d s = D (dName d) (s + dSize d) (files d)

updateTreeSize :: Tree Dir -> Int -> Tree Dir
updateTreeSize t s = Node (updateDirSize (rootLabel t) s) (subForest t)

readInt :: String -> Int
readInt = read

readDir :: [String] -> Tree Dir -> ([String], Tree Dir)
readDir [] t = ([], t)
readDir (('$':cmd):cmds) t = doCommand (('$':cmd):cmds) t
readDir ((stripPrefix "dir " -> Just name):cmds) t = readDir cmds $ addDirToTree t $ newTree name
readDir (fileCmd:cmds) t =
    readDir cmds $ addFileToTree t $ uncurry F $ swap $ bimap readInt tail $ span (\x -> x /= ' ') fileCmd

enterDir :: String -> [String] -> Tree Dir -> ([String], Tree Dir)
enterDir dirName cmds t = uncurry doCommand
    $
    (
        \tList -> second (
            (\f -> (Node (updateDirSize (rootLabel t) $ (dSize . rootLabel . head) f) f))
            . flip (:) (tail tList)
        )
        $ (doCommand cmds . head) tList
    )
    $ uncurry (++) (swap $ span (\x -> dirName /= (dName . rootLabel) x) (subForest t))

doCommand :: [String] -> Tree Dir -> ([String], Tree Dir)
doCommand [] t = ([], t)
doCommand ("$ ls":cmds) t = readDir cmds t
doCommand ("$ cd ..":cmds) t = (cmds, t)
doCommand ((stripPrefix "$ cd " -> Just dirName):cmds) t = enterDir dirName cmds t

part1 :: Tree Dir -> Int
part1 = sum . filter (\x -> x <= 100000) . map dSize . flatten

part2 :: Tree Dir -> Int
part2 t =
    let total = (dSize . rootLabel) t
    in (minimum . filter (\x -> x >= 30000000 - (70000000 - total)) . map dSize . flatten) t

main :: IO ()
main = do
    contents <- readFile "./input_michael"
    let lineList = lines contents
    let root = Node (D "/" 0 []) []
    let (_, fileTree) = doCommand (tail lineList) root
    print "part 1"
    print $ part1 fileTree
    print "part 2"
    print $ part2 fileTree