module Spear.Scene.Graph
(
    Property
,   SceneGraph(..)
,   ParseError
,   loadSceneGraph
,   loadSceneGraphFromFile
,   node
)
where


import qualified Data.ByteString.Char8 as B
import Data.List (find, intersperse)
import Data.Maybe (isJust)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Prim
import qualified Text.Parsec.ByteString as P
import qualified Text.Parsec.Token as PT


type Property = (String, [String])


data SceneGraph
    = SceneNode
    { nodeID     :: String
    , properties :: [Property]
    , children   :: [SceneGraph]
    }
    | SceneLeaf
    { nodeID     :: String
    , properties :: [Property]
    }


instance Show SceneGraph where
    show sceneGraph = show' "" sceneGraph
        where
            show' tab (SceneNode nid props children) =
                tab ++ nid ++ "\n" ++ tab ++ "{\n" ++ (printProps tab props) ++
                (concat . fmap (show' $ "   " ++ tab) $ children) ++ '\n':tab ++ "}\n"
            
            show' tab (SceneLeaf nid props) =
                tab ++ nid ++ '\n':tab ++ "{\n" ++ tab ++ (printProps tab props) ++ '\n':tab ++ "}\n"


printProp :: Property -> String
printProp (key, vals) = key ++ " = " ++ (concat $ intersperse ", " vals)


printProps :: String -> [Property] -> String
printProps tab props =
    let
        tab' = '\n':(tab ++ tab)
        longestKeyLen = maximum . fmap (length . fst) $ props
        
        align :: Int -> String -> String
        align len str =
            let (key, vals) = break ((==) '=') str
                thisLen     = length key
                padLen      = len - thisLen + 1
                pad         = replicate padLen ' '
            in
                key ++ pad ++ vals
    in
        case concat . intersperse tab' . fmap (align longestKeyLen . printProp) $ props of
            [] -> []
            xs -> tab ++ xs


-- | Load the scene graph from the given string.
loadSceneGraph :: String -> Either ParseError SceneGraph
loadSceneGraph str = parse sceneGraph "(unknown)" $ B.pack str


-- | Load the scene graph specified by the given file.
loadSceneGraphFromFile :: FilePath -> IO (Either ParseError SceneGraph)
loadSceneGraphFromFile = P.parseFromFile sceneGraph


-- | Get the node identified by the given string from the given scene graph.
node :: String -> SceneGraph -> Maybe SceneGraph
node str SceneLeaf {} = Nothing
node str n@(SceneNode nid _ children)
    | str == nid = Just n
    | otherwise  = case find isJust $ fmap (node str) children of
        Nothing -> Nothing
        Just x  -> x


sceneGraph :: P.Parser SceneGraph
sceneGraph = do
    g <- graph
    whitespace
    eof
    return g


graph :: P.Parser SceneGraph
graph = do
    nid <- name
    whitespace
    char '{'
    props    <- many . try $ whitespace >> property
    children <- many . try $ whitespace >> graph
    whitespace
    char '}'
    
    return $ case null children of
        True  -> SceneLeaf nid props
        False -> SceneNode nid props children


property :: P.Parser Property
property = do
    key <- name
    spaces >> char '=' >> spaces
    vals <- cells name
    return (key, vals)


cells :: P.Parser String -> P.Parser [String]
cells p = do
    val  <- p
    vals <- remainingCells p
    return $ val:vals


remainingCells :: P.Parser String -> P.Parser [String]
remainingCells p =
    try (whitespace >> char ',' >> whitespace >> cells p)
    <|> (return [])


name :: P.Parser String
name = many1 $ choice [oneOf "-/.()?_", alphaNum]


whitespace :: P.Parser ()
whitespace = skipMany $ choice [space, newline]
