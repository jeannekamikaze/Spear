module Spear.Scene.Loader
(
    SceneResources(..)
,   CreateStaticObject
,   CreateAnimatedObject
,   loadScene
,   validate
,   resourceMap
,   loadObjects
)
where


import Spear.Assets.Model as Model
import qualified Spear.GLSL as GLSL
import Spear.Math.Matrix4 as M4
import Spear.Math.Vector3 as V3
import Spear.Math.Vector4
import Spear.Render.AnimatedModel
import Spear.Render.Material
import Spear.Render.Program
import Spear.Render.StaticModel
import Spear.Render.Texture
import Spear.Scene.Light
import Spear.Scene.Graph
import Spear.Scene.SceneResources
import Spear.Setup

import Control.Monad.State.Strict
import Control.Monad.Trans (lift)
import Data.List as L (find)
import Data.Map as M
import qualified Data.StateVar as SV (get)
import Graphics.Rendering.OpenGL.Raw.Core31
import Text.Printf (printf)


type Loader = StateT SceneResources Setup


loaderSetup = lift
loaderIO    = loaderSetup . setupIO
loaderError = loaderSetup . setupError


type CreateStaticObject   a = String -> Matrix4 -> StaticModelResource   -> a
type CreateAnimatedObject a = String -> Matrix4 -> AnimatedModelResource -> a


-- | Load the scene specified by the given file.
loadScene :: FilePath -> Setup (SceneResources, SceneGraph)
loadScene file = do
    result <- setupIO $ loadSceneGraphFromFile file
    case result of
        Left err -> setupError $ show err
        Right g  -> case validate g of
            Nothing  -> do
                sceneRes <- resourceMap g
                return (sceneRes, g)
            Just err -> setupError err


-- | Validate the given SceneGraph.
validate :: SceneGraph -> Maybe String
validate _ = Nothing


-- | Load the scene described by the given 'SceneGraph'.
resourceMap :: SceneGraph -> Setup SceneResources
resourceMap g = execStateT (resourceMap' g) emptySceneResources


resourceMap' :: SceneGraph -> Loader ()
resourceMap' node@(SceneLeaf nid props) = do
    case nid of
        "shader-program" -> newShaderProgram node
        "model"          -> newModel         node
        "light"          -> newLight         node
        x                -> return ()

resourceMap' node@(SceneNode nid props children) = do
    mapM_ resourceMap' children


-- Lookup the given resource in the data pool. Load it if it is not present, otherwise return it.
loadResource :: String -- ^ Resource name.
             -> (SceneResources -> Map String a) -- ^ Map getter.
             -> (String -> a -> Loader ()) -- ^ Function to modify resources.
             -> Setup a -- ^ Resource loader.
             -> Loader a
loadResource key field modifyResources load = do
    sceneData <- get
    case M.lookup key $ field sceneData of
        Just val -> return val
        Nothing  -> do
            loaderIO $ printf "Loading %s..." key
            resource <- loaderSetup load
            loaderIO $ printf "done\n"
            modifyResources key resource
            return resource


addShader name shader =
    modify $ \sceneData -> sceneData { shaders = M.insert name shader $ shaders sceneData }


addStaticProgram name prog =
    modify $ \sceneData -> sceneData { staticPrograms = M.insert name prog $ staticPrograms sceneData }


addAnimatedProgram name prog =
    modify $ \sceneData -> sceneData { animatedPrograms = M.insert name prog $ animatedPrograms sceneData }


addTexture name tex =
    modify $ \sceneData -> sceneData { textures = M.insert name tex $ textures sceneData }


addStaticModel name model =
    modify $ \sceneData -> sceneData { staticModels = M.insert name model $ staticModels sceneData }


addAnimatedModel name model =
    modify $ \sceneData -> sceneData { animatedModels = M.insert name model $ animatedModels sceneData }


-- Get the given resource from the data pool.
getResource :: (SceneResources -> Map String a) -> String -> Loader a
getResource field key = do
    sceneData <- get
    case M.lookup key $ field sceneData of
        Just val -> return val
        Nothing  -> loaderSetup . setupError $ "Oops, the given resource has not been loaded: " ++ key




----------------------
-- Resource Loading --
----------------------

newModel :: SceneGraph -> Loader ()
newModel (SceneLeaf _ props) = do
    name <- asString $ mandatory "name"           props
    file <- asString $ mandatory "file"           props
    tex  <- asString $ mandatory "texture"        props
    prog <- asString $ mandatory "shader-program" props
    ke   <- asVec4   $ mandatory "ke"             props
    ka   <- asVec4   $ mandatory "ka"             props
    kd   <- asVec4   $ mandatory "kd"             props
    ks   <- asVec4   $ mandatory "ks"             props
    shi  <- asFloat  $ mandatory "shi"            props
    
    let rotation = asRotation $ value "rotation" props
        scale    = asVec3 $ value "scale" props
    
    loaderIO $ printf "Loading model %s..." name
    model    <- loaderSetup $ loadModel' file rotation scale
    loaderIO . putStrLn $ "done"
    texture  <- loadTexture  tex
    sceneRes <- get
    
    let material = Material ke ka kd ks shi
    
    case animated model of
        False ->
            case M.lookup prog $ staticPrograms sceneRes of
                Nothing -> (loaderError $ "Static shader program " ++ prog ++ " does not exist") >> return ()
                Just p  ->
                    let StaticProgram _ channels _ = p
                    in do
                        model' <- loaderSetup $ staticModelResource channels material texture model
                        loadResource name staticModels addStaticModel (return model')
                        return ()
        True ->
            case M.lookup prog $ animatedPrograms sceneRes of
                Nothing -> (loaderError $ "Animated shader program " ++ prog ++ " does not exist") >> return ()
                Just p  ->
                    let AnimatedProgram _ channels _ = p
                    in do
                        model' <- loaderSetup $ animatedModelResource channels material texture model
                        loadResource name animatedModels addAnimatedModel (return model')
                        return ()


loadModel' :: FilePath -> Maybe Rotation -> Maybe Vector3 -> Setup Model
loadModel' file rotation scale = do
    model <- Model.loadModel file
    case rotation of
        Just rot -> setupIO $ rotateModel model rot
        Nothing  -> return ()
    case scale of
        Just s  -> setupIO $ Model.transform (scalev s) model
        Nothing -> return ()
    return model


rotateModel :: Model -> Rotation -> IO ()
rotateModel model (Rotation x y z order) = case order of
    XYZ -> Model.transform (rotZ z * rotY y * rotX x) model
    XZY -> Model.transform (rotY y * rotZ z * rotX x) model
    YXZ -> Model.transform (rotZ z * rotX x * rotY y) model
    YZX -> Model.transform (rotX x * rotZ z * rotY y) model
    ZXY -> Model.transform (rotY y * rotX x * rotZ z) model
    ZYX -> Model.transform (rotX x * rotY y * rotZ z) model


loadTexture :: FilePath -> Loader GLSL.Texture
loadTexture file = loadResource file textures addTexture $ loadTextureImage file gl_LINEAR gl_LINEAR


newShaderProgram :: SceneGraph -> Loader ()
newShaderProgram (SceneLeaf _ props) = do
    (vsName, vertShader) <- Spear.Scene.Loader.loadShader GLSL.VertexShader props
    (fsName, fragShader) <- Spear.Scene.Loader.loadShader GLSL.FragmentShader props
    name       <- asString $ mandatory "name" props
    stype      <- asString $ mandatory "type" props
    texChan    <- fmap read $ asString $ mandatory "texture-channel" props
    ambient    <- asString $ mandatory "ambient"    props
    diffuse    <- asString $ mandatory "diffuse"    props
    specular   <- asString $ mandatory "specular"   props
    shininess  <- asString $ mandatory "shininess"  props
    texture    <- asString $ mandatory "texture"    props
    modelview  <- asString $ mandatory "modelview"  props
    normalmat  <- asString $ mandatory "normalmat"  props
    projection <- asString $ mandatory "projection" props
    prog       <- loaderSetup $ GLSL.newProgram [vertShader, fragShader]
    
    let getUniformLoc name =
            loaderSetup $ (setupIO . SV.get $ GLSL.uniformLocation prog name) `GLSL.assertGL` name
    
    ka    <- getUniformLoc ambient
    kd    <- getUniformLoc diffuse
    ks    <- getUniformLoc specular
    shi   <- getUniformLoc shininess
    tex   <- getUniformLoc texture
    mview <- getUniformLoc modelview
    nmat  <- getUniformLoc normalmat
    proj  <- getUniformLoc projection
    
    case stype of
        "static" -> do
            vertChan  <- fmap read $ asString $ mandatory "vertex-channel" props
            normChan  <- fmap read $ asString $ mandatory "normal-channel" props
            
            let channels = StaticProgramChannels vertChan normChan texChan
                uniforms = StaticProgramUniforms ka kd ks shi tex mview nmat proj
            
            loadResource name staticPrograms addStaticProgram $
                return $ StaticProgram prog channels uniforms
            return ()
        
        "animated" -> do
            vertChan1  <- fmap read $ asString $ mandatory "vertex-channel1" props
            vertChan2  <- fmap read $ asString $ mandatory "vertex-channel2" props
            normChan1  <- fmap read $ asString $ mandatory "normal-channel1" props
            normChan2  <- fmap read $ asString $ mandatory "normal-channel2" props
            fp <- asString $ mandatory "fp" props
            p  <- getUniformLoc fp
            
            let channels = AnimatedProgramChannels vertChan1 vertChan2 normChan1 normChan2 texChan
                uniforms = AnimatedProgramUniforms ka kd ks shi tex p mview nmat proj
            
            loadResource name animatedPrograms addAnimatedProgram $
                return $ AnimatedProgram prog channels uniforms
            return ()


loadShader :: GLSL.ShaderType -> [Property] -> Loader (String, GLSL.GLSLShader)
loadShader _ [] = loaderSetup . setupError $ "Loader::vertexShader: empty list"
loadShader shaderType ((stype, file):xs) =
    if shaderType == GLSL.VertexShader   && stype == "vertex-shader" ||
       shaderType == GLSL.FragmentShader && stype == "fragment-shader"
    then let f = concat file
         in loadShader' f shaderType >>= \shader -> return (f, shader)
    else Spear.Scene.Loader.loadShader shaderType xs


loadShader' :: String -> GLSL.ShaderType -> Loader GLSL.GLSLShader
loadShader' file shaderType = loadResource file shaders addShader $ GLSL.loadShader file shaderType


newLight :: SceneGraph -> Loader ()
newLight _ = return ()




--------------------
-- Object Loading --
--------------------


-- | Load objects from the given 'SceneGraph'.
loadObjects :: CreateStaticObject a -> CreateAnimatedObject a -> SceneResources -> SceneGraph -> Setup [a]
loadObjects newSO newAO sceneRes g =
    case node "layout" g of
        Nothing -> return []
        Just n  -> do
            let gos = concat . fmap (newObject newSO newAO sceneRes) $ children n
            forM gos $ \go -> case go of
                Left err -> setupError err
                Right go -> return go


-- to-do: use a strict accumulator and make loadObjects tail recursive.
newObject :: CreateStaticObject a -> CreateAnimatedObject a -> SceneResources -> SceneGraph -> [Either String a]
newObject newSO newAO sceneRes (SceneNode nid props children) =
    let o = newObject' newSO newAO sceneRes nid props
    in o : (concat $ fmap (newObject newSO newAO sceneRes) children)

newObject newSO newAO sceneRes (SceneLeaf nid props) = [newObject' newSO newAO sceneRes nid props]


newObject' :: CreateStaticObject a -> CreateAnimatedObject a -> SceneResources
           -> String -> [Property] -> Either String a
newObject' newSO newAO sceneRes nid props = do
    -- Optional properties.
    let name     = (asString $ value "name"     props) `unspecified` "unknown"
        model    = (asString $ value "model"    props) `unspecified` "ghost"
        position = (asVec3   $ value "position" props) `unspecified` vec3 0 0 0
        rotation = (asVec3   $ value "rotation" props) `unspecified` vec3 0 0 0
        right'   = (asVec3   $ value "right"    props) `unspecified` vec3 1 0 0
        up'      = (asVec3   $ value "up"       props) `unspecified` vec3 0 1 0
        forward' =  asVec3   $ value "forward"  props
        scale    = (asVec3   $ value "scale"    props) `unspecified` vec3 1 1 1
    
    -- Compute the object's vectors if a forward vector has been specified.
    let (right, up, forward) = vectors forward'
    
    case M.lookup model $ staticModels sceneRes of
        Just m  -> Right $ newSO name (M4.transform right up forward position) m
        Nothing -> case M.lookup model $ animatedModels sceneRes of
            Just m  -> Right $ newAO name (M4.transform right up forward position) m
            Nothing -> Left $ "Loader::newObject: model " ++ model ++ " has not been loaded."


vectors :: Maybe Vector3 -> (Vector3, Vector3, Vector3)
vectors forward = case forward of
    Nothing -> (V3.unitX, V3.unitY, V3.unitZ)
    Just f  ->
        let r = f `cross` V3.unitY
            u = r `cross` f
        in
            (r, u, f)




----------------------
-- Helper functions --
----------------------

-- Get the value of the given key.
value :: String -> [Property] -> Maybe [String]
value name props = case L.find ((==) name . fst) props of
    Nothing   -> Nothing
    Just prop -> Just . snd $ prop


unspecified :: Maybe a -> a -> a
unspecified (Just x) _ = x
unspecified Nothing  x = x


mandatory :: String -> [Property] -> Loader [String]
mandatory name props = case value name props of
    Nothing -> loaderError $ "Loader::mandatory: key not found: " ++ name
    Just x  -> return x


asString :: Functor f => f [String] -> f String
asString = fmap concat


asFloat :: Functor f => f [String] -> f Float
asFloat = fmap (read . concat)


asVec4 :: Functor f => f [String] -> f Vector4
asVec4 val = fmap toVec4 val
    where toVec4 (x:y:z:w:_) = vec4 (read x) (read y) (read z) (read w)
          toVec4 (x:[])      = let x' = read x in vec4 x' x' x' x'


asVec3 :: Functor f => f [String] -> f Vector3
asVec3 val = fmap toVec3 val
    where toVec3 (x:y:z:_) = vec3 (read x) (read y) (read z)
          toVec3 (x:[])    = let x' = read x in vec3 x' x' x'


asRotation :: Functor f => f [String] -> f Rotation
asRotation val = fmap parseRotation val
    where parseRotation (ax:ay:az:order:_) = Rotation (read ax) (read ay) (read az) (readOrder order)


data Rotation = Rotation
    { ax    :: Float
    , ay    :: Float
    , az    :: Float
    , order :: RotationOrder
    }


data RotationOrder = XYZ | XZY | YXZ | YZX | ZXY | ZYX deriving Eq


readOrder :: String -> RotationOrder
readOrder "xyz" = XYZ
readOrder "xzy" = XZY
readOrder "yxz" = YXZ
readOrder "yzx" = YZX
readOrder "zxy" = ZXY
readOrder "zyx" = ZYX
