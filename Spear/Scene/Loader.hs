{-# LANGUAGE FlexibleContexts #-}

module Spear.Scene.Loader
  ( SceneResources (..),
    loadScene,
    validate,
    resourceMap,
    value,
    unspecified,
    mandatory,
    asString,
    asFloat,
    asVec3,
    asVec4,
  )
where

import           Control.Monad.State.Strict
import           Control.Monad.Trans        (lift)
import           Data.List                  as L (find)
import           Data.Map                   as M
import qualified Data.StateVar              as SV (get)
import           Prelude                    hiding ((*))
import           Spear.Assets.Model         as Model
import           Spear.Game
import qualified Spear.GL                   as GL
import           Spear.Math.Algebra
import           Spear.Math.Collision
import           Spear.Math.Matrix3         as M3
import           Spear.Math.Matrix4         as M4
import           Spear.Math.MatrixUtils     (fastNormalMatrix)
import           Spear.Math.Vector
import           Spear.Render.AnimatedModel as AM
import           Spear.Render.Material
import           Spear.Render.Program
import           Spear.Render.StaticModel   as SM
import           Spear.Scene.Graph
import           Spear.Scene.SceneResources
import           Text.Printf                (printf)

type Loader = Game SceneResources

-- | Load the scene specified by the given file.
loadScene :: FilePath -> Game s (SceneResources, SceneGraph)
loadScene file = do
  result <- gameIO $ loadSceneGraphFromFile file
  case result of
    Left err -> gameError $ show err
    Right g -> case validate g of
      Nothing -> do
        sceneRes <- resourceMap g
        return (sceneRes, g)
      Just err -> gameError err

-- | Validate the given SceneGraph.
validate :: SceneGraph -> Maybe String
validate _ = Nothing

-- | Load the scene described by the given 'SceneGraph'.
resourceMap :: SceneGraph -> Game s SceneResources
resourceMap g = execSubGame (resourceMap' g) emptySceneResources

resourceMap' :: SceneGraph -> Loader ()
resourceMap' node@(SceneLeaf nid props) = do
  case nid of
    "shader-program" -> newShaderProgram node
    "model"          -> newModel node
    x                -> return ()
resourceMap' node@(SceneNode nid props children) = do
  mapM_ resourceMap' children

-- Lookup the given resource in the data pool. Load it if it is not present, otherwise return it.
loadResource ::
  -- | Resource name.
  String ->
  -- | Map getter.
  (SceneResources -> Map String a) ->
  -- | Function to modify resources.
  (String -> a -> Loader ()) ->
  -- | Resource loader.
  Loader a ->
  Loader a
loadResource key field modifyResources load = do
  sceneData <- get
  case M.lookup key $ field sceneData of
    Just val -> return val
    Nothing -> do
      gameIO $ printf "Loading %s..." key
      resource <- load
      gameIO $ printf "done\n"
      modifyResources key resource
      return resource

addShader name shader = modify $ \sceneData ->
  sceneData {shaders = M.insert name shader $ shaders sceneData}

addCustomProgram name prog = modify $ \sceneData ->
  sceneData {customPrograms = M.insert name prog $ customPrograms sceneData}

addStaticProgram name prog = modify $ \sceneData ->
  sceneData {staticPrograms = M.insert name prog $ staticPrograms sceneData}

addAnimatedProgram name prog = modify $ \sceneData ->
  sceneData {animatedPrograms = M.insert name prog $ animatedPrograms sceneData}

addTexture name tex = modify $ \sceneData ->
  sceneData {textures = M.insert name tex $ textures sceneData}

addStaticModel name model = modify $
  \sceneData -> sceneData {staticModels = M.insert name model $ staticModels sceneData}

addAnimatedModel name model = modify $
  \sceneData -> sceneData {animatedModels = M.insert name model $ animatedModels sceneData}

-- Get the given resource from the data pool.
getResource :: (SceneResources -> Map String a) -> String -> Loader a
getResource field key = do
  sceneData <- get
  case M.lookup key $ field sceneData of
    Just val -> return val
    Nothing -> gameError $ "Oops, the given resource has not been loaded: " ++ key

----------------------
-- Resource Loading --
----------------------

newModel :: SceneGraph -> Loader ()
newModel (SceneLeaf _ props) = do
  name <- asString $ mandatory' "name" props
  file <- asString $ mandatory' "file" props
  tex <- asString $ mandatory' "texture" props
  prog <- asString $ mandatory' "shader-program" props
  ke <- asVec4 $ mandatory' "ke" props
  ka <- asVec4 $ mandatory' "ka" props
  kd <- asVec4 $ mandatory' "kd" props
  ks <- asVec4 $ mandatory' "ks" props
  shi <- asFloat $ mandatory' "shi" props

  let rotation = asRotation $ value "rotation" props
      scale = asVec3 $ value "scale" props

  gameIO $ printf "Loading model %s..." name
  model <- loadModel' file rotation scale
  gameIO . putStrLn $ "done"
  texture <- loadTexture tex
  sceneRes <- get

  let material = Material ke ka kd ks shi

  case animated model of
    False ->
      case M.lookup prog $ staticPrograms sceneRes of
        Nothing -> (gameError $ "Static shader program " ++ prog ++ " does not exist") >> return ()
        Just p ->
          let StaticProgram _ channels _ = p
           in do
                model' <- staticModelResource channels material texture model
                loadResource name staticModels addStaticModel (return model')
                return ()
    True ->
      case M.lookup prog $ animatedPrograms sceneRes of
        Nothing -> (gameError $ "Animated shader program " ++ prog ++ " does not exist") >> return ()
        Just p ->
          let AnimatedProgram _ channels _ = p
           in do
                model' <- animatedModelResource channels material texture model
                loadResource name animatedModels addAnimatedModel (return model')
                return ()

loadModel' :: FilePath -> Maybe Rotation -> Maybe Vector3 -> Game s Model
loadModel' file rotation scale = do
  let transform =
        ( case rotation of
            Nothing  -> Prelude.id
            Just rot -> rotateModel rot
        )
          . ( case scale of
                Nothing -> Prelude.id
                Just s -> flip Model.transformVerts $
                  \(Vec3 x' y' z') -> Vec3 (x s * x') (y s * y') (z s * z')
            )

  (fmap transform $ Model.loadModel file) >>= gameIO . toGround

rotateModel :: Rotation -> Model -> Model
rotateModel (Rotation ax ay az order) model =
  let mat = case order of
        XYZ -> rotZ az * rotY ay * rotX ax
        XZY -> rotY ay * rotZ az * rotX ax
        YXZ -> rotZ az * rotX ax * rotY ay
        YZX -> rotX ax * rotZ az * rotY ay
        ZXY -> rotY ay * rotX ax * rotZ az
        ZYX -> rotX ax * rotY ay * rotZ az
      normalMat = fastNormalMatrix mat

      vTransform (Vec3 x' y' z') =
        let v = mat `M4.mulp` (vec3 x' y' z') in Vec3 (x v) (y v) (z v)

      nTransform (Vec3 x' y' z') =
        let v = normalMat `M3.mul` (vec3 x' y' z') in Vec3 (x v) (y v) (z v)
   in flip Model.transformVerts vTransform . flip Model.transformNormals nTransform $ model

loadTexture :: FilePath -> Loader GL.Texture
loadTexture file =
  loadResource file textures addTexture $
    GL.loadTextureImage file GL.GL_LINEAR GL.GL_LINEAR

newShaderProgram :: SceneGraph -> Loader ()
newShaderProgram (SceneLeaf _ props) = do
  (vsName, vertShader) <- Spear.Scene.Loader.loadShader GL.VertexShader props
  (fsName, fragShader) <- Spear.Scene.Loader.loadShader GL.FragmentShader props
  name <- asString $ mandatory' "name" props
  stype <- asString $ mandatory' "type" props
  prog <- GL.newProgram [vertShader, fragShader]

  let getUniformLoc name = (gameIO . SV.get $ GL.uniformLocation prog name) `GL.assertGL` name

  case stype of
    "static" -> do
      ambient <- asString $ mandatory' "ambient" props
      diffuse <- asString $ mandatory' "diffuse" props
      specular <- asString $ mandatory' "specular" props
      shininess <- asString $ mandatory' "shininess" props
      texture <- asString $ mandatory' "texture" props
      modelview <- asString $ mandatory' "modelview" props
      normalmat <- asString $ mandatory' "normalmat" props
      projection <- asString $ mandatory' "projection" props

      ka <- getUniformLoc ambient
      kd <- getUniformLoc diffuse
      ks <- getUniformLoc specular
      shi <- getUniformLoc shininess
      tex <- getUniformLoc texture
      mview <- getUniformLoc modelview
      nmat <- getUniformLoc normalmat
      proj <- getUniformLoc projection

      vertChan <- fmap read $ asString $ mandatory' "vertex-channel" props
      normChan <- fmap read $ asString $ mandatory' "normal-channel" props
      texChan <- fmap read $ asString $ mandatory' "texture-channel" props

      let channels = StaticProgramChannels vertChan normChan texChan
          uniforms = StaticProgramUniforms ka kd ks shi tex mview nmat proj

      loadResource name staticPrograms addStaticProgram $
        return $ StaticProgram prog channels uniforms
      return ()
    "animated" -> do
      ambient <- asString $ mandatory' "ambient" props
      diffuse <- asString $ mandatory' "diffuse" props
      specular <- asString $ mandatory' "specular" props
      shininess <- asString $ mandatory' "shininess" props
      texture <- asString $ mandatory' "texture" props
      modelview <- asString $ mandatory' "modelview" props
      normalmat <- asString $ mandatory' "normalmat" props
      projection <- asString $ mandatory' "projection" props

      ka <- getUniformLoc ambient
      kd <- getUniformLoc diffuse
      ks <- getUniformLoc specular
      shi <- getUniformLoc shininess
      tex <- getUniformLoc texture
      mview <- getUniformLoc modelview
      nmat <- getUniformLoc normalmat
      proj <- getUniformLoc projection

      vertChan1 <- fmap read $ asString $ mandatory' "vertex-channel1" props
      vertChan2 <- fmap read $ asString $ mandatory' "vertex-channel2" props
      normChan1 <- fmap read $ asString $ mandatory' "normal-channel1" props
      normChan2 <- fmap read $ asString $ mandatory' "normal-channel2" props
      texChan <- fmap read $ asString $ mandatory' "texture-channel" props
      fp <- asString $ mandatory' "fp" props
      p <- getUniformLoc fp

      let channels = AnimatedProgramChannels vertChan1 vertChan2 normChan1 normChan2 texChan
          uniforms = AnimatedProgramUniforms ka kd ks shi tex p mview nmat proj

      loadResource name animatedPrograms addAnimatedProgram $
        return $ AnimatedProgram prog channels uniforms
      return ()
    _ -> do
      loadResource name customPrograms addCustomProgram $ return prog
      return ()

loadShader :: GL.ShaderType -> [Property] -> Loader (String, GL.GLSLShader)
loadShader _ [] = gameError $ "Loader::vertexShader: empty list"
loadShader shaderType ((stype, file) : xs) =
  if shaderType == GL.VertexShader && stype == "vertex-shader"
    || shaderType == GL.FragmentShader && stype == "fragment-shader"
    then
      let f = concat file
       in loadShader' f shaderType >>= \shader -> return (f, shader)
    else Spear.Scene.Loader.loadShader shaderType xs

loadShader' :: String -> GL.ShaderType -> Loader GL.GLSLShader
loadShader' file shaderType = loadResource file shaders addShader $ GL.loadShader shaderType file

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
unspecified Nothing x  = x

mandatory :: String -> [Property] -> Game s [String]
mandatory name props = case value name props of
  Nothing -> gameError $ "Loader::mandatory: key not found: " ++ name
  Just x  -> return x

mandatory' :: String -> [Property] -> Loader [String]
mandatory' name props = mandatory name props

asString :: Functor f => f [String] -> f String
asString = fmap concat

asFloat :: Functor f => f [String] -> f Float
asFloat = fmap (read . concat)

asVec2 :: Functor f => f [String] -> f Vector2
asVec2 val = fmap toVec2 val
  where
    toVec2 (x : y : _) = vec2 (read x) (read y)
    toVec2 (x : [])    = let x' = read x in vec2 x' x'

asVec3 :: Functor f => f [String] -> f Vector3
asVec3 val = fmap toVec3 val
  where
    toVec3 (x : y : z : _) = vec3 (read x) (read y) (read z)
    toVec3 (x : [])        = let x' = read x in vec3 x' x' x'

asVec4 :: Functor f => f [String] -> f Vector4
asVec4 val = fmap toVec4 val
  where
    toVec4 (x : y : z : w : _) = vec4 (read x) (read y) (read z) (read w)
    toVec4 (x : [])            = let x' = read x in vec4 x' x' x' x'

asRotation :: Functor f => f [String] -> f Rotation
asRotation val = fmap parseRotation val
  where
    parseRotation (ax : ay : az : order : _) = Rotation (read ax) (read ay) (read az) (readOrder order)

data Rotation = Rotation
  { ax    :: Float,
    ay    :: Float,
    az    :: Float,
    order :: RotationOrder
  }

data RotationOrder = XYZ | XZY | YXZ | YZX | ZXY | ZYX deriving (Eq)

readOrder :: String -> RotationOrder
readOrder "xyz" = XYZ
readOrder "xzy" = XZY
readOrder "yxz" = YXZ
readOrder "yzx" = YZX
readOrder "zxy" = ZXY
readOrder "zyx" = ZYX
