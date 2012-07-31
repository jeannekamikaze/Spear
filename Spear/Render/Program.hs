module Spear.Render.Program
(
    StaticProgram(..)
,   AnimatedProgram(..)
,   Program(..)
,   ProgramUniforms(..)
,   StaticProgramChannels(..)
,   StaticProgramUniforms(..)
,   AnimatedProgramChannels(..)
,   AnimatedProgramUniforms(..)
)
where


import Spear.GLSL.Management (GLSLProgram)


import Graphics.Rendering.OpenGL.Raw.Core31


data StaticProgram = StaticProgram
    { staticProgram         :: GLSLProgram
    , staticProgramChannels :: StaticProgramChannels
    , staticProgramUniforms :: StaticProgramUniforms
    }


data AnimatedProgram = AnimatedProgram
    { animatedProgram         :: GLSLProgram
    , animatedProgramChannels :: AnimatedProgramChannels
    , animatedProgramUniforms :: AnimatedProgramUniforms
    }


data StaticProgramChannels = StaticProgramChannels
    { vertexChannel  :: GLuint -- ^ Vertex channel.
    , normalChannel  :: GLuint -- ^ Normal channel.
    , stexChannel    :: GLuint -- ^ Texture channel.
    }


data AnimatedProgramChannels = AnimatedProgramChannels
    { vertexChannel1 :: GLuint -- ^ Vertex channel 1.
    , vertexChannel2 :: GLuint -- ^ Vertex channel 2.
    , normalChannel1 :: GLuint -- ^ Normal channel 1.
    , normalChannel2 :: GLuint -- ^ Normal channel 2.
    , atexChannel    :: GLuint -- ^ Texture channel.
    }


data StaticProgramUniforms = StaticProgramUniforms
    { skaLoc         :: GLint -- ^ Material ambient uniform location.
    , skdLoc         :: GLint -- ^ Material diffuse uniform location.
    , sksLoc         :: GLint -- ^ Material specular uniform location.
    , sshiLoc        :: GLint -- ^ Material shininess uniform location.
    , stexLoc        :: GLint -- ^ Texture sampler location.
    , smodelviewLoc  :: GLint -- ^ Modelview matrix location.
    , snormalmatLoc  :: GLint -- ^ Normal matrix location.
    , sprojLoc       :: GLint -- ^ Projection matrix location.
    }


data AnimatedProgramUniforms = AnimatedProgramUniforms
    { akaLoc         :: GLint -- ^ Material ambient uniform location.
    , akdLoc         :: GLint -- ^ Material diffuse uniform location.
    , aksLoc         :: GLint -- ^ Material specular uniform location.
    , ashiLoc        :: GLint -- ^ Material shininess uniform location.
    , atexLoc        :: GLint -- ^ Texture sampler location.
    , fpLoc          :: GLint -- ^ Frame progress uniform location.
    , amodelviewLoc  :: GLint -- ^ Modelview matrix location.
    , anormalmatLoc  :: GLint -- ^ Normal matrix location.
    , aprojLoc       :: GLint -- ^ Projection matrix location.
    }


class Program a where
    program :: a -> GLSLProgram


instance Program StaticProgram where
    program = staticProgram


instance Program AnimatedProgram where
    program = animatedProgram


class ProgramUniforms a where
    kaLoc        :: a -> GLint
    kdLoc        :: a -> GLint
    ksLoc        :: a -> GLint
    shiLoc       :: a -> GLint
    texLoc       :: a -> GLint
    modelviewLoc :: a -> GLint
    normalmatLoc :: a -> GLint
    projLoc      :: a -> GLint


instance ProgramUniforms StaticProgramUniforms where
    kaLoc        = skaLoc
    kdLoc        = skdLoc
    ksLoc        = sksLoc
    shiLoc       = sshiLoc
    texLoc       = stexLoc
    modelviewLoc = smodelviewLoc
    normalmatLoc = snormalmatLoc
    projLoc      = sprojLoc
    


instance ProgramUniforms AnimatedProgramUniforms where
    kaLoc        = akaLoc
    kdLoc        = akdLoc
    ksLoc        = aksLoc
    shiLoc       = ashiLoc
    texLoc       = atexLoc
    modelviewLoc = amodelviewLoc
    normalmatLoc = anormalmatLoc
    projLoc      = aprojLoc
