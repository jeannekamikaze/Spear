module Spear.Render.Light
where


import Spear.Vector
import Graphics.Rendering.OpenGL.Raw


data LightData = LightData {
	ambient	:: Vector Float,
	diffuse	:: Vector Float,
	spec	:: Vector Float
}


data Light =
	ParallelLight {
		ld :: LightData
	}
|	PointLight {
		ld :: LightData
	}
|	SpotLight {
		ld :: LightData
	}
