module Spear.Render.Material
( Material(..)
)
where


import Spear.Math.Vector4


data Material = Material
    { ke :: Vector4
    , ka :: Vector4
    , kd :: Vector4
    , ks :: Vector4
    , shininess :: Float
    }
