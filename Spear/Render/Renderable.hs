module Spear.Render.Renderable
where


class Renderable a where
    
    -- | Renders the given 'Renderable'.
    render :: a -> IO ()
