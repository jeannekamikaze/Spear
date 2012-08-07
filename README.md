Spear
=====

Spear is a simple 3D game framework I have been working on since I started learning Haskell. The project's goal is to put what I learn into practise, to explore how far I can get with Haskell and if the results are decent enough, to build one or two game demos along the way.

Installation
------------

Simply clone the repo and build with cabal:

```
git clone https://github.com/jeannekamikaze/Spear.git
cd Spear
cabal install
```

Documentation
-------------

An online copy of the haddocks can be found [here][0].

Features
--------

### Application and Input
* Easy way to set up a window with the desired OpenGL context version.
* Raw polled, toggled and delayed input.
* High resolution timer.

### Assets
* MD2 and OBJ model loaders.
* BMP image loader.
* Assets backed up by Resource for automatic (and optionally, manual) deletion.

### Collision
* Simple collision library featuring AABBs and bounding spheres (more to come).

### OpenGL
* High level OpenGL >=3 wrappers.
* OpenGL resources (VAOs, buffers, textures, etc.) backed up by Resource for automatic (and optionally, manual) deletion.

### Math
* Vectors, matrices, quaternions, cameras, etc.
* The Spatial type class for objects that can be moved around in 3D space.

### Render
* Static and vertex-animated model resources, compiled into a VAO for efficient rendering.
* Static and vertex-animated model renderers. Vertex animation is done in a vertex shader.

### Scene
* Automated loading of scenes and scene resources as described by scene files.

[0]: http://shellblade.net/docs/Spear/index.html
