Spear
=====

Spear is a simple 2.5D game engine I have been working on since I started learning Haskell.
The project's goal is to put what I learn into practise, to explore how far I can get with Haskell and if the results
are decent enough, to build one or two game demos along the way.

Installation (Ubuntu)
---------------------

Install dependencies, then build with cabal:

```
$ sudo apt install libxxf86vm-dev libglfw3-dev
$ git clone https://github.com/jeannekamikaze/Spear.git
$ cd Spear
$ cabal build
```

Run a demo:

```
$ cabal run pong
```

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
* Simple collision library featuring AABBs and bounding circles.

### OpenGL
* OpenGL >=3 wrapper library.
* OpenGL resources (VAOs, buffers, textures, etc.) backed up by Resource for automatic (and optionally, manual) deletion.

### Math
* Vectors, matrices, quaternions, cameras, segments, rays, etc.
* The Spatial2 and Spatial3 type classes for objects that can be moved around in 2D and 3D space, respectively.

### Render
* Static and vertex-animated model resources, compiled into a VAO for efficient rendering.
* Static and vertex-animated model renderers. Vertex animation is done in a vertex shader.

### Scene
* Automated loading of scenes and scene resources as described by scene files.
