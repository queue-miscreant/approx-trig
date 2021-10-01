Viewer
======
A (VERY INEFFICIENT) 3D viewer in Haskell.
Pros:
- Uses no trigonometric functions

Cons:
- Neither uses nor benefits from standard SDL functionalities for cameras
- Attempts drawing all vertices, not just ones in view
- No culling of distant vertices
- Does not draw tris in order of distance from camera
- Gets easily desynced because of variations in framerate

Icos.hs
-------
Functions for drawing a perspective projection of 3D space using quaternions and
a rather good trig approximation.
Currently includes no sane features like culling or removing vertices out of view.

Grid.hs
-------
Functions for moving in 2D space, as well as animators for the camera defined in Icos.
