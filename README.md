approx-trigonometry
===================
Some notes involving stereographic projections of the circle and sphere.

Files
-----

### quat.py
Notebook of symbolic manipulations involving quaternions.

### sphere_tangent_proj.ggb
GeoGebra document demonstrating stereographic sphere and tangent plane at a
point. Shows normal and tangent vectors at the point and demonstrates the 
["hairiness"](https://en.wikipedia.org/wiki/Hairy_ball_theorem) of a sphere.


### Approximate Trig/stereographic_trig.ggb
GeoGebra document demonstrating approximations for sine and cosine based on
complex expression for a circle. Includes interpolation which improves accuracy
to around 1%.

### Approximate Trig/stereo_math.c
C program which tests speed of default implementations of `sin` and `cos` against 
the approximations derived in the above GeoGebra document.

### Approximate Trig/stereo_complex.c
C program which tests speed of default implementation of `cexp` against the
approximations derived in the above GeoGebra document.


### Viewer/\*
Sub-project: a 3D viewer written from scratch in Haskell using SDL. Uses the
approximations derived above in conjunction with quaternions to create an
image of a projection of 3D space.
