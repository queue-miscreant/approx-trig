# Some useful derivations via quaternions

from sympy import symbols, sqrt, sympify
from sympy.matrices import Matrix

p,q = symbols('p q') #sphere parametrization

a,b,c = symbols('a b c')
x,y,z = symbols('x y z')
o,s = symbols('o s') #c"o"sine, "s"ine

#four matrices isomorphic to quaternions
#specifically, I @ J = K, J @ K = I, K @ I = J
ONE = Matrix(	[[ 1, 0, 0, 0],
				 [ 0, 1, 0, 0],
				 [ 0, 0, 1, 0],
				 [ 0, 0, 0, 1]])
#I @ I = -ONE
I = Matrix(	[[ 0,-1, 0, 0],
			 [ 1, 0, 0, 0],
			 [ 0, 0, 0, 1],
			 [ 0, 0,-1, 0]])
#J @ J = -ONE
J = Matrix(	[[ 0, 0,-1, 0],
			 [ 0, 0, 0,-1],
			 [ 1, 0, 0, 0],
			 [ 0, 1, 0, 0]])
#K @ K = -ONE
K = Matrix(	[[ 0, 0, 0, 1],
			 [ 0, 0,-1, 0],
			 [ 0, 1, 0, 0],
			 [-1, 0, 0, 0]])

# matrices isomorphic to D_4, the dihedral group of degree 4
# these may also be encoded via 2x2 matrices, but have overlap
#DIH_J @ DIH_J = -ONE
DIH_J = Matrix(	[[ 0, 0, 1, 0],
				 [ 0, 0, 0, 1],
				 [ 1, 0, 0, 0],
				 [ 0, 1, 0, 0]])
#DIH_K @ DIH_K = -ONE
DIH_K = Matrix(	[[ 0, 0, 0,-1],
				 [ 0, 0, 1, 0],
				 [ 0, 1, 0, 0],
				 [-1, 0, 0, 0]])

def double_turn_inter():
	'''
	Interpolating polynomial for an approximation of sin(pi*x) and cos(pi*x)
	Returns list in rising coefficients
	'''
	silver = sqrt(2) - 1
	half = sympify(1)/2
	vandermonde = Matrix(
	[[1, 0		,		0,		0],
	 [1, half	, half**2, half**3],
	 [1, -half	, half**2, -half**3],
	 [1, 1		,		1,		1]]
	)
	result = vandermonde.inv() @ Matrix([0, silver, -silver, 1])
	return result[0:]

def stereograph(real=ONE, imag1=I, imag2=J):
	'''
	Stereographic projection of a sphere, obtained via quaternions
	Which pair of imaginary values used does not matter
	Can also be used for hyperboloids of one or two sheets, with DIH_* matrices
	'''
	# numerator
	num = (real + imag1*p + imag2*q) @ (real + imag1*p + imag2*q)
	# denominator entries only exist on the main diagonal
	den = (real + imag1*p + imag2*q) @ (real - imag1*p - imag2*q)

	result = num/den[0,0]

	return [((component @ result)[0,0] * (component @ component)[0,0]).cancel()
		for component in (real, imag1, imag2)]

def general_rotation():
	'''Symbolic quaternion rotation'''
	#great circle in plane ax + by + cz = 0
	#a**2 + b**2 + c**2 = 1
	q_1 = o*ONE - (a*I + b*J + c*K)*s
	q   = o*ONE + (a*I + b*J + c*K)*s
	#3-vector in space
	v   = x*I + y*J + z*K

	#the expression for v', the rotation induced by q
	rotated = q @ v @ q_1
	rotated.simplify()
	return rotated
