from __future__ import annotations
from dataclasses import dataclass
from math import sqrt

# Завдання 1. Комплексні числа (Complex)
@dataclass(frozen=True)
class Complex:
    re: float
    im: float

def complex_add(a: Complex, b: Complex) -> Complex:
    match a, b:
        case Complex(ar, ai), Complex(br, bi):
            return Complex(ar + br, ai + bi)

def complex_sub(a: Complex, b: Complex) -> Complex:
    match a, b:
        case Complex(ar, ai), Complex(br, bi):
            return Complex(ar - br, ai - bi)

def complex_mul(a: Complex, b: Complex) -> Complex:
    match a, b:
        case Complex(ar, ai), Complex(br, bi):
            return Complex(ar * br - ai * bi, ar * bi + ai * br)

def complex_conj(z: Complex) -> Complex:
    match z:
        case Complex(r, i):
            return Complex(r, -i)

def complex_abs(z: Complex) -> float:
    match z:
        case Complex(r, i):
            return sqrt(r * r + i * i)

c1 = Complex(1.0, 2.0)
c2 = Complex(3.0, -1.0)

print("Complex:")
print("c1 =", c1)
print("c2 =", c2)
print("c1 + c2 =", complex_add(c1, c2))
print("c1 - c2 =", complex_sub(c1, c2))
print("c1 * c2 =", complex_mul(c1, c2))
print("conj(c1) =", complex_conj(c1))
print("|c1| =", complex_abs(c1))


# Завдання 2. Вектори 3D (Vector3)
@dataclass(frozen=True)
class Vector3:
    x: float
    y: float
    z: float

def v3_norm(v: Vector3) -> float:
    match v:
        case Vector3(x, y, z):
            return sqrt(x * x + y * y + z * z)

def v3_dot(a: Vector3, b: Vector3) -> float:
    match a, b:
        case Vector3(ax, ay, az), Vector3(bx, by, bz):
            return ax * bx + ay * by + az * bz

def v3_cross(a: Vector3, b: Vector3) -> Vector3:
    match a, b:
        case Vector3(ax, ay, az), Vector3(bx, by, bz):
            return Vector3(
                ay * bz - az * by,
                az * bx - ax * bz,
                ax * by - ay * bx,
            )

def v3_mixed(a: Vector3, b: Vector3, c: Vector3) -> float:
    bc = v3_cross(b, c)
    return v3_dot(a, bc)

v1 = Vector3(1.0, 0.0, 0.0)
v2 = Vector3(0.0, 1.0, 0.0)
v3 = Vector3(0.0, 0.0, 1.0)

print("\nVector3:")
print("v1 =", v1)
print("v2 =", v2)
print("v3 =", v3)
print("|v1| =", v3_norm(v1))
print("dot(v1, v2) =", v3_dot(v1, v2))
print("cross(v1, v2) =", v3_cross(v1, v2))
print("mixed(v1, v2, v3) =", v3_mixed(v1, v2, v3))


# Завдання 3. Матриця 2×2 (Matrix2)
@dataclass(frozen=True)
class Matrix2:
    a11: float
    a12: float
    a21: float
    a22: float

def m2_det(m: Matrix2) -> float:
    match m:
        case Matrix2(a11, a12, a21, a22):
            return a11 * a22 - a12 * a21

def m2_inv(m: Matrix2) -> Matrix2:
    match m:
        case Matrix2(a11, a12, a21, a22):
            det = a11 * a22 - a12 * a21
            if det == 0:
                raise ValueError("Matrix is not invertible (det = 0)")
            inv_det = 1.0 / det
            return Matrix2(
                inv_det * a22,
                -inv_det * a12,
                -inv_det * a21,
                inv_det * a11,
            )

def m2_mul(a: Matrix2, b: Matrix2) -> Matrix2:
    match a, b:
        case Matrix2(a11, a12, a21, a22), Matrix2(b11, b12, b21, b22):
            return Matrix2(
                a11 * b11 + a12 * b21,
                a11 * b12 + a12 * b22,
                a21 * b11 + a22 * b21,
                a21 * b12 + a22 * b22,
            )

m1 = Matrix2(1, 2, 3, 4)
m2 = Matrix2(0, 1, -1, 0)

print("\nMatrix2:")
print("m1 =", m1)
print("det(m1) =", m2_det(m1))
print("inv(m1) =", m2_inv(m1))
print("m1 * m2 =", m2_mul(m1, m2))