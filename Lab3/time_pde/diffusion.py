# 1.3.2 The Diffusion Equation
import matplotlib.pyplot as plt
import numpy as np


def main():
    x = np.linspace(0, 1, 1.0 / dx)

    # Initial conditions
    u_0 = np.sin(np.pi * x)

    # Boundary conditions
    u_0[0] = 0.0
    u_0[-1] = 0.0


if __name__ == '__main__':
    main()
