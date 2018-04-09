# 1.3.2 The Diffusion Equation
import matplotlib.pyplot as plt
import numpy as np


class Diffusion1D:
    def __init__(self, initial_state, beta, dt, dx, r):
        self.current_state = np.array(initial_state)

        self.beta = float(beta)
        self.dt = float(dt)
        self.dx = float(dx)
        self.r = float(r)

    def step(self):
        current = np.copy(self.current_state)
        
        self.current_state[1:-1] = current[1:-1] + self.r * (
            current[2:] - 2 * current[1:-1] + current[:-2]
        )
        
        return self.current_state


def main():
    # Define constants
    beta = 1.0
    dx = 1.0 / 50.0
    dt = 0.9 * dx**2 / (2.0 * beta)
    ts = np.arange(0, 300) * dt
    x = np.linspace(0, 1, 50)

    # Initial conditions
    u_0 = np.sin(np.pi * x)

    # Exact solution
    exact = np.exp(- beta * np.pi**2 * ts[-1]) * np.sin(np.pi * x)

    # Boundary conditions
    u_0[0] = 0.0
    u_0[-1] = 0.0

    # Solve the diffusion equation
    df = Diffusion1D(u_0, beta, dt, dx, r)
    for t in ts:
        df.step()

    # Plot solution
    fig, subs = plt.subplots(nrows=3)
    subs[0].plot(x, u_0)
    subs[1].plot(x, df.current_state)
    subs[2].plot(x, exact)
    plt.savefig('diffusion')


if __name__ == '__main__':
    main()
