# 1.3.3 The Advection-Diffusion Equation
import matplotlib.pyplot as plt
import numpy as np
import datetime


class AdvectionDiffusion1D:
    def __init__(self, initial_state, r, s):
        self.current_state = np.array(initial_state)
        self.r = float(r)
        self.s = float(s)

    def step(self, eps=1e-4):
        """Computes the next step using a first order forward difference
        scheme for the time derivative, a first order backward
        difference scheme for the advection component, and a second
        order centred difference scheme for the diffusive compnent.

        Parameters
        ------------
        eps: (float) maximum error when iterating using the Jacobi
                     method.

        Returns
        ---------
        An array with the updated values.
        """
        diff1 = 0.5 * (self.current_state[1:-1] - self.current_state[0:-2])
        diff2 = self.current_state[2:] - 2 * self.current_state[1:-1] + self.current_state[:-2]

        self.current_state[1:-1] += self.s * diff2 - self.r * diff1

        return self.current_state


def main():
    # Set constants
    c = 0.5
    beta = 0.1
    x = np.linspace(0, 1, 101)
    dx = x[1] - x[0]
    dt = 0.9 * dx**2 / (c * dx + 2 * beta)
    ts = np.linspace(0, 57, int(1.0 / dt))
    
    r = c * dt / dx
    s = beta * dt / dx**2

    # Initial conditions
    u_0 = np.sin(np.pi * x)

    # Boundary coniditions
    u_0[0] = 0.0
    u_0[-1] = 0.0

    # Solve the equation
    ad = AdvectionDiffusion1D(u_0, r, s)
    for step, t in enumerate(ts):
        ad.step()
        if step == int(0.25 * ts.size):
            plt.plot(x, ad.current_state)
        if step == int(0.5 * ts.size):
            plt.plot(x, ad.current_state)
        if step == int(0.75 * ts.size):
            plt.plot(x, ad.current_state)

    # Plot the solution
    plt.title('Advection-Diffusion of $\sin(\pi x)$')
    plt.plot(x, u_0, label='$t=0$s')
    plt.plot(x, ad.current_state, label='$t=57$s')
    plt.legend()
    plt.savefig('advection_diffusion')

    print('Number of time steps: {}'.format(ts.size))
    print('Location of peak: {}'.format(x[ad.current_state == np.max(ad.current_state)]))

if __name__ == '__main__':
    main()
