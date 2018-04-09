# 1.3.3 The Advection-Diffusion Equation
import matplotlib.pyplot as plt
import numpy as np


class AdvectionDiffusion1D:
    def __init__(self, initial_state):
        self.current_state = initial_state

    def step(self):
        """Computes the next step using a first order forward difference
        scheme for the time derivative, a first order backward
        difference scheme for the advection component, and a second
        order centred difference scheme for the diffusive compnent.

        Parameters
        ------------

        Returns
        ---------
        An array with the updated values.
        """
        return self.current_state


def main():
    # Set constants
    c = 0.5
    beta = 0.1
    ts = np.linspace(0, 57, 1.0 / dt)
    x = np.linspace(0, 60, 101)

    # Initial conditions
    mean = 3.0
    std_dev = 12.0
    u_0 = gaussian(x, mean, std_dev)

    # Boundary coniditions
    u_0[0] = 0.0
    u_0[-1] = 0.0

    # Solve the equation
    ad = AdvectionDiffusion1D(u_0)
    for t in ts:
        ad.step()

    # Plot the solution
    plt.plot(x, u_0, label='$t=0$s')
    plt.plot(x, ad.current_state, label='$t=57$s')
    plt.legend()
    plt.savefig('advection_diffusion')


if __name__ == '__main__':
    main()
