# 1.3.1 Advection equation
import logging
import matplotlib.pyplot as plt
import numpy as np


def centre_diff_1D(x, dx, order=1):
    """Computes the first or second derivative using centred finite
    difference method.

    Parameters
    ------------
    x     : (np.array) the values used to compute the derivative.
    dx    : (float)    the distance between points in x.
    order : (int)      the order of derivative, can be 1 or 2.

    Returns
    ---------
    An array with the same size as x. The derivatives on the boundary
    are set to 0.
    """
    diff = np.zeros(x.size)
    if order == 1:
        diff[1:-1] = (x[2:] - x[:-2]) / (2 * dx)
    elif order == 2:
        diff[1:-1] = (x[2:] - 2 * x[1:-1] + x[:-2]) / dx**2

    return diff

def forward_euler_1D(x, dx, r):
    """Computes the next time iteration using the explicit forward Euler
    method with centred finite differencing to compute the spatial
    derivatives.

    Parameters
    ------------
    x  : (np.array) the current values.
    dx : (float)    the distance between the points in x.
    r  : (float)    the Courant number.

    Returns:
    ----------
    An array with the updated values of x.
    """
    return x + r * centre_diff_1D(x, dx, order=1) * dx

def backward_euler_1D(x, r, eps=1e-4):
    """Computes the next time iteration using the implicit backward
    Euler method with centred finite differencing to compute the spatial
    derivatives.

    The Jacobi method is used to estimate the next time step.

    Parameters
    ------------
    x   : (np.array) the current values.
    r   : (float)    the Courant number.
    eps : (float)    the maximum error value. The default value is 1e-4.

    Returns
    ---------
    An array of the updated values of x.
    """
    guess = np.zeros(x.size)
    guess[:1] = x[:1]
    guess[-1:] = x[-1:]

    current = np.copy(x)

    while np.max(np.abs(guess - current)) > eps:
        current = np.copy(guess)
        guess[1:-1] = x[1:-1] + 0.5 * r * (current[2:] - current[:-2])

    return guess

def lax_wendroff_1D(x, dx, r):
    """Computes the next time iteration using the explicit Lax-Wendroff
    method to compute the spatial derivatives.

    Parameters
    ------------
    x: (np.array) the current values.
    dx: (float) the distance between points in x.
    r: (float) the Courant number.

    Returns
    ---------
    An array with the updated values of x.
    """
    diff1 = x[2:] - x[:-2]
    diff2 = x[2:] - 2 * x[1:-1] + x[:-2]

    lw = np.copy(x)
    lw[1:-1] = x[1:-1] + 0.5 * r * diff1 + 0.5 * r**2 * diff2

    return lw

def crank_nicholson_1D(x, dx, r, eps=1e-4):
    print('computing crank-nicholson')
    guess = np.zeros(x.size)
    guess[:1] = x[:1]
    guess[-1:] = x[-1:]

    current = np.copy(x)

    forward = x[2:] - x[:-2]

    while np.max(np.abs(guess - current)) > eps:
        current = np.copy(guess)

        backward = current[2:] - current[:-2]
        guess[1:-1] = x[1:-1] + 0.25 * r * (forward + backward)

    return guess


class Advection1D:
    def __init__(self, initial_state, c, dx, dt, r):
        self.current_state = np.array(initial_state)

        self.c = float(c)
        self.dx = float(dx)
        self.dt = float(dt)
        self.r = float(r)

    def step(self, method):
        self.current_state = getattr(self, method)()
        return self.current_state

    def forward_euler(self):
        return forward_euler_1D(self.current_state, self.dx, self.r)

    def backward_euler(self):
        return backward_euler_1D(self.current_state, self.r)

    def lax_wendroff(self):
        return lax_wendroff_1D(self.current_state, self.dx, self.r)

    def crank_nicholson(self):
        return crank_nicholson_1D(self.current_state, self.dx, self.r)

    
def main():
    # Conduct 7 trials with different parameters
    cs =  [0.5,  0.5,  0.5,    0.5,    0.5,    0.5,  0.5 ]
    dxs = [0.04, 0.02, 0.0137, 0.0101, 0.0099, 0.02, 0.02]
    dts = [0.02, 0.02, 0.02,   0.02,   0.02,   0.01, 0.04]
    rs =  [0.25, 0.5,  0.728,  0.99,   1.11,   0.25, 1.0 ]
    params = zip(cs, dxs, dts, rs)

    # Solve advection equation for each set of parameters
    for trial_num, (c, dx, dt, r) in enumerate(params):
        # Create variables
        x = np.linspace(-2.0, +2.0, int(4.0 / dx))
        ts = np.linspace(0.0, 2.0, int(2.0 / dt))

        # Initial conditions for 4 cases
        case_1 = np.sin(2 * x)
        case_2 = 1.0 * (x >= 0.0)
        case_3 = np.zeros(x.size)
        case_3[int(x.size / 2)] = 1.0 / dx
        case_4 = np.exp(- 4.0 * x**2)
        cases = np.array([case_1, case_2, case_3, case_4])

        # Exact solutions
        x_ct = x + c * ts[-1] * np.ones(x.size)
        exact_1 = np.sin(2 * x_ct)
        exact_2 = 1.0 * (x_ct >= 0.0)
        exact_3 = np.zeros(x_ct.size)
        exact_3[int(x_ct.size / 4)] = 1.0 / dx
        exact_4 = np.exp(- 4.0 * x_ct**2)
        exacts = np.array([exact_1, exact_2, exact_3, exact_4])

        for case_num, (case, exact) in enumerate(zip(cases, exacts)):
            # Create different steppers
            fw = Advection1D(case, c, dx, dt, r)
            bw = Advection1D(case, c, dx, dt, r)
            lw = Advection1D(case, c, dx, dt, r)
            cn = Advection1D(case, c, dx, dt, r)

            # Solve each case
            for t in ts:
                fw.step('forward_euler')
                bw.step('backward_euler')
                lw.step('lax_wendroff')
                cn.step('crank_nicholson')

            plt.clf()
            fig, subs = plt.subplots(nrows=6)
            subs[0].plot(x, case)
            subs[1].plot(x, exact)
            subs[2].plot(x, fw.current_state)
            subs[3].plot(x, bw.current_state)
            subs[4].plot(x, lw.current_state)
            subs[5].plot(x, cn.current_state)
            plt.savefig(
                'forward_euler_case_{}_trial_{}'.format(case_num, trial_num)
            )
            plt.close(fig)


if __name__ == '__main__':
    main()
