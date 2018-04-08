# 1.3.1 Advection equation
import matplotlib.pyplot as plt
import numpy as np


def centre_diff_1D(x, dx, n):
    """Computes the first derivative using centred finite difference
    method.

    Parameters
    ------------
    x  : (np.array) the values used to compute the derivative.
    dx : (float)    the distance between points in x.
    n  : (int)      the size of the ghost zones on both ends of x.

    Returns
    ---------
    An array with the same size as x. The values in the ghost zones are
    set to zero.
    """
    x[n:-n] = (x[n+1:] - x[:-n-1]) / (2 * dx)
    x[:n] = 0.0
    x[-n:] = 0.0
    return x

def forward_euler_1D(x, dx, r, n):
    """Computes the next time iteration using the explicit forward Euler
    method with centred finite differencing to compute the spatial
    derivatives.

    Parameters
    ------------
    x  : (np.array) the current values.
    dx : (float)    the distance between the points in x.
    r  : (float)    the Courant number.
    n  : (int)      the size of the ghost zones on both ends of x.

    Returns:
    ----------
    An array with the updated values of x.
    """
    return x + 0.5 * r * centre_diff_1D(x, dx, n) * dx

def backward_euler_1D(x, r, n, eps=1e-4):
    """Computes the next time iteration using the implicit backward
    Euler method with centred finite differencing to compute the spatial
    derivatives.

    The Jacobi method is used to estimate the next time step.

    Parameters
    ------------
    x   : (np.array) the current values.
    r   : (float)    the Courant number.
    n   : (int)      the size of the ghost zones on both ends of x.
    eps : (float)    the maximum error value. The default value is 1e-4.

    Returns
    ---------
    An array of the updated values of x.
    """
    current = x

    guess = np.zeros(x.size)
    guess[:n] = x[:n]
    guess[-n:] = x[-n:]

    while np.max(guess - current) > eps:
        guess[n:-n] = current[n:-n] + 0.5 * r * (guess[n+1:] - guess[:-n-1])

    return guess

def lax_wendroff():
    pass

def crank_nicholson():
    pass

class Advection1D:
    def __init__(self, initial_state, c, dx, dt, r, ghost_size):
        self.current_state = np.array(initial_state)

        if isinstance(ghost_size, int):
            self.ghost_size = ghost_size
        else:
            raise TypeError('Number of ghost zones should be type int')

        self.c = c
        self.dx = dx
        self.dt = dt
        self.r = r

    def boundary():
        pass

    def step(self, method):
        self.current_state = getattr(self, method)()
        return self.current_state

    def forward_euler(self):
        return forward_euler_1D(self.current_state, self.dx, self.r, self.ghost_size)

    def backward_euler(self):
        return backward_euler_1D(self.current_state, self.r, self.ghost_size)

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
        case_2 = 1.0 * (x >= 1.0)
        case_3 = (1.0 / dx) * (x == 0.0)
        case_4 = np.exp(- 4.0 * x**2)
        cases = np.array([case_1, case_2, case_3, case_4])

        for case_num, case in enumerate(cases):
            # Create different steppers
            av = Advection1D(case, c, dx, dt, r, 1)
            fw = av
            bw = av
            lw = av
            cn = av

            # Solve each case
            for t in ts:
                fw.step('forward_euler')
                bw.step('backward_euler')
                #lw.step('lax_wendroff')
                #cn.step('crank_nicholson')

            plt.clf()
            fig, subs = plt.subplots(nrows=2)
            subs[0].plot(fw.current_state)
            subs[1].plot(bw.current_state)
            plt.savefig('forward_euler_case_{}_trial_{}'.format(case_num, trial_num))


if __name__ == '__main__':
    main()
