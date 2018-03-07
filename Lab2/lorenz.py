import numpy as np


class RungeKuttaError(Exception):
    pass


def lorenz(position=np.array(5, 5, 5), sigma=10.0, b=8.0/3.0, r=28.0):
    """Calculates the time derivatives at the current position. for the
    given Lorenz system.

    Parameters:
    -------------
    position: (np.array) the current x, y, and z positions.
    sigma: (float) dxdt parameter.
    r: (float) dydt parameter.
    b: (float) dzdt parameter.

    Returns:
    ----------
    A np.array of the time derivatives at the current position.
    """
    try:
        x, y, z = position
    except:
        raise RungeKuttaError('The input vector should be have a size of 3')

    dxdt = sigma * (y - x)
    dydt = (r * x) - y - (x * z)
    dzdt = (x * y) - (b * z)

    return np.array([dxdt, dydt, dzdt])

def runge_kutta(
        position=np.array(5, 5, 5),
        nsteps=100,
        step_size=0.1,
        velocity=lorenz,
        **kwargs
):
    """Integrates the differential equations defined using the fourth-
    order Runge-Kutta method.

    Parameters:
    -------------
    position  : (np.array) the initial position to be used as input for
        differential equations.
    nsteps    : (int) the number of steps to integrate.
    step_size : (float) the size of each step.
    velocity  : (function) the system containing the differential
        equations. It should take the position as input and return the
        their respective derivatives as an equal sized np.array.
    kwargs    : key-value pairs used as other parameters for the velocity
        function.

    Returns:
    ----------
    An nsteps-sized np.array containing the estimated positions.
    """
    steps = np.zeros(shape=nsteps)
    steps[0] = position

    for n in range(1, nsteps):
        current = steps[n - 1]

        k1 = velocity(current, kwargs)
        k2 = velocity(current + 0.5 * k1 * step_size, **kwargs)
        k3 = velocity(current + 0.5 * k2 * step_size, **kwargs)
        k4 = velocity(current + k3 * step_size, **kwargs)

        steps[n] = current + (k1 + (2 * k2) + (2 * k3) + k4) * step_size / 6.0

    return steps


def main():
    # 4.1.1 Equlibrium
    # ------------------
    # Equilibrium points are:
    #     x = y = \sqrt{b (r - 1)}, z = r - 1

    # 4.1.2 First Simulation
    # ------------------------
    sigma = 10.0
    r = 28.0
    b = 8.0 / 3.0

    init = np.array(5, 5, 5)
    
