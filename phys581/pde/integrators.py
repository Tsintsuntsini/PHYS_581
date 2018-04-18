# Integration Methods


def runge_kutta(system, initial, n_steps = 100, step_size=0.1, **kwargs):
    """Integrates the differential equations using the fourth-order
    Runge-Kutta method.

    Parameters
    ------------
    system    : (function) the system of differential equations. It
                           should take arguments

        system(initial_position, **kwargs)

    initial   : (np.array) the initial state of the system.
    n_steps   : (int)      the number of steps used to integrate.
    step_size : (float)    the size of each step.
    kwargs    :            keyword arguments for system.

    Returns
    ---------
    np.array with the updated values.
    """
    step = np.copy(initial)
    for n in range(1, n_steps):
        current = step

        k1 = system(current, **kwargs)
        k2 = system(current + 0.5 * k1 * step_size, **kwargs)
        k3 = system(current + 0.5 * k2 * step_size, **kwargs)
        k4 = system(current + k3 * step_size, **kwargs)

        step = current + (k1 + (2 * k2) + (2 * k3) + k4) * step_size / 6.0

    return step
