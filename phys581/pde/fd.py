# Finite Differences


def centre_diff(x, dx, order=1):
    """Computes the first/second derivative using central finite
    differencing.

    Parameters
    ------------
    x: (np.array) the values used to compute the derivative.
    dx : (float) the distance between points in x.
    order: (int) the order of the derivative. Only 1 and 2 are supported.
    """
    diff = np.zeros(x.size)

    if order == 1:
        diff[1:-1] = (x[2:] - x[:-2]) / (2.0 * dx)
    elif order == 2:
        diff[1:-1] = (x[2:] - 2 * x[1:-1] + x[:-2]) / dx**2
    else:
        raise NotImplementedError(
            'Only first and second derivatives are supported.'
        )

    return diff
