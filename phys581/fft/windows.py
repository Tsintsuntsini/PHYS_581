# Filtering Windows
import numpy as np


def hanning_window(size):
    """Computes the Hanning window.

    Parameters
    ------------
    size: (int) the number of points in the window.

    Returns
    ---------
    np.array with the values of the window.
    """
    t = 2.0 * np.pi * np.arange(0, size) / (size - 1.0)
    return 0.5 * (1.0 - np.cos(t))


def hamming_window(size):
    """Computes the Hamming window.

    Parameters
    ------------
    size: (int) the number of points in the window.

    Returns
    ---------
    np.array with the values of the window.
    """
    t = 2.0 * np.pi * np.arange(0, size) / (size - 1.0)
    return 0.54 - 0.46 * np.cos(t)


def blackmann_window(size):
    """Computes the Blackmann window.

    Parameters
    ------------
    size: (int) the number of points in the window.

    Returns
    ---------
    np.array with the values of the window.
    """
    t = 2.0 * np.pi * np.arange(0, size) / (size - 1.0)
    return 0.42 - 0.5 * np.cos(t) + 0.08 * np.cos(2.0 * t)


def blackmann_harris_window(size):
    """Computes the Blackmann-Harris window.

    Parameters
    ------------
    size: (int) the number of points in the window.

    Returns
    ---------
    np.array with the values of the window.
    """
    t = 2.0  * np.pi * np.arange(0, size) / (size - 1.0)
    bh = 0.35875 - 0.48829 * np.cos(t) + 0.14128 * np.cos(2.0 * t) \
         - 0.01168 * np.cos(3.0 * t)
    return bh


def main():
    import matplotlib.pyplot as plt

    # Test windows
    size = 100

    hann = hanning_window(size)
    plt.plot(hann, label='Hanning')

    hamm = hamming_window(size)
    plt.plot(hamm, label='Hamming')

    black = blackmann_window(size)
    plt.plot(black, label='Blackmann')

    harris = blackmann_harris_window(size)
    plt.plot(harris, label='Blackmann-Harris')

    plt.legend()
    plt.plot()
    plt.show()
