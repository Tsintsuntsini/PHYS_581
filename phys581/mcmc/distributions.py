# Distributions
import numpy as np


def gaussian(x, mu, sigma, normalize=True):
    """Computes the Gaussian (normal) distribution of x.

    Parameters
    ------------
    x         : (float or np.array) the input values.
    mu        : (float)             the mean.
    sigma     : (float)             the standard deviation.
    normalize : (bool)              true if the output values are to be
                                    normalized.
    """
    if normalize:
        N = np.sqrt(2.0 * np.pi * sigma**2)
    else:
        N = 1.0
    return 1.0 / N * np.exp(- (x - mu)**2 / (2 * sigma**2))
