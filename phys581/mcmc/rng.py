# Random Distributions
import numpy as np

from .distributions import gaussian


def autocorrelation(sequence, periodic=False):
    """Autocorrelation test for random numbers.

    The autocorrelation tests for independence and how a sequence of
    random numbers is related to itself shifted by k numbers.

    Parameters
    ------------
    sequence : (np.array) array of random numbers.
    periodic : (bool)     true if the sequence is to be evaluated as a
                          periodic sequence.

    Returns
    ---------
    np.array of autocorrelation values corresponding to the interval.
    """
    if periodic:
        x = np.append(sequence, sequence)
    else:
        x = np.copy(sequence)

    N = x.size
    mean = np.mean(sequence)

    ac = np.array([
        np.sum((x[:N-k] - mean) * (x[k:] - mean)) / np.sum((x[:N-k] - mean)**2)
        for k in range(sequence.size)
    ])

    return ac


def chi_square(observed, expected=None):
    """Chi-square test for random numbers.

    The chi-square tests for uniformity and to check if a random
    sequence is independently and identically distributed.

    Parameters
    ------------
    observed : (np.array) array of the observed frequencies within a
                          given interval.
    expected : (np.array) array of the expected frequencies corresponding
                          to the interval in the observed. If none is
                          given, then the expected values are assumed to
                          be uniform.

    Returns
    ---------
    The chi-square value.
    """
    if expected is None:
        expected = np.sum(observed) / observed.size

    return np.sum((observed - expected)**2 / expected)


def rng_gaussian(
    size=1, mu=0.0, sigma=0.5, method='box_muller', minum=-1.0, maxum=+1.0
):
    """Draws random numbers from a Gaussian (normal) distribution.

    The Box-Muller method so the random numbers will always be in the
    range (mu - sigma, mu + sigma). The Accept/Reject method allows the
    minimum and maximum values to be specified.

    Parameters
    ------------
    size   : (int or list) size of the output array.
    mu     : (float)       the mean of the distribution.
    sigma  : (float)       the standard deviation or square root of the
                           variance of the distribution.
    method : (str)         either 'box_muller' or 'accept_reject'.
    minum  : (float)       the minimum value, only considered in the
                           'accept_reject' method.
    maxum  : (float)       the maximum value, only considered in the
                           'accept_reject' method.

    Returns
    ---------
    np.array of random numbers.
    """
    if method == 'box_muller':
        u1 = rng_uniform(size)
        u2 = rng_uniform(size)

        z = np.sqrt(- 2.0 * np.log(u1)) * np.cos(2.0 * np.pi * u2)
        z = z * sigma + mu

    elif method == 'accept_reject':
        peak = gaussian(mu, mu, sigma)
        trial = rng_uniform(size, minum, maxum)

        condition = gaussian(trial, mu, sigma) < rng_uniform(size, maxum=peak)
        while np.any(condition):
            trial[condition] = rng_uniform(np.sum(condition), minum, maxum)
            condition = gaussian(trial, mu, sigma) < rng_uniform(size, maxum=peak)

        z = trial

    else:
        raise NotImplementedError(
            '{} is not implemented, only "box_muller" and "accept_reject"'
            .format(method)
        )

    return z


def rng_uniform(size=1, minum=0.0, maxum=1.0):
    """Draws random numbers from a uniform distribution.

    The random numbers will be within the range (minum, maxum).

    Parameters
    ------------
    size  : (int or list) size of the output array.
    minum : (float)       the minimum value.
    maxum : (float)       the maximum value.

    Returns
    ---------
    np.array of random numbers.
    """
    return (maxum - minum) * np.random.random(size) + minum


def main():
    import matplotlib.pyplot as plt

    # Test autocorrelation
    random = np.random.random(100)
    auto = autocorrelation(random)
    plt.plot(auto)
    plt.show()

    auto = autocorrelation(random, periodic=True)
    plt.plot(auto)
    plt.show()

    # Test chi-square
    freqs = np.array([np.sum(np.logical_and(0.1 * k < random, random < 0.1 * k + 0.1)) for k in range(0, 9)])
    print(chi_square(freqs))

    # Test Gaussian
    g = rng_gaussian(100, 0, 1, method='box_muller')
    plt.hist(g)
    plt.show()

    g = rng_gaussian(100, 0, 1, method='accept_reject', minum=-4.0, maxum=4.0)
    plt.hist(g)
    plt.show()


if __name__ == '__main__':
    main()
