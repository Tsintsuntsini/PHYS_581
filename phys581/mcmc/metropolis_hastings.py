# Metropolis-Hastings Algorithm
import numpy as np

from .rng import rng_uniform


class MetropolisHastings:
    """Class that performs Metropolis-Hastings random walks.

    Parameters
    ------------
    eval_target   : (function) target distribution which evaluates the
                               position of the walkers. Should take
                               arguments

        eval_target(x, y, **kwargs)

    eval_proposal : (function) proposal distribution which evaluates the
                               position of the walkers. Should take
                               arugments

        eval_proposal(x, y, **kwargs)

    draw_proposal : (function) draws random numbers from the proposal
                               distribution. Should take arguments

        draw_proposal(size, **kwargs)

    n_walkers     : (int)      the number of walkers to keep track of.
    target_args   : (dict)     keyword arguments for eval_target.
    proposal_args : (dict)     keyword arguments for proposal_target.
    draw_args     : (dict)     keyword arguments for draw_proposal.
    """
    def __init__(
        self, eval_target, eval_proposal, draw_proposal, n_walkers=1,
        target_args={}, proposal_args={}, draw_args={}
    ):
        self.eval_target = eval_target
        self.eval_proposal = eval_proposal
        self.draw_proposal = draw_proposal

        self.walkers = np.zeros(n_walkers)

        self.target_args = target_args
        self.proposal_args = proposal_args
        self.draw_args = draw_args

    def step(self, T=1.0):
        """Takes a single Metropolis-Hastings step.

        Parameters
        ------------
        T: (float) the temperature for stimulated annealing. This
                   variable is not stored in the class and should be
                   kept track of outside of the class. Default value is
                   T=1.

        Returns
        ---------
        np.array with the updated position of the walkers.
        """
        proposal = self.draw_proposal(
            self.walkers.size, self.walkers, **self.draw_args
        )
        acceptance = (
            self.eval_target(proposal, **self.target_args)
            * self.eval_proposal(proposal, self.walkers, **self.proposal_args)
        ) / (
            self.eval_target(self.walkers, **self.target_args)
            * self.eval_proposal(self.walkers, proposal, **self.proposal_args)
        )**(1.0 / T)

        accepted = rng_uniform(self.walkers.size) < acceptance
        self.walkers[accepted] = proposal[accepted]

        return np.copy(self.walkers)


def main():
    import matplotlib.pyplot as plt

    from . import gaussian, rng_gaussian

    # Test Metropolis-Hastings
    def target(x):
        return 1.0 / (2.0 * np.sqrt(2.0)) * (np.sin(5.0 * x) + np.sin(2.0 * x) + 2.0) * np.exp(-x**2)

    mh = MetropolisHastings(
        target, gaussian, rng_gaussian,
        proposal_args={'sigma': 1.0},
        draw_args={'sigma': 1.0})

    x = np.linspace(-3, 3, 100)
    burn = [mh.step(1.0) for step in range(300)]
    steps = np.array([mh.step() for T in np.linspace(1, 0.1, 1000)])
    plt.plot(x, target(x))
    plt.hist(steps[:,0], bins=50, normed=True)
    plt.show()


if __name__ == '__main__':
    main()
