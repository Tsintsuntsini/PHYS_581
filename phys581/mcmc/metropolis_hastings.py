# Metropolis-Hastings Algorithm


class MetropolisHastings:
    """Class that performs Metropolis-Hastings random walks.

    Parameters
    ------------
    """
    def __init__(self, eval_target, eval_proposal, draw_proposal, n_walkers=1, **kwargs):
        self.eval_target = eval_target
        self.eval_proposal = eval_proposal
        self.draw_proposal = draw_proposal

        self.walkers = np.zeros(n_walkers)

        self.kwargs = kwargs

    def step(self):
        proposal = self.draw_proposal(self.walkers, self.kwargs)
        acceptance = (
            self.target(proposal, self.kwargs)
            * self.eval_proposal(proposal, self.walkers, self.kwargs)
        ) / (
            self.target(self.walkers, self.kwargs)
            * self.eval_proposal(self.walkers, proposal, self.kwargs)
        )

        accepted = rng_uniform(self.walker.size) < acceptance

        self.walkers[accepted] = proposal[accepted]

        return self.walker
