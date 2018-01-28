# This isn't working properly yet

import numpy as np

class VMC:
    def __init__(
            self,
            trial_wavefunction,
            electron_mass=1.0,
            n_electrons=1,
            n_dimensions=1,
            n_steps=1,
            nuclear_charge=1.0,
            proposal_distribution=None,
            step_size=0.1,
            **kwargs
    ):
        self.kwargs = kwargs
        self.M = electron_mass
        self.N = n_electrons
        self.n_steps = n_steps

        if proposal_distribution is None:
            proposal_distribution = np.random.normal
        self.P = proposal_distribution

        self.Psi = trial_wavefunction
        self.step = step_size
        self.dims = n_dimensions
        self.Z = nuclear_charge

        # Initalize random configuration of electrons
        self.R = np.random.random([self.N, self.dims])

    def eval_energy(
            self,
            current,
            proposal,
            configuration,
            Psi=None,
            M=None,
            Z=None,
            interactions=None
    ):
        """Evaluates the local energy of the moved electron.
        """
        if Psi is None:
            Psi = self.Psi

        if M is None:
            M = self.M

        if Z is None:
            Z = self.Z

        K = - 0.5 * M * (proposal - current)**2
        V = - Z / proposal

        if interactions is None:
            I = 0
        else:
            I = np.sum(
                [1 / np.abs(position - proposal) for position in interactions]
            )

        H = K + V + I

        return H / Psi(configuration)

    def eval_proposal(self, current, proposal, Psi=None):
        """Evaluates the probability of a proposed move being accepted.
        """
        if Psi is None:
            Psi = self.Psi

        ratio = np.abs(Psi(proposal))**2 / np.abs(Psi(current))**2

        return np.min([1.0, ratio])

    def run(self, verbose=True):
        """Runs the entire simulation.
        """
        for step in range(self.n_steps):
            self.update()
            if verbose:
                print("Step {}: E = {}".format(step, self.E))

        return self.E

    def propose_move(
            self,
            step_size=None,
            proposal_distribution=None,
            **kwargs
    ):
        """Propose a move from the current electron position.
        """
        if step_size is None:
            step_size = self.step

        if proposal_distribution is None:
            P = self.P

        if len(kwargs) == 0:
            kwargs = self.kwargs

        return P(**kwargs) * step_size

    def update(self, configuration=None):
        """Updates a single step for all electrons in the current
        configuration.
        """
        if configuration is None:
            configuration = self.R

        E = 0
        for indx, position in enumerate(configuration):
            proposal = configuration
            proposal[indx] += self.propose_move()
            acceptance = self.eval_proposal(configuration, proposal)
            if np.random.randn() < acceptance:
                configuration = proposal
            E += self.eval_energy(
                position,
                proposal[indx],
                proposal,
                interactions=configuration[:indx]
            )

        self.R = configuration
        self.E = E

    @property
    def configuration(self):
        """The current configuration of electron positions.
        """
        return self.R

    @property
    def expectation_value(self):
        """The expectation value of the current  electron configuration.
        """
        return self.E

    @property
    def electron_mass(self):
        """The mass of each electron.
        """
        return self.M

    @property
    def n_electrons(self):
        """The number of electrons in the simulation.
        """
        return self.N

    @property
    def n_dimensions(self):
        """The number of dimensions of the position vector for each
        electron.
        """
        return self.dims

    @property
    def nuclear_charge(self):
        """The charge of the nucleus.
        """
        return self.Z

    @property
    def step_size(self):
        """The maximum size of each Metropolis step.
        """
        return self.step

def wavefunction(r):
    """The trial wavefunction.
    """
    if len(r) != 2:
        raise RuntimeError(
            'Wavefunction requires two elements, received {}'.format(len(r))
        )
    x_1, x_2 = r
    return np.exp(-(x_1 + x_2)) * np.exp(np.abs(x_1 - x_2) / 2)


def main():
    # Initial setup
    psi_trial = None    # Trial wavefunction

    # Variational Monte Carlo
    vmc = VMC(
        trial_wavefunction=wavefunction,
        electron_mass=1.0,
        n_electrons=2,
        n_dimensions=1,
        n_steps=5000,
        nuclear_charge=2.0,
        proposal_distribution=None,
        step_size=0.01,
        loc=0.0,
        scale=0.05,
        size=[1]
    )

    vmc.run()
    print(vmc.configuration)

if __name__ == '__main__':
    main()
