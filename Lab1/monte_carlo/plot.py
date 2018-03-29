import numpy as np
import matplotlib.pyplot as plt

with open('rng.txt') as file:
    lines = file.readlines()

for indx, line in enumerate(lines):
    lines[indx] = np.array(list(map(float, line.split())))

tau, mu, phi = lines

fig, (sub1, sub2, sub3) = plt.subplots(nrows=3)

sub1.hist(tau, bins=40, normed=True)
sub2.hist(mu, bins=40, normed=True)
sub3.hist(phi, bins=40, normed=True)

sub1.set_title('RNG for Photon Parameters')
sub3.set_xlabel('Random Value')
sub2.set_ylabel('Number of Occurences')

plt.savefig('rng.png')
