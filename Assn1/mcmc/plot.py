import numpy as np
import matplotlib.pyplot plt

# 3.1 Metropolis Algorithm: Random Walk
# ---------------------------------------
with open('random_walk.txt','r') as file:
    rw = np.array(list(map(float, file.read().split())))

means = np.array([np.mean(rw[:i]) for i in range(rw.shape)])

# TODO: Format better
fig, (sub1, sub2) = plt.subplots(nrows=2, figsize=(8, 10))

sub1.hist(rw, bins=20)

sub2.plot(mean)
sub2.plot(np.arange(0, rw.shape))

plt.savefig('random_walk.png')
