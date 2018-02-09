import numpy as np
import matplotlib.pyplot as plt

def gaussian(x, mu, sigma):
    factor = 1 / (np.sqrt(2 * np.pi) * sigma)
    return factor * np.exp(- (x - mu)**2 / (2 * sigma**2))

# 1.2 Random Numbers from a Normal Distribution
with open('normal.txt', 'r') as file:
    lines = file.readlines()

ar, ctl = np.array([list(map(float, line.split())) for line in lines])

mu = 5.0
sigma = 1.25
x = np.linspace(mu - 4 * sigma, mu + 4 * sigma, 100)

plt.title('Comparison of Methods to Sample from a Normal Distribution')
plt.xlabel('x-position')
plt.ylabel('Probability')
plt.hist(ar, bins=100, normed=True, alpha=0.6, label='Accept/Reject')
plt.hist(ctl, bins=100, normed=True, alpha=0.6, label='Central Limit')
plt.plot(x, gaussian(x, mu, sigma), label='Analytical')
plt.legend(loc='best')
plt.savefig('normal.png')
