import numpy as np
import matplotlib.pyplot as plt

# with open('mcmc1.txt', 'r') as file:
#     lines = file.readlines()
# with open('mcmc2.txt', 'r') as file:
#     lines = file.readlines()
with open('burn1.txt', 'r') as file:
    lines = file.readlines()
# with open('burn2.txt', 'r') as file:
#     lines = file.readlines()

for indx, line in enumerate(lines):
    lines[indx] = list(map(float, line.split()))

# sigma1, sigma2, sigma3 = lines
# accept1, accept2, accept3 = sigma1.pop(), sigma2.pop(), sigma3.pop()
# sigma1, sigma2, sigma3 = np.array(sigma1), np.array(sigma2), np.array(sigma3)
x1, x2, x3 = lines
accept1, accept2, accept3 = x1.pop(), x2.pop(), x3.pop()
x1, x2, x3 = np.array(x1), np.array(x2), np.array(x3)

fig, (sub1, sub2) = plt.subplots(nrows=2, figsize=(8, 10))

x = np.linspace(-3, 3, 120)
P = (1 / (2 * np.sqrt(np.pi))) * (np.sin(5 * x) + np.sin(2 * x) + 2) * np.exp(- x**2)
sub1.plot(x, P, color='blue', label='$P(x)$')

sub1.set_xlim(-3, 3)
sub1.set_ylim(0, 1)
# sub1.set_title('Metropolis-Hastings with Varying Proposal Distributions')
sub1.set_title('Independent Walkers without Burn-In')
# sub1.set_title('Independent Walkers with Burn-In')
sub1.set_xlabel('$x$ Position')
sub1.set_ylabel('Probability')
sub1.hist(
    x1, bins=60, normed=True, color='black', alpha=0.5,
    label='Walker 1, $\\alpha = {}$'.format(round(accept1, 3))
)
sub1.hist(
    x2, bins=60, normed=True, color='green', alpha=0.5, 
    label='Walker 2, $\\alpha = {}$'.format(round(accept2, 3))
)
sub1.hist(
    x3, bins=60, normed=True, color='red', alpha=0.5,
    label='Walker 3, $\\alpha = {}$'.format(round(accept3, 3))
)
sub1.legend(loc='best')

# y = np.arange(0, 800)
y = np.arange(0, 1000)
# y = np.arange(0, 50000)
sub2.set_xlabel('$x$ Position')
sub2.set_ylabel('Iteration Number')
sub2.plot(x1, y, color='black', alpha=0.5)
sub2.plot(x2, y, color='green', alpha=0.5)
sub2.plot(x3, y, color='red', alpha=0.5)

# plt.savefig('mcmc1.png')
# plt.savefig('mcmc2.png')
plt.savefig('burn1.png')
# plt.savefig('burn2.png')

