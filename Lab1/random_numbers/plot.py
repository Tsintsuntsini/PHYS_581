import numpy as np
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt

# Plotting the points of LCG
# ----------------------------
# with open('main.txt', 'r') as file:
#     numbers = file.read().split()

# numbers = np.array(list(map(float, numbers)))

# First Example
#plt.title('Linear Congruetial Correlation\n with $A=7$, $C=0$, $M=10$')

# Second Example
#plt.title('Linear Congruential Correlation\n with $A=106$, $C=1283$, $M=6075$')

# Third Example
#plt.title('Linear Congruential Correlation\n with $A=107$, $C=1283$, $M=6075$')

# Fourth example
# plt.title(
#     'Linear Congruential Correlation\n with $A=1103515245, $C=12345$, M=32768$'
# )
# plt.xlabel('$x_n$')
# plt.ylabel('$x_{n+1}$')
# plt.scatter(numbers[:-1], numbers[1:], s=1)
# plt.savefig('main.png')

# Plotting RNG tests
# --------------------
with open('main.txt', 'r') as file:
    lines = file.readlines()

for indx, line in enumerate(lines):
    lines[indx] = np.array(list(map(float, line.split())))

lcg, auto_lcg, fortran, auto_fortran = lines

fig, (sub1, sub2) = plt.subplots(nrows=2)
<<<<<<< HEAD
sub1.plot(lcg)
sub1.set_title('Linear Congruential RNG')
sub1.set_xlabel('Time')
sub1.set_ylabel('Random Value')
sub2.plot(auto_lcg)
sub2.set_xlabel('Time')
sub2.set_ylabel('Autocorrelation Value')
plt.savefig('main.png')
=======

# sub1.set_title('Linear Congruetial Autocorrelation')
sub1.set_title('Fortran RNG Autocorrelation')

# sub1.plot(lcg)
sub1.plot(fortran)

sub1.set_xlabel('Time')
sub1.set_ylabel('Random Value')

# sub2.plot(auto_lcg)
sub2.plot(auto_fortran)

sub2.set_xlabel('Time')
sub2.set_ylabel('Autocorrelation Value')

# fig.savefig('lcg_auto')
fig.savefig('fortran_auto')
>>>>>>> 8c06e1bd792608b85ab8d81170af163b5db9196c
