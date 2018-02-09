import numpy as np
import matplotlib.pyplot as plt

with open('edged_square.txt', 'r') as file:
    lines = file.readlines()

start1, start2, start3, start4 = \
    np.array([list(map(float, line.split())) for line in lines])

fig, ((sub1, sub2), (sub3, sub4)) = plt.subplots(nrows=2, ncols=2)

sub1.set_title('Starting Point $1$')
sub1.set_ylim(0, 0.4)
sub1.hist(start1[50:], bins=4, range=(0.5, 4.5), normed=True)
sub1.axvline(x=1.0, ymax=0.75, c='red')
sub1.axvline(x=2.0, ymax=0.5, c='red')
sub1.axvline(x=3.0, ymax=0.75, c='red')
sub1.axvline(x=4.0, ymax=0.5, c='red')

sub2.set_title('Starting Point $2$')
sub2.set_ylim(0, 0.4)
sub2.hist(start2[50:], bins=4, range=(0.5, 4.5), normed=True)
sub2.axvline(x=1.0, ymax=0.75, c='red')
sub2.axvline(x=2.0, ymax=0.5, c='red')
sub2.axvline(x=3.0, ymax=0.75, c='red')
sub2.axvline(x=4.0, ymax=0.5, c='red')

sub3.set_title('Starting Point $3$')
sub3.set_ylim(0, 0.4)
sub3.hist(start3[50:], bins=4, range=(0.5, 4.5), normed=True)
sub3.axvline(x=1.0, ymax=0.75, c='red')
sub3.axvline(x=2.0, ymax=0.5, c='red')
sub3.axvline(x=3.0, ymax=0.75, c='red')
sub3.axvline(x=4.0, ymax=0.5, c='red')

sub4.set_title('Starting Point $4$')
sub4.set_ylim(0, 0.4)
sub4.hist(start4[50:], bins=4, range=(0.5, 4.5), normed=True)
sub4.axvline(x=1.0, ymax=0.75, c='red')
sub4.axvline(x=2.0, ymax=0.5, c='red')
sub4.axvline(x=3.0, ymax=0.75, c='red')
sub4.axvline(x=4.0, ymax=0.5, c='red')

plt.savefig('edged_square.png')
