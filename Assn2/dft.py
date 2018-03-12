# FFT Assignment


# Part 1: DFT vs. FFT
# =====================
import numpy as np


# 1.1 DFT vs. FFT
# -----------------

# Part a
# - - - -
dw = 0.1
w_max = 3.0

w = np.linspace(-w_max, +w_max, dw)
t = np.arange(0, w.size) / (2 * w_max)

# numpy normalizes by a factor of 1/N
X_w = 1.0 * (np.abs(w) <= np.pi / 3)
X_t = np.fft.irfft(X_w)

plt.title('Inverse FFT of Boxcar')
plt.xlabel('Time')
plt.ylabel('Amplitude')
plt.plot(t, X_t)
plt.savefig('boxcar_ifft')

# Part d
# - - - -
t = np.linspace(0, 10, 1000)

dc = 5.0 * np.ones(t.size)
f1 = 2.0 * np.cos(2 * np.pi * t - np.pi / 2.0)
f2 = 3.0 * np.cos(4 * np.pi * t)
f = dc + f1 + f2

print('f(t) = ', f[0:4])
print('F{f(t)} = ', np.fft.fft(f[0:4]))

plt.title('Components of a Signal')
plt.xlabel('Time')
plt.ylabel('Amplitude')
plt.plot(t, dc)
plt.plot(t, f1)
plt.plot(t, f2)
plt.plot(t, f)
plt.savefig('components')

