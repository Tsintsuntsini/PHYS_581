# Part 1: Selected Topics
# =========================
import matplotlib.pyplot as plt
import numpy as np

# 1.2: Power Spectrum and Filtering
# -----------------------------------
def wave(t, amp=1, freq=1, phase=0):
    return amp * np.sin(2 * np.pi * freq * t - phase)

def hann_window():
    return None

def blackmann_harris_window():
    return None

# part a: proof
# - - - - - - - -
# The proof is in the report

# part b: first plot
# - - - - - - - - - -
t = np.arange(0, 250)
freq = np.fft.fftfreq(t.size, t[1] - t[0])
samples_0 = wave(t, amp=10, freq=60)
power_0 = t.size / 2 * np.abs(np.fft.fft(samples_0))**2

fig, subs = plt.subplot(nrows=2, dpi=300)

subs[0].plot(freq, power_0)

# part c: second plot
# - - - - - - - - - - -
t = np.arange(0, 250)
freq = np.fft.fftfreq(t.size, t[1] - t[0])
samples_1 = wave(t, amp=10, freq=59.673)
power_1 = t.size / 2 * np.abs(np.fft.fft(samples_1))**2

subs[1].plot(freq, power_1)

plt.savefig('power_spectrum')

# part d: hann window
# - - - - - - - - - - -
# plot hann window and its fft
plt.plot(t, hann(t))

# part e: apply on part b
# - - - - - - - - - - - - -
hann_0 = hann(samples_0)
plt.plot(t, hann_0)

# part f: apply on part c
# - - - - - - - - - - - - -
hann_1 = hann(samples_1)
plt.plot(freq, np.fft.fft(hann_1)
