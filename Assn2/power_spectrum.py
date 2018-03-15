# Part 1: Selected Topics
# =========================
import matplotlib.pyplot as plt
import numpy as np


def power_spectrum(x):
    return np.abs(np.fft.fftshift(np.fft.fft(x)))**2


# 1.2: Power Spectrum and Filtering
# -----------------------------------
def wave(t, amp=1, freq=1, phase=0):
    return amp * np.sin(2 * np.pi * freq * t - phase)

def hann_window(size):
    return 0.5 * (1 - np.cos(2 * np.pi * np.arange(0, size) / (size - 1)))

def blackmann_harris_window(size):
    t = 2 * np.pi * np.arange(0, size) / (size - 1)
    window = 0.35875 - 0.48829 * np.cos(t) + 0.14128 * np.cos(2 * t) \
             - 0.01168 * np.cos(3 * t)
    return window

# part a: proof
# - - - - - - - -
# The proof is in the report

# part b: first plot
# - - - - - - - - - -
t = np.arange(0, 250) / 240.0
freq = np.fft.fftshift(np.fft.fftfreq(t.size, t[1] - t[0]))

samples_0 = wave(t, amp=10, freq=60)
power_0 = power_spectrum(samples_0)

fig, subs = plt.subplots(nrows=2, figsize=(8, 10), dpi=300)

subs[0].set_title('Power Spectrum of Sine at $60$Hz')
subs[0].set_xlabel('Frequency')
subs[0].set_ylabel('Power')
subs[0].set_yscale('log')
subs[0].plot(freq, power_0)

# part c: second plot
# - - - - - - - - - - -
samples_1 = wave(t, amp=10, freq=59.673)
power_1 = power_spectrum(samples_1)

subs[1].set_title('Power Spectrum of Sine at $59.673$Hz')
subs[1].set_xlabel('Frequency')
subs[1].set_ylabel('Power')
subs[1].set_yscale('log')
subs[1].plot(freq, power_1)

plt.savefig('power_spectrum')

# part d: hann window
# - - - - - - - - - - -
hann = hann_window(t.size)

fig, subs = plt.subplots(nrows=2, figsize=(8, 10), dpi=300)

subs[0].set_title('Hann Window')
subs[0].set_xlabel('Time')
subs[0].set_ylabel('Amplitude')
subs[0].plot(t, hann)

subs[1].set_title('DFT of Hann Window')
subs[1].set_xlabel('Frequency')
subs[1].set_ylabel('Amplitude')
subs[1].set_yscale('log')
subs[1].plot(freq[1:], power_spectrum(hann)[1:])

plt.savefig('hann_window')

# part e: apply on part b
# - - - - - - - - - - - - -
hann_0 = samples_0 * hann_window(samples_0.size)

fig, subs = plt.subplots(nrows=2, figsize=(8, 10), dpi=300)

subs[0].set_title('Hann Windowed Sine at $60$Hz')
subs[0].set_xlabel('Time')
subs[0].set_ylabel('Amplitude')
subs[0].plot(t, hann_0)

subs[1].set_title('DFT of Hann Windowed Sine at $60$Hz')
subs[1].set_xlabel('Frequency')
subs[1].set_ylabel('Amplitude')
subs[1].set_yscale('log')
subs[1].plot(freq, power_spectrum(hann_0))

plt.savefig('hann_0')

# part f: apply on part c
# - - - - - - - - - - - - -
hann_1 = samples_1 * hann_window(samples_1.size)

fig, subs = plt.subplots(nrows=2, figsize=(8, 10), dpi=300)

subs[0].set_title('Hann Windowed Sine at $59.673$Hz')
subs[0].set_xlabel('Time')
subs[0].set_ylabel('Amplitude')
subs[0].plot(t, hann_1)

subs[1].set_title('DFT of Hann Windowed Sine at $59.673$Hz')
subs[1].set_xlabel('Frequency')
subs[1].set_ylabel('Power')
subs[1].set_yscale('log')
subs[1].plot(freq, power_spectrum(hann_1))

plt.savefig('hann_1')

# part g: blackman-harris window
# - - - - - - - - - - - - - - - -
bh = blackmann_harris_window(t.size)
bh_0 = samples_0 * bh
bh_1 = samples_1 * bh

fig, subs = plt.subplots(nrows=2, ncols=3, figsize=(18, 10), dpi=300)

subs[0][0].set_title('Blackman-Harris Window')
subs[0][0].set_xlabel('Time')
subs[0][0].set_ylabel('Amplitude')
subs[0][0].plot(t, bh)

subs[1][0].set_title('DFT of Blackman-Harris Window')
subs[1][0].set_xlabel('Frequency')
subs[1][0].set_ylabel('Amplitude')
subs[1][0].set_yscale('log')
subs[1][0].plot(freq[1:], np.fft.fftshift(np.abs(np.fft.fft(bh)))[1:])

subs[0][1].set_title('Windowed Sine at $60$Hz')
subs[0][1].set_xlabel('Time')
subs[0][1].set_ylabel('Amplitude')
subs[0][1].plot(t, bh_0)

subs[1][1].set_title('Power Spectrum of Windowed Sine at $60$Hz')
subs[1][1].set_xlabel('Frequency')
subs[1][1].set_ylabel('Power')
subs[1][1].set_yscale('log')
subs[1][1].plot(freq, power_spectrum(bh_0))

subs[0][2].set_title('Windowed Sine at $59.673$Hz')
subs[0][2].set_xlabel('Time')
subs[0][2].set_ylabel('Amplitude')
subs[0][2].plot(t, bh_1)

subs[1][2].set_title('Power Spectrum of Windowed Sine at $59.673$Hz')
subs[1][2].set_xlabel('Frequency')
subs[1][2].set_ylabel('Power')
subs[1][2].set_yscale('log')
subs[1][2].plot(freq, power_spectrum(bh_1))

plt.savefig('blackman_harris')
