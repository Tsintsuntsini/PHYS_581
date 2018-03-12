# FFT Assignment 2: Part 3
# ==========================
import matplotlib.pyplot as plt
import numpy as np


# 2.2 Heart Beats
# -----------------
with open('phys581-beats.txt', 'r') as file:
    beats = np.array(list(map(float, file.readlines())))

f_s = 125.0    # Hz
freq = np.fft.fftfreq(beats.size, 1 / f_s)
beats_fft = np.fft.fft(beats)

plt.title('Frequency Spectrum of Heart Beats')
plt.xlabel('Frequency')
plt.ylabel('Amplitude')
plt.plot(freq, np.abs(beats_fft))
plt.savefig('beats')


# 2.4 Application to Financial Series
# -------------------------------------
with open('phys581-stocks.txt', 'r') as file:
    stocks = [line.split() for line in file.readlines()[1:]]

months = np.arange(0, stocks.size)
stocks = np.array([list(map(float, line)) for line in stocks[:][1:]]).T
sandp, ford, gm, microsoft, sun, ustb3m = stocks

plt.title('Company Stocks from 2002-2007')
plt.xlabel('Months')
plt.ylabel('Stock Price')
plt.legend(loc='best')
plt.plot(months, sandp, label='SandP')
plt.plot(months, ford, label='Ford')
plt.plot(months, gm, label='GM')
plt.plot(months, microsoft, label='Microsoft')
plt.plot(months, sun, label='Sun')
plt.plot(months, ustb3m, label='USTB3M')
plt.savefig('stocks_time')

returns = np.array(
    [np.log(stocks[i,:] - stocks[i-1,:]) for i in range(len(stocks))]
)
months = np.arange(0, len(returns))
sandp, ford, gm, microsoft, sun, ustb3m = returns

fig, subs = plt.subplots(nrows=2, ncols=3, fig_size=(12, 8))
subs[0][0].set_title('SandP')
subs[0][0].set_xlabel('Months')
subs[0][0].set_ylabel('Continuously Compounded Returns')
subs[0][0].plot(months, sandp)
subs[0][1].set_title('Ford')
subs[0][1].set_xlabel('Months')
subs[0][1].set_ylabel('Continuously Compounded Returns')
subs[0][1].plot(months, ford)
subs[0][2].set_title('GM')
subs[0][2].set_xlabel('Months')
subs[0][2].set_ylabel('Continuously Compounded Returns')
subs[0][2].plot(months, gm)
subs[1][0].set_title('Mircosoft')
subs[1][0].set_xlabel('Months')
subs[1][0].set_ylabel('Continuously Compounded Returns')
subs[1][0].plot(months, mircosoft)
subs[1][1].set_title('Sun')
subs[1][1].set_xlabel('Months')
subs[1][1].set_ylabel('Continuously Compounded Returns')
subs[1][1].plot(months, sun)
subs[1][2].set_title('USTB3M')
subs[1][2].set_xlabel('Months')
subs[1][2].set_ylabel('Continuously Compounded Returns')
subs[1][2].plot(months, ustb3m)
plt.savefig('stocks_returns')
