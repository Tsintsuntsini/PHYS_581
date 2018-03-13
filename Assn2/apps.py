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
plt.xlim(-20, +20)
plt.yscale('log')
plt.plot(freq, np.abs(beats_fft))
plt.savefig('beats')
plt.clf()


# 2.4 Application to Financial Series
# -------------------------------------

# Part a: time series
# - - - - - - - - - - -
with open('phys581-stocks.txt', 'r') as file:
    stocks = [line.split() for line in file.readlines()[1:]]

stocks = np.array(list(map(list, zip(*stocks)))[1:], dtype=float)
months = np.arange(0, len(stocks[0]))
sandp, ford, gm, microsoft, sun, ustb3m = stocks

fig, subs = plt.subplots(nrows=2, figsize=(8, 14), dpi=300)
subs[0].set_title('SandP Stock Prices from 2002-2007')
subs[0].set_xlabel('Time')
subs[0].set_ylabel('Stock Price')
subs[0].set_xlim(0, 65)
subs[0].set_xticks(range(0, months.size, 12))
subs[0].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
subs[0].tick_params(axis='x', rotation=45)
subs[0].plot(months, sandp)
subs[0].grid()
subs[1].set_title('Company Stocks from 2002-2007')
subs[1].set_xlabel('Months')
subs[1].set_ylabel('Stock Price')
subs[1].set_xlim(0, 65)
subs[1].set_ylim(0, 100)
subs[1].set_xticks(range(0, months.size, 12))
subs[1].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
subs[1].tick_params(axis='x', rotation=45)
subs[1].plot(months, ford, label='Ford')
subs[1].plot(months, gm, label='GM')
subs[1].plot(months, microsoft, label='Microsoft')
subs[1].plot(months, sun, label='Sun')
subs[1].plot(months, ustb3m, label='USTB3M')
subs[1].grid()
subs[1].legend(loc='best')
plt.savefig('stocks_time')

# Part b: compounded returns
# - - - - - - - - - - - - - -
returns = np.array(
    [np.log(stocks[:,i] / stocks[:,i-1]) for i in range(1, months.size)]
).T
months = np.arange(0, len(returns[0]))
sandp, ford, gm, microsoft, sun, ustb3m = returns

fig, subs = plt.subplots(nrows=2, ncols=3, figsize=(14, 14), dpi=300)

subs[0][0].set_title('SandP')
subs[0][0].set_xlabel('Months')
subs[0][0].set_ylabel('Continuously Compounded Returns')
subs[0][0].set_xlim(0, 65)
subs[0][0].set_xticks(range(0, months.size, 12))
subs[0][0].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
subs[0][0].tick_params(axis='x', rotation=45)
subs[0][0].plot(months, sandp)
subs[0][0].grid()

subs[0][1].set_title('Ford')
subs[0][1].set_xlabel('Months')
subs[0][1].set_xlim(0, 65)
subs[0][1].set_xticks(range(0, months.size, 12))
subs[0][1].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
subs[0][1].tick_params(axis='x', rotation=45)
subs[0][1].plot(months, ford)
subs[0][1].grid()

subs[0][2].set_title('GM')
subs[0][2].set_xlabel('Months')
subs[0][2].set_xlim(0, 65)
subs[0][2].set_xticks(range(0, months.size, 12))
subs[0][2].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
subs[0][2].tick_params(axis='x', rotation=45)
subs[0][2].plot(months, gm)
subs[0][2].grid()

subs[1][0].set_title('Mircosoft')
subs[1][0].set_xlabel('Months')
subs[1][0].set_ylabel('Continuously Compounded Returns')
subs[1][0].set_xlim(0, 65)
subs[1][0].set_xticks(range(0, months.size, 12))
subs[1][0].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
subs[1][0].tick_params(axis='x', rotation=45)
subs[1][0].plot(months, microsoft)
subs[1][0].grid()

subs[1][1].set_title('Sun')
subs[1][1].set_xlabel('Months')
subs[1][1].set_xlim(0, 65)
subs[1][1].set_xticks(range(0, months.size, 12))
subs[1][1].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
subs[1][1].tick_params(axis='x', rotation=45)
subs[1][1].plot(months, sun)
subs[1][1].grid()

subs[1][2].set_title('USTB3M')
subs[1][2].set_xlabel('Months')
subs[1][2].set_xlim(0, 65)
subs[1][2].set_xticks(range(0, months.size, 12))
subs[1][2].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
subs[1][2].tick_params(axis='x', rotation=45)
subs[1][2].plot(months, ustb3m)
subs[1][2].grid()

plt.savefig('stocks_returns')

# Part c: autocorrelation
# - - - - - - - - - - - - -

# Part d: power spectrum
# - - - - - - - - - - - -

# Part e: dow jones
# - - - - - - - - - -
with open('phys581-dow.txt', 'r') as file:
    dow = np.array(list(map(float, file.readlines())))

t = np.arange(0, dow.size)
freq = np.fft.fftfreq(t.size)
dow_fft = np.abs(np.fft.fft(dow))**2

fig, subs = plt.subplots(ncols=2)

subs[0].plot(t, dow)

subs[1].plot(freq, dow_fft)
subs[1].set_yscale('log')

plt.savefig('dow')

# Part f: autocorrelation
# - - - - - - - - - - - - -
