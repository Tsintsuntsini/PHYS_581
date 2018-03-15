# FFT Assignment 2: Part 3
# ==========================
import matplotlib.pyplot as plt
import numpy as np


def autocorrelation(x):
    """Calculates the autocorrelation of an array.

    The autocorrelation is calculated using the equation
    \begin{equation}
      \begin{aligned}
        AC(k)
        &= \frac{1}{(N - k) \sigma^2}
           \sum_{n=1}^{N-k} (x_n - \bar{x}) (x_{n+k} - \bar{x}) \\
        &= \frac{\sum_{n=1}^{N-k} (x_n - \bar{x}) (x_{n+k} - \bar{x})}
                {\sum_{n=1}^{N-k} (x_n - \bar{x})^2}
      \end{aligned}
    \end{equation}

    Parameters
    ------------
    x : (1D np.array or list) vector of values.

    Returns
    ---------
    The autocorrelation vector of the same size as the input vector.
    """
    def ac(x, k):
        size = x.size
        mean = np.mean(x)

        upper = np.sum((x[:size-k] - mean) * (x[k:] - mean))
        lower = np.sum((x[:size-k] - mean) * (x[:size-k] - mean))
        return upper / lower

    try:
        x = np.array(x)
    except Exception:
        raise ValueError('Input should be a list or numpy array')

    if len(x.shape) != 1:
        raise TypeError(
            """Input should be an one-dimensional list or array, given array
            has dimension {}""".format(x.shape)
         )

    return np.array([ac(x, k) for k in range(0, x.size)])


def power_spectrum(x):
    return np.abs(np.fft.fftshift(np.fft.fft(x)))**2


# 2.2 Heart Beats
# -----------------
with open('phys581-beats.txt', 'r') as file:
    beats = np.array(list(map(float, file.readlines())))

f_s = 125.0
freq = np.fft.fftshift(np.fft.fftfreq(beats.size, 1 / f_s))
beats_fft = np.fft.fftshift(np.fft.fft(beats))

plt.title('Frequency Spectrum of Heart Beats')
plt.xlabel('Frequency')
plt.ylabel('Amplitude')
plt.xlim(0, +20)
plt.yscale('log')
plt.plot(freq[int(freq.size / 2):], np.abs(beats_fft)[int(freq.size / 2):])
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
freq = np.fft.fftshift(np.fft.fftfreq(months.size))
sandp, ford, gm, microsoft, sun, ustb3m = stocks

fig, subs = plt.subplots(nrows=2, figsize=(8, 14), dpi=300)
subs[0].set_title('SandP Stock Prices from 2002-2007')
subs[0].set_xlabel('Time')
subs[0].set_ylabel('Stock Price')
subs[0].set_xlim(0, 65)
subs[0].set_xticks(range(0, months.size, 12))
subs[0].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
#subs[0].tick_params(axis='x', rotation=45)
subs[0].tick_params(axis='x', tickdir=45)
subs[0].plot(months, sandp)
subs[0].grid()

subs[1].set_title('Company Stocks from 2002-2007')
subs[1].set_xlabel('Time')
subs[1].set_ylabel('Stock Price')
subs[1].set_xlim(0, 65)
subs[1].set_ylim(0, 100)
subs[1].set_xticks(range(0, months.size, 12))
subs[1].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
#subs[1].tick_params(axis='x', rotation=45)
subs[1].tick_params(axis='x', tickdir=45)
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
months_ret = np.arange(0, len(returns[0]))
freq_ret = np.fft.fftshift(np.fft.fftfreq(months_ret.size))
sandp_ret, ford_ret, gm_ret, microsoft_ret, sun_ret, ustb3m_ret = returns

fig, subs = plt.subplots(nrows=2, ncols=3, figsize=(14, 14), dpi=300)

subs[0][0].set_title('SandP')
subs[0][0].set_xlabel('Months')

subs[0][0].set_ylabel('Continuously Compounded Returns')
subs[0][0].set_xlim(0, 65)
subs[0][0].set_xticks(range(0, months.size, 12))
subs[0][0].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
#subs[0][0].tick_params(axis='x', rotation=45)
subs[0][0].tick_params(axis='x', tickdir=45)
subs[0][0].plot(months_ret, sandp_ret)
subs[0][0].grid()

subs[0][1].set_title('Ford')
subs[0][1].set_xlabel('Months')
subs[0][1].set_xlim(0, 65)
subs[0][1].set_xticks(range(0, months.size, 12))
subs[0][1].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
#subs[0][1].tick_params(axis='x', rotation=45)
subs[0][1].tick_params(axis='x', tickdir=45)
subs[0][1].plot(months_ret, ford_ret)
subs[0][1].grid()

subs[0][2].set_title('GM')
subs[0][2].set_xlabel('Months')
subs[0][2].set_xlim(0, 65)
subs[0][2].set_xticks(range(0, months.size, 12))
subs[0][2].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
#subs[0][2].tick_params(axis='x', rotation=45)
subs[0][2].tick_params(axis='x', tickdir=45)
subs[0][2].plot(months_ret, gm_ret)
subs[0][2].grid()

subs[1][0].set_title('Mircosoft')
subs[1][0].set_xlabel('Months')
subs[1][0].set_ylabel('Continuously Compounded Returns')
subs[1][0].set_xlim(0, 65)
subs[1][0].set_xticks(range(0, months.size, 12))
subs[1][0].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
#subs[1][0].tick_params(axis='x', rotation=45)
subs[1][0].tick_params(axis='x', tickdir=45)
subs[1][0].plot(months_ret, microsoft_ret)
subs[1][0].grid()

subs[1][1].set_title('Sun')
subs[1][1].set_xlabel('Months')
subs[1][1].set_xlim(0, 65)
subs[1][1].set_xticks(range(0, months.size, 12))
subs[1][1].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
#subs[1][1].tick_params(axis='x', rotation=45)
subs[1][1].tick_params(axis='x', tickdir=45)
subs[1][1].plot(months_ret, sun_ret)
subs[1][1].grid()

subs[1][2].set_title('USTB3M')
subs[1][2].set_xlabel('Months')
subs[1][2].set_xlim(0, 65)
subs[1][2].set_xticks(range(0, months.size, 12))
subs[1][2].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
#subs[1][2].tick_params(axis='x', rotation=45)
subs[1][2].tick_params(axis='x', tickdir=45)
subs[1][2].plot(months_ret, ustb3m_ret)
subs[1][2].grid()

plt.savefig('stocks_returns')

# Part c: autocorrelation
# - - - - - - - - - - - - -
fig, subs = plt.subplots(nrows=2, ncols=3, figsize=(14, 14), dpi=300)

subs[0][0].set_title('SandP')
subs[0][0].set_xlabel('Months')
subs[0][0].set_ylabel('Autocorrelation Value')
subs[0][0].set_xlim(0, 65)
subs[0][0].set_xticks(range(0, months.size, 12))
subs[0][0].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
#subs[0][0].tick_params(axis='x', rotation=45)
subs[0][0].tick_params(axis='x', tickdir=45)
subs[0][0].plot(months, autocorrelation(sandp))
subs[0][0].grid()

subs[0][1].set_title('Ford')
subs[0][1].set_xlabel('Months')
subs[0][1].set_xlim(0, 65)
subs[0][1].set_xticks(range(0, months.size, 12))
subs[0][1].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
#subs[0][1].tick_params(axis='x', rotation=45)
subs[0][1].tick_params(axis='x', tickdir=45)
subs[0][1].plot(months, autocorrelation(ford))
subs[0][1].grid()

subs[0][2].set_title('GM')
subs[0][2].set_xlabel('Months')
subs[0][2].set_xlim(0, 65)
subs[0][2].set_xticks(range(0, months.size, 12))
subs[0][2].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
#subs[0][2].tick_params(axis='x', rotation=45)
subs[0][2].tick_params(axis='x', tickdir=45)
subs[0][2].plot(months, autocorrelation(gm))
subs[0][2].grid()

subs[1][0].set_title('Mircosoft')
subs[1][0].set_xlabel('Months')
subs[1][0].set_ylabel('Autocorrelation Value')
subs[1][0].set_xlim(0, 65)
subs[1][0].set_xticks(range(0, months.size, 12))
subs[1][0].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
#subs[1][0].tick_params(axis='x', rotation=45)
subs[1][0].tick_params(axis='x', tickdir=45)
subs[1][0].plot(months, autocorrelation(microsoft))
subs[1][0].grid()

subs[1][1].set_title('Sun')
subs[1][1].set_xlabel('Months')
subs[1][1].set_xlim(0, 65)
subs[1][1].set_xticks(range(0, months.size, 12))
subs[1][1].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
#subs[1][1].tick_params(axis='x', rotation=45)
subs[1][1].tick_params(axis='x', tickdir=45)
subs[1][1].plot(months, autocorrelation(sun))
subs[1][1].grid()

subs[1][2].set_title('USTB3M')
subs[1][2].set_xlabel('Months')
subs[1][2].set_xlim(0, 65)
subs[1][2].set_xticks(range(0, months.size, 12))
subs[1][2].set_xticklabels([2002, 2003, 2004, 2005, 2006, 2007])
#subs[1][2].tick_params(axis='x', rotation=45)
subs[1][2].tick_params(axis='x', tickdir=45)
subs[1][2].plot(months, autocorrelation(ustb3m))
subs[1][2].grid()

plt.savefig('stocks_ac')

# Part d: power spectrum
# - - - - - - - - - - - -
fig, subs = plt.subplots(nrows=2, ncols=3, sharey=True, figsize=(14, 12), dpi=300)

subs[0][0].set_title('SandP')
subs[0][0].set_xlabel('Frequency')
subs[0][0].set_ylabel('Power')
subs[0][0].set_yscale('log')
subs[0][0].plot(freq_ret, power_spectrum(sandp_ret))
subs[0][0].grid()

subs[0][1].set_title('Ford')
subs[0][1].set_xlabel('Frequency')
subs[0][1].set_yscale('log')
subs[0][1].plot(freq_ret, power_spectrum(ford_ret))
subs[0][1].grid()

subs[0][2].set_title('GM')
subs[0][2].set_xlabel('Frequency')
subs[0][2].set_yscale('log')
subs[0][2].plot(freq_ret, power_spectrum(gm_ret))
subs[0][2].grid()

subs[1][0].set_title('Mircosoft')
subs[1][0].set_xlabel('Frequency')
subs[1][0].set_ylabel('Power')
subs[1][0].set_yscale('log')
subs[1][0].plot(freq_ret, power_spectrum(microsoft_ret))
subs[1][0].grid()

subs[1][1].set_title('Sun')
subs[1][1].set_xlabel('Frequency')
subs[1][1].set_yscale('log')
subs[1][1].plot(freq_ret, power_spectrum(sun_ret))
subs[1][1].grid()

subs[1][2].set_title('USTB3M')
subs[1][2].set_xlabel('Frequency')
subs[1][2].set_yscale('log')
subs[1][2].plot(freq_ret, power_spectrum(ustb3m_ret))
subs[1][2].grid()

plt.savefig('stocks_power_spectrum')


# Part e: dow jones
# - - - - - - - - - -
with open('phys581-dow.txt', 'r') as file:
    dow = np.array(list(map(float, file.readlines())))

t = np.arange(0, dow.size)
freq = np.fft.fftshift(np.fft.fftfreq(t.size))

fig, subs = plt.subplots(ncols=2, figsize=(12, 8), dpi=300)

subs[0].set_title('Dow Jones from 2006-2010')
subs[0].set_xlabel('Time (Days)')
subs[0].set_ylabel('Average Stock Price')
subs[0].plot(t, dow)

subs[1].set_title('Power Spectrum of Dow Jones')
subs[1].set_xlabel('Frequency')
subs[1].set_ylabel('Power')
subs[1].set_yscale('log')
subs[1].plot(freq, power_spectrum(dow))

plt.savefig('dow')

# Part f: autocorrelation
# - - - - - - - - - - - - -
plt.figure(dpi=300)

plt.title('Autocorrelation of Dow Jones')
plt.xlabel('Time')
plt.ylabel('Autocorrelation Value')
plt.plot(t, autocorrelation(dow))

plt.savefig('dow_ac')
