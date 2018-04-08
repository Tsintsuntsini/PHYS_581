# 1.3.1 Advection equation
import numpy as np

class Advection1D:
    def __init__(self, u_0, ghost, c, dx, dt, r):
        self.u_0 = np.array(u_0)
        
        if isinstance(ghost, int):
            self.ghost = ghost
        else:
            raise TypeError('Number of ghost zones should be type int')A

        self.c = c
        self.dx = dx
        self.dt = dt
        self.r = r

    def boundary():
        pass

    def step(self, method):
        self.u_0 = getattr(self, '{}_step'.format(method))(self.u_0)
        return self.u_0

    def forward__euler_step(self, f):
        return f + 0.5 * self.r * self.centre_diff(f)

    def centre_diff(self, f):
        n = self.ghost
        f[n:-n] = (f[n+1:] - f[:-n-1]) / (2 * self.dx)
        return f

def backward_euler():
    pass

def lax_wendroff():
    pass

def crank_nicholson():
    pass

def main():
    # Create variables
    x = np.linspace(-2.0, +2.0, int(4.0 / dx))
    ts = np.linspace(0.0, 2.0, int(2.0 / dt))

    # Initial conditions for 4 cases
    case_1 = np.sin(2 * x)
    case_2 = 1.0 * (x >= 1.0)
    case_3 = (1.0 / dx) * (x == 0.0)
    case_4 = np.exp(- 4.0 * x**2)
    cases = np.array(case_1, case_2, case_3, case_4)

    # Conduct 7 trials with different parameters
    cs =  [0.5,  0.5,  0.5,    0.5,    0.5,    0.5,  0.5 ])
    dxs = [0.04, 0.02, 0.0137, 0.0101, 0.0099, 0.02, 0.02])
    dts = [0.02, 0.02, 0.02,   0.02,   0.02,   0.01, 0.04])
    rs =  [0.25, 0.5,  0.728,  0.99,   1.11,   0.25, 1.0 ])
    params = zip(cs, dxs, dts, rs)

    # Solve advection equation for all possible cases
    for u_0 in cases:
        for param in params:
            # Create different steppers
            av = Advection(u_0, 1, *param)
            fw = av
            bw = av
            lw = av
            cn = av
            for t in ts:
                fw.step('forward_euler')
                bw.step('backward_euler')
                lw.step('lax_wendroff')
                cn.step('crank_nicholson')

if __name__ == '__main__':
    main()
