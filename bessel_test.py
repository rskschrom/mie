from scipy.special import jv, digamma, yv
import numpy as np

# define domain
nx = 10
maxx = 10.
x = np.linspace(maxx/nx, maxx, nx)
j = jv(5., x)
y = yv(1., x)

for i in range(nx):
    print x[i], j[i], y[i]

# test digamma function
print digamma(10.)
