import random
import math


N = 5000
x_0 = random.gauss(10,100)
i = 1

samples = []
samples.append(x_0)

def p(x):
    return (0.3 * math.exp( (-0.2 * x * x) )) + (0.7 * math.exp( (-0.2 * (x-10)**2 )))

x = x_0
while i <= N:
    u = random.random()
    x_star = random.gauss(x, 100)
    if (u < min(1, p(x_star)/p(x))):
        x = x_star
    samples.append(x)
    i+=1

# print samples

plt.hist(samples, bins=100, range=(-10,20))
plt.savefig('Histogram.png')
plt.close()


