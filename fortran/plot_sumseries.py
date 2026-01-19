import numpy as np
import matplotlib.pyplot as plt

# Read data from output.txt
data = np.loadtxt('output.txt', skiprows=1)  # Assuming the first row is header

# Extracting columns
N_values = data[:, 0]
seriessummation_values = data[:, 1]
sum_diff_values = data[:, 2]

# Plotting with log scales
plt.figure(figsize=(10, 6))

plt.subplot(2, 1, 1)  # Top subplot for sum_diff
plt.plot(N_values, sum_diff_values, label='sum_diff')
plt.xscale('log')
plt.yscale('log')
plt.title('Sum Difference vs N')
plt.xlabel('N (log scale)')
plt.ylabel('Sum Difference (log scale)')
plt.legend()

plt.subplot(2, 1, 2)  # Bottom subplot for seriessummation
plt.plot(N_values, seriessummation_values, label='seriessummation')
plt.xscale('log')
plt.yscale('log')
plt.title('Series Summation vs N')
plt.xlabel('N (log scale)')
plt.ylabel('Series Summation (log scale)')
plt.legend()

plt.tight_layout()
plt.show()

