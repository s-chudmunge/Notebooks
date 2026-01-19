import numpy as np
import matplotlib.pyplot as plt

# Read data from output files
data_single = np.loadtxt('output_single_precision.txt')
data_double = np.loadtxt('output_double_precision.txt')

# Extracting N and corresponding differences
N_single, diff_single = data_single[:, 0], data_single[:, 1]
N_double, diff_double = data_double[:, 0], data_double[:, 1]

# Plotting
plt.figure(figsize=(10, 6))

# Plotting single precision results
plt.loglog(N_single, diff_single, label='Single Precision', linestyle='-', marker='o', color='blue')

# Plotting double precision results
plt.loglog(N_double, diff_double, label='Double Precision', linestyle='-', marker='s', color='red')

# Adding labels and title
plt.xlabel('N')
plt.ylabel('|S_up - S_down| / (S_up + S_down)')
plt.title('Difference between Series vs. N (log-log)')

# Adding legend
plt.legend()

# Showing grid
plt.grid(True)

# Showing the plot
plt.show()

