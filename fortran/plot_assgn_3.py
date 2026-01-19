import matplotlib.pyplot as plt

data = []
with open('output_assgn_3.txt', 'r') as file:
    for line in file:
        values = line.split()
        data.append([float(value) for value in values])

x_data = [row[0] for row in data]
y_data = [row[1] for row in data]
t_data = [row[2] for row in data]

plt.figure(figsize=(8, 6))
plt.plot(t_data, x_data, label='x vs t', color='blue')
plt.xlabel('Time')
plt.ylabel('x')
plt.title('x vs t')
plt.legend()
plt.show()

plt.figure(figsize=(8, 6))
plt.plot(t_data, y_data, label='y vs t', color='green')
plt.xlabel('Time')
plt.ylabel('y')
plt.title('y vs t')
plt.legend()
plt.show()

plt.figure(figsize=(8, 6))
plt.plot(x_data, y_data, label='x vs y', color='red')
plt.xlabel('x')
plt.ylabel('y')
plt.title('x vs y')
plt.legend()
plt.show()

