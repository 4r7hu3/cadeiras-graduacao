
entrada = open('times.txt', 'r')

Times = []
for i in range(16):
    lin1 = entrada.readline()
    lin2 = entrada.readline()
    lin3 = entrada.readline()
    lin4 = entrada.readline()

    lin1 = lin1[:-1]
    lin2 = lin2[:-1]
    lin3 = lin3[:-1]
    lin4 = lin4[:-1]

    time = (lin1, lin2, int(lin3), lin4)

    print(time)
    
    Times.append(time)

entrada.close()