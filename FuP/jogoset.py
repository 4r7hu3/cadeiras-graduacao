import random

Cores = ['vermelho', 'verde   ', 'azul    ']
Preenchimentos = ['vazado   ', 'hachurado', 'sólido   ']
Formas = ['D', 'O', 'S']

Baralho = []

# Criar todas as cartas e colocá-la em uma lista
for numero in range(1,4):
    for forma in Formas:
        for cor in Cores:
            for pre in Preenchimentos:
                carta = (numero, forma, cor, pre)
                Baralho.append( carta )

# Embaralhar cartas
random.shuffle(Baralho)

# Separação entre cartas da mesa e aquelas que
# ainda irão para a mesa
Mesa = Baralho[:12]
Sobra = Baralho[12:]

# Mostrar as cartas que estão na mesa
print('Mesa:')
for carta in Mesa:
    print(carta)
