import random

Times = []

Times.append({'nome': 'Manchester City',
              'grupo': 'A',
              'posicao': 1,
              'pais': 'Inglaterra'})
Times.append({'nome': 'Paris Saint-Germain',
              'grupo': 'A',
              'posicao': 2,
              'pais': 'França'})
Times.append({'nome': 'Liverpool',
              'grupo': 'B',
              'posicao': 1,
              'pais': 'Inglaterra'})
Times.append({'nome': 'Atlético Madrid',
              'grupo': 'B',
              'posicao': 2,
              'pais': 'Espanha'})
Times.append({'nome': 'Ajax',
              'grupo': 'C',
              'posicao': 1,
              'pais': 'Holanda'})
Times.append({'nome': 'Sporting CP',
              'grupo': 'C',
              'posicao': 2,
              'pais': 'Portugal'})
Times.append({'nome': 'Real Madrid',
              'grupo': 'D',
              'posicao': 1,
              'pais': 'Espanha'})
Times.append({'nome': 'Inter Milan',
              'grupo': 'D',
              'posicao': 2,
              'pais': 'Itália'})
Times.append({'nome': 'Bayern Munich',
              'grupo': 'E',
              'posicao': 1,
              'pais': 'Alemanha'})
Times.append({'nome': 'Benfica',
              'grupo': 'E',
              'posicao': 2,
              'pais': 'Portugal'})
Times.append({'nome': 'Manchester United',
              'grupo': 'F',
              'posicao': 1,
              'pais': 'Inglaterra'})
Times.append({'nome': 'Villarreal',
              'grupo': 'F',
              'posicao': 2,
              'pais': 'Espanha'})
Times.append({'nome': 'Lille',
              'grupo': 'G',
              'posicao': 1,
              'pais': 'França'})
Times.append({'nome': 'Red Bull Salzburg',
              'grupo': 'G',
              'posicao': 2,
              'pais': 'Áustria'})
Times.append({'nome': 'Juventus',
              'grupo': 'H',
              'posicao': 1,
              'pais': 'Itália'})
Times.append({'nome': 'Chelsea',
              'grupo': 'H',
              'posicao': 2,
              'pais': 'Inglaterra'})

def imprime(time):
    s = '{} ({},{})'
    return s.format(time['nome'],
                    time['grupo'],
                    time['pais'][:3])

#---------------------------------------------
Primeiros = []
Segundos = []

for time in Times:
    if time['posicao'] == 1:
        Primeiros.append( time )
    elif time['posicao'] == 2:
        Segundos.append( time )

if len(Primeiros) != len(Segundos):
    print('Há algo errado nos dados!')
    exit(1)

# Flag para indicar se o sorteio gerado atende às regras
sorteioValido = False

# Gerar sorteio e verificar se atende às regras
while sorteioValido == False:

    # A lista <indices> armazena um grupo de jogos:
    # a i-ésima posição indica que o primeiro
    # colocado do i-ésimo grupo jogará com o segundo
    # colocado do indices[i]-ésimo grupo.
    indices = list(range(8)) # inteiros de 0 a 7
    random.shuffle(indices)

    # Verificar país e grupo de cada par
    sorteioValido = True
    for i in indices:
        if (i == indices[i] or
            Primeiros[i]['pais'] == Segundos[indices[i]]['pais']):
            sorteioValido = False
            print('=========== Sorteio inválido ===========')
            break


print('\n============ Sorteio válido ============')
for c,i in enumerate(indices):
    print('Jogo {}: '.format(c+1), end='')
    print('{} X {}'.format(imprime(Primeiros[i]),
                           imprime(Segundos[indices[i]])))

