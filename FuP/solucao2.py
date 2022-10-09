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
    s = '{} (g: {}, p: {})'
    return s.format(time['nome'], time['grupo'], time['pais'][:3])

# Função auxiliar para verificar se dois times pertencem ao
# mesmo grupo ou ao mesmo país.
def verificarRegras(t1, t2):
    if (t1['grupo'] == t2['grupo'] or
        t1['pais'] == t2['pais']):
        return False
    else:
        return True
#---------------------------------------------

# Flag para indicar se o sorteio gerado atende às regras
sorteioValido = False

while sorteioValido == False:

    # Separar times em duas listas: primeiros e segundos colocados
    Primeiros = []
    Segundos = []
    for time in Times:
        if time['posicao'] == 1:
            Primeiros.append( time )
        elif time['posicao'] == 2:
            Segundos.append( time )

    Sorteio = []
    sorteioValido = True
    while len(Primeiros) > 0:
            time1 = random.choice(Primeiros)
            time2 = random.choice(Segundos)

            # Verificar se o par atende às regras
            if verificarRegras(time1, time2):
                Sorteio.append((time1,time2))
                Primeiros.remove(time1)
                Segundos.remove(time2)
            # Se não atende e só possui um par sobrando,
            # não será possível concluir; vamos reiniciar
            # todo o processo.
            elif len(Primeiros) == 1:
                sorteioValido = False
                print('Opa, vou tentar de novo!')
                break

# Se chegou neste ponto, temos um sorteio válido
print('============ Sorteio válido ============')
for (i,(t1,t2)) in enumerate(Sorteio):
    print('Jogo {}:'.format(i+1))
    print('{} X {}'.format(imprime(t1), imprime(t2)))
    print()

