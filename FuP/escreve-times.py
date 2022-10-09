
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

#---------------------------------------------

ArqTimesUEFA = open('times.txt', 'w')

for time in Times:
    ArqTimesUEFA.write(time['nome'] + '\n')
    ArqTimesUEFA.write(time['grupo'] + '\n')
    ArqTimesUEFA.write(str(time['posicao']) + '\n')
    ArqTimesUEFA.write(time['pais'] + '\n')
   
ArqTimesUEFA.close()


