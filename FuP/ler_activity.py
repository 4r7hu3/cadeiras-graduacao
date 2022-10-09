
arq = open('activity_2338606858.tcx', 'r')
vet = arq.readlines()

for x in range(len(vet)):

  if vet[x].startswith('            <HeartRateBpm>'):
    value = vet[x+1]
    value = value[len('              <Value>'):]
    L = value.split('</Value>')
    print(L[0])
