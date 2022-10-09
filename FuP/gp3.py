word = input("Digite uma frase ou palavra: ")
word = word.lower()
vet = list(word)
tests = [' ', ',', '.', '-', '?', '!']

print("")

verif = []

for i in vet:
	for j in tests:
		if j in vet:
			vet.remove(j)
	verif.append(i)

verif.reverse()

if verif == vet:
	print("Você encontrou um palíndromo :)")
else:
	print("Você não encontrou um palíndromo ;(")
