#chama o módulo regex
import re

#criação da expressão
# ?= checa a presença do caractere pelo menos uma vez e .* permite o caractere ser usado em qualquer lugar (sem restrições)
# . significa qualquer caractere e \W qualquer caractere não alfa numérico, e usei () para separar as expressões em grupos
pdr = '(?=.*\d)(?=.*[A-Z])(?=.*\W).{4}$'

senha = input('Digite a nova senha: ')
repeticao = input('\nRepita a senha, por favor: ')

#compara as strings
comp = re.match(pdr, senha)

#uso do método search para checagem de caracteres na string
if senha == repeticao:
	if comp:
		print("\nSenha validada!")
	else:
		print("\nSenha não validada!")
		if len(senha)<4 or len(senha)>4:
			print("\nCrie uma senha de 4 dígitos.")
		if re.search(r'[A-Z]', senha) == None:
			print("Pelo menos uma letra maiúscula.")
		if re.search(r'\d', senha) == None:
			print("Pelo menos um dígito.")
		if re.search(r'\W', senha) == None:
			print("Pelo menos um caractere especial.")
else:
  print('\nCalma, jovem, a senha repetida não confere.')