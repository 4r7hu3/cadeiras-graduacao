
prod1 = "macarrão"
prod2 = "sal"
prod3 = "borracha para panela de pressão"
prod4 = "desentupidor para fogão a gás"
prod5 = "cuscuzeira"

pre1 = 5.39
pre2 = 2.50
pre3 = 10.00
pre4 = 7.12
pre5 = 8.88

preco1 = "{:.2f}".format(pre1)
preco2 = "{:.2f}".format(pre2)
preco3 = "{:.2f}".format(pre3)
preco4 = "{:.2f}".format(pre4)
preco5 = "{:.2f}".format(pre5)

print("{}{}".format(prod1, preco1.rjust(40-len(prod1), '.')))
print("{}{}".format(prod2, preco2.rjust(40-len(prod2), '.')))
print("{}{}".format(prod3, preco3.rjust(40-len(prod3), '.')))
print("{}{}".format(prod4, preco4.rjust(40-len(prod4), '.')))
print("{}{}".format(prod5, preco5.rjust(40-len(prod5), '.')))


