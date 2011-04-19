# -*- coding: utf-8 -*-

from pyswip.prolog import Prolog
from pyswip.easy import *


prolog = Prolog()
prolog.consult("test.pl")
for sol in list(prolog.query("list_sum([1, 2, 6], Res)")):

	print sol['Res'] #este es un int pelado
	
print "---------------"
for sol in prolog.query("suma(s(s(0)), s(0), Res)"):
	print sol['Res'].handle #supongo que será como un ID
	print type(sol['Res'].name),sol['Res'].name #string con el nombre
	print sol['Res'].arity #int con la aridad
	print sol['Res'].args #lista
	print "for:"
	for x in sol['Res'].args: #imprimo todos los argumentos del functor
		print x
print "---------------"
for sol in prolog.query("padre(homero, Res)"):
	print type(sol['Res']) #una lista es una lista (CAPTAIN OBVIOUS)
	for x in sol['Res']:
		print type(x)
		print x
		
	