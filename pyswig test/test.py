# -*- coding: utf-8 -*-

from pyswip.prolog import *
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
		
		
print
print
print "aca esta la papa", list(prolog.query("heavy(X)"))
for sol in prolog.query("heavy(X)"):
	print type(sol['X']),sol['X'].args
	for x in sol['X'].args[0]:
		print x
		
	if sol['X'].name=="b":
		print "asd",type(sol['X'].args)
	
#prolog.query(":-dynamic action/1")
list(prolog.query("retract(action(_))"))
list(prolog.query("assertz(action(skip))"))
for sol in prolog.query("action(X)"):
	print type(sol['X']),sol['X']
# print  bool(prolog.query("asserteameEsta(skip)"))
# print  prolog.query("action(X)").next()['X']
	
		

# assertz = Functor("assert", 1)
# retract = Functor("retract", 1)
# father = Functor("father", 2)
# padre = Functor("padre",2)
# Res = Variable()
# call(assertz(father("a","b")))
# q = Query(father("x","b"))
# print u"debería dar false", q.nextSolution()
# call(retract(father("_","_")))
# call(assertz(father("x","b")))
# q = Query(father("x","b"))
# print u"debería dar true", q.nextSolution()
# q = Query(padre("homero",Res))
# print u"debería dar lisa", q.nextSolution(), Res.value
# q = Query(padre("homero",Res))
# print u"debería dar lisa", q.nextSolution()

