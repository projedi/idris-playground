module autoimplicit

myCast : {auto prf : x = y} -> x -> y
myCast {prf=refl} x = x
