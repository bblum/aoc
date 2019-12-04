import Data.List
f=filter
l=length
p a o x=a(uncurry o)$zip x$tail x
x=f(p all(<=))$map show[130254..678275]
main=print(l$f(p any(==))x,l$f(any((==2).l).group)x)
