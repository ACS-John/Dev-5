library 'S:\Core\Library': fntop,fnxit, fnerror,fnhamsterfio
on error goto ERTN
fntop(program$)
fnhamsterfio(env$('cursys')&' '&program$(pos(program$,'\',-1)+1:pos(program$,'.')-1))
XIT: fnxit
include: ertn