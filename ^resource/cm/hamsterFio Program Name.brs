library 'S:\Core\Library': fnTop,fnXit, fnerror,fnhamsterfio
on error goto Ertn
fnTop(program$)
fnhamsterfio(env$('cursys')&' '&program$(pos(program$,'\',-1)+1:pos(program$,'.')-1))
Xit: fnXit
include: Ertn