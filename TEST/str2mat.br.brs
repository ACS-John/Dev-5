dim line$*512
dim item$(0)*40
line$=',2,3,4,5,,7,8,,10,,'
str2mat(line$,mat item$, ',')
! str2mat(line$&' ',mat item$, ',')   ! this is a workaround
pr 'udim=';udim(mat item$)
for x=udim(mat item$)-3 to udim(mat item$)
  pr 'item$(';x;')=';item$(x)
nex x
