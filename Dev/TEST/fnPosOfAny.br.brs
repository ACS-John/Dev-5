autoLibrary
dim textToSearch$*1024
! fnPosOfAny(textToSearch$*1024,mat searchFor$; fromEnd)

str2mat('1234567890',mat searchFor$,'')
! mat searchFor$(0)
! fnAddOneC(mat searchFor$,'0')
! fnAddOneC(mat searchFor$,'1')
! fnAddOneC(mat searchFor$,'2')
! fnAddOneC(mat searchFor$,'3')
! fnAddOneC(mat searchFor$,'4')
! fnAddOneC(mat searchFor$,'5')
! fnAddOneC(mat searchFor$,'6')
! fnAddOneC(mat searchFor$,'7')
! fnAddOneC(mat searchFor$,'8')
! fnAddOneC(mat searchFor$,'9')

textToSearch$=' dude 127 dude '
pr 'text to search is ';textToSearch$
pr 'first number is in pos ';fnPosOfAny(textToSearch$,mat searchFor$)
pr 'last number is in pos ';fnPosOfAny(textToSearch$,mat searchFor$, -1)
end