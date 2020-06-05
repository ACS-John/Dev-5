! Replace Test\Error.br
! test the error routine
 
	autoLibrary
	on error goto Ertn
 
! dim message$(4)*40,cap$*128
! note message$(4) is not dimmed long enough
 
	pr #0,using 'form pos 1,7*N 3,C 2,C 2': 1,2,3,4,5,6,7,"s",5,5
! fnTop(prg$="Test\Error",cap$="Test Error")
! message$(1)="This is my message"
! message$(2)="It can be asdfjfdskljfsdalkjfdsalkjsfdalkjfsdmany lines long"
! message$(3)="This is "
! message$(4)="This is j"
! mt=4
! fnmsgbox(mat message$, response$, cap$, mt)
! pr "The answer is "&response$
Xit: !
	pr "Exit Successful"
stop
 
include: Ertn

 
