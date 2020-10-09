 
	autoLibrary
	on error goto Ertn
 
	dim message$(4)*400,cap$*128
! note message$(4) is not dimmed long enough
 
	fnTop(prg$="Test\msgbox",cap$="Test fnMsgBox")
	for mt=0 to 5
		message$(1)="This is my message" : _
		message$(2)="It can be many lines long" : _
		message$(3)="This is " : _
		message$(4)="This is j  - this is a very long line - much longer than previous lines it's a real whopper - i mean it man - big"
! mt=0+256 : _
		fnmsgbox(mat message$, response$, cap$, mt)
		pr "The answer is "&response$
	next mt
Xit: stop
 
include: ertn
 
