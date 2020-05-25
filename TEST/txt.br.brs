! Replace test\txt
! _____ top of program functions ________
	autoLibrary
	dim text$*50,response$(15)*40,cap$*128
	fnTop("Test\txt",cap$="Test Txt")
! ________ top of screen stuff __________
	fnTos(sn$='testtxt')
! fnTxt(Line, Pos, MyLen; MaxLen, Align, &Mask$, Disable, &ToolTipText$, Container, TabCon, AddToMask$*40) : Response$(rc+=1)=“Default Answer”
	fnTxt(5,5,10,0,0,"71")
	fnCmdSet(2)
	fnAcs(sn$, 0, mat response$, ck)
