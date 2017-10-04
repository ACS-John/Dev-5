00010 ! Replace test\txt
00020 ! _____ top of program functions ________
00030   library 'S:\Core\Library': fntop,fntxt,fntos,fnacs,fncmdset
00040   dim text$*50,response$(15)*40,cap$*128
00050   fntop("Test\txt",cap$="Test Txt")
00060 ! ________ top of screen stuff __________
00070   fntos(sn$='testtxt')
00080 ! fnTxt(Line, Pos, MyLen; MaxLen, Align, &Mask$, Disable, &ToolTipText$, Container, TabCon, AddToMask$*40) : Response$(rc+=1)=“Default Answer”
00090   fntxt(5,5,10,0,0,"71")
00100   fncmdset(2)
00110   fnacs(sn$, 0, mat response$, ck)
