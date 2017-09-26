00010 ! Replace test\txtDisable
00020 ! _____ top of program functions ________
00030   library 'S:\Core\Library': fntop,fntxt,fntos,fnacs,fncmdset
00040   dim text$*50,response$(15)*40,cap$*128
00050   let fntop("Test\TxtDisable",cap$="Test Disabled Text")
00060 ! ________ top of screen stuff __________
00070   let fntos(sn$='testtxt')
00080 ! fnTxt(Line, Pos, MyLen; MaxLen, Align, &Mask$, Disable, &ToolTipText$, Container, TabCon, AddToMask$*40) : Response$(rc+=1)=“Default Answer”
00090   let fntxt(5,5,10,0,0,"",1)
00100   let fncmdset(2)
00110   let fnacs(sn$, 0, mat response$, ck)
