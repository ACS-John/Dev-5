00010 ! Replace test\pic
00020 ! _____ top of program functions ________
00030   library 'S:\Core\Library': fntop,fntxt,fntos,fnacs,fncmdset,fnpic
00040   dim text$*50,response$(15)*40,cap$*128
00050   let fntop("Test\txt",cap$="Test Txt")
00060 ! ________ top of screen stuff __________
00070   let fntos(sn$='testtxt')
00080 ! fnTxt(Line, Pos, MyLen; MaxLen, Align, &Mask$, Disable, &ToolTipText$, Container, TabCon, AddToMask$*40) : Response$(rc+=1)=�Default Answer�
00085   let fnpic(1,1,4,30,"Logo.bmp")
00090   let fntxt(5,5,10,0,0,"2000")
00100   let fncmdset(2)
00110   let fnacs(sn$, 0, mat response$, ck)
00120 ! 
