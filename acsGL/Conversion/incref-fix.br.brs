00020   library 'S:\Core\Library': fnxit,fntop
00030   dim io1$(2)
00040   let io1$(1)="11,50,N 2,U,N"
00050   fntop(program$,"CHANGE_ME")
00060   let io1$(2)="12,46,N 2,U,N"
00070   pr newpage
00080   close #101: ioerr L90
00090 L90: open #101: "SROW=10,SCOL=10,EROW=13,ECOL=52,BORDER=DR,CAPTION=FIX INCOME STATEMENT REFERENCE #'S",display,outin 
00100   pr f "11,11,C 40": "ENTER THE COMPANY # WITH GOOD NUMBERS:"
00110   pr f "12,11,C 40": "ENTER THE COMPANY # TO BE CHANGED:"
00120   pr f "14,13,C 34,R,N": "PRESS F1 TO CONTINUE OR F5 TO STOP"
00130 L130: input fields mat io1$,attr "R": cn1,cn2 conv CONV1
00140   if ce>0 then let io1$(ce)(ce1:ce2)="U": ce=0
00150   if cmdkey>0 then goto L220 else ce=curfld
00160 L160: ce=ce+1: if ce>udim(io1$) then ce=1
00170 L170: let io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1) : if ce1=0 then goto L160
00180   ce2=ce1+1 : let io1$(ce)(ce1:ce1)="UC" : goto L130
00190 CONV1: if ce>0 then let io1$(ce)(ce1:ce2)="U"
00200   ce=cnt+1
00210 ERR1: pr f "24,78,C 1": bell : goto L170
00220 L220: if cmdkey=5 then goto L350
00250   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.H"&str$(cn1)&",KFName="&env$('Q')&"\GLmstr\GLINDEX.H"&str$(cn1)&"",internal,outin,keyed 
00260   open #2: "Name="&env$('Q')&"\GLmstr\GLmstr.H"&str$(cn2)&",KFName="&env$('Q')&"\GLmstr\GLINDEX.H"&str$(cn2)&",Shr",internal,outin,keyed 
00270 L270: read #1,using L280: k$,rf4 eof END1
00280 L280: form pos 1,c 12,pos 72,pd 3
00290   rewrite #2,using L300,key=k$: rf4 nokey L270
00300 L300: form pos 72,pd 3
00310   goto L270
00320 END1: ! 
00330   close #1: 
00340   close #2: 
00350 L350: stop 
