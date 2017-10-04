00020   library 'S:\Core\Library': fncno
00030   fncno(cno)
00060   dim z$*5,a$(3)*30,prg$*20,ma(20)
00070   open #255: "Name=LPT2:",display,output 
00080   open #1: "Name="&env$('Q')&"\TMmstr\CLmstr.H"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndex.H"&str$(cno)&",Shr",internal,input,keyed 
00085   do 
00090     pr newpage
00100     pr f "10,10,C 50": "Account Number to pr (Blank to Stop):"
00108 ASK: ! 
00110     input fields "10,60,c 5,UE,N": z$
00120     if rtrm$(z$)="" then stop 
00140     read #1,using "form pos 6,3*c 30",key=lpad$(rtrm$(z$),5): mat a$ nokey ASK
00160     pr #255,using "form pos 1,c 40": mat a$
00180     pr #255,using "form c 1,skip 17": ""
00200     if ln=4 then pr #255: : ln=0 else ln=ln+1
00210   loop 
