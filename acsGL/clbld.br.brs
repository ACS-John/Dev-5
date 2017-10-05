00010 ! Replace S:\acsGL\CLBld
00020 ! Create Checkbook System Files
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fntos,fnlbl,fntxt,fncomboa,fnacs,fncmdkey,fngethandle
00050   fntop(program$,cap$="Create Checkbook System Files")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim cnam$*40,dat$*20,de$*50,vn$(4)*30 ,ta(2),cap$*128,io2$(2),wrd2$(2)*38
00090   dim resp$(4)*50,option$(2)*25,contact$*30,email$*50,myact$*20
00100 ! ______________________________________________________________________
00110   fncno(cno,cnam$)
00150 ! ______________________________________________________________________
00160   dv$="A"
00170 MENU1: ! 
00180   fntos(sn$="ClBld") !:
        mylen=50: mypos=mylen+3 : right=1
00190   fnlbl(1,1,"Insert Blank Formatted Diskette In Selected Drive:",mylen,right)
00200   fntxt(1,mypos,1,0,right,"",0,"The information needs to be placed on a diskette.  If you do not have a diskette drive, use your C: drive and transfer the information to a CD.",0 ) !:
        resp$(1)= dv$
00210   option$(1)="Build G/L Master File" !:
        option$(2)="Build Payee File"
00220   fncomboa("TypeOfFile",3,25,mat option$,"You must indicate the type of entry you will be entering.",25)
00230   resp$(2)=str$(sel)
00240   fncmdkey("&Next",1,1,0,"Allows you to enter transactions.")
00250   fncmdkey("&Cancel",5,0,1,"Returns to menu without transferring files.")
00260   fnacs(sn$,0,mat resp$,ckey)
00270   if ckey=5 then goto XIT
00280   dv$=resp$(1)
00290   if trim$(resp$(2))="Build G/L Master File" then ti1=1 else ti1=2
00300   dv$=dv$&":"
00310   on ti1 goto L330,END1
00320 ! ______________________________________________________________________
00330 L330: open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00340   open #2: "Name="&dv$&"GLmstr.h"&str$(cno)&",SIZE=0,RecL=62,Replace",internal,output 
00350 L350: read #1,using L360: gl$,de$ eof END1
00360 L360: form pos 1,c 12,c 50
00370   write #2,using L360: gl$,de$
00380   goto L350
00390 ! ______________________________________________________________________
00400 END1: close #1: ioerr L410
00410 L410: close #2: ioerr L420
00420 L420: open #2: "Name="&dv$&"PAYMSTR.h"&str$(cno)&",SIZE=0,RecL=276,Replace",internal,output 
00430   open #payeegl:=fngethandle: "Name="&env$('Q')&"\CLmstr\PayeeGLBreakdown.h"&str$(cno)&",Version=1,KFName="&env$('Q')&"\CLmstr\Payeeglbkdidx.h"&str$(cno)&",Use,RecL=56,KPs=1,KLn=8,Shr",internal,outin,keyed 
00440   open #1: "Name="&env$('Q')&"\GLmstr\paymstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\payidx1.h"&str$(cno)&",Shr",internal,input,keyed  ! Ioerr 580
00450 READ_GL1099: ! 
00460   read #1,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20': vn$,mat vn$,typ,ss$,ph$,contact$,email$,fax$,myact$ eof EOF_GL1099
00470   write #2,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20': vn$,mat vn$,typ,ss$,ph$,contact$,email$,fax$,myact$
00480   goto READ_GL1099
00490 ! ______________________________________________________________________
00500 EOF_GL1099: ! 
00510   close #1: 
00520   open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno)&",Shr",internal,input,keyed ioerr L410
00530   vn$(3)=""
00540 L540: read #1,using 'Form POS 1,N 4,3*C 25,C 11,PD 5.2': eno,vn$(1),vn$(2),vn$(4),ss$,ytdp eof L580
00550   write #2,using 'Form POS 1,G 8,4*C 30,PD 5.2,N 2,C 11': eno,mat vn$,ytdp,0,ss$
00560   goto L540
00570 ! ______________________________________________________________________
00580 L580: close #1: 
00590   close #2: 
00600   execute "Copy "&env$('Q')&"\GLmstr\PayeeGLBreakdown.h"&str$(cno)&" a:"
00610   goto XIT
00620 ! ______________________________________________________________________
00630 XIT: fnxit
00640 ! ______________________________________________________________________
00650 ! <Updateable Region: ERTN>
00660 ERTN: fnerror(program$,err,line,act$,"xit")
00670   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00680   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00690   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00700 ERTN_EXEC_ACT: execute act$ : goto ERTN
00710 ! /region
00720 ! ______________________________________________________________________
