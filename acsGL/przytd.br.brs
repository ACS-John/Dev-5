00010 ! Replace S:\acsGL\PRZYTD
00020 ! Zero Year To Date and Quarter To Date Information
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fncno,fnerror,fnprocess,fnconsole,fnTos,fnLbl,fnTxt,fnCmdSet,fnAcs,fnmsgbox
00050   fntop(program$,cap$="Zero YTD Payroll Information")
00060   on error goto Ertn
00070 ! ______________________________________________________________________
00080   dim k$(3)*25,ss$*11,m(36),adr(2),n(2),d(14),cnam$*40,cap$*128,ml$(3)*80
00090 ! ______________________________________________________________________
00100   fncno(cno,cnam$)
00110   if fnprocess=1 then goto L300
00120 L120: fnTos(sn$="PrZqtr") !:
        mylen=42: mypos=mylen+3 : right=2
00130   fnLbl(1,1,"* * * * *   WARNING   * * * * *",mylen,right)
00140   fnLbl(3,1,"This program zeroes all year to date",mylen,right)
00150   fnLbl(4,1,"information. It should be run at",mylen,right)
00160   fnLbl(5,1,"at the end of each year after all",mylen,right)
00170   fnLbl(6,1,"quarterly and annual reports have been run.",mylen,right)
00180   fnLbl(8,1,"Enter 'ZERO' to continue:",mylen,right)
00190   fnTxt(8,mypos,4,0,right,"",0,"You must type the word 'Zero' to indicate that you for sure want to zero the year.",0 ) !:
        resp$(1)=""
00200   fnCmdSet(2)
00210   fnAcs(sn$,0,mat resp$,ckey)
00220 ! 
00230   if ckey=5 then goto XIT
00240   pas$=uprc$(resp$(1))
00250   if pas$="ZERO" then goto L300
00260 MSGBOX1: ! 
00270   mat ml$(2) !:
        ml$(1)="          Incorrect password! " !:
        ml$(2)="Click OK to try again; else Cancel to stop." !:
        fnmsgbox(mat ml$,resp$,cap$,49)
00280   if resp$="OK" then goto L120 else goto XIT
00290 ! ______________________________________________________________________
00300 L300: open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr",internal,outIn,keyed 
00310 L310: read #1,using L320: x eof L350
00320 L320: form pos 91,36*pd 5.2,2*n 5
00330   rewrite #1,using L320: mat m,mat adr
00340   goto L310
00350 L350: close #1: 
00360 ! _____________________
00370   open #1: "Name=[Q]\GLmstr\ACPRCKS.h[cno],size=0,RecL=110,Replace",internal,output 
00380   close #1: 
00390   goto XIT
00400 ! ______________________________________________________________________
00410 ! <Updateable Region: ERTN>
00420 ERTN: fnerror(program$,err,line,act$,"xit")
00430   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00440   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00450   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00460 ERTN_EXEC_ACT: execute act$ : goto ERTN
00470 ! /region
00480 ! ______________________________________________________________________
00490 XIT: fnxit
00500 ! ______________________________________________________________________
