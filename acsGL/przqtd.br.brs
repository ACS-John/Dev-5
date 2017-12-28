00010 ! Replace S:\acsGL\PRZQTD
00020 ! Zero Quarterly Information (After-Fact-Payroll)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnprocess,fnTos,fnLbl,fnTxt,fnCmdSet,fnAcs,fnmsgbox
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim n(2),k$(3)*25,ss$*11,m(36),adr(2),cnam$*40,cap$*128,ml$(3)*80
00080 ! ______________________________________________________________________
00090   fntop(program$,cap$="Zero QTD Payroll Information")
00100   fncno(cno,cnam$)
00130   if fnprocess=1 then goto L290
00140 L140: fnTos(sn$="PrZqtr") !:
        mylen=40: mypos=mylen+3 : right=2
00150   fnLbl(1,1,"* * * * *   WARNING   * * * * *",mylen,right)
00160   fnLbl(3,1,"This program zeroes all quarterly",mylen,right)
00170   fnLbl(4,1,"information. It should be run at",mylen,right)
00180   fnLbl(5,1,"the end of each quarter after all",mylen,right)
00190   fnLbl(6,1,"quarterly reports have been run.",mylen,right)
00200   fnLbl(8,1,"Enter 'ZERO' to continue:",mylen,right)
00210   fnTxt(8,mypos,4,0,right,"",0,"You must type the word 'Zero' to indicate that you for sure want to zero the quarter.",0 ) !:
        resp$(1)=""
00220   fnCmdSet(2)
00230   fnAcs(sn$,0,mat resp$,ckey)
00240   if ckey=5 then goto XIT
00250   pas$=uprc$(resp$(1))
00260   if pas$="ZERO" then goto L290
00270 MSGBOX1: ! 
00271   mat ml$(2) !:
        ml$(1)="          Incorrect password! " !:
        ml$(2)="Click OK to try again; else Cancel to stop." !:
        fnmsgbox(mat ml$,resp$,cap$,49)
00273   if resp$="OK" then goto L140 else goto XIT
00280 ! ______________________________________________________________________
00290 L290: open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\PRINDEX.h"&env$('cno')&",Shr",internal,outIn,keyed 
00300 L300: read #1,using 'Form POS 91,36*PD 5.2': mat m eof L340
00310   for j=2 to 36 step 2 : m(j)=0 : next j
00320   rewrite #1,using 'Form POS 91,36*PD 5.2': mat m
00330   goto L300
00340 L340: close #1: 
00350   goto XIT
00360 ! ______________________________________________________________________
00370 XIT: fnxit
00380 ! ______________________________________________________________________
00390 ! <Updateable Region: ERTN>
00400 ERTN: fnerror(program$,err,line,act$,"xit")
00410   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00420   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00430   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00440 ERTN_EXEC_ACT: execute act$ : goto ERTN
00450 ! /region
00460 ! ______________________________________________________________________
