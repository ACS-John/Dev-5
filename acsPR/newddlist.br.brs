00010 ! Replace S:\acsPR\newDDLIST
00020 ! Direct Deposit listing
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fnwin3b,fnopenprn,fncloseprn,fndate_mmddyy_to_ccyymmdd,fnxit,fntop,fnTos,fnLbl,fnTxt,fnCmdKey,fnAcs,fnChk,fnCmdSet,fnmsgbox
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,d1$*20,ml$(3)*80,tcp(32),cp(32),tdc(10)
00080   dim path$*30 ! Path to Save File to
00090   dim email$*30 ! OR E-Mail Address to send file to
00100 ! em17  = Last Payroll Date (from first screen of employee record, !:
        !         not departmental record)
00110 ! tdt4  = Last Payroll Date (from Departmental record)
00120 ! tcp(32) = Net Pay
00130   dim em$(3)*30 ! (1)=Emp Name, (2)=Emp Addr, (3)=Emp CSZ
00140 ! ______________________________________________________________________
00150   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01) ! /r
00160 ! ______________________________________________________________________
00170   fntop(program$,cap$="Direct Deposit List")
00200   open #mstr=1: "Name="&env$('Q')&"\PRmstr\RPmstr.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00210   open #dd=30: "Name="&env$('Q')&"\PRmstr\DD.h"&env$('cno')&",RecL=72,KFName="&env$('Q')&"\PRmstr\DDidx1.h"&env$('cno')&",Shr,kps=1,kln=10,Use",internal,outIn,keyed 
00220   goto SCREEN1
00230 ! ______________________________________________________________________
00240 XIT: fnxit
00250 ! ______________________________________________________________________
00260 ERTN: fnerror(program$,err,line,act$,"XIT")
00270   if uprc$(act$)<>"PAUSE" then goto L300
00280   execute "list -"&str$(line) : pause : goto L300
00290   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." !:
        pr "" : pause 
00300 L300: execute act$ : goto ERTN
00310 ! ______________________________________________________________________
00320 SCREEN1: ! 
00330   mypos=20
00340 ASK_INFO: ! 
00350   fnTos(sn$="DD")
00360   fnLbl(1,35,"",1,1) ! bigger screen
00370   fnLbl(2,1,"As of Date:",mypos,1)
00380   fnTxt(2,mypos+3,10,0,1,"3",0,"This report will list any employees who direct deposit on the date the report is printed.")
00390   resp$(1)=str$(d1)
00400   fnCmdKey("&Print",1,1,0,"Print the direct deposit listing." ) !:
        fnCmdKey("E&xit",5,0,1,"Returns to menu")
00410   fnAcs(sn$,0,mat resp$,ckey) ! ask employee #
00420   if ckey=5 then goto XIT
00430   ppd=val(resp$(1))
00440   open #ddout=22: "Name=DDout"&wsid$&".txt,RecL=96,EOL=CRLF,Replace",external,output 
00450   fnopenprn
00460   gosub HDR ! pr header
00470 READ_DD: ! 
00480 L480: read #dd,using "Form pos 1,C 10,C 1,N 9,N 2,N 17": key$,dd$,rtn,acc,acn eof DONE
00490   if uprc$(dd$)<>"Y" then goto READ_DD !:
          ! Y means Yes Direct Deposit is active for this person
00500   key$=lpad$(rtrm$(ltrm$(key$)),8) !:
        read #mstr,using 'Form pos 9,3*C 30,Pos 162,N 6,Pos 173',key=key$: mat em$,em17 nokey L480
00510   pr #255,using "form pos 1,c 40,n 14,n 4,n 17": key$&" "&em$(1),rtn,acc,acn pageoflow PRINT_NEWPAGE
00520   goto READ_DD
00530 ! ______________________________________________________________________
00540 DONE: ! r:
00550   fncloseprn
00570 goto XIT ! /r
00580 PRINT_NEWPAGE: ! r:
00590   pr #255: newpage
00600   gosub HDR
00610 continue ! /r
00620 HDR: ! r:
00630   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
00640   pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
00650   pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
00660   pr #255: "\qc  {\f181 \fs16 \b As of "&cnvrt$("pic(zzzz/zz/zz)",ppd)&"}"
00670   pr #255: "\ql   "
00680   pr #255: "   Emp # Name                                  Routing  C/S      Account"
00690 return ! /r
