00010 ! Replace S:\acsPR\newpr1099 (formerly)
00040 ! r: setup, fntop, open files, on error, etc
00050   library 'S:\Core\Library': fntop,fnxit, fnerror,fn1099print,fngethandle,fn1099print_close,fnask_1099_info
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim vn$*8,nam$*30,ad$(3)*30,ss$*11,a$(3)*40,b$(2)*12,box(11)
00100   dim cap$*128
00110   dim tcp(32),tdc(10)
00120   fntop(program$,cap$="Print 1099 Forms")
00130   open #hCompany:=fngethandle: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr", internal,input,relative
00140   read #hCompany,using "Form POS 1,3*C 40,2*C 12": mat a$,mat b$
00150   close #hCompany: 
00160   open #hrpmstr:=fngethandle: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
00170   open #hChecks:=fngethandle: "Name="&env$('Q')&"\PRmstr\payrollchecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outin,keyed 
00220 ! /r
00250   if ~fnask_1099_info (seltp,type,min1,beg_date,end_date) then goto XIT
00880 START: ! r: main loop
00890   mat ad$=("")
00900   read #hrpmstr,using 'form pos 1,c 8,3*c 30,c 11': vn$,nam$,ad$(1),ad$(2),ss$ eof FINIS
00930   eno=val(vn$) ioerr START
00940   mat box=(0)
00950   checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
00960   restore #hChecks,key>=checkkey$: nokey START
00970   do
00972     read #hChecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof CALL_1099_LIBRARY
00980     if heno<>eno then goto CALL_1099_LIBRARY
00990     if prd=>beg_date and prd<=end_date then 
01000       amt1=tcp(seltp+4)
01010       box(type)=box(type)+amt1
01012     end if
01020   loop
01040   CALL_1099_LIBRARY: ! 
01050   if box(type)=>min1 then 
01090     fn1099print(cnvrt$("n 8",eno),mat a$,nam$(1:18),mat ad$,trim$(ss$),mat box)
01100   end if
01120   goto START
01130 ! /r
01140 FINIS: ! r:
01150   close #hrpmstr: ioerr ignore
01152   close #hChecks: ioerr ignore
01162   seltp=type=min1=0 
01164   vn$=nam$="" 
01166   mat a$=(""): mat ad$=(""): mat box=(0) 
01168   fn1099print_close
01170   goto XIT ! /r
01190 XIT: fnxit
01740 ! <Updateable Region: ERTN>
01750 ERTN: fnerror(program$,err,line,act$,"xit")
01760   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01770   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01780   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01790 ERTN_EXEC_ACT: execute act$ : goto ERTN
01800 ! /region
