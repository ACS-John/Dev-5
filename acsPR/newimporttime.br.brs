00010 ! Replace S:\acsPR\newimporttime
00020 ! Capture Time for Time Sheet Entry                                            this program pulls time form any system that can create the following           layout : employee # n 8 : name c 30 : department n 3: reghrs n 7.2 :            othrs n 7.2 : vachrs n 7.2 : sickhrs n 7.2 : holhrs n 7.2
00030 ! the file name should be \program files\acs\timeclock.h&cno
00040 ! this program places the information in same file that simple time             clock uses in the input time sheets program
00050 ! ______________________________________________________________________
00060   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnwin3b,fnwait,fnerror,fnmsgbox,fntos,fnlbl,fntxt,fncmdkey,fnacs
00070   on error goto ERTN
00080 ! ______________________________________________________________________
00090   dim hen$*8,cap$*128,message$*40,io1$(7)*35,ml$(4)*70,pathtotimecard$*200
00100   dim msgline$(2)*60,response$(5)*1,simple$*50,wait$*40,name$*30,prname$*30
00110   dim ln$*76,filename$*40
00120 ! ______________________________________________________________________
00130   let fntop(program$,cap$="Import Time from Time Clock System")
00150   let pathtotimecard$="c:\progra~1\acs\"
00160 ! 
00170   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,outin,keyed 
00180   open #2: "Name="&env$('Q')&"\PRmstr\RPTRAIL.h"&env$('cno')&",Shr",internal,outin,relative 
00190 ! ______________________________________________________________________
00200 ASK_PAYROLL_DATE: ! 
00210   let fntos(sn$="Importtime") !:
        let respc=0
00220   let fnlbl(1,1,"",34,1) ! bigger screen
00230   let fnlbl(2,1,"Payroll Date:",20,1)
00240   let fntxt(2,23,10,0,1,"3",0,"Always use the calculation date.")
00250   let resp$(respc+=1)=str$(ppd)
00260   let fncmdkey("&Next",1,1,0,"Proceed with importing time." ) !:
        let fncmdkey("E&xit",5,0,1,"Returns to menu")
00270   let fnacs(sn$,0,mat resp$,ckey) ! ask employee #
00280   if ckey=5 then goto XIT
00290   let endingdate=val(resp$(1))
00300 ! ______________________________________________________________________
00310   let fnopenprn
00320   let filename$="Payroll"&cnvrt$("Pic(zzzzzzzz)",endingdate)(5:6) &"-"&cnvrt$("Pic(zzzzzzzz)",endingdate)(7:8)&"-" &cnvrt$("Pic(zzzzzzzz)",endingdate)(3:4) &".txt"
00330 ! if env$('client')="West Rest Haven" then let filename$=cnvrt$("Pic(zzzzzzzz)",endingdate)(5:6) &"-"&cnvrt$("Pic(zzzzzzzz)",endingdate)(7:8)&"-" &cnvrt$("Pic(zzzzzzzz)",endingdate)(3:4) &".txt"
00340 ! if env$('client')="West Rest Haven" then execute "Copy c:\Acs\local\wrhPayroll"&filename$&" "&pathtotimecard$&"TimeCard.h"&env$('cno')
00350   gosub HDR
00360   let fnwait(101,cap$,wait$="Importing: please wait...",0)
00370   on fkey 5 goto L580
00380   let simple$=pathtotimecard$&"TimeCard.h"&env$('cno')
00390   open #3: "Name="&pathtotimecard$&"TimeCard\SimpleSummary,KFName="&pathtotimecard$&"TimeCard\SSIndex,Replace,RecL=46,KPs=1,KLn=16",internal,outin,keyed 
00400   open #5: "Name="&simple$&",RecL=76",display,input 
00410 L410: linput #5: ln$ eof L570
00420   let eno=val(ln$(1:8)) conv MESSAGE1
00430   let dep=val(ln$(39:41)) conv L440
00440 L440: let reghrs=val(ln$(42:48)) conv L450
00450 L450: let othrs=val(ln$(49:55)) conv L460
00460 L460: let vachrs=val(ln$(56:62)) conv L470
00470 L470: let sickhrs=val(ln$(63:69)) conv L480
00480 L480: let holhrs=val(ln$(70:76)) conv L490
00490 L490: let name$=ln$(9:38)
00500   write #3,using L530: eno,dep,val(env$('cno')),reghrs,othrs,vachrs,sickhrs,holhrs,0
00510   read #1,using L520,key=lpad$(str$(eno),8): prname$ nokey MESSAGE2
00520 L520: form pos 9,c 30
00530 L530: form pos 1,n 8,n 3,n 5,6*pd 5.2
00540   print #255,using L550: eno,name$(1:27),dep,reghrs,othrs,vachrs,sickhrs,holhrs pageoflow PAGE_O_FLOW
00550 L550: form pos 1,n 8,x 1,c 27,x 1,n 3,5*n 10.2
00560   goto L410
00570 L570: ! thru
00580 L580: close #3: ioerr L590
00590 L590: execute "Index "&pathtotimecard$&"TimeCard\SimpleSummary "&pathtotimecard$&"TimeCard\SSIndex 1 16 Replace DupKeys -n"
00600   goto XIT
00610 ! ______________________________________________________________________
00620 ! <Updateable Region: ERTN>
00630 ERTN: let fnerror(program$,err,line,act$,"xit")
00640   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00650   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00660   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00670 ERTN_EXEC_ACT: execute act$ : goto ERTN
00680 ! /region
00690 ! ______________________________________________________________________
00700 XIT: let fncloseprn: let fnxit
00710 MESSAGE1: ! bad data
00720   mat ml$(4) !:
        let ml$(1)="Cannot read the data! It must be a corrupted" !:
        let ml$(2)="file or in the wrong format.  You must export" !:
        let ml$(3)="the time from your time clock system before" !:
        let ml$(4)="attempting to run this menu option." !:
        let fnmsgbox(mat ml$,resp$,cap$,48)
00730   goto XIT
00740 MESSAGE2: ! bad employee number
00750   mat ml$(4) !:
        let ml$(1)="Employee # "&str$(eno)&" has time imported" !:
        let ml$(2)="from the time clock, but does not have a matching" !:
        let ml$(3)="employee number in the payroll system.  This" !:
        let ml$(4)="person will be skipped.   "&name$ !:
        let fnmsgbox(mat ml$,resp$,cap$,48)
00760   goto L410
00770 HDR: ! 
00780   print #255,using L790: time$,env$('cnam'),date$,"Time Card Summary",cnvrt$("pic(zzzz/zz/zz)",endingdate)
00790 L790: form skip 2,pos 1,c 20,pos 20,cc 40,skip 1,pos 1,c 20,pos 20,cc 40,skip 1,pos 20,cc 40,skip 2
00800   print #255: "Employee  Name                        Dep  Regular  Overtime  Vacation      Sick   Holiday"
00810   return 
00820 PAGE_O_FLOW: ! 
00830   print #255: newpage
00840   gosub HDR
00850   continue 
