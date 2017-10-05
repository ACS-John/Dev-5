00010 ! Replace S:\acsGL\PRMerge
00020 ! ACCOUNTANTS P/R MERGE (Posts payroll checks entered directly from G/L to the after-the-fact payroll records in G/L)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fnopenprn,fncloseprn,fnprocess,fncno,fnerror,fntos,fnlbl,fnopt,fncmdkey,fnacs,fncmdset,fntxt,fncombof
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim k$(3)*25,ss$*11,d(22),m(36),adr(2),n(2),fb$(4),en$*4,cap$*128,tr(7)
00080 L80: dim tr$*12,td$*30,jv$(3)*6,resp$(10)*80
00090 ! ______________________________________________________________________
00100   fntop(program$,cap$="Post Payroll Checks")
00110   fncno(cno)
00120   if exists(env$('Q')&"\glmstr\PRmstr.h"&str$(cno))=0 then goto XIT
00130   open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.H"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00140   open #2: "Name="&env$('Q')&"\GLmstr\GL_Work_"&env$('acsUserId')&".h"&str$(cno)&",NoShr",internal,outin,relative 
00150   if lrec(2)=0 then goto XIT
00160   open #3: "Name="&env$('Q')&"\GLmstr\ACPRCKS.h"&str$(cno)&",Shr",internal,outin,relative 
00170 READ_ENTRIES: ! 
00175 L160: read #2,using L180: t$,tr(4),tr(5),tr(6),tr(7),tr$,td$,ven$,mat jv$,key$ eof L500
00180   rec2=rec(2)
00185   if tr(7)=7 or tr(7)=16 then goto L160 ! already posted (7 pr only, 16 both)
00187 L180: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8,c 6,c 5,c 3,pos 93,c 12
00189   if tr(6)<>4 then goto L160 ! must be payroll check transaction
00191   en$=lpad$(rtrm$(ven$),4) soflow L870
00193   if trim$(olden$)<>"" and olden$<>en$ or holdtr$<>"" and holdtr$<>tr$ then gosub REWRITE_RECORD
00220 L220: if olden$<>en$ or holdtr$<>tr$ then read #1,using L230,key=en$: eno,mat m,mat adr nokey L870
00230 L230: form pos 1,n 4,pos 91,36*pd 5.2,2*n 5
00240   j=val(jv$(2)) ! pull code from Jv$
00250   if j=0 then j=17 ! put in miscellaneous if breakdown code lost
00260   if j=1 or j=17 or j=18 then m(j*2-1)=m(j*2-1)+tr(5) : d(j+3)=tr(5) else m(j*2-1)=m(j*2-1)-tr(5) : d(j+3)=-tr(5)
00270   if j=1 or j=17 or j=18 then m(j*2)=m(j*2)+tr(5) else m(j*2)=m(j*2)-tr(5)
00280   if j=17 then goto L310 ! skip weeks worked
00290   d(22)+=tr(5) ! add net
00310 L310: olden$=en$: holdtr4=tr(4): holdtr$=tr$: rewrite #2,using L470,rec=rec2: tr(7)+7: goto READ_ENTRIES
00320 REWRITE_RECORD: ! 
00330 L330: r9=lrec(3)+1
00340   nca=0
00350   d(1)=eno: d(2)=holdtr4 : d(3)=val(holdtr$)
00360   write #3,using ' Form POS 1,N 4,2*PD 4,19*PD 5.2,PD 3',rec=r9,reserve: mat d,nca duprec L330
00370   mat d=(0)
00380   if adr(2)=0 then goto L430
00390   read #3,using L400,rec=adr(2),reserve: nca
00400 L400: form pos 108,pd 3
00410   nca=r9
00420   rewrite #3,using L400,rec=adr(2),reserve: nca
00430 L430: if adr(1)=0 then adr(1)=r9
00440   adr(2)=r9
00450   if trim$(olden$)="" then goto L480
00460   rewrite #1,using L230,key=olden$: eno,mat m,mat adr nokey L460
00465 L460: rewrite #2,using L470: tr(7)+7 ! add to posting code of 9 from gl merge
00470 L470: form pos 27,n 2
00480 L480: if eofcode=1 then goto L500 ! write last record and quit
00490   return 
00500 L500: ! EOJ
00510   if eofcode=0 then eofcode=1: goto REWRITE_RECORD
00520   close #1: 
00530   close #2: 
00540   close #3: 
00550   if print1=1 then let fncloseprn
00560   goto XIT
00570 ! ______________________________________________________________________
00580   if fnprocess=1 then goto L800
00590   pr newpage
00600   pr f "4,10,c 60": "The Following Employee is not on File."
00610   pr f "7,2,c 60": "    Emp Number    Check Number    Date   Gross-Pay"
00620   pr f "9,1,PIC(ZZZZZZZZZZ#),N": d(1)
00630   pr f "9,20,PIC(ZZZZZZ#),N": d(3)
00640   pr f "9,32,PIC(ZZ/ZZ/ZZ),N": d(2)
00650   pr f "9,40,PIC(--------.##),N": d(4)
00660   pr f "20,1,C 66,N": "Enter 0 to Add this Employee or Enter Correct Employee Number"
00670 L670: input fields "20,70,N 4,UE,N": numb conv L670
00680   if numb>0 then d(1)=numb: goto L220
00690   pr newpage
00700   pr f "5,10,c 25": "Employee Name" !:
        pr f "7,10,c 25": "Address" !:
        pr f "9,10,c 25": "City, State, Zip Code" !:
        pr f "11,10,c 25": "Social Security Number"
00710   let fb$(1)="5,40,C 25,UT,N" !:
        let fb$(2)="7,40,C 25,UT,N" !:
        let fb$(3)="9,40,C 25,UT,N" !:
        let fb$(4)="11,40,C 11,UT,N"
00720 L720: input fields mat fb$: k$(1),k$(2),k$(3),ss$ conv L720
00730 L730: mat m=(0)
00740   mat adr=(0)
00750   write #1,using L760: d(1),mat k$,ss$,mat m,mat adr
00760 L760: form pos 1,n 4,3*c 25,c 11,36*pd 5.2,2*n 5
00770   goto L220
00780 ! ______________________________________________________________________
00790 ! ______________________________________________________________________
00800 L800: fnopenprn
00810   if print1=0 then pr #255: "The Employees listed here were not previously on file." !:
          pr #255: "Use Payroll Employee File maintenance to enter" !:
          pr #255: "their Name and Address" !:
          pr #255: "_______________________________________________________________________________"
00820   pr #255: d(1)
00830   print1=1
00840   mat k$=(" ")
00850   ss$=" "
00860   goto L730
00870 L870: fntos(sn$="prmerge") !:
        mylen=40: mypos=mylen+3 : right=1
00880   fnlbl(1,10,"  Employee Number: "&ven$,mylen,left)
00890   fnlbl(2,10,"   Check Number: "&tr$,mylen,left)
00900   fnlbl(3,10, "           Date: "&cnvrt$("pic(zz/zz/zz)",tr(4)),mylen,left)
00910   fnlbl(4,10, "  Gross Pay: "&cnvrt$("pic(-------.zz)",tr(5)) ,mylen,left)
00920   fnlbl(7,5, "This employee number does not exist!" ,60,0)
00930   fnopt(8,10,"Add this Employee",0,0) !:
        resp$(1)="True"
00940   fnopt(9,10,"Change Employee Number",0,0) !:
        resp$(2)="False"
00950   fncmdkey("&Next",1,1,0,"Allows you to either add the employee or change the employee #.")
00960   fnacs(sn$,0,mat resp$,ckey)
00970   if resp$(1)="True" then gosub ADD : goto L220
00980   if resp$(2)="True" then gosub CHANGE_EMPLOYEE_NUMBER : goto L220
00990   goto L80
01000 ! ______________________________________________________________________
01010 ADD: ! 
01020   fntos(sn$="prmerge3") !:
        mylen=15: mypos=mylen+3 : right=1: rc=0
01030   fnlbl(1,1,"Name:",mylen,right)
01040   fntxt(1,mypos,30,0,left,"",0,"Enter the employee information.",0 ) !:
        resp$(rc+=1)=k$(1)
01050   fnlbl(2,1,"Address:",mylen,right)
01060   fntxt(2,mypos,30,0,left,"",0,"Enter the employee information.",0 ) !:
        resp$(rc+=1)=k$(2)
01070   fnlbl(3,1,"City, St Zip:",mylen,right)
01080   fntxt(3,mypos,30,0,left,"",0,"",0 ) !:
        resp$(rc+=1)=k$(3)
01090   fnlbl(4,1,"SS Number:",mylen,right)
01100   fntxt(4,mypos,11,0,left,"",0,"Enter the employee social security number.",0 ) !:
        resp$(rc+=1)=ss$
01110   fncmdset(2)
01120   fnacs(sn$,0,mat resp$,ckey)
01130   if ckey=5 then goto L220
01140   k$(1)=resp$(1) !:
        k$(2)=resp$(2) !:
        k$(3)=resp$(3) !:
        ss$=resp$(4)
01150   mat m=(0)
01160   mat adr=(0)
01170   write #1,using L1180: en$,mat k$,ss$,mat m,mat adr
01180 L1180: form pos 1,c 4,3*c 25,c 11,36*pd 5.2,2*n 5
01190   return 
01200 ! 
01210 CHANGE_EMPLOYEE_NUMBER: ! 
01220   fntos(sn$="Prmerge4") !:
        mylen=18: mypos=mylen+3 : right=1: rc=0
01230   fnlbl(1,1,"Employee Number:",mylen,right)
01240   fncombof("PRmstr",1,mypos,27,env$('Q')&"\GLmstr\PRmstr.h"&str$(cno),1,4,5,30,'',0,pas, "Choose from the list of employees.",0) !:
        resp$(1)=""
01250   fncmdset(2)
01260   fnacs(sn$,0,mat resp$,ckey)
01270   if ckey=5 then goto L220
01280   en$=lpad$(rtrm$(resp$(1)(1:4)),4)
01290   goto L220
01300 ! ______________________________________________________________________
01310 XIT: fnxit
01320 ! ______________________________________________________________________
01330 ! <Updateable Region: ERTN>
01340 ERTN: fnerror(program$,err,line,act$,"xit")
01350   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01360   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01370   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01380 ERTN_EXEC_ACT: execute act$ : goto ERTN
01390 ! /region
01400 ! ______________________________________________________________________
