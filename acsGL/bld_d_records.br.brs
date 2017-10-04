00010 ! Replace S:\acsGL\Bld_D_Records
00020 ! Create Type "D" Records
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fnwait,fncno,fnfkey,fnoldmsgbox,fnwin3b,fncno,fnerror,fnconsole
00050   fntop(program$,cap$="Financial Statement")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim cnam$*40,dat$*20,io1$(9),gln(2,3),ta(2),ac(18),te$*1,cap$*128
00090   dim d$*50,bc(13),bp(13),bm(13),rf(6),dn$*3,an$*6,sn$*3,glk$*12,fsk$*5
00100 ! ______________________________________________________________________
00110   fncno(cno)
00120 ! 
00125   fnconsole(on=1)
00130   on fkey 5 goto XIT
00140   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00150   let fil$(1)="ACGLFNSB" : let idx$(1)="FNSBINDX"
00160   let fil$(2)="ACGLFNSc" : let idx$(2)="FNScINDX"
00170   let fil$(3)="ACGLFNSi" : let idx$(3)="FNSiINDX"
00180   let fil$(4)="ACGLFNSj" : let idx$(4)="FNSjINDX"
00190   let fil$(5)="ACGLfNSf" : let idx$(5)="FNSfINDX"
00200   let fil$(6)="ACGLfNSg" : let idx$(6)="FNSGINDX"
00210   let io1$(1)="5,32,Nz 3,UT,N"
00220   let io1$(2)="5,36,Nz 6,UT,N"
00230   let io1$(3)="5,43,Nz 3,UT,N"
00240   let io1$(4)="5,50,Nz 3,UT,N"
00250   let io1$(5)="5,54,Nz 6,UT,N"
00260   let io1$(6)="5,61,Nz 3,UT,N"
00270   let io1$(7)="8,68,Cu 1,UT,N"
00280   let io1$(8)="11,73,Nz 1,UT,N"
00290   let io1$(9)="13,49,Nz 5,UT,N"
00300 ! ______________________________________________________________________
00310 L310: pr newpage
00320   close #2: ioerr L330 ! close any reference file that is opened
00330 L330: let win=101
00340   cap$='Create Type "D" Records' ! use correct name above for printing
00350   fnwin3b(win,cap$,14,75,0,2,5,2)
00360   pr #win: newpage
00370 L370: pr #win,fields "2,1,Cc 75,R,N": 'Create "D" records for a range of general ledger numbers'
00380   pr #win,fields "4,32,Cc 14,N": "From"
00390   pr #win,fields "4,50,Cc 14,N": "To"
00400   pr #win,fields "5,2,Cr 28,N": "Range of General Ledger #s:"
00410   pr #win,fields "7,1,Cc 75,R,N": "Type of G/L Accounts Being Selected (Only one type at a time!)"
00420   pr #win,fields "8,2,Cr 65,N": "A (Asset),L (Liability),Q (Equity),I (Income), E (Expense:"
00430   pr #win,fields "10,2,Cr 46,N": "Starting Financial Statement Reference Number:"
00440   pr #win,fields "10,1,Cc 75,r,N": "Type of Financial Statement Being Designed"
00450   pr #win,fields "11,2,Cr 70,N": "1=Balance Sheet, 2=2nd B/S, 3=I/C, 4=2nd I/C, 5=Fund, 6=2nd Fund:"
00460   pr #win,fields "13,2,Cr 46,N": "Starting Financial Statement Reference Number:"
00470   input #win,fields mat io1$: mat gln,type$,fs,fin conv CONV1
00480   if ce>0 then let io1$(ce)(ce1:ce2)="U": ce=0
00490   if curfld<>8 or fin>0 then goto L550
00500   close #2: ioerr L510
00510 L510: open #2: "Name="&env$('Q')&"\GLmstr\"&fil$(fs)&".h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\"&idx$(fs)&".h"&str$(cno)&",Shr",internal,outin,keyed 
00520 L520: read #2,using L920: rno eof L540
00530   goto L520
00540 L540: pr #win,fields io1$(9): rno+20
00550 L550: if cmdkey>0 then goto L620 else ce=curfld+1
00560   if ce>udim(io1$) then ce=1
00570 L570: let io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",9)
00580   ce2=ce1+1 : let io1$(ce)(ce1:ce1)="UC" : goto L370
00590 CONV1: if ce>0 then let io1$(ce)(ce1:ce2)="U"
00600   ce=cnt+1
00610 ERR1: pr fields "24,78,C 1": bell : goto L570
00620 L620: ! 
00630   if cmdkey=5 then goto XIT
00640 ! ______________________________________________________________________
00650   if gln(1,2)=0 then ce=1: goto ERR1
00660   if gln(2,2)=0 then ce=4: goto ERR1
00670   let gl1=val(cnvrt$("PIC(###)",gln(1,1))&cnvrt$("PIC(######)",gln(1,2))&cnvrt$("PIC(###)",gln(1,3)))
00680   let gl2=val(cnvrt$("PIC(###)",gln(2,1))&cnvrt$("PIC(######)",gln(2,2))&cnvrt$("PIC(###)",gln(2,3)))
00690   if gl1=0 then ce=1: goto ERR1
00700   if gl2=0 then ce=4: goto ERR1
00710   let gf=gl3-gl1
00720   let glk$=lpad$(str$(gln(1,1)),3)&lpad$(str$(gln(1,2)),6)&lpad$(str$(gln(1,3)),3)
00730   if fin=0 then ce=8: goto ERR1
00740   if uprc$(type$)="A" or uprc$(type$)="L" or uprc$(type$)="Q" or uprc$(type$)="I" or uprc$(type$)="E" then goto L750 else ce=7: goto ERR1
00750 L750: if fs<1 or fs>5 then ce=9: goto ERR1
00760 ! ______________________________________________________________________
00770   pr #101: newpage
00780   pr #win,fields "5,1,Cc 75,N": "Please wait..."
00790   let ff=20 ! incrument reference numbers
00800   let te$="D" ! all detail records
00810   ac(1)=3 ! indent all d records to position 3
00820   let rno=fin
00830   close #2: ioerr L840
00840 L840: open #2: "Name="&env$('Q')&"\GLmstr\"&fil$(fs)&".h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\"&idx$(fs)&".h"&str$(cno)&",Shr",internal,outin,keyed 
00850   restore #1,key=glk$: nokey L980
00860 L860: read #1,using L870: dno,ano,sno,d$,mat rf eof L980
00870 L870: form pos 1,n 3,n 6,n 3,c 50,6*pd 3,42*pd 6.2,2*pd 3
00880   let gl=val(cnvrt$("PIC(###)",dno)&cnvrt$("PIC(######)",ano)&cnvrt$("PIC(###)",sno))
00890   if gl>gl2 then goto L310
00900   if type$="A" or type$="E" then ac(5)=0 else ac(5)=1 !:
          ! set liabilities, equity and revenue to reverse sign
00910   write #2,using L920: rno,d$,te$,mat ac
00920 L920: form pos 1,n 5,c 50,c 1,2*n 2,15*n 1,n 3
00930   let rf(fs)=rno ! rewrite new reference back into g/l account  ?
00940   rewrite #1,using L950: mat rf
00950 L950: form pos 63,6*pd 3
00960   let rno=rno+ff
00970   goto L860
00980 L980: close #2: 
00990   if fs>0 then !:
          execute "Index "&env$('Q')&"\GLmstr\"&fil$(fs)&".h"&str$(cno)&","&env$('Q')&"\GLmstr\"&idx$(fs)&".h"&str$(cno)&",1,5,Replace,DupKeys"
01000   goto L310
01010 ! ______________________________________________________________________
01020 XIT: chain "S:\acsGL\financialstatement"
01030 ! ______________________________________________________________________
01040 ! <Updateable Region: ERTN>
01050 ERTN: let fnerror(program$,err,line,act$,"xit")
01060   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01070   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01090 ERTN_EXEC_ACT: execute act$ : goto ERTN
01100 ! /region
