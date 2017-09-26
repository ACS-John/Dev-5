00010 !  Replace S:\acsPR\prW2b
00020 ! Print W-2s for second state - chained to from newprw2a (now Payroll\PrintW2Forms)  though (9/27/2016) !:
        ! FORM TYPE 22222 FOR WAGE AND TAX STATEMENT  - 1993
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnerror,fnopenprn,fncloseprn,fngethandle,fnxit
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim em$(3)*30,ss$*11,d(14),w(12),s(12),t(12),ta(1)
00080   dim a$(3)*40,b$*12,f$*8,g$*8,h$*8,d$(10)*8,e$(10)*12
00090   dim fa$(1),fb$(1),fc$(1),fd$(1),l$(10),eno$*8
00100 ! ______________________________________________________________________
00120   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input 
00130   read #1,using L140: mat a$,b$,mat d$,loccode,mat e$
00140 L140: form pos 1,3*c 40,c 12,pos 150,10*c 8,n 2,pos 317,10*c 12
00150   for j=1 to 3: let a$(j)=a$(j)(1:30): next j
00160   close #1: 
00170   print newpage
00180   print fields "10,5,C 60": "CHECK POSITION OF W2 FORMS FOR SECOND STATE"
00190   print fields "12,15,C 60": "PRESS ENTER TO CONTINUE:"
00200   input fields "12,40,C 1,I,N": pause$
00210   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
00220   open #2: "Name="&env$('Q')&"\PRmstr\PRW2ADDR.h"&env$('cno')&",NoShr",internal,input 
00230   open #hAddr:=fngethandle: "Name="&env$('Temp')&"\Addr."&session$&",NoShr",internal,input,relative 
00240   read #hAddr,using 'form pos 1,n 10.2,n 1',rec=1: ficamax,w1
00245   let ficamaw=ficamax*10
00260   let first=1
00270 ADDR_LOOP_TOP: ! r:
00272   read #2,using 'form pos 1,pd 3': addr eof EO_ADDR
00290   if addr=1 then goto ADDR_LOOP_TOP
00300   read #hAddr,using L310,rec=addr: eno,tcd,ty21,ty3,tlwh,pf$
00310 L310: form pos 1,n 8,n 2,3*pd 5.2,c 8
00320   if read1=1 then goto L360
00330   let oldeno=eno
00340   let oldtcd=tcd
00350   let read1=1
00360   L360: !
00362   if oldeno=eno and oldtcd=tcd then goto L380
00370   if oldeno><eno or oldtcd><tcd then goto EMP_READ
00380   L380: !
00382   let w(9)=w(9)+ty21 ! STATE WAGES
00390   let w(7)=w(7)+ty3 ! STATE WH
00400   if tlwh=0 then goto L430 ! NO LOCAL WH
00410   let w(10)=w(10)+ty21 ! LOCAL WAGES
00420   let w(8)=w(8)+tlwh ! LOCAL WH
00430 L430: let oldeno=eno
00440   let oldtcd=tcd
00450   goto ADDR_LOOP_TOP 
00460 EMP_READ: ! 
00462   let eno$=lpad$(str$(oldeno),8)
00470   read #1,using 'form pos 9,3*c 30,c 11',key=eno$: mat em$,ss$
00490   let g$=ltrm$(eno$)
00500   let stcode$=e$(oldtcd)
00510   let state$=d$(oldtcd)(1:2)
00520   gosub PRINTW2
00530   mat w=(0)
00540   if lstrec=1 then goto FINIS
00550   goto L380
00560   EO_ADDR: !
00562   let lstrec=1
00570   let oldeno=eno
00580   let oldtcd=tcd
00590 goto EMP_READ ! /r
00600 FINIS: ! r:
00602   close #1: 
00610   close #2: 
00620   close #hAddr: 
00630   fncloseprn 
00632 goto XIT ! /r 
00640 XIT: fnxit
00670 PRINTW2: ! r: PRINT W2 FORM
00680   let fnopenprn(cp,0,0,process)
00690   ! if rtrm$(file$(255))="PRN:/SELECT" then print #255,using ' form pos 1,c 9': hex$("2B0205000A1021")
00720   print #255,using L740: ss$
00730   print #255,using L770: b$,w(2),w(1)
00740   L740: form pos 23,c 12,skip 2
00750   print #255,using L770: a$(1),w(5),w(3)
00760   print #255,using L770: a$(2),w(11),w(12)
00770   L770: form pos 5,c 32,2*pic(zzzzzzzzzzzz.zz),skip 2
00780   print #255,using L770: a$(3),w(6),0
00790   print #255,using L770: g$,w(4),dcb
00800   print #255,using L770: em$(1),amt(1)+amt(2),0
00810   print #255,using L820: em$(2),desc$(3),desc$(5)
00820   L820: form pos 5,c 32,c 15,c 16,skip 2
00830   print #255,using L820: em$(3),desc$(4),desc$(6)
00840   print #255,using L850: px$,x$
00850   L850: form skip 1,pos 51,c 1,pos p1,c 1,skip 2
00860   print #255,using L870: state$,stcode$,w(9),w(7),pf$,w(10),w(8)
00870   L870: form pos 4,c 2,x 2,c 13,n 10.2,n 9.2,x 1,c 8,2*n 9.2,skip 2
01070   gosub NEWPGE
01180 return ! /r
01210 NEWPGE: ! r:
01212   let pl=33 ! INSERT PAGE LENGTH IN LINES
01220   let sk=pl-(krec(255)-int(krec(255)/pl)*pl)
01230   print #255,using L1240: ""
01240   L1240: form c 1,skip sk
01250 return ! /r
01270 ! <Updateable Region: ERTN>
01280 ERTN: let fnerror(program$,err,line,act$,"xit")
01290   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01300   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01310   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01320 ERTN_EXEC_ACT: execute act$ : goto ERTN
01330 ! /region
01340 ! ______________________________________________________________________
