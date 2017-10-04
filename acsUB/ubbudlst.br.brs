00010 ! Replace S:\acsUB\ubBudLst
00020 ! -- Budget Customer List
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fndat,fnwait,fnerror,fnopenprn,fncloseprn,fnxit,fnacs,fntxt,fncomboa,fnlbl,fntos,fncmdset,fntop
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim z$*10,e$(4)*30,dat$*20,idx$(3)*20,wrd2$(3)
00080   dim sel$(3)*38,cap$*128,txt$*40,resp$(10)*20
00090 ! ______________________________________________________________________
00120   fndat(dat$)
00130   fntop(program$,cap$="Customer List")
00140   let idx$(1)="ubIndex" !:
        let idx$(2)="ubIndx2" !:
        let idx$(3)="ubIndx3"
00150 ! ______________________________________________________________________
00160 SCR1: ! 
00170   let sn$="ubBudLst" !:
        fntos(sn$) !:
        let respc=0
00180   let mylen=22 !:
        let mypos=mylen+2
00190   fnlbl(1,1,"Sort by:",mylen,1)
00200   let wrd2$(1)="Account" !:
        let wrd2$(2)="Customer Name" !:
        let wrd2$(3)="Street" !:
        fncomboa("bs",1,mypos,mat wrd2$) !:
        let resp$(respc+=1)=wrd2$(1)
00210   fnlbl(2,1,"Report Heading Date:",mylen,1)
00220   fntxt(2,mypos,20) !:
        let resp$(respc+=1)=dat$
00230   fnlbl(3,1,"Print:",mylen,1)
00240   let sel$(1)="Active customers" !:
        let sel$(2)="Inactive customers" !:
        let sel$(3)="[All]" !:
        fncomboa("bs2",3,mypos,mat sel$) !:
        let resp$(respc+=1)=sel$(1)
00250   fncmdset(2)
00260   fnacs(sn$,0,mat resp$,ckey)
00270   if ckey=5 then goto XIT
00280   if resp$(1)=wrd2$(1) then let q0=1 else !:
          if resp$(1)=wrd2$(2) then let q0=2 else !:
            if resp$(1)=wrd2$(3) then let q0=3 ! sort by
00290   let dat$=resp$(2) !:
        fndat(dat$,2)
00300   if resp$(3)=sel$(1) then let ti2=1 else !:
          if resp$(3)=sel$(2) then let ti2=2 else !:
            if resp$(3)=sel$(3) then let ti2=3 ! active, inactive, etc...
00310 ! ______________________________________________________________________
00320   on fkey 5 goto DONE
00330   fnopenprn(cp,0,0,process)
00340   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\"&idx$(q0)&".h"&env$('cno')&",Shr",internal,input,keyed 
00350   gosub BUD1
00360   gosub HEADER
00370   goto READ_CUSTOMER
00380 ! ______________________________________________________________________
00390 READ_CUSTOMER: ! 
00400   read #1,using L430: z$,mat e$,final,bal eof DONE
00410   if bud1=1 then gosub BUD2
00420   if totba=0 then goto READ_CUSTOMER
00430 L430: form pos 1,c 10,pos 11,4*c 30,pos 1821,n 1,pos 292,pd 4.2
00440   if ti2=3 then goto L470
00450   if ti2=1 and final><0 then goto READ_CUSTOMER
00460   if ti2=2 and final=0 then goto READ_CUSTOMER
00470 L470: pr #255,using L480: z$,e$(2),e$(1)(1:25),totba pageoflow PGOF
00480 L480: form x 5,c 10,x 5,c 30,x 7,c 25,n 11.2,skip 2
00490   goto READ_CUSTOMER
00500 ! ______________________________________________________________________
00510 PGOF: ! 
00520   pr #255: newpage
00530   gosub HEADER
00540   goto READ_CUSTOMER
00550 ! ______________________________________________________________________
00560 HEADER: ! 
00570   let p2=p2+1
00580   pr #255: "\qc {\fs24 "&env$('cnam')&"}"
00590 ! pr #255: "\qc {\fs28 {\b "&env$('program_caption')&"}}"
00600   pr #255: "\qc {\fs28 {\b Budget Customer List }}"
00610   pr #255: "\qc {\fs24 "& dat$&"}"
00620   pr #255: "\qc {\fs20 "&date$("mm/dd/yy")&"   "&time$ &"   Page "&str$(p2)&"}"
00630   pr #255: ""
00640   pr #255: tab(7);"Account No";tab(21);"Name";tab(58);"Meter Address";tab(81);"Budget Amount"
00650   return 
00660 ! ______________________________________________________________________
00670 DONE: close #1: ioerr L680
00680 L680: let fncloseprn
00690 XIT: let fnxit
00700 ! ______________________________________________________________________
00710 BUD1: bud1=0
00720   dim ba(13),badr(2),bt1(14,2),bd1(5),bd2(5),bd3(5),bd$(5)*30
00730   open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&env$('cno')&",Shr",internal,outin,keyed ioerr L760
00740   open #82: "Name="&env$('Q')&"\UBmstr\BudTrans.h"&env$('cno')&",Shr",internal,outin,relative 
00750   bud1=1
00760 L760: return 
00770 ! ______________________________________________________________________
00780 BUD2: ! 
00790   let totba=0
00800   if bud1=0 then goto L860
00810   read #81,using L820,key=z$: z$,mat ba,mat badr nokey L860
00820 L820: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
00830   if ba(12)>0 then let totba=ba(12): goto L860 ! TOTAL BILL BUDGETED
00840   for j=2 to 12: let totba=totba+ba(j): next j
00850   if env$('client')="Findlay" then let totba=totba-ba(8) ! don't add the penalty
00860 L860: return 
00870 ! ______________________________________________________________________
00880 ! <Updateable Region: ERTN>
00890 ERTN: let fnerror(program$,err,line,act$,"xit")
00900   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00910   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00920   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00930 ERTN_EXEC_ACT: execute act$ : goto ERTN
00940 ! /region
00950 ! ______________________________________________________________________
