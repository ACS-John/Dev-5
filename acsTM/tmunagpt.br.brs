00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fncno,fnxit,fntop,fncloseprn,fnopenprn,fnerror
00040   fntop("S:\acsTM\tmunblwk",cap$="Unbilled Work In Process")
00050   on fkey 5 goto L450
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim cno$*5,cna$*30,en$*9,e$*25,l$(50)*9,d(8),l(50,6),t(6)
00090   dim cat$(30)*30,cnam$*40,cap$*128
00100   fncno(cno,cnam$)
00110   let namtab=66-int(len(rtrm$(cnam$))/2)
00120 ! ______________________________________________________________________
00130   open #1: "Name="&env$('Q')&"\TMmstr\TMCat.h"&str$(cno)&",Shr",internal,input,relative ioerr ERTN
00140   read #1,using L150,rec=1: mat cat$ ioerr ERTN
00150 L150: form pos 1,30*c 30
00160   close #1: 
00170 ! ______________________________________________________________________
00180   open #1: "Name="&env$('Q')&"\TMmstr\Work2.H"&wsid$&",NoShr",internal,input ioerr ERTN
00190   open #2: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndex.h"&str$(cno)&",Shr",internal,input,keyed ioerr ERTN
00200   pr newpage
00210   pr fields "10,10,Cc 60,n": "Printing Unbilled Aging by Partner..."
00220   pr fields "12,30,Cc 20,B,5": "Cancel (F5)"
00230   fnopenprn(cp,58,220,process)
00240   gosub L520
00250 L250: read #1,using L260: cno$,cna$,en$,mat d eof L300 ioerr ERTN
00260 L260: form pos 1,c 5,c 30,c 9,n 2,n 6,pd 4.2,5*pd 4.2
00270   gosub L750
00280   gosub L870
00290   goto L250
00300 L300: close #1,free: 
00310   close #2: 
00320   open #3: "Name="&env$('Q')&"\TMmstr\EMmstr.H"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\EMIndex.h"&str$(cno)&",Shr",internal,input,keyed ioerr ERTN
00330   gosub L350
00340   goto L420
00350 L350: pr #255,using L360: "_________   _________  _________  _________  _________  _________"
00360 L360: form pos 36,c 65,skip 1
00370   pr #255,using L380: mat t
00380 L380: form x 20,"Total",pos 34,n 11.2,x 1,5*n 11.2,skip 1
00390   pr #255,using L360: "=========   =========  =========  =========  =========  ========="
00400   return 
00410 ! ______________________________________________________________________
00420 L420: gosub L640
00430   gosub L980
00440   gosub L350
00450 L450: close #1: ioerr L460
00460 L460: close #2: ioerr L470
00470 L470: close #6: ioerr L480
00480 L480: let fncloseprn
00490 XIT: let fnxit
00500 ! ______________________________________________________________________
00510 L510: pr #255: newpage
00520 L520: pr #255,using L530: date$,cnam$
00530 L530: form skip 2,pos 1,c 8,pos namtab,c 40,skip 1
00540   pr #255,using L550: time$,"Unbilled Work in Process - Aging Summary"
00550 L550: form pos 1,c 8,pos 46,c 40,skip 1
00560   pr #255,using L570: dat
00570 L570: form pos 62,pic(zz/zz/zz),skip 2
00580   pr #255,using L590: "Client     Client ","Accumulated","Current","30 - 60"," 60 - 90"," Over 90","Oldest    Category"
00590 L590: form pos 3,c 18,pos 43,c 11,pos 61,c 7,pos 72,c 7,pos 82,c 8,pos 93,c 8,pos 103,c 18,skip 1
00600   pr #255,using L610: "Number      Name","Hours      Amount","Days","Days","Days","Item"
00610 L610: form pos 3,c 16,pos 40,c 17,pos 74,c 4,pos 84,c 4,pos 95,c 4,pos 104,c 4,skip 2
00620   return 
00630 ! ______________________________________________________________________
00640 L640: pr #255: newpage
00650   pr #255,using L660: cnam$,"Unbilled Work in Process Summarized by Partner in Charge"
00660 L660: form skip 2,pos namtab,c 40,skip 1,pos 39,c 56,skip 1
00670   pr #255,using L570: dat
00680   pr #255,using L690: "Partner Name","Accumulated","Current","30 - 60"," 60 - 90"," Over 90"
00690 L690: form pos 3,c 18,pos 42,c 11,pos 61,c 7,pos 72,c 7,pos 81,c 8,pos 95,c 8,pos 103,c 18,skip 1
00700   pr #255,using L710: "Hours      Amount","Days","Days","Days"
00710 L710: form pos 40,c 17,pos 74,c 4,pos 84,c 4,pos 95,c 4,skip 2
00720   pr #255: 
00730   return 
00740 ! ______________________________________________________________________
00750 L750: let d7=int(d(2)/10000)
00760   let d8=int((d(2)-d7*10000)/100)
00770   let d9=d(2)-d7*10000-d8*100
00775   if d(1)=0 then let d(1)=1 ! to prevent error
00780   pr #255,using L790: cno$,cna$,d(3),d(4),d(5),d(6),d(7),d(8),d8,"/",d9,"/",d7,cat$(d(1))(1:22) pageoflow L810
00790 L790: form pos 2,c 5,pos 8,c 30,pos 38,n 7.2,pos 46,5*n 11.2,x 1,n 2,c 1,pic(##),c 1,n 2,x 1,c 22,skip 1
00800   goto L820
00810 L810: gosub L510
00820 L820: for j=1 to 6
00830     let t(j)=t(j)+d(j+2)
00840   next j
00850   return 
00860 ! ______________________________________________________________________
00870 L870: for j=1 to 50
00880     if l$(j)=en$ then goto L930
00890     if rtrm$(l$(j))="" then goto L920
00900   next j
00910   goto L930
00920 L920: let l$(j)=en$
00930 L930: for k=1 to 6
00940     let l(j,k)=l(j,k)+d(k+2)
00950   next k
00960   return 
00970 ! ______________________________________________________________________
00980 L980: mat t=(0)
00990   for j=1 to 50
01000     if rtrm$(l$(j))="" then goto L1110
01010     read #3,using L1020,key=lpad$(rtrm$(l$(j)),9): e$ nokey L1040 ioerr ERTN
01020 L1020: form pos 10,c 25
01030     goto L1050
01040 L1040: let e$="UNASSIGNED"
01050 L1050: pr #255,using L1060: e$,l(j,1),l(j,2),l(j,3),l(j,4),l(j,5),l(j,6)
01060 L1060: form pos 5,c 25,pos 38,n 7.2,x 1,5*n 11.2,skip 1
01070     for j1=1 to 6
01080       let t(j1)=t(j1)+l(j,j1)
01090     next j1
01100   next j
01110 L1110: return 
01120 ! ______________________________________________________________________
01130 ! <Updateable Region: ERTN>
01140 ERTN: let fnerror(program$,err,line,act$,"xit")
01150   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01160   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01170   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01180 ERTN_EXEC_ACT: execute act$ : goto ERTN
01190 ! /region
01200 ! ______________________________________________________________________
