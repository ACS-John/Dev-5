00010 ! Replace S:\acsGL\bankreconciliation
00020 ! Bank Reconciliation File - Hamster
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster,fnagl$
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,lbl$(10)*38,tln(10),p$(10)*160,fltyp$(10),sln(10),mask(10),sp(10),c$(10,8)*40
00080 ! ______________________________________________________________________
00090   let fntop(program$,cap$='Bank Reconciliation')
00100   gosub BUILD_LAYOUT
00110   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE !:
        gosub HAMSTER : gosub FIXGLACCOUNTS: gosub CLOSE_FILE: gosub INDEX
00120   goto XIT
00130 ! ______________________________________________________________________
00140 OPEN_FILE: ! !:
        let open_file_count=1 ! this value is used in the close_file sub routine
00150   if exists(env$('Q')&"\GLmstr\bankrec.H"&str$(cno))=0 then goto L190
00160   if exists(env$('Q')&"\GLmstr\glstdidx.h"&str$(cno))=0 then gosub INDEX
00170   open #open_file_count: "Name="&env$('Q')&"\GLmstr\bankrec.H"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\bankrec-idx.H"&str$(cno)&",Version=1,Shr",internal,outin,keyed 
00180   goto L220
00190 L190: open #open_file_count: "Name="&env$('Q')&"\GLmstr\bankrec.h"&str$(cno)&",Version=1,Replace,RecL=91",internal,outin 
00200   gosub CLOSE_FILE
00210   gosub INDEX
00220 L220: return 
00230 ! ______________________________________________________________________
00240 INDEX: ! 
00250   execute "Index "&env$('Q')&"\GLmstr\bankrec.H"&str$(cno)&' '&env$('Q')&"\GLmstr\bankrec-idx.h"&str$(cno) &" 79/3/4 12/1/8 Replace,DupKeys"
00260   return 
00270 ! ______________________________________________________________________
00280 FIXGLACCOUNTS: ! left pad general ledger number and reference number
00290   restore #open_file_count: 
00300 L300: read #open_file_count, using "form pos 43,c 12,pos 4,c 8": gl$,tr$ eof L340
00310   let gl$=lpad$(rtrm$(gl$),12)
00311   let tr$=lpad$(rtrm$(tr$),8)
00320   rewrite #open_file_count, using "form pos 43,c 12,pos 4,c 8": gl$,tr$
00330   goto L300
00340 L340: return 
00350 ! ______________________________________________________________________
00360 CLOSE_FILE: for j=1 to open_file_count
00370     close #j: ioerr L380
00380 L380: next j
00390   return 
00400 ! ______________________________________________________________________
00410 BUILD_LAYOUT: ! 
00420   let fncno(cno)
00430 ! ** Field Labels    ** !:
        let ic=0 ! temporary Item Counter
00440   let lbl$(ic+=1)="Bank G/L" !:
        let lbl$(ic+=1)="T/C" !:
        let lbl$(ic+=1)="Ref #" !:
        let lbl$(ic+=1)="Date"
00442   let lbl$(ic+=1)="Amount" !:
        let lbl$(ic+=1)="Payee" !:
        let lbl$(ic+=1)="Description" !:
        let lbl$(ic+=1)="P/C"
00444   let lbl$(ic+=1)="Cleared" !:
        let lbl$(ic+=1)="S/C"
00460 ! ** Text Box / Field Display   Lengths   ** !:
        let ic=0 ! temporary Item Counter !:
        let mmddyy=8 !:
        let ccyymmdd=10
00470   let tln(ic+=1)=12 !:
        let tln(ic+=1)=1 !:
        let tln(ic+=1)=8 !:
        let tln(ic+=1)=8
00472   let tln(ic+=1)=10.2 !:
        let tln(ic+=1)=8 !:
        let tln(ic+=1)=35 !:
        let tln(ic+=1)=1
00474   let tln(ic+=1)=8 !:
        let tln(ic+=1)=1
00500 ! ** Field Types ** !:
        let ic=0
00510   let fltyp$(ic+=1)='CR' !:
        let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='N'
00512   let fltyp$(ic+=1)='PD' !:
        let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='N'
00514   let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='N'
00530 ! ** Field Storage Lengths ** !:
        let ic=0 !:
        let mmddyy=6 : let ccyymmdd=8
00540   let sln(ic+=1)=12 !:
        let sln(ic+=1)=1 !:
        let sln(ic+=1)=8 !:
        let sln(ic+=1)=6
00542   let sln(ic+=1)=10.2 !:
        let sln(ic+=1)=8 !:
        let sln(ic+=1)=35 !:
        let sln(ic+=1)=1
00544   let sln(ic+=1)=6 !:
        let sln(ic+=1)=1
00570 ! ** Field Masks ** !:
        let ic=0 !:
        let pointtwo=32 : let number=30 !:
        let ccyymmdd=3 : let mmddyy=1 : let glnumber=53
00580   let mask(ic+=1)=0 !:
        let mask(ic+=1)=30 !:
        let mask(ic+=1)=0 !:
        let mask(ic+=1)=1
00582   let mask(ic+=1)=10 !:
        let mask(ic+=1)=0 !:
        let mask(ic+=1)=0 !:
        let mask(ic+=1)=30
00584   let mask(ic+=1)=1 !:
        let mask(ic+=1)=30
00610 ! ** Storage Positions ** !:
        ! starting field position - default to the same as order displayed !:
        let ic=0
00620   let sp(ic+=1)=79 !:
        let sp(ic+=1)=3 !:
        let sp(ic+=1)=4 !:
        let sp(ic+=1)=12
00622   let sp(ic+=1)=18 !:
        let sp(ic+=1)=28 !:
        let sp(ic+=1)=36 !:
        let sp(ic+=1)=71
00624   let sp(ic+=1)=72 !:
        let sp(ic+=1)=78
00650 ! ** Combo Boxes **                                                   !:
        let cl=1 : let c$(cl,1)='ComboF' !:
        let c$(cl,2)=env$('Q')&"\GLmstr\GLmstr.h"&str$(cno) !:
        let c$(cl,3)="1" : let c$(cl,4)="12" !:
        let c$(cl,5)="13" : let c$(cl,6)="30" !:
        let c$(cl,7)=env$('Q')&"\GLmstr\glindex.h"&str$(cno) !:
        ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     !:
        let limit_to_list$='1'
00652 ! ** Combo Boxes **                                                   !:
        let cl=2 : let c$(cl,1)='ComboF' !:
        let c$(cl,2)=env$('Q')&"\GLmstr\transcode.h"&str$(cno) !:
        let c$(cl,3)="1" : let c$(cl,4)="2" !:
        let c$(cl,5)="3" : let c$(cl,6)="30" !:
        let c$(cl,7)=env$('Q')&"\GLmstr\transcode-idx.h"&str$(cno) !:
        ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     !:
        let limit_to_list$='1'
00670   return 
00680 ! ______________________________________________________________________
00690 HAMSTER: ! 
00700   let fnhamster("TrAlloc",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00710   return 
00720 ! ______________________________________________________________________
00730 XIT: let fnxit
00740 ! ______________________________________________________________________
00750 ! <Updateable Region: ERTN>
00760 ERTN: let fnerror(program$,err,line,act$,"xit")
00770   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00780   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00790   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00800 ERTN_EXEC_ACT: execute act$ : goto ERTN
00810 ! /region
00820 ! ______________________________________________________________________
00830   let fltyp$(ic+=1)='N'
