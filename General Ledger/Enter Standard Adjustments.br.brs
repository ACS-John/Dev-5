00010 ! formerly S:\acsGL\bldstdaJ
00020 ! Standard Adjustments - Hamster (will need a conversion pgm. now only one adjustment per entry - was 10!)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster,fnagl$
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim lbl$(4)*38,tln(4),p$(4)*160,fltyp$(4),sln(4),mask(4),sp(4),c$(4,8)*40
00080 ! ______________________________________________________________________
00090   let fntop(program$)
00100   gosub BUILD_LAYOUT
00110   gosub OPEN_FILE : gosub CLOSE_FILE    : gosub OPEN_FILE 
00112   gosub HAMSTER   : gosub FIXGLACCOUNTS : gosub CLOSE_FILE : gosub INDEX
00120   goto XIT
00130 ! ______________________________________________________________________
00140 OPEN_FILE: !
00142   let open_file_count=1 ! this value is used in the close_file sub routine
00150   if exists(env$('Q')&"\GLmstr\GLSTdad.H"&str$(cno))=0 then goto L190
00160   if exists(env$('Q')&"\GLmstr\glstdidx.h"&str$(cno))=0 then gosub INDEX
00170   open #open_file_count: "Name="&env$('Q')&"\GLmstr\glstdad.H"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\glstdidx.H"&str$(cno)&",Version=1,Shr",internal,outin,keyed 
00180   goto L220
00190 L190: open #open_file_count: "Name="&env$('Q')&"\GLmstr\glstdad.h"&str$(cno)&",Version=1,Replace,RecL=59",internal,outin 
00200   gosub CLOSE_FILE
00210   gosub INDEX
00220 L220: return 
00230 ! ______________________________________________________________________
00240 INDEX: ! 
00250   execute "Index "&env$('Q')&"\GLmstr\glstdad.H"&str$(cno)&' '&env$('Q')&"\GLmstr\glstdidx.h"&str$(cno) &" 1 12 Replace,DupKeys"
00260 return 
00270 ! ______________________________________________________________________
00280 FIXGLACCOUNTS: ! r: left pad general ledger number and reference number
00290   restore #open_file_count: 
00300   do
00302     read #open_file_count, using "form pos 43,c 12": gl$ eof L340
00310     let gl$=lpad$(rtrm$(gl$),12)
00320     rewrite #open_file_count, using "form pos 43,c 12,": gl$
00330   loop
00340 L340: !
00342 return ! /r
00350 ! ______________________________________________________________________
00360 CLOSE_FILE: ! r:
00362   for j=1 to open_file_count
00370     close #j: ioerr ignore
00380   next j
00390 return ! /r
00400 ! ______________________________________________________________________
00410 BUILD_LAYOUT: ! 
00420   let fncno(cno)
00430 ! ** Field Labels    ** !:
        let ic=0 ! temporary Item Counter
00440   let lbl$(ic+=1)="Reference" !:
        let lbl$(ic+=1)="Description" !:
        let lbl$(ic+=1)="G/L Number" !:
        let lbl$(ic+=1)="Amount"
00460 ! ** Text Box / Field Display   Lengths   ** !:
        let ic=0 ! temporary Item Counter !:
        let mmddyy=8 !:
        let ccyymmdd=10
00470   let tln(ic+=1)=12 !:
        let tln(ic+=1)=30 !:
        let tln(ic+=1)=12 !:
        let tln(ic+=1)=10.2
00500 ! ** Field Types ** !:
        let ic=0
00510   let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='PD'
00530 ! ** Field Storage Lengths ** !:
        let ic=0 !:
        let mmddyy=6 : let ccyymmdd=8
00540   let sln(ic+=1)=12 !:
        let sln(ic+=1)=30 !:
        let sln(ic+=1)=12 !:
        let sln(ic+=1)=5.2
00570 ! ** Field Masks ** !:
        let ic=0 !:
        let pointtwo=32 : let number=30 !:
        let ccyymmdd=3 : let mmddyy=1 : let glnumber=53
00580   let mask(ic+=1)=0 !:
        let mask(ic+=1)=0 !:
        let mask(ic+=1)=0 !:
        let mask(ic+=1)=pointtwo
00610 ! ** Storage Positions ** !:
        ! starting field position - default to the same as order displayed !:
        let ic=0
00620   let sp(ic+=1)=1 !:
        let sp(ic+=1)=13 !:
        let sp(ic+=1)=43 !:
        let sp(ic+=1)=55
00650 ! ** Combo Boxes **                                                   !:
        let cl=3 : let c$(cl,1)='ComboF' !:
        let c$(cl,2)=env$('Q')&"\GLmstr\GLmstr.h"&str$(cno) !:
        let c$(cl,3)="1" : let c$(cl,4)="12" !:
        let c$(cl,5)="13" : let c$(cl,6)="30" !:
        let c$(cl,7)=env$('Q')&"\GLmstr\glindex.h"&str$(cno) !:
        ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     !:
        let limit_to_list$='1'
00670 return 
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
