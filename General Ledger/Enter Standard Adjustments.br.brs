00010 ! formerly S:\acsGL\bldstdaJ
00020 ! Standard Adjustments - Hamster (will need a conversion pgm. now only one adjustment per entry - was 10!)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnHamster,fnagl$
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim lbl$(4)*38,tln(4),p$(4)*160,fltyp$(4),sln(4),mask(4),sp(4),c$(4,8)*40
00080 ! ______________________________________________________________________
00090   fntop(program$)
00100   gosub BUILD_LAYOUT
00110   gosub OPEN_FILE : gosub CLOSE_FILE    : gosub OPEN_FILE 
00112   gosub HAMSTER   : gosub FIXGLACCOUNTS : gosub CLOSE_FILE : gosub INDEX
00120   goto XIT
00130 ! ______________________________________________________________________
00140 OPEN_FILE: !
00142   open_file_count=1 ! this value is used in the close_file sub routine
00150   if exists(env$('Q')&"\GLmstr\GLSTdad.H"&env$('cno'))=0 then goto L190
00160   if exists(env$('Q')&"\GLmstr\glstdidx.h"&env$('cno'))=0 then gosub INDEX
00170   open #open_file_count: "Name="&env$('Q')&"\GLmstr\glstdad.H"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\glstdidx.H"&env$('cno')&",Version=1,Shr",internal,outIn,keyed 
00180   goto L220
00190 L190: open #open_file_count: "Name="&env$('Q')&"\GLmstr\glstdad.h"&env$('cno')&",Version=1,Replace,RecL=59",internal,outIn 
00200   gosub CLOSE_FILE
00210   gosub INDEX
00220 L220: return 
00230 ! ______________________________________________________________________
00240 INDEX: ! 
00250   execute "Index "&env$('Q')&"\GLmstr\glstdad.H"&env$('cno')&' '&env$('Q')&"\GLmstr\glstdidx.h"&env$('cno') &" 1 12 Replace,DupKeys"
00260 return 
00270 ! ______________________________________________________________________
00280 FIXGLACCOUNTS: ! r: left pad general ledger number and reference number
00290   restore #open_file_count: 
00300   do
00302     read #open_file_count, using "form pos 43,c 12": gl$ eof L340
00310     gl$=lpad$(rtrm$(gl$),12)
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
00420   fncno(cno)
00430 ! ** Field Labels    ** !:
        ic=0 ! temporary Item Counter
00440   lbl$(ic+=1)="Reference" !:
        lbl$(ic+=1)="Description" !:
        lbl$(ic+=1)="G/L Number" !:
        lbl$(ic+=1)="Amount"
00460 ! ** Text Box / Field Display   Lengths   ** !:
        ic=0 ! temporary Item Counter !:
        mmddyy=8 !:
        ccyymmdd=10
00470   tln(ic+=1)=12 !:
        tln(ic+=1)=30 !:
        tln(ic+=1)=12 !:
        tln(ic+=1)=10.2
00500 ! ** Field Types ** !:
        ic=0
00510   fltyp$(ic+=1)='C' !:
        fltyp$(ic+=1)='C' !:
        fltyp$(ic+=1)='C' !:
        fltyp$(ic+=1)='PD'
00530 ! ** Field Storage Lengths ** !:
        ic=0 !:
        mmddyy=6 : ccyymmdd=8
00540   sln(ic+=1)=12 !:
        sln(ic+=1)=30 !:
        sln(ic+=1)=12 !:
        sln(ic+=1)=5.2
00570 ! ** Field Masks ** !:
        ic=0 !:
        pointtwo=32 : number=30 !:
        ccyymmdd=3 : mmddyy=1 : glnumber=53
00580   mask(ic+=1)=0 !:
        mask(ic+=1)=0 !:
        mask(ic+=1)=0 !:
        mask(ic+=1)=pointtwo
00610 ! ** Storage Positions ** !:
        ! starting field position - default to the same as order displayed !:
        ic=0
00620   sp(ic+=1)=1 !:
        sp(ic+=1)=13 !:
        sp(ic+=1)=43 !:
        sp(ic+=1)=55
00650 ! ** Combo Boxes **                                                   !:
        cl=3 : c$(cl,1)='ComboF' !:
        c$(cl,2)=env$('Q')&"\GLmstr\GLmstr.h"&env$('cno') !:
        c$(cl,3)="1" : c$(cl,4)="12" !:
        c$(cl,5)="13" : c$(cl,6)="30" !:
        c$(cl,7)=env$('Q')&"\GLmstr\glindex.h"&env$('cno') !:
        ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     !:
        limit_to_list$='1'
00670 return 
00680 ! ______________________________________________________________________
00690 HAMSTER: ! 
00700   fnHamster("TrAlloc",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00710   return 
00720 ! ______________________________________________________________________
00730 XIT: fnxit
00740 ! ______________________________________________________________________
00750 ! <Updateable Region: ERTN>
00760 ERTN: fnerror(program$,err,line,act$,"xit")
00770   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00780   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00790   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00800 ERTN_EXEC_ACT: execute act$ : goto ERTN
00810 ! /region
00820 ! ______________________________________________________________________
00830   fltyp$(ic+=1)='N'
