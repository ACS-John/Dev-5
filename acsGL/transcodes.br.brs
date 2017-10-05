00010 ! Replace S:\acsGL\transcodes
00020 ! Special transactions type file - Hamster
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster,fnagl$
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,lbl$(2)*38,tln(2),p$(2)*160,fltyp$(2),sln(2),mask(2),sp(2),c$(2,8)*40
00080 ! ______________________________________________________________________
00090   fntop(program$,cap$='Transaction Codes')
00100   gosub BUILD_LAYOUT
00110   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE !:
        gosub HAMSTER : gosub CLOSE_FILE: gosub INDEX
00120   goto XIT
00130 ! ______________________________________________________________________
00140 OPEN_FILE: ! !:
        open_file_count=1 ! this value is used in the close_file sub routine
00150   if exists(env$('Q')&"\GLmstr\transcodes.h"&str$(cno))=0 then goto L190
00160   if exists(env$('Q')&"\GLmstr\transcodes-idx.h"&str$(cno))=0 then gosub INDEX
00170   open #open_file_count: "Name="&env$('Q')&"\GLmstr\transcodes.H"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\transcodes-idx.H"&str$(cno)&",Version=1,Shr",internal,outin,keyed 
00180   goto L220
00190 L190: open #open_file_count: "Name="&env$('Q')&"\GLmstr\transcodes.h"&str$(cno)&",Version=1,Replace,RecL=90",internal,outin 
00200   gosub CLOSE_FILE
00210   gosub INDEX
00220 L220: return 
00230 ! ______________________________________________________________________
00240 INDEX: ! 
00250   execute "Index "&env$('Q')&"\GLmstr\transcodes.h"&str$(cno)&' '&env$('Q')&"\GLmstr\transcodes-idx.h"&str$(cno) &" 1 2 Replace,DupKeys"
00260   return 
00270 ! ______________________________________________________________________
00280 CLOSE_FILE: for j=1 to open_file_count
00290     close #j: ioerr L300
00300 L300: next j
00310   return 
00320 ! ______________________________________________________________________
00330 BUILD_LAYOUT: ! 
00340   fncno(cno)
00350 ! ** Field Labels    ** !:
        ic=0 ! temporary Item Counter
00360   lbl$(ic+=1)="Trans Code" !:
        lbl$(ic+=1)="Description"
00370 ! ** Text Box / Field Display   Lengths   ** !:
        ic=0 ! temporary Item Counter !:
        mmddyy=8 !:
        ccyymmdd=10
00380   tln(ic+=1)=2 !:
        tln(ic+=1)=20
00390 ! ** Field Types ** !:
        ic=0
00400   fltyp$(ic+=1)='n' !:
        fltyp$(ic+=1)='C'
00410 ! ** Field Storage Lengths ** !:
        ic=0 !:
        mmddyy=6 : ccyymmdd=8
00420   sln(ic+=1)=2 !:
        sln(ic+=1)=20
00430 ! ** Field Masks ** !:
        ic=0 !:
        pointtwo=32 : number=30 !:
        ccyymmdd=3 : mmddyy=1 : let glnumber=53
00440   mask(ic+=1)=number !:
        mask(ic+=1)=0
00450 ! ** Storage Positions ** !:
        ! starting field position - default to the same as order displayed !:
        ic=0
00460   sp(ic+=1)=1 !:
        sp(ic+=1)=3
00470 ! ** Combo Boxes **
00480 ! cL=1 : c$(CL,1)='ComboF' !:
        ! c$(CL,2)=env$('Q')&'\CLmstr\PayMstr.h'&STR$(CNO) !:
        ! c$(CL,3)='1' : c$(CL,4)='8' !:
        ! c$(CL,5)='9' : c$(CL,6)='30' !:
        ! c$(CL,7)=env$('Q')&'\CLmstr\PayIdx1.h'&STR$(CNO) !:
        ! c$(CL,8)=LIMIT_TO_LIST$
00490   return 
00500 ! ______________________________________________________________________
00510 HAMSTER: ! 
00520   fnhamster("TrAlloc",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00530   return 
00540 ! ______________________________________________________________________
00550 XIT: fnxit
00560 ! ______________________________________________________________________
00570 ! <Updateable Region: ERTN>
00580 ERTN: fnerror(program$,err,line,act$,"xit")
00590   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00600   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00610   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00620 ERTN_EXEC_ACT: execute act$ : goto ERTN
00630 ! /region
00640 ! ______________________________________________________________________
00650   fltyp$(ic+=1)='N'
