00010 ! Replace S:\acsGL\fnsfm
00020 ! General Ledger Financial Statement Layout - Hamster
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnHamster,fnTos,fnFra,fnOpt,fnCmdKey,fnAcs,fncno
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,lbl$(21)*38,tln(21),p$(21)*160,fltyp$(21),sln(21),mask(21),sp(21),c$(21,8)*40
00080   dim fil$(6)*18,idx$(6)*18,id$(6)*40
00090 ! ______________________________________________________________________
00100   fntop(program$,cap$='Financial Statement Design')
00110   fncno(cno)
00120   id$(1)=" 1. Balance Sheet File" !:
        fil$(1)="ACGLFNSB.H[cno]": idx$(1)="FNSBINDX.H[cno]"
00130   id$(2)=" 2. Income Statement File" !:
        fil$(2)="ACGLFNSI.H[cno]": idx$(2)="FNSIINDX.H[cno]"
00140   id$(3)=" 3. Fund Statement / Cash Flow File" !:
        fil$(3)="ACGLFNSF.H[cno]": idx$(3)="FNSFINDX.H[cno]"
00150   id$(4)=" 4. Secondary Balance Sheet File" !:
        fil$(4)="ACGLFNSC.H[cno]": idx$(4)="FNSCINDX.H[cno]"
00160   id$(5)=" 5. Secondary Income Statement File" !:
        fil$(5)="ACGLFNSJ.H[cno]": idx$(5)="FNSJINDX.H[cno]"
00170   id$(6)=" 6. Secondary Fund / Cash Flow File" !:
        fil$(6)="ACGLFNSG.H[cno]": idx$(6)="FNSGINDX.H[cno]"
00180   gosub BUILD_LAYOUT
00190 MAIN: ! 
00200   fnTos(sn$="FsDesign") !:
        mylen=20: mypos=mylen+3 : right=1
00210   fnFra(1,1,6,60,"Financial Statement Choices","Choose the financial statement to work with.")
00220   fnOpt(1,2,id$(1),0,1) !:
        resp$(1)="True"
00230   fnOpt(2,2,id$(2) ,0,1) !:
        resp$(2)="False"
00240   fnOpt(3,2,id$(3),0,1) !:
        resp$(3)="False"
00250   fnOpt(4,2,id$(4),0,1) !:
        resp$(4)="False"
00260   fnOpt(5,2,id$(5),0,1) !:
        resp$(5)="False"
00270   fnOpt(6,2,id$(6),0,1) !:
        resp$(6)="False"
00280   fnCmdKey("&Next",1,1,0,"Access the chosen financial statement design..")
00290   fnCmdKey("&Cancel",5,1,0,"Return to main menu.")
00300   fnAcs(sn$,0,mat resp$,ckey)
00310   if ckey=5 then goto XIT
00320   if resp$(1)="True" then selection=1
00330   if resp$(2)="True" then selection=2
00340   if resp$(3)="True" then selection=3
00350   if resp$(4)="True" then selection=4
00360   if resp$(5)="True" then selection=5
00370   if resp$(6)="True" then selection=6
00380   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE !:
        gosub HAMSTER !:
        gosub CLOSE_FILE !:
        gosub INDEX
00390   goto MAIN
00400 ! ______________________________________________________________________
00410 OPEN_FILE: ! !:
        open_file_count=0 ! this value is used in the close_file sub routine
00420   open #open_file_count+=1: "Name=[Q]\GLmstr\"&fil$(selection)&",KFName=[Q]\GLmstr\"&idx$(selection)&",Use,RecL=83,KPs=1,KLn=5,Shr",internal,outIn,keyed 
00430   return 
00440 ! ______________________________________________________________________
00450 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
00460 ! ______________________________________________________________________
00470 INDEX: ! 
00480   execute "Index [Q]\GLmstr\"&fil$(selection)&' '&"[Q]\GLmstr\"&idx$(selection)&" 1 5 Replace DupKeys"
00490   return 
00500 ! ______________________________________________________________________
00510 BUILD_LAYOUT: ! 
00520   fncno(cno)
00530 ! ** Field Labels    ** !:
        ic=0 ! temporary Item Counter
00540   lbl$(ic+=1)="F/S #" !:
        lbl$(ic+=1)="Description" !:
        lbl$(ic+=1)="Type of Entry" !:
        lbl$(ic+=1)="Starting Print" !:
        lbl$(ic+=1)="Lines to Skip"
00550   lbl$(ic+=1)="Dollar Sign" !:
        lbl$(ic+=1)="Underlines" !:
        lbl$(ic+=1)="Reverse Sign" !:
        lbl$(ic+=1)="B/S Column"
00560   lbl$(ic+=1)="Print Accumulator" !:
        lbl$(ic+=1)="Clr 1" !:
        lbl$(ic+=1)="Clr 2" !:
        lbl$(ic+=1)="Clr 3"
00570   lbl$(ic+=1)="Clr 4" !:
        lbl$(ic+=1)="Clr 5" !:
        lbl$(ic+=1)="Clr 6" !:
        lbl$(ic+=1)="Clr 7"
00580   lbl$(ic+=1)="Clr 8" !:
        lbl$(ic+=1)="Clr 9"
00590   lbl$(ic+=1)="I/C % Base" !:
        lbl$(ic+=1)="Cost Center"
00600 ! ** Text Box / Field Display   Lengths   ** !:
        ic=0 ! temporary Item Counter !:
        mmddyy=8 !:
        ccyymmdd=10
00610   tln(ic+=1)=5 !:
        tln(ic+=1)=50 !:
        tln(ic+=1)=1 !:
        tln(ic+=1)=2 !:
        tln(ic+=1)=2
00620   tln(ic+=1)=1 !:
        tln(ic+=1)=1 !:
        tln(ic+=1)=1 !:
        tln(ic+=1)=1
00630   tln(ic+=1)=1 !:
        tln(ic+=1)=1 !:
        tln(ic+=1)=1 !:
        tln(ic+=1)=1
00640   tln(ic+=1)=1 !:
        tln(ic+=1)=1 !:
        tln(ic+=1)=1
00650   tln(ic+=1)=1 !:
        tln(ic+=1)=1 !:
        tln(ic+=1)=1 !:
        tln(ic+=1)=3
00660   tln(ic+=1)=5
00670 ! ** Field Types ** !:
        ic=0
00680   fltyp$(ic+=1)='c' !:
        fltyp$(ic+=1)='c' !:
        fltyp$(ic+=1)='C' !:
        fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='N'
00690   fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='N'
00700   fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='N'
00710   fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='N'
00720   fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='N'
00730   fltyp$(ic+=1)='N'
00740 ! ** Field Storage Lengths ** !:
        ic=0 !:
        mmddyy=6 : ccyymmdd=8
00750   sln(ic+=1)=5 !:
        sln(ic+=1)=50 !:
        sln(ic+=1)=1 !:
        sln(ic+=1)=2 !:
        sln(ic+=1)=2
00760   sln(ic+=1)=1 !:
        sln(ic+=1)=1 !:
        sln(ic+=1)=1 !:
        sln(ic+=1)=1
00770   sln(ic+=1)=1 !:
        sln(ic+=1)=1 !:
        sln(ic+=1)=1 !:
        sln(ic+=1)=1
00780   sln(ic+=1)=1 !:
        sln(ic+=1)=1 !:
        sln(ic+=1)=1
00790   sln(ic+=1)=1 !:
        sln(ic+=1)=1 !:
        sln(ic+=1)=1 !:
        sln(ic+=1)=3
00800   sln(ic+=1)=5
00810 ! ** Field Masks ** !:
        ic=0 !:
        pointtwo=32 : number=30 !:
        ccyymmdd=3 : mmddyy=1 : glnumber=53
00820   mask(ic+=1)=0 !:
        mask(ic+=1)=0 !:
        mask(ic+=1)=0 !:
        mask(ic+=1)=number !:
        mask(ic+=1)=number
00830   mask(ic+=1)=number !:
        mask(ic+=1)=number !:
        mask(ic+=1)=number !:
        mask(ic+=1)=number
00840   mask(ic+=1)=number !:
        mask(ic+=1)=number !:
        mask(ic+=1)=number !:
        mask(ic+=1)=number
00850   mask(ic+=1)=number !:
        mask(ic+=1)=number !:
        mask(ic+=1)=number !:
        mask(ic+=1)=number
00860   mask(ic+=1)=number !:
        mask(ic+=1)=number !:
        mask(ic+=1)=number
00870   mask(ic+=1)=number
00880 ! ** Storage Positions ** !:
        ! default to the same as order displayed !:
        ic=0
00890   sp(ic+=1)=1 !:
        sp(ic+=1)=6 !:
        sp(ic+=1)=56 !:
        sp(ic+=1)=57 !:
        sp(ic+=1)=59
00900   sp(ic+=1)=61 !:
        sp(ic+=1)=62 !:
        sp(ic+=1)=63 !:
        sp(ic+=1)=64
00910   sp(ic+=1)=65 !:
        sp(ic+=1)=66 !:
        sp(ic+=1)=67 !:
        sp(ic+=1)=68
00920   sp(ic+=1)=69 !:
        sp(ic+=1)=70 !:
        sp(ic+=1)=71 !:
        sp(ic+=1)=72
00930   sp(ic+=1)=73 !:
        sp(ic+=1)=74: !:
        sp(ic+=1)=75
00940   sp(ic+=1)=78
00950   return 
00960 ! ______________________________________________________________________
00970 HAMSTER: ! 
00980   fnHamster("Acglfnsb",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00990   return 
01000 ! ______________________________________________________________________
01010 XIT: fnxit
01020 ! ______________________________________________________________________
01030 ! <Updateable Region: ERTN>
01040 ERTN: fnerror(program$,err,line,act$,"xit")
01050   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01070   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01080 ERTN_EXEC_ACT: execute act$ : goto ERTN
01090 ! /region
01100 ! ______________________________________________________________________
