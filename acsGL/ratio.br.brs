00010 ! Replace S:\acsGL\ratio
00020 ! Ratio File  (was: Form POS 1,G 3,C 40,280*PD 4',Key=AC$: HAC$,NA$,MAT R  Now:  Form POS 1,G 3,C 40,80*c 12',Key=AC$: HAC$,NA$,MAT gl$
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fnerror,fnwait,fncno,fnoldmsgbox,fnsearch,fnopenprn,fncloseprn,fndat,fnprocess,fntos,fnlbl,fncombof,fncmdkey,fnacs,fntxt,fnchk,fnflexinit1,fnflexadd1,fnhamster,fnmsgbox,fnqgl,fnrgl$,fnagl$
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim gln(80,3),k4$*2,cap$*128,message$*40
00080   dim gl$(80)*12,cnam$*40,dat$*20,hac$*3,na$*40
00090   dim e$(2)*12,option$(6)*60,item$(7)*80
00100   dim heading$*70,form$*80,numeric_format$*20,selection$*70,resp$(90)*50
00110 ! ______________________________________________________________________
00120   fntop(program$,cap$="Ratio")
00130   fncno(cno,cnam$) !:
        fndat(dat$)
00142   ratiomst=10
00150   if exists(env$('Q')&"\GLmstr\ratiomst.h"&str$(cno))=0 then gosub CREATE_FILES
00160   if exists(env$('Q')&"\GLmstr\ratioidx.h"&str$(cno))=0 then gosub INDEX
00170   if exists(env$('Q')&"\GLmstr\schindx2.h"&str$(cno))=0 then gosub INDEX
00180 L180: open #ratiomst: "Name="&env$('Q')&"\GLmstr\RatioMST.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\RatioIDX.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L1380
00190   goto RATIOMSTGRID
00200   close #ratiomst: ioerr L210
00210 L210: execute "Index "&env$('Q')&"\GLmstr\RatioMST.h"&str$(cno)&' '&env$('Q')&"\GLmstr\SchIndX2.h"&str$(cno)&" 3 30 Replace DupKeys -n"
00220   goto L180
00230 RATIOMSTGRID: ! 
00240   fntos(sn$="Ratiomst") !:
        respc=0
00250   mat chdr$(3) : mat cmask$(3) : mat flxitm$(3) !:
        chdr$(1)="Rec" !:
        chdr$(2)="Ratio #" : chdr$(3)="Ratio Name" !:
        cmask$(1)='30' : cmask$(2)='': cmask$(3)=''
00260   frame=0
00270   restore #ratiomst: !:
        fnflexinit1('Ratiomst1',lc=1,1,10,50,mat chdr$,mat cmask$,1)
00280 READ_RATIOMST: ! read Ratiomst file
00290   read #ratiomst,using 'Form POS 1,G 3,C 40,80*c 12': hac$,na$,mat gl$ eof EO_RATIOMST_GRID
00300   item$(1)=str$(rec(ratiomst)) !:
        item$(2)=hac$: item$(3)=na$ !:
        fnflexadd1(mat item$)
00310   goto READ_RATIOMST
00320 EO_RATIOMST_GRID: ! 
00330   fnlbl(11,1,"")
00340   fncmdkey("&Add",1,0,0,"Allows you to add new Ratios.")
00350 ! 
00360   fncmdkey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit to change any existing Ratio.")
00370   fncmdkey("&Review G/L #",4,0,0,"Click to review the general ledger numbers used in this ratio.")
00380   fncmdkey("&Delete",8,0,0,"Highlight any record and click Delete to remove the Ratio.")
00390 ! fnCMDKEY("&Print",3,0,0,"Takes you directly to the pr Ratios option")
00400   fncmdkey("E&xit",5,0,1,"Exits to main menu")
00410   fnacs(sn$,0,mat resp$,ckey)
00420   if ckey=5 then goto XIT
00430   add=edit=0
00440   editrec=val(resp$(1))
00450   if ckey=2 then edit=1
00460   if ckey=4 then goto GL_NUMBERS
00470 ! If CKEY=3 Then Chain "S:\acsGL\acglschp" ! prints prints a Ratiomst
00480   if ckey=1 then add=1 !:
          hac$=na$="" !:
          mat gl$=("") !:
          goto ADD_EDIT_RATIOMST ! add
00490 ! to ADD_EDIT_Ratiomst ! add
00500   if ckey=2 then !:
          read #ratiomst,using 'Form POS 1,G 3,C 40,80*c 12',rec=editrec: hac$,na$,mat gl$ norec RATIOMSTGRID !:
          holdsn=sn !:
          goto ADD_EDIT_RATIOMST
00510   if ckey=8 then !:
          read #ratiomst,using 'Form POS 1,G 3,C 40,80*c 12',rec=editrec,release: hac$,na$,mat gl$ norec RATIOMSTGRID !:
          delete #ratiomst,rec=editrec: !:
          goto RATIOMSTGRID
00520   pause 
00530 ! 
00540 ADD_EDIT_RATIOMST: ! 
00550   fntos(sn$="Ratiomst2") !:
        mylen=20: mypos=mylen+3 : right=1
00560   fnlbl(1,1,"Ratio Number:",mylen,right)
00570   fncombof('glRatiomst',1,mypos,0,env$('Q')&"\GLmstr\ratiomst.h"&str$(cno),1,3,4,40,env$('Q')&"\GLmstr\ratioidx.h"&str$(cno),add_all)
00580   if edit=1 then resp$(1)=hac$
00590   if add=1 then resp$(1)=""
00600   fnlbl(2,1,"Ratio Nane::",mylen,right)
00610   fntxt(2,mypos,40,0,left,"",0,"",0 ) !:
        resp$(2)=na$
00620   fncmdkey("&Next",1,1,0,"Save the ratio.")
00630   fncmdkey("&Cancel",5,0,1,"Returns to list of Ratios withouit saving any changes.")
00640   fnacs(sn$,0,mat resp$,ckey)
00650   if ckey=5 then goto RATIOMSTGRID
00660   hac$=resp$(1)(1:3) conv ADD_EDIT_RATIOMST
00670   na$=resp$(2)
00680   if edit=1 then goto REWRITE_EXISTING_RATIOMST
00690   if add=1 then goto WRITE_NEW_RATIOMST
00700   pause 
00710 ! ______________________________________________________________________
00720 REWRITE_EXISTING_RATIOMST: ! 
00730   if hac$="" or trim$(hac$)="0" then goto ADD_EDIT_RATIOMST
00740   if holdhac$<>hac$ and holdhac$<>"" then goto MSGBOX1 else goto L780
00750 MSGBOX1: ! 
00760   mat ml$(3) !:
        ml$(1)="You are changing Ratio # "&holdhac$&" to " !:
        ml$(2)="Ratio # "&hac$&".  Click OK to continue, " !:
        ml$(3)="else Cancel to prevent changing the #." !:
        fnmsgbox(mat ml$,resp$,cap$,49)
00770   if resp$="OK" then goto L780 else goto ADD_EDIT_RATIOMST
00780 L780: rewrite #ratiomst,using 'Form POS 1,G 3,C 40,80*c 12',rec=editrec: hac$,na$,mat gl$
00790   goto L830
00800 ! ______________________________________________________________________
00810 WRITE_NEW_RATIOMST: write #ratiomst,using 'Form POS 1,G 3,C 40,80*c 12',rec=editrec: hac$,na$,mat gl$
00820   new1=1
00830 L830: goto RATIOMSTGRID
00840 ! ______________________________________________________________________
00850   close #ratiomst: 
00860   if new1=1 then gosub L940
00870   goto XIT
00880 ! ______________________________________________________________________
00890 CREATE_FILES: ! 
00900   close #ratiomst: ioerr L910
00910 L910: open #ratiomst: "Name="&env$('Q')&"\GLmstr\RatioMST.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\RatioIDX.h"&str$(cno),internal,outin,keyed ioerr L930
00920   close #ratiomst,free: ioerr L930
00930 L930: open #ratiomst: "Name="&env$('Q')&"\GLmstr\ratiomst.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\ratioidx.h"&str$(cno)&",RecL=1163,KPs=1,KLn=3,replace",internal,outin,keyed 
00940 L940: close #ratiomst: ioerr L950
00950 L950: close #11: ioerr L970
00960 INDEX: ! (main Ratio files)
00970 L970: execute "Index "&env$('Q')&"\GLmstr\RatioMST.h"&str$(cno)&' '&env$('Q')&"\GLmstr\RatioIDX.h"&str$(cno)&" 1 3 Replace DupKeys -n"
00980   return 
00990 ! ______________________________________________________________________
01000 PROOF: restore #ratiomst,key>="   ": eof L1010 ioerr RATIOMSTGRID
01010 L1010: on fkey 5 goto L1330
01020   fnopenprn
01030 L1030: read #ratiomst,using 'Form POS 1,G 3,C 40,80*c 12',key=ac$: hac$,na$,mat gl$ eof L1330
01040   pr #255,using L1050: date$('mm/dd/yy'),time$,"Print Ratio File Proof List"
01050 L1050: form skip 1,pos 1,c 8,skip 1,pos 1,c 8,pos 51,c 31,skip 1
01060   pr #255,using L1070: cnam$,dat$
01070 L1070: form pos 1,cc 122,skip 1,pos 1,cc 122,skip 2
01080   pr #255,using L1090: "ratiomst Number",sn
01090 L1090: form pos 1,c 15,pos 20,pic(zz),skip 1
01100   pr #255,using L1110: "ratiomst Name  ",schnam$
01110 L1110: form pos 1,c 15,pos 20,c 80,skip 1
01120   pr #255,using L1110: "FootNote       ",ft$
01130   pr #255,using L1160: "Dollar Sign Print",dp
01140   pr #255,using L1160: "Reverse Sign",rs
01150   pr #255,using L1160: "Print Current Month Figures",cm
01160 L1160: form pos 1,c 27,pos 30,pic(#),skip 1
01170   pr #255: tab(29);"Dept  Account Sub"
01180   for j=1 to 80
01190     if gl$(j)="  0     0  0" then goto L1270
01200     if j1><48 then goto L1250
01210     pr #255: newpage
01220     pr #255,using L1230: "G/L Account Number",gl$(j)(1:3),gl$(j)(4:9),gl$(j)(10:12)
01230 L1230: form skip 6,pos 1,c 18,pos 30,c 3,x 2,c 6,x 2,c 3,skip 1
01240     goto L1270
01250 L1250: pr #255,using L1260: "G/L Account Number",gl$(j)(1:3),gl$(j)(4:9),gl$(j)(10:12)
01260 L1260: form pos 1,c 18,pos 30,c 3,x 2,c 6,x 2,c 3,skip 1
01270 L1270: j1=j1+1
01280   next j
01290   j1=0
01300   pr #255: newpage
01310   goto L1030
01320 ! ______________________________________________________________________
01330 L1330: fncloseprn
01340   on fkey 5 ignore 
01350   if fnprocess=1 then goto XIT
01360   goto ADD_EDIT_RATIOMST
01370 ! ______________________________________________________________________
01380 L1380: if err=4152 then goto L930 else goto ERTN
01390 ! ______________________________________________________________________
01400 XIT: fnxit
01410 ! ______________________________________________________________________
01420 ! ______________________________________________________________________
01430 ! <Updateable Region: ERTN>
01440 ERTN: fnerror(program$,err,line,act$,"xit")
01450   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01460   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01470   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01480 ERTN_EXEC_ACT: execute act$ : goto ERTN
01490 ! /region
01500 ! ______________________________________________________________________
01510 GL_NUMBERS: ! 
01520 LEFT_SIDE: ! 
01530   fntos(sn$="Ratiomst3") !:
        resp=0
01540   fnlbl(1,35,"Left Side Of Ratio",30,0)
01550   mypos(1)=1: mypos (2)=50
01560   for j=2 to 40 step 2
01570     for x=1 to 2
01580       fnqgl(j/2+1,mypos(x),0,2,pas) !:
            if x =1 then resp$(resp+=1)=fnrgl$(gl$(j-1)) else resp$(resp+=1)=fnrgl$(gl$(j))
01590     next x
01600   next j
01610   fncmdkey("&Left Side",2,0,0,"Enter all G/L Numbers to be used on the left side of the ratio.")
01620   fncmdkey("&Rignt Side",3,0,0,"Enter all G/L Numbers to be used on the right side of the ratio.")
01630   fncmdkey("&Finished",6,0,0,"Finished with general ledger assignments.")
01640   fncmdkey("E&xit",5,0,1,"Exits to main menu")
01650   fnacs(sn$,0,mat resp$,ckey)
01660   if ckey=5 then goto RATIOMSTGRID
01670   for j=1 to 40
01680     gl$(j)=fnagl$(resp$(j))
01690   next j
01700   if ckey=2 then goto LEFT_SIDE
01710   if ckey=3 then goto RIGHT_SIDE
01720   if ckey=6 then goto REWRITE_EXISTING_RATIOMST
01730 RIGHT_SIDE: : resp=0
01740   fntos(sn$="Ratiomst4")
01750   mypos(1)=1: mypos (2)=50
01760   fnlbl(1,35,"Right Side Of Ratio",30,0)
01770   for j=2 to 40 step 2
01780     for x=1 to 2
01790       fnqgl(j/2+1,mypos(x),0,2,pas) !:
            if x=1 then resp$(resp+=1)=fnrgl$(gl$(40+j-1)) else resp$(resp+=1)=fnrgl$(gl$(40+j))
01800     next x
01810   next j
01820   fncmdkey("&Left Side",2,0,0,"Enter all G/L Numbers to be used on the left side of the ratio.")
01830   fncmdkey("&Rignt Side",3,0,0,"Enter all G/L Numbers to be used on the right side of the ratio.")
01840   fncmdkey("&Finished",6,0,0,"Finished with general ledger assignments.")
01850   fncmdkey("E&xit",5,0,1,"Exits to main menu")
01860   fnacs(sn$,0,mat resp$,ckey)
01870   if ckey=5 then goto RATIOMSTGRID
01880   for j=1 to 40
01890     gl$(j+40)=fnagl$(resp$(j))
01900   next j
01910   if ckey=2 then goto LEFT_SIDE
01920   if ckey=3 then goto RIGHT_SIDE
01930   if ckey=6 then goto REWRITE_EXISTING_RATIOMST
01940   goto RATIOMSTGRID
