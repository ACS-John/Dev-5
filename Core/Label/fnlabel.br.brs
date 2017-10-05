00010 ! Replace S:\Core\Label\fnLabel
00020 ! pr label library, finds out how to format the labels and then !:
        ! prints them using PrintAce so bar codes pr right
00030   def library fnlabel(win,&cap$,mat linestyle$,cp,nw)
00040     library 'S:\Core\Library': fnerror,fnwait,fnopenprn,fncloseprn, fnacs,fnlbl,fntos,fnbutton,fnfra,fntxt,fncmdset,fncomboa,fnxit,fnreg_read,fnreg_write,fnpa_finis,fnpa_newpage,fnpa_open
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070     dim label_hold_cap$*128
00080     dim labeltext$(5)*120,wabel$(10,3,5)*120,opt$(3)*43,resp$(10)*80
00082     fn_test_mat_lab$
00090 ! wabel$(10,3,5) wabel$(x,y,z) wabel$(left/right,Up/Down,LabelLine)
00100 ! ______________________________________________________________________
00110 ! win= window number to use for windows !:
          ! cap$= referring programs cap$ (don't modify, just include) !:
          ! LabelText$(x)= each line of label text, 4 lines per label !:
          ! linestyle$(x) indicates which hex code to use on that line of pr !:
          ! (this linestyle feature is not yet written !:
          ! CP = 1=Condensed pr 2 or 0=Normal Print
00140 ! if uprc$(linestyle$(5))="BAR" then label_pos1=01 : label_pos2=75 : label_pos3=150 else label_pos1=02 : label_pos2=30 : label_pos3=63
00150 ! top_marg=2
00160     label_hold_cap$=cap$
00170     cap$=cap$&" Label Printing"(1:udim(cap$))
00180 ! _*_*_*_____________________
00190 L190: gosub ASK_LABEL_FORMAT
00200     if ckey=5 then goto LABEL_XIT
00210     if label_format=1 then !:
            gosub L1
00220     if label_format=2 then 
00230       gosub ASK_LABEL_FORMAT_2_LSTART
00240       if ckey=5 then goto L190
00250       gosub ASK_L2_MARGINS
00260       if ckey=5 then goto L190
00270       gosub L2
00280     end if 
00290     goto LABEL_XIT
00300 ! ______________________________________________________________________
00310 ASK_LABEL_FORMAT: ! r:
00320     fntos(sn$="labellib1")
00330     fnlbl(1,1,"Label Format:",15,1)
00340     opt$(1)="Avery 8160 (30/Page Ink Jet)" !:
          opt$(2)="Avery 5160 (30/Page Laser)" !:
          opt$(3)="Universal Data Processing (1-Up Dot-Matrix)" !:
          fncomboa("labellib1."&wsid$,1,17,mat opt$,"Choose either dot-matrix or laser type labels",32) !:
          resp$(1)=opt$(1)
00350     fncmdset(2)
00360     fnacs(sn$,win,mat resp$,ckey)
00370     if ckey=5 then goto XIT_ASK_LABEL
00380     if resp$(1)=opt$(3) then !:
            label_format=1 else label_format=2
00390 XIT_ASK_LABEL: return  ! /r
00400 ! ______________________________________________________________________
00410 ASK_LABEL_FORMAT_2_LSTART: ! r:
00420     fntos(sn$="labellib2")
00430     fnfra(1,2,10,28,"Select First Label to Print","If you only have a partial sheet of labels, you can select the starting place of the first label") !:
          myline=1: mypos=1
00440     for j=1 to 30
00450       fnbutton(myline,mypos,"  "&cnvrt$("pic(zz)",j)&"  ",j+20,"Starts with label "&str$(j),0,6,1)
00460       if j/3=int(j/3) then myline+=1
00470       mypos+=10
00480       if mypos>30 then mypos=1
00490     next j
00500     fncmdset(1)
00510     fnacs(sn$,win,mat resp$,ckey)
00520     if ckey=5 then goto XIT_ASK_LABEL_2
00530     lstart=ckey-21 ! because the other routine starts out adding one.
00540     labx=int((lstart+2)/3) !:
          laby=lstart-((labx-1)*3)
00550 XIT_ASK_LABEL_2: ! 
00552     return  ! /r
00570 ASK_L2_MARGINS: ! r:
00572     fnreg_read('top_marg',top_marg$) : top_marg=val(top_marg$) conv ignore
00573     fnreg_read('label_pos1',label_pos1$) : label_pos1=val(label_pos1$) conv ignore
00574     fnreg_read('label_pos2',label_pos2$) : label_pos2=val(label_pos2$) conv ignore
00575     fnreg_read('label_pos3',label_pos3$) : label_pos3=val(label_pos3$) conv ignore
00576     if top_marg=0 and label_pos1+label_pos2+label_pos3=0 then 
00577       if uprc$(linestyle$(5))="BAR" then 
00578         label_pos1=01 : label_pos2=75 : label_pos3=150
00579       else 
00580         label_pos1=02 : label_pos2=30 : label_pos3=63
00581       end if 
00582       top_marg=2
00583     end if  ! top_marg=0 and label_pos1+label_pos2+label_pos3=0
00584     sn$="labellib3" !:
          fntos(sn$)
00590     fnlbl(1,1,"Top Margin (lines):",24,1)
00600     fntxt(1,26,3,3,1,'20',0,"Increase or decrease the top margin to move the pr up or down on the labels") !:
          resp$(1)=str$(top_marg)
00610     fnlbl(2,1,"Left Column Position:",24,1)
00620     fntxt(2,26,3,3,1,'20',0,"Increase or decrease the position to move the left label right or left") !:
          resp$(2)=str$(label_pos1)
00630     fnlbl(3,1,"Center Column Position:",24,1)
00640     fntxt(3,26,3,3,1,'20',0,"Increase or decrease the position to move the center label right or left") !:
          resp$(3)=str$(label_pos2)
00650     fnlbl(4,1,"Right Column Position:",24,1)
00660     fntxt(4,26,3,3,1,'20',0,"Increase or decrease the position to move the right label right or left") !:
          resp$(4)=str$(label_pos3)
00670     fncmdset(2)
00680     fnacs(sn$,win,mat resp$,ckey)
00690     top_marg=val(resp$(1)) !:
          label_pos1=val(resp$(2)) !:
          label_pos2=val(resp$(3)) !:
          label_pos3=val(resp$(4))
00692     if ckey<>5 then 
00694       fnreg_write('top_marg',str$(top_marg))
00696       fnreg_write('label_pos1',str$(label_pos1))
00698       fnreg_write('label_pos2',str$(label_pos2))
00700       fnreg_write('label_pos3',str$(label_pos3))
00702     end if 
00708     return  ! /r
00720 LABEL_DONE: ! r:
00730     if uprc$(linestyle$(5))="BAR" then goto RELEASE_PRINT ! bar code to PrintAce
00740     close #win: ioerr ignore
00750     fncloseprn
00760     close #88: 
00770     return  ! /r
00860 L2: ! 
00870     gosub OPEN_LABEL_WSID
00880 L2_NEXT: ! 
00890     mat wabel$=("") : fn_test_mat_lab$
00900 L900: read #88,using "Form POS 1,5*C 120": mat labeltext$ eof L2_DONE
00910 ! wabel$(10,3,4) wabel$(x,y,z) wabel$(Up/Down,left/right,LabelLine)
00920     laby=laby+1
00930     if laby>3 then laby=1 : labx=labx+1
00940     if labx>10 or labx<1 then labx=1
00950     for j=1 to 5
00952       wabel$(labx,laby,j)=labeltext$(j)
00954     next j
00960     if laby=>3 and labx=>10 then 
00962       laby=0 : labx=0
00964       gosub L2_PRINT : goto L2_NEXT
00968     end if  ! laby=>3 and labx=>10
00970     goto L900
01110 L2_DONE: ! 
01120     gosub L2_PRINT
01130     goto LABEL_DONE
01140 ! ___________________________
01150 L2_PRINT: ! 
01160     if uprc$(linestyle$(5))="BAR" then goto BARCODE_PRINT
01170     fnopenprn
01180     fnwait(win,cap$,"Printing: Please wait...",1)
01190     if top_marg>0 then !:
            pr #255,using "Form POS 1,C 1,SKIP "&str$(top_marg): ""
01200     for x=1 to 10
01210       for z=1 to 5
01220         for y=1 to 3
01230 !     If UPRC$(LINESTYLE$(Z))="BAR" AND wabel$(X,Y,Z)<>"" Then !:
                !     pRINTEDABARCODE=1 !:
                !     fnBARCODE(wabel$(X,Y,Z),LABEL_POS(Y))
01240 !     If wabel$(X,Y,Z)<>"" Then Let FNBARCODE(wabel$(X,Y,Z),LABEL_POS(Y))
01250         next y
01260 ! 
01270         if printedabarcode=1 then pr #255: "" !:
                printedabarcode=0 !:
                goto L1310
01280 !   If LINESTYLE$(Z)<>"" Then Let FNSETLINESTYLE(LINESTYLE$(Z))
01290         pr #255,using L1300: wabel$(x,1,z)(1:25),wabel$(x,2,z)(1:25),wabel$(x,3,z)(1:25)
01300 L1300:  form pos label_pos1,c 25,pos label_pos2,c 25,pos label_pos3,c 25
01310 L1310: next z
01320       if x<10 then pr #255: "" : pr #255: ""
01330       if x=10 then pr #255: newpage
01340     next x
01350     return 
01360 ! 
01370 ! 
01380 ! 
01390 BARCODE_PRINT: ! 
01400     addy=4
01410     gosub VBOPENPRINT
01420     if top_marg>0 then !:
            pr #20: 'Call Print.AddText(" ",'&str$(label_pos3)&','&str$(top_marg*(addy)+ymargin)&')'
01430     for x=1 to 10
01440       for z=1 to 5
01450         if z=5 then goto L1460 else goto L1560
01460 L1460:  lyne+=addy
01470         for j=1 to 3
01480           v=val(wabel$(x,j,z)) conv L1560
01490           bc$=trim$(wabel$(x,j,z))
01500           if j=1 then label_pos =0
01510           if j=2 then label_pos =2.75
01520           if j=3 then label_pos =5.25
01530           if trim$(bc$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(label_pos)&','&str$(x+.1)&',"'&bc$&'")'
01540         next j
01550         goto L1580
01560 L1560:  pr #20: 'Call Print.AddText("'&trim$(wabel$(x,1,z))&'",'&str$(label_pos1)&','&str$(lyne+=addy+ymargin)&')' !:
              pr #20: 'Call Print.AddText("'&trim$(wabel$(x,2,z))&'",'&str$(label_pos2)&','&str$(lyne+ymargin)&')' !:
              pr #20: 'Call Print.AddText("'&trim$(wabel$(x,3,z))&'",'&str$(label_pos3)&','&str$(lyne+ymargin)&')'
01570 !      Form POS LABEL_POS1,C 25,POS LABEL_POS2,C 25,POS LABEL_POS3,C 25
01580 L1580: next z
01590       if x<10 then lyne+=addy*1.9
01600       if x=10 then let fnpa_newpage
01610     next x
01620     return 
01630 ! ______________________________________________________________________
01640 L1: ! 
01650     fnopenprn
01660     fnwait(win,cap$,"Printing: Please wait...",1)
01670     gosub OPEN_LABEL_WSID
01680 L1_NEXT: ! 
01690     read #88,using "Form POS 1,5*C 120": mat labeltext$ eof L1_DONE
01710     for z=1 to 5
01720 !   If UPRC$(LINESTYLE$(Z))="BAR" AND LABELTEXT$(Z)<>"" Then !:
            !   fnBARCODE(LABELTEXT$(Z),LABEL_POS1) : pr #255: "" !:
            !   Goto 1400
01730 !   If LINESTYLE$(Z)<>"" Then Let FNSETLINESTYLE(LINESTYLE$(Z))
01740       pr #255,using L1750: labeltext$(z)(1:70)
01750 L1750: form pos 2,c 70
01760     next z
01770     pr #255: ""
01780     goto L1_NEXT
01860 L1_DONE: ! 
01870     goto LABEL_DONE
01880 ! ______________________________________________________________________
01890 OPEN_LABEL_WSID: ! r:
01900     open #88: "Name="&env$('temp')&"\Label.dat,RecL=600,Use",internal,outin ioerr XNOW
01910     return  ! /r
01920 ! ______________________________________________________________________
01930 LABEL_XIT: ! 
01940     close #88: ioerr ignore
01950     execute "Free "&env$('temp')&"\Label.dat -n" ioerr XNOW
01960 XNOW: ! 
01970     cap$=label_hold_cap$
01980   fnend 
01990 ! ______________________________________________________________________
02000 VBOPENPRINT: ! 
02010   fnpa_open ! if file(20)=-1 then
02020 !   open #20: "Name="&env$('Q')&"\UBmstr\label"&wsid$&".txt,Replace,RecL=5000",display,output
02030 !   pr #20: 'Call Print.MyOrientation("Portrait")'
02032   pr #20: 'Call Print.MyFontsize(12)'
02040   lyne=4
02050 ! end if
02060   return 
02070 ! ______________________________________________________________________
02080 RELEASE_PRINT: ! 
02090   close #1: ioerr ignore
02100   close #3: ioerr ignore
02120   fnpa_finis
02140 XIT: fnxit
20200   def fn_test_mat_lab$
20220     if udim(wabel$,1)<>10 or udim(wabel$,2)<>3 or udim(wabel$,3)<>5 then 
20240       mat wabel$(10,3,5)
20260     end if  ! udim(wabel$,1)<>10 or udim(wabel$,2)<>3 or udim(wabel$,3)<>5
20280   fnend  ! fn_test_mat_lab$
75000 IGNORE: continue 
76020 ! r:  ertn - fails to label_xit instead of xit
76040 ERTN: fnerror(program$,err,line,act$,"label_xit")
76060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
76080   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
76100   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
76120 ERTN_EXEC_ACT: execute act$ : goto ERTN
76140 ! /r
