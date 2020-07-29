00010 ! REPLACE RTFLIB_dll.br
00019   library "RTFLIB_dll.br": fnrtf,fnamt$,fntext$
00020   dim types$(4)*2,styles$(4)*1000,data$(20)*1200,sub$(1)*1000
00030   rtffile=126 !:
        open #rtffile: "name=c:\temp.rtf,recl=1000,replace",display,output 
00040   types$(1)="H" !:
        types$(2)="F" !:
        types$(3)="D" !:
        types$(4)="T"
00050   styles$(1)="li0|ri0|fs18|cfBlue|tc3.25|Header"
00060   styles$(2)="li0|ri0|fs8|cfRed|tc3.25|Footer"
00070   styles$(3)="li0.25|ri0|fs10||tl2.5|td3|tl3.2|td4.0|tl4.2|td4.6|tl4.8|td5.4|Data"
00075   mask$="pic(ZZZ,ZZ#)"
00080   styles$(4)="li0.5|ri0|fs10|tl2.5|td3|tl3.2|td4.0|tl4.2|td4.6|tl4.8|td5.4|Totals"
00090   data$(1)="H|\b\tab Title Of Report"
00100   data$(2)="F|\tab Page Footer"
00105   data$(3)="D|Description |"&fnamt$(1000,mask$,"$")&"|"&fnamt$(2000,mask$,"$")&"|"&fnamt$(3000,mask$,"$")&"|"&fnamt$(4000,mask$,"$")
00110   for a=4 to 17
00120     if mod(a,2) then data$(a)="D|Description |"&fnamt$(1000,mask$)&"|"&fnamt$(2000,mask$)&"|"&fnamt$(3000,mask$)&"|"&fnamt$(4000,mask$) else data$(a)="D|Description |"&fnamt$(-1000,mask$)&"|"&fnamt$(-2000,mask$)&"|"&fnamt$(-3000,mask$)&"|"&fnamt$(-4000,mask$)
00130   next a
00135   data$(18)="D|"&fntext$("Description that is longer than the allowed size of the data space and needs to be out on multiple lines",30)&"|"&fnamt$(1000,mask$," ","s")&"|"&fnamt$(2000,mask$," ","s")&"|"&fnamt$(3000,mask$," ","s")&"|"&fnamt$(4000,mask$," ","s")
00140   data$(19)="T|Total|"&fnamt$(18000,mask$,"$","d")&"|"&fnamt$(36000,mask$,"$","d")&"|\cfRed "&fnamt$(54000,mask$,"$","d")&"|"&fnamt$(72000,mask$,"$","d")
00150   fnrtf(mat types$,mat styles$,mat data$,rtffile)
00157   dim rtffile$*250
00158   rtffile$=file$(rtffile)
00159   close #rtffile: 
00160   execute "sys "&env$("PD")&"spoolbat.bat "&rtffile$&" WORD"
00170   stop 
00180 INIT: ! 
00199 ! 
00200 ! 
00201 ! 
00210   datfmt$="MM-DD-CCYY" !:
        maxsrows=22 !:
        ssav=103 !:
        windev=owindev=69 !:
        mga$="24,2,c 78," !:
        pfk=23 !:
        ! Common Variables Almost Always Required By Fnsnap
00220   pgup=90 : pgdn=91 : event=98 !:
        esc=99 : up=102 : left=103 !:
        dn=104 : home=112 !:
        end=113
00221   click=201 : dblclick=202 !:
        help=100 : rtclick=100 !:
        rtdblclick=100
00222   upfld=105 : dnfld=106 : foflow=107 !:
        right=109 : left=108 : home=112 !:
        end=113 : fldplus=114 : fldminus=115
00500   return 
09000   def library fnextract(winno,spec$;header$*200,footer$*200,title$*200,mat select,nolines,nosort,nototals$*200,subtotal) !:
          ! -------------------------------- !:
          ! ?alls FNLISTPRINT.  The function name was changed after it was!:
          ! ?reated.  This function maintains compatability               !:
          ! -------------------------------- !
09010     library env$("PD")&"Core\fnsnap\rtflib_dll.br": fnlistprint
09020     fnlistprint(winno,spec$,header$,footer$,title$,mat select,nolines,nosort,nototals$,subtotal)
09030   fnend 
09999 ! ================================================================
10000   def library fnlistprint(winno,spec$*100;header$*200,footer$*200,title$*200,mat selected,nolines,nosort,nototals$*200,nosubtotal,print) !:
          ! -------------------------------- !:
          ! ?Extract information from a listbox and pr the information !:
          ! ?  in an RTF report using Word                                !:
          ! ?WINNO the window number in which the listbox appears         !:
          ! ?SPEC$ the identifier such as "1,1 LIST 5/25" for the list    !:
          ! ?HEADER$ the text to use as a page header on each page        !:
          ! ?FOOTER$ the text to use as a page footer in additon to page #!:
          ! ?   and report date                                           !:
          ! ?MAT SELECTED the separate array if any used to flag selected !:
          ! ?   elements                                                  !:
          ! ?NOLINES if true suppresses lines around cells                !:
          ! ?NOSORT if true ignores resorted sequence                     !:
          ! ?NOTOTALS$ string of 0/1/Xs 1 will surpress total pr       !:
          ! ?   X will suppress the printing of the column                !:
          ! ?NOSUBTOTAL if true prevents printing of subtotals            !:
          ! --------------------------------
10001     library 'S:\Core\Library': fnget_wordprocessor_exe
10002     fnget_wordprocessor_exe(word_processor_exe$, 'word')
10004     dim mprint$*50,word_processor_exe$*256
10005     if pr then mprint$=" /q /n /mFilePrintDefault /mFileCloseOrExit" else mprint$=""
10010     library env$("PD")&"Core\fnsnap\RTFLIB_dll.br": fnrtf,fnamt$,fntext$
10020     library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnmsexe$,fngethandle,fndelrow,fndelrow$,fn_cs,fnCopyc2s,fnCopys2c,fnwaitwin
10030     dim theaders$(1)*50,headers$(1)*50,widths(1),forms$(1)*50,aa$(1)*50,ab$(1)*50,ba$(1)*50,bb$(1)*50,cc$(1)*3000,lastcc$(1)*3000,ccs$(1)*3000,ccst$(1)*3000,ccs$*2000,ccb$*2000,headers$*2000,cc$*20000,dorder(1),sorder(1),exclude(1),outfile$*100,dummy$*2000
10040     execute 'proc '&env$("PD")&'Core\fnsnap\tt'
10045     _cs=fn_cs
10050     dim textfile$*100,rtfout$*100
10060     input #winno, fields spec$&",rowcnt,sel,nowait ": selno
10070     if (selno>1 or sum(mat selected)>1) and msgbox("Do you ONLY want to include selected items in your report?","List Report",yn$,"QST")=2 then !:
            selected=1 !:
          else selected=0
10072     input #winno, fields spec$&",MASK, nowait": mat mask_array
10073     if sum(mat mask_array)>0 then masked=1 else masked=0
10080 ! -------------------------------- !:
          ! ?Read the Header arrays                                       !:
          ! ?                                                             !:
          ! -------------------------------- !
10090     input #winno, fields spec$&",HEADERS,,nowait ": (mat headers$,mat widths,mat forms$)
10095     x=udim(mat forms$) !:
          for a=1 to x !:
            forms$(a)=trim$(forms$(a)) !:
          next a
10096     nototals$=rpad$(uprc$(nototals$),x,"0")
10100 ! -------------------------------- !:
          ! ?Get the order in which records are displayed                 !:
          ! ?Get the order in which the columns were sorted               !:
          ! -------------------------------- !
10110     input #winno, fields spec$&",rowsub,all,displayed_order,nowait": mat dorder
10112     input #winno, fields spec$&",sort_order,all,nowait": mat sort_order !:
          ! -------------------------------- !:
          ! ?Get the order in which columns have been sorted and displayed!:
          ! ?                                                             !:
          ! -------------------------------- !
10114     for a=1 to udim(mat sort_order) : sort_order(a)=abs(sort_order(a)) : next a
10120     x=udim(dorder) !:
          mat sorder(x) !:
          for a=1 to x : sorder(a)=a : next a
10125     if nosort then mat dorder=sorder
10130     exclude=0
10140 ! -------------------------------- !:
          ! ?Determine if any columns are zero width or any headers are   !:
          ! ?blank or flagged to be excluded by an X in the nototals$     !:
          ! ?variable.  These will not be included in the report          !:
          ! -------------------------------- !
10150     input #winno, fields spec$&",colcnt,all,nowait": lcols
10160     for a=lcols to 1 step -1
10170       if widths(a)<=0 or trim$(headers$(a))<="" or uprc$(nototals$(a:a))="X" then 
10180         exclude+=1 : mat exclude(exclude)
10190         exclude(exclude)=a
10200       end if 
10210     next a
10215 ! -------------------------------- !:
          ! ?Remove any excluded columns                                  !:
          ! ?                                                             !:
          ! -------------------------------- !
10220     if exclude then for a=1 to exclude : fndelrow$(mat headers$,exclude(a)) : next a
10230     if exclude then for a=1 to exclude : fndelrow(mat widths,exclude(a)) : next a
10240     if exclude then for a=1 to exclude : fndelrow$(mat forms$,exclude(a)) : next a
10245     if exclude then for a=1 to exclude : fndelrow(mat sort_order,exclude(a)) : next a
10246     if sum(mat sort_order)>0 then !:
            sort_col=srch(mat sort_order,1) else !:
            sort_col=0 !:
            ! -------------------------------- !:
            ! ?et a flag to indicate what column was last chosen for sort   !:
            ! ?equence.  This will be used for sub-total breaks             !:
            ! -------------------------------- !
10250     lcols=udim(mat headers$) !:
          mat cc$(lcols)=("") !:
          mat col_format(lcols)=(0) !:
          mat col_total(lcols)=(0) !:
          mat col_subtotal(lcols)=(0)
10260     gosub SET_FORMAT
10265 ! -------------------------------- !:
          ! ?nly selected rows are to pr                               !:
          ! ?                                                             !:
          ! -------------------------------- !
10270     if selected or masked then 
10275 ! -------------------------------- !:
            ! ?etermine if external selected array or Windows selection     !:
            ! ?as used - MAT SELECTED is external array                     !:
            ! -------------------------------- !
10280       x=udim(mat selected)
10290       if not x or not sum(mat selected) then 
10295         mat asel(0)
10300         input #winno, fields spec$&",rowcnt,sel,nowait": asel !:
              if asel>1 then mat asel(asel) !:
                input #winno, fields spec$&",rowsub,sel,nowait": mat asel
10302         if (masked and not sum(asel)) then 
10303           am=0
10304           mat asel(sum(mat mask_array))
10305           for a=1 to udim(mask_array)
10306             if mask_array(a)>0 then !:
                    am+=1 !:
                    asel(am)=a
10307           next a
10309         end if 
10310       else 
10320         asel=sum(selected) !:
              mat asel(asel) !:
              as=0
10330         for a=1 to x
10331           if selected(dorder(a))>0 then as+=1 !:
                  asel(as)=dorder(a)
10332         next a
10340       end if 
10350     end if 
10360     aa=0
10365     waitwin=fnwaitwin("Creating RTF Report - Please wait")
10370 ! -------------------------------- !:
          ! ?tart writing RTF text file                                   !:
          ! ?                                                             !:
          ! -------------------------------- !
10380     open #(textfile:=fngethandle): "name=[Temp]\text"&session$&".txt,recl=5000,replace",display,output 
10385     if sort_col>0 then header$(inf:inf)="[RTFLINE] Sorted by "&headers$(sort_col)
10390     pr #textfile: "H|[SPEC(spec"&session$&".spc)]"&header$&"[BOTLINE]" !:
          pr #textfile: "F|"&footer$&"|Page [PAGE] |[RTFDATE][TOPLINE]" !:
          pr #textfile: "T| "&title$&"[RTFLINE]"
10393     mat theaders$(udim(headers$)) !:
          mat theaders$=headers$
10395     if sort_col>0 and nototals$(sort_col:sort_col)="2" then let fndelrow$(mat theaders$,sort_col)
10400     mat2str(mat theaders$,headers$," | ")
10410     pr #textfile: "1| "&headers$
10420 !:
          ! -------------------------------- !:
          ! ?rite detail lines to RTF text file                           !:
          ! ?                                                             !:
          ! -------------------------------- !
10430 START_SELECTED: ! 
10440     aa+=1
10450     if selected+masked and aa>udim(mat dorder) then mat cc$(0) : goto 10580 else !:
            if not selected+masked and aa>udim(dorder) then mat cc$(0) : goto 10580
10455 ! -------------------------------- !:
          ! ?nly selected rows will pr                                 !:
          ! ?                                                             !:
          ! -------------------------------- !
10460     if selected+masked then 
10461       if srch(mat asel,dorder(aa))>0 then 
10462         mat start(1): mat end(1) : mat start=(dorder(aa)) : mat end=start !:
              input #winno, fields spec$&",row,range,nowait": mat start,mat end,(mat cc$)
10463       else 
10464         goto START_SELECTED
10465       end if 
10466     end if 
10467 ! 
10469 ! -------------------------------- !:
          ! ?emove excluded columns from detail                           !:
          ! ?                                                             !:
          ! -------------------------------- !
10470     if not selected+masked then !:
            mat start(1): mat end(1) : mat start=(dorder(aa)) : mat end=(dorder(aa)) !:
            input #winno, fields spec$&",row,range,nowait": mat start,mat end,(mat cc$) !:
            mat ccl$(udim(mat cc$))
10475 ! -------------------------------- !:
          ! ?ll rows will pr                                           !:
          ! ?                                                             !:
          ! -------------------------------- !
10480     if exclude then for a=exclude to 1 step -1 : fndelrow$(mat cc$,exclude(a)) : next a
10485 ! -------------------------------- !:
          ! ?ormat cells for each column                                  !:
          ! ?                                                             !:
          ! -------------------------------- !
10490     for aaa=1 to udim(mat cc$)
10495 ! -------------------------------- !:
            ! ?heck numeric columns and add to column total if no conversion!:
            ! ?rror.  If a conversion error occurs mark the column as string!:
            ! ?o that no total will appear and no further conversion testing!:
            ! ?ill be done                                                  !:
            ! -------------------------------- !
10497       if aaa<=udim(col_format) then 
10500         if not col_format(aaa) then !:
                numeric=val(srep$(trim$(cc$(aaa)),"$","")) conv SET_NUM
10505 ! -------------------------------- !:
              ! ?ormat the numeric column if conversion works                 !:
              ! ?                                                             !:
              ! -------------------------------- !
10510         if not col_format(aaa) and trim$(hforms$(aaa))>"" then 
10512           cc$(aaa)=cnvrt$(hforms$(aaa),val(srep$(trim$(cc$(aaa)),"$",""))) error ignore
10514         end if 
10515 ! -------------------------------- !:
              ! ?ormat dates                                                  !:
              ! ?                                                             !:
              ! -------------------------------- !
10520         if col_format(aaa) and trim$(hforms$(aaa))(1:4)="DATE" then execute "LET CC$(AAA)=date$(val(TRIM$(cc$(aaa))),"&srep$(srep$(hforms$(aaa),'DATE(','"'),')','")')
10525 ! -------------------------------- !:
              ! ?ouble up \ to \\ so that the RTF reader will not mistake it  !:
              ! ?s an RTF command                                             !:
              ! -------------------------------- !
10530         cc$(aaa)=srep$(trim$(cc$(aaa)),"\","\\") ! pause
10535       end if 
10540     next aaa
10545 PRINT_DETAIL: !:
          ! -------------------------------- !:
          ! ?rint the detail elements to the report                       !:
          ! ?                                                             !:
          ! -------------------------------- !
10550     mat2str(mat cc$,cc$," | ") !:
          mat lastcc$(udim(cc$)) !:
          mat dummy$(udim(c$))
10551     if sort_col>0 and sort_count>1 and not cc$(sort_col)=lastcc$(sort_col) then 
10552       gosub PRINT_SUBTOTALS
10553     else 
10554       if not nosort and sort_col>0 and not cc$(sort_col)=lastcc$(sort_col) then 
10555         sort_count=0 !:
              mat col_subtotal=(0)
10556         if nototals$(sort_col:sort_col)="2" then !:
                pr #textfile: "5| "&cc$(sort_col) else !:
                pr #textfile: "5| |"
10558       end if 
10559     end if 
10560     for a=1 to lcols
10561       if not col_format(a) then !:
              col_total(a)+=val(srep$(cc$(a),"$","")) !:
              col_subtotal(a)+=val(srep$(cc$(a),"$",""))
10562     next a
10563     mat lastcc$(udim(cc$)) : mat lastcc$=cc$
10564     if nototals$(sort_col:sort_col)="2" then !:
            fndelrow$(mat cc$,sort_col) !:
            mat2str(mat cc$,cc$," | ") else !:
            mat2str(mat cc$,cc$," | ")
10565     if udim(mat cc$)>0 then pr #textfile: "2| "&cc$&"|" !:
            cc$="" !:
            mat cc$(udim(lastcc$)) !:
            mat cc$=("") !:
            sort_count+=1
10570 ! -------------------------------- !:
          ! ?hen the end of a list is reached the row extraction          !:
          ! ?DIM's to zero                                                !:
          ! -------------------------------- !
10580     if udim(mat cc$)>0 then !:
            goto START_SELECTED
10585 ! -------------------------------- !:
          ! ?eset the CC$ array to the correct number of columns          !:
          ! ?                                                             !:
          ! -------------------------------- !
10590     mat cc$(lcols): mat lastcc$(lcols)
10595     if sort_col>0 and sort_count>1 and not cc$(sort_col)=lastcc$(sort_col) then !:
            gosub PRINT_SUBTOTALS else !:
            if sort_col>0 and not cc$(sort_col)=lastcc$(sort_col) then !:
              sort_count=0 !:
              col_subtotal=0
10596 ! 
10600     for aa=1 to udim(col_total)
10605 ! -------------------------------- !:
            ! ?lear non-numeric column totals                               !:
            ! ?                                                             !:
            ! -------------------------------- !
10607       if col_format(aa)>0 or val(nototals$(aa:aa))>0 then 
10608         cc$(aa)=""
10609       else 
10610         cc$(aa)=str$(col_total(aa))
10615 ! -------------------------------- !:
              ! ?ormat numeric column totals to match data elements           !:
              ! ?                                                             !:
              ! -------------------------------- !
10620         if trim$(hforms$(aa))>"" then 
10622           cc$(aa)=cnvrt$(hforms$(aa),col_total(aa)) error ignore
10623         end if 
10625       end if 
10630     next aa
10632     mat cct$(udim(cc$)) !:
          mat cct$=cc$ !:
          if sort_col>0 and nototals$(sort_col:sort_col)="2" then let fndelrow$(mat cct$,sort_col)
10640     mat2str(mat cct$,cc$," | ")
10650     pr #textfile: "3| "&cc$&"|"
10655     mat col_total=(0) !:
          sort_count=0
10660     textfile$=file$(textfile)
10670     close #textfile: 
10680     open #textfile: "name="&textfile$,display,input 
10682     _seq=-1
10690     _seq+=1: open #(rtfout:=fngethandle): "name=[Temp]\temp"&session$&str$(_seq)&".rtf,eol=none,replace",display,output ioerr 10690
10700     rtfout$=file$(rtfout)
10705 ! close #waitwin:! PAUSE
10710     gosub BUILD_SPEC
10720     fnrtf(textfile,env$("temp")&"\spec"&session$&".spc",rtfout,env$("temp")&"\")
10730     if file(textfile)>-1 then close #textfile: 
10740     if file(rtfout)>-1 then close #rtfout: 
10742     if _cs then let fnCopys2c(rtfout$,outfile$:="temp\temp"&session$&".rtf",1)
10743     close #waitwin: ! PAUSE
10744     if close=1 then mw$=" -M" else mw$=""
10745     dim word$*150
10750     if _cs then 
10751       execute "config shell default client " !:
            word$=fnmsexe$("winword.exe")
10752       execute "sys -w"&mw$&" "&word$&" "&os_filename$("@:"&outfile$)&mprint$
10753       execute "config shell default server"
10754     else 
10755       execute "sys -w"&mw$&" -c "&word_processor_exe$&" "&os_filename$(rtfout$)&mprint$ !        execute "sys -w"&mw$&" -c "&fnmsexe$("winword.exe")&" "&os_filename$(rtfout$)&mprint$
10758     end if 
10760   fnend 
10769 ! =================================================================
10770 SET_NUM: !:
        ! -------------------------------- !:
        ! ?dd 1 to the col_format array to indicate it is NOT a numeric !:
        ! ?rray.  Numeric arrays have a zero col_format number          !:
        ! -------------------------------- !
10780   col_format(aaa)+=1
10790   continue 
10798 ! =================================================================
10800 BUILD_SPEC: ! build an RTF specification file !:
        ! -------------------------------- !:
        ! ?uild an RTF specification file in the %TEMP% directory based !:
        ! ?n the number of columns and text of the lIST to print.       !:
        ! -------------------------------- !
10810   open #(specfile=fngethandle): "name=[Temp]\spec"&session$&".spc,recl=2000,replace",display,output 
10820   pr #specfile: ""
10825 ! -------------------------------- !:
        ! ?et margins                                                   !:
        ! ?                                                             !:
        ! -------------------------------- !
10827   widths=sum(mat widths)
10830   pr #specfile: "let LMARGIN=.50"
10840   pr #specfile: "let RMARGIN=0.75 "
10845 ! -------------------------------- !:
        ! ?et page orientation                                          !:
        ! ?                                                             !:
        ! -------------------------------- !
10850   if widths>100 then !:
          pr #specfile: "let ORIENTATION$='LANDSCAPE'" : landscape=1 else !:
          pr #specfile: "let ORIENTATION$='PORTRAIT'" : landscape=0
10855 ! -------------------------------- !:
        ! ?ight add-in to end of line if style code is E and checklist  !:
        ! ?s true                                                       !:
        ! -------------------------------- !
10860   pr #specfile: "let LEFTTEXT$='Y   N   N/A '"
10870   pr #specfile: "let CHECKLIST=1"
10880   pr #specfile: "let PAPER$='LETTER'"
10885 ! -------------------------------- !:
        ! ?f NUME is true then sequential E styles will automatically   !:
        ! ?e numbered                                                   !:
        ! -------------------------------- !
10890   pr #specfile: "let NUME=0"
10895 ! -------------------------------- !:
        ! ?reate RTF style codes                                        !:
        ! ?                                                             !:
        ! -------------------------------- !
10900   pr #specfile: "MAT TYPES$(11)"
10910   pr #specfile: "LET TYPES$(1)='H'"
10920   pr #specfile: "LET TYPES$(2)='F'"
10930   pr #specfile: "LET TYPES$(3)='D'"
10940   pr #specfile: "LET TYPES$(4)='T'"
10950   pr #specfile: "LET TYPES$(5)='A'"
10960   pr #specfile: "LET TYPES$(6)='B'"
10970   pr #specfile: "LET TYPES$(7)='C'"
10980   pr #specfile: "LET TYPES$(8)='E'"
10990   pr #specfile: "LET TYPES$(9)='G'"
11000   pr #specfile: "LET TYPES$(10)='Y'"
11010   pr #specfile: "LET TYPES$(11)='I'"
11015 ! -------------------------------- !:
        ! ?reate details of RTF styles                                  !:
        ! ?                                                             !:
        ! -------------------------------- !
11020   pr #specfile: "MAT STYLES$(11)"
11030   if widths>100 then !:
          pr #specfile: "LET STYLES$(1)='li0|ri0|fARIAL|fs14|cfBlue|tc5.00|Header'" else !:
          pr #specfile: "LET STYLES$(1)='li0|ri0|fARIAL|fs14|cfBlue|tc3.25|Header'"
11040   if widths>100 then !:
          pr #specfile: "LET STYLES$(2)='li0|ri0|fARIAL|fs8|cfBlack|tc5.00|tr10.0|Footer'" else !:
          pr #specfile: "LET STYLES$(2)='li0|ri0|fARIAL|fs8|cfBlack|tc3.25|tr7.5|Footer'"
11050   pr #specfile: "LET STYLES$(3)='li0.5|QJ|fPALATINO|ri0|fs10|tl0.5|tl1.0|tl1.5|td5.4||Data'"
11060   pr #specfile: "LET STYLES$(4)='li0.5|QC|fARIAL|sa1|ri0|B|fs15|tl0.5||tc3.25|Heading 1'"
11070   pr #specfile: "LET STYLES$(5)='li0.25|ri0|fARIAL|B|fs14|tl0.5||tr5.4|Heading 2'"
11080   pr #specfile: "LET STYLES$(6)='li0.25|ri0|fARIAL|B|fs13|tl0.5||tr5.4|Heading 3'"
11090   pr #specfile: "LET STYLES$(7)='li0.25|ri0|fARIAL|B|fs12|tl0.5||td5.4|Heading 4'"
11100   pr #specfile: "LET STYLES$(8)='fi-0.5|td0.75|li1.0|ri0|fPALATINO|fs09|tl0.5|tl1.0|tc7.0|Yes no Lines'"
11110   pr #specfile: "LET STYLES$(9)='fi-0.4|li1.0|ri0|ft61|fs10|fCOURIER|tl0.5|tc4.0|td5.4|Program lines'"
11120   pr #specfile: "LET STYLES$(10)='fi-0.5|td0.75|li1.0|ri0|fPALATINO|fs12|tl0.5|tl1.0|td6.0|Detail steps'"
11130   pr #specfile: "LET STYLES$(11)='fi-1.25|li2.0|ri0|fPALATINO|fs12|tl2.0|Options'"
11140 ! -------------------------------- !:
        ! ?repare justification codes                                   !:
        ! ? right justify numeric columns                               !:
        ! ? center justify date columns                                 !:
        ! ? left justify string columns                                 !:
        ! -------------------------------- !
11150   mat hj$(udim(mat headers$))
11155 ! -------------------------------- !:
        ! ?repare cell row types                                        !:
        ! ? header row will repeat on each page                         !:
        ! ? detail rows within the table                                !:
        ! ? total row at the bottom of the table                        !:
        ! ? sub-total row                                               !:
        ! -------------------------------- !
11156 ! -------------------------------- !:
        ! ?eader row                                                    !:
        ! ?                                                             !:
        ! -------------------------------- !
11160   pr #specfile: "mat cells$(5)"
11164 ! -------------------------------- !:
        ! ?et smaller font sizes for wider reports                      !:
        ! ?                                                             !:
        ! -------------------------------- !
11165   if widths>180 then fs$="04" : fst$="06" else !:
          if widths>120 then fs$="06": fst$="08" else !:
            fs$="08": fst$="10"
11166 ! -------------------------------- !:
        ! ?emove grid lines if NOLINES is true                          !:
        ! ?                                                             !:
        ! -------------------------------- !
11167   if nolines then !:
          br$="br1" : brt$="brt1|brbd1": brst$="brt1|brb1" else !:
          br$="brltrb1" : brt$="brltr1|brbd2": brst$="brltr1|brb1"
11168   if sort_col>0 and nototals$(sort_col:sort_col)="2" then !:
          delcol=widths(sort_col) else !:
          delcol=0
11170   pr #specfile: "LET CELLS$(1)='li0.1|tg0.100|fPALATINO|fs"&fs$&"|trh|'"
11180   if landscape then page_width=10 else page_width=7.5
11190   for a=1 to udim(mat headers$)
11200     if not (sort_col=a and nototals$(a:a)="2") then pr #specfile: "let cells$(1)(inf:inf)='c"&str$((widths(a)/(widths-delcol))*page_width)&"|brtlrb1|vc|hc|sh15|'"
11210   next a
11215 ! -------------------------------- !:
        ! ?etail row                                                    !:
        ! ?                                                             !:
        ! -------------------------------- !
11220   pr #specfile: "let cells$(2)='li0.1|tg0.100|fPALATINO|fs"&fs$&"|'"
11230   for a=1 to udim(mat headers$)
11240     if pos(uprc$(forms$(a)),"PIC(")>0 or pos(uprc$(forms$(a)),"FMT(")>0 or uprc$(forms$(a)(1:2))="N " or col_format(a)=0 then !:
            hj$(a)="hr" !:
          else if pos(uprc$(forms$(a)),"DATE")>0 then hj$(a)="hc" !:
          else hj$(a)="hl"
11250   if not (sort_col=a and nototals$(a:a)="2") then pr #specfile: "let cells$(2)(inf:inf)='c"&str$((widths(a)/(widths-delcol))*page_width)&"|"&br$&"|vt|"&hj$(a)&"|'"
11260 next a
11262 ! PAUSE
11265 ! -------------------------------- !:
      ! ?otal row                                                     !:
      ! ?                                                             !:
      ! -------------------------------- !
11270 pr #specfile: "LET CELLS$(3)='li0.1|tg0.100|fPALATINO|fs"&fs$&"|'"
11280 brdr$="br1" !:
      brdr$(1)="brtlr1" !:
      brdr$(2)="brbd1"
11290 for a=1 to udim(mat headers$)
11302   if hj$(a)='hr' and not col_format(a) and not pos("12",nototals$(a:a)) then 
11304     if not (sort_col=a and nototals$(a:a)="2") then pr #specfile: "let cells$(3)(inf:inf)='c"&str$((widths(a)/(widths-delcol))*page_width)&"|"&brt$&"|vt|"&hj$(a)&"|'"
11305   else 
11306     if not (sort_col=a and nototals$(a:a)="2") then pr #specfile: "let cells$(3)(inf:inf)='c"&str$((widths(a)/(widths-delcol))*page_width)&"|"&brdr$&"|vt|"&hj$(a)&"|'"
11308   end if 
11310 next a
11320 ! CLOSE #SPECFILE:
11365 ! -------------------------------- !:
      ! ?ub-total row                                                 !:
      ! ?                                                             !:
      ! -------------------------------- !
11370 pr #specfile: "LET CELLS$(4)='li0.1|tg0.100|fPALATINO|fs"&fs$&"|'"
11380 brdr$="br1" !:
      brdr$(1)="brtlr1" !:
      brdr$(2)="brb1"
11390 for a=1 to udim(mat headers$)
11400   if hj$(a)='hr' and not col_format(a) and not pos("12",nototals$(a:a)) then 
11402     if not (sort_col=a and nototals$(a:a)="2") then pr #specfile: "let cells$(4)(inf:inf)='c"&str$((widths(a)/(widths-delcol))*page_width)&"|"&brst$&"|vt|"&hj$(a)&"|'"
11403   else 
11404     if not (sort_col=a and nototals$(a:a)="2") then pr #specfile: "let cells$(4)(inf:inf)='c"&str$((widths(a)/(widths-delcol))*page_width)&"|"&brdr$&"|vt|"&hj$(a)&"|'"
11406   end if 
11410 next a
11420 ! CLOSE #SPECFILE:
11465 ! -------------------------------- !:
      ! ?lank spacer row                                              !:
      ! ?                                                             !:
      ! -------------------------------- !
11470 pr #specfile: "LET CELLS$(5)='li0.1|tg0.100|fPALATINO|fs"&fst$&"|'"
11475 brdr$="br1"
11476 pr #specfile: "let cells$(5)(inf:inf)='c"&str$(page_width)&"|"&brdr$&"|vt|hl|'"
11495 close #specfile: 
11500 return 
11510 ! =================================================================
11520 SET_FORMAT: !:
      ! -------------------------------- !:
      ! ?etermine numeric v. string formats for initial processing    !:
      ! ?ased on FORMS$ array elements                                !:
      ! -------------------------------- !
11530 dim hforms$(1)*50,_b$(1)*50
11540 _h=udim(mat headers$) !:
      mat hforms$(_h)
11550 for a=1 to _h
11555   hforms$(a)=forms$(a)
11560   if uprc$(forms$(a))(1:4)="PIC(" then hforms$(a)=forms$(a)(1:pos(forms$(a),")"))
11570   if uprc$(forms$(a))(1:4)="FMT(" then hforms$(a)=forms$(a)(1:pos(forms$(a),")"))
11580   if uprc$(forms$(a))(1:2)="N " and pos(forms$(a),",")>0 then !:
          hforms$(a)=forms$(a)(1:pos(forms$(a),",")) else !:
          if uprc$(forms$(a))(1:2)="N " then hforms$(a)=forms$(a)
11590   if uprc$(forms$(a))(1:3)="ZD " and pos(forms$(a),",")>0 then !:
          hforms$(a)=forms$(a)(1:pos(forms$(a),")")) else !:
          if uprc$(forms$(a))(1:3)="ZD " then hforms$(a)=forms$(a)
11600 ! IF NOT NOTOTALS$(A:A)="0" THEN cOL_FORMAT(A)+=1
11610   if uprc$(forms$(a))(1:1)="C" then col_format(a)+=1
11620   if uprc$(forms$(a))(1:1)="V" then col_format(a)+=1
11630   if uprc$(forms$(a))(1:1)="G" then col_format(a)+=1
11640   if uprc$(forms$(a))(1:1)="L" then col_format(a)+=1
11650   if uprc$(forms$(a))(1:4)="DATE" then col_format(a)+=1 !:
          hforms$(a)=uprc$(forms$(a)(1:pos(forms$(a),")")))
11651   _p=pos(hforms$(a),")") !:
        _c=pos(hforms$(a),",",-1)
11652   if _c>_p then 
11653     _x=str2mat(hforms$(a),mat _b$,",")
11654     if _x>1 then mat _b$(_x-1)
11655     mat2str(mat _b$,hforms$(a),",")
11656   end if 
11660 next a
11670 return 
11680 ! =================================================================
11685 PRINT_SUBTOTALS: ! 
11690 ! -------------------------------- !:
      ! ?eset the ccs$ array to the correct number of columns          !:
      ! ?                                                             !:
      ! -------------------------------- !
11695 if not nosubtotal then 
11700   mat ccs$(lcols) !:
        mat ccb$(lcols) !:
        mat2str(mat ccb$,ccb$," | ")
11710   for aas=1 to udim(col_total)
11720 ! -------------------------------- !:
          ! ?lear non-numeric column totals                               !:
          ! ?                                                             !:
          ! -------------------------------- !
11730     if col_format(aas)>0 or val(nototals$(aas:aas))>0 then 
11740       ccs$(aas)=""
11750     else 
11760       ccs$(aas)=str$(col_subtotal(aas))
11770 ! -------------------------------- !:
            ! ?ormat numeric column totals to match data elements           !:
            ! ?                                                             !:
            ! -------------------------------- !
11780       if trim$(hforms$(aas))>"" then !:
              ccs$(aas)=cnvrt$(hforms$(aas),col_subtotal(aas)) !:
              col_subtotal(aas)=0
11790     end if 
11800   next aas
11802   mat ccst$(udim(ccs$)) !:
        mat ccst$=ccs$ !:
        if sort_col>0 and nototals$(sort_col:sort_col)="2" then let fndelrow$(mat ccst$,sort_col)
11810   mat2str(mat ccst$,ccs$," | ")
11820   pr #textfile: "4| "&ccs$
11825   sort_count=0 !:
        mat col_subtotal=(0)
11826   mat bb$(udim(mat cc$))=("") !:
        mat2str(mat bb$,ccs$," | ")
11827   if sort_col>0 and nototals$(sort_col:sort_col)="2" then !:
          pr #textfile: "5| "&cc$(sort_col) else !:
          pr #textfile: "5| "&ccs$
11828 end if 
11830 ZPRINT_SUBTOTALS: return 
11990 ! =================================================================
12000 def library fnlistfilter(winno,spec$*100,filter$;nosort,delay) !:
        ! -------------------------------- !:
        ! ?Extract information from a listbox and pr the information !:
        ! ?a new LIST saving the old list to a single cell              !:
        ! ?The old list can be recalled by ESC                          !:
        ! ?WINNO the window number in which the listbox appears         !:
        ! ?SPEC$ the list secifier 1,1,LIST 12/15 for example           !:
        ! ?FILTER$ the contiguous letters to look for                   !:
        ! ?NOSORT if true forces the filtered list to original sequence !:
        ! ?DELAY  if true waits for the enter key before filtering      !:
        ! --------------------------------
12010   library env$("PD")&"Core\fnsnap\rtflib_dll.br": fnlistcopy
12020   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnmsexe$,fngethandle,fndelrow,fndelrow$,fnlistspec$,fnwaitwin,fnwaitbar
12030   dim headers$(1)*50,widths(1),forms$(1)*50,aa$(1)*50,ab$(1)*50,ba$(1)*50,bb$(1)*50,cc$(1)*3000,headers$*2000,cc$*20000,dorder(1),sorder(1),exclude(1)
12040   execute 'proc '&env$("PD")&'Core\fnsnap\tt'
12050   input #winno, fields spec$&",HEADERS,,nowait ": (mat headers$,mat widths,mat forms$) !:
        ! -------------------------------- !:
        ! ?Read the Header arrays                                       !:
        ! ?                                                             !:
        ! -------------------------------- !
12060   filterspec$=fnlistspec$(filterwin,srow:=10,scol:=10,frows:=10,fcols:=min(sum(widths),70),arows:=1,mat headers$,mat widths,mat forms$,"Filtered","",0,2) !:
        ! -------------------------------- !:
        ! ?Open the filtered array list and window                      !:
        ! ?                                                             !:
        ! -------------------------------- !
12070   input #winno, fields spec$&",rowsub,all,displayed_order,nowait": mat dorder !:
        ! -------------------------------- !:
        ! ?Get the order in which records are displayed                 !:
        ! ?                                                             !:
        ! -------------------------------- !
12080   x=udim(dorder) !:
        mat sorder(x) !:
        for a=1 to x : sorder(a)=a : next a !:
        ! -------------------------------- !:
        ! ?et SORDER to 1 through x for record read in same order as    !:
        ! ?riginal unsorted list                                        !:
        ! -------------------------------- !
12090   if nosort then mat dorder=sorder
12100 ! -------------------------------- !:
        ! ?etermine numeric v. string columns based on FORMS$ array     !:
        ! ?                                                             !:
        ! -------------------------------- !
12110   lcols=udim(mat headers$) !:
        mat cc$(lcols) !:
        mat col_format(lcols)=(0) !:
        mat col_total(lcols)=(0)
12120   gosub SET_FORMAT
12130 BUILD_FILTER: a$=kstat$(1) !:
        ! -------------------------------- !:
        ! ?Set a filter for processing incoming records                 !:
        ! ?                                                             !:
        ! -------------------------------- !
12140   if unhex$(a$)="08" then !:
          filter$=filter$(1:len(filter$)-1) !:
          pr #filterwin,fields "12,2,c 30": "Current filter "&filter$ !:
          goto BUILD_FILTER !:
        else !:
          if len(unhex$(a$))<4 and not unhex$(a$)="0D" then filter$=filter$&a$
12150   if len(unhex$(a$))>2 then mat cc$(0) : goto END_FILTER
12160   pr #filterwin,fields "12,2,c ": "Current filter "&filter$
12170   if delay and not unhex$(a$)="0D" then goto BUILD_FILTER
12180 START_FILTER: ! 
12190   aa=udim(dorder)+1 !:
        x=0
12200   waitwin=fnwaitwin("      Processing filter           ") ! 
12210 START_FILTER1: !:
        ! -------------------------------- !:
        ! ?ead the incoming array and process for filtered items        !:
        ! ?                                                             !:
        ! -------------------------------- !
12215   fnwaitbar(udim(dorder),udim(dorder)-aa)
12220   aa-=1
12230   if aa<=0 then !:
          mat cc$(0) : close #waitwin: : waitwin=0 : goto BUILD_FILTER
12240   mat start(1): mat end(1) : mat start=(dorder(aa)) : mat end=(dorder(aa)) !:
        input #winno, fields spec$&",row,range,nowait": mat start,mat end,mat cc$ !:
        mat dd$(udim(mat cc$))=cc$ !:
        ! -------------------------------- !:
        ! ?ead each row in the input array                              !:
        ! ?                                                             !:
        ! -------------------------------- !
12250   pr mat cc$
12260 ! -------------------------------- !:
        ! ?ormat cells for each column                                  !:
        ! ?                                                             !:
        ! -------------------------------- !
12270   for aaa=1 to udim(mat cc$)
12280     if not col_format(aaa) then !:
            col_total(aaa)+=val(srep$(cc$(aaa),"$","")) conv SET_NUM !:
            ! -------------------------------- !:
            ! ?heck numeric columns and add to column total if no conversion!:
            ! ?rror.  If a conversion error occurs mark the colun as string !:
            ! ?o that no total will appear and no further conversion testing!:
            ! ?ill be done                                                  !:
            ! -------------------------------- !
12290     if not col_format(aaa) and trim$(hforms$(aaa))>"" then !:
            cc$(aaa)=cnvrt$(hforms$(aaa),val(srep$(cc$(aaa),"$",""))) !:
            ! -------------------------------- !:
            ! ?ormat the numeric column if conversion works                 !:
            ! ?                                                             !:
            ! -------------------------------- !
12300     if col_format(aaa) and trim$(hforms$(aaa))(1:4)="DATE" then !:
            execute "LET CC$(AAA)=date$(val(cc$(aaa)),"&srep$(srep$(hforms$(aaa),'DATE(','"'),')','")') !:
            ! -------------------------------- !:
            ! ?ormat dates                                                  !:
            ! ?                                                             !:
            ! -------------------------------- !
12310     if pos(uprc$(cc$(aaa)),uprc$(filter$))>0 then !:
            x+=1 !:
            mat asel(x) !:
            asel(x)=start !:
            goto SHOW_FILTER
12320   next aaa
12330   goto END_FILTER
12340 ! PAUSE
12350 SHOW_FILTER: ! 
12360   if x=1 then !:
          pr #filterwin, fields filterspec$&",HEADERS,[H]": (mat headers$,mat widths,mat forms$) !:
          pr #filterwin, fields filterspec$&",RANGE": 1,udim(dorder),mat dd$ !:
          mat dd$=("") !:
          mat cc$=("") !:
          goto START_FILTER1 !:
          ! -------------------------------- !:
          ! ?dd the row to the top of the output array and process the    !:
          ! ?ext row                                                      !:
          ! -------------------------------- !
12370   if x>1 then pr #filterwin, fields filterspec$&",RANGE": 1,0,mat dd$ !:
          mat dd$=("") !:
          mat cc$=("") !:
          goto START_FILTER1 !:
          ! -------------------------------- !:
          ! ?dd the row to the top of the output array and process the    !:
          ! ?ext row                                                      !:
          ! -------------------------------- !
12380 END_FILTER: if filter$>"" and len(unhex$(a$))<4 then !:
          goto START_FILTER1
12390 ! -------------------------------- !:
        ! ?eset the CC$ array to the correct number of columns          !:
        ! ?                                                             !:
        ! -------------------------------- !
12400   mat cc$(lcols)
12405   input #filterwin,fields filterspec$&",rowsub,all,displayed_order,nowait": mat dorder
12410   if udim(dorder)>0 then let fnlistcopy(filterwin,filterspec$,winno,spec$)
12420   close #filterwin: 
12430 fnend 
12999 ! ================================================================
13000 def library fnlistcopy(readwin,readspec$,savewin,savespec$) !:
        ! -------------------------------- !:
        ! ?ransfers the contents of one listbox to another. Both lists  !:
        ! ?ust have the same header information in terms of the number  !:
        ! ?f columns                                                    !:
        ! ?EADWIN the window number where the source list is located    !:
        ! ?EADSPEC$ the specification of row,col LIST rows/cols of sourc!:
        ! ?AVEWIN the window number of the receiving list               !:
        ! ?AVESPEC$ the specification row,col LIST rows/cols of destina !:
        ! -------------------------------- !
13010   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fngethandle,fndelrow,fndelrow$,fnlistspec$
13020   dim sh$(1)*50,sw(1),sf$(1)*50,ss$(1)*100,ss$*20000,rdo(1),sdo(1),rso(1),sso(1)
13030   copy_count=0
13040   input #readwin, fields readspec$&",HEADERS,,nowait ": (mat sh$,mat sw,mat sf$)
13050   x=udim(mat sf$) !:
        for a=1 to x !:
          sf$(a)=trim$(sf$(a)) !:
        next a
13060   input #readwin, fields readspec$&",rowsub,all,displayed_order,nowait": mat rdo
13070   copy_count=udim(rdo)
13080   input #readwin, fields readspec$&",row,all,nowait": mat ss$
13090   pr #savewin, fields savespec$&",=R": mat ss$ !:
        ! -------------------------------- !:
        ! ?Erase the contents of the existing SAVE LIST and replace     !:
        ! ?with the new records                                         !:
        ! -------------------------------- !
13100   fnlistcopy=copy_count
13110 fnend 
13120 ! ===================================================================
14000 def library fnlistsave(savewin,savespec$,&sspec$;_swin,&_srow,&_scol) !:
        ! -------------------------------- !:
        ! ?ave a copy of a list in the lower left corner of window 0    !:
        ! ?s the default.  Optionally another window and location can   !:
        ! ?e specified.                                                 !:
        ! ?SWIN window to save LIST to default is 0                     !:
        ! ?SROW row number within window default is bottom row          !:
        ! ?SCOL column number of row to save LIST default is col 1      !:
        ! ?he saved coy of the list occupies a 1 x 1 character space in !:
        ! ?he designated window                                         !:
        ! ?NLISTSAVE = value of _SWIN                                   !:
        ! -------------------------------- !
14010   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnmsexe$,fngethandle,fndelrow,fndelrow$,fnlistspec$,fnwinrowcol
14020   dim sh$(1)*50,sw(1),sf$(1)*50,ss$(1)*20,ss$*20000,so(1)
14030   fnlistsave=0
14040   input #savewin, fields savespec$&",HEADERS,,nowait ": (mat sh$,mat sw,mat sf$)
14050   x=udim(mat sf$) !:
        for a=1 to x !:
          sf$(a)=trim$(sf$(a)) !:
        next a
14060   fnwinrowcol(_swin,lrow,lcol) !:
        pr lrow !:
        ! -------------------------------- !:
        ! ?et the screen size of the main window                        !:
        ! ?                                                             !:
        ! -------------------------------- !
14061   if not _srow then _srow=lrow+1 else _srow=min(_srow,lrow)
14062   if not _scol then _scol=1 else _scol=min(_scol,lcol)
14070   pr #_swin, fields (sspec$:=str$(_srow)&","&str$(_scol)&",LIST 1/1")&",HEADERS,[H]": (mat sh$,mat sw,mat sf$) !:
        ! -------------------------------- !:
        ! ?reate a list header in a single cell in the lower left       !:
        ! ?orner of the main screen                                     !:
        ! -------------------------------- !
14090 START_SAVE: ! 
14120   input #savewin, fields savespec$&",row,all,nowait": mat ss$
14130   pr #_swin,fields sspec$&",=R": mat ss$
14140 END_SAVE: ! 
14145   fnlistsave=_swin
14150 fnend 
15000 def library fnlistenter(winno,spec$*100,element) !:
        ! -------------------------------- !:
        ! ?Extract information from a listbox and pr the information !:
        ! ?in an RTF report using Word                                  !:
        ! ?WINNO the window number in which the listbox appears         !:
        ! ?SPEC$ the list secifier 1,1,LIST 12/15 for example           !:
        ! --------------------------------
15010 ! LIBRARY env$("PD")&"Core\fnsnap\rtflib_dll.br": FNLISTCOPY
15020   library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnmsexe$,fngethandle,fndelrow,fndelrow$,fnlistspec$,fnwaitwin,fnwaitbar
15030   dim headers$(1)*50,widths(1),forms$(1)*50,aa$(1)*50,ab$(1)*50,ba$(1)*50,bb$(1)*50,cc$(1)*3000,headers$*2000,cc$*20000,dorder(1),sorder(1),exclude(1)
15040   dim inwrk$(1)*60,indate$(1)*60
15050   execute 'proc '&env$("PD")&'Core\fnsnap\tt'
15060   input #winno, fields spec$&",HEADERS,,nowait ": (mat headers$,mat widths,mat forms$) !:
        ! -------------------------------- !:
        ! ?Read the Header arrays                                       !:
        ! ?                                                             !:
        ! -------------------------------- !
15070   input #winno, fields spec$&",rowsub,all,displayed_order,nowait": mat dorder !:
        ! -------------------------------- !:
        ! ?Get the order in which records are displayed                 !:
        ! ?                                                             !:
        ! -------------------------------- !
15075   input #winno, fields spec$&",colcnt,all,nowait": lcols !:
        ! -------------------------------- !:
        ! ?Get the number of columns in the control                     !:
        ! ?                                                             !:
        ! -------------------------------- !
15080   for a=1 to lcols : hlen=max(hlen,len(headers$(a))) : next a
15090   for a=1 to lcols : wlen=max(wlen,widths(a)) : next a
15097   sort_cnt=0
15100   x=udim(dorder)
15110 ! IF NOSORT THEN MAT DORDER=SORDER
15120 ! -------------------------------- !:
        ! ?etermine numeric v. string columns based on FORMS$ array     !:
        ! ?                                                             !:
        ! -------------------------------- !
15130   mat cc$(lcols) !:
        mat col_format(lcols)=(0) !:
        mat col_total(lcols)=(0)
15140   gosub SET_FORMAT
15150 GET_ROW: !:
        ! -------------------------------- !:
        ! ?Set a row of data to change or edit                          !:
        ! ?                                                             !:
        ! -------------------------------- !
15160   mat start(1): mat end(1) : mat start=(dorder(element)): mat end=start !:
        input #winno, fields spec$&",row,range,nowait": mat start,mat end,mat cc$ !:
        mat dd$(udim(mat cc$))=cc$ !:
        ! -------------------------------- !:
        ! ?ead the selected row from the list ELEMENT                   !:
        ! ?                                                             !:
        ! -------------------------------- !
15170   pr mat cc$
15180 ! -------------------------------- !:
        ! ?ormat cells for each column                                  !:
        ! ?                                                             !:
        ! -------------------------------- !
15190   for aaa=1 to udim(mat cc$)
15200 ! -------------------------------- !:
          ! ?heck numeric columns and add to column total if no conversion!:
          ! ?rror.  If a conversion error occurs mark the colun as string !:
          ! ?o that no total will appear and no further conversion testing!:
          ! ?ill be done                                                  !:
          ! -------------------------------- !
15210     if not col_format(aaa) then col_total(aaa)+=val(srep$(cc$(aaa),"$","")) conv SET_NUM !:
            ! -------------------------------- !:
            ! ?heck to see if conversion works for otherwise numeric column !:
            ! ?                                                             !:
            ! -------------------------------- !
15220     if not col_format(aaa) and trim$(hforms$(aaa))>"" then !:
            cc$(aaa)=cnvrt$(hforms$(aaa),val(srep$(cc$(aaa),"$",""))) !:
            ! -------------------------------- !:
            ! ?ormat the numeric column if conversion works                 !:
            ! ?                                                             !:
            ! -------------------------------- !
15230 ! 
15240   next aaa
15250 ! -------------------------------- !:
        ! ?pen a window and allow data entry                            !:
        ! ?                                                             !:
        ! -------------------------------- !
15260 ! OPEN #(ENTERWIN:=FNGETHANDLE): "srow=5,scol=5,rows="&STR$(UDIM(MAT CC$)+1)&",cols="&STR$(50)&",PARENT=NONE",DISPLAY,outIn
15261   open #(enterwin:=fngethandle): "srow=5,scol=5,rows="&str$(udim(mat cc$)+1)&",cols="&str$(hlen+wlen+10)&",PARENT=NONE",display,outIn 
15270   for a=1 to udim(mat headers$)
15280     pr #enterwin,fields str$(a)&",2,c "&str$(hlen)&",N/W:T": trim$(headers$(a))
15290     mat inwrk$(a): mat indate(a): mat indate$(a) !:
          if hforms$(a)>"" then !:
            inwrk$(a)=str$(a)&","&str$(hlen+3)&","&hforms$(a)&",[D]" else !:
            inwrk$(a)=str$(a)&","&str$(hlen+3)&","&forms$(a)&",[D]"
15292     _p=pos(forms$(a),")",-1) !:
          _c=pos(forms$(a),",",-1) !:
          if _c>_p and pos(uprc$(forms$(a)),"P",_c)>0 then inwrk$(a)=srep$(inwrk$(a),",[D]",",P[D]")
15293     if widths(a)=0 then inwrk$(a)=srep$(inwrk$(a),",[D]",",P[D]")
15294     if pos(uprc$(inwrk$(a)),"DATE(")>0 then !:
            inwrk$(a)(pos(inwrk$(a),"("):pos(inwrk$(a),")"))="(MMDDYY)" !:
            indate(a)=1 !:
            indate$(a)=inwrk$(a)(1:pos(inwrk$(a),","))&str$(hlen+2+10)&",C "&str$(50-(hlen+2+10))
15300   next a
15310 ENTER_DATA: ! 
15320   rinput #enterwin, fields mat inwrk$: mat cc$ !:
        af=curfld !:
        ak=fkey
15321   if indate(af) then cc=val(cc$(af)) conv 15320
15322   if indate(af) and not cc then !:
          curfld(af) !:
          fnwaitwin("Invalid date use MMDDYY","Date Error","OK") !:
          goto 15320
15323   ! IF INDATE(AF) THEN !:                                                                     pr #ENTERWIN, FIELDS INDATE$(AF): " "
15330   if curfld=udim(mat cc$) then goto 15400
15340   if not ak then let curfld(af+1,ak) else let curfld(af,ak)
15350   goto ENTER_DATA
15360 END_ENTER_DATA: ! 
15370   mat cc$(lcols)
15380 ! INPUT #FILTERWIN,FIELDS FILTERSPEC$&",rowsub,all,displayed_order,nowait": MAT DORDER
15390 ! 
15400   close #enterwin: 
15405 ! pr #WINNO,FIELDS SPEC$&",CELL_RANGE": (ELEMENT-1)*UDIM(CC$)+1,(ELEMENT)*(UDIM(CC$)),MAT CC$ !:
        pr #winno,fields spec$&",RANGE": element,element,mat cc$
15410 fnend 
50000 ! !:
      ! -------------------------------- !:
      ! ?                                                             !:
      ! ?                                                             !:
      ! ?                                                             !:
      ! ?                                                             !:
      ! ?                                                             !:
      ! ?                                                             !:
      ! ?                                                             !:
      ! ?                                                             !:
      ! ?                                                             !:
      ! ?                                                             !:
      ! ?                                                             !:
      ! ?                                                             !:
      ! ?                                                             !:
      ! ?                                                             !:
      ! ?                                                             !:
      ! -------------------------------- !
50001 def library fnrtf(txtfile,specfile$*100,rtffile;picpath$*100,subpath$*100)
50010 ! mat TYPES$ holds single letters used to designate type!:
        ! of line such as H=header F=Footer D=Detail data T=Total line
50020 ! mat STYLES$ holds the formatting specifications followed !:
        ! by the name of the style such as HEADER !:
        ! segments of the style are separated by a pipe | character !:
        ! elements of the style might be li0.5 for left indent 1/2 inch !:
        ! rt2 sets a tab at 2 inches !:
        ! dt2.5 sets a decimal tab at 2 1/2 inches
50030 ! mat DATA$ holds the body of the report !:
        ! the first character designates what style is to be applied to the !:
        ! line the remaining elements separated by pipes are the data to be !:
        ! printed using the style !:
        ! a long sentence will be wrapped by the RTF reader and so long text!:
        ! lines do not need to be broken
50040 ! RFTFILE is the file number already opened to hold the report !:
        ! this file should be a display file with an ".rtf" suffix
50050   library env$("PD")&"Core\fnsnap\RTFLIB_dll.br": fnamt$,fntext$,fntype
50051   orientation$="PORTRAIT" !:
        paper$="LETTER" !:
        lmargin=rmargin=tmargin=bmargin=0.5 !:
        nume=0
50052   mat styles$(1)=("") !:
        mat cells$(1)=("") !:
        mat types$(1)=("") !:
        lefttext$=""
50053   setspec=1 !:
        linput #txtfile: data$
50054   if pos(data$,"[SPEC(") then gosub SET_SPECFILE !:
          restore #txtfile: 
50056   execute "proc *"&specfile$
50057   setspec=0
50058   if uprc$(trim$(orientation$))="LANDSCAPE" then landscape=1 else landscape=0
50060   t=udim(mat types$) !:
        crlf$=chr$(13)&chr$(10)
50070   dim s$(1)*2000,se$(1)*1000,colors$(19),data$*32000,bold$(1),fs$*100,fc$*100,sa$*100
50071   dim types$(1)*2,styles$(1)*2000,cells$(1)*2000,lefttext$*100,papersize$*100
50072   dim boxmargins$*1000,oldmargins$*1000,boxmarginsn$*1000
50073   dim subx$(1)*500
50080   colors$(1)="[BLACK]" !:
        colors$(2)="[BLUE]" !:
        colors$(3)="[LTBLUE]" !:
        colors$(4)="[LTGREEN]" !:
        colors$(5)="[PINK]" !:
        colors$(6)="[RED]" !:
        colors$(7)="[YELLOW]" !:
        colors$(8)="[WHITE]" !:
        colors$(9)="[DKBLUE]"
50081   colors$(10)="[BLUEGREEN]" !:
        colors$(11)="[GREEN]" !:
        colors$(12)="[PURPLE]" !:
        colors$(13)="[BURGUNDY]" !:
        colors$(14)="[LTOLIVE]" !:
        colors$(15)="[GRAY]" !:
        colors$(16)="[LTGRAY]" !:
        colors$(17)="[DKGREEN]" !:
        colors$(18)="[OLIVE]"
50082   colors$(19)="[SEAGREEN]"
50090   mat s$(t) !:
        mat se$(t) !:
        mat lin$(t) !:
        mat bold$(t)
50100   mat lin$=("\lin0") !:
        mat bold$=("") !:
        qa$="\ql "
50110   gosub SET_STYLES
50112   if exists(subpath$&"subtext.txt")=2 then execute "proc *"&subpath$&"subtext.txt"
50120 BUILD_RTF: linput #txtfile: data$ eof ZBUILD_RTF
50121   if pos(data$,"[SPEC(")>0 then gosub SET_SPECFILE ! Change pr specifications
50122   if pos(data$,"[NEWCELL(")>0 then gosub SET_NEWCELL ! Change cell specifications
50123   if pos(data$,"[FONT(")>0 then gosub SET_FONT ! Change font
50124   if pos(data$,"[SUB(")>0 then gosub SET_SUB
50126   if pos(data$,"[RTFCOL(") then xs=pos(data$,"[RTFCOL(")+8 !:
          xe=pos(data$,")",xs)-1 !:
          x=val(data$(xs:xe)) !:
          pr #rtffile: "\sect \sectd \sbknone\linex0\cols"&str$(x)&"\sectdefaultcl " !:
          data$=srep$(data$,"[RTFCOL("&str$(x)&")]","") !:
          ! Start printing in columns with no separator
50127   if pos(data$,"[RTFCOLL(") then xs=pos(data$,"[RTFCOLL(")+9 !:
          xe=pos(data$,")",xs)-1 !:
          x=val(data$(xs:xe)) !:
          pr #rtffile: "\sect \sectd \sbknone\linex0\cols"&str$(x)&"\linebetcol\sectdefaultcl " !:
          data$=srep$(data$,"[RTFCOLL("&str$(x)&")]","") !:
          ! Start printing in columns with a line separating the columns
50128   if pos(data$,"[WD(") then xs=pos(data$,"[WD(")+4 !:
          xe=pos(data$,")]",xs)-1 !:
          x=val(data$(xs:xe)) !:
          data$=srep$(data$,"[WD("&str$(x)&")]",'{{\field{\*\fldinst SYMBOL '&str$(x)&' \\f "Wingdings" \\s 17}{\fldrslt\f14}}}') !:
          ! Insert WingDing symbol
50130   if trim$(data$)<="" then pr #rtffile: "\par " !:
          goto 50120 !:
          ! Insert a blank line
50135   data$=srep$(data$,chr$(9),"|")
50140   if data$(1:1)="N" and pos(data$,"|")=3 then !:
          newdata=1 !:
          data$=data$(2:len(data$)) !:
        else !:
          newdata=0
50142   if laststyle=1 and pos("0123456789",data$(1:1)) then pr #prntfil: "\pard \ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0"&crlf$ !:
          ! Start printing cells
50150   data$=fnrtfcolor$(data$)
50160   if data$(1:2)="D|" then 
50170     x=len(data$)
50180     if x<30 and data$(x:x)=":" then !:
            data$=data$(1:2)&"{\b "&data$(3:len(data$))&"}"
50190   end if 
50200 ! -------------------------------- !:
        ! ?Format Detail Step Section Code E                            !:
        ! ?                                                             !:
        ! -------------------------------- !
50210   if uprc$(data$(1:1))="E" then 
50212     if checklist>0 then 
50213       linecnt+=1 !:
            if pos(data$(1:7),".")>1 or pos(data$(1:7),")")>1 then !:
              numplc=pos(data$," ") else !:
              numplc=0
50214       if nume then !:
              data$=data$(1:2)&"\tab "&str$(linecnt)&".)"&"\tab "&fntext$(data$(3:len(data$)),50)&"\tab "&lefttext$&"[RTFLINE]" !:
            else if numplc>0 then data$=data$(1:numplc-1)&"\tab    "&fntext$(ltrm$(data$(numplc:len(data$))),50)&"\tab "&lefttext$&"[RTFLINE]" !:
            else data$=fntext$(ltrm$(data$),50)&"\tab "&lefttext$&"[RTFLINE]"
50219   else 
50220     x=0 !:
          x=val(data$(3:7)) conv 50240
50230     if x>0 then !:
            data$=data$(1:2)&"{\par\b "&data$(3:6)&"}"&data$(7:len(data$))
50240     if data$(3:len(data$))="Cause" then !:
            data$(3:len(data$))="  "&data$(3:len(data$)) else !:
            data$=srep$(data$,"Cause ","  Cause [RTFLINE]")
50250     if data$(3:len(data$))="Remedy" then !:
            data$(3:len(data$))="  "&data$(3:len(data$)) else !:
            data$=srep$(data$,"Remedy ","  Remedy [RTFLINE]")
50300   end if 
50310 else linecnt=0 ! END IF
50319 ! -------------------------------- !:
      ! ?Create special processing for PROGRAM G lines                !:
      ! ?                                                             !:
      ! -------------------------------- !
50320 if uprc$(data$(1:1))="G" then 
50330   data$=srep$(data$,"!:","!:[RTFLINE]")
50340 end if 
50349 ! -------------------------------- !:
      ! ?Perform bracket replacement parameters                       !:
      ! ?                                                             !:
      ! -------------------------------- !
50350 data$=srep$(data$,"[RTFPAGE]","\page ")
50360 data$=srep$(data$,"[RTFLINE]","\line ")
50370 data$=srep$(data$,"[RTFDATE]",date$("Month DD, CCYY"))
50380 ! 
50390 ! -------------------------------- !:
      ! ?Draw box around text following this point                    !:
      ! ?                                                             !:
      ! -------------------------------- !
50400 if pos(data$,"[RTFBOX]")>0 then !:
        rtfbox=1 !:
        data$=srep$(data$,"[RTFBOX]","") !:
        d$=""
50410 ! -------------------------------- !:
      ! ?End the box started by prior line                            !:
      ! ?                                                             !:
      ! -------------------------------- !
50420 if pos(data$,"[\RTFBOX]")>0 then !:
        rtfbox=0 !:
        data$=srep$(data$,"[\RTFBOX]","") !:
        d$=""
50430 ! -------------------------------- !:
      ! ?Create a line above text                                     !:
      ! ?                                                             !:
      ! -------------------------------- !
50440 if pos(data$,"[TOPLINE]")>0 then !:
        data$=srep$(data$,"[TOPLINE]","") !:
        topline=1 !:
        d$=""
50450 if pos(data$,"[\TOPLINE]")>0 then !:
        data$=srep$(data$,"[\TOPLINE]","") !:
        topline=0 !:
        d$=""
50460 ! -------------------------------- !:
      ! ?Create a line below text                                     !:
      ! ?                                                             !:
      ! -------------------------------- !
50470 if pos(data$,"[BOTLINE]")>0 then !:
        data$=srep$(data$,"[BOTLINE]","") !:
        botline=1 !:
        d$=""
50480 if pos(data$,"[\BOTLINE]")>0 then !:
        data$=srep$(data$,"[\BOTLINE]","") !:
        botline=0 !:
        d$=""
50490 ! -------------------------------- !:
      ! ?Create a line in the middle of a box. Verticle lines are     !:
      ! ?created by using Bar Tabs in a style line                    !:
      ! -------------------------------- !
50500 if pos(data$,"[MIDLINE]")>0 then !:
        midline=1 !:
        data$=srep$(data$,"[MIDLINE]","") !:
        d$=""
50510 if pos(data$,"[\MIDLINE]")>0 then !:
        midline=0 !:
        data$=srep$(data$,"[\MIDLINE]","") : d$=""
50520 ! -------------------------------- !:
      ! ? Create a header to appear at the top of every page          !:
      ! ?                                                             !:
      ! -------------------------------- !
50530 if uprc$(data$(1:1))="H" then 
50540   gosub HEADER
50550   goto 50640
50560 end if 
50570 ! -------------------------------- !:
      ! ?Create a footer to appear at the bottom of every page        !:
      ! ?                                                             !:
      ! -------------------------------- !
50580 if uprc$(data$(1:1))="F" then 
50590   gosub FOOTER
50600   goto 50640
50610 end if 
50620 if not uprc$(data$(1:1))=d$ then gosub NEW_STYLE
50630 gosub PICTURE ! pr #RTFFILE: "{\par "&SREP$(DATA$(3:LEN(DATA$)),"|","}{\tab}{ ")&"}"
50640 d$=uprc$(data$(1:1))
50650 goto BUILD_RTF
50660 ZBUILD_RTF: pr #rtffile: "}"
50665 header=0
50670 fnend 
50680 ! ==========================================================
50690 HEADER: ! 
50700 if header then 
50701   pr #rtffile: "\par\sect \sectd \linex0\endnhere\sectlinegrid360\sectdefaultcl"&crlf$
50702   if lmargin>0 then pr #rtffile: "\margl"&fntwips$(lmargin)&"\marglsxn"&fntwips$(lmargin)&crlf$
50703   if rmargin>0 then pr #rtffile: "\margr"&fntwips$(rmargin)&"\margrsxn"&fntwips$(rmargin)&crlf$
50704   if tmargin>0 then pr #rtffile: "\margt"&fntwips$(tmargin)&"\margtsxn"&fntwips$(tmargin)&crlf$
50705   if bmargin>0 then pr #rtffile: "\margb"&fntwips$(bmargin)&"\margbsxn"&fntwips$(bmargin)&crlf$
50706 ! \marglsxn1800\margrsxn1440\margtsxn720\margbsxn720
50707 !  IF THEADER >0 THEN pr #RTFFILE: "\linex0\headery"&FNTWIPS$(THEADER)&"\footery"&FNTWIPS$(TFOOTER)&"\endnhere\sectlinegrid"&FNTWIPS$(THEADER)&"\sectdefaultcl"&CRLF$
50708 ! 
50709 end if 
50710 if botline then 
50720 ! 
50730   pr #rtffile: "{\header \pard \plain "&srep$(s$(srch(mat types$,"H")),"\widctlpar","\widctlpar\brdrb\brdrs\brdrw10\brsp20 ")&"{"&srep$(srep$(data$(3:len(data$)),"|","}{\tab}{ "),"[RTFBAR]","|")&"\par }}"&crlf$ ! " {"&DATA$(3:LEN(DATA$))&" \par }}"&CRLF$
50740   botline=0
50750 else 
50760   pr #rtffile: "{\header \pard \plain "&s$(srch(mat types$,"H"))&" {"&srep$(srep$(data$(3:len(data$)),"|","}{\tab}{"),"[RTFBAR]","|")&" \par }}"&crlf$
50770 end if 
50780 header+=1
50790 return 
50800 FOOTER: ! 
50810 if topline then 
50820   pr #rtffile: "{\footer \pard \plain "&srep$(srep$(srep$(s$(srch(mat types$,"F")),"widctlpar","widctlpar\brdrt\brdrs\brdrw10\brsp20 ")&" {"&srep$(data$(3:len(data$)),"[PAGE]","  {{\field{\*\fldinst{\f4  PAGE }}{\fldrslt{\f4 2}}}}"),"|","}{\tab}{ "),"[RTFBAR]","|")&" \par }}"&crlf$
50830   topline=0
50840 else 
50850   pr #rtffile: "{\footer \pard \plain "&s$(srch(mat types$,"F"))&" {"&srep$(srep$(srep$(data$(3:len(data$)),"[PAGE]","  {{\field{\*\fldinst{\f4  PAGE }}{\fldrslt{\f4 2}}}}"),"|","}{\tab}{"),"[RTFBAR]","|")&" \par }}"&crlf$
50860 end if 
50870 footer+=1
50880 return 
50890 NEW_STYLE: ! 
50900 ! IF SRCH(MAT TYPES$,UPRC$(DATA$(1:1)))>0 THEN pr #RTFFILE: "\pard \plain "&S$(SRCH(MAT TYPES$,UPRC$(DATA$(1:1))))
50910 if rtfbox then 
50911   xlis=pos(s$(srch(mat types$,uprc$(data$(1:1)))),"\li") !:
        xlie=pos(s$(srch(mat types$,uprc$(data$(1:1)))),"\",xlis+1)-1
50912   boxmargins$="\li"&str$(lmargin*1440)&"\ri"&str$((lmargin+rmargin)*2*1440)
50913   oldmargins$=s$(srch(mat types$,uprc$(data$(1:1))))(xlis:xlie)
50914   boxmarginsn$="\aspalpha\aspnum\faauto\adjustright"&srep$(srep$(boxmargins$,"\ri","\rin"),"\li","lin")&"\itap0"
50920   if midline then 
50930     if srch(mat types$,uprc$(data$(1:1)))>0 then pr #rtffile: "\pard \plain "&srep$(srep$(s$(srch(mat types$,uprc$(data$(1:1)))),"widctlpar","wictlpar\brdrt\brdrs\brdrw10\brsp20 \brdrl\brdrs\brdrw10\brsp80 \brdrb\brdrs\brdrw10\brsp20 \brdrr\brdrs\brdrw10\brsp80 \brdrbtw\brdrs\brdrw10\brsp20 "),oldmargins$,boxmargins$)&crlf$&boxmarginsn$&crlf$
50940   else 
50950     if srch(mat types$,uprc$(data$(1:1)))>0 then pr #rtffile: "\pard \plain "&srep$(srep$(s$(srch(mat types$,uprc$(data$(1:1)))),"widctlpar","wictlpar\brdrt\brdrs\brdrw10\brsp20 \brdrl\brdrs\brdrw10\brsp80 \brdrb\brdrs\brdrw10\brsp20 \brdrr\brdrs\brdrw10\brsp80 "),oldmargins$,boxmargins$)&crlf$&boxmarginsn$&crlf$
50960   end if 
50970 else 
50980   if midline then 
50990     if srch(mat types$,uprc$(data$(1:1)))>0 then pr #rtffile: "\pard \plain "&s$(srch(mat types$,uprc$(data$(1:1))))&crlf$
51000   else 
51010     if srch(mat types$,uprc$(data$(1:1)))>0 then pr #rtffile: "\pard \plain "&s$(srch(mat types$,uprc$(data$(1:1))))&crlf$
51020   end if 
51030 end if 
51040 d$=uprc$(data$(1:1))
51050 return 
51060 SET_STYLES: ! 
51070 perin=1440
51080 for a=1 to t
51090   text$=trim$(text$)
51100   ap=0
51110 ! s$(A)="\s"&STR$(A+10)&"\q1 "
51120   s$(a)="\s"&str$(a+10)&"\ql "
51130 ! ----------------------------------
51140 SET_STYLES_1: ap+=1
51150   tx=pos(styles$(a),"|")
51160   if tx<0 then tx=len(styles$(a))+1
51170   ap+=1
51180 ! -------------------------------- !:
        ! ?First line indent                                            !:
        ! ?                                                             !:
        ! -------------------------------- !
51190   if uprc$(styles$(a)(1:2))="FI" then 
51200     s$(a)=s$(a)&"\fi"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
51210     goto ZSET_STYLES_1
51220   end if 
51230   ap+=1
51240 ! -------------------------------- !:
        ! ?Left indent                                                  !:
        ! ?                                                             !:
        ! -------------------------------- !
51250   if uprc$(styles$(a)(1:2))="LI" then 
51260     s$(a)=s$(a)&"\li"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
51270     lin$(a)="\lin"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
51280     goto ZSET_STYLES_1
51290   end if 
51300 ! -------------------------------- !:
        ! ?Right indent                                                 !:
        ! ?                                                             !:
        ! -------------------------------- !
51310   if uprc$(styles$(a)(1:2))="RI" then 
51320     s$(a)=s$(a)&"\ri"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
51330     goto ZSET_STYLES_1
51340   end if 
51350   if ap=1 then s$(a)=s$(a)&"\li0\ri0\widctlpar" !:
        else if pos(s$(a),"widctlpar")<1 then s$(a)=s$(a)&"\widctlpar "
51360 ! -------------------------------- !:
      ! ?Tab left align                                               !:
      ! ?                                                             !:
      ! -------------------------------- !
51370 if uprc$(styles$(a)(1:2))="TL" then 
51380   s$(a)=s$(a)&"\tx"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
51390   goto ZSET_STYLES_1
51400 end if 
51410 ! -------------------------------- !:
      ! ?Tab center align                                             !:
      ! ?                                                             !:
      ! -------------------------------- !
51420 if uprc$(styles$(a)(1:2))="TC" then 
51430   s$(a)=s$(a)&"\tqc\tx"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
51440   goto ZSET_STYLES_1
51450 end if 
51460 ! -------------------------------- !:
      ! ?Tab right align                                              !:
      ! ?                                                             !:
      ! -------------------------------- !
51470 if uprc$(styles$(a)(1:2))="TR" then 
51480   s$(a)=s$(a)&"\tqr\tx"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
51490   goto ZSET_STYLES_1
51500 end if 
51510 ! -------------------------------- !:
      ! ?Tab decimal point align                                      !:
      ! ?                                                             !:
      ! -------------------------------- !
51520 if uprc$(styles$(a)(1:2))="TD" then 
51530   s$(a)=s$(a)&"\tqdec\tx"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
51540   goto ZSET_STYLES_1
51550 end if 
51560 ! -------------------------------- !:
      ! ?Tab vertical Bar align                                       !:
      ! ?                                                             !:
      ! -------------------------------- !
51570 if uprc$(styles$(a)(1:2))="TB" then 
51580   s$(a)=s$(a)&"\tb"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
51590   goto ZSET_STYLES_1
51600 end if 
51601 ! -------------------------------- !:
      ! ?Font if other than Times new roman (Panto....)               !:
      ! ?Only Courier and Arial are currently configured              !:
      ! -------------------------------- !
51604 if uprc$(styles$(a)(1:9))="FPALATINO" then ff$="\f29" !:
        goto ZSET_STYLES_1
51605 if uprc$(styles$(a)(1:6))="FFRITZ" then ff$="\f28" !:
        goto ZSET_STYLES_1
51606 if uprc$(styles$(a)(1:6))="FTIMES" then ff$="\f0" !:
        goto ZSET_STYLES_1
51607 if uprc$(styles$(a)(1:6))="FARIAL" then ff$="\f1" !:
        goto ZSET_STYLES_1
51608 if uprc$(styles$(a)(1:8))="FCOURIER" then ff$="\f2" !:
        goto ZSET_STYLES_1
51610 ! -------------------------------- !:
      ! ?Font size                                                    !:
      ! ?                                                             !:
      ! -------------------------------- !
51620 if uprc$(styles$(a)(1:2))="FS" then 
51630   fs$=str$(round(val(styles$(a)(3:tx-1))*2,0))
51640   goto ZSET_STYLES_1
51650 end if 
51660 ! -------------------------------- !:
      ! ?Font color                                                   !:
      ! ?                                                             !:
      ! -------------------------------- !
51670 if uprc$(styles$(a)(1:2))="CF" then 
51680 ! INPUT FIELDS "23,64,c 1": PAUSE$
51690   fc$="\cf"&str$(max(0,srch(mat colors$,uprc$(styles$(a)(3:tx-1)))))&" "
51700   goto ZSET_STYLES_1
51710 end if 
51720 ! -------------------------------- !:
      ! ?Font BOLD                                                    !:
      ! ?                                                             !:
      ! -------------------------------- !
51730 if uprc$(styles$(a)(1:1))="B" then 
51740   bold$(a)="\b"
51750   goto ZSET_STYLES_1
51760 end if 
51770 ! -------------------------------- !:
      ! ?Space After Paragraph                                        !:
      ! ?                                                             !:
      ! -------------------------------- !
51780 if uprc$(styles$(a)(1:2))="SA" then 
51790   sa$="\sa"&cnvrt$("pic(####)",val(style$(a)(3:tx-1))*320)&" "
51800   goto ZSET_STYLES_1
51810 end if 
51820 ! -------------------------------- !:
      ! ?Paragraph Alignment                                          !:
      ! ?                                                             !:
      ! -------------------------------- !
51830 if uprc$(styles$(a)(1:1))="Q" and pos("LRCJ",uprc$(styles$(a)(2:2)))>0 then 
51840   qa$="\q"&lwrc$(styles$(a)(2:2))&" "
51850   s$(a)=srep$(s$(a),"\ql ",qa$)
51860   goto ZSET_STYLES_1
51870 end if 
51880 ZSET_STYLES_1: styles$(a)=styles$(a)(tx+1:len(styles$(a)))
51890 if len(styles$(a))>1 and pos(styles$(a),"|")>0 then goto SET_STYLES_1
51900 s$(a)=s$(a)&"\aspalpha\aspnum\faauto\adjustright\rin0"&lin$(a)&"\itap0 "&bold$(a)&ff$&"\fs"&fs$&fc$&sa$&"\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 " !:
      se$(a)="\sbasedon"&str$((a-1)+10)&" \snext"&str$(a+10)
51910 fc$=fs$=sa$=ff$=""
51920 next a
51930 pr #rtffile: "{\rtf1\ansi\ansicpg1252\uc1 \deff0\deflang1033\deflangfe1033"&crlf$
51940 pr #rtffile: "{\fonttbl"&crlf$
51945 pr #rtffile: "{\f0\froman\fcharset0\fprq2{\*\panose 02020603050405020304}Times New Roman;}"&crlf$
51950 pr #rtffile: "{\f1\fswiss\fcharset0\fprq2{\*\panose 020b0604020202020204}Arial;}"&crlf$
51955 pr #rtffile: "{\f2\fmodern\fcharset0\fprq1{\*\panose 02070309020205020404}Courier New;}"&crlf$
51956 pr #rtffile: "{\f14\fnil\fcharset2\fprq2{\*\panose 05000000000000000000}Wingdings;}"&crlf$
51957 pr #rtffile: "{\f28\froman\fcharset0\fprq2{\*\panose 02020500000000000000}Fritz-Quad;}"&crlf$
51958 pr #rtffile: "{\f29\froman\fcharset0\fprq2{\*\panose 02040502050505030304}Palatino Linotype;}"&crlf$
51960 pr #rtffile: "{\f42\fbidi \fmodern\fcharset0\fprq2{\*\panose 050b0009000000000000}OCR-A;}"&crlf$
51962 pr #rtffile: "{\f69\fmodern\fcharset0\fprq1{\*\panose 020b0609040504020204}Lucida Console;}"&crlf$
51970 pr #rtffile: "}"&crlf$
51980 pr #rtffile: "{\colortbl;\red0\green0\blue0;\red0\green0\blue255;\red0\green255\blue255;\red0\green255\blue0;\red255\green0\blue255;\red255\green0\blue0;\red255\green255\blue0;\red255\green255\blue255;"&crlf$
51990 pr #rtffile: "\red0\green0\blue128;\red0\green128\blue128;\red0\green128\blue0;\red128\green0\blue128;\red128\green0\blue0;\red128\green128\blue0;\red128\green128\blue128;\red192\green192\blue192;\red255\green255\blue255;\red217\green217\blue217;}"&crlf$
52000 pr #rtffile: "{\stylesheet"&crlf$
52010 pr #rtffile: "{\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs20\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 \snext0 Normal;}"&crlf$
52020 pr #rtffile: "{\*\cs10 \additive Default Paragraph Font;}"&crlf$
52030 for a=1 to t
52040   pr #rtffile: "{"&s$(a)&se$(a)&" "&styles$(a)&";}"&crlf$
52050 next a
52060 pr #rtffile: "}"&crlf$
52070 pr #rtffile: "{\info"&crlf$
52080 pr #rtffile: "{\title This is the first line}"&crlf$
52090 pr #rtffile: "{\title This is the first line}"&crlf$
52100 pr #rtffile: "{\operator George L. Tisdale}"&crlf$
52110 pr #rtffile: "{\creatim\yr"&date$("ccyy")&"\mo"&date$("mm")&"\dy"&date$("dd")&"\hr"&time$(1:2)&"\min"&time$(4:5)&"}"&crlf$
52120 pr #rtffile: "{\revtim\yr"&date$("ccyy")&"\mo"&date$("mm")&"\dy"&date$("dd")&"\hr"&time$(1:2)&"\min"&time$(4:5)&"}"&crlf$
52130 pr #rtffile: "{\version2}"&crlf$
52140 pr #rtffile: "{\edmins0}"&crlf$
52150 pr #rtffile: "{\nofpages1}"&crlf$
52160 pr #rtffile: "{\nofwords0}"&crlf$
52170 pr #rtffile: "{\nofchars0}"&crlf$
52180 pr #rtffile: "{\*\company Advanced Informonics Corporation}"&crlf$
52190 pr #rtffile: "{\nofcharsws0}"&crlf$
52200 pr #rtffile: "{\vern8247}"&crlf$
52210 if lmargin>0 then pr #rtffile: "\margl"&str$(ip(lmargin*1440))&crlf$
52220 if rmargin>0 then pr #rtffile: "\margr"&str$(ip(rmargin*1440))&crlf$
52222 if tmargin>0 then pr #rtffile: "\margt"&str$(ip(tmargin*1440))&crlf$
52224 if bmargin>0 then pr #rtffile: "\margb"&str$(ip(bmargin*1440))&crlf$
52230 pr #rtffile: "}"&crlf$ !:
      papersize$="\paperw12240\paperh15840"
52231 if uprc$(trim$(paper$))="LETTER" or paper$<="" then 
52232   if uprc$(trim$(orientation$))="LANDSCAPE" then !:
          papersize$="\paperw15840\paperh12240" else !:
          papersize$="\paperw12240\paperh15840"
52233 end if 
52234 ! 
52235 if uprc$(trim$(paper$))="LEGAL" then 
52236   if uprc$(trim$(orientation$))="LANDSCAPE" then !:
          papersize$="\paperw20160\paperh12240" else !:
          papersize$="\paperw12240\paperh20160"
52237 end if 
52238 pr #rtffile: papersize$&crlf$
52239 ! IF UPRC$(TRIM$(ORIENTATION$))="LANDSCAPE" THEN pr #RTFFILE: PAPERSIZE$&CRLF$
52240 pr #rtffile: "\widowctrl\ftnbj\aenddoc\noxlattoyen\expshrtn\noultrlspc\dntblnsbdb\nospaceforul\hyphcaps0\formshade\horzdoc\dgmargin\dghspace180\dgvspace180\dghorigin720\dgvorigin720\dghshow1\dgvshow1"&crlf$
52245 dim theader$*100
52246 if theader>0 and tfooter>0 then !:
        theader$="\headery"&fntwips$(theader)&"\footery"&fntwips$(tfooter) else if theader>0 then !:
        theader$="\headery"&fntwips$(theader) else if tfooter>0 then theader$="\footery"&fntwips$(tfooter) else !:
        theader$=""
52250 pr #rtffile: "\jexpand\viewkind4\viewscale100\pgbrdrhead\pgbrdrfoot\splytwnine\ftnlytwnine\htmautsp\nolnhtadjtbl\useltbaln\alntblind\lytcalctblwd\lyttblrtgr\lnbrkrule \fet0\sectd \linex0"&theader$&"\endnhere\sectlinegrid360\sectdefaultcl "&crlf$
52255 ! IF LANDSCAPE=1 THEN pr #RTFFILE: "\lndscpsxn\psz1"
52256 if landscape=1 then pr #rtffile: "\landscape"
52260 return 
52270 def library fnamt$*50(value,mask$*20;sign$,underline$,reverse)
52280   dim av$*30
52290   if reverse>0 then reverse=-1 else reverse=1
52300   underline$=uprc$(underline$)
52310   on pos("SD",underline$) goto SINGLE,DOUBLE none NO_UNDERLINE
52320 NO_UNDERLINE: ! 
52330   if value*reverse<0 then av$=sign$&"(\tab "&trim$(cnvrt$(mask$,abs(value)))&"}{)" else av$=sign$&" \tab "&trim$(cnvrt$(mask$,abs(value)))&"}{ "
52340   goto ZAMT
52350 SINGLE: ! 
52360   if value*reverse<0 then av$=sign$&"(\ul\tab "&trim$(cnvrt$(mask$,abs(value)))&"}{)" else av$=sign$&" \ul\tab "&trim$(cnvrt$(mask$,abs(value)))&"}{ "
52370   goto ZAMT
52380 DOUBLE: ! 
52390   if value*reverse<0 then av$=sign$&"(\uldb\tab "&trim$(cnvrt$(mask$,abs(value)))&"}{)" else av$=sign$&"\uldb\tab "&trim$(cnvrt$(mask$,abs(value)))&"}{ "
52400   goto ZAMT
52410 ZAMT: fnamt$=av$
52420 fnend 
52430 def library fntext$*4000(text$*4000,alen)
52440   dim t$*4000
52450   t$=""
52460   text$=trim$(text$)
52461   pr text$
52462   if text$(1:9)="[RTFLINE]" then text$(1:9)=""
52463   xl=min(pos(text$,"[RTFLINE]",9)-1,alen) !:
        if xl<1 then xl=alen else !:
          if xl>alen then xl=alen
52464   pr "XL="&str$(xl),"ALEN="&str$(alen)
52465 ! PAUSE
52470   x=pos(text$&" "," ",xl)
52480   if x<2 then goto 52520
52490   if len(t$) then t$=t$&"\line "&text$(1:x) else t$=text$(1:x)
52500   text$=trim$(text$(x:len(text$)))
52510   if len(trim$(text$))>xl then goto 52460
52520 ! IF LEN(TEXT$) THEN t$=T$&"\line "&TEXT$
52530   if len(t$)>0 and len(text$) then t$=t$&"\line "&text$ else if len(text$)>0 then t$=t$&text$
52540 fntext$=t$
52545 pr t$
52546 pr "*********************************************"
52547 ! PAUSE
52550 fnend 
52560 def fnrtfcolor$*6000(cd$*6000)
52561   acolor=0
52562   do while acolor<19
52563     acolor+=1
52564     if pos(cd$,colors$(acolor))>0 then cd$=srep$(cd$,colors$(acolor),"\cf"&str$(acolor)&" ")
52565   loop 
52568   fnrtfcolor$=cd$
52640 fnend 
52700 ! --------------------------------
52710 SET_NEWCELL: ! 
52720 ncs=pos(data$,"[NEWCELL(")+9
52730 nce=pos(data$,")]",ncs)-1
52735 ! PAUSE
52740 if exists(picpath$&trim$(data$(ncs:nce))) then execute "proc *"&picpath$&trim$(data$(ncs:nce)) else input fields "10,10,c 10": pause$
52750 data$=srep$(data$,"[NEWCELL("&data$(ncs:nce)&")]","")
52755 ! PAUSE
52760 return 
52770 ! ---------------------------------
52780 SET_FONT: ! 
52782 nfs=pos(data$,"[FONT(")+6 !:
      nfe=pos(data$,")]",nfs)-1
52784 if nfs>0 then !:
        data$=srep$(data$,"[FONT(ARIAL)]","\f1 ") !:
        data$=srep$(data$,"[FONT(COURIER)]","\f2 ") !:
        data$=srep$(data$,"[FONT(FRITZ)]","\f28 ") !:
        data$=srep$(data$,"[FONT(PALATINO)]","\f29 ") !:
        data$=srep$(data$,"[FONT(OCR-A)]","\f42 ") !:
        data$=srep$(data$,"[FONT(LUCIDA)]","\f69 ")
52786 return 
52800 SET_SUB: ! substitute values
52802 nss=pos(data$,"[SUB(")+5 !:
      nse=pos(data$,")]",nss)-1
52804 if nse>0 then 
52806   execute "let subx="&data$(nss:nse)
52808   if subx<1 then !:
          msgbox("The replaceable parameter "&data$(nss-1:nse+1)&" was not found in the data-set. The parameter will be ommitted") !:
          data$(nss-5:nse+2)="" !:
        else subx$=data$(nss-5:nse+2)
52810   if subx>0 then data$=srep$(data$,subx$,sub$(subx))
52812   goto SET_SUB
52814 end if 
52816 return 
53000 PICTURE: !:
      ! -------------------------------- !:
      ! ?Merge in RTF picture file and pr final DATA line          !:
      ! ?                                                             !:
      ! -------------------------------- !
53005 dim pict_name$*100
53007 if newdata then pr #rtffile: "{\cf9 " !:
        ! -------------------------------- !:
        ! ?Sets text of newlines to BLUE                                !:
        ! ?                                                             !:
        ! -------------------------------- !
53010 if pos(data$,"[PICT(") >0 then 
53011 ! -------------------------------- !:
        ! ?PICT indicares a figure to insert centered on a page with a  !:
        ! ?cation underneath specifying the figure number               !:
        ! -------------------------------- !
53012   pict_start=pos(data$,"[PICT(")
53014   pict_end=pos(data$,")]",pict_start)
53016   pr #rtffile: "{\par \qc "&srep$(data$(3:pict_start-1),"|","}{\tab}{")
53018   if picpath$>"" then !:
          pict_name$=picpath$&"\"&trim$(data$(pict_start+6:pict_end-1)) else !:
          pict_name$=trim$(data$(pict_start+6:pict_end-1))
53020   fntype(pict_name$,rtffile)
53022   x=pos(pict_name$,"\",-1)+1 !:
        pr #rtffile: "{\par \fs16 Figure: "&pict_name$(x:len(pict_name$))&" \par }"
53024   pr #rtffile: srep$(data$(pict_end+2:len(data$)),"|","}{\tab}{")&"}"&crlf$
53040 else if pos(data$,"[SPICT(") >0 then 
53041 ! -------------------------------- !:
        ! ?SPICT represents a signiture or similar RTF graphic to be    !:
        ! ?positioned to the left side of the page                      !:
        ! -------------------------------- !
53042   pict_start=pos(data$,"[SPICT(")
53044   pict_end=pos(data$,")]",pict_start)
53046   pr #rtffile: "{\par \ql "&srep$(data$(3:pict_start-1),"|","}{\tab}{")
53048   if picpath$>"" then !:
          pict_name$=srep$(picpath$&"\"&trim$(data$(pict_start+7:pict_end-1)),"\\","\") else !:
          pict_name$=trim$(data$(pict_start+7:pict_end-1))
53050   fntype(pict_name$,rtffile)
53052   ! x=POS(PICT_NAME$,"\",-1)+1 !:                                                         pr #RTFFILE: "{\par \fs16 Figure: "&PICT_NAME$(X:LEN(PICT_NAME$))&" \par }"
53054   pr #rtffile: srep$(data$(pict_end+2:len(data$)),"|","}{\tab}{")&"}"&crlf$
53080 else if pos("1234567890",data$(1:1))>0 then 
53082   mat ccells$(udim(cells$)) : fncells(rtffile,data$,mat cells$)
53085 else 
53090   pr #rtffile: "{"&srep$(srep$(data$(3:len(data$)),"|","}{\tab}{"),"[RTFBAR]","|")&"\par }"&crlf$
53100 end if 
53103 if newdata then pr #rtffile: "}" !:
        ! -------------------------------- !:
        ! ? Turns off BLUE newline text                                 !:
        ! ?                                                             !:
        ! -------------------------------- !
53105 laststyle=0
53110 return 
53999 ! -------------------------------- !:
      ! ?Create table lines and pr to existing pr file          !:
      ! ?                                                             !:
      ! -------------------------------- !
54000 def fncells(prntfil,cdata$*2000,mat ccells$)
54010 ! prntfil is the already open rtfoutput file !:
        ! cdata$ is the copy of DATA$ being used by the function !:
        ! mat ccells$ is the matrix of cell definitions received as MAT cells$
54020 ! INPUT FIELDS "24,64,c 1": PAUSE$
54030   dim brdr$*500,celldata$(1)*1000,call$*500,csize$(1)*500,celldef$*3000
54040   css=cse=0 ! starting position
54050 ! -------------------------------- !:
        ! ?Parse the incoming data line to determine what table         !:
        ! ?format to use                                                !:
        ! -------------------------------- !
54060 SETCSS: css=cse+1
54070   cse=min(len(cdata$)+1,pos(cdata$&"|","|",css))
54080   if css=1 then 
54090     cell=val(cdata$(css:cse-1))
54100     cnum=0
54110     goto SETCSS
54120   end if 
54130 ! -------------------------------- !:
        ! ? Set matrix CELLDATA$ to contents of mat cells$              !:
        ! ?                                                             !:
        ! -------------------------------- !
54140   if css<len(cdata$) then 
54150     cnum+=1 !:
          mat celldata$(cnum) !:
          celldata$(cnum)=cdata$(css:cse-1)
54160     goto SETCSS
54170   end if 
54175 ! -------------------------------- !:
        ! ?Begin building table definition parameters from CELLS$(cell) !:
        ! ?                                                             !:
        ! -------------------------------- !
54180   cnum=css=cse=0 !:
        cface$=cfont$="" !:
        call$=crlf$&"\trowd \trgaph108\trleft-108"
54190 ! -------------------------------- !:
        ! ? Build the RTF properties for the table line needed          !:
        ! ?                                                             !:
        ! -------------------------------- !
54200 SETCELL: css=cse+1
54210   cse=pos(ccells$(cell)&"|","|",css)
54220   if cse<=len(ccells$(cell)) then 
54230 ! -------------------------------- !:
          ! ?Left side indent if any                                      !:
          ! ?                                                             !:
          ! -------------------------------- !
54240     if pos(ccells$(cell)(css:cse-1),"li")=1 then !:
            cindent$=fntwips$(val(ccells$(cell)(css+2:cse-1))) !:
            cindent=val(cindent$) !:
            call$=call$&"\trleft"&cindent$ !:
            goto SETCELL
54250 ! -------------------------------- !:
          ! ?Set the distance between cells.  The number is 1/2 the       !:
          ! ?distance measured in inches converted to twips               !:
          ! -------------------------------- !
54260     ! IF POS(CCELLS$(CELL)(CSS:CSE-1),"tg")=1 THEN !:                                           x$=FNTWIPS$(.5*VAL(CCELLS$(CELL)(CSS+2:CSE-1))) !:                                    cALL$=SREP$(CALL$,"\trgaph108\trleft-108","\trgaph"&X$&"\trleft-"&X$&" ") !:          GOTO SETCELL
54261     if pos(ccells$(cell)(css:cse-1),"tg")=1 then !:
            x$=fntwips$(.5*val(ccells$(cell)(css+2:cse-1))) !:
            call$=srep$(call$,"\trgaph108\trleft-108","\trgaph"&x$) !:
            goto SETCELL
54269 ! -------------------------------- !:
          ! ?Designate header rows for repeat on subsequent pages         !:
          ! ?Rows must be the first row(s) in the table and contiguous    !:
          ! -------------------------------- !
54270     if pos(ccells$(cell)(css:cse-1),"trh")=1 then !:
            call$=call$&"\trhdr" !:
            goto SETCELL
54272 ! -------------------------------- !:
          ! ?Designate row height in postive inches for AT LEAST height   !:
          ! ?use negative inches for absolute height                      !:
          ! -------------------------------- !
54274     if pos(ccells$(cell)(css:cse-1),"trrh")=1 then !:
            call$=call$&"\trrh"&fntwips$(val(ccells$(cell)(css+4:cse-1))) !:
            goto SETCELL
54280 ! -------------------------------- !:
          ! ?Set font size in points                                      !:
          ! ?                                                             !:
          ! -------------------------------- !
54290     if pos(ccells$(cell)(css:cse-1),"fs")=1 and cnum=0 then !:
            cfont$="\fs"&str$(val(ccells$(cell)(css+2:cse-1))*2)&"" !:
            goto SETCELL
54291 ! -------------------------------- !:
          ! ?Set font type face                                           !:
          ! ?                                                             !:
          ! -------------------------------- !
54292     if uprc$(ccells$(cell)(css:cse-1))="FTIMES" and cnum=0 then !:
            cface$="\f0" !:
            goto SETCELL
54293     if uprc$(ccells$(cell)(css:cse-1))="FARIAL" and cnum=0 then !:
            cface$="\f1" !:
            goto SETCELL
54294     if uprc$(ccells$(cell)(css:cse-1))="FCOURIER" and cnum=0 then !:
            cface$="\f2" !:
            goto SETCELL
54295     if uprc$(ccells$(cell)(css:cse-1))="FFRITZ" and cnum=0 then !:
            cface$="\f28" !:
            goto SETCELL
54296     if uprc$(ccells$(cell)(css:cse-1))="FOCR-A" and cnum=0 then !:
            cface$="\f42" !:
            goto SETCELL
54297     if uprc$(ccells$(cell)(css:cse-1))="FPALATINO" and cnum=0 then !:
            cface$="\f29" !:
            goto SETCELL
54298     if uprc$(ccells$(cell)(css:cse-1))="FLUCIDA" and cnum=0 then !:
            cface$="\f69" !:
            goto SETCELL
54300 ! -------------------------------- !:
          ! ?Build formats for each Cell left to right                    !:
          ! ?                                                             !:
          ! -------------------------------- !
54310     if pos(ccells$(cell)(css:cse-1),"c")=1 then 
54320       cnum+=1 !:
            mat cfmt$(cnum): mat csize$(cnum) : mat csize(cnum) !:
            cfmt$(cnum)="" : mat cface$(cnum) : mat cfont$(cnum) !:
            cface$(cnum)=cface$ !:
            cfont$(cnum)=cfont$
54330       csize(cnum)=val(fntwips$(val(ccells$(cell)(css+1:cse-1))))
54340       ! cSIZE$(CNUM)="\cellx"&STR$(SUM(CSIZE)+CINDENT) !:                                     GOTO SETCELL
54341       csize$(cnum)="\cellx"&str$(sum(csize)+cindent) !:
            goto SETCELL
54350     end if 
54360 ! -------------------------------- !:
          ! ?Determine border width from 0 to 5 (0 to 75 TWIPS)           !:
          ! ?                                                             !:
          ! -------------------------------- !
54370     if pos(ccells$(cell)(css:cse-1),"br")=1 then 
54380       brdr$=""
54390       if pos(cells$(cell)(css+2:cse),"s")>0 then brdrtyp$="s" else !:
              if pos(cells$(cell)(css+2:cse),"o")>0 then brdrtyp$="o" else !:
                if pos(cells$(cell)(css+2:cse),"d")>0 then brdrtyp$="d" else !:
                  if pos(cells$(cell)(css+2:cse),"a")>0 then brdrtyp$="a" else !:
                    brdrtyp$="s"
54400       brdrw=min(5,val(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(ccells$(cell)(css:cse-1),"b",""),"t",""),"r",""),"l",""),"b",""),"a",""),"s",""),"d",""),"o","")))*15
54410       if (x:=pos(ccells$(cell)(css+2:cse-1),"t"))>0 then !:
              brdr$="\clbrdrt"&fnbrdr$(brdrtyp$)&"\brdrw"&str$(brdrw)&" "
54420       if (x:=pos(ccells$(cell)(css+2:cse-1),"r"))>0 then !:
              brdr$=brdr$&"\clbrdrr"&fnbrdr$(brdrtyp$)&"\brdrw"&str$(brdrw)&" "
54430       if (x:=pos(ccells$(cell)(css+1:cse-1),"l"))>0 then !:
              brdr$=brdr$&"\clbrdrl"&fnbrdr$(brdrtyp$)&"\brdrw"&str$(brdrw)&" "
54440       if (x:=pos(ccells$(cell)(css+1:cse-1),"b"))>0 then !:
              brdr$=brdr$&"\clbrdrb"&fnbrdr$(brdrtyp$)&"\brdrw"&str$(brdrw)&" "
54450       csize$(cnum)=brdr$&csize$(cnum)
54460       goto SETCELL
54470     end if 
54480 ! -------------------------------- !:
          ! ?Set vertical alignment for each cell                         !:
          ! ?                                                             !:
          ! -------------------------------- !
54490     if pos(ccells$(cell)(css:cse-1),"v")=1 then 
54500       if pos(ccells$(cell)(css+1:cse-1),"t")=1 then csize$(cnum)=srep$(csize$(cnum),"\cellx","\clvertalt\cellx")
54510       if pos(ccells$(cell)(css+1:cse-1),"c")=1 then csize$(cnum)=srep$(csize$(cnum),"\cellx","\clvertalc\cellx")
54520       if pos(ccells$(cell)(css+1:cse-1),"b")=1 then csize$(cnum)=srep$(csize$(cnum),"\cellx","\clvertalb\cellx")
54530       goto SETCELL
54540     end if 
54550 ! -------------------------------- !:
          ! ?Set horizontal alignment of each cell                        !:
          ! ?                                                             !:
          ! -------------------------------- !
54560     if pos(ccells$(cell)(css:cse-1),"h")=1 then 
54570       if pos(ccells$(cell)(css+1:cse-1),"r")=1 then cfmt$(cnum)=cfmt$(cnum)&"\qr"
54580       if pos(ccells$(cell)(css+1:cse-1),"c")=1 then cfmt$(cnum)=cfmt$(cnum)&"\qc"
54590       if pos(ccells$(cell)(css+1:cse-1),"l")=1 then cfmt$(cnum)=cfmt$(cnum)&"\ql"
54600       goto SETCELL
54610     end if 
54620 ! -------------------------------- !:
          ! ?Set the cell font if different than the default              !:
          ! ?                                                             !:
          ! -------------------------------- !
54630     if uprc$(ccells$(cell)(css:cse-1))="FTIMES" then !:
            cface$(cnum)="\f0 " !:
            goto SETCELL
54640     if uprc$(ccells$(cell)(css:cse-1))="FARIAL" then !:
            cface$(cnum)="\f1 " !:
            goto SETCELL
54650     if uprc$(ccells$(cell)(css:cse-1))="FCOURIER" then !:
            cface$(cnum)="\f2 " !:
            goto SETCELL
54660     if uprc$(ccells$(cell)(css:cse-1))="FFRITZ" then !:
            cface$(cnum)="\f28 " !:
            goto SETCELL
54661     if uprc$(ccells$(cell)(css:cse-1))="FPALATINO" then !:
            cface$(cnum)="\f29 " !:
            goto SETCELL
54662     if uprc$(ccells$(cell)(css:cse-1))="FOCR-A" then !:
            cface$(cnum)="\f42 " !:
            goto SETCELL
54665     if uprc$(ccells$(cell)(css:cse-1))="FLUCIDA" then !:
            cface$(cnum)="\f69 " !:
            goto SETCELL
54670 ! -------------------------------- !:
          ! ?Set the cell font if different than the default              !:
          ! ?                                                             !:
          ! -------------------------------- !
54680     if pos(ccells$(cell)(css:cse-1),"fs")=1 and cnum>0 then !:
            cfont$(cnum)="\fs"&str$(val(ccells$(cell)(css+2:cse-1))*2)&" " !:
            goto SETCELL
54690 ! -------------------------------- !:
          ! ?Set the backround shading of each cell                       !:
          ! ?                                                             !:
          ! -------------------------------- !
54700     if pos(ccells$(cell)(css:cse-1),"sh")=1 then 
54710       csize$(cnum)=srep$(csize$(cnum),"\cellx","\clshdng"&str$(val(ccells$(cell)(css+2:cse-1))*100)&" "&"\cellx")
54720       goto SETCELL
54730     end if 
54740     goto SETCELL
54750   end if 
54760 PRINT_CELLS: !:
        ! -------------------------------- !:
        ! ?Start the printing of the CELL row                           !:
        ! ?                                                             !:
        ! -------------------------------- !
54770   mat celldata$(cnum) !:
        ! -------------------------------- !:
        ! ?Make the data matrix the same size as the number of cells    !:
        ! ?                                                             !:
        ! -------------------------------- !
54780 ! INPUT FIELDS "24,64,c 1": PAUSE$
54790   celldef$=call$
54800 ! -------------------------------- !:
        ! ?Define the cell row and number and size of cells             !:
        ! ?                                                             !:
        ! -------------------------------- !
54810   for a=1 to cnum
54820     if a=cnum then celldef$=celldef$&csize$(a)&crlf$ else celldef$=celldef$&csize$(a)
54830   next a
54840 ! -------------------------------- !:
        ! ?Print the cells                                              !:
        ! ?                                                             !:
        ! -------------------------------- !
54850   pr #prntfil: cface$&cfont$&celldef$
54860   for a=1 to cnum
54870     if a=1 then pr #prntfil: "\pard \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0"&crlf$
54880     pr #prntfil: cfmt$(a)&"{"&cface$(a)&cfont$(a)&srep$(srep$(celldata$(a),"|","}{ \tab }{ "),"[RTFBAR]","|")&"\cell }"&crlf$
54890     if a=cnum then pr #prntfil: "\pard \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0"&crlf$
54900   next a
54910   pr #prntfil: "{"&celldef$&crlf$&"\row }"&crlf$
54920   pr #prntfil: "\pard \ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0"&crlf$
54930   laststyle=1
54990 fnend 
55000 def fnbrdr$(btype$)
55010   fnbrdr$=""
55020   if btype$="s" then let fnbrdr$="\brdrs"
55030   if btype$="o" then let fnbrdr$="\brdrdot"
55040   if btype$="a" then let fnbrdr$="\brdrdash"
55050   if btype$="d" then let fnbrdr$="\brdrdb"
55060 fnend 
55100 def fntwips$(inch)
55110   if inch>0 then twips$=str$(round(inch*1440,0))
55112   if inch<0 then twips$="-"&str$(round(abs(inch)*1440,0))
55114   if inch=0 then twips$=""
55120   fntwips$=twips$
55130 fnend 
55200 ! --------------------------------
55210 SET_SPECFILE: ! 
55220 sps=pos(data$,"[SPEC(")+6
55230 spe=pos(data$,")]",sps)-1
55240 if not setspec then goto 55260
55250 if exists(picpath$&trim$(data$(sps:spe))) then specfile$=picpath$&trim$(data$(sps:spe)) else pause : msgbox("Designated SPEC file does not exist.") : input fields "10,10,c 10": pause$
55260 data$=srep$(data$,"[SPEC("&data$(sps:spe)&")]","")
55270 ! PAUSE
55280 return 
60000 def library fntype(infile$*100,outfile) !:
        ! 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커!:
        ! ?Moves the contents of one file into another without using      !:
        ! ?the BR TYPE command. OUTFILE must be an existing open file.    !:
        ! 읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸!
60010 ! INPUT FIELDS "23,60, C1": PAUSE$
60020   infile=30
60030   if file(infile)<0 then goto 60050
60040   infile+=1 !:
        goto 60030
60050   open #infile: "name="&infile$&",recl=1",external,input 
60060   infile_lrec=lrec(infile)
60070   close #infile: 
60080   infile_recl=min(32000,infile_lrec)
60090   open #infile: "name="&infile$&",RECL="&str$(infile_recl),external,input,relative 
60100   infile_rec=0
60110   infile_frm$="FORM C "&str$(infile_recl)
60120   if infile_recl=32000 then 
60130     if infile_rec*infile_recl+infile_recl<=infile_lrec then 
60140       read #infile,using infile_frm$: inrec$ !:
            pr #outfile: inrec$ !:
            infile_rec+=1
60150       goto 60130
60160     else 
60170       infile_frm$="FORM C "&str$(infile_lrec-infile_rec*32000)
60180       close #infile: 
60190       open #infile: "name="&infile$&",RECL="&str$(infile_lrec-infile_rec*32000),external,input,relative 
60200     end if 
60210   end if 
60220   read #infile,using infile_frm$,pos=infile_rec*infile_recl+1: inrec$ !:
        pr #outfile: inrec$
60230   dim inrec$*32000
60240 ZTYPE: close #infile: !:
        infile=0
60250 fnend 
62000 IGNORE: continue 
