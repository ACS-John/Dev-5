00010 ! Replace S:\Core\Select.br
00040   def library fnselect(win,&cap$,&q$,mat option$,&response$,display_cnam,button_option,win_align,pr_newpg)
00050 ! Q$         = Question to ask
00060 ! option$(x) = any f x different answers
00070 ! response$  = the number of the answer selected or value "F1"-"F100"
00090     library 'S:\Core\Library': fnwin3b,fnerror
00100     dim fio1$(22)*76,header$*80
00110     let q$=rtrm$(q$)
00120     for j=1 to udim(option$)
00130       let mxwo=max(mxwo,len(option$(j)))
00140     next j
00150     let win_width=mxwo+len(q$)+3
00160     let win_height=udim(option$)+2
00170     let wline=1
00180     if display_cnam=1 then let win_height+=2 : let wline+=2
00190     if display_cnam=2 then let win_height+=1 : let wline+=1
00200     for j=1 to udim(option$)
00210       let fio1$(j)=str$(j+wline)&","&str$(len(q$)+3)&",C "&str$(mxwo)&",N"
00220     next j
00230     let fnwin3b(win,cap$,win_height,win_width,display_cnam,button_option,win_align,pr_newpg)
00240     pr #win,fields str$(wline+1)&",2,C "&str$(len(q$))&",N": q$
00250     rinput #win,select mat fio1$,attr "H": mat option$
00260     let response$=str$(curfld)
00270     if cmdkey=1 and button_option<=1 then goto L310
00280     for j=1 to 100
00290       if cmdkey=j then let response$="F"&str$(curfld)
00300     next j
00310 L310: close #win: ioerr L320
00320 L320: fnend 
00330 ! ______________________________________________________________________
