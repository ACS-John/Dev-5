00010 ! Replace S:\Core\combo1.br
00020 ! ______________________________________________________________________
00030 FNCOMBO1: ! 
00040   def library fncombo1(win,file_number_or_name$,mat label$,mat starting_pos,mat field_len,mat cb_response$,filter$)
00050     library 'S:\Core\Library': fnwin3b,fnerror
00060     on error goto ERTN
00070 ! WIN = WINDOW TO use
00080 ! file_number_or_name$ = filename of file to open OR file number if already open
00090 ! mat label$ = field labels (headers)
00100 ! mat starting_pos = starting position of fields
00110 ! mat field_len = length of fields
00120 ! mat cb_response$ = choosen record (fields you've selected) !:
          ! this should be dim at*(same as longest field_len)
00130 ! filter$ = blank for none !:
          !         xx* = anything that starts with xx (filter-type1)!:
          !         *xx = anything that starts with xx (filter-type2)
00140 ! ___________________________
00150     dim frm$*999,choice$(20)*80,header$*80,cap$*128,fio1$(20)
00160 ! ___________________________
00170     max_size=20 ! should be the same as xx in dim choice$(xx)
00180 ! Let WIN_WIDTH=0 obsoluete
00190     let filnum=val(file_number_or_name$) conv OPEN20
00200     if file(filnum)=-1 then goto XIT
00210 CONTINUE_AFTER_OPEN20: ! 
00220     origional_rec=rec(filnum)
00230     restore #filnum,key>=lpad$("",kln(filnum)): 
00240     gosub BUILD_FRM
00250     gosub BUILD_HEADER
00260     goto START
00270 ! ___________________________
00280 BUILD_FRM: ! 
00290     let frm$="Form "
00300     for j=1 to udim(mat starting_pos) !:
            let frm$=frm$&"Pos "&str$(starting_pos(j)) !:
            let frm$=frm$&",C "&str$(field_len(j))
00310       if j<>udim(mat starting_pos) then let frm$=frm$&","
00320     next j
00330     return 
00340 ! ___________________________
00350 BUILD_HEADER: ! 
00360     header$=""
00370     for j=1 to udim(label$)
00380       header$=header$&(label$(j)(1:field_len(j)))
00390       if j<>1 and j<>udim(label$) then header$=header$&"³"
00400       let win_width+=field_len(j)+1
00410 ! If J<>1 AND J<>UDIM(LABEL$) Then Let WIN_WIDTH+=1
00420     next j
00430     return 
00440 ! ___________________________
00450 EOFILE: ! 
00460     eofile=1
00470     goto PAST_READ_LOOP
00480 ! ___________________________
00490 START: ! 
00500     hit_count=0 !:
          eofile=0
00510 NXT: ! 
00520     mat choice$(max_size)
00530     for j=1 to udim(choice$)
00540 L540: read #filnum,using frm$,release: mat cb_response$ eof EOFILE
00550       let filter_pass=0 !:
            gosub FILTER !:
            if filter_pass=1 then goto L560 !:
            else goto L540
00560 L560: hit_count+=1 !:
            choice$(j)=cb_response$(1)
00570       for k=2 to udim(cb_response$)
00580         choice$(j)=rtrm$(choice$(j)&"³"&cb_response$(k))
00590       next k
00600     next j
00610 PAST_READ_LOOP: ! 
00620     mat cb_response$=("")
00630     if j<max_size then mat choice$(j-1)
00640     if udim(choice$)=0 then goto DONE
00650     let win_height=udim(choice$)+1
00660     let win_width=32 ! reset to min. size to fit buttons !:
          for l=1 to win_height-1 !:
            let win_width=max(win_width,len(choice$(l))) !:
          next l
00670     for j=1 to udim(fio1$) !:
            let fio1$(j)=str$(j+1)&",1,C "&str$(win_width)&",N" !:
          next j
00680     fnwin3b(win,cap$,win_height,win_width,0,6,6,2)
00690     pr #win,fields "1,1,C "&str$(win_width)&",R,N": header$
00700 L700: rinput #win,select mat fio1$,attr "H": mat choice$
00710     if cmdkey=1 and eofile=0 then goto NXT
00720     if cmdkey=1 and eofile=1 then goto L700
00730     if cmdkey=2 then !:
            eofile=0 !:
            restore #filnum,key>=lpad$("",kln(filnum)): !:
            goto NXT
00740     if cmdkey=5 or cmdkey=99 then mat cb_response$=("") : goto DONE
00750     let x=0 !:
          for j=1 to udim(cb_response$) !:
            let x+=1 !:
            cb_response$(j)=choice$(curfld)(x:field_len(j)) !:
            let x+=field_len(j) !:
          next j
00760 ! 
00770 ! 
00780 ! 
00790 ! 
00800 ! 
00810     goto DONE
00820 ! ___________________________
00830 FILTER: ! 
00840     star_pos=pos(filter$,"*")
00850     if star_pos=1 then gosub FILTER_TYPE2
00860     if star_pos>1 then gosub FILTER_TYPE1
00870     if star_pos=0 then let filter_pass=1
00880     return 
00890 ! ____________________
00900 FILTER_TYPE1: ! 
00910     if cb_response$(1)(1:star_pos-1)=filter$(1:star_pos-1) then !:
            let filter_pass=1
00920     return 
00930 ! ____________________
00940 FILTER_TYPE2: ! 
00950     return 
00960 ! ___________________________
00970 OPEN20: ! 
00980     let filnum=20
00990     iopenedthis=1
01000     open #filnum: "Name="&file_number_or_name$,internal,input,keyed ioerr XIT
01010     goto CONTINUE_AFTER_OPEN20
01020 ! ___________________________
01030 DONE: ! 
01040     mat choice$(max_size)
01050     if iopenedthis=1 then close #filnum: ioerr XIT
01060     close #win: ioerr XIT
01070 XIT: ! 
01080   fnend 
01090 ! ___________________________
01100 ERTN: fnerror(program$,err,line,act$,"xit")
01110   if lwrc$(act$)<>"pause" then goto L1140
01120   execute "list -"&str$(line) !:
        pause  !:
        goto L1140
01130   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01140 L1140: execute act$
01150   goto ERTN
01160 ! ______________________________________________________________________
